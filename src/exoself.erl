%% @doc Exoself - Neural network lifecycle manager
%%
%% The exoself is responsible for spawning, linking, and managing the
%% lifecycle of a neural network phenotype from its genotype. It handles:
%%
%% - Spawning all network processes (sensors, neurons, actuators, cortex)
%% - Linking processes with their inputs and outputs
%% - Weight tuning with simulated annealing
%% - Network backup and restoration
%% - Fitness evaluation coordination
%%
%% == Lifecycle ==
%%
%% 1. prep/3 - Initialize state and spawn network
%% 2. loop/1 - Handle tuning cycles and evaluation
%% 3. Terminate - Clean up network processes
%%
%% == Tuning Algorithm ==
%%
%% The exoself uses memetic weight tuning with simulated annealing:
%% 1. Perturb subset of weights
%% 2. Evaluate network fitness
%% 3. If better: keep changes, update best
%% 4. If worse: restore previous weights
%% 5. Reduce perturbation over attempts (annealing)
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(exoself).

-include("records.hrl").

%% Suppress supertype warnings - specs are intentionally general for API clarity
-dialyzer({nowarn_function, [
    initialize_base_state/4,
    loop/1,
    handle_evaluation_complete/2,
    terminate_network/1
]}).

-export([
    start/3,
    prep/3,
    calculate_perturbation/4
]).

%% Internal exports for spawned process
-export([init/4]).

-record(exoself_state, {
    agent_id :: term(),
    morphology :: atom(),
    generation :: non_neg_integer(),
    population_monitor_pid :: pid() | undefined,
    id_to_process_map :: ets:tid(),
    cortex_pid :: pid() | undefined,
    sensor_pids = [] :: [pid()],
    neuron_pids = [] :: [pid()],
    actuator_pids = [] :: [pid()],
    sensor_ids = [] :: [term()],
    neuron_ids = [] :: [term()],
    actuator_ids = [] :: [term()],
    private_scape_pids = [] :: [pid()],
    public_scape_pids = [] :: [pid()],
    highest_fitness :: number() | undefined,
    evaluation_count = 0 :: non_neg_integer(),
    cycle_count = 0 :: non_neg_integer(),
    time_accumulated = 0 :: non_neg_integer(),
    max_attempts = 15 :: pos_integer(),
    current_attempt = 1 :: pos_integer(),
    tuning_duration_function :: {atom(), number()} | undefined,
    tuning_selection_function :: atom() | undefined,
    annealing_parameter = 0.5 :: float(),
    perturbation_range = 1.0 :: float(),
    substrate_pid :: pid() | undefined,
    cpp_pids = [] :: [pid()],
    cep_pids = [] :: [pid()],
    operation_mode = gt :: gt | validation | test
}).

%% @doc Start an exoself process for an agent.
%%
%% Spawns an exoself that will construct and manage the phenotype
%% for the given agent.
%%
%% @param AgentId The agent identifier
%% @param PopMonitorPid The population monitor PID (or undefined)
%% @param OpMode Operation mode: gt, validation, or test
%% @returns {ok, Pid} where Pid is the exoself process
-spec start(term(), pid() | undefined, gt | validation | test) -> {ok, pid()}.
start(AgentId, PopMonitorPid, OpMode) ->
    Pid = spawn_link(?MODULE, init, [self(), AgentId, PopMonitorPid, OpMode]),
    {ok, Pid}.

%% @doc Prepare and start the network (called by spawned process).
-spec prep(term(), pid() | undefined, gt | validation | test) -> no_return().
prep(AgentId, PopMonitorPid, OpMode) ->
    init(undefined, AgentId, PopMonitorPid, OpMode).

%% @doc Initialize the exoself and spawn the network.
-spec init(pid() | undefined, term(), pid() | undefined, gt | validation | test) -> no_return().
init(_CallerPid, AgentId, PopMonitorPid, OpMode) ->
    %% Read agent from genotype database
    {agent, Agent} = genotype:dirty_read({agent, AgentId}),

    %% Create ID to process mapping table
    IdToProcessMap = ets:new(id_to_process_map, [set, private]),

    %% Initialize base state
    State = initialize_base_state(Agent, PopMonitorPid, OpMode, IdToProcessMap),

    %% Spawn network components
    {SensorPids, SensorIds} = spawn_sensors(State, Agent),
    {NeuronPids, NeuronIds} = spawn_neurons(State, Agent),
    {ActuatorPids, ActuatorIds} = spawn_actuators(State, Agent),
    CortexPid = spawn_cortex(State, Agent, SensorPids, NeuronPids, ActuatorPids),

    %% Link network components
    link_sensors(State, Agent),
    link_neurons(State, Agent),
    link_actuators(State, Agent),

    %% Update state with spawned processes
    FinalState = State#exoself_state{
        cortex_pid = CortexPid,
        sensor_pids = SensorPids,
        neuron_pids = NeuronPids,
        actuator_pids = ActuatorPids,
        sensor_ids = SensorIds,
        neuron_ids = NeuronIds,
        actuator_ids = ActuatorIds
    },

    %% Start evaluation
    cortex:sync(CortexPid),

    %% Enter main loop
    loop(FinalState).

%% @private Initialize base state from agent record.
-spec initialize_base_state(#agent{}, pid() | undefined, atom(), ets:tid()) -> #exoself_state{}.
initialize_base_state(Agent, PopMonitorPid, OpMode, IdToProcessMap) ->
    #agent{
        id = AgentId,
        constraint = Constraint,
        generation = Generation,
        tuning_selection_f = TuningSelectionF,
        annealing_parameter = AnnealingParam,
        tuning_duration_f = TuningDurationF,
        perturbation_range = PerturbationRange
    } = Agent,

    Morphology = Constraint#constraint.morphology,

    #exoself_state{
        agent_id = AgentId,
        morphology = Morphology,
        generation = Generation,
        population_monitor_pid = PopMonitorPid,
        id_to_process_map = IdToProcessMap,
        tuning_selection_function = TuningSelectionF,
        annealing_parameter = AnnealingParam,
        tuning_duration_function = TuningDurationF,
        perturbation_range = PerturbationRange,
        operation_mode = OpMode
    }.

%% @private Spawn all sensor processes.
-spec spawn_sensors(#exoself_state{}, #agent{}) -> {[pid()], [term()]}.
spawn_sensors(State, Agent) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    {cortex, Cortex} = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    SensorIds = Cortex#cortex.sensor_ids,

    lists:foldl(
        fun(SensorId, {PidsAcc, IdsAcc}) ->
            {sensor, Sensor} = genotype:dirty_read({sensor, SensorId}),
            {ok, Pid} = sensor:start_link(#{
                id => SensorId,
                sensor_name => Sensor#sensor.name,
                vector_length => Sensor#sensor.vl,
                cortex_pid => self(),
                parameters => Sensor#sensor.parameters
            }),
            ets:insert(IdMap, {SensorId, Pid}),
            {[Pid | PidsAcc], [SensorId | IdsAcc]}
        end,
        {[], []},
        SensorIds
    ).

%% @private Spawn all neuron processes.
-spec spawn_neurons(#exoself_state{}, #agent{}) -> {[pid()], [term()]}.
spawn_neurons(State, Agent) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    {cortex, Cortex} = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    NeuronIds = Cortex#cortex.neuron_ids,

    lists:foldl(
        fun(NeuronId, {PidsAcc, IdsAcc}) ->
            {neuron, Neuron} = genotype:dirty_read({neuron, NeuronId}),
            {ok, Pid} = neuron:start_link(#{
                id => NeuronId,
                cortex_pid => self(),
                activation_function => Neuron#neuron.af,
                aggregation_function => Neuron#neuron.aggr_f
            }),
            ets:insert(IdMap, {NeuronId, Pid}),
            {[Pid | PidsAcc], [NeuronId | IdsAcc]}
        end,
        {[], []},
        NeuronIds
    ).

%% @private Spawn all actuator processes.
-spec spawn_actuators(#exoself_state{}, #agent{}) -> {[pid()], [term()]}.
spawn_actuators(State, Agent) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    {cortex, Cortex} = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    ActuatorIds = Cortex#cortex.actuator_ids,

    lists:foldl(
        fun(ActuatorId, {PidsAcc, IdsAcc}) ->
            {actuator, Actuator} = genotype:dirty_read({actuator, ActuatorId}),
            {ok, Pid} = actuator:start_link(#{
                id => ActuatorId,
                actuator_name => Actuator#actuator.name,
                vector_length => Actuator#actuator.vl,
                cortex_pid => self(),
                parameters => Actuator#actuator.parameters
            }),
            ets:insert(IdMap, {ActuatorId, Pid}),
            {[Pid | PidsAcc], [ActuatorId | IdsAcc]}
        end,
        {[], []},
        ActuatorIds
    ).

%% @private Spawn cortex process.
-spec spawn_cortex(#exoself_state{}, #agent{}, [pid()], [pid()], [pid()]) -> pid().
spawn_cortex(_State, Agent, SensorPids, NeuronPids, ActuatorPids) ->
    {ok, CortexPid} = cortex:start_link(#{
        id => Agent#agent.cx_id,
        exoself_pid => self(),
        sensor_pids => SensorPids,
        neuron_pids => NeuronPids,
        actuator_pids => ActuatorPids
    }),
    CortexPid.

%% @private Link sensors to their output neurons.
-spec link_sensors(#exoself_state{}, #agent{}) -> ok.
link_sensors(State, Agent) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    {cortex, Cortex} = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    lists:foreach(
        fun(SensorId) ->
            {sensor, Sensor} = genotype:dirty_read({sensor, SensorId}),
            [{SensorId, SensorPid}] = ets:lookup(IdMap, SensorId),
            FanoutPids = [
                begin
                    [{Id, Pid}] = ets:lookup(IdMap, Id),
                    Pid
                end
                || Id <- Sensor#sensor.fanout_ids
            ],
            SensorPid ! {link, fanout_pids, FanoutPids}
        end,
        Cortex#cortex.sensor_ids
    ).

%% @private Link neurons to their inputs and outputs.
-spec link_neurons(#exoself_state{}, #agent{}) -> ok.
link_neurons(State, Agent) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    {cortex, Cortex} = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    lists:foreach(
        fun(NeuronId) ->
            {neuron, Neuron} = genotype:dirty_read({neuron, NeuronId}),
            [{NeuronId, NeuronPid}] = ets:lookup(IdMap, NeuronId),

            %% Convert input IDs to PIDs
            InputPids = [
                begin
                    [{InputId, InputPid}] = ets:lookup(IdMap, InputId),
                    InputPid
                end
                || {InputId, _Weights} <- Neuron#neuron.input_idps
            ],

            %% Build input weights map (PID -> weights)
            InputWeights = maps:from_list([
                begin
                    [{InputId, InputPid}] = ets:lookup(IdMap, InputId),
                    {InputPid, Weights}
                end
                || {InputId, Weights} <- Neuron#neuron.input_idps
            ]),

            %% Convert output IDs to PIDs
            OutputPids = [
                begin
                    [{OutputId, OutputPid}] = ets:lookup(IdMap, OutputId),
                    OutputPid
                end
                || OutputId <- Neuron#neuron.output_ids
            ],

            %% Convert recurrent output IDs to PIDs
            ROPids = [
                begin
                    [{ROId, ROPid}] = ets:lookup(IdMap, ROId),
                    ROPid
                end
                || ROId <- Neuron#neuron.ro_ids
            ],

            %% Send link messages to neuron
            NeuronPid ! {link, input_pids, InputPids},
            NeuronPid ! {link, output_pids, OutputPids},
            NeuronPid ! {link, ro_pids, ROPids},
            NeuronPid ! {link, input_weights, InputWeights}
        end,
        Cortex#cortex.neuron_ids
    ).

%% @private Link actuators to their input neurons.
-spec link_actuators(#exoself_state{}, #agent{}) -> ok.
link_actuators(State, Agent) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    {cortex, Cortex} = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    lists:foreach(
        fun(ActuatorId) ->
            {actuator, Actuator} = genotype:dirty_read({actuator, ActuatorId}),
            [{ActuatorId, ActuatorPid}] = ets:lookup(IdMap, ActuatorId),
            FaninPids = [
                begin
                    [{Id, Pid}] = ets:lookup(IdMap, Id),
                    Pid
                end
                || Id <- Actuator#actuator.fanin_ids
            ],
            ActuatorPid ! {link, fanin_pids, FaninPids}
        end,
        Cortex#cortex.actuator_ids
    ).

%% @private Main loop handling evaluation results and tuning.
-spec loop(#exoself_state{}) -> ok.
loop(State) ->
    #exoself_state{
        agent_id = AgentId,
        cortex_pid = CortexPid,
        population_monitor_pid = PopMonitorPid,
        highest_fitness = HighestFitness,
        current_attempt = CurrentAttempt,
        max_attempts = MaxAttempts,
        evaluation_count = EvalCount
    } = State,

    receive
        {cortex, _CxId, evaluation_complete, Fitness} ->
            handle_evaluation_complete(Fitness, State);

        {cortex, _CxId, max_cycles_reached, _CycleCount} ->
            %% Evaluation complete, report to population monitor
            case PopMonitorPid of
                undefined ->
                    ok;
                _ ->
                    PopMonitorPid ! {self(), AgentId, EvalCount, HighestFitness}
            end,
            terminate_network(State);

        {population_monitor, terminate} ->
            terminate_network(State);

        terminate ->
            terminate_network(State);

        {population_monitor, continue} ->
            %% Continue with next tuning attempt
            case CurrentAttempt < MaxAttempts of
                true ->
                    perturb_weights(State),
                    cortex:sync(CortexPid),
                    loop(State#exoself_state{current_attempt = CurrentAttempt + 1});
                false ->
                    %% Max attempts reached
                    case PopMonitorPid of
                        undefined -> ok;
                        _ -> PopMonitorPid ! {self(), AgentId, EvalCount, HighestFitness}
                    end,
                    terminate_network(State)
            end
    end.

%% @private Handle evaluation completion.
-spec handle_evaluation_complete(number() | [number()], #exoself_state{}) -> ok.
handle_evaluation_complete(Fitness, State) ->
    #exoself_state{
        cortex_pid = CortexPid,
        highest_fitness = HighestFitness,
        current_attempt = CurrentAttempt,
        max_attempts = MaxAttempts,
        evaluation_count = EvalCount
    } = State,

    %% Calculate fitness value (may be list or single value)
    FitnessValue = case is_list(Fitness) of
        true -> lists:sum(Fitness);
        false -> Fitness
    end,

    %% Update highest fitness and decide whether to keep weights
    NewState = case HighestFitness of
        undefined ->
            %% First evaluation
            backup_weights(State),
            State#exoself_state{
                highest_fitness = FitnessValue,
                evaluation_count = EvalCount + 1
            };
        _ when FitnessValue > HighestFitness ->
            %% Improvement - backup new weights
            backup_weights(State),
            State#exoself_state{
                highest_fitness = FitnessValue,
                evaluation_count = EvalCount + 1
            };
        _ ->
            %% No improvement - restore previous weights
            restore_weights(State),
            State#exoself_state{
                evaluation_count = EvalCount + 1
            }
    end,

    %% Check if more attempts
    case CurrentAttempt < MaxAttempts of
        true ->
            perturb_weights(NewState),
            cortex:sync(CortexPid),
            loop(NewState#exoself_state{current_attempt = CurrentAttempt + 1});
        false ->
            %% Tuning complete
            NewState2 = NewState#exoself_state{current_attempt = 1},
            loop(NewState2)
    end.

%% @private Perturb weights using simulated annealing.
-spec perturb_weights(#exoself_state{}) -> ok.
perturb_weights(State) ->
    #exoself_state{
        neuron_pids = NeuronPids,
        perturbation_range = Range,
        annealing_parameter = AnnealingParam,
        current_attempt = Attempt,
        max_attempts = MaxAttempts
    } = State,

    %% Calculate current perturbation using annealing schedule
    Progress = Attempt / MaxAttempts,
    CurrentRange = Range * math:pow(1 - Progress, AnnealingParam),

    %% Perturb each neuron's weights
    lists:foreach(
        fun(NeuronPid) ->
            NeuronPid ! {perturb, CurrentRange}
        end,
        NeuronPids
    ).

%% @private Backup current weights from all neurons.
-spec backup_weights(#exoself_state{}) -> ok.
backup_weights(State) ->
    #exoself_state{neuron_pids = NeuronPids} = State,
    lists:foreach(
        fun(NeuronPid) ->
            NeuronPid ! backup
        end,
        NeuronPids
    ).

%% @private Restore previous weights to all neurons.
-spec restore_weights(#exoself_state{}) -> ok.
restore_weights(State) ->
    #exoself_state{neuron_pids = NeuronPids} = State,
    lists:foreach(
        fun(NeuronPid) ->
            NeuronPid ! restore
        end,
        NeuronPids
    ).

%% @private Terminate all network processes and clean up.
-spec terminate_network(#exoself_state{}) -> ok.
terminate_network(State) ->
    #exoself_state{
        cortex_pid = CortexPid,
        id_to_process_map = IdMap
    } = State,

    %% Terminate cortex (which will terminate sensors, neurons, actuators)
    case CortexPid of
        undefined ->
            ok;
        _ ->
            %% Monitor cortex before terminating
            Ref = erlang:monitor(process, CortexPid),
            cortex:terminate(CortexPid),
            %% Wait for cortex to finish (with timeout)
            receive
                {'DOWN', Ref, process, CortexPid, _Reason} ->
                    ok
            after 5000 ->
                %% Timeout - demonitor and continue (don't force-kill to avoid cascading failures)
                erlang:demonitor(Ref, [flush]),
                tweann_logger:warning("Exoself: cortex ~p did not terminate within 5s", [CortexPid])
            end
    end,

    %% Clean up ETS table
    ets:delete(IdMap),
    ok.

%% @doc Calculate perturbation for current attempt using annealing.
%%
%% Exported for testing.
-spec calculate_perturbation(float(), pos_integer(), pos_integer(), float()) -> float().
calculate_perturbation(InitialRange, Attempt, MaxAttempts, AnnealingParam) ->
    Progress = Attempt / MaxAttempts,
    InitialRange * math:pow(1 - Progress, AnnealingParam).
