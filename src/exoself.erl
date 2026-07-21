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
    handle_evaluation_complete/3,
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
    %% The process to notify with {exoself_terminated, Fitness} when this
    %% agent finishes. population_monitor's spawn_agent closure blocks on that
    %% message; before this field was captured, nothing sent it and every
    %% agent timed out at 5s with fitness [0.0]. See insight 001.
    caller_pid :: pid() | undefined,
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
    %% Memetic hill-climbing steps per evaluation. 15 was too few: XOR
    %% plateaued at RMSE ~0.36 because good topologies never got fully tuned.
    %% 60 solves XOR reliably (insight 012). DXNN2 computes this per agent via
    %% tuning_duration (roadmap item 2); until then it is a fixed default.
    max_attempts = 60 :: pos_integer(),
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
init(CallerPid, AgentId, PopMonitorPid, OpMode) ->
    %% Read agent from genotype database
    Agent = genotype:dirty_read({agent, AgentId}),

    %% Create ID to process mapping table
    IdToProcessMap = ets:new(id_to_process_map, [set, private]),

    %% Initialize base state
    State0 = initialize_base_state(Agent, PopMonitorPid, OpMode, IdToProcessMap),

    %% Spawn scapes before the sensors and actuators that talk to them, as in
    %% DXNN2. Scape name comes from the sensor/actuator records, deduplicated
    %% so a sensor and actuator naming the same scape share one process.
    {ScapePids, ScapeMap} = spawn_scapes(Agent),
    State = State0#exoself_state{caller_pid = CallerPid,
                                 private_scape_pids = ScapePids},

    %% Spawn network components
    {SensorPids, SensorIds} = spawn_sensors(State, Agent, ScapeMap),
    {NeuronPids, NeuronIds} = spawn_neurons(State, Agent),
    {ActuatorPids, ActuatorIds} = spawn_actuators(State, Agent, ScapeMap),
    CortexPid = spawn_cortex(State, Agent, SensorPids, NeuronPids, ActuatorPids),

    %% Link network components
    link_sensors(State, Agent),
    link_neurons(State, Agent),
    link_actuators(State, Agent, CortexPid),

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

    %% Size the memetic tuning effort to this agent's network, via the
    %% tuning_duration function the genotype assigned (insight 001 noted these
    %% were assigned and never used). Replaces the fixed max_attempts default.
    MaxAttempts = compute_max_attempts(FinalState),
    TunedState = FinalState#exoself_state{max_attempts = MaxAttempts},

    %% Start evaluation
    cortex:sync(CortexPid),

    %% Enter main loop
    loop(TunedState).

%% @private Attempts for this agent, from its tuning_duration function.
compute_max_attempts(#exoself_state{tuning_duration_function = undefined,
                                    max_attempts = Default}) ->
    Default;
compute_max_attempts(#exoself_state{tuning_duration_function = {Name, Param},
                                    neuron_ids = NIds, generation = Gen}) ->
    tuning_duration:duration(Name, Param, NIds, Gen);
compute_max_attempts(#exoself_state{max_attempts = Default}) ->
    Default.

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
%% @private
%% @doc Forward goal_reached to the population monitor.
%%
%% This is what makes evaluations-to-solve measurable: without it the counter
%% never freezes at the moment of solution and no comparison against published
%% figures is possible. Handbook Ch 14.
maybe_report_goal_reached(goal_reached, #exoself_state{population_monitor_pid = Pid, agent_id = AgentId})
  when is_pid(Pid) ->
    Pid ! {self(), goal_reached, AgentId},
    ok;
maybe_report_goal_reached(_HaltFlag, _State) ->
    ok.

%% @private Spawn the private scapes named by the sensors and actuators.
%%
%% Reads #sensor.scape and #actuator.scape, keeps the {private, Name} entries,
%% deduplicates, and spawns one process per distinct scape. Returns the pid
%% list (for termination) and a map from scape name to pid (for wiring into
%% sensors and actuators). Public scapes are not spawned here: they are meant
%% to already be running, independent of any one agent.
-spec spawn_scapes(#agent{}) -> {[pid()], #{atom() => pid()}}.
spawn_scapes(Agent) ->
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    SensorScapes = [ (genotype:dirty_read({sensor, Id}))#sensor.scape
                     || Id <- Cortex#cortex.sensor_ids ],
    ActuatorScapes = [ (genotype:dirty_read({actuator, Id}))#actuator.scape
                       || Id <- Cortex#cortex.actuator_ids ],
    Unique = lists:usort([ S || S <- SensorScapes ++ ActuatorScapes, S =/= undefined ]),
    lists:foldl(
        fun({private, Name}, {PidsAcc, MapAcc}) ->
                Pid = scape:gen(self(), node(), {Name, undefined}),
                {[Pid | PidsAcc], MapAcc#{Name => Pid}};
           ({public, _Name}, Acc) ->
                %% Public scapes are not spawned by the exoself.
                Acc
        end,
        {[], #{}},
        Unique
    ).

%% @private Look up a sensor/actuator's scape pid, undefined when none.
scape_pid_for(undefined, _ScapeMap) ->
    undefined;
scape_pid_for({private, Name}, ScapeMap) ->
    maps:get(Name, ScapeMap, undefined);
scape_pid_for({public, _Name}, _ScapeMap) ->
    undefined.

%% @private Spawn all sensor processes, wiring each to its scape.
-spec spawn_sensors(#exoself_state{}, #agent{}, #{atom() => pid()}) ->
    {[pid()], [term()]}.
spawn_sensors(State, Agent, ScapeMap) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    SensorIds = Cortex#cortex.sensor_ids,

    lists:foldl(
        fun(SensorId, {PidsAcc, IdsAcc}) ->
            Sensor = genotype:dirty_read({sensor, SensorId}),
            {ok, Pid} = sensor:start_link(#{
                id => SensorId,
                sensor_name => Sensor#sensor.name,
                vector_length => Sensor#sensor.vl,
                cortex_pid => self(),
                scape_pid => scape_pid_for(Sensor#sensor.scape, ScapeMap),
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
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    NeuronIds = Cortex#cortex.neuron_ids,

    lists:foldl(
        fun(NeuronId, {PidsAcc, IdsAcc}) ->
            Neuron = genotype:dirty_read({neuron, NeuronId}),
            {ok, Pid} = spawn_neuron_by_type(Neuron, NeuronId),
            ets:insert(IdMap, {NeuronId, Pid}),
            {[Pid | PidsAcc], [NeuronId | IdsAcc]}
        end,
        {[], []},
        NeuronIds
    ).

%% @private Spawn a neuron process, dispatching by neuron_type. Standard neurons
%% run the plain weighted-sum-and-activation neuron; ltc/cfc neurons run
%% neuron_ltc, which carries persistent internal state and continuous-time
%% dynamics. This mirrors constructor:spawn_neuron_by_type/2, so the evolutionary
%% path (population_monitor -> exoself) evaluates LTC genotypes as LTC, not as
%% standard neurons that silently ignore their temporal parameters.
-spec spawn_neuron_by_type(#neuron{}, term()) -> {ok, pid()}.
spawn_neuron_by_type(#neuron{neuron_type = Type} = Neuron, NeuronId)
  when Type =:= ltc; Type =:= cfc ->
    neuron_ltc:start_link(#{
        id => NeuronId,
        cortex_pid => self(),
        neuron_type => Type,
        time_constant => Neuron#neuron.time_constant,
        state_bound => Neuron#neuron.state_bound,
        ltc_backbone_weights => Neuron#neuron.ltc_backbone_weights,
        ltc_head_weights => Neuron#neuron.ltc_head_weights,
        internal_state => Neuron#neuron.internal_state,
        activation_function => Neuron#neuron.af,
        aggregation_function => Neuron#neuron.aggr_f
    });
spawn_neuron_by_type(Neuron, NeuronId) ->
    neuron:start_link(#{
        id => NeuronId,
        cortex_pid => self(),
        activation_function => Neuron#neuron.af,
        aggregation_function => Neuron#neuron.aggr_f
    }).

%% @private Spawn all actuator processes.
-spec spawn_actuators(#exoself_state{}, #agent{}, #{atom() => pid()}) ->
    {[pid()], [term()]}.
spawn_actuators(State, Agent, ScapeMap) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    ActuatorIds = Cortex#cortex.actuator_ids,

    lists:foldl(
        fun(ActuatorId, {PidsAcc, IdsAcc}) ->
            Actuator = genotype:dirty_read({actuator, ActuatorId}),
            {ok, Pid} = actuator:start_link(#{
                id => ActuatorId,
                actuator_name => Actuator#actuator.name,
                vector_length => Actuator#actuator.vl,
                cortex_pid => self(),
                scape_pid => scape_pid_for(Actuator#actuator.scape, ScapeMap),
                parameters => Actuator#actuator.parameters
            }),
            ets:insert(IdMap, {ActuatorId, Pid}),
            {[Pid | PidsAcc], [ActuatorId | IdsAcc]}
        end,
        {[], []},
        ActuatorIds
    ).

%% @private Split a `bias' entry out of a neuron's input_idps.
%%
%% add_bias records bias as {bias, [{Weight, Delta, LR, Params}]} among the
%% inputs, the same weight-spec form as every other connection (topological_
%% mutations:do_add_bias/2 via create_random_weight/0). The phenotype neuron
%% treats bias as a plain SCALAR (added during aggregation, perturbed as a
%% number), so this boundary must extract the scalar weight from the spec.
%% Returning the whole tuple made `bias + Number' in the perturb handler crash
%% with badarith. It is a constant contribution, not a signal from a process, so
%% it is never resolved to a pid. Defaults to 0.0 when the neuron has no bias.
extract_bias(InputIdps) ->
    case lists:keytake(bias, 1, InputIdps) of
        {value, {bias, [{W, _Delta, _LR, _Params}]}, Rest} -> {W, Rest};
        {value, {bias, [W]}, Rest} when is_number(W) -> {W, Rest};
        {value, {bias, W}, Rest} when is_number(W) -> {W, Rest};
        false -> {0.0, InputIdps}
    end.

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
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    lists:foreach(
        fun(SensorId) ->
            Sensor = genotype:dirty_read({sensor, SensorId}),
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
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    lists:foreach(
        fun(NeuronId) ->
            link_one_neuron(IdMap, NeuronId)
        end,
        Cortex#cortex.neuron_ids
    ).

%% @private Wire a single neuron to its inputs, outputs, weights and bias.
-spec link_one_neuron(ets:tid(), term()) -> ok.
link_one_neuron(IdMap, NeuronId) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    [{NeuronId, NeuronPid}] = ets:lookup(IdMap, NeuronId),

    %% `bias' is a constant input, not a process. Separate it from the
    %% real inputs so it is not looked up as a pid (which would
    %% badmatch []), and route its weight to the neuron's bias field.
    {BiasWeight, RealInputIdps} = extract_bias(Neuron#neuron.input_idps),

    %% Convert input IDs to PIDs
    InputPids = [
        begin
            [{InputId, InputPid}] = ets:lookup(IdMap, InputId),
            InputPid
        end
        || {InputId, _Weights} <- RealInputIdps
    ],

    %% Build input weights map (PID -> weights)
    InputWeights = maps:from_list([
        begin
            [{InputId, InputPid}] = ets:lookup(IdMap, InputId),
            {InputPid, Weights}
        end
        || {InputId, Weights} <- RealInputIdps
    ]),

    %% Partition outputs into feedforward and recurrent by layer (see
    %% constructor:link_neurons/2). A target at the same or lower layer
    %% is a feedback edge; the stored ro_ids is not trusted because
    %% mutations do not maintain it. The sets are disjoint.
    NeuronLayer = layer_of(Neuron#neuron.id),
    {FeedforwardIds, RecurrentIds} = lists:partition(
        fun(Id) -> layer_of(Id) > NeuronLayer end,
        Neuron#neuron.output_ids
    ),
    OutputPids = [
        begin
            [{FfId, FfPid}] = ets:lookup(IdMap, FfId),
            FfPid
        end
        || FfId <- FeedforwardIds
    ],
    ROPids = [
        begin
            [{ROId, ROPid}] = ets:lookup(IdMap, ROId),
            ROPid
        end
        || ROId <- RecurrentIds
    ],

    %% Send link messages to neuron
    NeuronPid ! {link, input_pids, InputPids},
    NeuronPid ! {link, output_pids, OutputPids},
    NeuronPid ! {link, ro_pids, ROPids},
    NeuronPid ! {link, input_weights, InputWeights},
    NeuronPid ! {link, bias, BiasWeight},
    ok.

%% @private Layer coordinate of an id {{Layer, Unique}, Type}.
-spec layer_of(term()) -> number().
layer_of({{Layer, _Unique}, _Type}) when is_number(Layer) -> Layer.

%% @private Link actuators to their input neurons.
-spec link_actuators(#exoself_state{}, #agent{}, pid()) -> ok.
link_actuators(State, Agent, CortexPid) ->
    #exoself_state{id_to_process_map = IdMap} = State,
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    lists:foreach(
        fun(ActuatorId) ->
            Actuator = genotype:dirty_read({actuator, ActuatorId}),
            [{ActuatorId, ActuatorPid}] = ets:lookup(IdMap, ActuatorId),
            FaninPids = [
                begin
                    [{Id, Pid}] = ets:lookup(IdMap, Id),
                    Pid
                end
                || Id <- Actuator#actuator.fanin_ids
            ],
            ActuatorPid ! {link, fanin_pids, FaninPids},
            %% Re-point the actuator at the real cortex. It was spawned with
            %% the exoself as a placeholder cortex_pid, because the cortex did
            %% not exist yet.
            ActuatorPid ! {link, cortex_pid, CortexPid}
        end,
        Cortex#cortex.actuator_ids
    ).

%% @private Main loop handling evaluation results and tuning.
-spec loop(#exoself_state{}) -> ok.
loop(State) ->
    #exoself_state{
        agent_id = AgentId,
        cortex_pid = _CortexPid,
        population_monitor_pid = PopMonitorPid,
        highest_fitness = HighestFitness,
        current_attempt = _CurrentAttempt,
        max_attempts = _MaxAttempts,
        evaluation_count = EvalCount
    } = State,

    receive
        %% Fitness channel: the cortex now reports the scape's accumulated
        %% fitness and halt flag alongside the outputs.
        {cortex, _CxId, evaluation_complete, _Outputs, Fitness, HaltFlag} ->
            handle_evaluation_complete(Fitness, HaltFlag, State);

        %% No scape attached: outputs only, no fitness available.
        {cortex, _CxId, evaluation_complete, Fitness} ->
            handle_evaluation_complete(Fitness, 0, State);

        {cortex, _CxId, max_cycles_reached, _CycleCount} ->
            %% Evaluation complete, report to population monitor
            report_and_terminate(PopMonitorPid, AgentId, EvalCount, HighestFitness, State);

        {population_monitor, terminate} ->
            terminate_network(State);

        terminate ->
            terminate_network(State);

        {population_monitor, continue} ->
            %% Continue with next tuning attempt
            handle_continue(State)
    end.

%% @private Report the best fitness to the population monitor (if any) and
%% tear down the phenotype.
-spec report_and_terminate(pid() | undefined, term(), non_neg_integer(),
                           number() | undefined, #exoself_state{}) -> ok.
report_and_terminate(PopMonitorPid, AgentId, EvalCount, HighestFitness, State) ->
    case PopMonitorPid of
        undefined ->
            ok;
        _ ->
            PopMonitorPid ! {self(), AgentId, EvalCount, HighestFitness}
    end,
    terminate_network(State).

%% @private Run the next tuning attempt, or finish when max attempts reached.
-spec handle_continue(#exoself_state{}) -> ok.
handle_continue(State) ->
    #exoself_state{
        agent_id = AgentId,
        cortex_pid = CortexPid,
        population_monitor_pid = PopMonitorPid,
        highest_fitness = HighestFitness,
        current_attempt = CurrentAttempt,
        max_attempts = MaxAttempts,
        evaluation_count = EvalCount
    } = State,
    case CurrentAttempt < MaxAttempts of
        true ->
            reset_neurons(State),
            perturb_weights(State),
            cortex:sync(CortexPid),
            loop(State#exoself_state{current_attempt = CurrentAttempt + 1});
        false ->
            %% Max attempts reached
            report_and_terminate(PopMonitorPid, AgentId, EvalCount, HighestFitness, State)
    end.

%% @private Handle evaluation completion.
-spec handle_evaluation_complete(number() | [number()], term(), #exoself_state{}) -> ok.
handle_evaluation_complete(Fitness, HaltFlag, State) ->
    _ = maybe_report_goal_reached(HaltFlag, State),
    #exoself_state{
        cortex_pid = CortexPid,
        highest_fitness = HighestFitness,
        current_attempt = CurrentAttempt,
        max_attempts = MaxAttempts,
        evaluation_count = EvalCount
    } = State,

    %% Fitness now arrives from the scape via the cortex.
    %%
    %% This previously read `lists:sum(Fitness)' over the network's own OUTPUT
    %% vector, because no scape existed to supply a fitness value and one had
    %% to be invented. Hill-climbing on that rewards emitting large numbers,
    %% not solving the task. A list is still summed for the no-scape case,
    %% where the cortex passes outputs through.
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
            reset_scapes(NewState),
            reset_neurons(NewState),
            perturb_weights(NewState),
            cortex:sync(CortexPid),
            loop(NewState#exoself_state{current_attempt = CurrentAttempt + 1});
        false ->
            %% Tuning complete for this agent.
            %%
            %% This branch previously reset the attempt counter and looped
            %% FOREVER, sending nothing to the caller. That is why every agent
            %% timed out at 5s with fitness [0.0] and no evolutionary run ever
            %% completed (insight 001). It now finishes the agent: tear down
            %% the phenotype and report the best fitness found to the process
            %% that spawned it.
            finish(NewState)
    end.

%% @private Report the agent's result and terminate its phenotype.
finish(State) ->
    #exoself_state{caller_pid = CallerPid,
                   highest_fitness = HighestFitness,
                   evaluation_count = EvalCount} = State,
    Fitness = case HighestFitness of
                  undefined -> 0.0;
                  F -> F
              end,
    terminate_network(State),
    _ = case CallerPid of
            undefined -> ok;
            %% Report the fitness AND how many scape evaluations this agent
            %% cost, so the population can total them. The memetic tuner runs
            %% many evaluations per agent, and an honest evaluations-to-solve
            %% figure (Sher's Table 14.1 metric) must count every one.
            _ -> CallerPid ! {exoself_terminated, Fitness, EvalCount}
        end,
    ok.

%% @private Reset every scape to fresh state before the next evaluation.
%%
%% A stateful scape (XOR walks through four cases) must start each evaluation
%% clean. xor_sim happens to self-reset on its final case, but relying on that
%% would break the moment an evaluation is cut short, so reset explicitly.
reset_scapes(#exoself_state{private_scape_pids = Pids}) ->
    [ Pid ! {self(), reset} || Pid <- Pids ],
    ok.

%% @private Reset all neurons between evaluation attempts, two-phase so recurrent
%% seeds are never lost to a flush.
%%
%% The memetic tuner re-evaluates the SAME neuron processes many times per
%% generation. Without a reset, a recurrent neuron waits on a feedback signal
%% that the previous attempt's final cycle may not have produced, hits its 10s
%% input_timeout, and the run grinds to a halt (every recurrent agent stalled for
%% seconds per attempt). LTC neurons likewise carry stale internal_state.
%%
%% Phase 1 (reset_prep): every neuron flushes any stale forward signals from its
%% mailbox, clears its input accumulator, and acks. Phase 2 (reset), sent only
%% after ALL neurons have flushed, re-seeds recurrent targets with [0.0] and
%% zeroes LTC internal_state. The barrier guarantees no neuron's fresh seed is
%% flushed by a neuron that had not yet reached phase 1.
reset_neurons(#exoself_state{neuron_pids = NeuronPids}) ->
    Self = self(),
    [ Pid ! {reset_prep, Self} || Pid <- NeuronPids ],
    gather_neuron_ready(length(NeuronPids)),
    [ Pid ! reset || Pid <- NeuronPids ],
    ok.

gather_neuron_ready(0) -> ok;
gather_neuron_ready(N) ->
    receive
        {neuron_reset_ready, _Pid} -> gather_neuron_ready(N - 1)
    after 5000 -> ok  %% a dead/stuck neuron must not hang the whole agent
    end.

%% @private Perturb weights using simulated annealing.
-spec perturb_weights(#exoself_state{}) -> ok.
perturb_weights(State) ->
    #exoself_state{
        perturbation_range = Range,
        annealing_parameter = AnnealingParam,
        tuning_selection_function = SelectionF,
        neuron_ids = NeuronIds,
        generation = Generation,
        id_to_process_map = IdMap
    } = State,

    %% Select which neurons to perturb, and by how much, via the agent's
    %% tuning_selection function. dynamic/dynamic_random perturb a subset
    %% biased toward recently-changed neurons, each with its own age-annealed
    %% spread, rather than jittering every neuron every attempt (insight 013).
    Strategy = case SelectionF of undefined -> all; S -> S end,
    Chosen = tuning_selection:select(Strategy, NeuronIds, Generation, Range, AnnealingParam),
    lists:foreach(
        fun({NeuronId, Spread}) ->
            perturb_neuron(IdMap, NeuronId, Spread)
        end,
        Chosen),
    ok.

%% @private Send a perturb message to a neuron's process, if it is registered.
-spec perturb_neuron(ets:tid(), term(), float()) -> ok | {perturb, float()}.
perturb_neuron(IdMap, NeuronId, Spread) ->
    case ets:lookup(IdMap, NeuronId) of
        [{NeuronId, Pid}] -> Pid ! {perturb, Spread};
        [] -> ok
    end.

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
        private_scape_pids = ScapePids,
        id_to_process_map = IdMap
    } = State,

    %% Terminate scapes first, before the cortex, matching DXNN2's teardown
    %% order. A scape outliving its network would leak a process per agent.
    [ Pid ! {self(), terminate} || Pid <- ScapePids ],

    %% Terminate cortex (which will terminate sensors, neurons, actuators)
    case CortexPid of
        undefined ->
            ok;
        _ ->
            await_cortex_termination(CortexPid)
    end,

    %% Clean up ETS table
    ets:delete(IdMap),
    ok.

%% @private Terminate the cortex and wait (bounded) for it to go down.
-spec await_cortex_termination(pid()) -> ok.
await_cortex_termination(CortexPid) ->
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
    end.

%% @doc Calculate perturbation for current attempt using annealing.
%%
%% Exported for testing.
-spec calculate_perturbation(float(), pos_integer(), pos_integer(), float()) -> float().
calculate_perturbation(InitialRange, Attempt, MaxAttempts, AnnealingParam) ->
    Progress = Attempt / MaxAttempts,
    InitialRange * math:pow(1 - Progress, AnnealingParam).
