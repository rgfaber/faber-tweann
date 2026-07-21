%% @doc Neural network coordinator for TWEANN networks.
%%
%% The cortex is the central coordinator of a neural network. It manages
%% the lifecycle of all neurons, sensors, and actuators, and orchestrates
%% the signal flow through the network during evaluation cycles.
%%
%% == Cortex Lifecycle ==
%%
%% 1. Spawned with network topology information
%% 2. Spawns all sensors, neurons, and actuators
%% 3. Initiates evaluation cycles by triggering sensors
%% 4. Waits for all actuators to complete
%% 5. Collects fitness from actuators
%% 6. Reports fitness or initiates next cycle
%% 7. Handles backup requests for weight storage
%%
%% == Evaluation Cycle ==
%%
%% 1. Cortex sends `sync' to all sensors
%% 2. Sensors read input and forward to neurons
%% 3. Neurons process and forward through network
%% 4. Actuators collect outputs and report to cortex
%% 5. Cortex aggregates results
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(cortex).

-export([
    start_link/1,
    init/1,
    sync/1,
    backup/1,
    terminate/1
]).

-record(state, {
    id :: term(),
    exoself_pid :: pid() | undefined,
    sensor_pids :: [pid()],
    neuron_pids :: [pid()],
    actuator_pids :: [pid()],
    cycle_acc :: [float()],
    %% Fitness channel accumulators. Fitness sums across actuators and cycles;
    %% halt_flag latches the strongest signal seen (goal_reached > 1 > 0).
    fitness_acc = 0.0 :: float(),
    halt_flag = 0 :: 0 | 1 | goal_reached,
    expected_actuators :: non_neg_integer(),
    cycle_count :: non_neg_integer(),
    max_cycles :: non_neg_integer() | infinity,
    sync_timeout :: pos_integer(),
    %% Event-driven mode: use network_pubsub instead of direct messages
    event_driven :: boolean()
}).

%% Default timeout for waiting on actuator outputs (30 seconds)
-define(DEFAULT_SYNC_TIMEOUT, 30000).

%% @doc Start a cortex process.
%%
%% Options:
%% - `id' - Unique identifier for this cortex
%% - `exoself_pid' - PID to report results to (optional)
%% - `sensor_pids' - List of sensor PIDs
%% - `neuron_pids' - List of neuron PIDs
%% - `actuator_pids' - List of actuator PIDs
%% - `max_cycles' - Maximum evaluation cycles (default: infinity)
-spec start_link(map()) -> {ok, pid()}.
start_link(Opts) ->
    Pid = spawn_link(?MODULE, init, [Opts]),
    {ok, Pid}.

%% @doc Initialize the cortex and enter the main loop.
-spec init(map()) -> no_return().
init(Opts) ->
    Id = maps:get(id, Opts),
    ExoselfPid = maps:get(exoself_pid, Opts, undefined),
    SensorPids = maps:get(sensor_pids, Opts, []),
    NeuronPids = maps:get(neuron_pids, Opts, []),
    ActuatorPids = maps:get(actuator_pids, Opts, []),
    MaxCycles = maps:get(max_cycles, Opts, infinity),
    SyncTimeout = maps:get(sync_timeout, Opts, ?DEFAULT_SYNC_TIMEOUT),
    EventDriven = maps:get(event_driven, Opts, false),

    %% Initialize network pubsub if event-driven mode enabled
    case EventDriven of
        true ->
            network_pubsub:init(Id),
            %% Subscribe to actuator output events
            network_pubsub:subscribe(Id, actuator_output_ready);
        false ->
            ok
    end,

    State = #state{
        id = Id,
        exoself_pid = ExoselfPid,
        sensor_pids = SensorPids,
        neuron_pids = NeuronPids,
        actuator_pids = ActuatorPids,
        cycle_acc = [],
        expected_actuators = length(ActuatorPids),
        cycle_count = 0,
        max_cycles = MaxCycles,
        sync_timeout = SyncTimeout,
        event_driven = EventDriven
    },

    loop(State).

%% @doc Trigger a synchronization cycle.
%%
%% This starts an evaluation cycle by signaling all sensors.
-spec sync(pid()) -> ok.
sync(CortexPid) ->
    CortexPid ! sync,
    ok.

%% @doc Request backup of all neuron weights.
%%
%% The cortex will collect weights from all neurons and send them
%% to the exoself for storage.
-spec backup(pid()) -> ok.
backup(CortexPid) ->
    CortexPid ! backup,
    ok.

%% @doc Terminate the cortex and all its components.
-spec terminate(pid()) -> ok.
terminate(CortexPid) ->
    CortexPid ! terminate,
    ok.

%% Internal functions

loop(State) ->
    Timeout = State#state.sync_timeout,
    receive
        sync ->
            NewState = handle_sync(State),
            loop(NewState);

        %% Direct message (legacy/non-event-driven mode)
        %% Fitness channel: the scape's fitness and halt flag ride along with
        %% the actuator's output. Before this existed the cortex had no notion
        %% of fitness at all and exoself invented one.
        {actuator_output, _ActuatorPid, Output, Fitness, HaltFlag} ->
            NewState = handle_actuator_output(Output, Fitness, HaltFlag, State),
            loop(NewState);

        %% Without a scape attached there is no fitness to carry.
        {actuator_output, _ActuatorPid, Output} ->
            NewState = handle_actuator_output(Output, 0.0, 0, State),
            loop(NewState);

        %% Event-driven: actuator output via pubsub
        {network_event, actuator_output_ready, Ev} when is_map(Ev) ->
            NewState = handle_actuator_output(
                maps:get(output, Ev),
                maps:get(fitness, Ev, 0.0),
                maps:get(halt_flag, Ev, 0),
                State),
            loop(NewState);

        backup ->
            handle_backup(State),
            loop(State);

        {backup, NeuronId, Weights, Bias} ->
            _ = handle_neuron_backup(NeuronId, Weights, Bias, State),
            loop(State);

        %% Event-driven: weights backed up via pubsub
        {network_event, weights_backed_up, #{neuron_id := NeuronId, weights := Weights, bias := Bias}} ->
            _ = handle_neuron_backup(NeuronId, Weights, Bias, State),
            loop(State);

        terminate ->
            handle_terminate(State),
            ok;

        {exoself, stop} ->
            handle_terminate(State),
            ok;

        {neuron_timeout, NeuronPid, NeuronId} ->
            %% Handle neuron timeout - remove from tracking and log
            tweann_logger:warning("Cortex ~p: Neuron ~p (~p) timed out",
                                 [State#state.id, NeuronId, NeuronPid]),
            loop(State);

        %% Catch-all: log and discard unexpected messages to prevent mailbox bloat
        UnexpectedMsg ->
            tweann_logger:warning("Cortex ~p received unexpected message: ~p",
                                 [State#state.id, UnexpectedMsg]),
            loop(State)
    after Timeout ->
        handle_timeout(State)
    end.

%% @private Handle receive timeout
handle_timeout(State) ->
    tweann_logger:warning("Cortex ~p sync timeout after ~pms",
                         [State#state.id, State#state.sync_timeout]),
    case State#state.exoself_pid of
        undefined ->
            ok;
        ExoselfPid ->
            _ = ExoselfPid ! {cortex, State#state.id, evaluation_timeout},
            ok
    end.

handle_sync(State) ->
    #state{
        id = Id,
        sensor_pids = SensorPids,
        cycle_count = CycleCount,
        event_driven = EventDriven
    } = State,

    NewCycleCount = CycleCount + 1,

    case EventDriven of
        true ->
            %% Event-driven: publish evaluation_cycle_started
            network_pubsub:publish(Id, evaluation_cycle_started, #{
                cycle => NewCycleCount,
                cortex_pid => self()
            });
        false ->
            %% Legacy: direct message to sensors
            sync_sensors(SensorPids)
    end,

    %% Reset cycle accumulator
    State#state{
        cycle_acc = [],
        cycle_count = NewCycleCount
    }.

%% @private Signal each sensor to begin a cycle (legacy direct-message mode).
sync_sensors(SensorPids) ->
    lists:foreach(
        fun(SensorPid) ->
            SensorPid ! {cortex, sync}
        end,
        SensorPids
    ).

handle_actuator_output(Output, Fitness, HaltFlag, State) ->
    #state{
        cycle_acc = CycleAcc,
        fitness_acc = FitnessAcc,
        halt_flag = HaltAcc,
        expected_actuators = ExpectedActuators,
        cycle_count = CycleCount,
        max_cycles = MaxCycles
    } = State,

    %% Accumulate output and fitness.
    %%
    %% Fitness sums across actuators and across cycles, as in DXNN2: a scape
    %% using lifetime-based fitness returns 0.0 until it halts, so the sum is
    %% the final value. A scape awarding per-step fitness accumulates.
    NewCycleAcc = [Output | CycleAcc],
    NewFitnessAcc = FitnessAcc + Fitness,
    %% goal_reached dominates a plain halt: any actuator reporting the task
    %% solved must not be masked by another reporting an ordinary stop.
    NewHalt = merge_halt(HaltAcc, HaltFlag),
    ReceivedCount = length(NewCycleAcc),

    %% Check if all actuators have reported
    case ReceivedCount >= ExpectedActuators of
        true ->
            %% All actuators for this cycle have reported. Whether the
            %% EVALUATION is finished is the scape's call, via the halt flag.
            %%
            %% A scape presents an evaluation as several sense-think-act
            %% cycles (XOR is four, one per case) and returns halt only on the
            %% last. Until then the cortex must run another cycle, not report.
            %% Reporting after a single cycle was why nothing worked: each
            %% "evaluation" saw only the first case and the scape, reset every
            %% attempt, never advanced.
            %%
            %% This uses the halt flag added with the fitness channel; the
            %% message protocol is unchanged. It is DXNN2's cortex loop:
            %% cycle until halt, accumulate fitness, then report once.
            cortex_cycle_decision(NewHalt, NewCycleAcc, NewFitnessAcc,
                                  CycleCount, MaxCycles, State);
        false ->
            State#state{cycle_acc = NewCycleAcc,
                        fitness_acc = NewFitnessAcc,
                        halt_flag = NewHalt}
    end.

%% @private Decide whether the evaluation continues or is complete.
cortex_cycle_decision(0, _CycleAcc, FitnessAcc, CycleCount, MaxCycles, State)
  when MaxCycles =:= infinity; CycleCount < MaxCycles ->
    %% Scape says continue: run another sense-think-act cycle. Keep the
    %% accumulated fitness, discard this cycle's outputs, re-trigger sensors.
    trigger_sensors(State),
    State#state{cycle_acc = [],
                fitness_acc = FitnessAcc,
                halt_flag = 0,
                cycle_count = CycleCount + 1};
cortex_cycle_decision(Halt, CycleAcc, FitnessAcc, CycleCount, _MaxCycles, State) ->
    %% Evaluation complete: either the scape halted (Halt =/= 0) or the cycle
    %% cap was reached with no natural halt. Report the accumulated fitness
    %% once, then reset for the exoself's next evaluation.
    #state{id = Id, exoself_pid = ExoselfPid} = State,
    Outputs = lists:flatten(lists:reverse(CycleAcc)),
    _ = case ExoselfPid of
            undefined -> ok;
            _ -> ExoselfPid ! {cortex, Id, evaluation_complete,
                               Outputs, FitnessAcc, Halt}
        end,
    %% If the report was forced by the cycle cap rather than a scape halt,
    %% signal it so the exoself can distinguish a genuine solution from a
    %% run that simply ran out of cycles. Halt =:= 0 here means "the scape
    %% never said stop, we hit the ceiling".
    _ = case {Halt, ExoselfPid} of
            {0, P} when is_pid(P) ->
                P ! {cortex, Id, max_cycles_reached, CycleCount};
            _ ->
                ok
        end,
    State#state{cycle_acc = [], fitness_acc = 0.0, halt_flag = 0}.

%% @private Trigger all sensors to begin another sense-think-act cycle.
trigger_sensors(#state{sensor_pids = SensorPids, event_driven = false}) ->
    [ SensorPid ! {cortex, sync} || SensorPid <- SensorPids ],
    ok;
trigger_sensors(#state{id = Id, event_driven = true}) ->
    network_pubsub:publish(Id, evaluation_cycle_started, #{cortex_pid => self()}),
    ok.

%% @private
%% @doc Combine halt flags across actuators. goal_reached wins over a plain
%% halt, which wins over continue.
merge_halt(goal_reached, _) -> goal_reached;
merge_halt(_, goal_reached) -> goal_reached;
merge_halt(1, _) -> 1;
merge_halt(_, 1) -> 1;
merge_halt(_, _) -> 0.

handle_backup(State) ->
    #state{
        id = Id,
        neuron_pids = NeuronPids,
        event_driven = EventDriven
    } = State,

    case EventDriven of
        true ->
            %% Event-driven: publish backup_requested
            network_pubsub:publish(Id, backup_requested, #{
                cortex_pid => self()
            });
        false ->
            %% Legacy: direct message to neurons
            backup_neurons(NeuronPids)
    end.

%% @private Request a weight backup from each neuron (legacy direct-message mode).
backup_neurons(NeuronPids) ->
    lists:foreach(
        fun(NeuronPid) ->
            neuron:backup(NeuronPid)
        end,
        NeuronPids
    ).

handle_neuron_backup(NeuronId, Weights, Bias, State) ->
    #state{
        id = Id,
        exoself_pid = ExoselfPid
    } = State,

    %% Forward to exoself for storage
    case ExoselfPid of
        undefined ->
            ok;
        _ ->
            ExoselfPid ! {cortex, Id, neuron_backup, NeuronId, Weights, Bias}
    end.

handle_terminate(State) ->
    #state{
        id = Id,
        sensor_pids = SensorPids,
        neuron_pids = NeuronPids,
        actuator_pids = ActuatorPids,
        event_driven = EventDriven
    } = State,

    AllPids = SensorPids ++ NeuronPids ++ ActuatorPids,

    %% Monitor all processes before sending terminate
    Monitors = lists:map(
        fun(Pid) ->
            Ref = erlang:monitor(process, Pid),
            {Pid, Ref}
        end,
        AllPids
    ),

    case EventDriven of
        true ->
            %% Event-driven: publish network_terminating
            network_pubsub:publish(Id, network_terminating, #{
                cortex_pid => self()
            }),
            %% Cleanup pubsub subscriptions
            network_pubsub:cleanup(Id);
        false ->
            %% Legacy: direct messages to all components
            terminate_components(SensorPids),
            terminate_components(NeuronPids),
            terminate_components(ActuatorPids)
    end,

    %% Wait for all processes to terminate (with timeout per process)
    wait_for_terminations(Monitors, 2000).

%% @private Send a terminate message to each component PID (legacy mode).
terminate_components(Pids) ->
    lists:foreach(
        fun(Pid) ->
            Pid ! {cortex, terminate}
        end,
        Pids
    ).

%% @private Wait for all monitored processes to terminate
wait_for_terminations([], _Timeout) ->
    ok;
wait_for_terminations([{Pid, Ref} | Rest], Timeout) ->
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            wait_for_terminations(Rest, Timeout)
    after Timeout ->
        %% Timeout waiting for process - demonitor and continue
        %% Don't force-kill to avoid cascading failures from linked processes
        erlang:demonitor(Ref, [flush]),
        tweann_logger:warning("Cortex: process ~p did not terminate within ~pms", [Pid, Timeout]),
        wait_for_terminations(Rest, Timeout)
    end.
