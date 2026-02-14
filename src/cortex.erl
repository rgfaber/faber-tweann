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
        {actuator_output, _ActuatorPid, Output} ->
            NewState = handle_actuator_output(Output, State),
            loop(NewState);

        %% Event-driven: actuator output via pubsub
        {network_event, actuator_output_ready, #{output := Output}} ->
            NewState = handle_actuator_output(Output, State),
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
            lists:foreach(
                fun(SensorPid) ->
                    SensorPid ! {cortex, sync}
                end,
                SensorPids
            )
    end,

    %% Reset cycle accumulator
    State#state{
        cycle_acc = [],
        cycle_count = NewCycleCount
    }.

handle_actuator_output(Output, State) ->
    #state{
        id = Id,
        exoself_pid = ExoselfPid,
        cycle_acc = CycleAcc,
        expected_actuators = ExpectedActuators,
        cycle_count = CycleCount,
        max_cycles = MaxCycles
    } = State,

    %% Accumulate output
    NewCycleAcc = [Output | CycleAcc],
    ReceivedCount = length(NewCycleAcc),

    %% Check if all actuators have reported
    case ReceivedCount >= ExpectedActuators of
        true ->
            %% All outputs received - flatten and report
            Outputs = lists:flatten(lists:reverse(NewCycleAcc)),

            %% Report to exoself if available
            _ = case ExoselfPid of
                undefined ->
                    ok;
                _ ->
                    ExoselfPid ! {cortex, Id, evaluation_complete, Outputs}
            end,

            %% Check if max cycles reached
            case MaxCycles of
                infinity ->
                    State#state{cycle_acc = []};
                N when CycleCount >= N ->
                    %% Max cycles reached - terminate
                    _ = case ExoselfPid of
                        undefined -> ok;
                        _ -> ExoselfPid ! {cortex, Id, max_cycles_reached, CycleCount}
                    end,
                    State#state{cycle_acc = []};
                _ ->
                    State#state{cycle_acc = []}
            end;
        false ->
            State#state{cycle_acc = NewCycleAcc}
    end.

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
            lists:foreach(
                fun(NeuronPid) ->
                    neuron:backup(NeuronPid)
                end,
                NeuronPids
            )
    end.

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
            %% Terminate all sensors
            lists:foreach(
                fun(SensorPid) ->
                    SensorPid ! {cortex, terminate}
                end,
                SensorPids
            ),

            %% Terminate all neurons
            lists:foreach(
                fun(NeuronPid) ->
                    NeuronPid ! {cortex, terminate}
                end,
                NeuronPids
            ),

            %% Terminate all actuators
            lists:foreach(
                fun(ActuatorPid) ->
                    ActuatorPid ! {cortex, terminate}
                end,
                ActuatorPids
            )
    end,

    %% Wait for all processes to terminate (with timeout per process)
    wait_for_terminations(Monitors, 2000).

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
