%% @doc Sensor process for TWEANN networks.
%%
%% Sensors are the input interface of a neural network. They read data
%% from the environment or problem domain and forward it to connected
%% neurons. Each sensor has a specific function that determines what
%% data it produces.
%%
%% == Sensor Lifecycle ==
%%
%% 1. Spawned by cortex with configuration
%% 2. Waits for sync signal from cortex
%% 3. Calls sensor function to get input data
%% 4. Forwards data to all connected neurons
%% 5. Repeats from step 2
%%
%% == Sensor Functions ==
%%
%% Sensor functions are atoms that map to actual functions in the
%% problem-specific module. Common examples:
%%
%% - `rng' - Random number generator (for testing)
%% - `xor_input' - XOR problem input
%% - `pole_input' - Pole balancing input
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(sensor).

-export([
    start_link/1,
    init/1
]).

-record(state, {
    id :: term(),
    cortex_pid :: pid(),
    network_id :: term(),
    sensor_name :: atom(),
    vector_length :: pos_integer(),
    fanout_pids :: [pid()],
    scape_pid :: pid() | undefined,
    parameters :: list(),
    %% Event-driven mode: use network_pubsub instead of direct messages
    event_driven :: boolean()
}).

%% @doc Start a sensor process.
%%
%% Options:
%% - `id' - Unique identifier for this sensor
%% - `cortex_pid' - PID of the controlling cortex
%% - `sensor_name' - Atom naming the sensor function
%% - `vector_length' - Length of output vector
%% - `fanout_pids' - List of PIDs to forward output to
%% - `scape_pid' - PID of the scape/environment (optional)
%% - `parameters' - Additional parameters for sensor function
-spec start_link(map()) -> {ok, pid()}.
start_link(Opts) ->
    Pid = spawn_link(?MODULE, init, [Opts]),
    {ok, Pid}.

%% @doc Initialize the sensor and enter the main loop.
-spec init(map()) -> no_return().
init(Opts) ->
    Id = maps:get(id, Opts),
    CortexPid = maps:get(cortex_pid, Opts),
    NetworkId = maps:get(network_id, Opts, Id),
    SensorName = maps:get(sensor_name, Opts),
    VectorLength = maps:get(vector_length, Opts, 1),
    FanoutPids = maps:get(fanout_pids, Opts, []),
    ScapePid = maps:get(scape_pid, Opts, undefined),
    Parameters = maps:get(parameters, Opts, []),
    EventDriven = maps:get(event_driven, Opts, false),

    %% Subscribe to network events if event-driven mode enabled
    case EventDriven of
        true ->
            network_pubsub:subscribe(NetworkId, evaluation_cycle_started),
            network_pubsub:subscribe(NetworkId, network_terminating);
        false ->
            ok
    end,

    State = #state{
        id = Id,
        cortex_pid = CortexPid,
        network_id = NetworkId,
        sensor_name = SensorName,
        vector_length = VectorLength,
        fanout_pids = FanoutPids,
        scape_pid = ScapePid,
        parameters = Parameters,
        event_driven = EventDriven
    },

    loop(State).

%% Internal functions

loop(State) ->
    receive
        %% Legacy: direct message from cortex
        {cortex, sync} ->
            handle_sync(State),
            loop(State);

        %% Event-driven: evaluation cycle started via pubsub
        {network_event, evaluation_cycle_started, _Data} ->
            handle_sync(State),
            loop(State);

        %% Legacy: direct terminate from cortex
        {cortex, terminate} ->
            handle_cleanup(State),
            ok;

        %% Event-driven: network terminating via pubsub
        {network_event, network_terminating, _Data} ->
            handle_cleanup(State),
            ok;

        {scape, Signal} ->
            %% Scape provides the signal directly
            handle_scape_signal(Signal, State),
            loop(State);

        {link, fanout_pids, FanoutPids} ->
            %% Dynamic linking from constructor
            loop(State#state{fanout_pids = FanoutPids});

        %% Catch-all: log and discard unexpected messages to prevent mailbox bloat
        UnexpectedMsg ->
            tweann_logger:warning("Sensor ~p received unexpected message: ~p",
                                 [State#state.id, UnexpectedMsg]),
            loop(State)
    end.

%% @private Cleanup on termination
handle_cleanup(State) ->
    case State#state.event_driven of
        true ->
            network_pubsub:cleanup(State#state.network_id);
        false ->
            ok
    end.

handle_sync(State) ->
    #state{
        id = Id,
        network_id = NetworkId,
        sensor_name = SensorName,
        vector_length = VectorLength,
        fanout_pids = FanoutPids,
        scape_pid = ScapePid,
        parameters = Parameters,
        event_driven = EventDriven
    } = State,

    %% Get sensor output
    Signal = case ScapePid of
        undefined ->
            %% Use built-in sensor function
            sense(SensorName, VectorLength, Parameters);
        _ ->
            %% Request from scape
            ScapePid ! {self(), sense, SensorName, Parameters},
            receive
                {ScapePid, sensory_signal, SensorySignal} ->
                    SensorySignal
            after 5000 ->
                %% Timeout - return zeros
                lists:duplicate(VectorLength, 0.0)
            end
    end,

    case EventDriven of
        true ->
            %% Event-driven: publish sensor_output_ready
            network_pubsub:publish(NetworkId, sensor_output_ready, #{
                from => self(),
                sensor_id => Id,
                signal => Signal
            });
        false ->
            %% Legacy: direct message to neurons
            lists:foreach(
                fun(NeuronPid) ->
                    NeuronPid ! {forward, self(), Signal}
                end,
                FanoutPids
            )
    end.

handle_scape_signal(Signal, State) ->
    #state{fanout_pids = FanoutPids} = State,

    %% Forward scape signal to all connected neurons
    lists:foreach(
        fun(NeuronPid) ->
            NeuronPid ! {forward, self(), Signal}
        end,
        FanoutPids
    ).

%% Built-in sensor functions

sense(rng, VectorLength, _Parameters) ->
    %% Random number generator - useful for testing
    [rand:uniform() * 2 - 1 || _ <- lists:seq(1, VectorLength)];

sense(ones, VectorLength, _Parameters) ->
    %% All ones - useful for bias-like behavior
    lists:duplicate(VectorLength, 1.0);

sense(zeros, VectorLength, _Parameters) ->
    %% All zeros
    lists:duplicate(VectorLength, 0.0);

sense(counter, VectorLength, _Parameters) ->
    %% Counter - returns [1.0, 2.0, ..., N]
    [float(I) / VectorLength || I <- lists:seq(1, VectorLength)];

sense(step, VectorLength, Parameters) ->
    %% Step function with configurable step value
    Step = proplists:get_value(step, Parameters, 0.1),
    %% Get current value from process dictionary
    Current = case get(step_value) of
        undefined -> 0.0;
        V -> V
    end,
    NewValue = Current + Step,
    put(step_value, NewValue),
    lists:duplicate(VectorLength, NewValue);

sense(_SensorName, VectorLength, _Parameters) ->
    %% Default: return zeros for unknown sensor
    lists:duplicate(VectorLength, 0.0).
