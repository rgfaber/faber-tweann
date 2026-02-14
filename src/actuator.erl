%% @doc Actuator process for TWEANN networks.
%%
%% Actuators are the output interface of a neural network. They receive
%% signals from neurons and produce actions or outputs that affect the
%% environment or are used for fitness evaluation.
%%
%% == Actuator Lifecycle ==
%%
%% 1. Spawned by cortex with configuration
%% 2. Waits for signals from input neurons
%% 3. Accumulates all inputs
%% 4. Calls actuator function to produce output
%% 5. Reports output to cortex
%% 6. Repeats from step 2
%%
%% == Actuator Functions ==
%%
%% Actuator functions determine how the neural output is used.
%% Common examples:
%%
%% - `pts' - Pass-through sum (for testing)
%% - `xor_output' - XOR problem output
%% - `pole_output' - Pole balancing action
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(actuator).

-export([
    start_link/1,
    init/1
]).

-record(state, {
    id :: term(),
    cortex_pid :: pid(),
    network_id :: term(),
    actuator_name :: atom(),
    vector_length :: pos_integer(),
    fanin_pids :: [pid()],
    scape_pid :: pid() | undefined,
    parameters :: list(),
    acc_input :: #{pid() => [float()]},
    expected_inputs :: non_neg_integer(),
    %% Event-driven mode: use network_pubsub for lifecycle events
    event_driven :: boolean()
}).

%% @doc Start an actuator process.
%%
%% Options:
%% - `id' - Unique identifier for this actuator
%% - `cortex_pid' - PID of the controlling cortex
%% - `actuator_name' - Atom naming the actuator function
%% - `vector_length' - Expected length of input vector
%% - `fanin_pids' - List of PIDs that send input to this actuator
%% - `scape_pid' - PID of the scape/environment (optional)
%% - `parameters' - Additional parameters for actuator function
-spec start_link(map()) -> {ok, pid()}.
start_link(Opts) ->
    Pid = spawn_link(?MODULE, init, [Opts]),
    {ok, Pid}.

%% @doc Initialize the actuator and enter the main loop.
-spec init(map()) -> no_return().
init(Opts) ->
    Id = maps:get(id, Opts),
    CortexPid = maps:get(cortex_pid, Opts),
    NetworkId = maps:get(network_id, Opts, Id),
    ActuatorName = maps:get(actuator_name, Opts),
    VectorLength = maps:get(vector_length, Opts, 1),
    FaninPids = maps:get(fanin_pids, Opts, []),
    ScapePid = maps:get(scape_pid, Opts, undefined),
    Parameters = maps:get(parameters, Opts, []),
    EventDriven = maps:get(event_driven, Opts, false),

    %% Subscribe to network events if event-driven mode enabled
    case EventDriven of
        true ->
            network_pubsub:subscribe(NetworkId, network_terminating);
        false ->
            ok
    end,

    State = #state{
        id = Id,
        cortex_pid = CortexPid,
        network_id = NetworkId,
        actuator_name = ActuatorName,
        vector_length = VectorLength,
        fanin_pids = FaninPids,
        scape_pid = ScapePid,
        parameters = Parameters,
        acc_input = #{},
        expected_inputs = length(FaninPids),
        event_driven = EventDriven
    },

    loop(State).

%% Internal functions

loop(State) ->
    receive
        {forward, FromPid, Signal} ->
            NewState = handle_forward(FromPid, Signal, State),
            loop(NewState);

        %% Legacy: direct terminate from cortex
        {cortex, terminate} ->
            handle_cleanup(State),
            ok;

        %% Event-driven: network terminating via pubsub
        {network_event, network_terminating, _Data} ->
            handle_cleanup(State),
            ok;

        {link, fanin_pids, FaninPids} ->
            %% Dynamic linking from constructor
            NewState = State#state{
                fanin_pids = FaninPids,
                expected_inputs = length(FaninPids)
            },
            loop(NewState);

        %% Catch-all: log and discard unexpected messages to prevent mailbox bloat
        UnexpectedMsg ->
            tweann_logger:warning("Actuator ~p received unexpected message: ~p",
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

handle_forward(FromPid, Signal, State) ->
    #state{
        acc_input = AccInput,
        expected_inputs = ExpectedInputs
    } = State,

    %% Accumulate the signal
    NewAccInput = maps:put(FromPid, Signal, AccInput),
    ReceivedCount = maps:size(NewAccInput),

    %% Check if we have all inputs
    case ReceivedCount >= ExpectedInputs of
        true ->
            process_and_report(State#state{acc_input = NewAccInput});
        false ->
            State#state{acc_input = NewAccInput}
    end.

process_and_report(State) ->
    #state{
        id = Id,
        cortex_pid = CortexPid,
        network_id = NetworkId,
        actuator_name = ActuatorName,
        fanin_pids = FaninPids,
        scape_pid = ScapePid,
        parameters = Parameters,
        acc_input = AccInput,
        event_driven = EventDriven
    } = State,

    %% Build input vector in correct order
    Input = build_input(FaninPids, AccInput),

    %% Process through actuator function
    Output = actuate(ActuatorName, Input, ScapePid, Parameters),

    case EventDriven of
        true ->
            %% Event-driven: publish actuator_output_ready
            network_pubsub:publish(NetworkId, actuator_output_ready, #{
                from => self(),
                actuator_id => Id,
                output => Output
            });
        false ->
            %% Legacy: direct message to cortex
            CortexPid ! {actuator_output, self(), Output}
    end,

    %% Reset accumulated inputs
    State#state{acc_input = #{}}.

build_input(FaninPids, AccInput) ->
    lists:flatten([maps:get(Pid, AccInput, [0.0]) || Pid <- FaninPids]).

%% Built-in actuator functions

actuate(pts, Input, _ScapePid, _Parameters) ->
    %% Pass-through sum - just return the input as-is
    Input;

actuate(identity, Input, _ScapePid, _Parameters) ->
    %% Identity - same as pts
    Input;

actuate(threshold, Input, _ScapePid, Parameters) ->
    %% Binary threshold - convert to 0/1 based on threshold
    Threshold = proplists:get_value(threshold, Parameters, 0.0),
    [if V > Threshold -> 1.0; true -> 0.0 end || V <- Input];

actuate(softmax, Input, _ScapePid, _Parameters) ->
    %% Softmax - convert to probability distribution
    Max = lists:max(Input),
    Exps = [math:exp(V - Max) || V <- Input],
    Sum = lists:sum(Exps),
    [E / Sum || E <- Exps];

actuate(argmax, Input, _ScapePid, _Parameters) ->
    %% Argmax - return index of maximum value (0-indexed)
    {_Max, Index, _} = lists:foldl(
        fun(V, {MaxV, MaxI, I}) ->
            case V > MaxV of
                true -> {V, I, I + 1};
                false -> {MaxV, MaxI, I + 1}
            end
        end,
        {hd(Input), 0, 0},
        Input
    ),
    [float(Index)];

actuate(scape, Input, ScapePid, Parameters) ->
    %% Send to scape for evaluation
    case ScapePid of
        undefined ->
            Input;
        _ ->
            ScapePid ! {self(), actuate, Input, Parameters},
            receive
                {ScapePid, result, Result} ->
                    Result
            after 5000 ->
                Input
            end
    end;

actuate(_ActuatorName, Input, _ScapePid, _Parameters) ->
    %% Default: pass through
    Input.
