%% @doc Neural processing unit for TWEANN networks.
%%
%% The neuron is the fundamental processing element in a neural network.
%% It receives signals from sensors or other neurons, aggregates them,
%% applies an activation function, and forwards the result to connected
%% neurons or actuators.
%%
%% == Neuron Lifecycle ==
%%
%% 1. Spawned by cortex with initial state
%% 2. Waits for signals from input connections
%% 3. Aggregates all inputs when complete
%% 4. Applies activation function
%% 5. Forwards output to all output connections
%% 6. Repeats from step 2
%%
%% == State ==
%%
%% The neuron maintains:
%%
%% - Input connections with weights
%% - Output connections (PIDs)
%% - Accumulated input signals
%% - Activation function
%% - Aggregation function
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(neuron).

-export([
    start_link/1,
    init/1,
    forward/3,
    backup/1
]).

-record(state, {
    id :: term(),
    cortex_pid :: pid(),
    network_id :: term(),
    activation_function :: atom(),
    aggregation_function :: atom(),
    input_pids :: [pid()],
    output_pids :: [pid()],
    ro_pids :: [pid()],  % recurrent output PIDs
    input_weights :: #{pid() => [{float(), float(), float(), list()}]},
    bias :: float(),
    acc_input :: #{pid() => [float()]},
    expected_inputs :: non_neg_integer(),
    input_timeout :: pos_integer(),
    timeout_count = 0 :: non_neg_integer(),
    %% Pre-compiled flat weights for NIF acceleration
    %% Format: {FlatWeights :: [float()], Bias :: float()} or undefined
    compiled_weights :: {[float()], float()} | undefined,
    %% Event-driven mode: use network_pubsub for lifecycle events
    event_driven :: boolean()
}).

%% Default timeout for waiting on neuron inputs (10 seconds)
-define(DEFAULT_INPUT_TIMEOUT, 10000).

%% Maximum consecutive timeouts before neuron terminates
-define(MAX_TIMEOUT_COUNT, 3).

%% @doc Start a neuron process.
%%
%% Options:
%% - `id' - Unique identifier for this neuron
%% - `cortex_pid' - PID of the controlling cortex
%% - `activation_function' - Atom naming the activation function (e.g., tanh)
%% - `aggregation_function' - Atom naming the aggregation function (e.g., dot_product)
%% - `input_pids' - List of PIDs that send input to this neuron
%% - `output_pids' - List of PIDs to forward output to
%% - `ro_pids' - List of recurrent output PIDs
%% - `input_weights' - Map of PID to list of weight tuples
%% - `bias' - Bias value for this neuron
-spec start_link(map()) -> {ok, pid()}.
start_link(Opts) ->
    Pid = spawn_link(?MODULE, init, [Opts]),
    {ok, Pid}.

%% @doc Initialize the neuron and enter the main loop.
-spec init(map()) -> no_return().
init(Opts) ->
    Id = maps:get(id, Opts),
    CortexPid = maps:get(cortex_pid, Opts),
    NetworkId = maps:get(network_id, Opts, Id),
    ActivationFn = maps:get(activation_function, Opts, tanh),
    AggregationFn = maps:get(aggregation_function, Opts, dot_product),
    InputPids = maps:get(input_pids, Opts, []),
    OutputPids = maps:get(output_pids, Opts, []),
    RoPids = maps:get(ro_pids, Opts, []),
    InputWeights = maps:get(input_weights, Opts, #{}),
    Bias = maps:get(bias, Opts, 0.0),
    InputTimeout = maps:get(input_timeout, Opts, ?DEFAULT_INPUT_TIMEOUT),
    EventDriven = maps:get(event_driven, Opts, false),

    %% Subscribe to network events if event-driven mode enabled
    case EventDriven of
        true ->
            network_pubsub:subscribe(NetworkId, backup_requested),
            network_pubsub:subscribe(NetworkId, network_terminating);
        false ->
            ok
    end,

    %% Pre-compile weights for NIF acceleration if using dot_product
    CompiledWeights = compile_weights_for_nif(AggregationFn, InputPids, InputWeights, Bias),

    State = #state{
        id = Id,
        cortex_pid = CortexPid,
        network_id = NetworkId,
        activation_function = ActivationFn,
        aggregation_function = AggregationFn,
        input_pids = InputPids,
        output_pids = OutputPids,
        ro_pids = RoPids,
        input_weights = InputWeights,
        bias = Bias,
        acc_input = #{},
        expected_inputs = length(InputPids),
        input_timeout = InputTimeout,
        compiled_weights = CompiledWeights,
        event_driven = EventDriven
    },

    loop(State).

%% @doc Send a signal to a neuron.
%%
%% Called by sensors or other neurons to forward their output.
-spec forward(pid(), pid(), [float()]) -> ok.
forward(NeuronPid, FromPid, Signal) ->
    NeuronPid ! {forward, FromPid, Signal},
    ok.

%% @doc Request the neuron to backup its current weights.
%%
%% The neuron will send its weights to the cortex for storage.
-spec backup(pid()) -> ok.
backup(NeuronPid) ->
    NeuronPid ! backup,
    ok.

%% Internal functions

loop(State) ->
    Timeout = State#state.input_timeout,
    receive
        {forward, FromPid, Signal} ->
            NewState = handle_forward(FromPid, Signal, State),
            loop(NewState);

        %% Legacy: direct backup request
        backup ->
            _ = handle_backup(State),
            loop(State);

        %% Event-driven: backup requested via pubsub
        {network_event, backup_requested, _Data} ->
            _ = handle_backup(State),
            loop(State);

        %% Legacy: direct terminate from cortex
        {cortex, terminate} ->
            handle_cleanup(State),
            ok;

        %% Event-driven: network terminating via pubsub
        {network_event, network_terminating, _Data} ->
            handle_cleanup(State),
            ok;

        {update_weights, NewWeights, NewBias} ->
            %% Recompile weights for NIF after update
            CompiledWeights = compile_weights_for_nif(
                State#state.aggregation_function,
                State#state.input_pids,
                NewWeights,
                NewBias
            ),
            NewState = State#state{
                input_weights = NewWeights,
                bias = NewBias,
                compiled_weights = CompiledWeights
            },
            loop(NewState);

        %% Dynamic linking from constructor
        {link, input_pids, InputPids} ->
            NewState = State#state{
                input_pids = InputPids,
                expected_inputs = length(InputPids)
            },
            loop(NewState);

        {link, output_pids, OutputPids} ->
            loop(State#state{output_pids = OutputPids});

        {link, ro_pids, RoPids} ->
            loop(State#state{ro_pids = RoPids});

        {link, input_weights, InputWeights} ->
            %% Recompile weights for NIF when linking new weights
            CompiledWeights = compile_weights_for_nif(
                State#state.aggregation_function,
                State#state.input_pids,
                InputWeights,
                State#state.bias
            ),
            loop(State#state{
                input_weights = InputWeights,
                compiled_weights = CompiledWeights
            });

        %% Catch-all: log and discard unexpected messages to prevent mailbox bloat
        UnexpectedMsg ->
            tweann_logger:warning("Neuron ~p received unexpected message: ~p",
                                 [State#state.id, UnexpectedMsg]),
            loop(State)
    after Timeout ->
        handle_input_timeout(State)
    end.

%% @private Cleanup on termination
handle_cleanup(State) ->
    case State#state.event_driven of
        true ->
            network_pubsub:cleanup(State#state.network_id);
        false ->
            ok
    end.

%% @private Handle input timeout
handle_input_timeout(State) ->
    MissingInputs = State#state.expected_inputs - maps:size(State#state.acc_input),
    NewTimeoutCount = State#state.timeout_count + 1,
    tweann_logger:warning("Neuron ~p input timeout after ~pms, missing ~p inputs (timeout ~p/~p)",
                         [State#state.id, State#state.input_timeout, MissingInputs,
                          NewTimeoutCount, ?MAX_TIMEOUT_COUNT]),
    %% Exit after MAX_TIMEOUT_COUNT consecutive timeouts to prevent zombie processes
    case NewTimeoutCount >= ?MAX_TIMEOUT_COUNT of
        true ->
            tweann_logger:error("Neuron ~p terminating after ~p consecutive timeouts",
                               [State#state.id, NewTimeoutCount]),
            %% Notify cortex of abnormal termination
            State#state.cortex_pid ! {neuron_timeout, self(), State#state.id},
            ok;
        false ->
            loop(State#state{timeout_count = NewTimeoutCount})
    end.

handle_forward(FromPid, Signal, State) ->
    #state{
        acc_input = AccInput,
        expected_inputs = ExpectedInputs
    } = State,

    %% Accumulate the signal and reset timeout count (input received)
    NewAccInput = maps:put(FromPid, Signal, AccInput),
    ReceivedCount = maps:size(NewAccInput),

    %% Check if we have all inputs
    case ReceivedCount >= ExpectedInputs of
        true ->
            process_and_forward(State#state{acc_input = NewAccInput, timeout_count = 0});
        false ->
            State#state{acc_input = NewAccInput, timeout_count = 0}
    end.

process_and_forward(State) ->
    #state{
        activation_function = ActivationFn,
        aggregation_function = AggregationFn,
        output_pids = OutputPids,
        ro_pids = RoPids,
        input_weights = InputWeights,
        bias = Bias,
        acc_input = AccInput,
        input_pids = InputPids,
        compiled_weights = CompiledWeights
    } = State,

    %% Aggregate inputs (use compiled weights if available for NIF acceleration)
    Aggregated = case {AggregationFn, CompiledWeights} of
        {dot_product, {FlatWeights, CompiledBias}} when is_list(FlatWeights) ->
            %% Fast path: use pre-compiled flat weights with NIF
            FlatSignals = flatten_signals(InputPids, AccInput),
            aggregate_compiled(FlatSignals, FlatWeights, CompiledBias);
        _ ->
            %% Fallback: build inputs/weights and use standard aggregation
            Inputs = build_inputs(InputPids, AccInput),
            Weights = build_weights(InputPids, InputWeights),
            aggregate(AggregationFn, Inputs, Weights) + Bias
    end,

    %% Apply activation
    Output = activate(ActivationFn, Aggregated),

    %% Forward to all output connections
    lists:foreach(
        fun(OutputPid) ->
            OutputPid ! {forward, self(), [Output]}
        end,
        OutputPids
    ),

    %% Forward to recurrent outputs
    lists:foreach(
        fun(RoPid) ->
            RoPid ! {forward, self(), [Output]}
        end,
        RoPids
    ),

    %% Reset accumulated inputs
    State#state{acc_input = #{}}.

build_inputs(InputPids, AccInput) ->
    [{Pid, maps:get(Pid, AccInput, [0.0])} || Pid <- InputPids].

build_weights(InputPids, InputWeights) ->
    [{Pid, maps:get(Pid, InputWeights, [{1.0, 0.0, 0.1, []}])} || Pid <- InputPids].

aggregate(dot_product, Inputs, Weights) ->
    %% Use NIF-accelerated version (falls back to pure Erlang if NIF not loaded)
    signal_aggregator:dot_product_nif(Inputs, Weights);
aggregate(mult_product, Inputs, Weights) ->
    signal_aggregator:mult_product(Inputs, Weights);
aggregate(diff_product, Inputs, Weights) ->
    signal_aggregator:diff_product(Inputs, Weights);
aggregate(Function, Inputs, Weights) ->
    signal_aggregator:Function(Inputs, Weights).

activate(tanh, X) -> functions:tanh(X);
activate(sigmoid, X) -> functions:sigmoid(X);
activate(sigmoid1, X) -> functions:sigmoid1(X);
activate(sin, X) -> functions:sin(X);
activate(cos, X) -> functions:cos(X);
activate(gaussian, X) -> functions:gaussian(X);
activate(linear, X) -> functions:linear(X);
activate(sgn, X) -> functions:sgn(X);
activate(bin, X) -> functions:bin(X);
activate(trinary, X) -> functions:trinary(X);
activate(multiquadric, X) -> functions:multiquadric(X);
activate(quadratic, X) -> functions:quadratic(X);
activate(cubic, X) -> functions:cubic(X);
activate(absolute, X) -> functions:absolute(X);
activate(sqrt, X) -> functions:sqrt(X);
activate(log, X) -> functions:log(X);
activate(relu, X) -> functions:relu(X);
activate(Function, X) -> functions:Function(X).

handle_backup(State) ->
    #state{
        id = Id,
        cortex_pid = CortexPid,
        network_id = NetworkId,
        input_weights = InputWeights,
        bias = Bias,
        event_driven = EventDriven
    } = State,

    case EventDriven of
        true ->
            %% Event-driven: publish weights_backed_up
            network_pubsub:publish(NetworkId, weights_backed_up, #{
                neuron_id => Id,
                weights => InputWeights,
                bias => Bias
            });
        false ->
            %% Legacy: direct message to cortex
            CortexPid ! {backup, Id, InputWeights, Bias}
    end.

%%==============================================================================
%% Pre-compiled Weight Acceleration
%%==============================================================================

%% @private Compile weights to flat format for NIF acceleration.
%%
%% Only compiles for dot_product aggregation when NIF is available.
%% Returns undefined for other aggregation functions.
compile_weights_for_nif(dot_product, InputPids, InputWeights, Bias) ->
    case tweann_nif:is_loaded() of
        true ->
            FlatWeights = flatten_weight_values(InputPids, InputWeights),
            {FlatWeights, Bias};
        false ->
            undefined
    end;
compile_weights_for_nif(_AggregationFn, _InputPids, _InputWeights, _Bias) ->
    undefined.

%% @private Flatten weight values from nested structure to flat list.
%%
%% Converts: #{Pid => [{W, DW, LP, LPs}, ...]} -> [W1, W2, W3, ...]
%% Preserves order based on InputPids ordering.
flatten_weight_values(InputPids, InputWeights) ->
    lists:flatmap(
        fun(Pid) ->
            WeightTuples = maps:get(Pid, InputWeights, [{1.0, 0.0, 0.1, []}]),
            [W || {W, _DW, _LP, _LPs} <- WeightTuples]
        end,
        InputPids
    ).

%% @private Flatten signals from accumulated input map.
%%
%% Converts: #{Pid => [S1, S2, ...]} -> [S1, S2, S3, ...]
%% Preserves order based on InputPids ordering.
flatten_signals(InputPids, AccInput) ->
    lists:flatmap(
        fun(Pid) ->
            maps:get(Pid, AccInput, [0.0])
        end,
        InputPids
    ).

%% @private Aggregate using pre-compiled flat weights.
%%
%% Uses NIF when available, falls back to pure Erlang.
aggregate_compiled(FlatSignals, FlatWeights, Bias) ->
    case tweann_nif:is_loaded() of
        true ->
            tweann_nif:dot_product_flat(FlatSignals, FlatWeights, Bias);
        false ->
            dot_product_erlang(FlatSignals, FlatWeights, Bias)
    end.

%% @private Pure Erlang dot product for fallback.
dot_product_erlang(Signals, Weights, Bias) ->
    lists:foldl(
        fun({S, W}, Acc) -> S * W + Acc end,
        Bias,
        lists:zip(Signals, Weights)
    ).
