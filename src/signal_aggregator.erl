%% @doc Signal aggregation functions for neural computation.
%%
%% This module provides functions that aggregate weighted inputs from multiple
%% sources into a single scalar value for activation function processing.
%%
%% == Aggregation Methods ==
%%
%% - `dot_product' - Standard weighted sum (most common)
%%
%% - `mult_product' - Multiplicative aggregation
%%
%% - `diff_product' - Differentiation-based aggregation (uses process dictionary)
%%
%% == Weight Tuple Format ==
%%
%% Weights are provided as tuples: `{Weight, DeltaWeight, LearningRate, ParamList}'
%%
%% - Weight: The actual weight value used for computation
%%
%% - DeltaWeight: Momentum term (ignored here, used by plasticity)
%%
%% - LearningRate: Learning parameter (ignored here)
%%
%% - ParamList: Additional parameters for plasticity rules (ignored here)
%%
%% Only the Weight value is used for aggregation. The other fields support
%% the plasticity system for weight updates during learning.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0

-module(signal_aggregator).

-include("types.hrl").

-export([
    dot_product/2,
    mult_product/2,
    diff_product/2,
    %% NIF-accelerated functions
    dot_product_nif/2,
    flatten_for_nif/2
]).

%%==============================================================================
%% Type Specifications
%%==============================================================================

-type input_signal() :: {element_id(), [float()]}.
-type input_signals() :: [input_signal()].

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Compute dot product of inputs and weights
%%
%% For each input source, multiplies each input signal component by its
%% corresponding weight and sums all results. This is the standard weighted
%% sum aggregation used in most neural networks.
%%
%% The bias term is handled specially - if present as the last weight entry
%% with source ID 'bias', its weight is added directly to the result.
%%
%% Weight tuple format: {W, DW, LP, LPs}
%%   W  - Weight value (used for computation)
%%   DW - Delta weight (ignored here, used by plasticity)
%%   LP - Learning parameter (ignored here)
%%   LPs - Parameter list (ignored here)
%%
%% @param InputSignals List of {SourceId, SignalVector} tuples
%% @param WeightedInputs List of {SourceId, [WeightSpec]} tuples
%% @returns Aggregated scalar value
%%
%% Example:
%%
%% Inputs = [{sensor1, [1.0, 0.5]}],
%% Weights = [{sensor1, [{0.3, 0.0, 0.1, []}, {0.7, 0.0, 0.1, []}]}],
%% Result = dot_product(Inputs, Weights).
%% Result = 1.0*0.3 + 0.5*0.7 = 0.65
-spec dot_product(input_signals(), weighted_inputs()) -> float().
dot_product(InputSignals, WeightedInputs) ->
    dot_product(InputSignals, WeightedInputs, 0.0).

%% @private
dot_product([{SourceId, Signals} | RestInputs],
            [{SourceId, Weights} | RestWeights], Acc) ->
    DotSum = weighted_sum(Signals, Weights, 0.0),
    dot_product(RestInputs, RestWeights, DotSum + Acc);
dot_product([], [{bias, [{BiasWeight, _DW, _LP, _LPs}]}], Acc) ->
    Acc + BiasWeight;
dot_product([], [], Acc) ->
    Acc.

%% @doc Compute multiplicative product of inputs and weights
%%
%% For each input source, multiplies each input signal component by its
%% corresponding weight, then multiplies all these products together.
%% Useful for AND-like logic in neural networks.
%%
%% Note: Any zero input will result in zero output due to multiplication.
%%
%% @param InputSignals List of {SourceId, SignalVector} tuples
%% @param WeightedInputs List of {SourceId, [WeightSpec]} tuples
%% @returns Aggregated scalar value
%%
%% Example:
%%
%% Inputs = [{sensor1, [0.5, 0.4]}],
%% Weights = [{sensor1, [{2.0, 0.0, 0.1, []}, {3.0, 0.0, 0.1, []}]}],
%% Result = mult_product(Inputs, Weights).
%% Result = (0.5*2.0) * (0.4*3.0) = 1.2
-spec mult_product(input_signals(), weighted_inputs()) -> float().
mult_product(InputSignals, WeightedInputs) ->
    mult_product(InputSignals, WeightedInputs, 1.0).

%% @private
mult_product([{SourceId, Signals} | RestInputs],
             [{SourceId, Weights} | RestWeights], Acc) ->
    Product = weighted_mult(Signals, Weights, 1.0),
    mult_product(RestInputs, RestWeights, Product * Acc);
mult_product([], [{bias, [{BiasWeight, _DW, _LP, _LPs}]}], Acc) ->
    Acc * BiasWeight;
mult_product([], [], Acc) ->
    Acc.

%% @doc Compute differentiation-based product of inputs
%%
%% Uses the difference between current and previous inputs, then applies
%% dot product aggregation. This implements temporal differentiation for
%% detecting changes in input signals.
%%
%% Warning: This function uses the process dictionary to store previous
%% input state. On first call, behaves like regular dot_product.
%%
%% @param InputSignals List of {SourceId, SignalVector} tuples
%% @param WeightedInputs List of {SourceId, [WeightSpec]} tuples
%% @returns Aggregated scalar value based on input differences
-spec diff_product(input_signals(), weighted_inputs()) -> float().
diff_product(InputSignals, WeightedInputs) ->
    case get(diff_product) of
        undefined ->
            put(diff_product, InputSignals),
            dot_product(InputSignals, WeightedInputs, 0.0);
        PrevInputs ->
            put(diff_product, InputSignals),
            DiffInputs = compute_input_diff(InputSignals, PrevInputs, []),
            dot_product(DiffInputs, WeightedInputs, 0.0)
    end.

%%==============================================================================
%% NIF-Accelerated Functions
%%==============================================================================

%% @doc NIF-accelerated dot product when available.
%%
%% This function attempts to use the Rust NIF for dot product computation.
%% Falls back to pure Erlang if NIF is not loaded.
%%
%% Performance: 40-100x faster than pure Erlang for N > 10 inputs.
%%
%% @param InputSignals List of {SourceId, SignalVector} tuples
%% @param WeightedInputs List of {SourceId, [WeightSpec]} tuples
%% @returns Aggregated scalar value
-spec dot_product_nif(input_signals(), weighted_inputs()) -> float().
dot_product_nif(InputSignals, WeightedInputs) ->
    case tweann_nif:is_loaded() of
        true ->
            {FlatSignals, FlatWeights, Bias} = flatten_for_nif(InputSignals, WeightedInputs),
            tweann_nif:dot_product_flat(FlatSignals, FlatWeights, Bias);
        false ->
            dot_product(InputSignals, WeightedInputs, 0.0)
    end.

%% @doc Flatten nested signal/weight structure for NIF consumption.
%%
%% Converts from:
%%   Signals: [{SourceId, [S1, S2, ...]}, ...]
%%   Weights: [{SourceId, [{W1, DW1, LP1, []}, {W2, DW2, LP2, []}, ...]}, ...]
%%
%% To:
%%   FlatSignals: [S1, S2, S3, ...]
%%   FlatWeights: [W1, W2, W3, ...]
%%   Bias: float()
%%
%% The flattened format is cache-friendly and suitable for SIMD vectorization
%% in the Rust NIF.
%%
%% @param InputSignals List of {SourceId, SignalVector} tuples
%% @param WeightedInputs List of {SourceId, [WeightSpec]} tuples
%% @returns {FlatSignals, FlatWeights, Bias}
-spec flatten_for_nif(input_signals(), weighted_inputs()) ->
    {[float()], [float()], float()}.
flatten_for_nif(InputSignals, WeightedInputs) ->
    flatten_for_nif(InputSignals, WeightedInputs, [], [], 0.0).

%% @private Flatten signals and weights recursively.
flatten_for_nif([{SourceId, Signals} | RestInputs],
                [{SourceId, Weights} | RestWeights], AccSignals, AccWeights, Bias) ->
    {NewSignals, NewWeights} = flatten_pair(Signals, Weights, AccSignals, AccWeights),
    flatten_for_nif(RestInputs, RestWeights, NewSignals, NewWeights, Bias);
flatten_for_nif([], [{bias, [{BiasWeight, _DW, _LP, _LPs}]}], AccSignals, AccWeights, _Bias) ->
    {lists:reverse(AccSignals), lists:reverse(AccWeights), BiasWeight};
flatten_for_nif([], [], AccSignals, AccWeights, Bias) ->
    {lists:reverse(AccSignals), lists:reverse(AccWeights), Bias}.

%% @private Flatten a single signal/weight pair.
flatten_pair([Signal | RestSignals], [{Weight, _DW, _LP, _LPs} | RestWeights],
             AccSignals, AccWeights) ->
    flatten_pair(RestSignals, RestWeights, [Signal | AccSignals], [Weight | AccWeights]);
flatten_pair([], [], AccSignals, AccWeights) ->
    {AccSignals, AccWeights}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Compute weighted sum of signals and weights
-spec weighted_sum([float()], [weight_spec()], float()) -> float().
weighted_sum([Signal | RestSignals], [{Weight, _DW, _LP, _LPs} | RestWeights], Acc) ->
    weighted_sum(RestSignals, RestWeights, Signal * Weight + Acc);
weighted_sum([], [], Acc) ->
    Acc.

%% @private
%% @doc Compute weighted product of signals and weights
-spec weighted_mult([float()], [weight_spec()], float()) -> float().
weighted_mult([Signal | RestSignals], [{Weight, _DW, _LP, _LPs} | RestWeights], Acc) ->
    weighted_mult(RestSignals, RestWeights, Signal * Weight * Acc);
weighted_mult([], [], Acc) ->
    Acc.

%% @private
%% @doc Compute difference between current and previous inputs
-spec compute_input_diff(input_signals(), input_signals(), input_signals()) ->
    input_signals().
compute_input_diff([{SourceId, Signals} | RestInputs],
                   [{SourceId, PrevSignals} | RestPrev], Acc) ->
    DiffSignals = vector_diff(Signals, PrevSignals, []),
    compute_input_diff(RestInputs, RestPrev, [{SourceId, DiffSignals} | Acc]);
compute_input_diff([], [], Acc) ->
    lists:reverse(Acc).

%% @private
%% @doc Compute element-wise difference between two vectors
-spec vector_diff([float()], [float()], [float()]) -> [float()].
vector_diff([A | RestA], [B | RestB], Acc) ->
    vector_diff(RestA, RestB, [A - B | Acc]);
vector_diff([], [], Acc) ->
    lists:reverse(Acc).
