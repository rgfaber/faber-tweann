%% @module signal_aggregator_test
%% @doc Unit tests for signal_aggregator module
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0

-module(signal_aggregator_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%==============================================================================
%% dot_product tests
%%==============================================================================

dot_product_single_input_test() ->
    Inputs = [{source1, [1.0]}],
    Weights = [{source1, [{0.5, 0.0, 0.1, []}]}],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    ?assertEqual(0.5, Result).

dot_product_multiple_components_test() ->
    Inputs = [{source1, [0.5, 0.3]}],
    Weights = [{source1, [{0.2, 0.0, 0.1, []}, {0.4, 0.0, 0.1, []}]}],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    Expected = 0.5 * 0.2 + 0.3 * 0.4,
    ?assert(abs(Result - Expected) < 0.0001).

dot_product_multiple_sources_test() ->
    Inputs = [
        {source1, [0.5]},
        {source2, [0.8]}
    ],
    Weights = [
        {source1, [{0.3, 0.0, 0.1, []}]},
        {source2, [{0.7, 0.0, 0.1, []}]}
    ],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    Expected = 0.5 * 0.3 + 0.8 * 0.7,
    ?assert(abs(Result - Expected) < 0.0001).

dot_product_negative_weights_test() ->
    Inputs = [{source1, [1.0]}],
    Weights = [{source1, [{-0.5, 0.0, 0.1, []}]}],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    ?assertEqual(-0.5, Result).

dot_product_negative_inputs_test() ->
    Inputs = [{source1, [-0.5]}],
    Weights = [{source1, [{2.0, 0.0, 0.1, []}]}],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    ?assertEqual(-1.0, Result).

dot_product_empty_inputs_test() ->
    Result = signal_aggregator:dot_product([], []),
    ?assertEqual(0.0, Result).

dot_product_with_bias_test() ->
    Inputs = [{source1, [1.0]}],
    Weights = [
        {source1, [{0.5, 0.0, 0.1, []}]},
        {bias, [{0.3, 0.0, 0.0, []}]}
    ],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    Expected = 1.0 * 0.5 + 0.3,
    ?assertEqual(Expected, Result).

dot_product_bias_only_test() ->
    Inputs = [],
    Weights = [{bias, [{0.5, 0.0, 0.0, []}]}],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    ?assertEqual(0.5, Result).

dot_product_zero_weights_test() ->
    Inputs = [{source1, [1.0, 2.0]}],
    Weights = [{source1, [{0.0, 0.0, 0.1, []}, {0.0, 0.0, 0.1, []}]}],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    ?assertEqual(0.0, Result).

dot_product_ignores_delta_weight_test() ->
    Inputs = [{source1, [1.0]}],
    Weights = [{source1, [{0.5, 999.0, 0.1, []}]}],
    Result = signal_aggregator:dot_product(Inputs, Weights),
    ?assertEqual(0.5, Result).

%%==============================================================================
%% mult_product tests
%%==============================================================================

mult_product_single_input_test() ->
    Inputs = [{source1, [0.5]}],
    Weights = [{source1, [{2.0, 0.0, 0.1, []}]}],
    Result = signal_aggregator:mult_product(Inputs, Weights),
    ?assertEqual(1.0, Result).

mult_product_multiple_components_test() ->
    Inputs = [{source1, [0.5, 0.4]}],
    Weights = [{source1, [{2.0, 0.0, 0.1, []}, {3.0, 0.0, 0.1, []}]}],
    Result = signal_aggregator:mult_product(Inputs, Weights),
    Expected = (0.5 * 2.0) * (0.4 * 3.0),
    ?assert(abs(Result - Expected) < 0.0001).

mult_product_zero_input_test() ->
    Inputs = [{source1, [0.0, 0.5]}],
    Weights = [{source1, [{1.0, 0.0, 0.1, []}, {1.0, 0.0, 0.1, []}]}],
    Result = signal_aggregator:mult_product(Inputs, Weights),
    ?assertEqual(0.0, Result).

mult_product_multiple_sources_test() ->
    Inputs = [
        {source1, [2.0]},
        {source2, [3.0]}
    ],
    Weights = [
        {source1, [{1.0, 0.0, 0.1, []}]},
        {source2, [{1.0, 0.0, 0.1, []}]}
    ],
    Result = signal_aggregator:mult_product(Inputs, Weights),
    Expected = 2.0 * 3.0,
    ?assertEqual(Expected, Result).

mult_product_with_bias_test() ->
    Inputs = [{source1, [2.0]}],
    Weights = [
        {source1, [{1.5, 0.0, 0.1, []}]},
        {bias, [{0.5, 0.0, 0.0, []}]}
    ],
    Result = signal_aggregator:mult_product(Inputs, Weights),
    Expected = (2.0 * 1.5) * 0.5,
    ?assertEqual(Expected, Result).

mult_product_empty_inputs_test() ->
    Result = signal_aggregator:mult_product([], []),
    ?assertEqual(1.0, Result).

%%==============================================================================
%% diff_product tests
%%==============================================================================

diff_product_first_call_test() ->
    %% First call should behave like dot_product
    erase(diff_product),
    Inputs = [{source1, [1.0]}],
    Weights = [{source1, [{0.5, 0.0, 0.1, []}]}],
    Result = signal_aggregator:diff_product(Inputs, Weights),
    ?assertEqual(0.5, Result),
    erase(diff_product).

diff_product_second_call_test() ->
    %% Second call should use difference from first
    erase(diff_product),
    Inputs1 = [{source1, [1.0]}],
    Weights = [{source1, [{1.0, 0.0, 0.1, []}]}],

    %% First call establishes baseline
    _Result1 = signal_aggregator:diff_product(Inputs1, Weights),

    %% Second call with different input
    Inputs2 = [{source1, [1.5]}],
    Result2 = signal_aggregator:diff_product(Inputs2, Weights),

    %% Difference is 1.5 - 1.0 = 0.5, times weight 1.0 = 0.5
    ?assertEqual(0.5, Result2),
    erase(diff_product).

diff_product_no_change_test() ->
    erase(diff_product),
    Inputs = [{source1, [1.0]}],
    Weights = [{source1, [{1.0, 0.0, 0.1, []}]}],

    %% First call
    _Result1 = signal_aggregator:diff_product(Inputs, Weights),

    %% Same input - difference is 0
    Result2 = signal_aggregator:diff_product(Inputs, Weights),
    ?assertEqual(0.0, Result2),
    erase(diff_product).
