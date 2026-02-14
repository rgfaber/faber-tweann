%% @doc Unit tests for network_onnx module.
%%
%% Tests ONNX export functionality for neural networks.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(network_onnx_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

network_onnx_exports_test() ->
    Exports = network_onnx:module_info(exports),
    ?assert(lists:member({to_onnx, 1}, Exports)),
    ?assert(lists:member({to_onnx, 2}, Exports)).

%% ============================================================================
%% to_onnx/1 Tests
%% ============================================================================

to_onnx_simple_network_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

to_onnx_no_hidden_layers_test() ->
    Network = network_evaluator:create_feedforward(3, [], 2),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)).

to_onnx_multiple_hidden_layers_test() ->
    Network = network_evaluator:create_feedforward(2, [8, 4], 1),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)).

to_onnx_large_network_test() ->
    Network = network_evaluator:create_feedforward(10, [20, 15, 10], 5),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)),
    %% Larger networks should produce more data
    ?assert(byte_size(Binary) > 500).

%% ============================================================================
%% to_onnx/2 Tests (with options)
%% ============================================================================

to_onnx_with_model_name_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Opts = #{model_name => <<"my_custom_model">>},
    Result = network_onnx:to_onnx(Network, Opts),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    %% Model name should be in the binary
    ?assert(binary:match(Binary, <<"my_custom_model">>) =/= nomatch).

to_onnx_with_producer_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Opts = #{producer => <<"test_producer">>},
    Result = network_onnx:to_onnx(Network, Opts),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    %% Producer name should be in the binary
    ?assert(binary:match(Binary, <<"test_producer">>) =/= nomatch).

to_onnx_with_all_options_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Opts = #{
        model_name => <<"full_options_model">>,
        producer => <<"macula_test">>
    },
    Result = network_onnx:to_onnx(Network, Opts),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(binary:match(Binary, <<"full_options_model">>) =/= nomatch),
    ?assert(binary:match(Binary, <<"macula_test">>) =/= nomatch).

to_onnx_empty_options_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Result = network_onnx:to_onnx(Network, #{}),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    %% Default producer should be present
    ?assert(binary:match(Binary, <<"faber-tweann">>) =/= nomatch).

%% ============================================================================
%% Activation Function Tests
%% ============================================================================

to_onnx_tanh_activation_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, tanh),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    %% Tanh should be in the binary (as ONNX op type)
    ?assert(binary:match(Binary, <<"Tanh">>) =/= nomatch).

to_onnx_sigmoid_activation_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, sigmoid),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    %% Sigmoid should be in the binary
    ?assert(binary:match(Binary, <<"Sigmoid">>) =/= nomatch).

to_onnx_relu_activation_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, relu),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    %% Relu should be in the binary
    ?assert(binary:match(Binary, <<"Relu">>) =/= nomatch).

to_onnx_linear_activation_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, linear),
    Result = network_onnx:to_onnx(Network),

    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    %% Linear maps to Identity in ONNX
    ?assert(binary:match(Binary, <<"Identity">>) =/= nomatch).

%% ============================================================================
%% Binary Format Verification Tests
%% ============================================================================

to_onnx_contains_input_output_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    {ok, Binary} = network_onnx:to_onnx(Network),

    %% Should contain input and output tensor names
    ?assert(binary:match(Binary, <<"input">>) =/= nomatch),
    ?assert(binary:match(Binary, <<"output">>) =/= nomatch).

to_onnx_contains_weight_tensors_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    {ok, Binary} = network_onnx:to_onnx(Network),

    %% Should contain weight tensor names
    ?assert(binary:match(Binary, <<"weight_1">>) =/= nomatch).

to_onnx_contains_bias_tensors_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    {ok, Binary} = network_onnx:to_onnx(Network),

    %% Should contain bias tensor names
    ?assert(binary:match(Binary, <<"bias_1">>) =/= nomatch).

to_onnx_contains_matmul_ops_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    {ok, Binary} = network_onnx:to_onnx(Network),

    %% Should contain MatMul operations
    ?assert(binary:match(Binary, <<"MatMul">>) =/= nomatch).

to_onnx_contains_add_ops_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    {ok, Binary} = network_onnx:to_onnx(Network),

    %% Should contain Add operations (for bias)
    ?assert(binary:match(Binary, <<"Add">>) =/= nomatch).

%% ============================================================================
%% Consistency Tests
%% ============================================================================

to_onnx_deterministic_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),

    %% Same network should produce same ONNX output
    {ok, Binary1} = network_onnx:to_onnx(Network),
    {ok, Binary2} = network_onnx:to_onnx(Network),

    ?assertEqual(Binary1, Binary2).

to_onnx_different_networks_differ_test() ->
    Network1 = network_evaluator:create_feedforward(2, [4], 1),
    Network2 = network_evaluator:create_feedforward(3, [5], 2),

    {ok, Binary1} = network_onnx:to_onnx(Network1),
    {ok, Binary2} = network_onnx:to_onnx(Network2),

    ?assertNotEqual(Binary1, Binary2).

%% ============================================================================
%% Size Scaling Tests
%% ============================================================================

to_onnx_size_scales_with_network_test() ->
    %% Smaller network
    Network1 = network_evaluator:create_feedforward(2, [], 1),
    {ok, Binary1} = network_onnx:to_onnx(Network1),

    %% Larger network
    Network2 = network_evaluator:create_feedforward(10, [20, 15], 5),
    {ok, Binary2} = network_onnx:to_onnx(Network2),

    %% Larger network should produce larger binary
    ?assert(byte_size(Binary2) > byte_size(Binary1)).

