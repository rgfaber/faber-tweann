%% @doc Unit tests for network_evaluator module.
%%
%% Tests the synchronous neural network evaluator for real-time inference.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(network_evaluator_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

network_evaluator_exports_test() ->
    Exports = network_evaluator:module_info(exports),
    ?assert(lists:member({create_feedforward, 3}, Exports)),
    ?assert(lists:member({create_feedforward, 4}, Exports)),
    ?assert(lists:member({create_feedforward, 5}, Exports)),
    ?assert(lists:member({evaluate, 2}, Exports)),
    ?assert(lists:member({evaluate_with_activations, 2}, Exports)),
    ?assert(lists:member({from_genotype, 1}, Exports)),
    ?assert(lists:member({get_weights, 1}, Exports)),
    ?assert(lists:member({set_weights, 2}, Exports)),
    ?assert(lists:member({get_topology, 1}, Exports)),
    ?assert(lists:member({get_viz_data, 3}, Exports)),
    ?assert(lists:member({to_json, 1}, Exports)),
    ?assert(lists:member({from_json, 1}, Exports)),
    ?assert(lists:member({to_binary, 1}, Exports)),
    ?assert(lists:member({from_binary, 1}, Exports)).

%% ============================================================================
%% create_feedforward Tests
%% ============================================================================

create_feedforward_simple_test() ->
    Network = network_evaluator:create_feedforward(2, [], 1),
    ?assert(is_tuple(Network)),
    Topology = network_evaluator:get_topology(Network),
    ?assertEqual([2, 1], maps:get(layer_sizes, Topology)).

create_feedforward_with_hidden_test() ->
    Network = network_evaluator:create_feedforward(3, [4, 5], 2),
    Topology = network_evaluator:get_topology(Network),
    ?assertEqual([3, 4, 5, 2], maps:get(layer_sizes, Topology)).

create_feedforward_with_activation_test() ->
    NetworkTanh = network_evaluator:create_feedforward(2, [4], 1, tanh),
    NetworkSigmoid = network_evaluator:create_feedforward(2, [4], 1, sigmoid),
    NetworkRelu = network_evaluator:create_feedforward(2, [4], 1, relu),
    NetworkLinear = network_evaluator:create_feedforward(2, [4], 1, linear),

    %% All should create valid networks
    ?assert(is_tuple(NetworkTanh)),
    ?assert(is_tuple(NetworkSigmoid)),
    ?assert(is_tuple(NetworkRelu)),
    ?assert(is_tuple(NetworkLinear)).

%% ============================================================================
%% evaluate Tests
%% ============================================================================

evaluate_basic_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],
    Outputs = network_evaluator:evaluate(Network, Inputs),

    ?assert(is_list(Outputs)),
    ?assertEqual(1, length(Outputs)),
    [Output] = Outputs,
    ?assert(is_float(Output)).

evaluate_tanh_bounds_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, tanh),
    Inputs = [1.0, 1.0],
    [Output] = network_evaluator:evaluate(Network, Inputs),

    %% Tanh output should be in [-1, 1]
    ?assert(Output >= -1.0),
    ?assert(Output =< 1.0).

evaluate_sigmoid_bounds_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, sigmoid),
    Inputs = [1.0, 1.0],
    [Output] = network_evaluator:evaluate(Network, Inputs),

    %% Sigmoid output should be in [0, 1]
    ?assert(Output >= 0.0),
    ?assert(Output =< 1.0).

evaluate_relu_non_negative_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, relu),
    Inputs = [1.0, 1.0],
    [Output] = network_evaluator:evaluate(Network, Inputs),

    %% ReLU output should be >= 0
    ?assert(Output >= 0.0).

evaluate_deterministic_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],

    %% Same network, same inputs should give same outputs
    Output1 = network_evaluator:evaluate(Network, Inputs),
    Output2 = network_evaluator:evaluate(Network, Inputs),

    ?assertEqual(Output1, Output2).

evaluate_multiple_outputs_test() ->
    Network = network_evaluator:create_feedforward(3, [4], 5),
    Inputs = [0.1, 0.2, 0.3],
    Outputs = network_evaluator:evaluate(Network, Inputs),

    ?assertEqual(5, length(Outputs)),
    lists:foreach(
        fun(O) -> ?assert(is_float(O)) end,
        Outputs
    ).

%% ============================================================================
%% evaluate_with_activations Tests
%% ============================================================================

evaluate_with_activations_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],

    {Outputs, Activations} = network_evaluator:evaluate_with_activations(Network, Inputs),

    %% Outputs should match regular evaluate
    ?assertEqual(Outputs, network_evaluator:evaluate(Network, Inputs)),

    %% Activations should have 3 layers (input, hidden, output)
    ?assertEqual(3, length(Activations)),

    [InputActs, HiddenActs, OutputActs] = Activations,
    ?assertEqual(2, length(InputActs)),
    ?assertEqual(4, length(HiddenActs)),
    ?assertEqual(1, length(OutputActs)).

evaluate_with_activations_input_preserved_test() ->
    Network = network_evaluator:create_feedforward(3, [4], 2),
    Inputs = [0.1, 0.2, 0.3],

    {_Outputs, Activations} = network_evaluator:evaluate_with_activations(Network, Inputs),

    %% First activation layer should be the inputs
    [InputActs | _] = Activations,
    ?assertEqual(Inputs, InputActs).

%% ============================================================================
%% get_weights / set_weights Tests
%% ============================================================================

get_weights_returns_flat_list_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Weights = network_evaluator:get_weights(Network),

    ?assert(is_list(Weights)),
    ?assert(length(Weights) > 0),
    lists:foreach(
        fun(W) -> ?assert(is_float(W)) end,
        Weights
    ).

get_weights_count_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Weights = network_evaluator:get_weights(Network),

    %% 2->4: 2*4 weights + 4 biases = 12
    %% 4->1: 4*1 weights + 1 bias = 5
    %% Total: 17
    ?assertEqual(17, length(Weights)).

set_weights_roundtrip_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    OrigWeights = network_evaluator:get_weights(Network),

    %% Set custom weights
    CustomWeights = [float(I) / 100 || I <- lists:seq(1, length(OrigWeights))],
    NewNetwork = network_evaluator:set_weights(Network, CustomWeights),

    %% Get weights should return what we set
    RetrievedWeights = network_evaluator:get_weights(NewNetwork),
    ?assertEqual(CustomWeights, RetrievedWeights).

set_weights_affects_evaluation_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],

    OrigOutput = network_evaluator:evaluate(Network, Inputs),

    %% Change weights
    Weights = network_evaluator:get_weights(Network),
    NewWeights = [W * 2 || W <- Weights],
    NewNetwork = network_evaluator:set_weights(Network, NewWeights),

    NewOutput = network_evaluator:evaluate(NewNetwork, Inputs),

    %% Output should be different
    ?assertNotEqual(OrigOutput, NewOutput).

%% ============================================================================
%% get_topology Tests
%% ============================================================================

get_topology_simple_test() ->
    Network = network_evaluator:create_feedforward(3, [5, 4], 2),
    Topology = network_evaluator:get_topology(Network),

    ?assert(is_map(Topology)),
    ?assertEqual([3, 5, 4, 2], maps:get(layer_sizes, Topology)),
    ?assertEqual(4, maps:get(num_layers, Topology)),
    ?assertEqual(14, maps:get(total_neurons, Topology)).  %% 3 + 5 + 4 + 2 = 14

get_topology_includes_connections_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Topology = network_evaluator:get_topology(Network),

    %% 2->4: 8 connections, 4->1: 4 connections = 12
    ?assertEqual(12, maps:get(total_connections, Topology)).

%% ============================================================================
%% Serialization Tests
%% ============================================================================

to_json_format_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, tanh),
    Json = network_evaluator:to_json(Network),

    ?assert(is_map(Json)),
    ?assertEqual(1, maps:get(<<"version">>, Json)),
    ?assertEqual(<<"tanh">>, maps:get(<<"activation">>, Json)),
    ?assert(is_list(maps:get(<<"layers">>, Json))).

to_json_from_json_roundtrip_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, sigmoid),
    Inputs = [0.5, 0.5],
    OrigOutput = network_evaluator:evaluate(Network, Inputs),

    Json = network_evaluator:to_json(Network),
    {ok, LoadedNetwork} = network_evaluator:from_json(Json),
    LoadedOutput = network_evaluator:evaluate(LoadedNetwork, Inputs),

    ?assertEqual(OrigOutput, LoadedOutput).

from_json_invalid_version_test() ->
    InvalidJson = #{<<"version">> => 99, <<"activation">> => <<"tanh">>, <<"layers">> => []},
    ?assertEqual({error, unsupported_version}, network_evaluator:from_json(InvalidJson)).

from_json_invalid_format_test() ->
    InvalidJson = #{<<"invalid">> => true},
    ?assertEqual({error, unsupported_version}, network_evaluator:from_json(InvalidJson)).

to_binary_from_binary_roundtrip_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],
    OrigOutput = network_evaluator:evaluate(Network, Inputs),

    Binary = network_evaluator:to_binary(Network),
    ?assert(is_binary(Binary)),

    {ok, LoadedNetwork} = network_evaluator:from_binary(Binary),
    LoadedOutput = network_evaluator:evaluate(LoadedNetwork, Inputs),

    ?assertEqual(OrigOutput, LoadedOutput).

from_binary_invalid_test() ->
    ?assertEqual({error, invalid_binary}, network_evaluator:from_binary(123)),
    {error, _} = network_evaluator:from_binary(<<1, 2, 3>>).

%% ============================================================================
%% get_viz_data Tests
%% ============================================================================

get_viz_data_structure_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],
    InputLabels = [<<"X1">>, <<"X2">>],

    VizData = network_evaluator:get_viz_data(Network, Inputs, InputLabels),

    ?assert(is_map(VizData)),
    ?assert(maps:is_key(nodes, VizData)),
    ?assert(maps:is_key(connections, VizData)),
    ?assert(maps:is_key(layer_sizes, VizData)),
    ?assert(maps:is_key(outputs, VizData)).

get_viz_data_nodes_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],

    VizData = network_evaluator:get_viz_data(Network, Inputs, []),
    Nodes = maps:get(nodes, VizData),

    %% Should have 2 + 4 + 1 = 7 nodes
    ?assertEqual(7, length(Nodes)),

    %% Each node should have expected fields
    [FirstNode | _] = Nodes,
    ?assert(maps:is_key(id, FirstNode)),
    ?assert(maps:is_key(layer, FirstNode)),
    ?assert(maps:is_key(activation, FirstNode)),
    ?assert(maps:is_key(type, FirstNode)).

get_viz_data_connections_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1),
    Inputs = [0.5, 0.5],

    VizData = network_evaluator:get_viz_data(Network, Inputs, []),
    Connections = maps:get(connections, VizData),

    %% Should have 2*4 + 4*1 = 12 connections
    ?assertEqual(12, length(Connections)),

    %% Each connection should have expected fields
    [FirstConn | _] = Connections,
    ?assert(maps:is_key(from, FirstConn)),
    ?assert(maps:is_key(to, FirstConn)),
    ?assert(maps:is_key(weight, FirstConn)).

%% ============================================================================
%% Output Activation Tests
%% ============================================================================

create_feedforward_with_output_activation_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, tanh, linear),
    ?assert(is_tuple(Network)),
    Topology = network_evaluator:get_topology(Network),
    ?assertEqual([2, 4, 1], maps:get(layer_sizes, Topology)).

output_activation_linear_unbounded_test() ->
    %% With tanh hidden + linear output, output can exceed [-1, 1]
    Network = network_evaluator:create_feedforward(2, [4], 1, tanh, linear),
    Weights = network_evaluator:get_weights(Network),
    %% Set large weights to force output outside tanh range
    LargeWeights = [W * 10.0 || W <- Weights],
    LargeNet = network_evaluator:set_weights(Network, LargeWeights),
    [Output] = network_evaluator:evaluate(LargeNet, [1.0, 1.0]),
    %% Linear output can exceed 1.0 or go below -1.0
    ?assert(is_float(Output)).

output_activation_undefined_same_as_activation_test() ->
    %% undefined output_activation should behave same as specifying same activation
    Net1 = network_evaluator:create_feedforward(2, [4], 1, tanh, undefined),
    Net2 = network_evaluator:create_feedforward(2, [4], 1, tanh),
    %% Set identical weights
    Weights = network_evaluator:get_weights(Net1),
    Net2b = network_evaluator:set_weights(Net2, Weights),
    Net1b = network_evaluator:set_weights(Net1, Weights),
    Out1 = network_evaluator:evaluate(Net1b, [0.5, 0.5]),
    Out2 = network_evaluator:evaluate(Net2b, [0.5, 0.5]),
    ?assertEqual(Out1, Out2).

output_activation_json_v2_roundtrip_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 3, tanh, linear),
    Inputs = [0.5, 0.5],
    OrigOutput = network_evaluator:evaluate(Network, Inputs),

    Json = network_evaluator:to_json(Network),
    ?assertEqual(2, maps:get(<<"version">>, Json)),
    ?assertEqual(<<"linear">>, maps:get(<<"output_activation">>, Json)),

    {ok, LoadedNetwork} = network_evaluator:from_json(Json),
    LoadedOutput = network_evaluator:evaluate(LoadedNetwork, Inputs),
    ?assertEqual(OrigOutput, LoadedOutput).

output_activation_json_v1_no_output_activation_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, sigmoid),
    Json = network_evaluator:to_json(Network),
    ?assertEqual(1, maps:get(<<"version">>, Json)),
    ?assertEqual(error, maps:find(<<"output_activation">>, Json)).

output_activation_with_activations_test() ->
    Network = network_evaluator:create_feedforward(2, [4], 1, tanh, linear),
    Inputs = [0.5, 0.5],
    {Outputs, Activations} = network_evaluator:evaluate_with_activations(Network, Inputs),
    ?assertEqual(Outputs, network_evaluator:evaluate(Network, Inputs)),
    ?assertEqual(3, length(Activations)).

