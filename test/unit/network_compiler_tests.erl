%% @doc Unit tests for the network compiler module.
%%
%% Tests the compilation of TWEANN genotypes to NIF-compatible format.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(network_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%%==============================================================================
%% Test Fixtures
%%==============================================================================

%% Create a simple sensor record
make_sensor(Id, Vl, FanoutIds) ->
    #sensor{
        id = Id,
        name = test_sensor,
        type = standard,
        vl = Vl,
        fanout_ids = FanoutIds
    }.

%% Create a simple neuron record
make_neuron(Id, Af, InputIdps, OutputIds) ->
    #neuron{
        id = Id,
        af = Af,
        input_idps = InputIdps,
        output_ids = OutputIds
    }.

%% Create a simple actuator record
make_actuator(Id, Vl, FaninIds) ->
    #actuator{
        id = Id,
        name = test_actuator,
        type = standard,
        vl = Vl,
        fanin_ids = FaninIds
    }.

%% Create a simple cortex record
make_cortex(Id, SensorIds, NeuronIds, ActuatorIds) ->
    #cortex{
        id = Id,
        sensor_ids = SensorIds,
        neuron_ids = NeuronIds,
        actuator_ids = ActuatorIds
    }.

%%==============================================================================
%% Compile Simple Network Tests
%%==============================================================================

compile_simple_feedforward_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {ok, Network, Weights} = network_compiler:compile_simple(2, [3], 1),
            ?assert(is_reference(Network)),
            ?assert(is_list(Weights)),
            %% Should have weights for: 2 inputs -> 3 hidden + 3 hidden -> 1 output
            %% = 2*3 + 3*1 = 9 weights
            ?assertEqual(9, length(Weights))
    end.

compile_simple_no_hidden_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {ok, Network, Weights} = network_compiler:compile_simple(3, [], 2),
            ?assert(is_reference(Network)),
            %% Direct connection: 3 inputs -> 2 outputs = 6 weights
            ?assertEqual(6, length(Weights))
    end.

compile_simple_deep_network_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {ok, Network, Weights} = network_compiler:compile_simple(2, [4, 4, 4], 1),
            ?assert(is_reference(Network)),
            %% 2->4 + 4->4 + 4->4 + 4->1 = 8 + 16 + 16 + 4 = 44 weights
            ?assertEqual(44, length(Weights))
    end.

%%==============================================================================
%% Compile from Records Tests
%%==============================================================================

compile_from_records_simple_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Create a simple 2 input -> 1 hidden -> 1 output network
            S1Id = {{-1.0, 0.1}, sensor},
            S2Id = {{-1.0, 0.2}, sensor},
            N1Id = {{0.5, 0.1}, neuron},
            A1Id = {{1.0, 0.1}, actuator},

            Sensors = [
                make_sensor(S1Id, 1, [N1Id]),
                make_sensor(S2Id, 1, [N1Id])
            ],

            Neurons = [
                make_neuron(N1Id, tanh, [
                    {S1Id, [{0.5, 0.0, 0.1, []}]},
                    {S2Id, [{0.3, 0.0, 0.1, []}]}
                ], [A1Id])
            ],

            Actuators = [
                make_actuator(A1Id, 1, [N1Id])
            ],

            Cortex = make_cortex({{0.0, 0.1}, cortex},
                                 [S1Id, S2Id], [N1Id], [A1Id]),

            {ok, Network} = network_compiler:compile_from_records(
                Cortex, Neurons, Sensors, Actuators
            ),
            ?assert(is_reference(Network)),

            %% Test evaluation
            Outputs = tweann_nif:evaluate(Network, [1.0, 1.0]),
            ?assertEqual(1, length(Outputs))
    end.

compile_from_records_with_bias_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            S1Id = {{-1.0, 0.1}, sensor},
            N1Id = {{0.5, 0.1}, neuron},
            A1Id = {{1.0, 0.1}, actuator},

            Sensors = [make_sensor(S1Id, 1, [N1Id])],

            Neurons = [
                make_neuron(N1Id, tanh, [
                    {S1Id, [{1.0, 0.0, 0.1, []}]},
                    {bias, [{0.5, 0.0, 0.1, []}]}  %% Bias connection
                ], [A1Id])
            ],

            Actuators = [make_actuator(A1Id, 1, [N1Id])],
            Cortex = make_cortex({{0.0, 0.1}, cortex}, [S1Id], [N1Id], [A1Id]),

            {ok, Network} = network_compiler:compile_from_records(
                Cortex, Neurons, Sensors, Actuators
            ),

            %% Test that bias is applied
            [Out1] = tweann_nif:evaluate(Network, [0.0]),
            %% With bias 0.5 and input 0.0: tanh(0.0 + 0.5) ≈ 0.462
            ?assert(abs(Out1 - 0.462) < 0.01)
    end.

compile_from_records_multi_layer_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% 1 input -> 2 hidden (layer 0.3) -> 1 hidden (layer 0.6) -> 1 output
            S1Id = {{-1.0, 0.1}, sensor},
            N1Id = {{0.3, 0.1}, neuron},  %% First hidden layer
            N2Id = {{0.3, 0.2}, neuron},  %% First hidden layer
            N3Id = {{0.6, 0.1}, neuron},  %% Second hidden layer
            A1Id = {{1.0, 0.1}, actuator},

            Sensors = [make_sensor(S1Id, 1, [N1Id, N2Id])],

            Neurons = [
                make_neuron(N1Id, tanh, [{S1Id, [{1.0, 0.0, 0.1, []}]}], [N3Id]),
                make_neuron(N2Id, tanh, [{S1Id, [{-1.0, 0.0, 0.1, []}]}], [N3Id]),
                make_neuron(N3Id, tanh, [
                    {N1Id, [{1.0, 0.0, 0.1, []}]},
                    {N2Id, [{1.0, 0.0, 0.1, []}]}
                ], [A1Id])
            ],

            Actuators = [make_actuator(A1Id, 1, [N3Id])],
            Cortex = make_cortex({{0.0, 0.1}, cortex},
                                 [S1Id], [N1Id, N2Id, N3Id], [A1Id]),

            {ok, Network} = network_compiler:compile_from_records(
                Cortex, Neurons, Sensors, Actuators
            ),

            Outputs = tweann_nif:evaluate(Network, [1.0]),
            ?assertEqual(1, length(Outputs))
    end.

%%==============================================================================
%% Evaluation Consistency Tests
%%==============================================================================

evaluation_consistency_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {ok, Network, _} = network_compiler:compile_simple(2, [4], 1),

            %% Same input should give same output
            Out1 = tweann_nif:evaluate(Network, [0.5, 0.5]),
            Out2 = tweann_nif:evaluate(Network, [0.5, 0.5]),
            ?assertEqual(Out1, Out2)
    end.

evaluation_different_inputs_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {ok, Network, _} = network_compiler:compile_simple(2, [4], 1),

            %% Different inputs should (usually) give different outputs
            [Out1] = tweann_nif:evaluate(Network, [0.0, 0.0]),
            [Out2] = tweann_nif:evaluate(Network, [1.0, 1.0]),
            %% They might be the same by chance, but very unlikely
            ?assertNotEqual(Out1, Out2)
    end.

%%==============================================================================
%% Batch Processing Tests
%%==============================================================================

batch_evaluation_consistency_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {ok, Network, _} = network_compiler:compile_simple(2, [3], 1),

            Inputs = [
                [0.0, 0.0],
                [1.0, 0.0],
                [0.0, 1.0],
                [1.0, 1.0]
            ],

            %% Batch evaluation
            BatchOutputs = tweann_nif:evaluate_batch(Network, Inputs),

            %% Individual evaluations
            IndividualOutputs = [tweann_nif:evaluate(Network, I) || I <- Inputs],

            %% Should match
            ?assertEqual(IndividualOutputs, BatchOutputs)
    end.
