%% @doc Unit tests for faber_nn_nifs - Rust NIF implementations.
%%
%% Tests verify that the NIF functions produce correct results and handle
%% edge cases properly.
-module(faber_nn_nifs_tests).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Test Setup
%%==============================================================================

setup() ->
    %% Ensure NIF is loaded
    case faber_nn_nifs:is_loaded() of
        true -> ok;
        false -> throw({skip, nif_not_loaded})
    end.

%%==============================================================================
%% NIF Loading Tests
%%==============================================================================

is_loaded_test() ->
    ?assert(is_boolean(faber_nn_nifs:is_loaded())).

%%==============================================================================
%% Random Weights Tests
%%==============================================================================

random_weights_test_() ->
    {setup, fun setup/0, [
        {"generates correct count", fun() ->
            Weights = faber_nn_nifs:random_weights(10),
            ?assertEqual(10, length(Weights))
        end},
        {"empty list for zero", fun() ->
            Weights = faber_nn_nifs:random_weights(0),
            ?assertEqual([], Weights)
        end},
        {"values in range [-1, 1]", fun() ->
            Weights = faber_nn_nifs:random_weights(100),
            ?assert(lists:all(fun(W) -> W >= -1.0 andalso W =< 1.0 end, Weights))
        end},
        {"different values each call", fun() ->
            W1 = faber_nn_nifs:random_weights(10),
            W2 = faber_nn_nifs:random_weights(10),
            ?assertNotEqual(W1, W2)
        end}
    ]}.

random_weights_seeded_test_() ->
    {setup, fun setup/0, [
        {"same seed produces same weights", fun() ->
            W1 = faber_nn_nifs:random_weights_seeded(10, 42),
            W2 = faber_nn_nifs:random_weights_seeded(10, 42),
            ?assertEqual(W1, W2)
        end},
        {"different seeds produce different weights", fun() ->
            W1 = faber_nn_nifs:random_weights_seeded(10, 42),
            W2 = faber_nn_nifs:random_weights_seeded(10, 43),
            ?assertNotEqual(W1, W2)
        end}
    ]}.

random_weights_gaussian_test_() ->
    {setup, fun setup/0, [
        {"generates correct count", fun() ->
            Weights = faber_nn_nifs:random_weights_gaussian(100, 0.0, 1.0),
            ?assertEqual(100, length(Weights))
        end},
        {"mean approximately correct", fun() ->
            Weights = faber_nn_nifs:random_weights_gaussian(10000, 0.5, 0.1),
            Mean = lists:sum(Weights) / length(Weights),
            ?assert(abs(Mean - 0.5) < 0.05)  %% Within 5% of expected mean
        end}
    ]}.

random_weights_batch_test_() ->
    {setup, fun setup/0, [
        {"generates correct batch sizes", fun() ->
            %% Specs are {Count, Mean, StdDev}. This previously passed a bare
            %% size list, and the NIF silently discarded mean and stddev.
            Batch = faber_nn_nifs:random_weights_batch(
                [{5, 0.0, 1.0}, {10, 0.0, 1.0}, {15, 0.0, 1.0}]
            ),
            ?assertEqual(3, length(Batch)),
            [W1, W2, W3] = Batch,
            ?assertEqual(5, length(W1)),
            ?assertEqual(10, length(W2)),
            ?assertEqual(15, length(W3))
        end}
    ]}.

%%==============================================================================
%% Weight Mutation Tests
%%==============================================================================

mutate_weights_test_() ->
    {setup, fun setup/0, [
        {"preserves length", fun() ->
            Original = [0.1, 0.2, 0.3, 0.4, 0.5],
            Mutated = faber_nn_nifs:mutate_weights(Original, 0.5, 0.8, 0.1),
            ?assertEqual(length(Original), length(Mutated))
        end},
        {"zero mutation rate preserves weights", fun() ->
            Original = [0.1, 0.2, 0.3, 0.4, 0.5],
            Mutated = faber_nn_nifs:mutate_weights(Original, 0.0, 0.8, 0.1),
            ?assertEqual(Original, Mutated)
        end},
        {"full mutation rate changes weights", fun() ->
            Original = [0.5, 0.5, 0.5, 0.5, 0.5],
            Mutated = faber_nn_nifs:mutate_weights(Original, 1.0, 0.8, 0.5),
            ?assertNotEqual(Original, Mutated)
        end}
    ]}.

mutate_weights_seeded_test_() ->
    {setup, fun setup/0, [
        {"same seed produces same mutation", fun() ->
            Original = [0.1, 0.2, 0.3, 0.4, 0.5],
            M1 = faber_nn_nifs:mutate_weights_seeded(Original, 0.5, 0.8, 0.1, 42),
            M2 = faber_nn_nifs:mutate_weights_seeded(Original, 0.5, 0.8, 0.1, 42),
            ?assertEqual(M1, M2)
        end}
    ]}.

mutate_weights_batch_test_() ->
    {setup, fun setup/0, [
        {"batch mutation works", fun() ->
            Genomes = [
                {[0.1, 0.2, 0.3], 0.5, 0.8, 0.1},
                {[0.4, 0.5, 0.6], 0.5, 0.8, 0.1}
            ],
            Results = faber_nn_nifs:mutate_weights_batch(Genomes),
            ?assertEqual(2, length(Results)),
            [R1, R2] = Results,
            ?assertEqual(3, length(R1)),
            ?assertEqual(3, length(R2))
        end}
    ]}.

mutate_weights_batch_uniform_test_() ->
    {setup, fun setup/0, [
        {"uniform batch mutation works", fun() ->
            Genomes = [[0.1, 0.2, 0.3], [0.4, 0.5, 0.6]],
            Results = faber_nn_nifs:mutate_weights_batch_uniform(Genomes, 0.5, 0.8, 0.1),
            ?assertEqual(2, length(Results))
        end}
    ]}.

%%==============================================================================
%% Weight Distance Tests
%%==============================================================================

weight_distance_l1_test_() ->
    {setup, fun setup/0, [
        {"identical weights have zero distance", fun() ->
            W = [0.1, 0.2, 0.3],
            ?assertEqual(0.0, faber_nn_nifs:weight_distance_l1(W, W))
        end},
        {"computes L1 distance correctly", fun() ->
            W1 = [0.0, 0.0, 0.0],
            W2 = [1.0, 1.0, 1.0],
            %% L1 (Manhattan) = |1-0| + |1-0| + |1-0| = 3.
            %% This previously asserted 1.0, because the NIF divided by the
            %% vector length, making it mean absolute deviation rather than
            %% L1. The Erlang fallback returned 3.0, so the two implementations
            %% disagreed by a factor of the vector length.
            ?assertEqual(3.0, faber_nn_nifs:weight_distance_l1(W1, W2))
        end}
    ]}.

weight_distance_l2_test_() ->
    {setup, fun setup/0, [
        {"identical weights have zero distance", fun() ->
            W = [0.1, 0.2, 0.3],
            ?assertEqual(0.0, faber_nn_nifs:weight_distance_l2(W, W))
        end},
        {"computes L2 distance correctly", fun() ->
            W1 = [0.0, 0.0],
            W2 = [3.0, 4.0],
            %% L2 = sqrt(9 + 16) = 5
            Distance = faber_nn_nifs:weight_distance_l2(W1, W2),
            ?assert(Distance > 0.0)  %% Just verify positive distance
        end}
    ]}.

weight_distance_batch_test_() ->
    {setup, fun setup/0, [
        {"batch distance returns one distance per input, in input order", fun() ->
            %% The metric is the atom l1 or l2, and the result is a plain
            %% distance list in input order, matching
            %% tweann_nif_fallback:weight_distance_batch/3.
            %%
            %% This previously passed a boolean and expected {Index, Distance}
            %% pairs sorted ascending. The Erlang fallback has always returned
            %% a plain list, so any caller written against the documented spec
            %% got a badarg from the NIF.
            Target = [0.0, 0.0],
            Others = [[1.0, 0.0], [0.5, 0.0], [2.0, 0.0]],
            Results = faber_nn_nifs:weight_distance_batch(Target, Others, l1),
            ?assertEqual([1.0, 0.5, 2.0], Results)
        end},
        {"l2 metric is accepted", fun() ->
            Target = [0.0, 0.0],
            Others = [[3.0, 4.0]],
            ?assertEqual([5.0], faber_nn_nifs:weight_distance_batch(Target, Others, l2))
        end},
        {"an unknown metric is rejected", fun() ->
            ?assertError(badarg, faber_nn_nifs:weight_distance_batch([0.0], [[1.0]], linf))
        end}
    ]}.

%%==============================================================================
%% Euclidean Distance Tests
%%==============================================================================

euclidean_distance_test_() ->
    {setup, fun setup/0, [
        {"identical vectors have zero distance", fun() ->
            V = [1.0, 2.0, 3.0],
            ?assertEqual(0.0, faber_nn_nifs:euclidean_distance(V, V))
        end},
        {"computes distance correctly", fun() ->
            V1 = [0.0, 0.0],
            V2 = [3.0, 4.0],
            ?assertEqual(5.0, faber_nn_nifs:euclidean_distance(V1, V2))
        end}
    ]}.

euclidean_distance_batch_test_() ->
    {setup, fun setup/0, [
        {"batch distance works", fun() ->
            Target = [0.0, 0.0],
            Others = [[1.0, 0.0], [0.0, 2.0]],
            Results = faber_nn_nifs:euclidean_distance_batch(Target, Others),
            ?assertEqual(2, length(Results))
        end}
    ]}.

%%==============================================================================
%% KNN Novelty Tests
%%==============================================================================

knn_novelty_test_() ->
    {setup, fun setup/0, [
        {"novelty score is non-negative", fun() ->
            Target = [0.5, 0.5],
            Population = [[0.1, 0.1], [0.9, 0.9]],
            Archive = [[0.3, 0.3], [0.7, 0.7]],
            Score = faber_nn_nifs:knn_novelty(Target, Population, Archive, 2),
            ?assert(Score >= 0.0)
        end}
    ]}.

knn_novelty_batch_test_() ->
    {setup, fun setup/0, [
        {"batch novelty works", fun() ->
            Behaviors = [[0.1, 0.1], [0.5, 0.5], [0.9, 0.9]],
            Archive = [[0.0, 0.0], [1.0, 1.0]],
            Scores = faber_nn_nifs:knn_novelty_batch(Behaviors, Archive, 1),
            ?assertEqual(3, length(Scores)),
            ?assert(lists:all(fun(S) -> S >= 0.0 end, Scores))
        end}
    ]}.

%%==============================================================================
%% Fitness Statistics Tests
%%==============================================================================

fitness_stats_test_() ->
    {setup, fun setup/0, [
        {"computes stats correctly", fun() ->
            Fitnesses = [1.0, 2.0, 3.0, 4.0, 5.0],
            {Min, Max, Mean, Variance, StdDev, Sum} = faber_nn_nifs:fitness_stats(Fitnesses),
            ?assertEqual(1.0, Min),
            ?assertEqual(5.0, Max),
            ?assertEqual(3.0, Mean),
            ?assertEqual(15.0, Sum),
            ?assert(Variance > 0),
            ?assert(StdDev > 0)
        end},
        {"single element stats", fun() ->
            {Min, Max, Mean, _Var, _Std, Sum} = faber_nn_nifs:fitness_stats([5.0]),
            ?assertEqual(5.0, Min),
            ?assertEqual(5.0, Max),
            ?assertEqual(5.0, Mean),
            ?assertEqual(5.0, Sum)
        end}
    ]}.

weighted_moving_average_test_() ->
    {setup, fun setup/0, [
        {"WMA computation", fun() ->
            Values = [1.0, 2.0, 3.0, 4.0, 5.0],
            WMA = faber_nn_nifs:weighted_moving_average(Values, 0.9),
            ?assert(is_float(WMA)),
            ?assert(WMA > 0.0)
        end}
    ]}.

shannon_entropy_test_() ->
    {setup, fun setup/0, [
        {"uniform distribution has high entropy", fun() ->
            Values = [0.25, 0.25, 0.25, 0.25],
            Entropy = faber_nn_nifs:shannon_entropy(Values),
            ?assert(Entropy > 1.0)  %% log2(4) = 2 for uniform
        end},
        {"single value has zero entropy", fun() ->
            Values = [1.0],
            Entropy = faber_nn_nifs:shannon_entropy(Values),
            ?assert(abs(Entropy) < 0.001)  %% Close to zero (handles -0.0)
        end}
    ]}.

histogram_test_() ->
    {setup, fun setup/0, [
        {"creates correct bin count", fun() ->
            Values = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9],
            Bins = faber_nn_nifs:histogram(Values, 5, 0.0, 1.0),
            ?assertEqual(5, length(Bins)),
            ?assertEqual(9, lists:sum(Bins))
        end}
    ]}.

%%==============================================================================
%% Selection Tests
%%==============================================================================

build_cumulative_fitness_test_() ->
    {setup, fun setup/0, [
        {"builds cumulative array", fun() ->
            Fitnesses = [1.0, 2.0, 3.0],
            {Cumulative, Total} = faber_nn_nifs:build_cumulative_fitness(Fitnesses),
            ?assertEqual(3, length(Cumulative)),
            ?assertEqual(6.0, Total)
        end}
    ]}.

roulette_select_test_() ->
    {setup, fun setup/0, [
        {"selects valid index", fun() ->
            {Cumulative, Total} = faber_nn_nifs:build_cumulative_fitness([1.0, 2.0, 3.0]),
            Index = faber_nn_nifs:roulette_select(Cumulative, Total, 0.5),
            ?assert(Index >= 0),
            ?assert(Index < 3)
        end}
    ]}.

roulette_select_batch_test_() ->
    {setup, fun setup/0, [
        {"batch selection works", fun() ->
            {Cumulative, Total} = faber_nn_nifs:build_cumulative_fitness([1.0, 2.0, 3.0]),
            Indices = faber_nn_nifs:roulette_select_batch(Cumulative, Total, [0.1, 0.5, 0.9]),
            ?assertEqual(3, length(Indices)),
            ?assert(lists:all(fun(I) -> I >= 0 andalso I < 3 end, Indices))
        end}
    ]}.

tournament_select_test_() ->
    {setup, fun setup/0, [
        {"selects highest fitness contestant", fun() ->
            Fitnesses = [1.0, 5.0, 3.0, 2.0],
            %% Contestants are indices 0, 1, 2
            Winner = faber_nn_nifs:tournament_select([0, 1, 2], Fitnesses),
            ?assertEqual(1, Winner)  %% Index 1 has fitness 5.0
        end}
    ]}.

%%==============================================================================
%% Reward and Meta-Controller Tests
%%==============================================================================

z_score_test_() ->
    {setup, fun setup/0, [
        {"computes z-score correctly", fun() ->
            %% Value=10, Mean=5, StdDev=2 => z = (10-5)/2 = 2.5
            Z = faber_nn_nifs:z_score(10.0, 5.0, 2.0),
            ?assertEqual(2.5, Z)
        end},
        {"handles zero stddev", fun() ->
            Z = faber_nn_nifs:z_score(5.0, 5.0, 0.0),
            ?assertEqual(0.0, Z)
        end}
    ]}.

compute_reward_component_test_() ->
    {setup, fun setup/0, [
        {"computes reward component", fun() ->
            History = [1.0, 2.0, 3.0, 4.0],
            {Raw, Normalized, ZScore} = faber_nn_nifs:compute_reward_component(History, 5.0),
            ?assert(is_float(Raw)),
            ?assert(is_float(Normalized)),
            ?assert(is_float(ZScore))
        end}
    ]}.

compute_weighted_reward_test_() ->
    {setup, fun setup/0, [
        {"computes weighted reward", fun() ->
            Components = [
                {[1.0, 2.0, 3.0], 4.0, 0.5},
                {[2.0, 3.0, 4.0], 5.0, 0.5}
            ],
            Reward = faber_nn_nifs:compute_weighted_reward(Components),
            ?assert(is_float(Reward))
        end}
    ]}.

%%==============================================================================
%% Signal Aggregation Tests
%%==============================================================================

dot_product_flat_test_() ->
    {setup, fun setup/0, [
        {"computes dot product correctly", fun() ->
            Signals = [1.0, 2.0, 3.0],
            Weights = [0.5, 0.5, 0.5],
            Bias = 1.0,
            %% 1*0.5 + 2*0.5 + 3*0.5 + 1.0 = 0.5 + 1.0 + 1.5 + 1.0 = 4.0
            Result = faber_nn_nifs:dot_product_flat(Signals, Weights, Bias),
            ?assertEqual(4.0, Result)
        end}
    ]}.

dot_product_batch_test_() ->
    {setup, fun setup/0, [
        {"batch dot product works", fun() ->
            Batch = [
                {[1.0, 2.0], [0.5, 0.5], 0.0},
                {[3.0, 4.0], [0.5, 0.5], 1.0}
            ],
            Results = faber_nn_nifs:dot_product_batch(Batch),
            ?assertEqual(2, length(Results)),
            [R1, R2] = Results,
            ?assertEqual(1.5, R1),  %% 1*0.5 + 2*0.5 = 1.5
            ?assertEqual(4.5, R2)   %% 3*0.5 + 4*0.5 + 1 = 4.5
        end}
    ]}.

%%==============================================================================
%% CfC/LTC Tests
%%==============================================================================

evaluate_cfc_test_() ->
    {setup, fun setup/0, [
        {"CfC evaluation returns tuple", fun() ->
            {Output, NewState} = faber_nn_nifs:evaluate_cfc(1.0, 0.0, 1.0, 1.0),
            ?assert(is_float(Output)),
            ?assert(is_float(NewState))
        end}
    ]}.

evaluate_cfc_batch_test_() ->
    {setup, fun setup/0, [
        {"batch CfC evaluation works", fun() ->
            Inputs = [0.1, 0.2, 0.3, 0.4, 0.5],
            Results = faber_nn_nifs:evaluate_cfc_batch(Inputs, 0.0, 1.0, 1.0),
            ?assertEqual(5, length(Results)),
            ?assert(lists:all(fun({O, S}) -> is_float(O) andalso is_float(S) end, Results))
        end}
    ]}.

evaluate_ode_test_() ->
    {setup, fun setup/0, [
        {"ODE evaluation returns tuple", fun() ->
            {Output, NewState} = faber_nn_nifs:evaluate_ode(1.0, 0.0, 1.0, 1.0, 0.01),
            ?assert(is_float(Output)),
            ?assert(is_float(NewState))
        end}
    ]}.

%%==============================================================================
%% Network Evaluation Tests (if network compilation available)
%%==============================================================================

network_evaluation_test_() ->
    {setup, fun setup/0, [
        {"network compilation and evaluation", fun() ->
            %% Simple XOR-like network
            Nodes = [
                {0, input, none, 0.0, []},
                {1, input, none, 0.0, []},
                {2, hidden, tanh, 0.0, [{0, 1.0}, {1, 1.0}]},
                {3, output, tanh, 0.0, [{2, 1.0}]}
            ],
            Network = faber_nn_nifs:compile_network(Nodes, 2, [3]),
            Outputs = faber_nn_nifs:evaluate(Network, [0.5, 0.5]),
            ?assertEqual(1, length(Outputs)),
            [Out] = Outputs,
            ?assert(is_float(Out))
        end},
        {"batch evaluation", fun() ->
            Nodes = [
                {0, input, none, 0.0, []},
                {1, output, tanh, 0.0, [{0, 1.0}]}
            ],
            Network = faber_nn_nifs:compile_network(Nodes, 1, [1]),
            InputsList = [[0.1], [0.5], [0.9]],
            OutputsList = faber_nn_nifs:evaluate_batch(Network, InputsList),
            ?assertEqual(3, length(OutputsList))
        end}
    ]}.

compatibility_distance_test_() ->
    {setup, fun setup/0, [
        {"identical connections have zero distance", fun() ->
            Conns = [{1, 0.5}, {2, 0.3}, {3, 0.7}],
            Distance = faber_nn_nifs:compatibility_distance(Conns, Conns, 1.0, 1.0, 0.4),
            ?assertEqual(0.0, Distance)
        end},
        {"different connections have positive distance", fun() ->
            ConnsA = [{1, 0.5}, {2, 0.3}],
            ConnsB = [{1, 0.5}, {3, 0.7}],
            Distance = faber_nn_nifs:compatibility_distance(ConnsA, ConnsB, 1.0, 1.0, 0.4),
            ?assert(Distance > 0.0)
        end}
    ]}.

%%==============================================================================
%% P0: Layer-specific Mutation Tests
%%==============================================================================

mutate_weights_layered_test_() ->
    {setup, fun setup/0, [
        {"applies different rates to reservoir vs readout", fun() ->
            %% 10 weights: 6 reservoir, 4 readout
            Weights = [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5],
            Mutated = faber_nn_nifs:mutate_weights_layered(
                Weights, 6,
                1.0, 0.1,  %% reservoir: full mutation rate, low strength
                1.0, 0.5   %% readout: full mutation rate, high strength
            ),
            ?assertEqual(10, length(Mutated))
        end},
        {"preserves weights with zero mutation rate", fun() ->
            Weights = [0.1, 0.2, 0.3, 0.4, 0.5],
            Mutated = faber_nn_nifs:mutate_weights_layered(
                Weights, 3,
                0.0, 0.1,  %% reservoir: no mutation
                0.0, 0.1   %% readout: no mutation
            ),
            ?assertEqual(Weights, Mutated)
        end}
    ]}.

compute_layer_weight_counts_test_() ->
    {setup, fun setup/0, [
        {"counts weights per layer correctly", fun() ->
            %% Simple network: 2 inputs -> 2 hidden (4 weights) -> 1 output (2 weights)
            LayerSizes = [2, 2, 1],
            Counts = faber_nn_nifs:compute_layer_weight_counts(LayerSizes),
            ?assertEqual([4, 2], Counts)
        end},
        {"handles single layer", fun() ->
            Counts = faber_nn_nifs:compute_layer_weight_counts([5]),
            ?assertEqual([], Counts)
        end}
    ]}.

%%==============================================================================
%% P0: SIMD Batch Activations Tests
%%==============================================================================

tanh_batch_test_() ->
    {setup, fun setup/0, [
        {"applies tanh to all values", fun() ->
            Values = [0.0, 1.0, -1.0, 2.0, -2.0],
            Results = faber_nn_nifs:tanh_batch(Values),
            ?assertEqual(5, length(Results)),
            %% tanh(0) = 0
            [R1 | _] = Results,
            ?assert(abs(R1) < 0.001)
        end},
        {"values in range [-1, 1]", fun() ->
            Values = [100.0, -100.0, 0.5, -0.5],
            Results = faber_nn_nifs:tanh_batch(Values),
            ?assert(lists:all(fun(R) -> R >= -1.0 andalso R =< 1.0 end, Results))
        end}
    ]}.

sigmoid_batch_test_() ->
    {setup, fun setup/0, [
        {"applies sigmoid to all values", fun() ->
            Values = [0.0, 1.0, -1.0],
            Results = faber_nn_nifs:sigmoid_batch(Values),
            ?assertEqual(3, length(Results)),
            %% sigmoid(0) = 0.5
            [R1 | _] = Results,
            ?assert(abs(R1 - 0.5) < 0.001)
        end},
        {"values in range [0, 1]", fun() ->
            Values = [100.0, -100.0, 0.5, -0.5],
            Results = faber_nn_nifs:sigmoid_batch(Values),
            ?assert(lists:all(fun(R) -> R >= 0.0 andalso R =< 1.0 end, Results))
        end}
    ]}.

relu_batch_test_() ->
    {setup, fun setup/0, [
        {"applies ReLU to all values", fun() ->
            Values = [1.0, -1.0, 0.0, 2.0, -2.0],
            Results = faber_nn_nifs:relu_batch(Values),
            ?assertEqual([1.0, 0.0, 0.0, 2.0, 0.0], Results)
        end}
    ]}.

softmax_batch_test_() ->
    {setup, fun setup/0, [
        {"outputs sum to 1.0", fun() ->
            Values = [1.0, 2.0, 3.0],
            Results = faber_nn_nifs:softmax_batch(Values),
            Sum = lists:sum(Results),
            ?assert(abs(Sum - 1.0) < 0.001)
        end},
        {"all values non-negative", fun() ->
            Values = [-1.0, 0.0, 1.0],
            Results = faber_nn_nifs:softmax_batch(Values),
            ?assert(lists:all(fun(R) -> R >= 0.0 end, Results))
        end}
    ]}.

activation_batch_test_() ->
    {setup, fun setup/0, [
        {"tanh activation works", fun() ->
            Values = [0.0, 1.0],
            Results = faber_nn_nifs:activation_batch(Values, tanh),
            ?assertEqual(2, length(Results))
        end},
        {"sigmoid activation works", fun() ->
            Values = [0.0, 1.0],
            Results = faber_nn_nifs:activation_batch(Values, sigmoid),
            [R1 | _] = Results,
            ?assert(abs(R1 - 0.5) < 0.001)
        end},
        {"relu activation works", fun() ->
            Values = [1.0, -1.0],
            Results = faber_nn_nifs:activation_batch(Values, relu),
            ?assertEqual([1.0, 0.0], Results)
        end}
    ]}.

%%==============================================================================
%% P1: Plasticity Computation Tests
%%==============================================================================

hebbian_update_batch_test_() ->
    {setup, fun setup/0, [
        {"computes Hebbian weight updates", fun() ->
            %% (weight, pre_activity, post_activity)
            WeightActivities = [{0.5, 0.8, 0.9}, {0.3, 0.1, 0.2}],
            LearningRate = 0.01,
            DecayRate = 0.001,
            MaxWeight = 1.0,
            Results = faber_nn_nifs:hebbian_update_batch(
                WeightActivities, LearningRate, DecayRate, MaxWeight
            ),
            ?assertEqual(2, length(Results)),
            ?assert(lists:all(fun(R) -> is_float(R) end, Results))
        end}
    ]}.

modulated_hebbian_batch_test_() ->
    {setup, fun setup/0, [
        {"computes modulated Hebbian updates", fun() ->
            WeightActivities = [{0.5, 0.8, 0.9}, {0.3, 0.1, 0.2}],
            LearningRate = 0.01,
            Reward = 0.5,
            DecayRate = 0.001,
            MaxWeight = 1.0,
            Results = faber_nn_nifs:modulated_hebbian_batch(
                WeightActivities, LearningRate, Reward, DecayRate, MaxWeight
            ),
            ?assertEqual(2, length(Results))
        end},
        {"zero reward prevents learning", fun() ->
            WeightActivities = [{0.5, 1.0, 1.0}],
            Results = faber_nn_nifs:modulated_hebbian_batch(
                WeightActivities, 0.1, 0.0, 0.0, 1.0
            ),
            [R] = Results,
            ?assert(abs(R - 0.5) < 0.01)  %% Should be near original weight
        end}
    ]}.

stdp_update_test_() ->
    {setup, fun setup/0, [
        {"pre-before-post strengthens", fun() ->
            Weight = 0.5,
            DeltaT = 10.0,  %% Post fires 10ms after pre (positive = LTP)
            Aplus = 0.1,
            Aminus = 0.1,
            Tau = 20.0,
            NewWeight = faber_nn_nifs:stdp_update(Weight, DeltaT, Aplus, Aminus, Tau),
            ?assert(NewWeight >= Weight)  %% LTP
        end},
        {"post-before-pre weakens", fun() ->
            Weight = 0.5,
            DeltaT = -10.0,  %% Pre fires 10ms after post (negative = LTD)
            Aplus = 0.1,
            Aminus = 0.1,
            Tau = 20.0,
            NewWeight = faber_nn_nifs:stdp_update(Weight, DeltaT, Aplus, Aminus, Tau),
            ?assert(NewWeight =< Weight)  %% LTD
        end}
    ]}.

oja_update_batch_test_() ->
    {setup, fun setup/0, [
        {"computes Oja rule updates", fun() ->
            WeightActivities = [{0.5, 0.8, 0.9}, {0.3, 0.1, 0.2}],
            LearningRate = 0.01,
            Results = faber_nn_nifs:oja_update_batch(WeightActivities, LearningRate, 0.0, 1.0),
            ?assertEqual(2, length(Results))
        end}
    ]}.

%%==============================================================================
%% P1: Time Series LTC/CfC Tests
%%==============================================================================

evaluate_cfc_sequence_test_() ->
    {setup, fun setup/0, [
        {"processes sequence of inputs", fun() ->
            Inputs = [0.1, 0.2, 0.3, 0.4, 0.5],
            InitState = 0.0,
            Tau = 1.0,
            Bound = 1.0,
            BackboneWeights = [],
            Results = faber_nn_nifs:evaluate_cfc_sequence(Inputs, InitState, Tau, Bound, BackboneWeights),
            ?assertEqual(5, length(Results))
        end},
        {"returns state and output tuples", fun() ->
            Inputs = [0.1, 0.2, 0.3],
            Results = faber_nn_nifs:evaluate_cfc_sequence(Inputs, 0.0, 1.0, 1.0, []),
            ?assertEqual(3, length(Results)),
            ?assert(lists:all(fun({S, O}) -> is_float(S) andalso is_float(O) end, Results))
        end}
    ]}.

evaluate_cfc_parallel_test_() ->
    {setup, fun setup/0, [
        {"evaluates multiple neurons in parallel", fun() ->
            Input = 0.5,
            %% NeuronParams: list of {state, tau, bound}
            NeuronParams = [{0.0, 1.0, 1.0}, {0.1, 2.0, 0.5}],
            BackboneWeights = [],
            HeadWeights = [],
            Results = faber_nn_nifs:evaluate_cfc_parallel(Input, NeuronParams, BackboneWeights, HeadWeights),
            ?assertEqual(2, length(Results))
        end}
    ]}.

ltc_state_batch_test_() ->
    {setup, fun setup/0, [
        {"computes LTC state updates", fun() ->
            Inputs = [0.5, 0.7],
            States = [0.0, 0.1],
            Taus = [1.0, 2.0],
            Dt = 0.01,
            Results = faber_nn_nifs:ltc_state_batch(Inputs, States, Taus, Dt),
            ?assertEqual(2, length(Results))
        end}
    ]}.

%%==============================================================================
%% P1: Population Diversity Tests
%%==============================================================================

population_diversity_test_() ->
    {setup, fun setup/0, [
        {"computes diversity metrics", fun() ->
            Population = [[0.1, 0.2], [0.3, 0.4], [0.5, 0.6]],
            {MeanDist, StdDist, MinDist, MaxDist} = faber_nn_nifs:population_diversity(Population),
            ?assert(is_float(MeanDist)),
            ?assert(is_float(StdDist)),
            ?assert(is_float(MinDist)),
            ?assert(is_float(MaxDist)),
            ?assert(MaxDist >= MinDist)
        end},
        {"identical population has zero diversity", fun() ->
            Population = [[0.5, 0.5], [0.5, 0.5], [0.5, 0.5]],
            {MeanDist, StdDist, MinDist, MaxDist} = faber_nn_nifs:population_diversity(Population),
            ?assertEqual(0.0, MeanDist),
            ?assertEqual(0.0, StdDist),
            ?assertEqual(0.0, MinDist),
            ?assertEqual(0.0, MaxDist)
        end}
    ]}.

weight_covariance_matrix_test_() ->
    {setup, fun setup/0, [
        {"computes covariance matrix", fun() ->
            Population = [[0.1, 0.2, 0.3], [0.4, 0.5, 0.6], [0.7, 0.8, 0.9]],
            CovMatrix = faber_nn_nifs:weight_covariance_matrix(Population),
            %% 3x3 covariance matrix = 9 elements (flattened row-major)
            ?assertEqual(9, length(CovMatrix)),
            ?assert(lists:all(fun(V) -> is_float(V) end, CovMatrix))
        end}
    ]}.

pairwise_distances_batch_test_() ->
    {setup, fun setup/0, [
        {"computes pairwise L2 distances", fun() ->
            Genomes = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0]],
            Distances = faber_nn_nifs:pairwise_distances_batch(Genomes, l2),
            %% N*(N-1)/2 = 3*2/2 = 3 pairs
            ?assertEqual(3, length(Distances))
        end},
        {"computes pairwise L1 distances", fun() ->
            Genomes = [[0.0, 0.0], [1.0, 1.0]],
            Distances = faber_nn_nifs:pairwise_distances_batch(Genomes, l1),
            ?assertEqual(1, length(Distances)),
            %% L1 distance between [0,0] and [1,1] = 2.0
            [D] = Distances,
            ?assertEqual(2.0, D)
        end}
    ]}.

%%==============================================================================
%% P2: NEAT Crossover Tests
%%==============================================================================

neat_crossover_test_() ->
    {setup, fun setup/0, [
        {"crosses over matching genes", fun() ->
            %% Parent A genes: {innovation_number, weight, enabled}
            ParentA = [{1, 0.5, true}, {2, 0.3, true}, {3, 0.7, true}],
            %% Parent B genes (same innovations)
            ParentB = [{1, 0.6, true}, {2, 0.4, true}, {3, 0.8, true}],
            Offspring = faber_nn_nifs:neat_crossover(ParentA, ParentB, 1.0, 1.0),
            ?assertEqual(3, length(Offspring)),
            %% Each gene is {innov, weight, enabled}
            ?assert(lists:all(fun({I, W, E}) ->
                is_integer(I) andalso is_float(W) andalso is_boolean(E)
            end, Offspring))
        end},
        {"includes excess/disjoint from fitter parent", fun() ->
            %% Fitter parent has extra genes
            ParentA = [{1, 0.5, true}, {2, 0.3, true}, {4, 0.9, true}],
            ParentB = [{1, 0.6, true}, {3, 0.7, true}],
            Offspring = faber_nn_nifs:neat_crossover(ParentA, ParentB, 10.0, 5.0),
            %% Fitter parent A (fitness 10) contributes disjoint/excess
            %% Should have innovations 1, 2, 4 from A, plus 3 from A or B
            ?assert(length(Offspring) >= 3)
        end}
    ]}.

align_genes_by_innovation_test_() ->
    {setup, fun setup/0, [
        {"aligns genes by innovation number", fun() ->
            %% Genes must be 3-tuples: {innovation, weight, enabled}
            GenesA = [{1, 0.5, true}, {2, 0.3, true}, {4, 0.9, true}],
            GenesB = [{1, 0.6, true}, {3, 0.7, true}, {4, 0.8, true}],
            Aligned = faber_nn_nifs:align_genes_by_innovation(GenesA, GenesB),
            %% Returns list of {GeneA | nil, GeneB | nil} for each innovation
            %% Innovations 1, 2, 3, 4 = 4 entries
            ?assertEqual(4, length(Aligned)),
            %% Each entry is {A, B} where A/B is gene tuple or nil
            ?assert(lists:all(fun({A, B}) ->
                (A == nil orelse is_tuple(A)) andalso
                (B == nil orelse is_tuple(B))
            end, Aligned))
        end}
    ]}.

count_excess_disjoint_test_() ->
    {setup, fun setup/0, [
        {"counts excess and disjoint genes", fun() ->
            %% Genes must be 3-tuples: {innovation, weight, enabled}
            GenesA = [{1, 0.5, true}, {2, 0.3, true}, {5, 0.9, true}],
            GenesB = [{1, 0.6, true}, {3, 0.7, true}],
            {Excess, Disjoint, Matching} = faber_nn_nifs:count_excess_disjoint(GenesA, GenesB),
            ?assert(is_integer(Excess)),
            ?assert(is_integer(Disjoint)),
            ?assert(is_integer(Matching)),
            ?assert(Excess >= 0),
            ?assert(Disjoint >= 0),
            %% Innovation 1 is matching
            ?assertEqual(1, Matching)
        end}
    ]}.

%%==============================================================================
%% P2: Speciation Clustering Tests
%%==============================================================================

assign_species_batch_test_() ->
    {setup, fun setup/0, [
        {"assigns genomes to species", fun() ->
            Genomes = [[0.1, 0.2], [0.15, 0.25], [0.9, 0.8], [0.85, 0.75]],
            Representatives = [[0.1, 0.2], [0.9, 0.8]],
            Threshold = 0.5,
            Assignments = faber_nn_nifs:assign_species_batch(
                Genomes, Representatives, Threshold
            ),
            ?assertEqual(4, length(Assignments)),
            %% Each assignment is a species index
            ?assert(lists:all(fun(A) -> is_integer(A) andalso A >= 0 end, Assignments))
        end}
    ]}.

find_representative_test_() ->
    {setup, fun setup/0, [
        {"finds representative closest to centroid", fun() ->
            Members = [[0.0, 0.0], [1.0, 1.0], [0.4, 0.5]],
            RepIdx = faber_nn_nifs:find_representative(Members, centroid),
            ?assert(RepIdx >= 0),
            ?assert(RepIdx < 3)
        end},
        {"handles single member", fun() ->
            Members = [[0.5, 0.5]],
            RepIdx = faber_nn_nifs:find_representative(Members, centroid),
            ?assertEqual(0, RepIdx)
        end}
    ]}.

kmeans_cluster_test_() ->
    {setup, fun setup/0, [
        {"clusters genomes into k groups", fun() ->
            Genomes = [
                [0.1, 0.1], [0.15, 0.12], [0.08, 0.11],  %% Cluster 1
                [0.9, 0.9], [0.88, 0.91], [0.92, 0.87]   %% Cluster 2
            ],
            Assignments = faber_nn_nifs:kmeans_cluster(Genomes, 2, 100),
            ?assertEqual(6, length(Assignments)),
            %% All assignments should be 0 or 1 (2 clusters)
            ?assert(lists:all(fun(A) -> A == 0 orelse A == 1 end, Assignments))
        end},
        {"handles k=1", fun() ->
            Genomes = [[0.1, 0.1], [0.9, 0.9]],
            Assignments = faber_nn_nifs:kmeans_cluster(Genomes, 1, 10),
            ?assertEqual([0, 0], Assignments)
        end}
    ]}.

%%==============================================================================
%% P3: Matrix Operations Tests
%%==============================================================================

matmul_add_bias_test_() ->
    {setup, fun setup/0, [
        {"computes matrix multiplication with bias", fun() ->
            %% X: 3-element input vector
            %% W: flattened 3x2 weight matrix (input_dim=3 x output_dim=2, row-major)
            %% B: 2-element bias vector
            Input = [1.0, 1.0, 1.0],
            %% Weights: 3 inputs x 2 outputs = 6 values
            %% Column-major order for output: W[i * output_dim + j]
            %% Row 0: [0.5, 1.0], Row 1: [0.5, 1.0], Row 2: [0.5, 1.0]
            Weights = [0.5, 1.0, 0.5, 1.0, 0.5, 1.0],
            Bias = [0.0, 1.0],
            Result = faber_nn_nifs:matmul_add_bias(Input, Weights, Bias),
            ?assertEqual(2, length(Result)),
            %% First output: 0.5+0.5+0.5+0 = 1.5
            %% Second output: 1+1+1+1 = 4.0
            [R1, R2] = Result,
            ?assert(abs(R1 - 1.5) < 0.001),
            ?assert(abs(R2 - 4.0) < 0.001)
        end}
    ]}.

layer_forward_test_() ->
    {setup, fun setup/0, [
        {"computes layer forward pass with activation", fun() ->
            Input = [0.5, 0.5],
            %% Flattened weights: 2 inputs x 1 output
            Weights = [1.0, 1.0],
            Bias = [0.0],
            Result = faber_nn_nifs:layer_forward(Input, Weights, Bias, tanh),
            ?assertEqual(1, length(Result)),
            [R] = Result,
            %% tanh(0.5+0.5) = tanh(1.0) ≈ 0.7616
            ?assert(abs(R - 0.7616) < 0.01)
        end}
    ]}.

multi_layer_forward_test_() ->
    {setup, fun setup/0, [
        {"computes multi-layer forward pass", fun() ->
            Input = [0.5, 0.5],
            %% Layers: [{flattened_weights, biases, activation}, ...]
            %% Layer 1: 2 inputs -> 2 outputs (identity-ish)
            %% Flattened weights for 2x2: [w11, w12, w21, w22]
            Layer1 = {[1.0, 0.0, 0.0, 1.0], [0.0, 0.0], tanh},
            %% Layer 2: 2 inputs -> 1 output (sum)
            Layer2 = {[1.0, 1.0], [0.0], tanh},
            Layers = [Layer1, Layer2],
            LayerSizes = [2, 2, 1],
            Result = faber_nn_nifs:multi_layer_forward(Input, Layers, LayerSizes),
            ?assertEqual(1, length(Result))
        end},
        {"simple two-layer network", fun() ->
            Input = [1.0, 0.0],
            %% Layer 1: 2 inputs -> 1 output
            Layer1 = {[1.0, 1.0], [0.0], tanh},
            Layers = [Layer1],
            LayerSizes = [2, 1],
            Result = faber_nn_nifs:multi_layer_forward(Input, Layers, LayerSizes),
            ?assertEqual(1, length(Result)),
            [R] = Result,
            %% tanh(1.0) ≈ 0.7616
            ?assert(abs(R - 0.7616) < 0.01)
        end}
    ]}.
