%% @doc Unit tests for TWEANN NIF v2 functions.
%%
%% Tests for distance, kNN, statistics, selection, and reward NIFs.
%% These tests require the NIF to be compiled first.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(tweann_nif_v2_tests).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Distance Function Tests
%%==============================================================================

euclidean_distance_basic_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Distance between [0,0] and [3,4] should be 5
            Distance = tweann_nif:euclidean_distance([0.0, 0.0], [3.0, 4.0]),
            ?assert(abs(Distance - 5.0) < 0.001)
    end.

euclidean_distance_same_vector_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Distance between same vectors should be 0
            Distance = tweann_nif:euclidean_distance([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]),
            ?assert(abs(Distance) < 0.001)
    end.

euclidean_distance_1d_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Distance = tweann_nif:euclidean_distance([5.0], [10.0]),
            ?assert(abs(Distance - 5.0) < 0.001)
    end.

euclidean_distance_batch_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Target = [0.0, 0.0],
            Others = [
                [3.0, 4.0],   %% Distance 5
                [1.0, 0.0],   %% Distance 1
                [0.0, 2.0]    %% Distance 2
            ],
            Results = tweann_nif:euclidean_distance_batch(Target, Others),
            ?assertEqual(3, length(Results)),
            %% Results are sorted by distance
            [{Idx0, D0}, {Idx1, D1}, {Idx2, D2}] = Results,
            ?assertEqual(1, Idx0),  %% [1,0] is closest
            ?assert(abs(D0 - 1.0) < 0.001),
            ?assertEqual(2, Idx1),  %% [0,2] is second
            ?assert(abs(D1 - 2.0) < 0.001),
            ?assertEqual(0, Idx2),  %% [3,4] is farthest
            ?assert(abs(D2 - 5.0) < 0.001)
    end.

euclidean_distance_batch_empty_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Results = tweann_nif:euclidean_distance_batch([1.0, 2.0], []),
            ?assertEqual([], Results)
    end.

%%==============================================================================
%% KNN Novelty Tests
%%==============================================================================

knn_novelty_basic_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Target = [0.0, 0.0],
            Population = [[1.0, 0.0], [0.0, 1.0], [2.0, 0.0]],
            Archive = [[5.0, 5.0]],
            K = 2,
            Novelty = tweann_nif:knn_novelty(Target, Population, Archive, K),
            ?assert(is_float(Novelty)),
            ?assert(Novelty >= 0.0)
    end.

knn_novelty_isolated_point_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Far point should have high novelty
            Target = [100.0, 100.0],
            Population = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0]],
            Archive = [],
            K = 2,
            Novelty = tweann_nif:knn_novelty(Target, Population, Archive, K),
            ?assert(Novelty > 50.0)  %% Should be very novel
    end.

knn_novelty_k_larger_than_pool_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Target = [0.0, 0.0],
            Population = [[1.0, 0.0]],
            Archive = [],
            K = 10,  %% K > available points
            Novelty = tweann_nif:knn_novelty(Target, Population, Archive, K),
            ?assert(is_float(Novelty)),
            ?assert(Novelty >= 0.0)
    end.

knn_novelty_batch_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Behaviors = [
                [0.0, 0.0],
                [50.0, 50.0],
                [100.0, 100.0]
            ],
            Archive = [[0.0, 0.0], [1.0, 0.0]],
            K = 2,
            Results = tweann_nif:knn_novelty_batch(Behaviors, Archive, K),
            ?assertEqual(3, length(Results)),
            [Nov0, Nov1, Nov2] = Results,
            %% Point at origin should have low novelty
            ?assert(Nov0 < Nov1),
            %% Most distant point should have highest novelty
            ?assert(Nov2 > Nov1)
    end.

%%==============================================================================
%% Statistics Function Tests
%%==============================================================================

fitness_stats_basic_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Fitnesses = [1.0, 2.0, 3.0, 4.0, 5.0],
            {Min, Max, Mean, Variance, StdDev, Sum} =
                tweann_nif:fitness_stats(Fitnesses),
            ?assert(abs(Min - 1.0) < 0.001),
            ?assert(abs(Max - 5.0) < 0.001),
            ?assert(abs(Mean - 3.0) < 0.001),
            ?assert(abs(Variance - 2.0) < 0.001),  %% Population variance
            ?assert(abs(StdDev - 1.4142) < 0.01),
            ?assert(abs(Sum - 15.0) < 0.001)
    end.

fitness_stats_single_value_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {Min, Max, Mean, Variance, StdDev, Sum} =
                tweann_nif:fitness_stats([42.0]),
            ?assert(abs(Min - 42.0) < 0.001),
            ?assert(abs(Max - 42.0) < 0.001),
            ?assert(abs(Mean - 42.0) < 0.001),
            ?assert(abs(Variance) < 0.001),
            ?assert(abs(StdDev) < 0.001),
            ?assert(abs(Sum - 42.0) < 0.001)
    end.

fitness_stats_sum_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Verify sum is correct
            {_, _, _, _, _, Sum} = tweann_nif:fitness_stats([1.0, 2.0, 3.0, 4.0]),
            ?assert(abs(Sum - 10.0) < 0.001)
    end.

weighted_moving_average_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Values = [1.0, 2.0, 3.0, 4.0, 5.0],
            Decay = 0.9,
            WMA = tweann_nif:weighted_moving_average(Values, Decay),
            ?assert(is_float(WMA)),
            %% Earlier values get higher weight with exponential decay from start
            %% w[i] = decay^i, so first element gets weight 1, second 0.9, etc.
            %% This means earlier values dominate
            ?assert(WMA > 1.0),
            ?assert(WMA < 5.0)
    end.

weighted_moving_average_no_decay_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Values = [1.0, 2.0, 3.0, 4.0, 5.0],
            Decay = 1.0,  %% Equal weighting
            WMA = tweann_nif:weighted_moving_average(Values, Decay),
            ?assert(abs(WMA - 3.0) < 0.001)  %% Simple average
    end.

shannon_entropy_uniform_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Uniform distribution should have high entropy
            Values = [0.25, 0.25, 0.25, 0.25],
            Entropy = tweann_nif:shannon_entropy(Values),
            %% NIF uses natural log, so max entropy = ln(4) ~ 1.386
            ?assert(abs(Entropy - 1.386) < 0.01)
    end.

shannon_entropy_concentrated_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Concentrated distribution should have low entropy
            Values = [0.97, 0.01, 0.01, 0.01],
            Entropy = tweann_nif:shannon_entropy(Values),
            ?assert(Entropy < 0.5)
    end.

histogram_basic_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Values = [0.1, 0.2, 0.3, 0.6, 0.7, 0.8, 0.9],
            NumBins = 2,
            MinVal = 0.0,
            MaxVal = 1.0,
            Hist = tweann_nif:histogram(Values, NumBins, MinVal, MaxVal),
            ?assertEqual(2, length(Hist)),
            [Bin1, Bin2] = Hist,
            ?assertEqual(3, Bin1),  %% 0.1, 0.2, 0.3 in [0, 0.5)
            ?assertEqual(4, Bin2)   %% 0.6, 0.7, 0.8, 0.9 in [0.5, 1.0]
    end.

histogram_out_of_range_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Values = [-1.0, 0.5, 2.0],  %% -1 and 2 are out of range
            NumBins = 2,
            Hist = tweann_nif:histogram(Values, NumBins, 0.0, 1.0),
            [Bin1, Bin2] = Hist,
            %% Out of range values are ignored
            ?assertEqual(0, Bin1),  %% 0.5 goes to second bin [0.5, 1.0]
            ?assertEqual(1, Bin2)
    end.

%%==============================================================================
%% Selection Function Tests
%%==============================================================================

build_cumulative_fitness_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Fitnesses = [1.0, 2.0, 3.0, 4.0],
            {Cumulative, Total} = tweann_nif:build_cumulative_fitness(Fitnesses),
            ?assertEqual(4, length(Cumulative)),
            ?assert(abs(Total - 10.0) < 0.001),
            [C1, C2, C3, C4] = Cumulative,
            ?assert(abs(C1 - 1.0) < 0.001),
            ?assert(abs(C2 - 3.0) < 0.001),
            ?assert(abs(C3 - 6.0) < 0.001),
            ?assert(abs(C4 - 10.0) < 0.001)
    end.

roulette_select_basic_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Cumulative = [1.0, 3.0, 6.0, 10.0],
            Total = 10.0,
            %% Select at different random values
            Idx0 = tweann_nif:roulette_select(Cumulative, Total, 0.05),  %% 0.5
            Idx1 = tweann_nif:roulette_select(Cumulative, Total, 0.15),  %% 1.5
            Idx2 = tweann_nif:roulette_select(Cumulative, Total, 0.45),  %% 4.5
            Idx3 = tweann_nif:roulette_select(Cumulative, Total, 0.95),  %% 9.5
            ?assertEqual(0, Idx0),  %% First bin
            ?assertEqual(1, Idx1),  %% Second bin
            ?assertEqual(2, Idx2),  %% Third bin
            ?assertEqual(3, Idx3)   %% Fourth bin
    end.

roulette_select_batch_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Cumulative = [1.0, 3.0, 6.0, 10.0],
            Total = 10.0,
            RandomVals = [0.05, 0.25, 0.55, 0.95],
            Indices = tweann_nif:roulette_select_batch(Cumulative, Total, RandomVals),
            ?assertEqual(4, length(Indices)),
            ?assert(lists:all(fun(I) -> I >= 0 andalso I < 4 end, Indices))
    end.

tournament_select_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Contestants = [0, 2, 4],
            Fitnesses = [10.0, 5.0, 20.0, 3.0, 15.0],  %% Index 4 has fitness 15.0
            Winner = tweann_nif:tournament_select(Contestants, Fitnesses),
            %% Winner should be index with highest fitness among contestants
            %% Contestants: 0->10.0, 2->20.0, 4->15.0
            ?assertEqual(2, Winner)  %% Index 2 has fitness 20.0
    end.

tournament_select_single_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Contestants = [1],
            Fitnesses = [5.0, 10.0, 15.0],
            Winner = tweann_nif:tournament_select(Contestants, Fitnesses),
            ?assertEqual(1, Winner)  %% Only contestant wins
    end.

%%==============================================================================
%% Reward and Meta-Controller Function Tests
%%==============================================================================

z_score_basic_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Value = 10.0,
            Mean = 5.0,
            StdDev = 2.5,
            ZScore = tweann_nif:z_score(Value, Mean, StdDev),
            %% z = (10 - 5) / 2.5 = 2.0
            ?assert(abs(ZScore - 2.0) < 0.001)
    end.

z_score_zero_stddev_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Zero std dev should return 0 or handle gracefully
            ZScore = tweann_nif:z_score(10.0, 5.0, 0.0),
            ?assert(is_float(ZScore)),
            ?assertEqual(0.0, ZScore)  %% Expected behavior
    end.

z_score_negative_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            ZScore = tweann_nif:z_score(0.0, 5.0, 2.5),
            %% z = (0 - 5) / 2.5 = -2.0
            ?assert(abs(ZScore - (-2.0)) < 0.001)
    end.

compute_reward_component_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            History = [1.0, 2.0, 3.0, 4.0, 5.0],
            Current = 6.0,
            %% Returns (raw_value, normalized_sigmoid, z_score)
            {Raw, Normalized, ZScore} =
                tweann_nif:compute_reward_component(History, Current),
            ?assert(is_float(Raw)),
            ?assert(is_float(Normalized)),
            ?assert(is_float(ZScore)),
            %% Raw should be the current value
            ?assert(abs(Raw - 6.0) < 0.001),
            %% Z-score for 6.0 with mean=3.0, std~1.4 should be positive
            ?assert(ZScore > 0.0),
            %% Normalized via sigmoid should be > 0.5 for positive z-score
            ?assert(Normalized > 0.5)
    end.

compute_reward_component_declining_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            History = [5.0, 4.0, 3.0, 2.0, 1.0],
            Current = 0.0,
            %% Returns (raw_value, normalized_sigmoid, z_score)
            {Raw, Normalized, ZScore} =
                tweann_nif:compute_reward_component(History, Current),
            ?assert(is_float(Raw)),
            %% Z-score for 0.0 with mean=3.0 should be negative
            ?assert(ZScore < 0.0),
            %% Normalized via sigmoid should be < 0.5 for negative z-score
            ?assert(Normalized < 0.5)
    end.

compute_weighted_reward_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Components: [{history, current_value, weight}, ...]
            Components = [
                {[1.0, 2.0, 3.0], 4.0, 0.5},   %% Improving
                {[5.0, 4.0, 3.0], 2.0, 0.3},   %% Declining
                {[1.0, 1.0, 1.0], 1.0, 0.2}    %% Stable
            ],
            Reward = tweann_nif:compute_weighted_reward(Components),
            ?assert(is_float(Reward))
            %% Reward is weighted sum of z-scores
    end.

compute_weighted_reward_single_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Components = [
                {[1.0, 2.0, 3.0], 4.0, 1.0}
            ],
            Reward = tweann_nif:compute_weighted_reward(Components),
            ?assert(is_float(Reward))
    end.

%%==============================================================================
%% Weight Utility Tests
%%==============================================================================

flatten_weights_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% WeightedInputs format: [{SourceId, [{Weight, DW, LR, Params}]}]
            WeightedInputs = [
                {1, [{0.5, 0.0, 0.1, []}, {0.3, 0.0, 0.1, []}]},
                {2, [{0.7, 0.0, 0.1, []}]}
            ],
            {Flat, Lengths} = tweann_nif:flatten_weights(WeightedInputs),
            %% 3 weights total
            ?assertEqual(3, length(Flat)),
            ?assertEqual([2, 1], Lengths),  %% 2 from source 1, 1 from source 2
            ?assert(abs(lists:nth(1, Flat) - 0.5) < 0.001),
            ?assert(abs(lists:nth(2, Flat) - 0.3) < 0.001),
            ?assert(abs(lists:nth(3, Flat) - 0.7) < 0.001)
    end.

flatten_weights_empty_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            {Flat, Lengths} = tweann_nif:flatten_weights([]),
            ?assertEqual([], Flat),
            ?assertEqual([], Lengths)
    end.

dot_product_preflattened_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            SignalsFlat = [1.0, 2.0, 3.0],
            WeightsFlat = [0.5, 0.5, 0.5],
            Bias = 0.0,
            %% dot = 1*0.5 + 2*0.5 + 3*0.5 = 3.0
            Result = tweann_nif:dot_product_preflattened(
                SignalsFlat, WeightsFlat, Bias),
            ?assert(abs(Result - 3.0) < 0.001)
    end.

dot_product_preflattened_with_bias_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            SignalsFlat = [1.0, 2.0],
            WeightsFlat = [1.0, 1.0],
            Bias = 5.0,
            %% dot = 1*1 + 2*1 + 5 = 8.0
            Result = tweann_nif:dot_product_preflattened(
                SignalsFlat, WeightsFlat, Bias),
            ?assert(abs(Result - 8.0) < 0.001)
    end.

dot_product_preflattened_empty_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Empty signals + bias only
            Result = tweann_nif:dot_product_preflattened([], [], 3.0),
            ?assert(abs(Result - 3.0) < 0.001)
    end.

%%==============================================================================
%% Edge Case and Error Handling Tests
%%==============================================================================

empty_vector_distance_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Empty vectors should return 0
            Distance = tweann_nif:euclidean_distance([], []),
            ?assert(abs(Distance) < 0.001)
    end.

mismatched_vector_distance_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Mismatched lengths - should handle gracefully
            %% Using shorter length or returning error
            Distance = tweann_nif:euclidean_distance([1.0, 2.0], [1.0]),
            ?assert(is_float(Distance) orelse Distance == error)
    end.

empty_fitness_stats_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Empty list should handle gracefully
            Result = tweann_nif:fitness_stats([]),
            ?assert(is_tuple(Result) orelse Result == error)
    end.

large_batch_knn_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            %% Test with larger batch
            Behaviors = [[float(I), float(I)] || I <- lists:seq(1, 100)],
            Archive = [[0.0, 0.0]],
            K = 5,
            Results = tweann_nif:knn_novelty_batch(Behaviors, Archive, K),
            ?assertEqual(100, length(Results)),
            %% All should be valid floats
            ?assert(lists:all(fun(N) -> is_float(N) andalso N >= 0.0 end, Results))
    end.

%%==============================================================================
%% Performance Sanity Tests
%%==============================================================================

batch_distance_performance_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Target = [0.0 || _ <- lists:seq(1, 100)],  %% 100-dim vector
            Others = [[float(I rem 10) || _ <- lists:seq(1, 100)]
                     || I <- lists:seq(1, 1000)],
            Start = erlang:monotonic_time(microsecond),
            _Results = tweann_nif:euclidean_distance_batch(Target, Others),
            End = erlang:monotonic_time(microsecond),
            Time = End - Start,
            %% Should complete in reasonable time (< 100ms)
            ?assert(Time < 100000)
    end.

fitness_stats_performance_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            Fitnesses = [rand:uniform() || _ <- lists:seq(1, 10000)],
            Start = erlang:monotonic_time(microsecond),
            _Stats = tweann_nif:fitness_stats(Fitnesses),
            End = erlang:monotonic_time(microsecond),
            Time = End - Start,
            %% Should complete in < 10ms
            ?assert(Time < 10000)
    end.

roulette_batch_performance_test() ->
    case tweann_nif:is_loaded() of
        false ->
            {skip, "NIF not loaded"};
        true ->
            N = 1000,
            Fitnesses = [rand:uniform() || _ <- lists:seq(1, N)],
            {Cumulative, Total} = tweann_nif:build_cumulative_fitness(Fitnesses),
            RandomVals = [rand:uniform() || _ <- lists:seq(1, N)],
            Start = erlang:monotonic_time(microsecond),
            _Indices = tweann_nif:roulette_select_batch(Cumulative, Total, RandomVals),
            End = erlang:monotonic_time(microsecond),
            Time = End - Start,
            %% 1000 selections should complete in < 5ms
            ?assert(Time < 5000)
    end.
