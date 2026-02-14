%% @doc Pure Erlang fallback implementations for TWEANN NIFs.
%%
%% This module provides pure Erlang implementations of all NIF functions.
%% These are used when the Rust NIF is not loaded or unavailable.
%% Performance will be slower than NIF but functionality is preserved.
%%
%% Copyright 2025 R.G. Lefever
%% Licensed under Apache-2.0
-module(tweann_nif_fallback).

-export([
    %% Network evaluation
    compile_network/3,
    evaluate/2,
    evaluate_batch/2,
    compatibility_distance/5,
    benchmark_evaluate/3,
    %% Signal aggregation
    dot_product_flat/3,
    dot_product_batch/1,
    dot_product_preflattened/3,
    flatten_weights/1,
    %% LTC/CfC functions
    evaluate_cfc/4,
    evaluate_cfc_with_weights/6,
    evaluate_ode/5,
    evaluate_ode_with_weights/7,
    evaluate_cfc_batch/4,
    %% Distance and KNN (Novelty Search)
    euclidean_distance/2,
    euclidean_distance_batch/2,
    knn_novelty/4,
    knn_novelty_batch/3,
    %% Statistics
    fitness_stats/1,
    weighted_moving_average/2,
    shannon_entropy/1,
    histogram/4,
    %% Selection
    build_cumulative_fitness/1,
    roulette_select/3,
    roulette_select_batch/3,
    tournament_select/2,
    %% Reward and Meta-Controller
    z_score/3,
    compute_reward_component/2,
    compute_weighted_reward/1,
    %% Batch Mutation (Evolutionary Genetics)
    mutate_weights/4,
    mutate_weights_seeded/5,
    mutate_weights_batch/1,
    mutate_weights_batch_uniform/4,
    random_weights/1,
    random_weights_seeded/2,
    random_weights_gaussian/3,
    random_weights_batch/1,
    weight_distance_l1/2,
    weight_distance_l2/2,
    weight_distance_batch/3
]).

%%==============================================================================
%% Network Evaluation Fallbacks
%%==============================================================================

%% @doc Compile network to internal format (Erlang record).
-spec compile_network(list(), non_neg_integer(), [non_neg_integer()]) -> map().
compile_network(Nodes, InputCount, OutputIndices) ->
    %% Store as map for Erlang-based evaluation
    #{
        nodes => maps:from_list([{Idx, Node} || {Idx, _, _, _, _} = Node <- Nodes]),
        input_count => InputCount,
        output_indices => OutputIndices,
        node_list => Nodes
    }.

%% @doc Evaluate network with inputs.
-spec evaluate(map(), [float()]) -> [float()].
evaluate(#{nodes := _Nodes, input_count := InputCount, output_indices := OutputIndices, node_list := NodeList}, Inputs) ->
    case length(Inputs) of
        InputCount ->
            %% Initialize activations with inputs
            InitActivations = maps:from_list(lists:zip(lists:seq(0, InputCount - 1), Inputs)),
            %% Forward propagate through nodes in order
            FinalActivations = lists:foldl(
                fun({Idx, Type, Activation, Bias, Connections}, Acc) ->
                    case Type of
                        input -> Acc;  %% Already initialized
                        _ ->
                            %% Compute weighted sum
                            Sum = lists:foldl(
                                fun({FromIdx, Weight}, S) ->
                                    FromVal = maps:get(FromIdx, Acc, 0.0),
                                    S + FromVal * Weight
                                end,
                                Bias,
                                Connections
                            ),
                            %% Apply activation
                            Output = apply_activation(Activation, Sum),
                            maps:put(Idx, Output, Acc)
                    end
                end,
                InitActivations,
                NodeList
            ),
            %% Extract outputs
            [maps:get(Idx, FinalActivations, 0.0) || Idx <- OutputIndices];
        _ ->
            []  %% Wrong input count
    end;
evaluate(_, _) ->
    [].

%% @doc Batch evaluate network.
-spec evaluate_batch(map(), [[float()]]) -> [[float()]].
evaluate_batch(Network, InputsList) ->
    [evaluate(Network, Inputs) || Inputs <- InputsList].

%% @doc Compute NEAT compatibility distance.
-spec compatibility_distance(list(), list(), float(), float(), float()) -> float().
compatibility_distance(ConnectionsA, ConnectionsB, C1, C2, C3) ->
    MapA = maps:from_list(ConnectionsA),
    MapB = maps:from_list(ConnectionsB),

    AllInnovations = lists:usort(maps:keys(MapA) ++ maps:keys(MapB)),
    N = max(1, max(length(ConnectionsA), length(ConnectionsB))),

    {Matching, Disjoint, Excess, WeightDiff} = lists:foldl(
        fun(Inn, {M, D, E, W}) ->
            case {maps:get(Inn, MapA, undefined), maps:get(Inn, MapB, undefined)} of
                {undefined, _} -> {M, D + 1, E, W};
                {_, undefined} -> {M, D + 1, E, W};
                {WA, WB} -> {M + 1, D, E, W + abs(WA - WB)}
            end
        end,
        {0, 0, 0, 0.0},
        AllInnovations
    ),

    AvgWeightDiff = case Matching of
        0 -> 0.0;
        _ -> WeightDiff / Matching
    end,

    (C1 * Excess / N) + (C2 * Disjoint / N) + (C3 * AvgWeightDiff).

%% @doc Benchmark evaluate (returns microseconds per evaluation).
-spec benchmark_evaluate(map(), [float()], pos_integer()) -> float().
benchmark_evaluate(Network, Inputs, Iterations) ->
    Start = erlang:monotonic_time(microsecond),
    benchmark_loop(Network, Inputs, Iterations),
    End = erlang:monotonic_time(microsecond),
    (End - Start) / Iterations.

benchmark_loop(_Network, _Inputs, 0) -> ok;
benchmark_loop(Network, Inputs, N) ->
    _ = evaluate(Network, Inputs),
    benchmark_loop(Network, Inputs, N - 1).

%%==============================================================================
%% Signal Aggregation Fallbacks
%%==============================================================================

%% @doc Flat dot product.
-spec dot_product_flat([float()], [float()], float()) -> float().
dot_product_flat(Signals, Weights, Bias) ->
    lists:foldl(
        fun({S, W}, Acc) -> Acc + S * W end,
        Bias,
        lists:zip(Signals, Weights)
    ).

%% @doc Batch dot product.
-spec dot_product_batch([{[float()], [float()], float()}]) -> [float()].
dot_product_batch(Batch) ->
    [dot_product_flat(S, W, B) || {S, W, B} <- Batch].

%% @doc Dot product with pre-flattened arrays.
-spec dot_product_preflattened([float()], [float()], float()) -> float().
dot_product_preflattened(SignalsFlat, WeightsFlat, Bias) ->
    dot_product_flat(SignalsFlat, WeightsFlat, Bias).

%% @doc Flatten nested weight structure.
-spec flatten_weights([{term(), [{float(), float(), float(), list()}]}]) ->
    {[float()], [non_neg_integer()]}.
flatten_weights(WeightedInputs) ->
    {Weights, Counts} = lists:foldl(
        fun({_SourceId, WeightList}, {WAcc, CAcc}) ->
            Ws = [W || {W, _DW, _LR, _Params} <- WeightList],
            {WAcc ++ Ws, CAcc ++ [length(Ws)]}
        end,
        {[], []},
        WeightedInputs
    ),
    {Weights, Counts}.

%%==============================================================================
%% LTC/CfC Fallbacks
%%==============================================================================

%% @doc Evaluate CfC (closed-form continuous-time) neuron.
-spec evaluate_cfc(float(), float(), float(), float()) -> {float(), float()}.
evaluate_cfc(Input, State, _Tau, Bound) ->
    %% CfC closed-form: x_new = sigma(-f) * x + (1 - sigma(-f)) * h
    %% Simplified: use tanh for backbone, linear for head
    F = math:tanh(Input),
    H = Input,
    SigNegF = sigmoid(-F),
    NewState = SigNegF * State + (1 - SigNegF) * H,
    ClampedState = clamp(NewState, -Bound, Bound),
    Output = math:tanh(ClampedState),
    {ClampedState, Output}.

%% @doc Evaluate CfC with custom weights.
-spec evaluate_cfc_with_weights(float(), float(), float(), float(), [float()], [float()]) ->
    {float(), float()}.
evaluate_cfc_with_weights(Input, State, _Tau, Bound, BackboneWeights, HeadWeights) ->
    %% Use weights for backbone and head computation
    F = compute_weighted_sum([Input, State, 1.0], BackboneWeights),
    H = compute_weighted_sum([Input, State], HeadWeights),
    SigNegF = sigmoid(-F),
    NewState = SigNegF * State + (1 - SigNegF) * H,
    ClampedState = clamp(NewState, -Bound, Bound),
    Output = math:tanh(ClampedState),
    {ClampedState, Output}.

%% @doc Evaluate ODE-based LTC neuron.
-spec evaluate_ode(float(), float(), float(), float(), float()) -> {float(), float()}.
evaluate_ode(Input, State, Tau, Bound, Dt) ->
    %% Euler integration: dx/dt = -[1/tau + f] * x + f * A
    F = math:tanh(Input),
    A = Bound,
    DxDt = -(1/Tau + F) * State + F * A,
    NewState = State + Dt * DxDt,
    ClampedState = clamp(NewState, -Bound, Bound),
    Output = math:tanh(ClampedState),
    {ClampedState, Output}.

%% @doc Evaluate ODE with custom weights.
-spec evaluate_ode_with_weights(float(), float(), float(), float(), float(), [float()], [float()]) ->
    {float(), float()}.
evaluate_ode_with_weights(Input, State, Tau, Bound, Dt, BackboneWeights, HeadWeights) ->
    F = compute_weighted_sum([Input, State, 1.0], BackboneWeights),
    A = compute_weighted_sum([Input, State], HeadWeights),
    DxDt = -(1/Tau + F) * State + F * A,
    NewState = State + Dt * DxDt,
    ClampedState = clamp(NewState, -Bound, Bound),
    Output = math:tanh(ClampedState),
    {ClampedState, Output}.

%% @doc Batch CfC evaluation for time series.
-spec evaluate_cfc_batch([float()], float(), float(), float()) -> [{float(), float()}].
evaluate_cfc_batch(Inputs, InitialState, Tau, Bound) ->
    {Results, _} = lists:foldl(
        fun(Input, {Acc, State}) ->
            {NewState, Output} = evaluate_cfc(Input, State, Tau, Bound),
            {Acc ++ [{NewState, Output}], NewState}
        end,
        {[], InitialState},
        Inputs
    ),
    Results.

%%==============================================================================
%% Distance and KNN Fallbacks (Novelty Search)
%%==============================================================================

%% @doc Euclidean distance between two vectors.
-spec euclidean_distance([float()], [float()]) -> float().
euclidean_distance(V1, V2) ->
    SumSq = lists:foldl(
        fun({A, B}, Acc) -> Acc + (A - B) * (A - B) end,
        0.0,
        lists:zip(V1, V2)
    ),
    math:sqrt(SumSq).

%% @doc Batch euclidean distance, sorted by distance.
-spec euclidean_distance_batch([float()], [[float()]]) -> [{non_neg_integer(), float()}].
euclidean_distance_batch(Target, Others) ->
    Distances = lists:map(
        fun({Idx, Other}) ->
            {Idx, euclidean_distance(Target, Other)}
        end,
        lists:zip(lists:seq(0, length(Others) - 1), Others)
    ),
    lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances).

%% @doc K-nearest neighbor novelty score.
-spec knn_novelty([float()], [[float()]], [[float()]], pos_integer()) -> float().
knn_novelty(Target, Population, Archive, K) ->
    AllOthers = [Other || Other <- Population, Other =/= Target] ++ Archive,
    case AllOthers of
        [] -> 0.0;
        _ ->
            Distances = [euclidean_distance(Target, Other) || Other <- AllOthers],
            Sorted = lists:sort(Distances),
            KNearest = lists:sublist(Sorted, K),
            lists:sum(KNearest) / length(KNearest)
    end.

%% @doc Batch KNN novelty for entire population.
-spec knn_novelty_batch([[float()]], [[float()]], pos_integer()) -> [float()].
knn_novelty_batch(Behaviors, Archive, K) ->
    [knn_novelty(B, Behaviors, Archive, K) || B <- Behaviors].

%%==============================================================================
%% Statistics Fallbacks
%%==============================================================================

%% @doc Compute fitness statistics in single pass.
-spec fitness_stats([float()]) -> {float(), float(), float(), float(), float(), float()}.
fitness_stats([]) ->
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
fitness_stats(Fitnesses) ->
    N = length(Fitnesses),
    {Min, Max, Sum, SumSq} = lists:foldl(
        fun(F, {MinAcc, MaxAcc, SumAcc, SumSqAcc}) ->
            {min(F, MinAcc), max(F, MaxAcc), SumAcc + F, SumSqAcc + F * F}
        end,
        {hd(Fitnesses), hd(Fitnesses), 0.0, 0.0},
        Fitnesses
    ),
    Mean = Sum / N,
    Variance = (SumSq / N) - (Mean * Mean),
    StdDev = math:sqrt(max(0.0, Variance)),
    {Min, Max, Mean, Variance, StdDev, Sum}.

%% @doc Weighted moving average with exponential decay.
-spec weighted_moving_average([float()], float()) -> float().
weighted_moving_average([], _Decay) -> 0.0;
weighted_moving_average(Values, Decay) ->
    {WeightedSum, TotalWeight, _} = lists:foldl(
        fun(Value, {WSum, TWeight, Weight}) ->
            {WSum + Weight * Value, TWeight + Weight, Weight * Decay}
        end,
        {0.0, 0.0, 1.0},
        Values
    ),
    WeightedSum / TotalWeight.

%% @doc Shannon entropy of a distribution.
-spec shannon_entropy([float()]) -> float().
shannon_entropy(Values) ->
    Positive = [V || V <- Values, V > 0],
    Total = lists:sum(Positive),
    case Total > 0 of
        false -> 0.0;
        true ->
            lists:foldl(
                fun(V, Acc) ->
                    P = V / Total,
                    Acc - P * math:log(P)
                end,
                0.0,
                Positive
            )
    end.

%% @doc Histogram binning.
-spec histogram([float()], pos_integer(), float(), float()) -> [non_neg_integer()].
histogram(Values, NumBins, MinVal, MaxVal) ->
    Range = MaxVal - MinVal,
    case Range > 0 andalso NumBins > 0 of
        false -> lists:duplicate(NumBins, 0);
        true ->
            BinWidth = Range / NumBins,
            Bins = lists:foldl(
                fun(V, Acc) ->
                    case V >= MinVal andalso V =< MaxVal of
                        false -> Acc;
                        true ->
                            BinIdx = min(NumBins - 1, trunc((V - MinVal) / BinWidth)),
                            maps:update_with(BinIdx, fun(C) -> C + 1 end, 1, Acc)
                    end
                end,
                #{},
                Values
            ),
            [maps:get(I, Bins, 0) || I <- lists:seq(0, NumBins - 1)]
    end.

%%==============================================================================
%% Selection Fallbacks
%%==============================================================================

%% @doc Build cumulative fitness array for roulette wheel.
-spec build_cumulative_fitness([float()]) -> {[float()], float()}.
build_cumulative_fitness([]) -> {[], 0.0};
build_cumulative_fitness(Fitnesses) ->
    MinFitness = lists:min(Fitnesses),
    Shift = case MinFitness < 0 of true -> -MinFitness + 1.0; false -> 0.0 end,
    {Cumulative, Total} = lists:foldl(
        fun(F, {Acc, RunningSum}) ->
            NewSum = RunningSum + F + Shift,
            {Acc ++ [NewSum], NewSum}
        end,
        {[], 0.0},
        Fitnesses
    ),
    {Cumulative, Total}.

%% @doc Roulette wheel selection with binary search.
-spec roulette_select([float()], float(), float()) -> non_neg_integer().
roulette_select(Cumulative, Total, RandomVal) ->
    Target = RandomVal * Total,
    binary_search_cumulative(Cumulative, Target, 0).

binary_search_cumulative([], _Target, Idx) -> max(0, Idx - 1);
binary_search_cumulative([H | T], Target, Idx) ->
    case H >= Target of
        true -> Idx;
        false -> binary_search_cumulative(T, Target, Idx + 1)
    end.

%% @doc Batch roulette selection.
-spec roulette_select_batch([float()], float(), [float()]) -> [non_neg_integer()].
roulette_select_batch(Cumulative, Total, RandomVals) ->
    [roulette_select(Cumulative, Total, R) || R <- RandomVals].

%% @doc Tournament selection.
-spec tournament_select([non_neg_integer()], [float()]) -> non_neg_integer().
tournament_select([], _Fitnesses) -> 0;
tournament_select(Contestants, Fitnesses) ->
    {Winner, _} = lists:foldl(
        fun(Idx, {BestIdx, BestFit}) ->
            Fit = lists:nth(Idx + 1, Fitnesses),
            case Fit > BestFit of
                true -> {Idx, Fit};
                false -> {BestIdx, BestFit}
            end
        end,
        {hd(Contestants), lists:nth(hd(Contestants) + 1, Fitnesses)},
        Contestants
    ),
    Winner.

%%==============================================================================
%% Reward and Meta-Controller Fallbacks
%%==============================================================================

%% @doc Z-score normalization.
-spec z_score(float(), float(), float()) -> float().
z_score(_Value, _Mean, StdDev) when abs(StdDev) < 1.0e-10 -> 0.0;
z_score(Value, Mean, StdDev) -> (Value - Mean) / StdDev.

%% @doc Compute reward component with normalization.
-spec compute_reward_component([float()], float()) -> {float(), float(), float()}.
compute_reward_component([], Current) ->
    {Current, sigmoid(Current), 0.0};
compute_reward_component(History, Current) ->
    N = length(History),
    Mean = lists:sum(History) / N,
    Variance = lists:sum([(H - Mean) * (H - Mean) || H <- History]) / N,
    StdDev = math:sqrt(max(0.0, Variance)),
    Z = z_score(Current, Mean, StdDev),
    Normalized = sigmoid(Z),
    {Current, Normalized, Z}.

%% @doc Batch compute weighted reward.
-spec compute_weighted_reward([{[float()], float(), float()}]) -> float().
compute_weighted_reward(Components) ->
    lists:sum([
        begin
            {_, Normalized, _} = compute_reward_component(History, Current),
            Normalized * Weight
        end
        || {History, Current, Weight} <- Components
    ]).

%%==============================================================================
%% Batch Mutation Fallbacks (Evolutionary Genetics)
%%==============================================================================

%% @doc Mutate weights with Gaussian perturbation.
%% Each weight has MutationRate chance of mutation.
%% Mutated weights are either perturbed (PerturbRate) or randomized.
-spec mutate_weights([float()], float(), float(), float()) -> [float()].
mutate_weights(Weights, MutationRate, PerturbRate, PerturbStrength) ->
    [mutate_single_weight(W, MutationRate, PerturbRate, PerturbStrength) || W <- Weights].

%% @doc Mutate weights with specific random seed for reproducibility.
-spec mutate_weights_seeded([float()], float(), float(), float(), integer()) -> [float()].
mutate_weights_seeded(Weights, MutationRate, PerturbRate, PerturbStrength, Seed) ->
    rand:seed(exsss, {Seed, Seed * 2, Seed * 3}),
    mutate_weights(Weights, MutationRate, PerturbRate, PerturbStrength).

%% @doc Batch mutate with per-genome parameters.
%% Each tuple: {Weights, MutationRate, PerturbRate, PerturbStrength}
-spec mutate_weights_batch([{[float()], float(), float(), float()}]) -> [[float()]].
mutate_weights_batch(Batch) ->
    [mutate_weights(W, MR, PR, PS) || {W, MR, PR, PS} <- Batch].

%% @doc Batch mutate with uniform parameters across all genomes.
-spec mutate_weights_batch_uniform([[float()]], float(), float(), float()) -> [[float()]].
mutate_weights_batch_uniform(WeightsList, MutationRate, PerturbRate, PerturbStrength) ->
    [mutate_weights(W, MutationRate, PerturbRate, PerturbStrength) || W <- WeightsList].

%% @doc Generate random weights in range [-1.0, 1.0].
-spec random_weights(non_neg_integer()) -> [float()].
random_weights(Count) ->
    [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, Count)].

%% @doc Generate random weights with specific seed.
-spec random_weights_seeded(non_neg_integer(), integer()) -> [float()].
random_weights_seeded(Count, Seed) ->
    rand:seed(exsss, {Seed, Seed * 2, Seed * 3}),
    random_weights(Count).

%% @doc Generate random weights from Gaussian distribution.
-spec random_weights_gaussian(non_neg_integer(), float(), float()) -> [float()].
random_weights_gaussian(Count, Mean, StdDev) ->
    [rand:normal(Mean, StdDev) || _ <- lists:seq(1, Count)].

%% @doc Batch generate random weights.
%% Each tuple: {Count, Mean, StdDev}
-spec random_weights_batch([{non_neg_integer(), float(), float()}]) -> [[float()]].
random_weights_batch(Batch) ->
    [random_weights_gaussian(C, M, S) || {C, M, S} <- Batch].

%% @doc L1 (Manhattan) distance between two weight vectors.
-spec weight_distance_l1([float()], [float()]) -> float().
weight_distance_l1(W1, W2) ->
    lists:sum([abs(A - B) || {A, B} <- lists:zip(W1, W2)]).

%% @doc L2 (Euclidean) distance between two weight vectors.
-spec weight_distance_l2([float()], [float()]) -> float().
weight_distance_l2(W1, W2) ->
    math:sqrt(lists:sum([(A - B) * (A - B) || {A, B} <- lists:zip(W1, W2)])).

%% @doc Batch compute distances between target and multiple weight vectors.
%% DistanceType: l1 | l2
-spec weight_distance_batch([float()], [[float()]], l1 | l2) -> [float()].
weight_distance_batch(Target, Others, DistanceType) ->
    DistFun = case DistanceType of
        l1 -> fun weight_distance_l1/2;
        l2 -> fun weight_distance_l2/2
    end,
    [DistFun(Target, Other) || Other <- Others].

%% @private Mutate a single weight.
mutate_single_weight(Weight, MutationRate, PerturbRate, PerturbStrength) ->
    case rand:uniform() < MutationRate of
        true ->
            case rand:uniform() < PerturbRate of
                true ->
                    %% Perturb with Gaussian noise
                    Weight + rand:normal() * PerturbStrength;
                false ->
                    %% Full random replacement
                    rand:uniform() * 2.0 - 1.0
            end;
        false ->
            Weight
    end.

%%==============================================================================
%% Internal Helper Functions
%%==============================================================================

%% @private Apply activation function.
apply_activation(tanh, X) -> math:tanh(X);
apply_activation(sigmoid, X) -> sigmoid(X);
apply_activation(relu, X) -> max(0.0, X);
apply_activation(linear, X) -> X;
apply_activation(sin, X) -> math:sin(X);
apply_activation(cos, X) -> math:cos(X);
apply_activation(abs, X) -> abs(X);
apply_activation(sgn, X) when X > 0 -> 1.0;
apply_activation(sgn, X) when X < 0 -> -1.0;
apply_activation(sgn, _X) -> 0.0;
apply_activation(_, X) -> X.  %% Default to linear

%% @private Sigmoid function.
sigmoid(X) -> 1.0 / (1.0 + math:exp(-X)).

%% @private Clamp value to range.
clamp(X, Min, Max) -> max(Min, min(Max, X)).

%% @private Compute weighted sum.
compute_weighted_sum(Values, Weights) ->
    case length(Values) =< length(Weights) of
        true ->
            lists:foldl(
                fun({V, W}, Acc) -> Acc + V * W end,
                0.0,
                lists:zip(Values, lists:sublist(Weights, length(Values)))
            );
        false ->
            0.0
    end.
