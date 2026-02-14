%% @doc Native Implemented Functions for high-performance network evaluation.
%%
%% This module provides accelerated network evaluation for TWEANN.
%%
%% == Implementation Priority ==
%%
%% 1. **Enterprise (faber_nn_nifs)**: If the faber_nn_nifs dependency is
%%    available (private git repo), its Rust NIFs are used automatically.
%%    This provides 10-15x speedup for compute-intensive operations.
%%
%% 2. **Pure Erlang (tweann_nif_fallback)**: If enterprise NIFs are not
%%    available, pure Erlang implementations are used. Always works.
%%
%% == Usage ==
%%
%% 1. Compile a genotype to a network reference:
%%    Network = tweann_nif:compile_network(Nodes, InputCount, OutputIndices)
%%
%% 2. Evaluate the network:
%%    Outputs = tweann_nif:evaluate(Network, Inputs)
%%
%% 3. For batch evaluation (many inputs, same network):
%%    OutputsList = tweann_nif:evaluate_batch(Network, InputsList)
%%
%% == Network Compilation Format ==
%%
%% Nodes are provided as a list of tuples:
%%   [{Index, Type, Activation, Bias, [{FromIndex, Weight}, ...]}, ...]
%%
%% Where:
%% - Index: integer node index (0-based)
%% - Type: atom (input | hidden | output | bias)
%% - Activation: atom (tanh | sigmoid | relu | etc.)
%% - Bias: float
%% - Connections: list of {FromIndex, Weight} tuples
%%
%% Nodes MUST be in topological order (inputs first, then hidden, then outputs).
%%
%% @copyright 2024-2026 R.G. Lefever
-module(tweann_nif).

-export([
    compile_network/3,
    evaluate/2,
    evaluate_batch/2,
    compatibility_distance/5,
    benchmark_evaluate/3,
    is_loaded/0,
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

-on_load(init/0).

-define(IMPL_KEY, {?MODULE, impl_module}).

%% @private
%% @doc Initialize the implementation module.
%%
%% Priority order:
%% 1. faber_nn_nifs (enterprise - private git repo)
%% 2. tweann_nif_fallback (pure Erlang)
init() ->
    ImplModule = detect_impl_module(),
    persistent_term:put(?IMPL_KEY, ImplModule),
    ok.

%% @private
%% @doc Detect which implementation module to use.
detect_impl_module() ->
    case code:which(faber_nn_nifs) of
        non_existing ->
            tweann_nif_fallback;
        _ ->
            case faber_nn_nifs:is_loaded() of
                true -> faber_nn_nifs;
                false -> tweann_nif_fallback
            end
    end.

%% @private
impl_module() ->
    persistent_term:get(?IMPL_KEY, tweann_nif_fallback).

%% @doc Check if enterprise NIFs are loaded.
-spec is_loaded() -> boolean().
is_loaded() ->
    impl_module() =:= faber_nn_nifs.

%%==============================================================================
%% Network Evaluation Functions
%%==============================================================================

%% @doc Compile a network for fast evaluation.
-spec compile_network(
    Nodes :: [{non_neg_integer(), atom(), atom(), float(), [{non_neg_integer(), float()}]}],
    InputCount :: non_neg_integer(),
    OutputIndices :: [non_neg_integer()]
) -> reference() | map().
compile_network(Nodes, InputCount, OutputIndices) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:compile_network(Nodes, InputCount, OutputIndices);
        _ -> tweann_nif_fallback:compile_network(Nodes, InputCount, OutputIndices)
    end.

%% @doc Evaluate a compiled network with given inputs.
-spec evaluate(Network :: reference() | map(), Inputs :: [float()]) -> [float()].
evaluate(Network, Inputs) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:evaluate(Network, Inputs);
        _ -> tweann_nif_fallback:evaluate(Network, Inputs)
    end.

%% @doc Evaluate a network with multiple input sets.
-spec evaluate_batch(Network :: reference() | map(), InputsList :: [[float()]]) -> [[float()]].
evaluate_batch(Network, InputsList) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:evaluate_batch(Network, InputsList);
        _ -> tweann_nif_fallback:evaluate_batch(Network, InputsList)
    end.

%% @doc Calculate compatibility distance between two genomes.
-spec compatibility_distance(
    ConnectionsA :: [{non_neg_integer(), float()}],
    ConnectionsB :: [{non_neg_integer(), float()}],
    C1 :: float(),
    C2 :: float(),
    C3 :: float()
) -> float().
compatibility_distance(ConnectionsA, ConnectionsB, C1, C2, C3) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:compatibility_distance(ConnectionsA, ConnectionsB, C1, C2, C3);
        _ -> tweann_nif_fallback:compatibility_distance(ConnectionsA, ConnectionsB, C1, C2, C3)
    end.

%% @doc Benchmark network evaluation.
-spec benchmark_evaluate(
    Network :: reference() | map(),
    Inputs :: [float()],
    Iterations :: pos_integer()
) -> float().
benchmark_evaluate(Network, Inputs, Iterations) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:benchmark_evaluate(Network, Inputs, Iterations);
        _ -> tweann_nif_fallback:benchmark_evaluate(Network, Inputs, Iterations)
    end.

%%==============================================================================
%% Signal Aggregation Functions
%%==============================================================================

%% @doc Fast dot product for signal aggregation.
-spec dot_product_flat(Signals :: [float()], Weights :: [float()], Bias :: float()) -> float().
dot_product_flat(Signals, Weights, Bias) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:dot_product_flat(Signals, Weights, Bias);
        _ -> tweann_nif_fallback:dot_product_flat(Signals, Weights, Bias)
    end.

%% @doc Batch dot product for multiple neurons.
-spec dot_product_batch(Batch :: [{[float()], [float()], float()}]) -> [float()].
dot_product_batch(Batch) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:dot_product_batch(Batch);
        _ -> tweann_nif_fallback:dot_product_batch(Batch)
    end.

%% @doc Dot product with pre-flattened data.
-spec dot_product_preflattened(Signals :: [float()], Weights :: [float()], Bias :: float()) -> float().
dot_product_preflattened(Signals, Weights, Bias) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:dot_product_preflattened(Signals, Weights, Bias);
        _ -> tweann_nif_fallback:dot_product_preflattened(Signals, Weights, Bias)
    end.

%% @doc Flatten weights for efficient dot product.
-spec flatten_weights(WeightedInputs :: [{non_neg_integer(), [{float(), float(), float(), [float()]}]}]) ->
    {[float()], [non_neg_integer()]}.
flatten_weights(WeightedInputs) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:flatten_weights(WeightedInputs);
        _ -> tweann_nif_fallback:flatten_weights(WeightedInputs)
    end.

%%==============================================================================
%% LTC/CfC (Liquid Time-Constant) Functions
%%==============================================================================

%% @doc CfC (Closed-form Continuous-time) evaluation.
-spec evaluate_cfc(Input :: float(), State :: float(), Tau :: float(), Bound :: float()) -> {float(), float()}.
evaluate_cfc(Input, State, Tau, Bound) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:evaluate_cfc(Input, State, Tau, Bound);
        _ -> tweann_nif_fallback:evaluate_cfc(Input, State, Tau, Bound)
    end.

%% @doc CfC evaluation with custom backbone and head weights.
-spec evaluate_cfc_with_weights(
    Input :: float(), State :: float(), Tau :: float(), Bound :: float(),
    BackboneWeights :: [float()], HeadWeights :: [float()]
) -> {float(), float()}.
evaluate_cfc_with_weights(Input, State, Tau, Bound, BackboneWeights, HeadWeights) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:evaluate_cfc_with_weights(Input, State, Tau, Bound, BackboneWeights, HeadWeights);
        _ -> tweann_nif_fallback:evaluate_cfc_with_weights(Input, State, Tau, Bound, BackboneWeights, HeadWeights)
    end.

%% @doc ODE-based LTC evaluation.
-spec evaluate_ode(Input :: float(), State :: float(), Tau :: float(), Bound :: float(), Dt :: float()) -> {float(), float()}.
evaluate_ode(Input, State, Tau, Bound, Dt) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:evaluate_ode(Input, State, Tau, Bound, Dt);
        _ -> tweann_nif_fallback:evaluate_ode(Input, State, Tau, Bound, Dt)
    end.

%% @doc ODE evaluation with custom weights.
-spec evaluate_ode_with_weights(
    Input :: float(), State :: float(), Tau :: float(), Bound :: float(), Dt :: float(),
    BackboneWeights :: [float()], HeadWeights :: [float()]
) -> {float(), float()}.
evaluate_ode_with_weights(Input, State, Tau, Bound, Dt, BackboneWeights, HeadWeights) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:evaluate_ode_with_weights(Input, State, Tau, Bound, Dt, BackboneWeights, HeadWeights);
        _ -> tweann_nif_fallback:evaluate_ode_with_weights(Input, State, Tau, Bound, Dt, BackboneWeights, HeadWeights)
    end.

%% @doc Batch CfC evaluation for time series.
-spec evaluate_cfc_batch(Inputs :: [float()], InitialState :: float(), Tau :: float(), Bound :: float()) -> [{float(), float()}].
evaluate_cfc_batch(Inputs, InitialState, Tau, Bound) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:evaluate_cfc_batch(Inputs, InitialState, Tau, Bound);
        _ -> tweann_nif_fallback:evaluate_cfc_batch(Inputs, InitialState, Tau, Bound)
    end.

%%==============================================================================
%% Distance and KNN Functions (Novelty Search)
%%==============================================================================

%% @doc Compute Euclidean distance between two behavior vectors.
-spec euclidean_distance(V1 :: [float()], V2 :: [float()]) -> float().
euclidean_distance(V1, V2) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:euclidean_distance(V1, V2);
        _ -> tweann_nif_fallback:euclidean_distance(V1, V2)
    end.

%% @doc Batch Euclidean distance from one vector to many.
-spec euclidean_distance_batch(Target :: [float()], Others :: [[float()]]) -> [{non_neg_integer(), float()}].
euclidean_distance_batch(Target, Others) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:euclidean_distance_batch(Target, Others);
        _ -> tweann_nif_fallback:euclidean_distance_batch(Target, Others)
    end.

%% @doc Compute k-nearest neighbor novelty score.
-spec knn_novelty(Target :: [float()], Population :: [[float()]], Archive :: [[float()]], K :: pos_integer()) -> float().
knn_novelty(Target, Population, Archive, K) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:knn_novelty(Target, Population, Archive, K);
        _ -> tweann_nif_fallback:knn_novelty(Target, Population, Archive, K)
    end.

%% @doc Batch kNN novelty for multiple behaviors.
-spec knn_novelty_batch(Behaviors :: [[float()]], Archive :: [[float()]], K :: pos_integer()) -> [float()].
knn_novelty_batch(Behaviors, Archive, K) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:knn_novelty_batch(Behaviors, Archive, K);
        _ -> tweann_nif_fallback:knn_novelty_batch(Behaviors, Archive, K)
    end.

%%==============================================================================
%% Statistics Functions
%%==============================================================================

%% @doc Compute fitness statistics in single pass.
-spec fitness_stats(Fitnesses :: [float()]) -> {float(), float(), float(), float(), float(), float()}.
fitness_stats(Fitnesses) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:fitness_stats(Fitnesses);
        _ -> tweann_nif_fallback:fitness_stats(Fitnesses)
    end.

%% @doc Compute weighted moving average.
-spec weighted_moving_average(Values :: [float()], Decay :: float()) -> float().
weighted_moving_average(Values, Decay) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:weighted_moving_average(Values, Decay);
        _ -> tweann_nif_fallback:weighted_moving_average(Values, Decay)
    end.

%% @doc Compute Shannon entropy.
-spec shannon_entropy(Values :: [float()]) -> float().
shannon_entropy(Values) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:shannon_entropy(Values);
        _ -> tweann_nif_fallback:shannon_entropy(Values)
    end.

%% @doc Create histogram bins.
-spec histogram(Values :: [float()], NumBins :: pos_integer(), MinVal :: float(), MaxVal :: float()) -> [non_neg_integer()].
histogram(Values, NumBins, MinVal, MaxVal) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:histogram(Values, NumBins, MinVal, MaxVal);
        _ -> tweann_nif_fallback:histogram(Values, NumBins, MinVal, MaxVal)
    end.

%%==============================================================================
%% Selection Functions
%%==============================================================================

%% @doc Build cumulative fitness array for roulette selection.
-spec build_cumulative_fitness(Fitnesses :: [float()]) -> {[float()], float()}.
build_cumulative_fitness(Fitnesses) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:build_cumulative_fitness(Fitnesses);
        _ -> tweann_nif_fallback:build_cumulative_fitness(Fitnesses)
    end.

%% @doc Roulette wheel selection with binary search.
-spec roulette_select(Cumulative :: [float()], Total :: float(), RandomVal :: float()) -> non_neg_integer().
roulette_select(Cumulative, Total, RandomVal) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:roulette_select(Cumulative, Total, RandomVal);
        _ -> tweann_nif_fallback:roulette_select(Cumulative, Total, RandomVal)
    end.

%% @doc Batch roulette selection.
-spec roulette_select_batch(Cumulative :: [float()], Total :: float(), RandomVals :: [float()]) -> [non_neg_integer()].
roulette_select_batch(Cumulative, Total, RandomVals) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:roulette_select_batch(Cumulative, Total, RandomVals);
        _ -> tweann_nif_fallback:roulette_select_batch(Cumulative, Total, RandomVals)
    end.

%% @doc Tournament selection.
-spec tournament_select(Contestants :: [non_neg_integer()], Fitnesses :: [float()]) -> non_neg_integer().
tournament_select(Contestants, Fitnesses) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:tournament_select(Contestants, Fitnesses);
        _ -> tweann_nif_fallback:tournament_select(Contestants, Fitnesses)
    end.

%%==============================================================================
%% Reward and Meta-Controller Functions
%%==============================================================================

%% @doc Compute z-score normalization.
-spec z_score(Value :: float(), Mean :: float(), StdDev :: float()) -> float().
z_score(Value, Mean, StdDev) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:z_score(Value, Mean, StdDev);
        _ -> tweann_nif_fallback:z_score(Value, Mean, StdDev)
    end.

%% @doc Compute reward component with normalization.
-spec compute_reward_component(History :: [float()], Current :: float()) -> {float(), float(), float()}.
compute_reward_component(History, Current) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:compute_reward_component(History, Current);
        _ -> tweann_nif_fallback:compute_reward_component(History, Current)
    end.

%% @doc Batch compute weighted reward.
-spec compute_weighted_reward(Components :: [{[float()], float(), float()}]) -> float().
compute_weighted_reward(Components) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:compute_weighted_reward(Components);
        _ -> tweann_nif_fallback:compute_weighted_reward(Components)
    end.

%%==============================================================================
%% Batch Mutation Functions (Evolutionary Genetics)
%%==============================================================================

%% @doc Mutate weights using gaussian perturbation.
-spec mutate_weights(Weights :: [float()], MutationRate :: float(), PerturbRate :: float(), PerturbStrength :: float()) -> [float()].
mutate_weights(Weights, MutationRate, PerturbRate, PerturbStrength) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:mutate_weights(Weights, MutationRate, PerturbRate, PerturbStrength);
        _ -> tweann_nif_fallback:mutate_weights(Weights, MutationRate, PerturbRate, PerturbStrength)
    end.

%% @doc Mutate weights with explicit seed for reproducibility.
-spec mutate_weights_seeded(Weights :: [float()], MutationRate :: float(), PerturbRate :: float(), PerturbStrength :: float(), Seed :: non_neg_integer()) -> [float()].
mutate_weights_seeded(Weights, MutationRate, PerturbRate, PerturbStrength, Seed) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:mutate_weights_seeded(Weights, MutationRate, PerturbRate, PerturbStrength, Seed);
        _ -> tweann_nif_fallback:mutate_weights_seeded(Weights, MutationRate, PerturbRate, PerturbStrength, Seed)
    end.

%% @doc Batch mutate multiple genomes with per-genome parameters.
-spec mutate_weights_batch(Genomes :: [{[float()], float(), float(), float()}]) -> [[float()]].
mutate_weights_batch(Genomes) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:mutate_weights_batch(Genomes);
        _ -> tweann_nif_fallback:mutate_weights_batch(Genomes)
    end.

%% @doc Batch mutate with uniform parameters.
-spec mutate_weights_batch_uniform(Genomes :: [[float()]], MutationRate :: float(), PerturbRate :: float(), PerturbStrength :: float()) -> [[float()]].
mutate_weights_batch_uniform(Genomes, MutationRate, PerturbRate, PerturbStrength) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:mutate_weights_batch_uniform(Genomes, MutationRate, PerturbRate, PerturbStrength);
        _ -> tweann_nif_fallback:mutate_weights_batch_uniform(Genomes, MutationRate, PerturbRate, PerturbStrength)
    end.

%% @doc Generate random weights uniformly distributed in [-1, 1].
-spec random_weights(N :: non_neg_integer()) -> [float()].
random_weights(N) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:random_weights(N);
        _ -> tweann_nif_fallback:random_weights(N)
    end.

%% @doc Generate random weights with explicit seed.
-spec random_weights_seeded(N :: non_neg_integer(), Seed :: non_neg_integer()) -> [float()].
random_weights_seeded(N, Seed) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:random_weights_seeded(N, Seed);
        _ -> tweann_nif_fallback:random_weights_seeded(N, Seed)
    end.

%% @doc Generate gaussian random weights from N(Mean, StdDev).
-spec random_weights_gaussian(N :: non_neg_integer(), Mean :: float(), StdDev :: float()) -> [float()].
random_weights_gaussian(N, Mean, StdDev) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:random_weights_gaussian(N, Mean, StdDev);
        _ -> tweann_nif_fallback:random_weights_gaussian(N, Mean, StdDev)
    end.

%% @doc Batch generate random weights for multiple genomes.
-spec random_weights_batch(Sizes :: [non_neg_integer()]) -> [[float()]].
random_weights_batch(Sizes) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:random_weights_batch(Sizes);
        _ -> tweann_nif_fallback:random_weights_batch(Sizes)
    end.

%% @doc Compute L1 (Manhattan) distance between weight vectors.
-spec weight_distance_l1(Weights1 :: [float()], Weights2 :: [float()]) -> float().
weight_distance_l1(Weights1, Weights2) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:weight_distance_l1(Weights1, Weights2);
        _ -> tweann_nif_fallback:weight_distance_l1(Weights1, Weights2)
    end.

%% @doc Compute L2 (Euclidean) distance between weight vectors.
-spec weight_distance_l2(Weights1 :: [float()], Weights2 :: [float()]) -> float().
weight_distance_l2(Weights1, Weights2) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:weight_distance_l2(Weights1, Weights2);
        _ -> tweann_nif_fallback:weight_distance_l2(Weights1, Weights2)
    end.

%% @doc Batch compute weight distances from target to many others.
-spec weight_distance_batch(Target :: [float()], Others :: [[float()]], UseL2 :: boolean()) -> [{non_neg_integer(), float()}].
weight_distance_batch(Target, Others, UseL2) ->
    case impl_module() of
        faber_nn_nifs -> faber_nn_nifs:weight_distance_batch(Target, Others, UseL2);
        _ -> tweann_nif_fallback:weight_distance_batch(Target, Others, UseL2)
    end.
