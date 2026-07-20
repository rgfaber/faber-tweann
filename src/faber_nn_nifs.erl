%% @doc High-performance Rust NIFs for faber_tweann.
%%
%% This module provides native implementations of compute-intensive operations
%% for neuroevolution. It was absorbed into faber_tweann in v2.0.0 from the
%% separate faber-nn-nifs package, and is built from source by
%% priv/build-nifs.sh during compilation.
%%
%% Do not call this module directly. Use tweann_nif, which dispatches to either
%% this module or tweann_nif_fallback according to the {faber_tweann,
%% [{nif_impl, _}]} setting, and reports the active path via tweann_nif:impl/0.
%%
%% Every function here that also exists in tweann_nif_fallback must produce the
%% same result. That is enforced by
%% test/unit/nif_fallback_conformance_tests.erl, which exists because the two
%% implementations silently disagreed for months while both test suites passed.
%%
%% @copyright 2025 rgfaber
%% @license Apache-2.0
-module(faber_nn_nifs).

-export([
    %% NIF loading
    is_loaded/0,

    %% Network compilation and evaluation
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
    weight_distance_batch/3,

    %% P0: Layer-specific Mutation
    mutate_weights_layered/6,
    compute_layer_weight_counts/1,

    %% P0: Batch Activations
    tanh_batch/1,
    sigmoid_batch/1,
    relu_batch/1,
    softmax_batch/1,
    activation_batch/2,

    %% P1: Plasticity Computation
    hebbian_update_batch/4,
    modulated_hebbian_batch/5,
    stdp_update/5,
    oja_update_batch/4,

    %% P1: Time Series LTC/CfC
    evaluate_cfc_sequence/5,
    evaluate_cfc_parallel/4,
    ltc_state_batch/4,

    %% P1: Population Diversity
    population_diversity/1,
    weight_covariance_matrix/1,
    pairwise_distances_batch/2,

    %% P2: NEAT Crossover
    neat_crossover/4,
    align_genes_by_innovation/2,
    count_excess_disjoint/2,

    %% P2: Speciation Clustering
    assign_species_batch/3,
    find_representative/2,
    kmeans_cluster/3,

    %% P3: Matrix Operations
    matmul_add_bias/3,
    layer_forward/4,
    multi_layer_forward/3
]).

-on_load(init/0).

%% ============================================================================
%% NIF Loading
%% ============================================================================

-spec init() -> ok | {error, term()}.
init() ->
    %% This module was absorbed into faber_tweann in v2.0.0, so its shared
    %% library lives in faber_tweann's priv dir. There is no faber_nn_nifs
    %% application any more, and code:priv_dir(?MODULE) would return
    %% {error, bad_name}.
    PrivDir = case code:priv_dir(faber_tweann) of
        {error, bad_name} ->
            %% Application not in the code path yet (e.g. during eunit before
            %% the .app is loaded). Locate priv relative to this beam file.
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename), "..", "priv"]);
                _ ->
                    "priv"
            end;
        Dir ->
            Dir
    end,
    SoName = filename:join(PrivDir, "libfaber_nn_nifs"),
    case erlang:load_nif(SoName, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;  %% Already loaded
        {error, Reason} -> {error, Reason}
    end.

%% @doc Check if NIF is loaded successfully.
-spec is_loaded() -> boolean().
is_loaded() ->
    try
        %% Try a simple NIF call to verify loading
        _ = random_weights(0),
        true
    catch
        error:undef -> false;
        error:nif_not_loaded -> false
    end.

%% ============================================================================
%% Network Compilation and Evaluation
%% ============================================================================

-spec compile_network(list(), non_neg_integer(), [non_neg_integer()]) -> reference().
compile_network(_Nodes, _InputCount, _OutputIndices) ->
    erlang:nif_error(nif_not_loaded).

-spec evaluate(reference(), [float()]) -> [float()].
evaluate(_Network, _Inputs) ->
    erlang:nif_error(nif_not_loaded).

-spec evaluate_batch(reference(), [[float()]]) -> [[float()]].
evaluate_batch(_Network, _InputsList) ->
    erlang:nif_error(nif_not_loaded).

-spec compatibility_distance(list(), list(), float(), float(), float()) -> float().
compatibility_distance(_ConnectionsA, _ConnectionsB, _C1, _C2, _C3) ->
    erlang:nif_error(nif_not_loaded).

-spec benchmark_evaluate(reference(), [float()], pos_integer()) -> float().
benchmark_evaluate(_Network, _Inputs, _Iterations) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% Signal Aggregation
%% ============================================================================

-spec dot_product_flat([float()], [float()], float()) -> float().
dot_product_flat(_Signals, _Weights, _Bias) ->
    erlang:nif_error(nif_not_loaded).

-spec dot_product_batch([{[float()], [float()], float()}]) -> [float()].
dot_product_batch(_Batch) ->
    erlang:nif_error(nif_not_loaded).

-spec dot_product_preflattened([float()], [float()], float()) -> float().
dot_product_preflattened(_SignalsFlat, _WeightsFlat, _Bias) ->
    erlang:nif_error(nif_not_loaded).

-spec flatten_weights([{term(), [{float(), float(), float(), list()}]}]) ->
    {[float()], [non_neg_integer()]}.
flatten_weights(_WeightedInputs) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% LTC/CfC Functions
%% ============================================================================

-spec evaluate_cfc(float(), float(), float(), float()) -> {float(), float()}.
evaluate_cfc(_Input, _State, _Tau, _Bound) ->
    erlang:nif_error(nif_not_loaded).

-spec evaluate_cfc_with_weights(float(), float(), float(), float(), [float()], [float()]) ->
    {float(), float()}.
evaluate_cfc_with_weights(_Input, _State, _Tau, _Bound, _BackboneWeights, _HeadWeights) ->
    erlang:nif_error(nif_not_loaded).

-spec evaluate_ode(float(), float(), float(), float(), float()) -> {float(), float()}.
evaluate_ode(_Input, _State, _Tau, _Bound, _Dt) ->
    erlang:nif_error(nif_not_loaded).

-spec evaluate_ode_with_weights(float(), float(), float(), float(), float(), [float()], [float()]) ->
    {float(), float()}.
evaluate_ode_with_weights(_Input, _State, _Tau, _Bound, _Dt, _BackboneWeights, _HeadWeights) ->
    erlang:nif_error(nif_not_loaded).

-spec evaluate_cfc_batch([float()], float(), float(), float()) -> [{float(), float()}].
evaluate_cfc_batch(_Inputs, _InitialState, _Tau, _Bound) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% Distance and KNN (Novelty Search)
%% ============================================================================

-spec euclidean_distance([float()], [float()]) -> float().
euclidean_distance(_V1, _V2) ->
    erlang:nif_error(nif_not_loaded).

-spec euclidean_distance_batch([float()], [[float()]]) -> [{non_neg_integer(), float()}].
euclidean_distance_batch(_Target, _Others) ->
    erlang:nif_error(nif_not_loaded).

-spec knn_novelty([float()], [[float()]], [[float()]], pos_integer()) -> float().
knn_novelty(_Target, _Population, _Archive, _K) ->
    erlang:nif_error(nif_not_loaded).

-spec knn_novelty_batch([[float()]], [[float()]], pos_integer()) -> [float()].
knn_novelty_batch(_Behaviors, _Archive, _K) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% Statistics
%% ============================================================================

-spec fitness_stats([float()]) -> {float(), float(), float(), float(), float(), float()}.
fitness_stats(_Fitnesses) ->
    erlang:nif_error(nif_not_loaded).

-spec weighted_moving_average([float()], float()) -> float().
weighted_moving_average(_Values, _Decay) ->
    erlang:nif_error(nif_not_loaded).

-spec shannon_entropy([float()]) -> float().
shannon_entropy(_Values) ->
    erlang:nif_error(nif_not_loaded).

-spec histogram([float()], pos_integer(), float(), float()) -> [non_neg_integer()].
histogram(_Values, _NumBins, _MinVal, _MaxVal) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% Selection
%% ============================================================================

-spec build_cumulative_fitness([float()]) -> {[float()], float()}.
build_cumulative_fitness(_Fitnesses) ->
    erlang:nif_error(nif_not_loaded).

-spec roulette_select([float()], float(), float()) -> non_neg_integer().
roulette_select(_Cumulative, _Total, _RandomVal) ->
    erlang:nif_error(nif_not_loaded).

-spec roulette_select_batch([float()], float(), [float()]) -> [non_neg_integer()].
roulette_select_batch(_Cumulative, _Total, _RandomVals) ->
    erlang:nif_error(nif_not_loaded).

-spec tournament_select([non_neg_integer()], [float()]) -> non_neg_integer().
tournament_select(_Contestants, _Fitnesses) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% Reward and Meta-Controller
%% ============================================================================

-spec z_score(float(), float(), float()) -> float().
z_score(_Value, _Mean, _StdDev) ->
    erlang:nif_error(nif_not_loaded).

-spec compute_reward_component([float()], float()) -> {float(), float(), float()}.
compute_reward_component(_History, _Current) ->
    erlang:nif_error(nif_not_loaded).

-spec compute_weighted_reward([{[float()], float(), float()}]) -> float().
compute_weighted_reward(_Components) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% Batch Mutation (Evolutionary Genetics)
%% ============================================================================

-spec mutate_weights([float()], float(), float(), float()) -> [float()].
mutate_weights(_Weights, _MutationRate, _PerturbRate, _PerturbStrength) ->
    erlang:nif_error(nif_not_loaded).

-spec mutate_weights_seeded([float()], float(), float(), float(), integer()) -> [float()].
mutate_weights_seeded(_Weights, _MutationRate, _PerturbRate, _PerturbStrength, _Seed) ->
    erlang:nif_error(nif_not_loaded).

-spec mutate_weights_batch([{[float()], float(), float(), float()}]) -> [[float()]].
mutate_weights_batch(_Genomes) ->
    erlang:nif_error(nif_not_loaded).

-spec mutate_weights_batch_uniform([[float()]], float(), float(), float()) -> [[float()]].
mutate_weights_batch_uniform(_Genomes, _MutationRate, _PerturbRate, _PerturbStrength) ->
    erlang:nif_error(nif_not_loaded).

-spec random_weights(non_neg_integer()) -> [float()].
random_weights(_N) ->
    erlang:nif_error(nif_not_loaded).

-spec random_weights_seeded(non_neg_integer(), integer()) -> [float()].
random_weights_seeded(_N, _Seed) ->
    erlang:nif_error(nif_not_loaded).

-spec random_weights_gaussian(non_neg_integer(), float(), float()) -> [float()].
random_weights_gaussian(_N, _Mean, _StdDev) ->
    erlang:nif_error(nif_not_loaded).

-spec random_weights_batch([{non_neg_integer(), float(), float()}]) -> [[float()]].
random_weights_batch(_Specs) ->
    erlang:nif_error(nif_not_loaded).

-spec weight_distance_l1([float()], [float()]) -> float().
weight_distance_l1(_Weights1, _Weights2) ->
    erlang:nif_error(nif_not_loaded).

-spec weight_distance_l2([float()], [float()]) -> float().
weight_distance_l2(_Weights1, _Weights2) ->
    erlang:nif_error(nif_not_loaded).

-spec weight_distance_batch([float()], [[float()]], l1 | l2) -> [float()].
weight_distance_batch(_Target, _Others, _DistanceType) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P0: Layer-specific Mutation
%% ============================================================================

%% @doc Mutate weights with different rates for reservoir vs readout layers.
%%
%% Applies different mutation parameters to reservoir (hidden) layers and
%% readout (output) layer, supporting the reservoir computing paradigm where
%% readout weights often benefit from more aggressive mutation.
%%
%% ReservoirWeightCount specifies how many weights belong to reservoir layers.
%% The remaining weights are treated as readout layer.
-spec mutate_weights_layered([float()], non_neg_integer(),
                             float(), float(),
                             float(), float()) -> [float()].
mutate_weights_layered(_Weights, _ReservoirWeightCount,
                       _ReservoirMutRate, _ReservoirStrength,
                       _ReadoutMutRate, _ReadoutStrength) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Compute weight counts per layer from topology.
%%
%% Given a topology structure (list of layer sizes), returns the number of
%% weights in each layer. Useful for layer-specific mutation.
%%
%% Topology format: [InputSize, Hidden1Size, Hidden2Size, ..., OutputSize]
%% Returns: [Layer1Weights, Layer2Weights, ...] where Layer N weights =
%%          LayerN_size * LayerN-1_size (fully connected assumption)
-spec compute_layer_weight_counts([non_neg_integer()]) -> [non_neg_integer()].
compute_layer_weight_counts(_Topology) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P0: Batch Activations
%% ============================================================================

%% @doc Apply tanh activation to a batch of values elementwise.
-spec tanh_batch([float()]) -> [float()].
tanh_batch(_Values) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Apply sigmoid activation to a batch of values elementwise.
-spec sigmoid_batch([float()]) -> [float()].
sigmoid_batch(_Values) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Apply ReLU activation to a batch of values elementwise.
-spec relu_batch([float()]) -> [float()].
relu_batch(_Values) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Apply softmax to a batch of values (normalized exponential).
-spec softmax_batch([float()]) -> [float()].
softmax_batch(_Values) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Apply specified activation function to batch of values.
%% Activation: tanh | sigmoid | relu | linear | sin | cos | gaussian
-spec activation_batch([float()], atom()) -> [float()].
activation_batch(_Values, _Activation) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P1: Plasticity Computation
%% ============================================================================

%% @doc Batch Hebbian weight update.
%%
%% Implements Hebbian learning rule: dW = eta * pre * post
%% Where eta is the learning rate, pre is presynaptic activity,
%% post is postsynaptic activity.
%%
%% Input: list of {Weight, PreActivity, PostActivity} tuples
%% LearningRate: global learning rate multiplier
%% Returns: list of updated weights
-spec hebbian_update_batch([{float(), float(), float()}], float(), float(), float()) -> [float()].
hebbian_update_batch(_WeightActivities, _LearningRate, _DecayRate, _MaxWeight) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Batch modulated Hebbian update with reward signal.
%%
%% Implements reward-modulated Hebbian: dW = eta * reward * pre * post
%% Used for reinforcement learning where weight updates are gated by reward.
-spec modulated_hebbian_batch([{float(), float(), float()}], float(), float(), float(), float()) -> [float()].
modulated_hebbian_batch(_WeightActivities, _LearningRate, _Reward, _DecayRate, _MaxWeight) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Spike-Timing Dependent Plasticity (STDP) update.
%%
%% Implements STDP rule where weight change depends on relative timing
%% of pre and post synaptic spikes:
%% - Pre before post (positive delta_t): potentiation (strengthen)
%% - Post before pre (negative delta_t): depression (weaken)
%%
%% A_plus, A_minus: amplitude of potentiation/depression
%% Tau_plus, Tau_minus: time constants for decay
-spec stdp_update(float(), float(), float(), float(), float()) -> float().
stdp_update(_Weight, _DeltaT, _Aplus, _Aminus, _TauPlus) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Batch Oja's rule update (normalized Hebbian).
%%
%% Implements Oja's rule: dW = eta * (post * pre - post^2 * W)
%% This is a normalized Hebbian rule that prevents unbounded weight growth
%% and extracts principal components.
-spec oja_update_batch([{float(), float(), float()}], float(), float(), float()) -> [float()].
oja_update_batch(_WeightActivities, _LearningRate, _DecayRate, _MaxWeight) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P1: Time Series LTC/CfC
%% ============================================================================

%% @doc Evaluate CfC over a sequence of inputs with state persistence.
%%
%% Processes a time series through the CfC neuron, maintaining state
%% across timesteps. More efficient than calling evaluate_cfc repeatedly.
%%
%% Inputs: list of input values (time series)
%% InitialState: starting state value
%% Tau: time constant
%% Bound: output bound
%% BackboneWeights: optional backbone network weights
%%
%% Returns: list of {State, Output} tuples for each timestep
-spec evaluate_cfc_sequence([float()], float(), float(), float(), [float()]) ->
    [{float(), float()}].
evaluate_cfc_sequence(_Inputs, _InitialState, _Tau, _Bound, _BackboneWeights) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Evaluate multiple CfC neurons in parallel.
%%
%% Processes the same input through multiple CfC neurons with different
%% parameters. Useful for ensemble or population-based evaluation.
%%
%% Input: single input value
%% NeuronParams: list of {State, Tau, Bound} for each neuron
%% Returns: list of {NewState, Output} for each neuron
-spec evaluate_cfc_parallel(float(), [{float(), float(), float()}], [float()], [float()]) ->
    [{float(), float()}].
evaluate_cfc_parallel(_Input, _NeuronParams, _BackboneWeights, _HeadWeights) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Batch LTC state update for multiple neurons.
%%
%% Efficiently updates states for multiple LTC neurons given inputs.
%% Used when simulating a layer of LTC neurons.
%%
%% Inputs: input values for each neuron
%% States: current states for each neuron
%% Taus: time constants for each neuron
%% Dt: time step size
%%
%% Returns: list of new states
-spec ltc_state_batch([float()], [float()], [float()], float()) -> [float()].
ltc_state_batch(_Inputs, _States, _Taus, _Dt) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P1: Population Diversity
%% ============================================================================

%% @doc Compute population diversity metrics.
%%
%% Analyzes a population of weight vectors and computes diversity metrics:
%% - Mean pairwise distance
%% - Standard deviation of pairwise distances
%% - Minimum distance (most similar pair)
%% - Maximum distance (most different pair)
%%
%% Returns: {MeanDist, StdDist, MinDist, MaxDist}
-spec population_diversity([[float()]]) -> {float(), float(), float(), float()}.
population_diversity(_Population) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Compute weight covariance matrix.
%%
%% Computes the covariance matrix of weights across a population.
%% Used for CMA-ES and other covariance-aware evolution strategies.
%%
%% Population: list of weight vectors (all same length)
%% Returns: flattened covariance matrix (row-major order)
-spec weight_covariance_matrix([[float()]]) -> [float()].
weight_covariance_matrix(_Population) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Compute all pairwise distances in batch.
%%
%% Efficiently computes distances between all pairs in a population.
%% DistType: l1 | l2 | euclidean
%%
%% Returns: flattened distance matrix (upper triangular, row-major)
-spec pairwise_distances_batch([[float()]], l1 | l2) -> [float()].
pairwise_distances_batch(_Population, _DistType) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P2: NEAT Crossover
%% ============================================================================

%% @doc NEAT-style crossover between two genomes.
%%
%% Performs NEAT crossover where genes are aligned by innovation number:
%% - Matching genes: randomly select from either parent
%% - Disjoint/Excess genes: taken from more fit parent
%%
%% Genome format: list of {InnovationNum, Weight, Enabled} tuples
%% FitnessA, FitnessB: fitness values to determine which parent's
%%                     disjoint/excess genes to use
%%
%% Returns: offspring genome
-spec neat_crossover([{integer(), float(), boolean()}],
                     [{integer(), float(), boolean()}],
                     float(), float()) -> [{integer(), float(), boolean()}].
neat_crossover(_GenomeA, _GenomeB, _FitnessA, _FitnessB) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Align genes by innovation number.
%%
%% Returns aligned gene lists for crossover or comparison.
%% Each position contains {GeneA, GeneB} where Gene is either
%% the actual gene or 'none' if missing.
%%
%% Returns: [{GeneA | none, GeneB | none}, ...]
-spec align_genes_by_innovation([{integer(), float(), boolean()}],
                                [{integer(), float(), boolean()}]) ->
    [{term(), term()}].
align_genes_by_innovation(_GenomeA, _GenomeB) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Count excess and disjoint genes between two genomes.
%%
%% Excess genes: genes with innovation > max of other genome
%% Disjoint genes: non-matching genes within shared innovation range
%%
%% Returns: {ExcessCount, DisjointCount, MatchingCount}
-spec count_excess_disjoint([{integer(), float(), boolean()}],
                            [{integer(), float(), boolean()}]) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
count_excess_disjoint(_GenomeA, _GenomeB) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P2: Speciation Clustering
%% ============================================================================

%% @doc Assign genomes to species in batch.
%%
%% Assigns each genome to the most compatible existing species, or
%% creates a new species if no compatible species exists.
%%
%% Genomes: list of weight vectors
%% Representatives: current species representatives
%% Threshold: compatibility threshold for same-species assignment
%%
%% Returns: list of species indices (0-based)
-spec assign_species_batch([[float()]], [[float()]], float()) -> [non_neg_integer()].
assign_species_batch(_Genomes, _Representatives, _Threshold) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Find representative for a species.
%%
%% Finds the genome closest to the centroid (most representative)
%% of the given species members.
%%
%% Members: list of weight vectors in the species
%% Method: centroid | random | fittest
%%
%% Returns: index of representative in members list
-spec find_representative([[float()]], atom()) -> non_neg_integer().
find_representative(_Members, _Method) ->
    erlang:nif_error(nif_not_loaded).

%% @doc K-means clustering for genomes.
%%
%% Clusters genomes into K groups using k-means algorithm.
%% Useful for diversity-based speciation.
%%
%% Genomes: list of weight vectors
%% K: number of clusters
%% MaxIterations: maximum iterations for convergence
%%
%% Returns: list of cluster assignments (0 to K-1)
-spec kmeans_cluster([[float()]], pos_integer(), pos_integer()) -> [non_neg_integer()].
kmeans_cluster(_Genomes, _K, _MaxIterations) ->
    erlang:nif_error(nif_not_loaded).

%% ============================================================================
%% P3: Matrix Operations
%% ============================================================================

%% @doc Matrix multiply with bias addition.
%%
%% Computes Y = X * W + B efficiently for neural network forward pass.
%%
%% X: input vector (1 x N)
%% W: weight matrix (N x M, flattened row-major)
%% B: bias vector (1 x M)
%% Dims: {N, M} dimensions
%%
%% Returns: output vector (1 x M)
-spec matmul_add_bias([float()], [float()], [float()]) -> [float()].
matmul_add_bias(_X, _W, _B) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Single layer forward pass with activation.
%%
%% Computes layer output: activation(X * W + B)
%%
%% X: input vector
%% W: weight matrix (flattened)
%% B: bias vector
%% Activation: tanh | sigmoid | relu | linear
%%
%% Returns: activated output vector
-spec layer_forward([float()], [float()], [float()], atom()) -> [float()].
layer_forward(_X, _W, _B, _Activation) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Multi-layer forward pass through network.
%%
%% Efficiently processes input through multiple layers.
%% Avoids crossing NIF boundary for each layer.
%%
%% Input: input vector
%% Layers: list of {Weights, Biases, Activation} for each layer
%%
%% Returns: final output vector
-spec multi_layer_forward([float()], [{[float()], [float()], atom()}], [non_neg_integer()]) -> [float()].
multi_layer_forward(_Input, _Layers, _LayerSizes) ->
    erlang:nif_error(nif_not_loaded).
