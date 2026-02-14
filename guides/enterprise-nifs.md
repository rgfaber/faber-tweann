# Enterprise NIF Acceleration

This guide explains how to enable high-performance Rust NIFs for enterprise users.

## Overview

faber-tweann has two editions:

| Feature | Community (hex.pm) | Enterprise (private git) |
|---------|-------------------|-------------------------|
| Core TWEANN | Yes | Yes |
| Pure Erlang fallback | Yes | Yes |
| Bundled Rust NIFs | No | Yes |
| Enterprise NIF package | No | Yes |
| Performance | Baseline | 10-15x faster |
| Rust toolchain required | No | Yes |

## Performance Improvements

Enterprise NIFs provide significant speedups for compute-intensive operations:

| Operation | Speedup |
|-----------|---------|
| Network evaluation | ~10x |
| Batch mutation | ~13x |
| KNN novelty (search) | ~12x |
| Fitness statistics | ~12x |
| Weight distance | ~15x |

## Installation

### Community Edition (Default)

Install from hex.pm - no additional setup required:

```erlang
{deps, [
    {faber_tweann, "~> 0.17.0"}
]}.
```

Uses pure Erlang fallbacks automatically.

### Enterprise Edition

Add the private enterprise NIF package alongside faber-tweann:

```erlang
{deps, [
    {faber_tweann, "~> 0.17.0"},
    {faber_nn_nifs, {git, "git@github.com:rgfaber/faber-nn-nifs.git", {tag, "v0.2.0"}}}
]}.
```

Requirements:
- Rust 1.70+ and Cargo installed
- SSH access to the private faber-nn-nifs repository

## How It Works

The `tweann_nif` module automatically detects and uses enterprise NIFs:

```
Priority Order:
1. faber_nn_nifs (enterprise) - If available and loaded
2. Bundled NIF (tweann_nif) - If compiled with Rust
3. Pure Erlang (tweann_nif_fallback) - Always available
```

No code changes required - detection is automatic at startup.

## Verification

Check which implementation is being used:

```erlang
1> tweann_nif:is_loaded().
true  %% NIFs available (enterprise or bundled)

2> faber_nn_nifs:is_loaded().
true  %% Enterprise NIFs specifically
```

## Accelerated Functions

The enterprise NIFs accelerate 67 functions across these categories:

### Network Evaluation
- `compile_network/3` - Compile topology for fast evaluation
- `evaluate/2` - Forward propagation
- `evaluate_batch/2` - Batch evaluation
- `compatibility_distance/5` - NEAT speciation distance
- `benchmark_evaluate/3` - Performance benchmarking

### Signal Aggregation
- `dot_product_flat/3` - Weighted sum with bias
- `dot_product_batch/1` - Batch weighted sums
- `dot_product_preflattened/3` - Pre-optimized dot product
- `flatten_weights/1` - Weight structure optimization

### LTC/CfC Neurons
- `evaluate_cfc/4` - Closed-form continuous-time evaluation
- `evaluate_cfc_with_weights/6` - CfC with custom weights
- `evaluate_ode/5` - ODE-based LTC evaluation
- `evaluate_ode_with_weights/7` - ODE with custom weights
- `evaluate_cfc_batch/4` - Batch CfC for time series

### Novelty Search
- `euclidean_distance/2` - Vector distance
- `euclidean_distance_batch/2` - Batch distances
- `knn_novelty/4` - K-nearest neighbor novelty
- `knn_novelty_batch/3` - Batch novelty computation

### Statistics
- `fitness_stats/1` - Single-pass min/max/mean/variance/stddev/sum
- `weighted_moving_average/2` - WMA computation
- `shannon_entropy/1` - Entropy calculation
- `histogram/4` - Histogram binning

### Selection
- `build_cumulative_fitness/1` - Roulette wheel setup
- `roulette_select/3` - Binary search roulette selection
- `roulette_select_batch/3` - Batch selection
- `tournament_select/2` - Tournament selection

### Meta-Controller
- `z_score/3` - Z-score normalization
- `compute_reward_component/2` - Reward signal computation
- `compute_weighted_reward/1` - Multi-component rewards

### Evolutionary Genetics
- `mutate_weights/4` - Gaussian weight mutation
- `mutate_weights_seeded/5` - Reproducible mutation
- `mutate_weights_batch/1` - Batch mutation with per-genome params
- `mutate_weights_batch_uniform/4` - Batch with uniform params
- `random_weights/1` - Generate random weights
- `random_weights_seeded/2` - Seeded random weights
- `random_weights_gaussian/3` - Gaussian distributed weights
- `random_weights_batch/1` - Batch weight generation
- `weight_distance_l1/2` - L1 (Manhattan) distance
- `weight_distance_l2/2` - L2 (Euclidean) distance
- `weight_distance_batch/3` - Batch distance computation

### SIMD Batch Activations (NEW in v0.2.0)
- `tanh_batch/1` - Vectorized tanh activation
- `sigmoid_batch/1` - Vectorized sigmoid activation
- `relu_batch/1` - Vectorized ReLU activation
- `softmax_batch/1` - Vectorized softmax activation

### Layer-Specific Mutation (NEW in v0.2.0)
- `layer_specific_mutate/3` - Mutate with per-layer rates
- `layer_specific_mutate_batch/1` - Batch layer-specific mutation

### Plasticity Rules (NEW in v0.2.0)
- `hebbian_update/4` - Hebbian plasticity weight update
- `modulated_hebbian_batch/4` - Batch modulated Hebbian learning
- `stdp_update/5` - STDP (Spike-Timing Dependent Plasticity)
- `oja_update/5` - Oja's learning rule

### Extended CfC/LTC (NEW in v0.2.0)
- `evaluate_cfc_sequence/5` - Sequential CfC evaluation
- `evaluate_cfc_parallel/1` - Parallel CfC for multiple neurons
- `ltc_state_batch/4` - Batch LTC state computation

### Population Analysis (NEW in v0.2.0)
- `population_diversity/1` - Mean/min/max/variance of population
- `weight_covariance_matrix/1` - Weight correlation analysis
- `pairwise_distances_batch/2` - Batch pairwise distance matrix

### NEAT Crossover (NEW in v0.2.0)
- `neat_crossover/4` - NEAT-style genome crossover
- `align_genes_by_innovation/2` - Align genes by innovation number
- `count_excess_disjoint/2` - Count excess and disjoint genes

### Speciation (NEW in v0.2.0)
- `kmeans_speciation/3` - K-means based speciation clustering

### Matrix Operations (NEW in v0.2.0)
- `matmul_add_bias/3` - Matrix multiply with bias addition
- `layer_forward/3` - Single layer forward pass
- `multi_layer_forward/2` - Multi-layer forward pass

### Meta-Learning (NEW in v0.2.0)
- `gradient_free_meta_learn/4` - Gradient-free meta-learning

### Network Compression (NEW in v0.2.0)
- `network_prune/2` - Prune small weights
- `weight_quantize/2` - Quantize weights to fixed precision

## Enterprise Licensing

Contact licensing@rgfaber for enterprise access to:
- Private faber-nn-nifs repository
- Priority support
- Custom NIF development

## Troubleshooting

### NIFs not detected

Check if faber_nn_nifs is in your dependency path:

```erlang
code:which(faber_nn_nifs).
%% Should return path, not 'non_existing'
```

### Rust compilation fails

Ensure Rust toolchain is installed:

```bash
rustc --version   # Should be 1.70+
cargo --version
```

### Performance not improved

Verify NIFs are loaded:

```erlang
faber_nn_nifs:is_loaded().  %% Should be true
```

If false, check for NIF loading errors in the Erlang shell output.
