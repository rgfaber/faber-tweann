# LTC Usage Guide

This guide shows you how to use Liquid Time-Constant (LTC) neurons in your faber-tweann projects.

## Quick Start

### Using the ltc_dynamics Module Directly

For standalone LTC computation without the full TWEANN infrastructure:

```erlang
%% CfC (Closed-form) - Fast mode (~100x faster than ODE)
Input = 1.0,
InitialState = 0.0,
Tau = 1.0,           %% Time constant
Bound = 1.0,         %% State bound

{NewState, Output} = ltc_dynamics:evaluate_cfc(Input, InitialState, Tau, Bound).
%% NewState ≈ 0.68, Output ≈ 0.68

%% Multiple steps with state persistence
{State1, _} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 1.0, 1.0),
{State2, _} = ltc_dynamics:evaluate_cfc(1.0, State1, 1.0, 1.0),
{State3, _} = ltc_dynamics:evaluate_cfc(1.0, State2, 1.0, 1.0).
%% State converges toward tanh(1.0) ≈ 0.76
```

### Using LTC Neurons in a Network

To create neurons with LTC dynamics, set the `neuron_type` field:

```erlang
%% Create a neuron record with CfC dynamics
Neuron = #neuron{
    id = {{0.5, rand:uniform()}, neuron},
    cx_id = CortexId,
    neuron_type = cfc,           %% Enable CfC mode
    time_constant = 1.0,
    state_bound = 1.0,
    ltc_backbone_weights = [],   %% Simple mode (no learned weights)
    ltc_head_weights = [],
    internal_state = 0.0,
    af = tanh,
    aggr_f = dot_product,
    input_idps = [],
    output_ids = []
}.
```

## LTC Dynamics API

### CfC Evaluation (Fast)

```erlang
-spec evaluate_cfc(Input, State, Tau, Bound) -> {NewState, Output}
    when Input :: float() | [float()],
         State :: float(),
         Tau :: float(),
         Bound :: float(),
         NewState :: float(),
         Output :: float().

%% Example: Single input
{NewState, Output} = ltc_dynamics:evaluate_cfc(0.5, 0.0, 1.0, 1.0).

%% Example: Vector input (summed internally)
{NewState, Output} = ltc_dynamics:evaluate_cfc([0.3, 0.2, 0.5], 0.0, 1.0, 1.0).
```

### CfC with Custom Weights

```erlang
-spec evaluate_cfc(Input, State, Tau, Bound, Params) -> {NewState, Output}
    when Params :: #{
        backbone_weights => [float()],
        head_weights => [float()]
    }.

Params = #{
    backbone_weights => [0.5, 0.1],  %% Custom f() backbone
    head_weights => [0.8, -0.2]      %% Custom h() head
},
{NewState, Output} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 1.0, 1.0, Params).
```

### ODE Evaluation (Accurate)

For training or when accuracy is more important than speed:

```erlang
-spec evaluate_ode(Input, State, Tau, Bound, Dt) -> {NewState, Output}
    when Dt :: float().  %% Time step for Euler integration

%% Smaller Dt = more accurate, slower
{NewState, Output} = ltc_dynamics:evaluate_ode(1.0, 0.0, 1.0, 1.0, 0.01).

%% Larger Dt = less accurate, faster
{NewState, Output} = ltc_dynamics:evaluate_ode(1.0, 0.0, 1.0, 1.0, 0.1).
```

## Utility Functions

### State Management

```erlang
%% Clamp state to bounds
ClampedState = ltc_dynamics:clamp_state(1.5, 1.0).  %% Returns 1.0
ClampedState = ltc_dynamics:clamp_state(-2.0, 1.0). %% Returns -1.0

%% Reset state
InitialState = ltc_dynamics:reset_state().  %% Returns 0.0
```

### Activation Functions

```erlang
%% Sigmoid (used for gating)
S = ltc_dynamics:sigmoid(0.0).   %% Returns 0.5
S = ltc_dynamics:sigmoid(2.0).   %% Returns ~0.88

%% Tanh (used for target state)
T = ltc_dynamics:tanh(0.0).      %% Returns 0.0
T = ltc_dynamics:tanh(1.0).      %% Returns ~0.76
```

### Network Functions

```erlang
%% Compute backbone f() - modulates time constant
F = ltc_dynamics:compute_backbone(Input, Tau, Weights).

%% Compute head h() - target state
H = ltc_dynamics:compute_head(Input, Weights).

%% Compute liquid time constant
EffectiveTau = ltc_dynamics:compute_liquid_tau(Input, State, BaseTau, Params).
```

## Neuron Process API

### Starting an LTC Neuron

```erlang
{ok, Pid} = neuron_ltc:start_link(#{
    id => NeuronId,
    cortex_pid => CortexPid,
    neuron_type => cfc,              %% or 'ltc' for ODE mode
    time_constant => 1.0,
    state_bound => 1.0,
    ltc_backbone_weights => [],
    ltc_head_weights => [],
    internal_state => 0.0,
    input_pids => InputPids,
    output_pids => OutputPids,
    ro_pids => [],
    input_weights => WeightsMap,
    bias => 0.0
}).
```

### Sending Signals

```erlang
%% Forward a signal to the neuron
neuron_ltc:forward(NeuronPid, FromPid, [Signal]).
```

### State Operations

```erlang
%% Reset internal state (e.g., start of new episode)
neuron_ltc:reset_state(NeuronPid).

%% Get current internal state
neuron_ltc:get_state(NeuronPid).
%% Receive: {ltc_state, NeuronId, InternalState}
```

### Updating Parameters

```erlang
%% Update LTC-specific parameters
Pid ! {update_ltc_params, #{
    time_constant => 1.5,
    state_bound => 2.0,
    ltc_backbone_weights => [0.5],
    ltc_head_weights => [0.8]
}}.
```

## Parameter Tuning Guide

### Time Constant (tau)

| Value | Effect | Use Case |
|-------|--------|----------|
| 0.1-0.5 | Fast response | Quick reactions, high-frequency data |
| 0.5-2.0 | Balanced | General purpose |
| 2.0-10.0 | Slow, smooth | Filtering noise, long-term patterns |

### State Bound (A)

| Value | Effect | Use Case |
|-------|--------|----------|
| 0.5 | Constrained output | Stable, conservative |
| 1.0 | Standard range | General purpose |
| 2.0+ | Wide range | When larger dynamics needed |

### Time Step (dt) for ODE Mode

| Value | Accuracy | Speed |
|-------|----------|-------|
| 0.001 | Very high | Very slow |
| 0.01 | High | Slow |
| 0.1 | Moderate | Moderate |
| 0.5 | Low | Fast |

**Recommendation**: Use CfC mode for inference (no dt needed), ODE only for training/research.

## Example: Time Series Prediction

```erlang
%% Process a sequence with LTC
process_sequence(Sequence) ->
    InitState = 0.0,
    Tau = 1.0,
    Bound = 1.0,

    {FinalState, Outputs} = lists:foldl(
        fun(Input, {State, Acc}) ->
            {NewState, Output} = ltc_dynamics:evaluate_cfc(Input, State, Tau, Bound),
            {NewState, [Output | Acc]}
        end,
        {InitState, []},
        Sequence
    ),

    {FinalState, lists:reverse(Outputs)}.

%% Usage
Sequence = [0.1, 0.3, 0.5, 0.7, 0.9],
{FinalState, Outputs} = process_sequence(Sequence).
%% Outputs show smooth temporal integration
```

## Example: Adaptive Response

```erlang
%% LTC naturally responds faster to sudden changes
demonstrate_adaptation() ->
    Tau = 1.0,
    Bound = 1.0,

    %% Steady state with constant input
    {S1, _} = ltc_dynamics:evaluate_cfc(0.5, 0.0, Tau, Bound),
    {S2, _} = ltc_dynamics:evaluate_cfc(0.5, S1, Tau, Bound),
    {S3, _} = ltc_dynamics:evaluate_cfc(0.5, S2, Tau, Bound),
    %% S3 ≈ tanh(0.5) ≈ 0.46 (converged)

    %% Sudden change - larger input causes faster response
    {S4, _} = ltc_dynamics:evaluate_cfc(2.0, S3, Tau, Bound),
    %% S4 jumps significantly toward tanh(2.0) ≈ 0.96

    {S1, S2, S3, S4}.
```

## Integration with Evolution

LTC parameters evolve alongside network topology using the standard TWEANN mutation and crossover operators.

### LTC Mutation Operators

The `genome_mutator` module includes LTC-specific mutation operators:

```erlang
%% Mutate time constant (tau)
%% Perturbs tau by a small random amount within bounds [0.01, 100.0]
genome_mutator:mutate_time_constant(AgentId).

%% Mutate state bound (A)
%% Perturbs bound by a small random amount within [0.1, 10.0]
genome_mutator:mutate_state_bound(AgentId).

%% Mutate neuron type
%% Switches between standard <-> ltc <-> cfc with configurable probability
genome_mutator:mutate_neuron_type(AgentId).

%% Mutate LTC weights (backbone and head)
%% Perturbs both the f() backbone and h() head network weights
genome_mutator:mutate_ltc_weights(AgentId).
```

### LTC Crossover

When two agents with LTC neurons are crossed over, LTC parameters are inherited:

```erlang
%% Crossover inherits LTC parameters from fitter parent (or random if equal)
%% Parameters inherited: neuron_type, time_constant, state_bound,
%%                       ltc_backbone_weights, ltc_head_weights, internal_state

%% Example: crossover preserves LTC dynamics
{ok, ChildId} = crossover:crossover(ParentA_Id, ParentB_Id).
%% Child neurons inherit LTC parameters from parents
```

### LTC-Aware Speciation

The species identification system considers LTC parameters when computing compatibility distance:

```erlang
%% Calculate LTC-specific distance between two agents
LtcDistance = species_identifier:calculate_ltc_distance(AgentA_Id, AgentB_Id).

%% Combined distance (behavioral + LTC)
%% Default weights: 70% behavioral, 30% LTC
CombinedDistance = species_identifier:calculate_combined_distance(
    AgentA_Id, AgentB_Id,
    0.7,    %% Behavioral weight
    0.3     %% LTC weight
).
```

The LTC distance is computed from:
- **ltc_ratio**: Proportion of LTC neurons in the network
- **avg_tau**: Average time constant across LTC neurons
- **avg_bound**: Average state bound
- **tau_std**: Standard deviation of tau values (diversity measure)

### Rust NIF Acceleration

LTC evaluation is accelerated via Rust NIFs for high-throughput applications:

```erlang
%% CfC evaluation via Rust NIF (~100x faster than pure Erlang ODE)
{NewState, Output} = tweann_nif:evaluate_cfc(Input, State, Tau, Bound).

%% CfC with custom weights via NIF
{NewState, Output} = tweann_nif:evaluate_cfc_with_weights(
    Input, State, Tau, Bound, BackboneWeights, HeadWeights
).

%% ODE evaluation via NIF (when accuracy is needed)
{NewState, Output} = tweann_nif:evaluate_ode(Input, State, Tau, Bound, Dt).

%% Batch CfC evaluation (process entire sequences efficiently)
Results = tweann_nif:evaluate_cfc_batch(InputSequence, InitialState, Tau, Bound).
%% Returns: [{State1, Output1}, {State2, Output2}, ...]
```

## Performance Tips

1. **Use CfC mode for inference** - 100x faster than ODE
2. **Use Rust NIFs for production** - `tweann_nif:evaluate_cfc/4` for maximum throughput
3. **Batch evaluations when possible** - Use `tweann_nif:evaluate_cfc_batch/4` for sequences
4. **Reset state between episodes** - Prevents state leakage across training episodes
5. **Tune tau per application** - Different tasks need different dynamics

## Troubleshooting

### State Explosion

If outputs become very large:
- Check that `state_bound` is set appropriately
- Verify inputs are normalized
- Consider reducing `tau`

### Slow Convergence

If the network takes too long to respond:
- Decrease `tau` for faster dynamics
- Use CfC mode instead of ODE
- Check that inputs are scaled properly

### NaN/Inf Values

If you see NaN or Inf:
- The `clamp` function should prevent this
- Check for division by zero in custom weights
- Ensure `tau` is always positive (minimum 0.001)

## Next Steps

- Read [LTC Neurons Concepts](neuron-types.md) for theory
- See [Custom Morphologies](CUSTOM_MORPHOLOGIES.md) to create LTC-based morphologies
- Check the `ltc_dynamics` module documentation for complete function reference
