# Liquid Time-Constant Neurons

Liquid Time-Constant (LTC) neurons are a breakthrough in neural network design that enable **adaptive temporal processing**. Unlike traditional neurons that produce instantaneous outputs, LTC neurons maintain internal state that evolves continuously based on input dynamics.

**faber-tweann is the first TWEANN library to implement LTC neurons in Erlang/OTP.**

![LTC Neuron Architecture](assets/ltc-neuron-architecture.svg)

## Quick Selection Guide

**Which neuron type should you use?** Use this decision tree:

```
Is your task temporal (time-series, sequences, control)?
├── NO → Use Traditional neurons (fastest, simplest)
└── YES → Do you need production-grade speed?
    ├── NO → Use LTC (ODE) for maximum accuracy during research
    └── YES → Use CfC (100x faster, equivalent expressivity)
```

| If your problem involves... | Use | Why |
|-----------------------------|-----|-----|
| Static classification (images, patterns) | **Traditional** | No temporal dynamics needed |
| Real-time control (robotics, games) | **CfC** | Fast response with temporal memory |
| Time-series prediction | **CfC** | Adaptive dynamics, production speed |
| Research/training on temporal tasks | **LTC (ODE)** | Maximum accuracy for learning |
| Maximum inference throughput | **Traditional** | Lowest computational overhead |

**Key Insight:** CfC is the practical choice for most temporal tasks. It provides equivalent expressivity to LTC-ODE at ~100x the speed. Reserve Traditional neurons for non-temporal tasks, and LTC-ODE for research where accuracy matters more than speed.

## Why LTC Neurons?

Traditional neural networks struggle with:

- **Temporal sequences**: Cannot naturally model time-varying signals
- **Adaptive response**: Fixed response speed regardless of input
- **Temporal memory**: No built-in mechanism for remembering past inputs

LTC neurons solve these problems with **input-dependent time constants** that allow the network to automatically adjust how quickly it responds to different inputs.

## The Mathematics

### Standard Neuron

A standard neuron computes an instantaneous output:

```
y = f(sum(w_i * x_i) + bias)
```

This is memoryless - the output depends only on current inputs.

### LTC Neuron

An LTC neuron maintains internal state `x(t)` governed by an ODE:

```
dx(t)/dt = -[1/tau + f(x, I, theta)] * x(t) + f(x, I, theta) * A
```

Where:
- `x(t)` = internal state at time t
- `tau` = base time constant (learnable)
- `f(...)` = nonlinear function producing the "liquid" time constant
- `A` = state bound (prevents explosion)
- `I(t)` = input at time t

The **liquid** aspect comes from the time constant varying based on both input and state - the neuron "flows" between fast and slow response modes.

![LTC Dynamics](assets/ltc-dynamics.svg)

### CfC Closed-Form Approximation

Solving the ODE numerically is computationally expensive. The **Closed-form Continuous-time (CfC)** approximation provides equivalent expressivity with ~100x speedup:

```
x(t+dt) = sigma(-f) * x(t) + (1 - sigma(-f)) * h
```

Where:
- `sigma` = sigmoid gate
- `f` = backbone network output (time constant modulator)
- `h` = head network output (target state)

This closed-form solution is what faber-tweann uses for production inference.

![CfC Closed-Form Architecture](assets/cfc-closed-form.svg)

## Neuron Types Comparison

![Neuron Types Comparison](assets/ltc-vs-standard-neurons.svg)

| Feature | Standard | LTC (ODE) | CfC (Fast) |
|---------|----------|-----------|------------|
| Internal State | No | Yes | Yes |
| Temporal Memory | No | Yes | Yes |
| Adaptive Dynamics | No | Yes | Yes |
| Speed | Fast | Slow | Fast (~100x vs ODE) |
| Use Case | Pattern recognition | Training/research | Production inference |

## Implementation in faber-tweann

### Core Modules

**ltc_dynamics.erl** - Core LTC/CfC computation:

```erlang
%% CfC evaluation (fast, closed-form)
ltc_dynamics:evaluate_cfc(Input, State, Tau, Bound) -> {NewState, Output}.

%% ODE evaluation (accurate, slower)
ltc_dynamics:evaluate_ode(Input, State, Tau, Bound, Dt) -> {NewState, Output}.
```

**neuron_ltc.erl** - LTC neuron process:

```erlang
%% Spawn an LTC neuron
neuron_ltc:start_link(#{
    id => NeuronId,
    neuron_type => cfc,           %% or 'ltc' for ODE mode
    time_constant => 1.0,         %% tau
    state_bound => 1.0,           %% A
    ltc_backbone_weights => [],
    ltc_head_weights => [],
    internal_state => 0.0
}).
```

### Extended Neuron Record

The `#neuron` record has been extended with LTC fields:

```erlang
-record(neuron, {
    %% ... standard fields ...

    %% LTC Extension Fields
    neuron_type = standard,       %% standard | ltc | cfc
    time_constant = 1.0,          %% tau (evolvable)
    state_bound = 1.0,            %% A (bounds)
    ltc_backbone_weights = [],    %% f() backbone
    ltc_head_weights = [],        %% h() head
    internal_state = 0.0          %% x(t) persistent
}).
```

### Constructor Integration

The `constructor.erl` module automatically spawns the appropriate neuron type:

```erlang
%% Neurons with neuron_type = cfc spawn as neuron_ltc
%% Neurons with neuron_type = ltc spawn as neuron_ltc (ODE mode)
%% Neurons with neuron_type = standard spawn as neuron
```

## Key Properties

### State Persistence

Unlike standard neurons, LTC neurons maintain state between evaluations:

```erlang
%% First evaluation
{State1, _} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 1.0, 1.0).
%% State1 is now non-zero

%% Second evaluation uses State1
{State2, _} = ltc_dynamics:evaluate_cfc(0.0, State1, 1.0, 1.0).
%% State2 influenced by State1
```

This enables temporal memory without explicit recurrent connections.

### Bounded Dynamics

The state bound `A` ensures numerical stability:

```erlang
%% State always clamped to [-A, A]
clamp_state(State, Bound) when State > Bound -> Bound;
clamp_state(State, Bound) when State < -Bound -> -Bound;
clamp_state(State, _Bound) -> State.
```

### Adaptive Time Constants

The "liquid" time constant varies with input:

```erlang
compute_liquid_tau(Input, State, BaseTau, Params) ->
    %% Modulation based on input magnitude
    Modulation = 1.0 + sigmoid(Input),
    EffectiveTau = BaseTau * Modulation,
    %% Clamp to reasonable range
    max(0.001, min(EffectiveTau, 100.0)).
```

## When to Use LTC Neurons

### Ideal Use Cases

1. **Time-series prediction**: Stock prices, sensor data, weather
2. **Real-time control**: Robotics, game AI, autonomous systems
3. **Sequence modeling**: Natural language, audio, video
4. **Adaptive response**: Systems that need different response speeds

### Snake AI Example

For the Snake Duel game, LTC neurons enable:

- **Temporal awareness**: Remember recent moves and food positions
- **Adaptive hunting**: Fast response when prey is near, cautious when threatened
- **Pattern learning**: Recognize opponent behavior patterns over time

### Not Ideal For

- **Static classification**: Use standard neurons for pure pattern matching
- **Maximum speed critical**: Standard neurons have less overhead
- **Simple problems**: XOR doesn't need temporal dynamics

## Academic References

The LTC implementation in faber-tweann is based on peer-reviewed research:

1. **Hasani, R., Lechner, M., et al. (2021)**
   "Liquid Time-constant Networks"
   *Proceedings of the AAAI Conference on Artificial Intelligence*

2. **Hasani, R., Lechner, M., et al. (2022)**
   "Closed-form Continuous-time Neural Networks"
   *Nature Machine Intelligence*

3. **Beer, R.D. (1995)**
   "On the Dynamics of Small Continuous-Time Recurrent Neural Networks"
   *Adaptive Behavior, 3(4)*

## Research Opportunities

LTC neurons in an evolutionary context open exciting research directions:

### Temporal Dynamics Evolution

Unlike fixed architectures like LSTMs, faber-tweann evolves LTC parameters:

- **Tau Evolution**: Networks discover optimal time constants for different temporal scales
- **Multi-timescale Systems**: Single networks can evolve neurons with different tau values, capturing both fast reactions and slow trends
- **Adaptive Plasticity**: Combine LTC dynamics with Hebbian learning for biologically plausible temporal learning

### Hybrid Architectures

The ability to mix standard and LTC neurons enables novel architectures:

- **Temporal Gating**: Standard neurons for pattern recognition, LTC for temporal integration
- **Hierarchical Timing**: Fast LTC neurons at input, slow LTC at decision layers
- **Emergent Temporal Structure**: Let evolution discover which neurons need temporal dynamics

### Comparative Studies

faber-tweann provides a unique platform for comparing temporal approaches:

- **LTC vs LSTM**: Evolved LTC networks vs manually-designed recurrent architectures
- **CfC Efficiency**: When does the ~100x speedup justify the closed-form approximation?
- **Temporal Necessity**: Which problems truly require temporal dynamics vs simple feedforward?

### Application Domains

High-potential research applications:

- **Robotics**: Evolved LTC controllers for locomotion with natural rhythm emergence
- **Time-series**: Financial prediction, sensor anomaly detection, predictive maintenance
- **Game AI**: Agents that adapt response timing to opponent behavior
- **Edge Computing**: Lightweight temporal models for embedded systems

### Publishing Opportunities

If you use faber-tweann LTC neurons in research, consider contributing:

- Evolved network architectures that solve interesting problems
- Performance comparisons with other temporal network approaches
- Novel morphologies designed for specific temporal tasks

## Next Steps

- See the [LTC Usage Guide](ltc-usage-guide.md) for practical examples
- Explore [Custom Morphologies](CUSTOM_MORPHOLOGIES.md) to create LTC-based morphologies
- Check the `ltc_dynamics` module documentation for detailed function reference
