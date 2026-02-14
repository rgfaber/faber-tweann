# LTC Evolution Guide

This guide explains how evolution discovers optimal LTC parameters, enabling networks to develop **multi-timescale processing** automatically.

## Why Evolve LTC Parameters?

Without LTC evolution, all LTC neurons use the same fixed parameters (tau=1.0, A=1.0). This limits the network to a single response timescale.

With LTC evolution, each neuron can develop its own temporal dynamics:

| Evolved tau | Behavior | Use Case |
|-------------|----------|----------|
| 0.01 - 0.1 | Ultra-fast reflexes | Collision avoidance, immediate threats |
| 0.1 - 1.0 | Quick response | Recent events, short-term tracking |
| 1.0 - 10.0 | Balanced | General processing, pattern recognition |
| 10.0 - 100.0 | Slow integration | Trends, long-term context, planning |

## Mutation Operators

Four LTC-specific mutation operators are included in the default mutation set:

### mutate_neuron_type (probability: 5)

Switches a random neuron between types:

```
standard <-> ltc <-> cfc
```

This allows evolution to discover which neurons benefit from temporal dynamics. Some neurons may stay standard (pure pattern matching), while others evolve to LTC/CfC (temporal processing).

```erlang
%% Apply manually (usually done automatically during evolution)
ltc_mutations:mutate_neuron_type(AgentId).
```

### mutate_time_constant (probability: 20)

Perturbs the time constant (tau) of a random LTC/CfC neuron:

- **Bounds**: 0.001 to 100.0
- **Method**: Multiplicative perturbation (keeps tau positive)

```erlang
%% Tau controls response speed
%% Lower tau = faster response, less memory
%% Higher tau = slower response, more memory
ltc_mutations:mutate_time_constant(AgentId).
```

### mutate_state_bound (probability: 10)

Perturbs the state bound (A) of a random LTC/CfC neuron:

- **Bounds**: 0.1 to 10.0
- **Method**: Multiplicative perturbation (keeps A positive)

```erlang
%% State bound controls output range
%% Internal state is clamped to [-A, A]
ltc_mutations:mutate_state_bound(AgentId).
```

### mutate_ltc_weights (probability: 30)

Perturbs the backbone and head network weights:

```erlang
%% Backbone weights control the f() function (time constant modulation)
%% Head weights control the h() function (target state)
ltc_mutations:mutate_ltc_weights(AgentId).
```

## Phenotype Benefits

When LTC parameters evolve, networks develop specialized temporal processing:

### Multi-Timescale Processing

Different neurons evolve different tau values, enabling simultaneous processing at multiple timescales:

```
Input Signal
    |
    +---> [tau=0.1] ---> Fast layer (immediate reactions)
    |
    +---> [tau=1.0] ---> Medium layer (recent context)
    |
    +---> [tau=10.0] --> Slow layer (trends, planning)
    |
    v
 Output (combines all timescales)
```

### Emergent Memory Horizons

LTC internal state decays based on tau. Different tau values create different memory windows:

| tau | Approximate Memory | What It Remembers |
|-----|-------------------|-------------------|
| 0.1 | ~100ms | Last few frames |
| 1.0 | ~1s | Recent events |
| 10 | ~10s | Medium-term patterns |
| 100 | ~100s | Long-term context |

Evolution discovers which parts of the network need memory and how much.

### Natural Signal Filtering

Tau acts as a frequency filter:

- **Low tau** = high-pass filter (responds to fast changes, ignores slow drift)
- **High tau** = low-pass filter (smooths noise, tracks trends)

A network might evolve:
- Low-tau neurons watching for sudden movements (predator detection)
- High-tau neurons tracking overall position trends (navigation)

### Hybrid Architecture Discovery

Evolution decides which neurons should be LTC:

- Neurons that benefit from temporal processing evolve to LTC/CfC
- Neurons that do pure pattern matching stay standard

This creates efficient networks that only pay the LTC computational cost where needed.

## Domain Examples

### Snake Game

```
Vision Sensors
      |
      +---> [tau=0.05, cfc] --> Wall proximity (ultra-fast reflex)
      |
      +---> [tau=0.5, cfc]  --> Food tracking (recent positions)
      |
      +---> [tau=5.0, cfc]  --> Opponent behavior (patterns)
      |
      +---> [tau=50, cfc]   --> Territory control (strategy)
      |
      v
 Movement Actions
```

### Cart-Pole Balancing

```
Pole Angle + Velocity
      |
      +---> [tau=0.1, cfc] --> Balance correction (immediate)
      |
      +---> [tau=2.0, cfc] --> Position drift (short-term)
      |
      +---> [tau=20, cfc]  --> Energy efficiency (long-term)
      |
      v
 Force Output
```

### Trading

```
Price Stream
      |
      +---> [tau=0.01, cfc] --> Tick reaction
      |
      +---> [tau=1.0, cfc]  --> Minute patterns
      |
      +---> [tau=60, cfc]   --> Hour trends
      |
      +---> [tau=1440, cfc] --> Daily context
      |
      v
 Trade Decision
```

## Configuration

### Default Mutation Probabilities

In `records.hrl`, the default constraint includes:

```erlang
mutation_operators = [
    %% ... topological mutations ...

    %% LTC mutations - enable multi-timescale evolution
    {mutate_neuron_type, 5},       % Switch between standard/ltc/cfc
    {mutate_time_constant, 20},    % Perturb tau (response speed)
    {mutate_state_bound, 10},      % Perturb state bound A
    {mutate_ltc_weights, 30}       % Perturb backbone/head weights
]
```

### Custom Probabilities

Create a custom constraint to adjust LTC mutation rates:

```erlang
Constraint = #constraint{
    morphology = my_morphology,
    mutation_operators = [
        %% Increase LTC mutation rates for temporal tasks
        {mutate_neuron_type, 10},      % More type switching
        {mutate_time_constant, 40},    % More tau tuning
        {mutate_state_bound, 20},      % More bound tuning
        {mutate_ltc_weights, 50},      % More weight tuning

        %% Standard mutations
        {add_neuron, 20},
        {add_outlink, 20},
        {mutate_weights, 50}
    ]
}.
```

### Disable LTC Evolution

If you want fixed LTC parameters (no evolution):

```erlang
Constraint = #constraint{
    morphology = my_morphology,
    mutation_operators = [
        %% Only standard mutations
        {add_neuron, 40},
        {add_outlink, 40},
        {mutate_weights, 100}
        %% No LTC mutations
    ]
}.
```

## Best Practices

### Start with Mixed Population

Initialize some neurons as LTC and some as standard:

```erlang
%% In your morphology, create neurons with varying types
Neurons = [
    #neuron{neuron_type = standard, ...},
    #neuron{neuron_type = cfc, time_constant = 0.5, ...},
    #neuron{neuron_type = cfc, time_constant = 5.0, ...}
].
```

This gives evolution a head start on discovering useful timescales.

### Appropriate Tau Bounds

The default bounds (0.001 to 100.0) work for most applications. Adjust if needed:

```erlang
%% In ltc_mutations.erl:perturb_time_constant/2
NewTau = clamp(CurrentTau * (1.0 + Delta), 0.001, 100.0).
```

For faster-than-realtime simulation, you might want tighter bounds.

### Monitor Evolved Tau Distribution

Track the distribution of tau values in your population:

```erlang
get_tau_distribution(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    TauValues = [begin
        N = genotype:dirty_read({neuron, NId}),
        case N#neuron.neuron_type of
            standard -> undefined;
            _ -> N#neuron.time_constant
        end
    end || NId <- Cortex#cortex.neuron_ids],
    [T || T <- TauValues, T /= undefined].
```

If all neurons converge to similar tau values, the task may not need multi-timescale processing.

### Reset State Between Episodes

For episodic tasks, reset LTC state at episode boundaries:

```erlang
%% Reset all LTC neurons in network
reset_ltc_states(CortexPid) ->
    cortex:reset_ltc_states(CortexPid).
```

This prevents state leakage between episodes.

## Biological Analogy

LTC evolution mimics how biological neural circuits evolved different time constants:

| Brain Region | Typical Timescale | Function |
|--------------|------------------|----------|
| Brainstem reflexes | milliseconds | Immediate survival responses |
| Motor cortex | 100s of ms | Movement coordination |
| Prefrontal cortex | seconds to minutes | Planning, working memory |
| Hippocampus | hours to days | Memory consolidation |

Evolution discovered that different computations need different timescales. LTC evolution lets artificial networks discover the same principle.

## Academic References

LTC evolution is inspired by research on evolved plasticity and temporal processing:

1. **Hasani et al. (2021)** - Liquid Time-constant Networks
2. **Soltoggio et al. (2008)** - Evolutionary advantages of neuromodulated plasticity
3. **Beer (1995)** - Dynamics of continuous-time recurrent neural networks
4. **Clune et al. (2013)** - Evolutionary origins of modularity

## Next Steps

- See [LTC Neurons](neuron-types.md) for the mathematical foundations
- See [LTC Usage Guide](ltc-usage-guide.md) for practical API usage
- See [Custom Morphologies](CUSTOM_MORPHOLOGIES.md) to create LTC-based morphologies
