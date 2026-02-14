# TWEANN Basics

## What is TWEANN?

**TWEANN** stands for **Topology and Weight Evolving Artificial Neural Networks**. Unlike traditional neural networks where the structure is fixed and only the weights are trained, TWEANNs evolve both:

1. **Topology** - The structure: number of neurons, layers, and connections
2. **Weights** - The synaptic connection strengths

## Why Use TWEANN?

Traditional neural networks require you to:
- Decide the architecture upfront (how many layers? how many neurons?)
- Risk underfitting (too simple) or overfitting (too complex)
- Manually experiment with different architectures

TWEANN solves this by:
- Starting with minimal networks (just inputs and outputs)
- Automatically discovering the right complexity through evolution
- Adding neurons and connections only when beneficial
- Pruning unnecessary structure naturally through selection

## How Does It Work?

### 1. Start Simple

A TWEANN agent begins as the simplest possible network:
- Input sensors (read environment)
- Output actuators (perform actions)
- Direct connections between them
- No hidden neurons initially

### 2. Evolve Through Mutation

During evolution, networks undergo random mutations:
- **Add neuron** - Insert a new neuron on a connection
- **Add connection** - Link existing neurons
- **Remove connection** - Prune unused links
- **Modify weights** - Adjust connection strengths
- **Change activation** - Switch neuron activation functions

### 3. Select the Best

After evaluation:
- Agents compete based on fitness (how well they perform)
- Best performers survive and reproduce
- Poor performers are eliminated
- Successful mutations spread through the population

### 4. Repeat

Over many generations:
- Networks grow more complex as needed
- Effective structures emerge naturally
- Population converges on solutions
- Diversity is maintained through speciation

## Key Concepts

### Genotype vs Phenotype

- **Genotype**: The blueprint - stored in Mnesia database as records
  - Defines structure: neurons, connections, sensors, actuators
  - Persistent and can be saved/loaded
  - Modified by mutation operators

- **Phenotype**: The running network - actual Erlang processes
  - Built from genotype by constructor
  - Sensors, neurons, actuators run as separate processes
  - Communicates via message passing
  - Temporary - destroyed after evaluation

### Morphology

A morphology defines the problem domain:
- What sensors are available (inputs)
- What actuators can do (outputs)
- How fitness is computed
- Example morphologies: XOR logic, pole balancing, trading

### Fitness

Fitness measures how well an agent performs:
- Single objective: one score (e.g., accuracy)
- Multi-objective: multiple scores (e.g., accuracy vs complexity)
- Pareto dominance for multi-objective selection
- Computed during phenotype evaluation

### Speciation

Networks are grouped into species based on behavioral similarity:
- Protects innovation (new structures need time to optimize weights)
- Maintains diversity (prevents premature convergence)
- Allows exploration of different solution strategies
- Species compete fairly within their niche

## Process Architecture

Faber TWEANN uses Erlang's process model:

```
Population Monitor (gen_server)
  |
  +-- Exoself (agent coordinator)
        |
        +-- Cortex (network controller)
              |
              +-- Sensors (read inputs)
              +-- Neurons (process signals)
              +-- Actuators (generate outputs)
```

Key features:
- Each component is a separate Erlang process
- Processes communicate via message passing
- Crashes propagate (fail-fast philosophy)
- Timeouts prevent infinite hangs
- Supervision ensures clean restarts

## Example: Evolving XOR

The XOR problem is a classic benchmark - a non-linearly separable function.

### Step 1: Define Constraint

```erlang
Constraint = #constraint{
    morphology = xor_mimic,
    connection_architecture = recurrent
}.
```

### Step 2: Create Initial Agent

```erlang
{ok, AgentId} = genotype:construct_agent(Constraint).
```

This creates a minimal network stored in Mnesia.

### Step 3: Build and Test

```erlang
Phenotype = constructor:construct_phenotype(AgentId),
cortex:sync(Phenotype#phenotype.cortex_pid).
```

The initial network performs poorly (random weights).

### Step 4: Evolve

```erlang
genome_mutator:mutate(AgentId).
```

Random mutations modify the genotype (add neuron, change weight, etc.).

### Step 5: Repeat

```erlang
NewPhenotype = constructor:construct_phenotype(AgentId),
cortex:sync(NewPhenotype#phenotype.cortex_pid).
```

Test the mutated network. If fitness improves, keep it. Otherwise, revert.

### Population Evolution

For automatic evolution:

```erlang
PMP = #pmp{
    population_id = xor_population,
    init_specie_size = 20,
    generation_limit = 100,
    fitness_goal = 0.9
}.

population_monitor:start(PMP).
```

The population monitor handles the full evolutionary cycle automatically.

## When to Use TWEANN

**Good fit:**
- Unknown optimal network architecture
- Complex, non-linear problems
- Multi-objective optimization needed
- Interpretable solutions desired (can analyze evolved topology)
- Erlang/OTP environment

**Consider alternatives:**
- Large-scale datasets (gradient descent is faster)
- Image/text processing (CNNs/Transformers are proven)
- Real-time learning needed (TWEANN is offline evolution)
- Non-OTP environment

## Next Steps

- [Quick Start](quickstart.md) - Hands-on code examples
- [Architecture](architecture.md) - Deep dive into system design
- See module documentation for detailed API reference

## Further Reading

- Stanley, K.O., Miikkulainen, R. *"Evolving Neural Networks Through Augmenting Topologies"* (2002)
- Sher, G.I. *"Handbook of Neuroevolution Through Erlang"* (2013)
