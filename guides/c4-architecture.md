# C4 Architecture Model

This document presents the Faber TWEANN architecture using the C4 model (Context, Container, Component, Code), providing multiple levels of abstraction from high-level system context down to detailed component interactions.

## Level 1: System Context

The system context diagram shows how Faber TWEANN fits into the broader environment.

![C4 Context Diagram](assets/c4-context.svg)

### Key Elements

**Users:**
- **Developer** - Creates evolutionary neural network applications, constructs agents, and runs evolution experiments

**System:**
- **Faber TWEANN** - Erlang/OTP library that evolves neural network topology and weights through natural selection

**External Systems:**
- **Mnesia** - Distributed database that stores genotypes (network blueprints) persistently
- **Environment** - Problem domain that provides fitness feedback (XOR, cart-pole, trading, games, etc.)

### Interactions

1. Developer constructs agents and runs evolution using the TWEANN API
2. TWEANN reads/writes genotypes to Mnesia for persistence
3. TWEANN evaluates networks against the environment to compute fitness
4. Environment provides feedback that drives selection and evolution

---

## Level 2: Container

The container diagram shows the high-level architecture of Faber TWEANN, broken down into major containers (applications, databases, services).

![C4 Container Diagram](assets/c4-container.svg)

### Containers

**Core Data:**
- **Genotype Store** - Mnesia database storing neural network blueprints as Erlang records
- **Morphology System** - Problem-specific configurations defining sensors, actuators, and fitness functions

**Evolution:**
- **Genome Mutator** - Applies topology and weight mutations (add/remove neurons, modify connections)
- **Population Monitor** - gen_server managing evolutionary lifecycle (selection, survival, reproduction)

**Construction:**
- **Constructor** - Builds running process networks (phenotypes) from stored genotypes

**Execution:**
- **Network Runtime** - OTP processes implementing cortex, sensors, neurons, and actuators as linked processes

**External:**
- **Environment** - External system providing observations and receiving actions

### Key Flows

1. Developer creates agents via Genotype Store
2. Genome Mutator modifies genotypes with random mutations
3. Constructor spawns Network Runtime processes from genotypes
4. Network Runtime evaluates against Environment
5. Population Monitor collects fitness and triggers next generation

---

## Level 3: Component

The component diagram zooms into the Evolution System container, showing the internal modules and their responsibilities.

![C4 Component Diagram](assets/c4-component.svg)

### Component Groups

**Core Data Layer:**
- `genotype` - CRUD operations for neural network blueprints
- `morphology` - Problem domain definitions
- `records.hrl` - Type definitions (agent, neuron, sensor, actuator)

**Construction Layer:**
- `constructor` - Phenotype builder
- `exoself` - Agent coordinator and evaluation manager

**Network Components:**
- `cortex` - Network controller (gen_server)
- `sensor` - Input reading process
- `neuron` - Signal processing process
- `actuator` - Output generation process

**Evolution Operators:**
- `genome_mutator` - Topology mutations
- `crossover` - Sexual reproduction
- `perturbation_utils` - Weight mutations

**Population Management:**
- `population_monitor` - Evolution loop (gen_server)
- `selection_algorithm` - Survivor selection
- `species_identifier` - Speciation for diversity
- `fitness_postprocessor` - Multi-objective optimization

**Utilities:**
- `functions` - Activation functions (tanh, sigmoid, ReLU, etc.)
- `signal_aggregator` - Dot product, signal combination
- `selection_utils` - Roulette wheel, weighted selection
- `tweann_logger` - Structured logging

### Component Interactions

1. `constructor` reads from `genotype` and spawns network processes
2. Network processes (`cortex`, `sensor`, `neuron`, `actuator`) communicate via message passing
3. `exoself` coordinates evaluation and reports fitness to `population_monitor`
4. `population_monitor` triggers `genome_mutator` and `selection_algorithm`
5. `genome_mutator` updates genotypes in Mnesia
6. Utility modules (`functions`, `signal_aggregator`) support neuron computation

---

## Level 4: Code (Evaluation Cycle)

For the code level, we show a detailed sequence diagram of the network evaluation cycle.

![Evaluation Cycle Sequence](assets/evaluation-cycle-sequence.svg)

### Detailed Flow

1. **Sync Trigger**: Cortex sends `{sync, self()}` to all sensors
2. **Sense**: Sensors read environment and forward inputs to neurons
3. **Think**: Neurons aggregate inputs, apply weights, activate, and forward to next layer
4. **Act**: Actuators collect outputs and interact with environment (scape)
5. **Fitness**: Scape computes fitness and reports back to cortex
6. **Complete**: Cortex reports evaluation complete to exoself

This cycle repeats for multiple episodes until fitness converges or generation limit is reached.

---

## Architecture Principles

### Separation of Concerns

Each layer has a clear responsibility:
- **Data Layer** - Persistence and definitions
- **Construction** - Genotype-to-phenotype translation
- **Network** - Runtime execution and computation
- **Evolution** - Genetic operators and selection
- **Population** - Multi-agent coordination

### Process-Based Concurrency

Neural network components run as separate Erlang processes:
- Natural parallelism (neurons compute independently)
- Fault isolation (crashes propagate via `spawn_link`)
- Message-passing communication (no shared state)

### Evolutionary Flexibility

Multiple extension points:
- New morphologies for different problems
- Pluggable selection algorithms
- Custom activation functions
- Multi-objective fitness functions

### Safety and Robustness

Built-in protection mechanisms:
- Timeouts prevent infinite hangs (cortex: 30s, neuron: 10s)
- Crash propagation ensures clean failure
- Logging for debugging and analysis

---

## Technology Stack

- **Language**: Erlang/OTP 24+
- **Database**: Mnesia (distributed, ACID)
- **Concurrency**: OTP processes, gen_server behaviors
- **Persistence**: Erlang records (not maps)
- **Logging**: OTP logger with structured output

---

## Next Steps

- [Architecture Overview](architecture.md) - Detailed layer descriptions
- [Quick Start](quickstart.md) - Hands-on code examples
- [TWEANN Basics](tweann-basics.md) - Conceptual introduction
