# Diagram Index

Visual guides to understanding TWEANN concepts and architecture.

## Core Concepts

### TWEANN Structure

The fundamental architecture of a Topology and Weight Evolving Artificial Neural Network.

![TWEANN Structure](assets/tweann-structure.svg)

**Key elements:**
- **Sensors** (green): Input layer receiving environmental signals
- **Hidden neurons** (blue): Processing nodes with evolving topology
- **Actuators** (orange): Output layer producing actions
- **Connections**: Weighted links that can be added/removed through evolution
- **Cortex**: Coordinator process managing sync cycles

---

### Neuroevolution Cycle

The iterative process of population-based optimization.

![Neuroevolution Cycle](assets/neuroevolution-cycle.svg)

**The four phases:**
1. **Population**: Collection of neural network genotypes
2. **Evaluation**: Run each phenotype, calculate fitness scores
3. **Selection**: Tournament or truncation selection of survivors
4. **Reproduction**: Mutation, crossover, and elitism to create next generation

---

### NEAT Evolution

NeuroEvolution of Augmenting Topologies - how structure evolves.

![NEAT Evolution](assets/neat-evolution.svg)

**Key innovations:**
- **add_node**: Split a connection to insert a new neuron
- **add_link**: Create a new connection between existing nodes
- **Innovation numbers**: Historical markings for meaningful crossover
- **Speciation**: Group similar topologies to protect innovation

---

## Architecture

### Genotype to Phenotype

Transformation from genetic encoding to living neural network.

![Genotype to Phenotype](assets/genotype-phenotype.svg)

**The Constructor pattern:**
- **Genotype** (left): Records stored in ETS (agent, cortex, sensor, actuator, neuron)
- **Phenotype** (right): Concurrent Erlang processes communicating via messages
- Each neuron becomes a `gen_server` with its own state and plasticity

---

### Supervision Tree

OTP supervision hierarchy for fault tolerance.

![Supervision Tree](assets/supervision-tree.svg)

---

### Module Dependencies

How the library modules relate to each other.

![Module Dependencies](assets/module-dependencies.svg)

---

### C4 Architecture Model

Software architecture using the C4 model.

#### Context Diagram
![C4 Context](assets/c4-context.svg)

#### Container Diagram
![C4 Container](assets/c4-container.svg)

#### Component Diagram
![C4 Component](assets/c4-component.svg)

---

## LTC Neurons

### LTC Neuron Architecture

Liquid Time-Constant neurons with temporal dynamics.

![LTC Neuron Architecture](assets/ltc-neuron-architecture.svg)

**Components:**
- Time constant (tau) controls adaptation speed
- Closed-form approximation (CfC) for efficient computation
- Temporal memory through leaky integration

---

### LTC vs Standard Neurons

Comparison between LTC and traditional neurons.

![LTC vs Standard Neurons](assets/ltc-vs-standard-neurons.svg)

---

## Learning Mechanisms

### Neural Plasticity

Online weight learning during the network's lifetime.

![Neural Plasticity](assets/neural-plasticity.svg)

**Plasticity rules:**
- **Hebbian**: "Cells that fire together, wire together"
- **Oja's Rule**: Self-normalizing Hebbian (performs online PCA)
- **Self-Modulation**: Neuron controls its own learning parameters
- **Neuromodulation**: External reward signal gates learning

---

### Activation Functions

The transfer functions available for neurons.

![Activation Functions](assets/activation-functions.svg)

**Function families:**
- **Bounded**: sigmoid, tanh (classification, gates)
- **Unbounded**: relu, softplus (deep networks, sparse activation)
- **Periodic**: sin, cos (rhythmic patterns, CPGs)
- **Localized**: gaussian (RBF networks, pattern recognition)
- **Binary**: step, sgn (threshold logic)

---

### Mutation Sequence

Step-by-step mutation process.

![Mutation Sequence](assets/mutation-sequence.svg)

---

### Evaluation Cycle Sequence

Detailed evaluation workflow.

![Evaluation Cycle Sequence](assets/evaluation-cycle-sequence.svg)

---

## Distributed Evolution

### Distributed Evolution Model

Multi-node evolution architecture.

![Distributed Evolution Model](assets/distributed-evolution-model.svg)

---

### Federated Populations Model

Island-based distributed populations.

![Federated Populations Model](assets/federated-populations-model.svg)

---

### Swarm Evolution Model

Swarm intelligence with evolved controllers.

![Swarm Evolution Model](assets/swarm-evolution-model.svg)

---

### Mega-Brain Architecture

Large-scale distributed neural architectures.

![Mega-Brain Architecture](assets/mega-brain-architecture.svg)

---

## Application Domains

### Military Swarm Coordination

Autonomous swarm systems.

![Military Swarm Coordination](assets/military-swarm-coordination.svg)

---

### Civil Infrastructure Resilience

Infrastructure protection applications.

![Civil Infrastructure Resilience](assets/civil-infrastructure-resilience.svg)

---

### Counter-Drone Detection Fusion

Multi-sensor fusion for detection.

![Counter-Drone Detection Fusion](assets/counter-drone-detection-fusion.svg)

---

### Layered Defense Zones

Multi-layer defense architecture.

![Layered Defense Zones](assets/layered-defense-zones.svg)

---

## See Also

- [TWEANN Basics](tweann-basics.md) - Introduction to neuroevolution concepts
- [Architecture Details](architecture.md) - In-depth system architecture
- [LTC Neurons](neuron-types.md) - Liquid Time-Constant neuron guide
- [Quick Start](quickstart.md) - Get started with examples
