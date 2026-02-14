# Architecture

## Overview

Faber TWEANN uses a layered architecture with clear separation of concerns:

![Module Dependencies](assets/module-dependencies.svg)

## Layers

### Core Data Layer

**Modules**: `genotype`, `morphology`

Handles persistent storage of neural network blueprints (genotypes) using Mnesia. Morphologies define problem-specific sensor and actuator configurations.

### Network Components Layer

**Modules**: `cortex`, `sensor`, `neuron`, `actuator`

Process-based implementation of neural network elements. Each component runs as a separate Erlang process with message-passing communication.

### Construction Layer

**Modules**: `constructor`, `exoself`

Constructs running phenotypes (process networks) from stored genotypes. The exoself acts as a coordinator for each agent's evaluation.

### Evolution Layer

**Modules**: `genome_mutator`, `crossover`, `selection_algorithm`

Implements genetic operators for topology and weight evolution. Supports both asexual (mutation) and sexual (crossover) reproduction.

### Population Layer

**Modules**: `population_monitor`, `species_identifier`, `fitness_postprocessor`

Manages multi-agent evolution with speciation for diversity preservation and multi-objective fitness evaluation.

### Utilities Layer

**Modules**: `tweann_logger`, `functions`, `signal_aggregator`, `perturbation_utils`, `selection_utils`

Helper functions for logging, activation functions, signal processing, weight perturbation, and selection algorithms.

## Process Hierarchy

![Supervision Tree](assets/supervision-tree.svg)

The process tree uses `spawn_link` for crash propagation:

- `population_monitor` (gen_server) manages multiple agents
- Each agent has an `exoself` coordinating its network
- Network processes (cortex, sensors, neurons, actuators) are linked
- Crashes propagate up, terminating the entire network evaluation

## Evaluation Flow

![Evaluation Cycle](assets/evaluation-cycle-sequence.svg)

The sense-think-act cycle:

1. **Sense**: Cortex triggers sensors, which read from environment
2. **Think**: Neurons receive signals, aggregate, and activate
3. **Act**: Actuators collect outputs and interact with environment
4. Fitness is computed and reported back to exoself

## Safety Features

### Timeouts

- **Cortex**: 30s sync timeout (configurable)
- **Neuron**: 10s input timeout (configurable)

Prevents infinite hangs if network components fail to respond.

### Crash Handling

All network processes use `spawn_link`:
- Component crashes terminate the entire network
- Exoself reports failure to population monitor
- Clean shutdown with proper resource cleanup

## Data Flow

```
Genotype (Mnesia)
    ↓ construct
Phenotype (Processes)
    ↓ evaluate
Fitness
    ↓ selection
Survivors
    ↓ mutation/crossover
New Genotypes
```

## Why This Architecture

### Process-Based Benefits

The process-per-neuron design offers unique advantages:

- **True Parallelism**: On multi-core systems, neurons evaluate concurrently
- **Fault Isolation**: A crashed neuron doesn't corrupt other components
- **Hot Code Loading**: Update logic without stopping evolution
- **Debuggability**: Inspect any component with standard Erlang tools
- **Distributed Potential**: Network components can span multiple nodes

### Comparison to Traditional Implementations

| Feature | Matrix-Based (NumPy/PyTorch) | Process-Based (faber-tweann) |
|---------|------------------------------|-------------------------------|
| Speed | Faster for batch | Better for real-time |
| Debugging | Tensor shapes | Individual processes |
| Fault tolerance | Full crash | Graceful degradation |
| Topology changes | Expensive reshape | Natural add/remove |
| Distribution | GPU only | CPU clusters |

### When to Choose faber-tweann

**Ideal for:**
- Embedded systems needing fault tolerance
- Real-time applications (games, robotics)
- Research on topology evolution
- Distributed AI systems
- When debuggability matters

**Consider alternatives when:**
- Batch training massive datasets (use PyTorch/TensorFlow)
- Maximum throughput on single machine (use matrix libraries)
- GPU acceleration is critical

## Next Steps

- See module documentation for detailed API reference
- Check [Quick Start](quickstart.md) for usage examples
