# Faber TWEANN

**Topology and Weight Evolving Artificial Neural Networks for Erlang**

## Introduction

Faber TWEANN is an evolutionary neural network library that implements the TWEANN paradigm - allowing neural networks to evolve both their topology (structure) and weights. Networks can add neurons, connections, sensors, and actuators while optimizing their synaptic weights through natural selection.

Based on DXNN2 by Gene Sher (from "Handbook of Neuroevolution Through Erlang"), this library provides a production-ready implementation with modern Erlang practices, process safety, and comprehensive logging.

## Why Choose faber-tweann

### Comparison with Other Neuroevolution Libraries

| Feature | faber-tweann | NEAT-Python | TensorNEAT | DXNN2 (original) |
|---------|---------------|-------------|------------|------------------|
| Language | Erlang | Python | Python/JAX | Erlang |
| LTC Neurons | Yes | No | No | No |
| Process-based | Yes | No | No | Yes |
| Fault tolerance | Yes | No | No | Limited |
| Hot code loading | Yes | No | No | Yes |
| ONNX export | Yes | No | Limited | No |
| Real-time inference | Yes | Limited | Limited | Yes |
| Mnesia persistence | Yes | File-based | None | Yes |
| Brain API | Yes | No | No | No |

### Unique Strengths

**LTC/CfC Neurons**: First TWEANN library to implement Liquid Time-Constant neurons, enabling evolved networks that adapt their temporal dynamics - critical for time-series and control problems.

**Production-Ready**: Comprehensive test suite (858 tests), structured logging, process timeouts, and crash handling make this suitable for real applications.

**Brain API**: Simple GenServer interface for real-time inference with optional Hebbian learning during operation - ideal for games, robotics, and interactive systems.

**ONNX Export**: Deploy evolved networks to Python, JavaScript, C++, mobile, and edge devices using standard tooling.

**BEAM Ecosystem**: Leverage Erlang's distributed capabilities for multi-node evolution, integrate with Phoenix for web interfaces, or run on Nerves for embedded systems.

### When to Use Alternatives

- **TensorNEAT/JAX**: For GPU-accelerated batch evolution with massive populations
- **NEAT-Python**: For quick prototyping in Python ecosystem
- **PyTorch/TensorFlow**: For deep learning with gradient descent

## Architecture

![Module Dependencies](assets/module-dependencies.svg)

The library follows a layered architecture:
- **Core**: Genotype storage and morphology definitions
- **Network**: Process-based neural network components (cortex, sensors, neurons, actuators)
- **Evolution**: Mutation operators, crossover, and selection algorithms
- **Population**: Multi-agent evolution with speciation

See the [Architecture Guide](architecture.md) for details.

## Documentation

### Getting Started
- [TWEANN Basics](tweann-basics.md) - What is TWEANN and how does it work?
- [Installation](installation.md) - Add to your project
- [Quick Start](quickstart.md) - Basic usage examples

### Architecture
- [C4 Architecture Model](c4-architecture.md) - Multi-level architectural views (Context, Container, Component, Code)
- [Architecture Details](architecture.md) - Layer-by-layer system design

### LTC Neurons (Advanced)
- [LTC Neurons](neuron-types.md) - Theory and concepts of Liquid Time-Constant neurons
- [LTC Usage Guide](ltc-usage-guide.md) - Practical API usage and examples
- [LTC Evolution](ltc-evolution.md) - Evolving multi-timescale networks

### Customization
- [Custom Morphologies](CUSTOM_MORPHOLOGIES.md) - Create your own problem domains

### Core Concepts
See the module documentation for detailed API reference on:
- **Genotypes** - Neural network blueprints (genotype module)
- **Phenotypes** - Running network processes (constructor, exoself modules)
- **Evolution** - Mutation and selection (genome_mutator, selection_algorithm modules)
- **Morphologies** - Problem domains (morphology module)
- **Speciation** - Diversity preservation (species_identifier module)
- **Multi-objective** - Pareto optimization (fitness_postprocessor module)
- **Process Safety** - Timeouts and crash handling (cortex, neuron modules)

### API Reference
- See the module documentation in the sidebar for complete API reference

## Acknowledgements

Based on DXNN2 by Gene Sher. Adapted and extended by [R.G. Lefever](https://rgfaber).

## Academic References

### Core TWEANN/NEAT Papers

- **Sher, G.I.** (2013). *Handbook of Neuroevolution Through Erlang*. Springer.
  Primary reference for DXNN2 architecture and Erlang-specific patterns.

- **Stanley, K.O. & Miikkulainen, R.** (2002). Evolving Neural Networks through Augmenting Topologies. *Evolutionary Computation*, 10(2), 99-127.
  Foundational NEAT paper introducing speciation and structural innovation protection.

### LTC/CfC Neurons

- **Hasani, R., Lechner, M., et al.** (2021). Liquid Time-constant Networks. *AAAI Conference on Artificial Intelligence*, 35(9), 7657-7666.
  Introduces adaptive time-constant neurons with continuous-time dynamics.

- **Hasani, R., Lechner, M., et al.** (2022). Closed-form Continuous-time Neural Networks. *Nature Machine Intelligence*, 4, 992-1003.
  CfC approximation enabling ~100x speedup over ODE-based evaluation.

### Foundational Work

- **Holland, J.H.** (1975). *Adaptation in Natural and Artificial Systems*. MIT Press.
  Foundational text on genetic algorithms.

- **Yao, X.** (1999). Evolving Artificial Neural Networks. *Proceedings of the IEEE*, 87(9), 1423-1447.
  Comprehensive neuroevolution survey.

## Related Projects

### Macula Ecosystem

- **macula** - HTTP/3 mesh networking for distributed neuroevolution
- **faber_neuroevolution** - Population-based evolutionary training engine

### External

- **DXNN2** - Gene Sher's original Erlang implementation
- **NEAT-Python** - Python NEAT implementation
- **LTC Reference** - MIT/ISTA reference LTC implementation

## License

Apache License 2.0
