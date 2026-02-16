# Changelog

All notable changes to faber-tweann will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-02-16

### Stable Release

First stable release. All APIs are considered stable.

### Features (stable since 0.x series)

- **TWEANN Evolution**: Topology and weight evolution with NEAT-style speciation
- **LTC Neurons**: Liquid Time-Constant neurons with adaptive temporal processing
- **CfC Mode**: Closed-form continuous-time evaluation (~100x faster than ODE)
- **Hybrid Networks**: Mix standard and LTC neurons in the same network
- **Morphology Registry**: Extensible sensor/actuator specifications per problem domain
- **Plasticity**: Hebbian, modulated Hebbian, STDP, Oja learning rules
- **NIF Acceleration**: Optional Rust NIF support via `faber_nn_nifs` (10-200x speedup)
- **Process Safety**: Timeouts, crash handling, comprehensive logging
- **863 tests passing**

### Changed

- Bumped version to 1.0.0 for stable release

---

## [0.1.0] - 2026-02-14

### Changed
- Renamed from `macula-tweann` to `faber-tweann` under `rgfaber` organization
- Renamed NIF dependency from `macula_nn_nifs` to `faber_nn_nifs`
- Updated all module prefixes from `macula_tweann` to `faber_tweann`
- Updated copyright to `R.G. Lefever`
- Reset version to 0.1.0 for fresh start under new name

---

*History from macula-tweann below*

## [0.18.0] - 2025-12-26

### Added
- **neuron_info module** - Neuron introspection API for extracting information from neurons:
  - `get_neuron_info/1` - Get comprehensive info from record or running process
  - `get_neuron_type/1` - Identify neuron type (standard, ltc, cfc)
  - `get_capabilities/1` - List neuron capabilities
  - `is_temporal/1` - Check if neuron has temporal dynamics
  - `describe/1` - Human-readable neuron description

## [0.17.0] - 2025-12-26

### Added
- CHANGELOG.md with comprehensive version history
- Documentation for 26 new NIFs in enterprise-nifs.md:
  - SIMD batch activations (tanh, sigmoid, relu, softmax)
  - Layer-specific mutation
  - Plasticity rules (Hebbian, modulated Hebbian, STDP, Oja)
  - Extended CfC/LTC functions
  - Population analysis
  - NEAT crossover operations
  - K-means speciation
  - Matrix operations
  - Gradient-free meta-learning
  - Network compression (pruning, quantization)

### Changed
- Updated faber-nn-nifs reference to v0.2.0
- Updated test count to 858 tests
- Updated version references to 0.17.0

## [0.16.1] - 2025-12-26

### Changed
- Minor documentation regeneration

## [0.16.0] - 2024-12-12

### Added
- Enterprise NIF package support via optional `faber_nn_nifs` dependency
- Automatic detection and fallback for NIF acceleration
- Neuron type selection guide in `guides/neuron-types.md`

### Changed
- Refactored NIF code out of Community Edition (now in separate enterprise package)
- `tweann_nif.erl` automatically detects `faber_nn_nifs` when available

### Fixed
- Memory leak: prevent NIF ResourceArc accumulation during evolution

## [0.15.3] - 2024-12-07

### Fixed
- Critical memory fix: prevent NIF ResourceArc accumulation during long evolution runs

## [0.15.2] - 2024-12-06

### Added
- Comprehensive diagram index in `guides/diagram-index.md`
- Documentation enhancements across all guides

## [0.15.1] - 2024-12-05

### Fixed
- Minor documentation fixes
- Code quality improvements

## [0.15.0] - 2024-12-04

### Added
- NIF acceleration Phase 2 with significant performance improvements
- NIF fallback module for graceful degradation without Rust
- Complete NIF implementations for all compute-intensive operations:
  - Network evaluation (10-15x faster)
  - Batch mutation operations
  - KNN novelty search
  - Fitness statistics
  - Selection algorithms

### Changed
- Improved NIF loading and error handling

## [0.14.0] - 2024-12-03

### Added
- Archive for legacy release documentation
- Improved version documentation

## [0.13.0] - 2024-11-30

### Added
- Memory optimization across all modules
- NIF acceleration Phase 1
- Comprehensive memory profiling

### Changed
- Reduced memory footprint for large populations
- Optimized weight storage format

## [0.12.0] - 2024-11-28

### Added
- Performance benchmarking suite
- Memory usage tracking

## [0.11.0] - 2024-11-25

### Added
- Plasticity system with multiple learning rules:
  - Hebbian plasticity
  - Modulated Hebbian
  - Oja's rule
  - STDP (Spike-Timing Dependent Plasticity)
- Brain runtime components for distributed execution
- LTC (Liquid Time-Constant) neurons with ODE dynamics
- CfC (Closed-form Continuous-time) neurons

### Changed
- Major code quality improvements
- Refactored neuron evaluation pipeline

## [0.10.0] - 2024-11-20

### Added
- Custom morphologies guide
- Morphology extraction from library to user-defined modules

### Changed
- Morphology implementations now user-extensible

## [0.9.0] - 2024-11-15

### Added
- Library refactoring for cleaner API
- Release planning documentation
- Real-time learning investigation

## [0.8.8] - 2024-11-12

### Changed
- Minor version bump for dependency updates

## [0.8.7] - 2024-11-10

### Added
- Distributed mega-brain vision documentation
- SVG diagrams for vision documents
- Usability and consequences analysis

## [0.8.6] - 2024-11-08

### Added
- Anti-drone defense addendum with comprehensive diagrams
- Military and civil resilience documentation
- AI arms race evolution analysis

## [0.8.5] - 2024-11-06

### Added
- C4 architecture diagrams (Context, Container, Component)
- Architecture guide with professional SVG diagrams
- Khepri vs Mnesia investigation document

## [0.8.4] - 2024-11-04

### Fixed
- SVG diagram paths for hexdocs rendering

## [0.8.3] - 2024-11-03

### Fixed
- Hexdocs documentation structure
- SVG diagram paths

## [0.8.2] - 2024-11-02

### Added
- TWEANN basics guide
- Documentation improvements

## [0.8.1] - 2024-11-01

### Added
- Architecture documentation
- Nx integration investigation for v0.9.0 roadmap

## [0.8.0] - 2024-10-30

### Added
- Process safety improvements
- Hardened core architecture

### Fixed
- EDoc XML parsing errors for hex.pm publication

## [0.7.0] - 2024-10-25

### Added
- Hardened core with improved stability
- Initial hex.pm publication support

## [0.6.0] - 2024-10-20

### Added
- Initial public release
- TWEANN (Topology and Weight Evolving Artificial Neural Networks)
- Multiple neuron types: Traditional, LTC, CfC
- Activation functions: tanh, sigmoid, ReLU, sin, gaussian, abs, linear
- Plasticity support
- Morphology registry
- Network evaluation pipeline

---

## Version History Summary

| Version | Date | Highlights |
|---------|------|------------|
| 1.0.0 | 2026-02-16 | Stable release, all APIs stable |
| 0.17.0 | 2025-12-26 | 26 new NIF docs, v0.2.0 NIF support |
| 0.16.1 | 2025-12-26 | Documentation regeneration |
| 0.16.0 | 2024-12-12 | Enterprise NIF package, memory fix |
| 0.15.x | 2024-12-04 | NIF acceleration Phase 2 |
| 0.14.0 | 2024-12-03 | Documentation archive |
| 0.13.0 | 2024-11-30 | Memory optimization, NIF Phase 1 |
| 0.11.0 | 2024-11-25 | Plasticity, LTC/CfC neurons |
| 0.8.x | 2024-10-30 | Process safety, hex.pm |
| 0.7.0 | 2024-10-25 | Hardened core |
| 0.6.0 | 2024-10-20 | Initial release |
