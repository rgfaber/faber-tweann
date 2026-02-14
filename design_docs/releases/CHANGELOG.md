# Changelog

All notable changes to the faber-tweann project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Planned
- See individual version documents for detailed planning

---

## [0.16.0] - 2025-12-23

### Summary
**Enterprise NIF Package Support** - Added automatic detection and integration with the separate `faber_nn_nifs` enterprise package, providing 10-15x performance improvements for compute-intensive operations.

### Added

#### Enterprise NIF Detection
- **tweann_nif.erl**: Automatic detection of enterprise NIF package
  - Checks for `faber_nn_nifs` module at startup
  - Uses `persistent_term` for cached implementation lookup
  - Priority: faber_nn_nifs (enterprise) > bundled NIF > pure Erlang fallback
  - Zero code changes required - detection is automatic

#### Documentation
- **guides/enterprise-nifs.md**: New comprehensive guide for enterprise NIFs
  - Performance comparison table (10-15x speedups)
  - Installation instructions for Community vs Enterprise editions
  - Complete list of 44 accelerated functions across 8 categories
  - Verification and troubleshooting sections
- **guides/installation.md**: Updated with Enterprise Edition section
  - Clear separation of Community and Enterprise installation
  - Enterprise requirements (Rust 1.70+, SSH access)
  - NIF verification examples

### Changed
- **rebar.config**: Added enterprise-nifs.md to ex_doc extras

### Enterprise NIF Package
The `faber_nn_nifs` package (v0.1.0) is now available as a separate enterprise-only repository:
- 44 Rust NIF functions for compute-intensive operations
- Categories: Network Evaluation, Signal Aggregation, LTC/CfC, Novelty Search, Statistics, Selection, Meta-Controller, Evolutionary Genetics
- 98 unit tests with full coverage
- Apache-2.0 license (enterprise license required for commercial use)

### Test Results
- 593 tests passing
- Dialyzer clean

---

## [0.15.3] - 2025-12-23

### Summary
**Memory Leak Prevention** - Fixed NIF ResourceArc memory accumulation during evolution by switching to lazy compilation and adding explicit memory management functions.

### Changed

#### Network Evaluator Memory Management
- **network_evaluator.erl**: Lazy NIF compilation to prevent memory leaks
  - `create_feedforward/3,4` no longer auto-compiles for NIF
  - `set_weights/2` no longer auto-compiles for NIF
  - NIF compilation is now opt-in via `compile_for_nif/1`
  - Networks use pure Erlang evaluation by default (fallback path)
  - **Rationale**: During breeding, `set_weights/2` is called for EVERY offspring. Eager compilation created millions of ResourceArc references that accumulated unboundedly, causing memory to grow without bound across generations.

### Added
- **network_evaluator.erl**: New memory management functions
  - `strip_compiled_ref/1` - Remove compiled_ref to release NIF memory
  - `compile_for_nif/1` - Explicit opt-in for NIF compilation
  - **Usage**: Call `strip_compiled_ref/1` before storing networks in archives, events, or long-term storage to prevent ResourceArc accumulation.

### Fixed
- **population_monitor.erl**: Changed "Generation" to "Cohort" in log messages
  - Aligns with terminology used elsewhere in the codebase

### Test Results
- 593 tests passing
- Dialyzer clean

---

## [0.15.2] - 2025-12-12

### Summary
**Documentation Enhancement** - Added comprehensive visual diagram index and consolidated all SVG assets into unified `assets/` directory.

### Added
- **guides/diagram-index.md**: New visual guide indexing all 25 SVG diagrams
  - Core Concepts: TWEANN structure, neuroevolution cycle, NEAT evolution
  - Architecture: Genotype/phenotype, supervision tree, C4 model
  - LTC Neurons: Architecture and comparison diagrams
  - Learning Mechanisms: Plasticity, activation functions, mutation sequences
  - Distributed Evolution: Multi-node, federated, swarm models
  - Application Domains: Military, civil, defense scenarios

- **New educational SVG diagrams** in `assets/`:
  - `tweann-structure.svg` - Core TWEANN architecture (sensors → hidden → actuators)
  - `neuroevolution-cycle.svg` - The evolutionary optimization loop
  - `neat-evolution.svg` - NEAT topology mutations and speciation
  - `genotype-phenotype.svg` - Constructor pattern transformation
  - `neural-plasticity.svg` - Online learning mechanisms (Hebbian, Oja, etc.)
  - `activation-functions.svg` - Comparison of activation functions

### Changed
- **rebar.config**: Consolidated assets configuration
  - Single assets path: `{assets, #{"assets" => "assets"}}`
  - Added diagram-index.md to ex_doc extras
  - Removed redundant `design_docs/diagrams` from hex files

### Documentation
- All 25 SVG diagrams now in unified `assets/` directory
- Visual diagram index for easier navigation in hexdocs
- Educational descriptions for each diagram category

---

## [0.15.1] - 2025-12-12

### Summary
**Community vs Enterprise Edition** - NIF compilation hooks are now disabled by default in the hex.pm package. The Community Edition uses pure Erlang fallbacks, while Enterprise Edition users can enable Rust NIF acceleration from source.

### Changed
- **rebar.config**: NIF compilation hooks commented out for hex.pm package
  - Community Edition (hex.pm) uses pure Erlang fallbacks
  - Enterprise Edition users can uncomment hooks to enable NIF acceleration
  - No Rust toolchain required for Community Edition users
- **README.md**: Added "Community vs Enterprise Edition" section documenting the two editions

### Documentation
- Clear documentation of feature differences between editions
- Instructions for Enterprise users to enable NIF acceleration
- Contact information for enterprise licensing

---

## [0.15.0] - 2025-12-12

### Summary
**NIF Acceleration Phase 2 Release** - Major NIF expansion with 18 new functions for novelty search, fitness statistics, selection, and reward computation. Includes complete pure Erlang fallback module for portability without Rust toolchain.

### Added

#### Distance and KNN Functions (Novelty Search)
- **native/src/lib.rs**: New NIF functions for novelty search
  - `euclidean_distance/2` - Distance between two behavior vectors
  - `euclidean_distance_batch/2` - Batch distance calculation sorted by distance
  - `knn_novelty/4` - K-nearest neighbor novelty score
  - `knn_novelty_batch/3` - Batch kNN novelty for entire population

#### Statistics Functions
- **native/src/lib.rs**: Vectorized fitness statistics
  - `fitness_stats/1` - Single-pass (min, max, mean, variance, std_dev, sum)
  - `weighted_moving_average/2` - Exponential decay weighted average
  - `shannon_entropy/1` - Entropy calculation for diversity metrics
  - `histogram/4` - Histogram binning for distribution analysis

#### Selection Functions
- **native/src/lib.rs**: Selection acceleration
  - `build_cumulative_fitness/1` - Build cumulative array for roulette wheel
  - `roulette_select/3` - O(log n) binary search roulette selection
  - `roulette_select_batch/3` - Batch roulette selection
  - `tournament_select/2` - Tournament selection

#### Reward and Meta-Controller Functions
- **native/src/lib.rs**: LC v2 reward computation
  - `z_score/3` - Z-score normalization
  - `compute_reward_component/2` - Component computation with sigmoid normalization
  - `compute_weighted_reward/1` - Weighted sum of reward components

#### Weight/Genome Utilities
- **native/src/lib.rs**: Weight structure optimization
  - `flatten_weights/1` - Flatten nested weight structure avoiding intermediate lists
  - `dot_product_preflattened/3` - Dot product on pre-flattened arrays

#### Test Coverage
- **test/unit/tweann_nif_v2_tests.erl**: 42 new tests for all NIF functions
  - Distance and kNN tests
  - Statistics function tests
  - Selection function tests
  - Reward computation tests
  - Performance sanity tests

### Changed
- **src/tweann_nif.erl**: Added exports and stubs for 18 new functions
- **src/tweann_nif.erl**: Rewritten with try-catch fallback pattern for portability
- All new NIFs use `DirtyCpu` scheduler for long-running operations

#### Pure Erlang Fallback Module
- **src/tweann_nif_fallback.erl**: NEW - Complete Erlang implementations of all NIFs
  - Full fallback for all 30+ NIF functions
  - Automatic fallback when NIF not loaded (no Rust compilation required)
  - Enables library use on any Erlang/OTP system without Rust toolchain
  - Helper functions: sigmoid, clamp, apply_activation

### Fixed
- **src/genome_mutator.erl**: Substrate mutations (add_cpp, add_cep) now log warning instead of silent no-op
- **src/genotype.erl:343**: Removed misleading TODO - link_FromElementToElement is fully implemented
- **src/genotype.erl:498**: Implemented update_fingerprint using species_identifier:create_fingerprint/1
- **src/network_evaluator.erl**: Documented feedforward approximation limitation with recommendation to use tweann_nif:compile_network/3 for exact topology evaluation

### Performance Targets
- Euclidean distance batch: 30-100x speedup for novelty search
- kNN novelty: 50-200x speedup for behavior distance calculations
- Fitness statistics: 5-10x speedup for single-pass computation
- Roulette selection: 5-15x speedup with O(log n) binary search

### Test Results
- **593 tests passing** (includes 42 new NIF v2 tests)
- Dialyzer clean
- All fallback functions verified working

---

## [0.14.0] - 2025-12-12

### Summary
**Documentation Cleanup Release** - Archived legacy release docs and vision documents to streamline hexdocs.

### Changed
- Moved version-specific release docs (v0.1.0 through v1.0.0) to `archive/releases/`
- Moved vision/addendum documents to `archive/vision/` (for future macula-vision repo)
- Updated `rebar.config` to exclude `archive/` from hex package
- Removed broken hexdocs links from guides
- Streamlined documentation for cleaner hexdocs experience

### Removed
- Removed duplicate addendum files from `guides/` (now only in archive)
- Removed `FUTURE_OPTIMIZATION.md` and `RELEASE_STRATEGY.md` (superseded)
- Removed `CODE_QUALITY_REVIEW_v0.10.0.md` (superseded)

### Test Results
- 801 tests passing
- Dialyzer clean

---

## [0.13.0] - 2025-12-07

### Summary
**Memory Optimization & NIF Acceleration Release** - Major performance and stability improvements with 4x memory reduction target and 10-50x speedup on hot paths.

### Added

#### NIF Acceleration (Phase 3)
- **native/src/lib.rs**: New NIF functions
  - `dot_product_flat/3` - Flat array dot product for signal aggregation
  - `dot_product_batch/1` - Batch dot product with dirty scheduler
  - Added `schedule = "DirtyCpu"` to `evaluate_batch` to prevent blocking
- **tweann_nif.erl**: Erlang wrappers for new NIFs

#### Signal Aggregation Optimization (Phase 4)
- **signal_aggregator.erl**: NIF-accelerated aggregation
  - `dot_product_nif/2` - NIF-backed dot product with Erlang fallback
  - `flatten_for_nif/2` - Convert nested weight structure to flat arrays
- **neuron.erl**: Pre-compiled weight matrices
  - `compiled_weights` field in state record
  - `compile_weights_for_nif/4` - Pre-compile at init/link time
  - `flatten_signals/2`, `aggregate_compiled/3` - Fast path evaluation
  - Weights recompiled on `{update_weights, ...}` and `{link, input_weights, ...}`

#### Benchmark Suite (Phase 5)
- **test/benchmark/bench_common.erl**: Benchmark utilities
  - `measure_time/1,2` - Execution timing in microseconds
  - `measure_memory/1` - Memory delta measurement
  - `run_trials/3`, `run_trials_gc/3` - Multiple trial execution
  - `calc_stats/1` - Statistical analysis (min, max, avg, median, std)
  - `format_bytes/1`, `format_time/1` - Human-readable formatting
- **test/benchmark/bench_forward_pass.erl**: Network evaluation benchmarks
  - Small/medium/large/XOR network tests
  - Batch evaluation benchmarks
  - Memory usage measurements
- **test/benchmark/bench_nif_vs_erlang.erl**: NIF comparison benchmarks
  - dot_product NIF vs Erlang comparison
  - Flat dot product benchmarks
  - Batch dot product benchmarks

### Changed

#### Memory Architecture (Phase 2)
- **genotype.erl**: Replaced Mnesia with ETS
  - 11 tables migrated from Mnesia RAM to ETS
  - Faster startup, lower overhead
  - Same semantics, simpler implementation
- **innovation.erl**: Migrated to atomics/counters
  - `counters` module for innovation numbers
  - `persistent_term` for counter reference storage
  - Eliminated Mnesia dependency
- **genotype.erl**: Generation-based cleanup
  - `cleanup_old_agents/1` - Remove agents older than N generations
  - `cap_evo_hist/2` - Limit evo_hist to last 50 mutations
  - `clear_dead_pool/1` - Clear dead_pool each generation

#### Process Lifecycle Fixes (Phase 1)
- **neuron.erl**: Fixed infinite timeout loop
  - Exit after 3 consecutive timeouts (was infinite loop)
  - `MAX_TIMEOUT_COUNT = 3` constant
  - Notifies cortex on timeout termination
- **population_monitor.erl**: Reduced timeout 60s → 5s
- **cortex.erl**: Synchronous termination
  - Wait for child processes before exit
  - Monitor-based termination tracking
- **exoself.erl**: Race condition fix on shutdown
- **All process modules**: Added catch-all receive clauses
  - Prevents mailbox bloat from unexpected messages
  - Logs warnings for debugging

#### Network Evaluator (Phase 3)
- **network_evaluator.erl**: NIF integration
  - Added `compiled_ref` field to network record
  - `maybe_compile_for_nif/1` - Compile network for NIF at creation
  - `evaluate/2` uses NIF when `compiled_ref` is available
  - `set_weights/2` recompiles for NIF after weight changes
- **network_onnx.erl**: Fixed for new record format
  - Handles both 3-tuple and 4-tuple network records

#### Configuration
- **rebar.config**:
  - Enabled NIF build hooks (were commented out)
  - Added `test/benchmark` to extra_src_dirs

### Fixed
- Zombie neuron processes from infinite timeout loop
- Orphaned child processes on cortex termination
- Mailbox bloat from unhandled messages
- Memory growth from unbounded evo_hist
- Memory growth from persistent dead_pool

### Performance Targets
- Memory: 2-4 GB → 500 MB - 1 GB (4x reduction)
- Forward pass: 5-10x faster with NIF for dot_product
- Generation time: 4-6x faster with NIF acceleration

### Test Results
- **801 tests passing** (10 new benchmark tests)
- Dialyzer clean
- Documentation links validated

### Dependencies
- No new dependencies
- Rust NIF uses existing rustler setup

---

## [0.12.0] - 2025-12-07

### Summary
**Complete Topology Evolution & Test Coverage Release** - NEAT-style topology evolution with innovation tracking, comprehensive test coverage (791 tests), and enhanced documentation.

### Added

#### Topology Evolution (NEAT-Style)
- **innovation.erl** (~200 lines): Innovation number tracking for structural mutations
  - `init/0`, `reset/0` - Initialize/reset innovation tracking
  - `get_or_create_link_innovation/2` - Track link additions
  - `get_or_create_node_innovation/2` - Track node additions
  - Mnesia persistence for innovation history

- **genome_crossover.erl** (~250 lines): Variable-topology crossover
  - `crossover/3` - NEAT-style crossover with gene alignment
  - `align_genomes/2` - Align genes by innovation number
  - `compatibility_distance/3` - Species distance calculation
  - Matching, disjoint, and excess gene handling

- **topological_mutations.erl** enhancements:
  - `add_sensor/2` - Add sensor to existing network
  - `add_actuator/2` - Add actuator to existing network
  - Innovation number assignment for all structural changes

#### Comprehensive Test Coverage
- **198 new tests** bringing total to **791 tests**
- New test files:
  - `functions_tests.erl` - 76 tests for activation functions
  - `morphology_tests.erl` - 25 tests for morphology system
  - `brain_system_tests.erl` - 28 tests for brain API
  - `network_evaluator_tests.erl` - 27 tests for synchronous evaluation
  - `network_onnx_tests.erl` - 21 tests for ONNX export
  - `app_tests.erl` - 21 tests for application modules

#### Documentation Enhancements
- **SVG Diagrams**: Created professional diagrams
  - `assets/ltc-neuron-architecture.svg` - LTC neuron diagram
  - `assets/module-dependencies.svg` - Module architecture
  - `guides/assets/planetary-mesh-vision.svg` - Distributed vision

- **Research Opportunities**: Added to ltc-neurons.md
  - Temporal dynamics evolution
  - Hybrid architecture research
  - Application domain suggestions

- **Value Sections**: Added competitive comparisons
  - "Why Choose faber-tweann" in overview.md
  - "Why This Architecture" in architecture.md
  - Comparison tables with alternatives

#### Tooling
- **validate-docs.sh**: Link validation script
  - SVG reference checking
  - Markdown link validation
  - ASCII diagram detection
  - CI-ready exit codes

### Changed
- Version bumped from 0.11.3 to 0.12.0
- README test count updated to 791
- Replaced ASCII diagram in vision guide with SVG

### Fixed
- Broken SVG links in README.md (created missing assets)
- ASCII diagram in vision-distributed-mega-brain.md replaced with SVG

### Academic References
- NEAT paper (Stanley & Miikkulainen, 2002) referenced in innovation.erl and genome_crossover.erl

### Test Results
- 791 tests passing
- Dialyzer clean
- All documentation links validated

---

## [0.11.2] - 2025-12-06

### Summary
**Documentation Link Fixes** - Fixed all internal documentation links to use `.md` extensions.

### Fixed
- Converted all `.html` links to `.md` in guides (ex_doc converts automatically)
- Fixed `custom_morphologies.html` -> `custom-morphologies.md`
- Fixed `api-reference.html` -> removed (use module docs in sidebar)
- Fixed `ltc_dynamics.html` -> removed (use module docs directly)
- Added `scripts/fix-html-links.sh` utility script

---

## [0.11.1] - 2025-12-06

### Summary
**Documentation Alignment Release** - Fixed release documentation structure and versioning.

### Fixed
- Renamed `v0.10.0-optimized.md` to `FUTURE_OPTIMIZATION.md` (content was mismatched)
- Created proper `v0.10.0-ltc-neurons.md` release document
- Updated rebar.config ex_doc to point to correct files

### Added
- `v0.10.0-ltc-neurons.md` - Comprehensive release document for LTC neurons feature

---

## [0.11.0] - 2025-12-06

### Summary
**ONNX Export & Documentation Release** - Export trained networks to ONNX format for inference in Python, JavaScript, and other frameworks.

### Added

#### ONNX Export
- **network_onnx.erl** (~200 lines): Export evolved networks to ONNX format
  - `export/2` - Export network to ONNX binary file
  - `to_onnx/1` - Convert network to ONNX protobuf structure
  - Supports feedforward networks with standard activation functions
  - Compatible with ONNX Runtime, PyTorch, TensorFlow

#### Documentation
- **Academic references** added to README.md and guides/overview.md
  - Hasani et al. (2021) - Liquid Time-constant Networks
  - Hasani et al. (2022) - Closed-form Continuous-time Neural Networks
  - Stanley & Miikkulainen (2002) - NEAT
  - Sher (2012) - Handbook of Neuroevolution Through Erlang (DXNN2)

- **scripts/check-links.sh**: Documentation link quality checker

### Fixed
- Broken DXNN2 reference link in v0.3.1-architectural-alignment.md

### Test Results
- 270+ tests passing
- Dialyzer clean

---

## [0.10.0] - 2025-12-03

### Summary
**Liquid Time-Constant (LTC) Neurons Release** - First TWEANN library with LTC/CfC neuron support in Erlang/OTP.

LTC neurons enable adaptive temporal processing with input-dependent time constants. This is a major feature release that extends faber-tweann with continuous-time neural dynamics based on peer-reviewed research.

### Added

#### Core LTC Modules
- **ltc_dynamics.erl** (~380 lines): Core LTC/CfC computation engine
  - `evaluate_cfc/4,5` - CfC closed-form evaluation (~100x faster than ODE)
  - `evaluate_ode/5,6` - ODE-based evaluation (Euler integration)
  - `compute_backbone/3` - Time constant modulation network
  - `compute_head/2` - Target state computation
  - `compute_liquid_tau/4` - Adaptive time constant calculation
  - `clamp_state/2`, `reset_state/0` - State management utilities
  - Full EDoc with academic references

- **neuron_ltc.erl** (~280 lines): LTC-specific neuron process
  - Full process lifecycle with internal state persistence
  - CfC and ODE modes supported
  - Reset/get state operations
  - LTC parameter update support

#### LTC Evolution Support (genome_mutator.erl)
- `mutate_neuron_type/1` - Switch neurons between standard/ltc/cfc modes
- `mutate_time_constant/1` - Perturb tau (base time constant)
- `mutate_state_bound/1` - Perturb state bound A
- `mutate_ltc_weights/1` - Perturb backbone/head network weights
- `select_ltc_neuron/1` - Helper to select LTC/CfC neurons

#### Rust NIF LTC Support (native/src/lib.rs)
- `evaluate_cfc/4` - Fast CfC evaluation in Rust
- `evaluate_cfc_with_weights/6` - CfC with custom backbone/head weights
- `evaluate_ode/5` - ODE-based evaluation in Rust
- `evaluate_ode_with_weights/7` - ODE with custom weights
- `evaluate_cfc_batch/4` - Batch CfC evaluation for time series

#### Extended Records
- **records.hrl**: Extended `#neuron` record with LTC fields
  - `neuron_type` (standard | ltc | cfc)
  - `time_constant` (τ - base time constant)
  - `state_bound` (A - prevents state explosion)
  - `ltc_backbone_weights` - f() backbone network
  - `ltc_head_weights` - h() head network
  - `internal_state` - x(t) persistent state

- **types.hrl**: New LTC type specifications
  - `neuron_type()`, `time_constant()`, `state_bound()`
  - `internal_state()`, `time_step()`
  - `ltc_backbone_weights()`, `ltc_head_weights()`
  - `ltc_params()` map type

#### Documentation
- **guides/ltc-neurons.md**: Comprehensive LTC concepts guide
  - Mathematical foundations (LTC ODE, CfC closed-form)
  - Neuron types comparison table
  - Implementation details and key properties
  - Use cases and academic references

- **guides/ltc-usage-guide.md**: Practical usage guide
  - API reference with examples
  - Parameter tuning guide
  - Time series processing examples
  - Troubleshooting section

- **design_docs/diagrams/ltc-neuron-architecture.svg**: Architecture diagram
- **design_docs/diagrams/ltc-vs-standard-neurons.svg**: Comparison diagram

### Changed
- **constructor.erl**: Extended to spawn LTC neurons
  - `spawn_neuron_by_type/2` dispatches based on `neuron_type`
  - `spawn_standard_neuron/2` for standard neurons
  - `spawn_ltc_neuron/3` for ltc/cfc neurons

- **README.md**: Updated with LTC as primary feature
- **rebar.config**: Added LTC guides to ex_doc configuration

### Performance
- CfC evaluation: ~100x faster than ODE-based LTC
- State bounded dynamics prevent numerical overflow
- Configurable time constants for different response speeds

### Academic References
- Hasani, R., Lechner, M., et al. (2021). "Liquid Time-constant Networks." AAAI 2021.
- Hasani, R., Lechner, M., et al. (2022). "Closed-form Continuous-time Neural Networks." Nature Machine Intelligence.

### Test Results
- 468 tests passing (including 68 new LTC tests: 45 core + 11 mutation + 12 NIF)
- Dialyzer clean (1 pre-existing warning)

---

## Migration from DXNN2

### Key Differences

1. **Naming**: All cryptic abbreviations replaced
   - `idps` -> `weighted_inputs`
   - `af` -> `activation_function`
   - `pf` -> `plasticity_function`
   - `vl` -> `vector_length`

2. **State Management**: Records instead of parameter lists
   - cortex: 10 parameters -> cortex_state record
   - neuron: 14 parameters -> neuron_state record
   - exoself: 24 parameters -> exoself_state record

3. **Error Handling**: Structured errors instead of exit()
   - `exit("ERROR...")` -> `{error, {type, reason}}`

4. **APIs**: Modern OTP
   - `now()` -> `erlang:monotonic_time()`
   - `random` -> `rand`

### Migration Steps

1. Update type imports from types.hrl
2. Update record field names
3. Update function return types for error cases
4. Update time-related code
5. Run test suite to verify

---

## References

- `design_docs/DXNN2_CODEBASE_ANALYSIS.md` - DXNN2 original codebase analysis (internal)
- `design_docs/README.md` - Refactoring principles (internal)

---

[Unreleased]: https://github.com/rgfaber/faber-tweann/compare/v0.16.0...HEAD
[0.16.0]: https://github.com/rgfaber/faber-tweann/compare/v0.15.3...v0.16.0
[0.15.3]: https://github.com/rgfaber/faber-tweann/compare/v0.15.2...v0.15.3
[0.15.2]: https://github.com/rgfaber/faber-tweann/compare/v0.15.1...v0.15.2
[0.15.1]: https://github.com/rgfaber/faber-tweann/compare/v0.15.0...v0.15.1
[0.15.0]: https://github.com/rgfaber/faber-tweann/compare/v0.14.0...v0.15.0
[0.14.0]: https://github.com/rgfaber/faber-tweann/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/rgfaber/faber-tweann/compare/v0.12.0...v0.13.0
[0.12.0]: https://github.com/rgfaber/faber-tweann/compare/v0.11.2...v0.12.0
[0.11.2]: https://github.com/rgfaber/faber-tweann/compare/v0.11.1...v0.11.2
[0.11.1]: https://github.com/rgfaber/faber-tweann/compare/v0.11.0...v0.11.1
[0.11.0]: https://github.com/rgfaber/faber-tweann/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/rgfaber/faber-tweann/releases/tag/v0.10.0
