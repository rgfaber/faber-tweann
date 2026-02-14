# Macula-TWEANN Memory Analysis Report

**Date:** 2025-12-07
**Status:** ANALYSIS COMPLETE - Awaiting User Direction
**Task:** Deep dive memory consumption analysis of faber-tweann library
**Codebase:** `/home/rl/work/github.com/rgfaber/faber-tweann`

---

## Executive Summary

Analysis reveals **systematic memory accumulation** across the codebase with no garbage collection mechanisms. The primary culprit is **Mnesia RAM-based storage** that grows unbounded throughout evolution runs.

**Estimated memory footprint for typical run (100 agents × 1000 generations):** 2-4 GB

---

## CRITICAL ISSUES (Immediate Impact)

### 1. Mnesia Genotype Database - Unbounded Growth

**Files:** `src/genotype.erl`, `include/records.hrl`

| Field | Location | Growth Pattern |
|-------|----------|----------------|
| `agent.evo_hist` | records.hrl:329 | +1 entry per mutation, never pruned |
| `agent.offspring_ids` | records.hrl:341 | Accumulates all offspring references |
| `agent.parent_ids` | records.hrl:342 | Accumulates all parent references |
| `specie.all_agent_ids` | records.hrl:412 | Every agent ever in species |
| `specie.dead_pool` | records.hrl:414 | All removed agents, never cleaned |
| `specie.hall_of_fame` | records.hrl:422 | Champions accumulate forever |
| `population.trace_acc` | records.hrl:577 | Statistics across all evaluations |

**Impact:**
- 100K+ agent records stored permanently in RAM
- Each agent: ~2KB (with topology info)
- Plus cortex/sensor/neuron/actuator: 10-20x multiplier
- **Estimated: 2-4 GB** for long runs

---

### 2. Innovation Tables - Permanent Historical Tracking

**File:** `src/innovation.erl:59-90`

```erlang
-record(link_innovation, {key :: {term(), term()}, innovation :: pos_integer()}).
-record(node_innovation, {key :: {term(), term()}, node_innovation, in_innovation, out_innovation}).
```

- Mnesia RAM storage (lines 71-72): `{ram_copies, [node()]}`
- **Never cleared** during evolution (only via explicit `reset()`)
- 10,000+ entries typical for topology evolution
- **Estimated: 2-5 MB** per run

---

### 3. Process Lifecycle - Orphaned Processes

| Component | File | Issue | Impact |
|-----------|------|-------|--------|
| Neuron | neuron.erl:159-169 | Infinite timeout loop on missing inputs | Zombies accumulate |
| Cortex | cortex.erl:251-280 | No wait for child termination | Orphaned children |
| Exoself | exoself.erl:499-515 | Race condition on shutdown | Network persists |
| PopMon | population_monitor.erl:276 | 60s timeout too long | Hanging networks |

**Neuron timeout loop (CRITICAL):**
```erlang
after Timeout ->
    handle_input_timeout(State)  %% Logs warning, then calls loop(State) forever
```
A neuron expecting 4 inputs receiving only 3 will **loop indefinitely**.

---

### 4. Deep Cloning in Crossover - O(N²) Cost

**File:** `src/crossover.erl:224-233`, `src/genotype.erl:379-429`

```erlang
%% Uses inefficient list append (line 558)
{AccIds ++ [NewId], AccMap#{OldId => NewId}}  %% O(n) per append = O(N²) total
```

**Cost calculation:**
- 200 neurons × 5 inputs = 1000 weight tuples per clone
- 50 agents × 100 generations = 5,000 crossovers
- **5 million weight tuple operations** per run

---

## HIGH SEVERITY ISSUES

### 5. Neuron Weight Maps - Permanent Process State

**File:** `src/neuron.erl:38-51`

```erlang
input_weights :: #{pid() => [{float(), float(), float(), list()}]}
```

- Each weight tuple: `{Weight, DeltaWeight, LearningRate, ParameterList}`
- 100 neurons × 100 inputs = 10,000 connections
- **~50 MB** for large evolved networks
- Never freed until network termination

---

### 6. Inefficient Weight Storage Format

**File:** `include/records.hrl:140-142`

```erlang
input_idps = [{SourceId, [{Weight, DeltaWeight, LearningRate, ParamList}...]}, ...]
```

**Problems:**
- Redundant: LearningRate and ParamList copied per connection
- Deep nesting: Lists of lists of tuples with lists
- No structural sharing

---

### 7. ETS Tables Per Evaluation

**File:** `src/exoself.erl:107`

```erlang
IdToProcessMap = ets:new(id_to_process_map, [set, private])
```

- Created per exoself (per agent evaluation)
- ~6KB per agent network
- If not GC'd between evaluations: **600KB per generation**

---

## MODERATE SEVERITY ISSUES

### 8. Experience Buffer in brain_learner

**File:** `src/brain_learner.erl:80-106`

- Default `max_buffer_size`: 1000 experiences
- Each experience: ~400 bytes (inputs + all activations + outputs)
- **~400KB per learner** (capped, but significant)

---

### 9. Process Dictionary Leaks

| Module | Key | Issue |
|--------|-----|-------|
| sensor.erl:166-176 | `step_value` | Never cleaned on termination |
| signal_aggregator.erl:136-146 | `diff_product` | Stores previous inputs forever |

---

### 10. No Catch-All Receive Clauses

**Files:** `sensor.erl`, `actuator.erl`, `neuron.erl`, `cortex.erl`

- Unexpected messages accumulate in mailbox silently
- No visibility into garbage message accumulation

---

## ARCHITECTURAL ANTI-PATTERNS

| Pattern | Location | Issue |
|---------|----------|-------|
| No GC triggers | Everywhere | `delete_Agent` exists but never auto-called |
| RAM-only Mnesia | genotype.erl | No disk backup, no cleanup |
| Full cloning | crossover.erl | No copy-on-write or references |
| List ++ append | genotype.erl:558 | O(N²) instead of O(N) |
| Record bloat | records.hrl | All fields present even if unused |

---

## MEMORY FOOTPRINT BREAKDOWN

**Scenario:** 100 agents, 1000 generations, 10→[16,8]→6 topology

| Component | Estimated Size | Growth |
|-----------|----------------|--------|
| Mnesia genotype records | 2.0 GB | Unbounded |
| Active network processes | 50 MB | Per generation |
| Innovation tracking | 5 MB | Unbounded |
| Fitness accumulation | 10 MB | Per generation |
| ETS/process state | 50 MB | Per generation |
| Traces/stats/hall of fame | 500 KB | Unbounded |
| **TOTAL** | **~2.1 GB** | |

**With 10,000 generations: 20+ GB** (likely OOM)

---

## ROOT CAUSES (Summary)

1. **Mnesia RAM-only storage** - No archival, no cleanup, no disk fallback
2. **No automatic garbage collection** - Dead agents persist forever
3. **No generation-based cleanup** - Old data never pruned
4. **Deep cloning instead of references** - Full copies on every operation
5. **Process lifecycle gaps** - Async termination, no monitoring
6. **Inefficient data structures** - Nested lists, redundant storage

---

## FILES REQUIRING ATTENTION

| Priority | File | Issues |
|----------|------|--------|
| CRITICAL | `src/genotype.erl` | Mnesia storage, clone operations |
| CRITICAL | `src/innovation.erl` | Unbounded tables |
| CRITICAL | `src/neuron.erl` | Infinite timeout loop |
| HIGH | `src/cortex.erl` | Termination handling |
| HIGH | `src/exoself.erl` | Race condition, ETS cleanup |
| HIGH | `src/crossover.erl` | O(N²) cloning |
| HIGH | `src/population_monitor.erl` | Long timeouts |
| MEDIUM | `include/records.hrl` | Record bloat, evo_hist |
| MEDIUM | `src/signal_aggregator.erl` | Process dict leak |

---

---

## DEEP DIVE: RUNTIME MEMORY BEHAVIOR

### Message Passing Overhead (The Hidden Culprit)

**Per forward pass through 100-neuron network:**
| Operation | Per-Unit | Count | Total |
|-----------|----------|-------|-------|
| Message allocation | 56 bytes | 600 | 33 KB |
| Neuron signal forward | 350 bytes | 100 | 35 KB |
| Input list construction | 240 bytes | 100 | 24 KB |
| Weight list construction | 240 bytes | 100 | 24 KB |
| Accumulator maps | 40 bytes | 500 | 20 KB |
| **Per cycle total** | | | **~136 KB** |

**Per agent evaluation (50 cycles × 15 tuning attempts):**
- Forward pass churn: 136 KB × 50 = **6.8 MB**
- Process creation overhead: **~1 MB**
- Network state: **~2-3 MB**
- **Total per agent: ~10 MB temporary allocations**

**GC Pressure per generation (100 agents):**
- 100 × 10 MB = **1 GB allocation churn**
- All temporary → aggressive GC required
- Low-spec hardware will struggle with GC pauses

### Hot Path: neuron.erl process_and_forward/1

```erlang
%% REBUILDS THESE LISTS EVERY CYCLE:
Inputs = build_inputs(InputPids, AccInput),      %% 240 bytes/neuron
Weights = build_weights(InputPids, InputWeights), %% 240 bytes/neuron
```

**Critical inefficiency:** Every neuron rebuilds input/weight lists on every forward pass instead of keeping compiled matrices.

---

## DEEP DIVE: MNESIA ASSESSMENT

### Current Configuration

| Table | Type | Storage | Indices | Size Limit |
|-------|------|---------|---------|------------|
| agent | set | ram_copies | none | none |
| cortex | set | ram_copies | none | none |
| sensor | set | ram_copies | none | none |
| actuator | set | ram_copies | none | none |
| neuron | set | ram_copies | none | none |
| substrate | set | ram_copies | none | none |
| specie | set | ram_copies | none | none |
| population | set | ram_copies | none | none |
| innovation_counter | set | ram_copies | none | none |
| link_innovation | set | ram_copies | none | none |
| node_innovation | set | ram_copies | none | none |

**Critical Finding: Mnesia is OVER-ENGINEERED for this workload**

| Aspect | Current (Mnesia) | Better (ETS) |
|--------|------------------|--------------|
| Startup cost | 100-500ms | ~0ms |
| Write overhead | Transaction machinery | Direct write |
| Persistence | None (ram_copies) | None |
| Distribution | Not used | Not needed |
| Crash safety | Low (multi-step writes) | Same |

**Recommendation:** ETS would provide identical semantics with lower overhead.

### Read/Write Pattern

- **Read-heavy:** 5:1 ratio (reads via `dirty_read`, writes via transaction)
- **No multi-record transactions:** Agent + cortex + neurons written separately
- **Risk:** Crash between writes = orphaned records (no recovery mechanism)

---

## DEEP DIVE: CLUSTER READINESS

### Current State: SINGLE-NODE ONLY

| Component | Current | For beam00-03 Cluster |
|-----------|---------|----------------------|
| Mnesia | `ram_copies` local only | Need `disc_copies` replicated |
| Innovation counter | Per-node counter | Need global consensus (Raft/DHT) |
| Population monitor | Single gen_server | Need distributed coordinator |
| Agent spawning | Local `spawn_link` | Need RPC or distributed supervisor |
| Process registry | Hardcoded PIDs | Need `gproc` or `pg` |

### What Would Break Across Nodes

1. **Innovation collision:** Node A and B both assign innovation #47 to different mutations
2. **Genotype isolation:** Node A can't read Node B's agents (RAM-only)
3. **Selection bottleneck:** Population monitor can't see remote agents
4. **Orphaned networks:** Remote exoself terminates, local cortex persists

### Cluster Deployment Options

| Phase | Description | Effort | Memory Impact |
|-------|-------------|--------|---------------|
| Phase 1 | Independent nodes (no coordination) | 0 weeks | Each node 2-4 GB |
| Phase 2 | Share best genotypes via Macula pub/sub | 4-6 weeks | Same + network |
| Phase 3 | Global selection coordinator | 8-10 weeks | Reduced (shared elites) |

---

## RECOMMENDATIONS BY HARDWARE CONSTRAINT

**Target: beam00-03 cluster (16-32 GB RAM, 4-core Celeron)**

### Immediate Fixes (Memory Reduction)

| Fix | Impact | Effort | Files |
|-----|--------|--------|-------|
| Add neuron exit on timeout | Prevents zombie accumulation | 1 day | neuron.erl:159 |
| Reduce PopMon timeout 60s→5s | Faster cleanup of hanging agents | 1 hour | population_monitor.erl:276 |
| Add catch-all receive clauses | Prevent mailbox bloat | 1 day | All process modules |
| Synchronous cortex termination | Prevent orphaned networks | 1 day | cortex.erl:251, exoself.erl:499 |

### Medium-Term (Architecture)

| Change | Impact | Effort | Trade-off |
|--------|--------|--------|-----------|
| Replace Mnesia with ETS | Faster startup, lower overhead | 1 week | No distribution |
| Generation-based cleanup | Prevent unbounded growth | 2 weeks | Lose old agents |
| Cap evo_hist to last 50 | ~90% memory reduction on evo_hist | 1 day | Lose mutation history |
| Pre-compile weight matrices | Reduce hot path allocation | 2 weeks | Code complexity |

### Long-Term (Scalability)

| Change | Impact | Effort | Prerequisite |
|--------|--------|--------|--------------|
| Distributed innovation (Raft) | Enable multi-node NEAT | 2 weeks | Khepri/Ra |
| Population coordinator | Global selection | 3-4 weeks | Macula integration |
| Copy-on-write genotypes | Reduce cloning overhead | 3 weeks | Major refactor |

---

## KEY METRICS FOR LOW-SPEC HARDWARE

**For beam00 (16 GB RAM):**
- Safe population size: **50-100 agents**
- Safe generations: **100-500** (with cleanup)
- Network size limit: **~200 neurons** per agent
- Expected memory: **1-2 GB** with fixes

**For beam01-03 (32 GB RAM):**
- Safe population size: **100-200 agents**
- Safe generations: **500-1000** (with cleanup)
- Network size limit: **~500 neurons** per agent
- Expected memory: **2-4 GB** with fixes

---

## NEXT STEPS (Pending User Direction)

This is an **analysis document**. No implementation will occur without explicit approval.

### Option A: Quick Stability Fixes
Fix process lifecycle issues (1-2 days work):
- Neuron timeout exit
- Synchronous termination
- Catch-all receives
- Reduced PopMon timeout

### Option B: Memory Architecture
Replace Mnesia + add cleanup (1-2 weeks):
- ETS migration
- Generation-based pruning
- evo_hist capping
- dead_pool cleanup

### Option C: Performance Optimization
Reduce hot path allocations (2-3 weeks):
- Pre-compile weight matrices
- Message batching
- Reduce list/map reconstruction

### Option D: Cluster Preparation
Enable multi-node operation (4-6 weeks):
- Distributed innovation counter
- Macula pub/sub integration
- Shared elite genotypes

---

## DEEP DIVE: NIF OPTIMIZATION OPPORTUNITIES

### Existing Rust Infrastructure

**Location:** `native/src/lib.rs` (772 lines)

**Already Implemented (DORMANT):**
- `compile_network` - Compile genotype to feedforward network
- `evaluate` / `evaluate_batch` - Network evaluation
- `compatibility_distance` - NEAT speciation
- CfC/ODE evaluation for LTC neurons
- 59 tests, release optimizations enabled

**Build Config (commented out in rebar.config):**
```erlang
{pre_hooks, [{"compile", "cd native && cargo build --release"}]},
{post_hooks, [{"compile", "cp native/target/release/libtweann_nif.so priv/"}]}
```

### NIF Candidate Ranking

| Rank | Candidate | Location | Speedup | Calls/Run | Impact |
|------|-----------|----------|---------|-----------|--------|
| 1 | **Signal Aggregation** | signal_aggregator.erl | 15-50x | 1-10M | 40-50% |
| 2 | **Batch Activation** | functions.erl | 20-30x | 100K-1M | 20-30% |
| 3 | Network Evaluation | native/src/lib.rs | Already NIF | - | Integrate |
| 4 | LTC Dynamics | native/src/lib.rs | Already NIF | - | Integrate |
| 5 | Crossover | crossover.erl | 5-15x | 1-5K | <5% |
| 6 | Distance Calc | species_identifier.erl | 2-5x | 1K/gen | <2% |

### Signal Aggregation (CRITICAL - #1 Priority)

**Current Erlang (hot path):**
```erlang
%% Called per neuron per forward pass - LIST PROCESSING
dot_product([{SourceId, Signals} | Rest], [{SourceId, Weights} | RestW], Acc) ->
    DotSum = weighted_sum(Signals, Weights, 0.0),
    dot_product(Rest, RestW, DotSum + Acc);
```

**Bottlenecks:**
- List pattern matching overhead
- 4-tuple weight unpacking `{W, DW, LP, LPs}`
- Recursive function calls
- Memory allocation per operation

**Proposed NIF:**
```rust
#[rustler::nif]
fn signal_aggregator_dot_product(
    inputs: Vec<(String, Vec<f64>)>,
    weights: Vec<(String, Vec<(f64, f64, f64, Vec<f64>)>)>,
) -> f64 {
    // Vectorized, SIMD-capable, zero intermediate allocation
}
```

### Batch Activation (HIGH Priority)

**Current:** Each neuron calls `functions:tanh(Val)` individually

**Proposed:** Batch all neurons through single NIF call:
```rust
#[rustler::nif]
fn activate_batch(values: Vec<f64>, activation: Atom) -> Vec<f64> {
    // Process 100+ neurons in single call
    // Amortize NIF overhead
    // Enable SIMD vectorization
}
```

### Celeron J4105 Considerations

| Factor | Impact | Strategy |
|--------|--------|----------|
| Low clock (1.5 GHz) | NIF call overhead expensive | **Batch operations** |
| Small L1 cache (32 KB) | List traversal = cache misses | **Flat arrays in Rust** |
| 4 cores, no hyperthreading | SIMD more valuable than threads | **Vectorization focus** |

**Break-even:** NIF profitable for networks > 10 neurons (amortizes 1-5µs call overhead)

---

## COMPREHENSIVE OPTIMIZATION ROADMAP

### Phase 1: Quick Stability (1-2 days)
**Goal:** Stop memory leaks, prevent zombie processes

| Task | File | Change |
|------|------|--------|
| Neuron timeout exit | neuron.erl:159 | Exit after 3 timeouts instead of infinite loop |
| Reduce PopMon timeout | population_monitor.erl:276 | 60s → 5s |
| Synchronous termination | cortex.erl:251, exoself.erl:499 | Monitor + wait for children |
| Catch-all receives | All process modules | Log and discard unexpected messages |

### Phase 2: Memory Architecture (1-2 weeks)
**Goal:** Bounded memory growth, efficient storage

| Task | Impact | Change |
|------|--------|--------|
| Replace Mnesia with ETS | Faster startup, lower overhead | 11 table migrations |
| Generation-based cleanup | Prevent unbounded growth | Delete agents > N gens old |
| Cap evo_hist | 90% reduction | Keep last 50 mutations only |
| Clear dead_pool | Prevent accumulation | Clear on each generation |

### Phase 3: NIF Integration (1-2 weeks)
**Goal:** 10-50x speedup on hot paths

| Task | Files | Speedup |
|------|-------|---------|
| Enable existing NIFs | rebar.config, build | - |
| Signal aggregation NIF | signal_aggregator.erl, lib.rs | 15-50x |
| Batch activation NIF | functions.erl, lib.rs | 20-30x |
| Integrate LTC NIF | ltc_dynamics.erl | 5-10x |

### Phase 4: Hot Path Optimization (1-2 weeks)
**Goal:** Reduce GC pressure in Erlang code

| Task | Current | Optimized |
|------|---------|-----------|
| Pre-compile weight matrices | Rebuild every cycle | Build once at link |
| Flat array signals | Nested lists | Contiguous memory |
| Message batching | 600 msgs/cycle | 10-20 batched sends |

### Phase 5: Cluster Preparation (Future)
**Goal:** Multi-node evolution on beam00-03

| Task | Prerequisite | Effort |
|------|--------------|--------|
| Distributed innovation | Khepri/Raft | 2 weeks |
| Population coordinator | Macula pub/sub | 3-4 weeks |
| Shared elites | Macula RPC | 1 week |

---

## SUCCESS METRICS

**Current (estimated):**
- Memory: 2-4 GB for 100 agents × 1000 generations
- Speed: ~60s per generation (100 agents, 50 neurons each)
- Stability: Zombie processes accumulate over hours

**Target after optimization:**
- Memory: **500 MB - 1 GB** (4x reduction)
- Speed: **10-15s per generation** (4-6x faster with NIFs)
- Stability: **No leaks** over 24+ hour runs

**Safe limits for beam00-03:**
| Node | RAM | Population | Generations | Network Size |
|------|-----|------------|-------------|--------------|
| beam00 | 16 GB | 100-150 | 1000+ | 300 neurons |
| beam01-03 | 32 GB | 200-300 | 2000+ | 500 neurons |

---

## DEEP DIVE: NEURON.ERL HOT PATH

### State Record Memory (lines 38-51)

```erlang
-record(state, {
    id,                    % 8-16 bytes
    cortex_pid,            % 12 bytes
    activation_function,   % 4-8 bytes (atom)
    aggregation_function,  % 4-8 bytes (atom)
    input_pids,            % 16 + N×12 bytes (list)
    output_pids,           % 16 + M×12 bytes (list)
    ro_pids,               % 16 + K×12 bytes (list)
    input_weights,         % 56+ bytes (map) + weight data
    bias,                  % 8 bytes
    acc_input,             % 56+ bytes (map) - REBUILT EVERY CYCLE
    expected_inputs,       % 8-16 bytes
    input_timeout          % 8-16 bytes
}).
```

**Total per neuron: ~450-700 bytes**

### Forward Pass Allocations (process_and_forward lines 189-230)

| Line | Operation | Allocation |
|------|-----------|------------|
| 202 | `build_inputs(InputPids, AccInput)` | N×16 bytes (list comprehension) |
| 205 | `build_weights(InputPids, InputWeights)` | N×variable bytes |
| 208 | `aggregate(AggregationFn, Inputs, Weights)` | ~32 bytes |
| 211 | `activate(ActivationFn, Aggregated + Bias)` | 8 bytes |
| 214-226 | `lists:foreach` message sends | (M+K)×24 bytes |
| 230 | `State#state{acc_input = #{}}` | 56+ bytes |

**Total per forward pass: ~250-1000 bytes** (freed after GC)

### Critical Inefficiency: build_inputs/build_weights

```erlang
%% Line 233 - CALLED EVERY CYCLE
build_inputs(InputPids, AccInput) ->
    [{Pid, maps:get(Pid, AccInput, [0.0])} || Pid <- InputPids].

%% Line 236 - CALLED EVERY CYCLE
build_weights(InputPids, InputWeights) ->
    [{Pid, maps:get(Pid, InputWeights, [{1.0, 0.0, 0.1, []}])} || Pid <- InputPids].
```

**Problem:** Lists rebuilt from scratch on every forward pass
**Solution:** Pre-compile weight matrices at link time, reuse across cycles

### Message Flow Per Cycle

| Direction | Count | Size | Total |
|-----------|-------|------|-------|
| Sensor→Neuron | N sensors | ~40 bytes | 160 bytes |
| Neuron→Neuron | 600 msgs | ~40 bytes | 24 KB |
| Neuron→Actuator | K msgs | ~40 bytes | 160 bytes |
| **Per cycle** | **~609** | | **~25 KB** |

**Per agent (50 cycles): 30,000 messages, 1.25 MB message allocation**

---

## DEEP DIVE: NIF INFRASTRUCTURE

### Existing Implementation Status

| Component | Location | Status |
|-----------|----------|--------|
| Rust code | `native/src/lib.rs` (771 lines) | Complete |
| Erlang wrapper | `src/tweann_nif.erl` (282 lines) | Complete |
| Compiled .so | `priv/tweann_nif.so` (457 KB) | Present |
| Build hooks | `rebar.config` | **Commented out** |

### Exported NIF Functions

| Function | Purpose | Currently Used? |
|----------|---------|-----------------|
| `compile_network/3` | Compile genotype to fast format | YES (network_compiler.erl:78) |
| `evaluate/2` | Single forward pass | NO (pure Erlang used) |
| `evaluate_batch/2` | Batch forward passes | NO |
| `compatibility_distance/5` | NEAT speciation | NO |
| `evaluate_cfc/4` | CfC LTC dynamics | NO |
| `evaluate_cfc_batch/4` | Batch CfC | NO |
| `evaluate_ode/5` | ODE LTC dynamics | NO |

### Activation Enablement Path

```
Current: brain.erl → network_evaluator:evaluate() → Pure Erlang loops
Target:  brain.erl → tweann_nif:evaluate() → Rust vectorized ops
```

**Required changes:**
1. Uncomment `rebar.config` hooks (lines 25-33)
2. Modify `brain.erl` to use compiled network + NIF evaluate
3. Add `signal_aggregator_dot_product` NIF for neuron hot path

---

## DEEP DIVE: MNESIA→ETS MIGRATION

### Tables to Migrate (11 total)

**Genotype Tables (8):**
| Table | Key Format | Records/Run |
|-------|------------|-------------|
| agent | `{float, agent}` | 100-1000 |
| cortex | `{{origin,float}, cortex}` | 100-1000 |
| sensor | `{{-1,float}, sensor}` | 200-4000 |
| actuator | `{{1,float}, actuator}` | 100-2000 |
| neuron | `{{float,float}, neuron}` | 1000-100000 |
| substrate | TBD | 0 (unused) |
| specie | TBD | 10-100 |
| population | TBD | 1-10 |

**Innovation Tables (3):**
| Table | Key Format | Records/Run |
|-------|------------|-------------|
| innovation_counter | `counter` atom | 1 |
| link_innovation | `{FromId, ToId}` | 100-10000 |
| node_innovation | `{FromId, ToId}` | 10-1000 |

### Migration Steps

**Phase A: Core Infrastructure (genotype.erl)**
```erlang
%% Replace init_db/0 (lines 86-124):
init_db() ->
    [ets:new(T, [set, public, named_table, {read_concurrency, true}])
     || T <- [agent, cortex, sensor, actuator, neuron, substrate, specie, population]],

    %% Counter for innovation
    CounterRef = counters:new(1, [atomics]),
    persistent_term:put(innovation_counter, CounterRef),

    [ets:new(T, [set, public, named_table, {read_concurrency, true}])
     || T <- [link_innovation, node_innovation]],
    ok.
```

**Phase B: Read/Write Operations**
```erlang
%% Replace read/1 (line 148):
[Record] = ets:lookup(Table, Key)  %% vs mnesia:read

%% Replace write/1 (line 157):
ets:insert(Table, Record)  %% vs mnesia:write

%% Replace dirty_read/1 (line 162):
case ets:lookup(Table, Key) of [{_, R}] -> R; [] -> undefined end
```

**Phase C: Innovation Counter**
```erlang
%% Replace next_innovation/0 (innovation.erl line 116):
CounterRef = persistent_term:get(innovation_counter),
counters:add(CounterRef, 1, 1)  %% vs mnesia:dirty_update_counter
```

### Files Requiring Changes

| File | Lines Changed | Priority |
|------|---------------|----------|
| `src/genotype.erl` | ~80 | HIGH |
| `src/innovation.erl` | ~60 | HIGH |
| `src/network_compiler.erl` | ~20 | MEDIUM |
| `src/network_evaluator.erl` | ~20 | MEDIUM |
| `test/fixtures/test_helpers.erl` | ~60 | MEDIUM |
| `test/unit/genotype_tests.erl` | ~10 | LOW |
| **Total** | **~250** | |

### Key Differences

| Aspect | Mnesia | ETS |
|--------|--------|-----|
| Return format | `[Record]` | `[{Key, Record}]` |
| Transaction | Required | Not needed |
| Persistence | Optional | None |
| Counter | `dirty_update_counter` | `counters` module |
| Select | `mnesia:dirty_select` | `ets:select` (same syntax) |

---

## DEEP DIVE: Nx TENSOR LIBRARY ANALYSIS

### Executive Summary

**Finding: Nx is NOT suitable for direct faber-tweann integration.**

The NEAT algorithm's frequent topology mutations make JIT compilation overhead prohibitive. The existing dormant Rust NIF already provides the same benefits without the drawbacks.

### Why Nx Doesn't Fit NEAT

| Issue | Impact on NEAT |
|-------|----------------|
| **JIT Compilation** | 10-50ms warmup per topology - paid on every mutation |
| **Erlang→Elixir Interop** | 1-2µs overhead per operation, adds up in hot paths |
| **Fixed Topology Assumption** | Nx/Axon expect layer-based networks, not arbitrary graphs |
| **Small Network Penalty** | For <200 neurons, JIT overhead exceeds computation time |

### Nx Memory Benefits (Theoretical)

If Nx could be used efficiently:
- **4-10x memory reduction** (flat binaries vs nested lists)
- **Cache-friendly** contiguous memory layout
- **SIMD vectorization** via EXLA backend

### Current Weight Representation Problem

```erlang
%% Current: Nested lists of 4-tuples (4-6 KB per 50 weights)
input_idps = [
  {SensorId1, [{W1, DW1, LR1, Params1}, {W2, DW2, LR2, Params2}]},
  {NeuronId1, [{W3, DW3, LR3, Params3}]},
  {bias,      [{BiasWeight, 0.0, 0.1, []}]}
]
```

**Problems:**
- 64+ bytes per weight tuple (vs 8 bytes for f64)
- Sequential list traversal in hot path
- Map overhead for PID lookups
- Rebuilds weight lists every forward pass

### Comparison: Nx vs Rust NIF vs Flat Erlang

| Aspect | Current Erlang | Flat Erlang | Rust NIF | Nx |
|--------|----------------|-------------|----------|-----|
| **Memory (50 weights)** | 4-6 KB | 1-2 KB | ~800 B | ~600 B |
| **JIT Startup** | 0 | 0 | 0 | 10-50ms |
| **Per-eval overhead** | 1-2ms | 0.5-1ms | 0.1-0.2ms | 0.5-1ms |
| **Plasticity support** | Full | Full | W only | Would need redesign |
| **Dependencies** | None | None | Cargo | Nx, EXLA |
| **Celeron J4105 fit** | Good | Better | Best | Poor (JIT) |

### Critical Finding: Dormant NIF Already Solves This

**File:** `native/src/lib.rs` (771 lines)

The existing Rust NIF already provides:
- Flat f64 array representation
- Weight-only storage (strips DW/LR/Params for inference)
- 10-100x faster than pure Erlang
- CfC and ODE LTC dynamics
- Compiled and present in `priv/tweann_nif.so`

**Currently unused functions:**
- `evaluate/2` - Single forward pass
- `evaluate_batch/2` - Batch evaluation
- `compatibility_distance/5` - NEAT speciation
- `evaluate_cfc/4`, `evaluate_ode/5` - LTC dynamics

### Recommended Path (Updated)

**Phase 1: Flat Arrays in Erlang** (1-2 weeks, 50-70% memory savings)
```erlang
%% Proposed: Parallel flat lists
Weights = [W1, W2, W3, ...],         %% f64 list
DeltaWeights = [DW1, DW2, DW3, ...], %% f64 list
LearningRates = [LR1, LR2, LR3, ...] %% f64 list
%% Keep Params separate (variable length)
```

**Benefits:**
- No new dependencies
- Compatible with existing plasticity rules
- 50-70% memory reduction
- Cache-friendly sequential access

**Phase 2: Activate Dormant NIFs** (1-2 weeks, 10-50x speedup)
1. Uncomment `rebar.config` build hooks
2. Modify `brain.erl` to use `tweann_nif:evaluate/2`
3. Already implemented - just needs wiring

**Phase 3: Skip Nx** (0 effort)
- Nx adds complexity without benefit for NEAT
- JIT overhead defeats purpose for variable topology
- Rust NIF provides same memory efficiency + better performance

### When Nx WOULD Make Sense

**Hypothetical future use cases:**
1. **Fixed-topology batch evaluation** - Same network, different inputs
2. **Post-evolution deployment** - Frozen champion network
3. **GPU acceleration** - Large networks (1000+ neurons)

**Not applicable to current NEAT evolution loop.**

### Celeron J4105 Specific Guidance

| Factor | Impact | Recommendation |
|--------|--------|----------------|
| 1.5 GHz clock | Low | Minimize JIT overhead → use NIF |
| 32 KB L1 cache | Small | Flat arrays → cache-friendly |
| 4 cores | Limited | SIMD more valuable than threads |
| No AVX-512 | Limited | Rust NIF uses AVX2 if available |

**Conclusion:** Rust NIF is the best fit for this hardware, not Nx.

---

## UPDATED OPTIMIZATION STRATEGY

Based on Nx analysis, the recommended approach is:

### Tier 1: Quick Wins (Days)
1. Fix process lifecycle bugs (neuron timeout, termination)
2. Reduce PopMon timeout 60s → 5s
3. Add catch-all receive clauses

### Tier 2: Memory Architecture (1-2 Weeks)
1. Replace Mnesia with ETS (11 tables)
2. Generation-based cleanup (delete old agents)
3. Cap evo_hist to last 50 mutations
4. Clear dead_pool each generation

### Tier 3: Erlang Representation (1-2 Weeks)
1. Flatten weight tuples to parallel lists
2. Pre-compile weight matrices at link time
3. Reduce list rebuilding in hot path

### Tier 4: NIF Activation (1-2 Weeks)
1. Uncomment rebar.config build hooks
2. Wire brain.erl to tweann_nif:evaluate/2
3. Add signal_aggregator NIF for hot path
4. Integrate LTC NIFs

### Skip: Nx Integration
- Not recommended for NEAT workload
- JIT overhead prohibitive
- Rust NIF provides same benefits

---

*Analysis updated 2025-12-07 with Nx tensor library evaluation*

---

## DEEP DIVE: BENCHMARKING SUITE DESIGN

### Recommended Module Structure

```
test/benchmark/
├── bench_common.erl          # Shared utilities (timing, memory, GC)
├── bench_memory_growth.erl   # Memory over generations
├── bench_forward_pass.erl    # Forward propagation latency
├── bench_nif_vs_erlang.erl   # NIF comparison
├── bench_evolution_ops.erl   # Mutation/crossover/clone timing
└── bench_hot_paths.erl       # Micro-benchmarks for hot functions
```

### Key Benchmark Commands

```bash
# Run all benchmarks
rebar3 eunit tests=benchmark

# Run specific module
rebar3 eunit tests=bench_forward_pass

# With profiling
erl -pa _build/test/lib/*/ebin -eval "eprof:start(), eprof:start_profiling([self()]), eunit:run(tests=benchmark), eprof:stop_profiling(), eprof:analyze()."
```

### Expected Baseline Measurements

| Metric | Current Estimate | Target After Optimization |
|--------|------------------|---------------------------|
| Forward pass (50 neurons) | 60-100 µs | 10-20 µs (with NIF) |
| Forward pass (200 neurons) | 250-400 µs | 40-80 µs (with NIF) |
| Memory per agent | 5-20 KB | Same (storage, not hot path) |
| Clone operation (100 neurons) | 5-15 ms | <20 ms |
| dot_product (10 inputs) | 0.5-1.5 µs | 0.01-0.05 µs (NIF) |
| GC count per 100 cycles | 5-20 | <10 (with flat arrays) |

### Benchmark Utilities (bench_common.erl)

Key functions to implement:
- `measure_time/1,2` - Wall-clock timing in µs
- `measure_memory/1,2` - Process memory delta
- `run_trials/3` - Multiple trials with statistics
- `run_trials_gc/3` - Trials with GC between each
- `format_bytes/1` - Human-readable memory formatting

---

## DEEP DIVE: MACULA-NEUROEVOLUTION ANALYSIS

### Memory Architecture Comparison

| Aspect | faber-tweann | macula-neuroevolution |
|--------|---------------|----------------------|
| Storage | Mnesia RAM tables | In-process state (ETS-like) |
| Unbounded growth | evo_hist, dead_pool | MAP-Elites grid, ages map |
| Process lifecycle | Zombie neurons | Meta-controller leak |
| Cleanup triggers | None (delete_Agent unused) | Generation boundary (partial) |

### Critical Issues Found

#### 1. MAP-Elites Grid Unbounded

**File:** `src/strategies/map_elites_strategy.erl:110-113`

```erlang
TotalCells = round(math:pow(Bins, Dimensions)),
%% No upper bound! 10^5 = 100,000 cells possible
```

Default is safe (10² = 100 cells), but user can configure:
- `behavior_dimensions = 5, bins_per_dimension = 10`
- Result: **100,000 cells**, each storing full individual!

**Recommendation:** Add validation: `TotalCells =< 10000`

#### 2. Steady-State Ages Map Leak

**File:** `src/strategies/steady_state_strategy.erl:127`

```erlang
Ages = increment_all_ages(State#ss_state.ages),
%% Dead individual IDs never removed from map
```

Over time, map accumulates entries for removed individuals.

**Recommendation:** Clean ages map during replacement.

#### 3. Pending Evaluations Overflow

**File:** `src/neuroevolution_server.erl:631-639`

```erlang
%% If evaluators crash, RequestId stays in map forever
NewPending = maps:remove(RequestId, PendingEvaluations),
```

Timeout mechanism exists but doesn't clean up on crash.

**Recommendation:** Add periodic cleanup of stale pending entries.

#### 4. Dual Storage (Network + Genome)

**File:** `src/strategies/generational_strategy.erl:405`

```erlang
#individual{
    network = Network,    % Full compiled network
    genome = Genome,      % Full connection genes
}
```

Each individual stores BOTH representations. For 100 individuals with 200 neurons:
- Network storage: ~2KB each = 200KB
- Genome storage: ~1KB each = 100KB
- **Total: 300KB** (could be 200KB with lazy compilation)

**Recommendation:** Store genome only, compile to network on-demand.

#### 5. Process Dictionary Leak

**File:** `src/neuroevolution_server.erl:690-703`

```erlang
case get(distributed_eval_results) of
    undefined -> [];
    Results -> Results
end.
```

Process dictionary persists across calls, can accumulate.

**Recommendation:** Clear after each evaluation batch.

### Good Patterns Found

- Generation history limited to 50 entries ✓
- Archive size bounded (novelty strategy) ✓
- Batch evaluation with concurrency control ✓

---

## DEEP DIVE: NIF LANGUAGE COMPARISON

### Language Decision Matrix

| Factor | Rust | Zig | C |
|--------|------|-----|---|
| **BEAM Safety** | Excellent (panic catch) | Risk (no protection) | High risk (segfault) |
| **Performance** | 40-100x vs Erlang | Same | Same |
| **Compile Time** | 10-15s | 5-10s | 5-10s |
| **Ecosystem** | ndarray, matrixmultiply | C interop | enif API |
| **Maturity** | Battle-tested (Discord) | Emerging | Legacy |

### Verdict: STICK WITH RUST

**Key reasons:**
1. Rustler catches panics before they unwind to C (prevents BEAM crash)
2. Discord uses Rust NIFs for 11M concurrent users
3. Ownership model eliminates memory bugs
4. ndarray/matrixmultiply crates handle vectorization

**Zig not recommended:** No automatic panic protection - crashes can kill BEAM.

### Celeron J4105 Hardware Constraints

| Feature | Status | Implication |
|---------|--------|-------------|
| AVX/AVX2 | **NOT SUPPORTED** | Cannot use AVX instructions |
| SSE4.2 | Supported | 2x floats per instruction (128-bit) |
| Cores | 4 @ 1.5GHz | Limit parallelism expectations |
| L1 Cache | 32KB | Flat arrays critical for performance |

**CRITICAL:** Do NOT target AVX - will cause "illegal instruction" errors on Gemini Lake!

**Realistic speedups:**
- SSE4.2 vectorization: 2-3x
- Rust NIF vs Erlang: 30-100x
- Combined: 60-300x total improvement

---

## DEEP DIVE: ADDITIONAL NIF OPPORTUNITIES

### Priority-Ranked Candidates

| Rank | Function | Location | Current | Speedup | Impact |
|------|----------|----------|---------|---------|--------|
| 1 | **dot_product** | signal_aggregator.erl:79-90 | Erlang | 40-100x | **CRITICAL** |
| 2 | **calculate_distance** | species_identifier.erl:127-140 | Erlang | 30-50x | HIGH |
| 3 | evaluate (exists) | tweann_nif.erl | NIF | - | Activate |
| 4 | evaluate_batch (exists) | tweann_nif.erl | NIF | - | Activate |
| 5 | weight_update | plasticity.erl:141-150 | Erlang | 30-50x | DEFER |

### Missing: Dirty Scheduler Declarations

**Current issue:** Your NIFs lack dirty scheduler markers.

**Impact:**
- `evaluate/2` for 100+ neurons: 1-10ms (blocks regular scheduler!)
- `evaluate_batch/2`: likely >1ms always

**Fix (add to lib.rs):**
```rust
#[rustler::nif(schedule = "DirtyCpu")]
fn evaluate_batch(...) -> ... { ... }
```

### Highest Priority NIF: dot_product

**Why this matters most:**
- Called millions of times per generation
- Every neuron × every evaluation × every network
- Population × generations multiplier

**Rust implementation (~20 lines):**
```rust
#[rustler::nif]
fn dot_product(
    signals: Vec<Vec<f64>>,
    weights: Vec<Vec<(f64, f64, f64, Vec<f64>)>>,
) -> f64 {
    signals.iter().zip(weights.iter())
        .map(|(sigs, wts)| {
            sigs.iter().zip(wts.iter())
                .map(|(s, (w, _, _, _))| s * w)
                .sum::<f64>()
        })
        .sum()
}
```

**Expected improvement:** 50-100x for N=20 signal sources

---

## COMPREHENSIVE OPTIMIZATION ROADMAP (UPDATED)

### Phase 1: Quick Stability (1-2 days)
**Goal:** Stop memory leaks, prevent zombie processes

| Task | File | Change |
|------|------|--------|
| Neuron timeout exit | neuron.erl:159 | Exit after 3 timeouts |
| Reduce PopMon timeout | population_monitor.erl:276 | 60s → 5s |
| Synchronous termination | cortex.erl, exoself.erl | Monitor + wait |
| Catch-all receives | All process modules | Log/discard unexpected |

### Phase 2: Memory Architecture (1-2 weeks)
**Goal:** Bounded memory growth

| Task | Impact | Change |
|------|--------|--------|
| Replace Mnesia with ETS | Faster, simpler | 11 table migrations |
| Generation-based cleanup | Bounded growth | Delete old agents |
| Cap evo_hist | 90% reduction | Keep last 50 |
| Clear dead_pool | Prevent accumulation | Clear each gen |

### Phase 3: NIF Activation (1-2 weeks)
**Goal:** 10-50x speedup on hot paths

| Task | Files | Speedup |
|------|-------|---------|
| Add dirty scheduler to evaluate_batch | native/src/lib.rs | Prevents blocking |
| Implement dot_product NIF | signal_aggregator.erl, lib.rs | 40-100x |
| Implement calculate_distance NIF | species_identifier.erl, lib.rs | 30-50x |
| Uncomment rebar.config hooks | rebar.config | Enable build |
| Wire brain.erl to tweann_nif:evaluate | brain.erl | Activate existing |

### Phase 4: Erlang Representation (1-2 weeks)
**Goal:** Reduce GC pressure

| Task | Current | Optimized |
|------|---------|-----------|
| Flatten weight tuples | Nested lists | Parallel flat lists |
| Pre-compile weight matrices | Rebuild every cycle | Build once at link |
| Message batching | 600 msgs/cycle | 10-20 batched |

### Phase 5: Benchmarking (Ongoing)
**Goal:** Measure and validate

| Task | Tool | Output |
|------|------|--------|
| Create benchmark suite | EUnit | test/benchmark/ |
| Baseline current performance | bench_* modules | Metrics log |
| Track regressions | CI/CD | Alerts on regression |

### Phase 6: macula-neuroevolution Fixes (1 week)
**Goal:** Prevent memory issues in evolution layer

| Task | File | Change |
|------|------|--------|
| Validate MAP-Elites grid size | map_elites_strategy.erl:113 | Max 10,000 cells |
| Clean ages map on replacement | steady_state_strategy.erl:127 | Remove dead IDs |
| Periodic pending cleanup | neuroevolution_server.erl:631 | Timeout + cleanup |
| Clear process dictionary | neuroevolution_server.erl:690 | After each batch |

### Skip: Nx Integration
- JIT overhead prohibitive for NEAT
- Rust NIF provides same benefits

---

## SUCCESS METRICS (UPDATED)

### Current State (Estimated)
| Metric | Value |
|--------|-------|
| Memory (100 agents × 1000 gen) | 2-4 GB |
| Forward pass (50 neurons) | 60-100 µs |
| Generation time (100 agents) | ~60s |
| Stability | Zombie processes over hours |

### Target After Optimization
| Metric | Value | Improvement |
|--------|-------|-------------|
| Memory | 500 MB - 1 GB | 4x reduction |
| Forward pass (50 neurons) | 10-20 µs | 5-10x faster |
| Generation time | 10-15s | 4-6x faster |
| Stability | No leaks 24+ hours | ∞ |

### Safe Limits for beam00-03

| Node | RAM | Population | Generations | Network Size |
|------|-----|------------|-------------|--------------|
| beam00 | 16 GB | 100-150 | 1000+ | 300 neurons |
| beam01-03 | 32 GB | 200-300 | 2000+ | 500 neurons |

---

## NOTE: FILES TO MODIFY

### faber-tweann (Priority Order)

| File | Changes | Priority |
|------|---------|----------|
| `native/src/lib.rs` | Add dirty schedulers, dot_product NIF | CRITICAL |
| `rebar.config` | Uncomment NIF build hooks | CRITICAL |
| `src/signal_aggregator.erl` | Call NIF for dot_product | CRITICAL |
| `src/neuron.erl` | Fix timeout exit | HIGH |
| `src/genotype.erl` | Migrate Mnesia→ETS | HIGH |
| `src/innovation.erl` | Migrate to counters module | HIGH |
| `src/cortex.erl` | Synchronous termination | MEDIUM |
| `src/exoself.erl` | Race condition fix | MEDIUM |
| `test/benchmark/*.erl` | Create benchmark suite | MEDIUM |

### macula-neuroevolution (Priority Order)

| File | Changes | Priority |
|------|---------|----------|
| `src/strategies/map_elites_strategy.erl` | Grid size validation | HIGH |
| `src/strategies/steady_state_strategy.erl` | Ages map cleanup | MEDIUM |
| `src/neuroevolution_server.erl` | Pending cleanup, process dict | MEDIUM |

---

*Analysis updated 2025-12-07 with benchmarking, macula-neuroevolution analysis, and NIF deep dive*
