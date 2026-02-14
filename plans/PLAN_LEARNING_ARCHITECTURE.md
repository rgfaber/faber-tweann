# faber-tweann Learning Architecture Plan

**Date:** 2025-12-04
**Priority:** 1 (After neuroevolution)
**Status:** Design Complete, Awaiting Implementation

---

## Problem Statement

**Current State:** faber-tweann has NO true online learning
- Weight tuning in `exoself.erl` uses random perturbation, not gradients
- Plasticity fields in weight_spec `{Weight, DeltaWeight, LearningRate, ParameterList}` are defined but unused
- No Hebbian rules, no backpropagation, no reinforcement signals
- Two evaluation paths that don't share learning code:
  - `network_evaluator.erl` - Stateless synchronous evaluation
  - Process-per-neuron via `cortex.erl` + `neuron.erl` - Training path but no learning

**User Request:** "Right now, our snakes remain stupid" - they need to actually learn.

---

## Key Design Decisions (User Confirmed)

| Decision | User Choice | Notes |
|----------|-------------|-------|
| Learning Mode | **Both modes available** | Learn during gameplay AND between matches |
| Subsystem Priority | **Incremental evolution** | Add learning first, refactor into subsystems later |
| Brain Scope | **Global Brain Architecture** | Beyond gaming paradigm - needs extensive reflection |
| Economic Model | **Hybrid (combine options)** | Needs separate reflection with community input |

---

## Research Foundation

### Hebbian Learning Theory

**Core Principle:** "Neurons that fire together wire together"

```
Δw_ij = η × pre_i × post_j
```
Where:
- `Δw_ij` = change in weight from neuron i to j
- `η` = learning rate
- `pre_i` = presynaptic activity
- `post_j` = postsynaptic activity

### Continuous-Time Learning Options

| Approach | Complexity | Biological Basis | Performance |
|----------|------------|------------------|-------------|
| Static (current) | Minimal | None | Fast inference, no adaptation |
| Hebbian | Low | High | Local, unsupervised |
| STDP | Medium | Very High | Temporal, causal |
| Backpropagation | High | Low | Supervised, accurate |
| Reinforcement | Medium | Medium | Reward-driven |
| Modulated Hebbian | Medium | High | Reward-modulated local |

**Recommended:** Start with **Modulated Hebbian** - biologically grounded, local computation, compatible with distributed systems.

---

## Proposed Learning Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  MACULA-TWEANN LEARNING SUBSYSTEM                               │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Plasticity Engine (NEW)                                 │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │  │
│  │  │   Hebbian   │  │    STDP     │  │   Modulated    │  │  │
│  │  │   Rules     │  │   Rules     │  │    Hebbian     │  │  │
│  │  └──────┬──────┘  └──────┬──────┘  └───────┬────────┘  │  │
│  │         └────────────────┼─────────────────┘           │  │
│  │                          ▼                              │  │
│  │                  ┌──────────────┐                       │  │
│  │                  │  Weight      │                       │  │
│  │                  │  Updater     │                       │  │
│  │                  └──────────────┘                       │  │
│  └──────────────────────────────────────────────────────────┘  │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Inference Engine (network_evaluator.erl + brain.erl)   │  │
│  │  - Forward propagation (unchanged)                       │  │
│  │  - Activation tracking (for learning)                    │  │
│  │  - State persistence (LTC neurons)                       │  │
│  └──────────────────────────────────────────────────────────┘  │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Evolution Engine (genome_mutator.erl + genotype.erl)   │  │
│  │  - Topology mutation (unchanged)                         │  │
│  │  - Weight perturbation (unchanged)                       │  │
│  │  - Speciation (unchanged)                                │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

---

## New Modules Required

| Module | Purpose | Priority |
|--------|---------|----------|
| `src/plasticity.erl` | Plasticity rule interface/behavior | High |
| `src/plasticity_hebbian.erl` | Basic Hebbian rule | High |
| `src/plasticity_modulated.erl` | Reward-modulated Hebbian | Medium |
| `src/plasticity_stdp.erl` | STDP rule (temporal) | Low |
| `src/learning_engine.erl` | Coordinates learning across network | High |
| `src/reward_signal.erl` | Reward/punishment signal propagation | Medium |

---

## Implementation Phases

### Phase L1: Plasticity Foundation
1. Create `src/plasticity.erl` behavior
   ```erlang
   -callback apply_rule(Weight, PreActivity, PostActivity, Reward) -> NewWeight.
   -callback init(Params) -> State.
   -callback reset(State) -> State.
   ```
2. Implement `plasticity_hebbian.erl` (basic rule)
3. Add activation tracking to `brain.erl`
4. Unit tests for weight updates

### Phase L2: Learning Integration
1. Create `learning_engine.erl` to coordinate learning
2. Integrate with `brain.erl` for online learning mode
3. Add experience buffer for batch learning
4. Connect reward signal from game (fitness proxy)

### Phase L3: Reward Signal Architecture
1. Create `reward_signal.erl` for propagating rewards
2. Implement `plasticity_modulated.erl` (reward-modulated Hebbian)
3. Connect game events to reward signals:
   - Food eaten → positive reward
   - Death → strong negative reward
   - Kill opponent → moderate positive
   - Survival → small continuous positive

### Phase L4: Learning Persistence
1. Update `genotype.erl` to persist learned weights
2. Add "learning checkpoint" feature
3. Distinguish evolved weights from learned weights
4. Versioning for weight snapshots

### Phase L5: Evaluation & Metrics
1. Add learning metrics (avg weight change, convergence)
2. Visualization of weight changes over time
3. Compare learning snakes vs random snakes
4. Benchmark: learning speed vs gameplay FPS

---

## Success Criteria

1. ⬜ Snakes demonstrably improve during single game session
2. ⬜ Learned behaviors persist between matches
3. ⬜ Learning + inference runs at 60 FPS (game tick rate)
4. ⬜ Hebbian rule shows measurable weight adaptation
5. ⬜ Food-seeking behavior emerges from reward signal
6. ⬜ Educational docs explain learning theory

---

## Critical Files to Modify/Create

### New Files

| File | Purpose | Priority |
|------|---------|----------|
| `src/plasticity.erl` | Behavior defining plasticity rule interface | High |
| `src/plasticity_hebbian.erl` | Basic Hebbian learning rule | High |
| `src/plasticity_modulated.erl` | Reward-modulated Hebbian | Medium |
| `src/learning_engine.erl` | Coordinates learning across network | High |
| `src/reward_signal.erl` | Reward propagation from game events | Medium |
| `test/unit/plasticity_tests.erl` | Unit tests for plasticity rules | High |

### Files to Modify

| File | Changes | Priority |
|------|---------|----------|
| `src/brain.erl` | Add learning mode, experience buffer | High |
| `src/network_evaluator.erl` | Expose weights for modification | High |
| `src/genotype.erl` | Add weight update (not just topology) | Medium |
| `include/records.hrl` | Add learning-related fields | Medium |
