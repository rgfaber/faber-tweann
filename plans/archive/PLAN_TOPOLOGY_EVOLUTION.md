# Topology Evolution Plan - faber-tweann

**Date:** 2025-12-07
**Priority:** 1
**Status:** Complete (All Phases T1, T2, T3 Implemented)
**Related:** macula-neuroevolution/plans/PLAN_TOPOLOGY_INTEGRATION.md

---

## Overview

Complete the topology evolution capabilities in faber-tweann to enable full TWEANN (Topology and Weight Evolving Artificial Neural Networks) functionality. This includes implementing missing mutation operators and adding innovation number tracking for NEAT-style crossover.

---

## Current State

### Implemented Topological Mutations ✅

| Operator | Probability | Status |
|----------|-------------|--------|
| `add_bias/1` | 10 | Complete |
| `add_outlink/1` | 40 | Complete |
| `add_inlink/1` | 40 | Complete |
| `add_neuron/1` | 40 | Complete |
| `outsplice/1` | 40 | Complete |
| `add_sensorlink/1` | 1 | Complete |
| `add_actuatorlink/1` | 1 | Complete |

### Newly Implemented ✅ (Phase T1)

| Operator | Priority | Status |
|----------|----------|--------|
| `add_sensor/1` | High | Complete |
| `add_actuator/1` | High | Complete |

### Not Implemented ❌

| Feature | Priority | Purpose |
|---------|----------|---------|
| Innovation Numbers | High | Required for NEAT-style variable-topology crossover |

---

## Phase T1: Add Sensor/Actuator Mutations

**Goal:** Enable networks to evolve their physical I/O capabilities

### Why This Matters

In robotics and embodied AI, the ability to evolve sensors/actuators is crucial:
- **Sensor Evolution**: A robot might discover it needs proximity sensors, or that additional visual inputs help
- **Actuator Evolution**: A robot might evolve from simple locomotion to manipulation capabilities
- **Morphological Plasticity**: The NN can adapt its interface to the environment

### Implementation

#### T1.1: Add Sensor Mutation

**File:** `src/topological_mutations.erl`

```erlang
%% @doc Add a new sensor to the network.
%%
%% Selects a sensor type from the morphology's available sensors
%% that isn't already in the network, creates it, and connects
%% it to a random neuron.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_sensor(term()) -> ok | {error, term()}.
add_sensor(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    Constraint = Agent#agent.constraint,

    %% Get available sensor types from morphology
    AvailableSensors = morphology:get_sensors(Constraint#constraint.morphology),

    %% Filter out sensors already in the network
    CurrentSensorTypes = get_current_sensor_types(Cortex),
    CandidateSensors = AvailableSensors -- CurrentSensorTypes,

    add_sensor_from_candidates(Agent, Cortex, CandidateSensors).
```

**Key considerations:**
1. Query morphology for available sensor types
2. Check which sensors already exist
3. Create new sensor with appropriate VL (vector length)
4. Connect to random neuron(s)
5. Update cortex sensor_ids list

#### T1.2: Add Actuator Mutation

**File:** `src/topological_mutations.erl`

```erlang
%% @doc Add a new actuator to the network.
%%
%% Selects an actuator type from the morphology's available actuators
%% that isn't already in the network, creates it, and connects
%% random neuron(s) to it.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}.
-spec add_actuator(term()) -> ok | {error, term()}.
add_actuator(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    Constraint = Agent#agent.constraint,

    %% Get available actuator types from morphology
    AvailableActuators = morphology:get_actuators(Constraint#constraint.morphology),

    %% Filter out actuators already in the network
    CurrentActuatorTypes = get_current_actuator_types(Cortex),
    CandidateActuators = AvailableActuators -- CurrentActuatorTypes,

    add_actuator_from_candidates(Agent, Cortex, CandidateActuators).
```

#### T1.3: Morphology Extensions

**File:** `src/morphology.erl`

Add functions to query available sensors/actuators:

```erlang
%% @doc Get all available sensor specifications for a morphology.
%% Returns list of {SensorName, VL} tuples.
-spec get_sensors(atom()) -> [{atom(), pos_integer()}].
get_sensors(Morphology) ->
    case Morphology:sensors() of
        Sensors when is_list(Sensors) -> Sensors;
        _ -> []
    end.

%% @doc Get all available actuator specifications for a morphology.
%% Returns list of {ActuatorName, VL} tuples.
-spec get_actuators(atom()) -> [{atom(), pos_integer()}].
get_actuators(Morphology) ->
    case Morphology:actuators() of
        Actuators when is_list(Actuators) -> Actuators;
        _ -> []
    end.
```

#### T1.4: Tests

**File:** `test/unit/topological_mutations_tests.erl`

Add tests:
- `add_sensor_success_test/0`
- `add_sensor_no_available_sensors_test/0`
- `add_actuator_success_test/0`
- `add_actuator_no_available_actuators_test/0`

### Deliverables

- [x] `add_sensor/1` implementation
- [x] `add_actuator/1` implementation
- [x] Morphology query functions (already existed as `morphology:get_Sensors/1` and `morphology:get_Actuators/1`)
- [x] Unit tests (4 tests: add_sensor_success_test, add_sensor_no_available_test, add_actuator_success_test, add_actuator_no_available_test)
- [ ] Update mutation_operators documentation

---

## Phase T2: Innovation Number Tracking

**Goal:** Enable NEAT-style crossover between variable topologies

### Why Innovation Numbers Matter

Without innovation numbers, crossing networks with different topologies is ambiguous:
- Which connections correspond between parents?
- How to align neurons that evolved independently?

Innovation numbers solve this by giving each structural change a unique historical marker.

### Implementation

#### T2.1: Innovation Number Generator

**File:** `src/innovation.erl` (NEW)

```erlang
-module(innovation).

-export([
    init/0,
    next_innovation/0,
    get_or_create_link_innovation/2,
    get_or_create_node_innovation/1
]).

%% Innovation numbers are stored in Mnesia for persistence
-record(innovation_counter, {
    id = counter,
    value = 0
}).

-record(link_innovation, {
    key,           % {FromId, ToId}
    innovation     % Innovation number
}).

-record(node_innovation, {
    key,           % {SplitLinkInnovation}
    innovation,    % Innovation number for the new node
    in_innovation, % Innovation for input link to new node
    out_innovation % Innovation for output link from new node
}).

%% @doc Get or create innovation number for a link.
%% Same link (from->to) always gets same innovation number.
-spec get_or_create_link_innovation(term(), term()) -> pos_integer().
get_or_create_link_innovation(FromId, ToId) ->
    Key = {FromId, ToId},
    case mnesia:dirty_read(link_innovation, Key) of
        [#link_innovation{innovation = Inn}] -> Inn;
        [] ->
            Inn = next_innovation(),
            mnesia:dirty_write(#link_innovation{key = Key, innovation = Inn}),
            Inn
    end.
```

#### T2.2: Connection Gene Record

**File:** `include/records.hrl`

Add connection gene representation:

```erlang
%% @doc Connection gene for NEAT-style evolution
%% Each connection has a unique innovation number for crossover alignment
-record(connection_gene, {
    innovation :: pos_integer(),      % Historical marker
    from_id :: term(),                % Source node ID
    to_id :: term(),                  % Target node ID
    weight :: float(),                % Connection weight
    enabled = true :: boolean()       % Can be disabled without deletion
}).
```

#### T2.3: Neuron Record Extension

**File:** `include/records.hrl`

Add innovation tracking to neurons:

```erlang
-record(neuron, {
    %% ... existing fields ...

    %% Innovation tracking (for NEAT crossover)
    innovation :: pos_integer() | undefined,

    %% Connection genes (alternative to input_idps for NEAT mode)
    connection_genes = [] :: [#connection_gene{}]
}).
```

#### T2.4: Innovation-Aware Mutations

Update topological mutations to record innovations:

```erlang
%% In add_neuron/1:
insert_neuron_with_innovation(Agent, Cortex, FromId, ToId, Weight) ->
    %% Get innovation for the split
    SplitInnovation = innovation:get_or_create_node_innovation({FromId, ToId}),

    NewNeuronId = genotype:generate_id(neuron),
    NewNeuron = #neuron{
        id = NewNeuronId,
        innovation = SplitInnovation#node_innovation.innovation,
        %% ... rest of neuron setup ...
    },
    %% ...
```

### Deliverables

- [x] `src/innovation.erl` module
- [x] Mnesia tables for innovation tracking (innovation_counter, link_innovation, node_innovation)
- [x] `#connection_gene{}` record in records.hrl
- [x] Innovation fields added to neuron, sensor, actuator records
- [x] Updated topological mutations (add_neuron, add_sensor, add_actuator)
- [x] Unit tests for innovation tracking (18 tests in innovation_tests.erl)

---

## Phase T3: Variable Topology Crossover

**Goal:** Enable meaningful crossover between structurally different networks

### Implementation

#### T3.1: Genome Alignment

**File:** `src/genome_crossover.erl` (NEW)

```erlang
-module(genome_crossover).

-export([
    crossover/3,
    align_genomes/2,
    compatibility_distance/3
]).

%% @doc Align two genomes by innovation number.
%% Returns {Matching, Disjoint1, Disjoint2, Excess1, Excess2}
-spec align_genomes(Genome1, Genome2) -> AlignmentResult when
    Genome1 :: [#connection_gene{}],
    Genome2 :: [#connection_gene{}],
    AlignmentResult :: {Matching, Disjoint1, Disjoint2, Excess1, Excess2},
    Matching :: [{#connection_gene{}, #connection_gene{}}],
    Disjoint1 :: [#connection_gene{}],
    Disjoint2 :: [#connection_gene{}],
    Excess1 :: [#connection_gene{}],
    Excess2 :: [#connection_gene{}].
```

#### T3.2: NEAT Crossover

```erlang
%% @doc Crossover two genomes using NEAT alignment.
%% Matching genes: randomly inherit from either parent
%% Disjoint/Excess genes: inherit from fitter parent
-spec crossover(Genome1, Genome2, FitterParent) -> ChildGenome when
    Genome1 :: [#connection_gene{}],
    Genome2 :: [#connection_gene{}],
    FitterParent :: 1 | 2,
    ChildGenome :: [#connection_gene{}].
crossover(Genome1, Genome2, FitterParent) ->
    {Matching, Disjoint1, Disjoint2, Excess1, Excess2} =
        align_genomes(Genome1, Genome2),

    %% Matching genes: random selection
    MatchingGenes = [random_select(G1, G2) || {G1, G2} <- Matching],

    %% Disjoint/Excess: from fitter parent
    ExtraGenes = case FitterParent of
        1 -> Disjoint1 ++ Excess1;
        2 -> Disjoint2 ++ Excess2
    end,

    MatchingGenes ++ ExtraGenes.
```

#### T3.3: Compatibility Distance

```erlang
%% @doc Calculate compatibility distance between two genomes.
%% Used for speciation decisions.
-spec compatibility_distance(Genome1, Genome2, Config) -> float() when
    Genome1 :: [#connection_gene{}],
    Genome2 :: [#connection_gene{}],
    Config :: #compatibility_config{}.
compatibility_distance(Genome1, Genome2, Config) ->
    {Matching, Disjoint1, Disjoint2, Excess1, Excess2} =
        align_genomes(Genome1, Genome2),

    N = max(length(Genome1), length(Genome2)),
    N_normalized = max(N, 1),  % Avoid division by zero

    ExcessCount = length(Excess1) + length(Excess2),
    DisjointCount = length(Disjoint1) + length(Disjoint2),

    AvgWeightDiff = case Matching of
        [] -> 0.0;
        _ ->
            Diffs = [abs(G1#connection_gene.weight - G2#connection_gene.weight)
                     || {G1, G2} <- Matching],
            lists:sum(Diffs) / length(Diffs)
    end,

    Config#compatibility_config.c1 * ExcessCount / N_normalized +
    Config#compatibility_config.c2 * DisjointCount / N_normalized +
    Config#compatibility_config.c3 * AvgWeightDiff.
```

### Deliverables

- [x] `src/genome_crossover.erl` module
- [x] Genome alignment by innovation (align_genomes/2)
- [x] NEAT-style crossover (crossover/3)
- [x] Compatibility distance calculation (compatibility_distance/3)
- [x] Configuration record for coefficients (#compatibility_config{})
- [x] Unit tests (14 tests in genome_crossover_tests.erl)
- [x] extract_connection_genes/1 to convert existing networks

---

## Success Criteria

1. [x] `add_sensor/1` and `add_actuator/1` work correctly
2. [x] Innovation numbers persist across runs (Mnesia tables)
3. [x] Same structural change gets same innovation
4. [x] Crossover produces valid offspring from different topologies
5. [x] Compatibility distance correctly measures structural difference
6. [x] All existing 559+ tests still pass (now 593 tests)
7. [x] New tests for all new functionality

---

## Dependencies

- **Mnesia** - Already used, extend for innovation tables
- **morphology.erl** - May need extensions for sensor/actuator queries

---

## Files to Create

| File | Purpose |
|------|---------|
| `src/innovation.erl` | Innovation number management |
| `src/genome_crossover.erl` | NEAT-style crossover |
| `test/unit/innovation_tests.erl` | Innovation tracking tests |
| `test/unit/genome_crossover_tests.erl` | Crossover tests |

## Files to Modify

| File | Changes |
|------|---------|
| `src/topological_mutations.erl` | Implement add_sensor, add_actuator |
| `src/morphology.erl` | Add get_sensors, get_actuators |
| `src/mutation_helpers.erl` | Helper functions for sensor/actuator |
| `include/records.hrl` | Add connection_gene, compatibility_config |
| `src/genotype.erl` | Initialize innovation tables |

---

## Estimated Effort

| Phase | Complexity | Dependencies |
|-------|------------|--------------|
| T1: Sensor/Actuator | Medium | Morphology understanding |
| T2: Innovation Numbers | Medium | Mnesia tables |
| T3: Variable Crossover | High | Innovation tracking complete |

---

## References

- Stanley, K.O. & Miikkulainen, R. (2002). "Evolving Neural Networks through Augmenting Topologies." *Evolutionary Computation*, 10(2), 99-127.
- Sher, G.I. (2013). *Handbook of Neuroevolution Through Erlang*. Springer.
