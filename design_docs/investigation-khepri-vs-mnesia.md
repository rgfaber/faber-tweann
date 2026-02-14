# Investigation: Khepri vs Mnesia for Faber TWEANN

**Date**: 2025-11-20
**Version**: 0.8.6
**Purpose**: Evaluate Khepri as potential replacement for Mnesia in genotype storage

---

## Executive Summary

**Recommendation**: **Keep Mnesia for now (v0.8.x - v1.0)**, investigate Khepri for **v2.0+**

**Rationale**:
- Khepri is still **Beta/Stable** (not production-proven yet)
- RabbitMQ only made it default in 4.2 (September 2024)
- Mnesia perfectly suits TWEANN's single-node use case
- No compelling performance/safety benefits for evolutionary workloads
- Migration would add complexity without clear value

**Monitor**: Khepri maturity for potential v2.0 adoption if distributed TWEANN becomes a goal.

---

## What is Khepri?

### Overview

Khepri is a **tree-like replicated on-disk database library for Erlang and Elixir** developed by the RabbitMQ team as a replacement for Mnesia.

- **Built on**: Ra (Erlang implementation of Raft consensus algorithm)
- **License**: Dual-licensed (Apache 2.0 + MPL 2.0)
- **Status**: Beta/Stable (as of RabbitMQ 4.2, September 2024)
- **Repository**: https://github.com/rabbitmq/khepri

### Key Features

1. **Raft Consensus**: Strong consistency with predictable partition behavior
2. **Tree Structure**: Hierarchical data organization (like filesystem paths)
3. **Stored Procedures**: Functions that execute automatically on data changes
4. **Triggers**: Event-driven data reactions
5. **Mnesia Migration Tool**: `khepri_mnesia_migration` for transitioning from Mnesia

### Architecture

```
Application
    ↓
Khepri API (transactions, put/get/delete)
    ↓
Ra (Raft consensus)
    ↓
Disk Storage
```

**Data Model**: Tree-based paths instead of tables
```erlang
%% Mnesia style
{agent, AgentId, Fitness, Generation, ...}

%% Khepri style
[agents, AgentId] -> #{fitness => ..., generation => ..., ...}
```

---

## Khepri vs Mnesia: Feature Comparison

| Feature | Mnesia | Khepri | Winner |
|---------|--------|--------|--------|
| **Bundled with Erlang** | ✅ Yes | ❌ No (external dep) | Mnesia |
| **Distributed Consensus** | ❌ No (old replication) | ✅ Yes (Raft) | Khepri |
| **Network Partition Recovery** | ⚠️ Difficult | ✅ Predictable | Khepri |
| **Single-Node Performance** | ✅ Excellent | ⚠️ Slower (Raft overhead) | Mnesia |
| **Multi-Node Performance** | ⚠️ Limited | ✅ Scales better | Khepri |
| **Transaction Model** | ✅ Familiar | ✅ Similar | Tie |
| **Query Language** | ✅ QLC (declarative) | ❌ Limited (path-based) | Mnesia |
| **Table Scans** | ✅ Efficient | ⚠️ Tree traversal | Mnesia |
| **Production Maturity** | ✅ 25+ years | ⚠️ <2 years | Mnesia |
| **Data Model** | ✅ Tables/Records | ⚠️ Tree/Maps | Mnesia (for TWEANN) |
| **Schema Evolution** | ✅ Well-understood | ❌ Unclear | Mnesia |

---

## Performance Benchmarks (RabbitMQ Workloads)

### RabbitMQ-Specific Results (High Concurrency)

| Scenario | Mnesia | Khepri (safe) | Winner |
|----------|--------|---------------|--------|
| 50 concurrent workers | 18,014 ops/s | 21,871 ops/s | **Khepri +21%** |
| 100 concurrent workers | 19,201 ops/s | 25,317 ops/s | **Khepri +32%** |
| 200 concurrent workers | ? | 25,851 ops/s | **Khepri** |
| No concurrency (single) | 5,631 ops/s | 567 ops/s | **Mnesia +900%** |

### Key Insights

1. **Khepri wins under high concurrency** (many workers writing simultaneously)
2. **Mnesia wins for single-threaded writes** (typical evolutionary workflow)
3. **Khepri better for durable entities** (persistent queues)
4. **Mnesia better for transient entities** (in-memory operations)

### RabbitMQ-Specific Improvements (Not Applicable to TWEANN)

| Operation | Mnesia | Khepri | Improvement |
|-----------|--------|--------|-------------|
| Start 1000 VHosts | 419s | 16s | **26x faster** |
| Rolling restart 1000 VHosts | 1447s | 106s | **14x faster** |
| Import 1000 Streams | 3.5s | 1.2s | **3x faster** |

**Note**: These are RabbitMQ metadata operations (virtual hosts, queues), not TWEANN genotype operations.

---

## TWEANN-Specific Analysis

### Current Mnesia Usage in TWEANN

**Modules**: `genotype.erl` (604 LOC)

**Operations**:
1. `init_db/0` - Create tables (agent, cortex, sensor, neuron, actuator)
2. `read/1`, `dirty_read/1` - Read genotype records
3. `write/1` - Write genotype records
4. `delete/1` - Delete genotypes
5. `construct_Agent/3` - Complex topology construction with transactions
6. `clone_Agent/1` - Deep copy genotypes

**Access Patterns**:
- **Single-node** - No distributed deployment
- **Low concurrency** - Typically 1 population monitor per node
- **Transaction-heavy** - Agent construction is atomic multi-table write
- **Full table scans** - Population fitness sorting
- **Schema-stable** - Records don't change often

### Would Khepri Help TWEANN?

#### Advantages ✅

1. **Better network partition handling** (if distributed in future)
2. **Raft consensus** (if multi-node evolution desired)
3. **Predictable cluster behavior** (if scaling horizontally)
4. **Modern architecture** (active development, RabbitMQ backing)

#### Disadvantages ❌

1. **Single-node TWEANN doesn't need Raft** (consensus overhead for no benefit)
2. **Mnesia faster for single-threaded writes** (typical evolution loop)
3. **Tree model awkward for relational genotypes** (agent -> cortex -> neurons)
4. **QLC queries lost** (Mnesia's declarative query language is valuable)
5. **External dependency** (Mnesia is built-in to Erlang)
6. **Migration effort** (rewrite genotype.erl, test thoroughly)
7. **Beta status** (production maturity concerns)

### TWEANN Workload Characteristics

| Characteristic | Favors Mnesia | Favors Khepri |
|----------------|---------------|---------------|
| Single-node deployment | ✅ | ❌ |
| Low write concurrency | ✅ | ❌ |
| Relational data model | ✅ | ❌ |
| Complex transactions | ✅ | 🟡 |
| Full table scans | ✅ | ❌ |
| Stability/maturity critical | ✅ | ❌ |

**Verdict**: Mnesia is a **better fit** for TWEANN's current architecture and workload.

---

## API Comparison

### Mnesia API (Current)

```erlang
%% Initialize
mnesia:create_schema([node()]),
mnesia:start(),
mnesia:create_table(agent, [
    {attributes, record_info(fields, agent)},
    {disc_copies, [node()]},
    {type, set}
]).

%% Read/Write
{ok, Agent} = mnesia:transaction(fun() ->
    mnesia:read({agent, AgentId})
end),

ok = mnesia:transaction(fun() ->
    mnesia:write(Agent#agent{fitness = NewFitness})
end).

%% Queries
Agents = mnesia:transaction(fun() ->
    mnesia:select(agent, [{#agent{fitness = '$1', _='_'},
                           [{'>', '$1', 0.5}],
                           ['$_']}])
end).
```

### Khepri API (Hypothetical)

```erlang
%% Initialize
khepri:start(),
khepri:create_cluster([node()]).

%% Read/Write
{ok, Agent} = khepri:get([agents, AgentId]),

ok = khepri:put([agents, AgentId], Agent#{fitness => NewFitness}).

%% Queries (harder)
{ok, AllAgents} = khepri:get_many([agents, '*']),
HighFitness = [A || A <- AllAgents, maps:get(fitness, A) > 0.5].
```

**Migration Effort**: Moderate (rewrite all `genotype.erl` operations)

---

## Migration Path Analysis

### If We Migrated to Khepri

**Effort Estimation**: 2-3 weeks
- Rewrite `genotype.erl` (~600 LOC)
- Convert records to maps (schema change)
- Rewrite transactions
- Update tests (~50 tests affected)
- Performance testing
- Documentation updates

**Risks**:
- **Beta software** in production library
- **Performance regression** for single-node use case
- **Breaking change** for users with existing Mnesia databases
- **Complex migration** for existing deployments
- **Tree model awkward** for relational genotypes

**Benefits**:
- Future-proof for distributed evolution
- Better partition handling (if needed)
- Active development and RabbitMQ backing

---

## Recommendations

### Short Term (v0.8.x - v1.0): Keep Mnesia ✅

**Reasons**:
1. **Stability**: Mnesia is battle-tested for 25+ years
2. **Performance**: Better for single-node, low-concurrency workload
3. **Simplicity**: No external dependencies
4. **Maturity**: Khepri still in early adoption phase
5. **Risk**: No compelling benefit justifies migration effort

**Action**: None required. Continue using Mnesia.

### Medium Term (v1.1 - v1.5): Monitor Khepri 👀

**Track**:
1. Khepri production adoption (RabbitMQ 4.3+ deployments)
2. Performance improvements (especially single-node writes)
3. API stabilization (query capabilities)
4. Community feedback (bug reports, issues)
5. Migration tool maturity (`khepri_mnesia_migration`)

**Trigger for Re-evaluation**:
- Distributed TWEANN becomes a roadmap item
- Multiple users request multi-node evolution
- Khepri reaches 2+ years production maturity
- Performance benchmarks show clear benefit for TWEANN workload

### Long Term (v2.0+): Consider Khepri 🔮

**If** distributed evolution becomes a goal, Khepri offers:
- **Raft consensus** for coordinated multi-node evolution
- **Partition tolerance** for geo-distributed populations
- **Horizontal scaling** for massive populations

**Migration Strategy** (if pursued):
1. Create abstraction layer (`storage_backend` behavior)
2. Implement both `mnesia_backend` and `khepri_backend`
3. Support both in parallel (user choice)
4. Provide migration tool
5. Default to Khepri in v2.0, deprecate Mnesia in v3.0

---

## Alternative: Storage Abstraction

Instead of migrating, we could **abstract storage backend**:

```erlang
%% storage_backend.erl
-callback init() -> ok.
-callback read(term()) -> {ok, term()} | {error, term()}.
-callback write(term()) -> ok | {error, term()}.
-callback delete(term()) -> ok | {error, term()}.
-callback transaction(fun()) -> {atomic, term()} | {aborted, term()}.

%% genotype.erl
-behavior(storage_backend).

%% Configuration (rebar.config or app.config)
{tweann, [
    {storage_backend, mnesia}  %% or khepri
]}.
```

**Benefits**:
- User choice between Mnesia and Khepri
- Easy to benchmark both
- Future-proof for other backends (SQLite, Postgres, etc.)
- No breaking changes

**Effort**: 1-2 weeks (behavior + both implementations)

---

## Khepri Limitations for TWEANN

### Architectural Mismatches

1. **Tree vs Relational**
   - Khepri is path-based: `[agents, AgentId, cortex, neurons, N1]`
   - TWEANN is relational: `agent -> cortex -> neurons -> actuators`
   - Awkward mapping between models

2. **Query Limitations**
   - Mnesia: Declarative QLC queries, pattern matching
   - Khepri: Path-based get_many, manual filtering
   - Population fitness ranking harder in Khepri

3. **Transaction Model**
   - Mnesia: Multi-table transactions, record locking
   - Khepri: Tree-path transactions, less granular

4. **Schema Evolution**
   - Mnesia: Well-documented migration patterns
   - Khepri: Unclear (new technology)

### Non-Issues (Khepri Handles Well)

1. **Transactions**: Both support atomic operations
2. **Durability**: Both persist to disk
3. **API similarity**: Both use functional transaction style

---

## Decision Matrix

| Factor | Weight | Mnesia Score | Khepri Score | Weighted |
|--------|--------|--------------|--------------|----------|
| **Single-node performance** | 30% | 9/10 | 5/10 | Mnesia +1.2 |
| **Production maturity** | 25% | 10/10 | 4/10 | Mnesia +1.5 |
| **Data model fit** | 20% | 9/10 | 5/10 | Mnesia +0.8 |
| **Future distributed support** | 15% | 3/10 | 9/10 | Khepri +0.9 |
| **No external deps** | 10% | 10/10 | 0/10 | Mnesia +1.0 |
| **Total** | 100% | **8.4** | **5.0** | **Mnesia Wins** |

---

## Conclusion

**Keep Mnesia for v0.8.x through v1.0.**

Khepri is an impressive technology with strong RabbitMQ backing, but it solves problems TWEANN doesn't currently have (distributed consensus, network partitions, horizontal scaling). Mnesia is:
- Faster for single-node operations
- More mature (25+ years production)
- Better suited to TWEANN's relational data model
- Zero external dependencies
- Well-understood schema evolution

**Revisit in v2.0** if distributed TWEANN becomes a roadmap item. At that point, Khepri will have 2+ years production maturity and clear benefits for multi-node evolution.

**Alternative**: Consider storage backend abstraction to support both Mnesia and Khepri as user choice.

---

## References

- Khepri GitHub: https://github.com/rabbitmq/khepri
- Khepri Docs: https://rabbitmq.github.io/khepri/
- RabbitMQ Khepri Roadmap: https://www.rabbitmq.com/blog/2025/09/01/6-khepri-default
- Khepri Benchmarks: https://github.com/rabbitmq/khepri-benchmark
- CloudAMQP Mnesia to Khepri Series:
  - Part 1: https://www.cloudamqp.com/blog/from-mnesia-to-khepri-part1.html
  - Part 2: https://www.cloudamqp.com/blog/from-mnesia-to-khepri-part2.html

---

## Appendix: Khepri API Examples

### Basic Operations

```erlang
%% Put data
ok = khepri:put([stock, wood, <<"lime tree">>], 150).

%% Get data
{ok, 150} = khepri:get([stock, wood, <<"lime tree">>]).

%% Check existence
true = khepri:exists([stock, wood, <<"lime tree">>]).

%% Delete data
ok = khepri:delete([stock, wood, <<"lime tree">>]).
```

### Transactions

```erlang
%% Transaction with khepri_tx
ok = khepri:transaction(fun() ->
    {ok, Value} = khepri_tx:get([path, to, key]),
    khepri_tx:put([path, to, key], Value + 1)
end).
```

### Advanced Operations

```erlang
%% Stored procedure (runs on data changes)
ok = khepri_adv:put([path, to, key], Value, #{
    on_action => fun(OldValue, NewValue) ->
        io:format("Changed from ~p to ~p~n", [OldValue, NewValue])
    end
}).

%% Get many (pattern matching)
{ok, Results} = khepri:get_many([stock, '_', '_']).
```

---

## Appendix: Migration Complexity Estimation

### Code Changes Required

| File | LOC | Changes | Effort |
|------|-----|---------|--------|
| `genotype.erl` | 604 | Complete rewrite | 5 days |
| `records.hrl` | 150 | Convert to maps | 1 day |
| Tests | 500 | Update all genotype tests | 3 days |
| Documentation | 100 | Update guides | 1 day |
| **Total** | | | **10 days** |

### Schema Migration

**From**: Mnesia tables (agent, cortex, neuron, sensor, actuator)
**To**: Khepri paths ([agents, AgentId], [neurons, NeuronId])

**Effort**: 2-3 days to write migration script + testing

### Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Performance regression | High | High | Benchmark before/after |
| Data loss during migration | Medium | Critical | Backup + validation |
| Khepri bugs | Medium | High | Extensive testing |
| User migration issues | High | Medium | Thorough documentation |
| Breaking changes | Certain | High | Major version bump |

**Overall Risk**: **HIGH** - Not justified by benefits for current use case.
