# faber-tweann Roadmap

What this package intends to implement but does not yet.

`README.md` states what **is**. This file states what **will be**. A capability
moves from here into the README when it lands, accompanied by a test that
exercises it, and for performance claims by committed benchmark output.

Nothing here may be described as a feature in the README, the guides, EDoc
comments, or the hex package description until it moves.

Ordering is not arbitrary. It follows the chapter dependency chain in Gene
Sher's *Handbook of Neuroevolution Through Erlang*, because that book is the
reference implementation for the DXNN2 architecture this package ports. See
`faber-ecosystem/docs/CONFORMANCE.md` for the module-by-module gap analysis and
`faber-ecosystem/plans/PLAN_FABER_FOUNDATION.md` for the full plan.

---

## 1. The scape and the fitness channel (Handbook Ch 7) — DONE

**Status:** implemented (insights 008, 009). `xor_sim` only; other scapes and
recurrent networks remain.

`scape.erl` and `xor_sim.erl` exist. The fitness channel runs end to end:
scape → actuator → cortex → exoself, with `goal_reached` propagation. The
exoself spawns scapes, sends `{exoself_terminated, Fitness}`, and terminates.
A single agent runs to completion through the process-per-neuron path and
produces real fitness (insight 009), and the population_monitor drives a full
generation of such evaluations (insight 010).

Still open under this heading:
- Only `xor_sim` exists. `pb_sim` (Handbook Ch 14), `fx_sim` (Ch 19), `flatland`
  (Ch 18) and `snake_duel` remain, referenced by their morphologies.
- Recurrent networks are not supported: a recurrent neuron waits on a feedback
  input that nothing produces on the first cycle. Only feedforward works.
- Multi-generation evolution is blocked; see 2b.

The message protocol is specified in `faber-ecosystem/docs/PROTOCOL.md`.

## 2. The memetic tuning layer (Handbook Ch 10)

**Status:** configured but never invoked.

DXNN2's distinguishing property over NEAT is that it interleaves a stochastic
hill-climber over synaptic weights with topological evolution. Three modules
drive it, and none exist here:

| Module | Role | Called from, in DXNN2 |
|---|---|---|
| `tuning_selection` | chooses which neurons to perturb | `exoself:219`, `genome_mutator:218,262` |
| `tuning_duration` | computes `max_attempts` per agent per generation | `exoself:137` |
| `tot_topological_mutations` | how many topological mutations to apply | `genome_mutator:71` |

`genotype.erl:212,214,217` assigns all three into the agent record and
`exoself.erl:71-72,147-163` carries them in state. Nothing ever calls them.
`exoself.erl:69` hardcodes `max_attempts = 15` where DXNN2 computes it.

Until this lands, faber-tweann is a topology-and-weight evolver but not DXNN.

## 2b. Genotype lifecycle integrity across generations

**Status:** blocks evolution through the process-per-neuron path (insight 010).

A single evaluation runs end to end (insight 009) and the population_monitor
drives one full generation of Sher-path evaluations. Multi-generation
evolution crashes: exoself:link_neurons hits a badmatch (a neuron references an
id absent from the process map) and neurons time out on missing inputs.

Bisected by data-only integrity probes: fresh construction is clean, a single
clone+mutation is clean, only the repeated clone/mutate/delete lifecycle
breaks. The fault is in the interaction of reproduce_population (clone
survivors) with cleanup_agents (delete non-survivors), or in mutation
operators adding connections the feedforward evaluator cannot order.

Until this is fixed, evolution cannot solve XOR through this path, and the
comparison against the domain_sdk control (insight 004, median 550
evaluations) cannot be made.

## 3. Mnesia genotype storage (Handbook Ch 8.4.1)

**Status:** not implemented. Storage is ETS, in-memory, lost on VM exit.

`genotype:init_db/0` creates ETS tables. There is no `mnesia:` call anywhere in
`src/`. `genotype.erl:166` notes that part of the API "exists for API
compatibility with the old Mnesia interface", and `rebar.config` records
"mnesia removed - now using ETS for genotype storage".

The README and `CLAUDE.md` claimed Mnesia persistence; those claims are being
corrected to describe ETS, and the capability is recorded here instead.

Intended: Mnesia with `disc_copies`, **single node only**. Not Mnesia
clustering across nodes, which would compete with the genome-over-mesh model
used downstream. This also resolves a live split-brain: `faber-neuroevolution`'s
`lc_chain.erl:398-450` reads genotypes expecting Mnesia while this package
stores them in ETS.

Cleanup that comes with it: remove the unused `mnesia` entry from
`faber_tweann.app.src`'s applications list and the vestigial
`{error, {mnesia_error, term()}}` type in `network_compiler.erl:43`.

## 4. Oja's rule (Handbook Ch 15.2)

**Status:** native half exists, Erlang half does not.

`native/faber_nn_nifs/src/lib.rs` exports `oja_update_batch/4`, but there is no
`plasticity_oja` module implementing the `plasticity` behaviour, so it is
unreachable. Present implementations are `plasticity_hebbian`,
`plasticity_modulated` and `plasticity_none`.

## 5. Substrate encoding and HyperNEAT (Handbook Ch 16, Ch 17)

**Status:** not implemented. `genotype.erl:271` raises
`substrate_not_implemented`.

This is the largest single item. Chapter 16 is 73 printed pages, roughly 42 of
listings, of which `substrate.erl` alone is a 19-page listing. Chapter 17 adds
18 pages.

New modules: `substrate`, `substrate_cpp` (coordinate pre-processor),
`substrate_cep` (connectivity expression producer).
Modified: `genotype`, `exoself`, `genome_mutator`, `morphology`, `records.hrl`.

A `#substrate{}` record was scaffolded but nothing implements it. It needs
`id, agent_id, densities, linkform, plasticity, cpp_ids, cep_ids`, plus a
`substrate_id` field on `#agent{}`.

Design note from the book, worth honouring: do **not** create separate
`#substrate_cpp{}` / `#substrate_cep{}` records. Sher explicitly rejects that
and instead adds a `type` field (`neural | substrate_cpp | substrate_cep`) to
the existing `#sensor{}` and `#actuator{}` records, so that every mutation
operator and linking function does not have to be duplicated.

Ch 16 can be built with `plasticity = none`; it does not hard-depend on Ch 15.
Ch 17 (abcn and iterative substrate plasticity) requires Ch 16.

When this lands, the "Substrate Networks" and "HyperNEAT via CPPNs" claims may
return to the faber-ecosystem README. Not before.

## 6. Measured SIMD

**Status:** claimed in the past, never implemented, claims withdrawn in v2.0.0.

`native/faber_nn_nifs/src/lib.rs` contains zero SIMD: no `std::arch`, no
`target_feature`, no `rayon`, no `par_iter`. The former "SIMD Batch
Activations" heading described plain sequential `map` calls. The only
concurrency mechanism is rustler's `DirtyCpu` scheduler flag.

Intended: real vectorisation on the dense/layered path. The sparse
topological-order graph walk in `CompiledNetwork::evaluate` will not vectorise
usefully and should be left scalar.

Prerequisite: a benchmark harness whose output is committed.
`test/benchmark/bench_nif_vs_erlang.erl` exists but no recorded numbers do.
No speedup figure may be published without committed output naming its
execution path via `tweann_nif:impl/0`.

## 7. Instrumentation (Handbook Ch 12, Ch 13)

**Status:** not implemented.

`#trace{}` and `#stat{}` records exist in `records.hrl` but no code emits them.
`benchmarker`, `polis` and `trainer` do not exist.

Without `benchmarker` and the `goal_reached` signal from item 1, average
evaluations-to-solve cannot be measured, and therefore no comparison against
the published pole-balancing literature is possible.

---

## Not planned

- **Mnesia clustering across nodes.** Single-node `disc_copies` only. Genome
  distribution belongs to the mesh layer, not to the storage engine.
- **Prebuilt NIF artifacts.** NIFs are built from source. Shipping prebuilt
  shared objects couples the package to a specific glibc.
- **A silent NIF fallback.** Removed in v2.0.0. Implementation is selected
  explicitly and a missing library raises. Two implementations silently
  disagreed for months because the native path was never exercised.
