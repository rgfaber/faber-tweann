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
- `xor_sim` and `pb_sim` exist. `pb_sim` (Handbook Ch 14) is a faithful port of
  the cart-pole physics with four variants (single/double pole x with/without
  velocity); `pb_1_with_velocity` solves through the DXNN path (insight 015).
  `fx_sim` (Ch 19), `flatland` (Ch 18) and `snake_duel` remain, referenced by
  their morphologies.
- Recurrent networks are supported. A recurrent (feedback) edge's source seeds
  its target with a default [0.0] at link time (faithful to DXNN2 neuron:prep/1),
  so the target does not deadlock on the first cycle; from cycle 1 the real
  feedback flows. Recurrence is derived at phenotype-build time by partitioning
  each neuron's outputs by layer (constructor and exoself), so it is robust to
  mutations that do not maintain the stale ro_ids cache. Proven in
  recurrent_neuron_tests (a self-recurrent neuron carries state across cycles)
  and recurrent_evolution_tests. This unblocks the WITHOUT-velocity pole variants
  (non-Markov, need memory) and the LTC/CfC comparison. Deferred: a flush/reseed
  handshake between memetic tuning attempts (attempts 2+ currently start from the
  previous attempt's final recurrent state rather than a fresh [0.0]; harmless,
  non-deadlocking, but not yet a clean reset).
- Multi-generation evolution works and solves XOR and pole; see 2b, 2c and
  insight 015.

The message protocol is specified in `faber-ecosystem/docs/PROTOCOL.md`.

## 2. The memetic tuning layer (Handbook Ch 10)

**Status:** `tuning_selection` + `tuning_duration` DONE (insight 014).
`tot_topological_mutations` still fixed.

DXNN2's distinguishing property over NEAT is that it interleaves a stochastic
hill-climber over synaptic weights with topological evolution. Three modules
drive it:

| Module | Role | State |
|---|---|---|
| `tuning_selection` | chooses which neurons to perturb | **built** — `src/tuning_selection.erl` (dynamic / dynamic_random / active / current / all), invoked from `exoself:perturb_weights/1` |
| `tuning_duration` | computes `max_attempts` per agent per generation | **built** — `src/tuning_duration.erl` (const / wsize_proportional), invoked from `exoself:compute_max_attempts/1` |
| `tot_topological_mutations` | how many topological mutations to apply | still a fixed count |

Both modules are faithful ports of DXNN2 and are now wired into `exoself`. The
genotype default is DXNN2-faithful (`dynamic_random` + `wsize_proportional`).

**Surprise (insight 014):** the DXNN2-faithful shallow tuner does NOT solve XOR
at a 30-agent / 50-generation budget — it plateaus at fitness ~1.2 while the
crude deep tuner (`all` + `const 60`) solves 3/3. XOR is a small precise-weight
problem where deep per-agent hill-climbing dominates and shallow-subset tuning
is starved. The tuner's real payoff is expected on larger problems (pole
balancing), which is the next measurement. `xor_evolves_tests` therefore pins
the deep config explicitly; the default stays DXNN2-faithful.

## 2b. Genotype lifecycle integrity across generations — DONE

**Status:** fixed (insight 011). Evolution runs 150+ generations, fitness
climbs monotonically. What remains is search quality, item 2c.

Five coupled bugs, all found by bisecting from a running population: spliced
neurons all landing on layer 0; link mutations ignoring feedforward direction;
`add_bias` crashing `link_neurons` (bias resolved as a pid); a crashing agent
cascading through `spawn_link` to kill the run; and the memetic weight tuner
being entirely dead (neurons ignored `perturb`/`backup`/`restore`). All fixed.


## 2c. Solve XOR through the DXNN path — DONE

**Status:** solved (insight 012). 4/4 runs solve at generations 12-22.

The 011 plateau (RMSE 0.36) was a tuning-depth limit, not topology: raising the
memetic hill-climber's attempts from 15 to 60 lets selection's good topologies
tune to completion, and XOR falls. test/integration/xor_evolves_tests.erl is
the permanent proof.

Remaining under this heading, deferred:
- max_attempts is a fixed default (60), not computed per agent. DXNN2 derives
  it via tuning_duration (item 2).
- Recurrent networks still lack first-cycle seeding; feedforward only.
- The insight-004 control comparison (evaluations-to-solve, DXNN path vs
  domain_sdk weight-only) is now unblocked and is the next real measurement.

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

## 8. A genotype that can leave the machine — 8a and 8b DONE, 8c open

**Status:** not implemented, and it is the gap that decides whether topology
evolution is usable by anything outside a single VM.

⚠ This section contains one **defect** alongside two capabilities. Recording a
defect here does not make it a feature; it is here because it is the thing that
must be fixed before either capability is worth building.

Topology evolution itself works. `genome_mutator` dispatches nine topological
operators, four LTC and three parametric, over a real genotype graph, with
crossover, innovation numbering, three selection algorithms, Pareto ranking and
parsimony pressure. `test/integration/xor_evolves_tests.erl` solves XOR through
the full process-per-neuron path in 12 to 22 generations, and pole balancing,
LTC and recurrent evolution have equivalent tests.

What an evolved genotype cannot do is leave the VM it was bred in.

**8a. ⚠ DEFECT: `network_evaluator:from_genotype/1` discards the weights and
reports success.**

Its `@doc` says it reads the agent's structure *and weights* "from Mnesia". Both
halves are false: there is no Mnesia (item 3), and no weight is transferred.
`build_network_from_structure/1` counts the neurons, invents a layer shape
(`N < 10 -> [N]`, otherwise two layers of `N div 2`) and fills it with **random
weights**, under its own comment *"Create network with random weights (topology
approximation only)"*. It then returns `{ok, Network}`.

An evolved champion handed to this function comes back the right size and
brain-dead, and nothing errors. The truthful note lives on a private function
three hundred lines below the public promise that contradicts it.

Intended: carry the weights when the evolved topology is representable as dense
layers, and return `{error, not_layerable}` when it is not. Correct the `@doc`.
An arbitrary or recurrent genotype must refuse rather than approximate, because a
silent approximation behind an `{ok, _}` is worse than no bridge at all.

**8b. Canonical genotype serialisation — DONE.**

Landed as `genotype_codec`, with `genotype:to_binary/1`, `from_binary/1` and
`genome_id/1` delegating to it. Moved to `README.md` per this file's rule.

Hand-rolled canonical encoding over the closed term subset a genotype actually
contains (atoms, integers, floats, binaries, proper lists, tuples), refusing
everything else rather than guessing. Verified against `include/records.hrl`:
these records contain no maps, and a map is the shape that made the sibling's
I.12 possible. Lossless, because choosing which fields "matter" would be the
same silent lossy conversion that 8a is a defect for. Atoms decode through
`binary_to_existing_atom/2`, so an untrusted genome cannot mint atoms and a
genome from an incompatible build is refused by name.

Two things learned by building it, both recorded because they are the kind of
thing that gets re-derived wrongly:

- **The VM cannot hold a non-finite float.** Arithmetic raises `badarith` rather
  than overflowing, and the bit syntax refuses to match infinity and NaN
  patterns. A finiteness check on the encode side is therefore defensive code
  for an impossible state. The check belongs only on decode, where bytes arrive
  from elsewhere.
- **A round-trip assertion cannot see a canonicality regression.** The first
  version of the zero-normalisation test passed with and without the
  normalisation it was named after, because both forms are deterministic and
  both round-trip. Found by injecting the regression rather than by reading.
  It asserts the bytes now. Three regressions were injected in total; two were
  caught by the tests as written and the third was not, which is why the count
  is worth stating.

Still true and worth keeping in view: this is orthogonal to item 3. Item 3 is
surviving a VM restart on one machine; this is a genome being a value that can
travel.

**8c. ONNX from an arbitrary DAG — STILL OPEN. The LAYERABLE path is now
verified, which it was not before, and two defects in it were fixed on the way.**

⚠ Read the status precisely. Arbitrary-DAG export is **not implemented**. What
changed is that the dense-layer path it would extend is no longer unverified.

**What was wrong, and how it was found.** Every eunit test for `network_onnx`
asserted only that bytes came out and that there were more than zero of them.
Nothing ever loaded a model, so nothing could see whether the exported graph
computed the right function. `scripts/check_onnx_export.escript` and
`scripts/check_onnx_export.py` now run the exported model in onnxruntime and
compare against `network_evaluator:evaluate/2`. That is a guard comparing two
sides of a boundary, and the first run failed 1 of 4.

1. **The output activation was ignored.** `get_network_data/1` never read
   `get_output_activation/1`, and the hidden activation was applied to every
   layer including the last. A relu-hidden, linear-output network exported with
   relu on its output: onnxruntime returned 0.0 where the evaluator returned
   -0.676. **Invisible whenever the two activations are equal**, which is what
   every pre-existing test used.
2. **`activation_to_onnx/1` ended in a catch-all returning Tanh.** The same
   silent substitution as `network_evaluator`'s private `apply_activation/2`, so
   an activation with no ONNX mapping exported a model computing something else
   and said nothing. Now a refusal.

Both fixed, both with eunit regressions that do not need Python. The script
covers seven cases and all seven agree, including one that goes the whole way:
a genotype that only ever existed in ETS, through `from_genotype/1` (item 8a),
to ONNX, to onnxruntime, at delta 0.0. **That is the first evidence that an
evolved controller can actually leave the BEAM**, as opposed to the claim that
it can.

**What remains for 8c proper.** `to_onnx/1` is still
`-spec to_onnx(network_evaluator:network())`, so it takes only the dense-layer
representation. An evolved topology that is recurrent, or that skips or crosses
layers, is refused by 8a's converter and therefore cannot reach the exporter at
all. Exporting those needs a topological sort, per-neuron Gather and Concat to
assemble arbitrary predecessor sets, and Loop or Scan for recurrence.

Note the evaluation half already exists: `tweann_nif:compile_network/3` and
`tweann_nif:evaluate/2` handle arbitrary DAG and recurrent topologies exactly.
What is missing is serialisation on either side of a runtime that is there. And
the work now has a harness that can tell whether it is right, which is the part
it did not have before.

## Not planned

- **Mnesia clustering across nodes.** Single-node `disc_copies` only. Genome
  distribution belongs to the mesh layer, not to the storage engine.
- **Prebuilt NIF artifacts.** NIFs are built from source. Shipping prebuilt
  shared objects couples the package to a specific glibc.
- **A silent NIF fallback.** Removed in v2.0.0. Implementation is selected
  explicitly and a missing library raises. Two implementations silently
  disagreed for months because the native path was never exercised.
