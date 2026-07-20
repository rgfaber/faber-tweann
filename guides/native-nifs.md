# Native Acceleration

faber_tweann ships Rust NIFs for its numeric hot paths. They are part of the
package and are built from source during compilation. There is no separate
edition, no private repository, and nothing to enable.

> Prior to v2.0.0 these NIFs lived in a separate `faber-nn-nifs` package
> described as an "enterprise" add-on. That package has been absorbed. If you
> depended on `faber_nn_nifs` directly, depend on `faber_tweann` instead.

## Requirements

A Rust toolchain must be available at compile time.

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

The build never downloads prebuilt artifacts. Prebuilt shared objects couple a
package to a specific glibc, which has caused deployment failures elsewhere in
this ecosystem.

## Selecting an implementation

Two implementations exist for the same numeric surface:

| Implementation | Module | Use |
|---|---|---|
| Native | `faber_nn_nifs` | Default. |
| Pure Erlang | `tweann_nif_fallback` | Platforms without Rust, and differential testing. |

Select with application environment:

```erlang
%% sys.config
[{faber_tweann, [{nif_impl, nif}]}].       %% default, require native
[{faber_tweann, [{nif_impl, fallback}]}].  %% require pure Erlang
```

`nif` is the default. If the native library is missing or fails to load,
faber_tweann raises on first use with an explanatory error. It does **not**
quietly fall back.

That is deliberate. Silent fallback means a system that keeps running, slower
and potentially differently, with no log line saying so. This package
previously auto-detected and fell back without notice, and as a result the
native path was never exercised in its own test suite: two implementations
drifted apart for months while every test passed.

To build without Rust:

```
FABER_TWEANN_SKIP_NIF=1 rebar3 compile
```

Then set `{nif_impl, fallback}` at runtime. Skipping the build without setting
this will raise, by design.

## Reporting the active path

```erlang
tweann_nif:impl().       %% faber_nn_nifs | tweann_nif_fallback
tweann_nif:is_loaded().  %% true when native
```

Record `tweann_nif:impl()` in any benchmark or experiment log. A performance
number that does not name its execution path is not a number.

## Keeping the two implementations honest

`test/unit/nif_fallback_conformance_tests.erl` runs both implementations over
the same inputs and asserts they agree.

When adding a function that has both a native and a fallback implementation,
add a case there. Testing each side against its own expectations is what
allowed `weight_distance_l1/2` to compute Manhattan distance in Erlang and
mean absolute deviation in Rust, both under the same name, with both test
suites green.

## Performance

This guide previously quoted speedup figures. They are not reproduced here,
because no committed measurement supported them and the two packages quoted
mutually inconsistent numbers.

Measured figures will be added once a benchmark harness runs and its output is
committed. Until then, `test/benchmark/bench_nif_vs_erlang.erl` exists and can
be run directly.
