# Contributing to faber-tweann

Thank you for considering a contribution.

This is a research engine before it is a product, so the conventions below are
about keeping results trustworthy rather than about style for its own sake.

## The two documents that govern what goes where

`README.md` states what **is**. `ROADMAP.md` states what **will be**. A
capability moves from the roadmap into the README when it lands, with a test
that exercises it, and for any performance claim with committed benchmark
output.

Nothing on the roadmap may be described as a feature in the README, the guides,
EDoc comments or the hex description until it moves. If you add a capability,
move its entry rather than writing about it twice.

## Before you open a pull request

```
rebar3 compile
rebar3 eunit
rebar3 dialyzer
rebar3 ex_doc
```

All four. `ex_doc` is included because EDoc formatting errors are only visible
at doc-build time and they block publishing.

## Tests

**A test that cannot fail is worse than no test.** Before trusting a green run,
revert the change it covers and confirm the test goes red. This repository has
shipped a test that passed with and without the property it was named after; it
was found by injecting the regression, not by reading the code.

Prefer a test that asserts a value over one that asserts a shape. A shape
assertion passes on a function that returns correctly-shaped nonsense, and that
is a defect this codebase has actually had.

Where a boundary exists, test both sides of it. `scripts/check_onnx_export.py`
is the model: it runs an exported network in onnxruntime and compares against
the evaluator, which is the only way to know an export is right.

## EDoc formatting

EDoc is stricter than markdown and the errors are unhelpful. In doc comments:

- no backticks, single or triple
- no HTML tags
- `@see` takes a module or `module:function/arity`, never a URL. Put URLs in
  plain `%%` comments
- no `@doc` before a `-callback` or a `-record`

## Refuse rather than approximate

When a conversion cannot be done faithfully, return an error naming what stopped
it. Do not return a best effort behind an `ok`. Several of the defects fixed in
this repository were exactly that: a silent fallback, a catch-all clause, an
approximation reported as success. They are expensive because nothing surfaces
them.

The same applies to limits: validate and reject rather than clamp. Clamping
changes the thing, and then its published identifier no longer identifies what
ran.

## Native code

The Rust crate in `native/faber_nn_nifs` is built from source at compile time
and a Rust toolchain is required. NIFs raise on a missing library rather than
falling back silently; two implementations drifted apart for months because the
native path was never exercised.

## Commits

Explain why, not what. The diff already says what.

## Licence

Contributions are made under Apache-2.0, matching `LICENSE`.
