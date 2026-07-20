# Installation

## Community Edition (Hex.pm)

The Community Edition is available on hex.pm and uses pure Erlang fallbacks for all operations. No Rust toolchain required.

### Rebar3 (Erlang Projects)

Add `faber_tweann` to your `rebar.config`:

```erlang
{deps, [
    {faber_tweann, "~> 0.17.0"}
]}.
```

Then fetch dependencies:

```bash
rebar3 compile
```

### Mix (Elixir Projects)

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:faber_tweann, "~> 0.16.0"}
  ]
end
```

Then fetch dependencies:

```bash
mix deps.get
```

## Native Acceleration

The Rust NIFs ship with faber_tweann and are built from source at compile
time. No extra dependency is needed.

Before v2.0.0 they lived in a separate `faber-nn-nifs` package. That package
has been absorbed; remove it from your deps if present.

A Rust toolchain is required:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

To build without it:

```bash
FABER_TWEANN_SKIP_NIF=1 rebar3 compile
```

and then select the pure Erlang implementation explicitly, since faber_tweann
will otherwise raise rather than fall back silently:

```erlang
[{faber_tweann, [{nif_impl, fallback}]}].
```

See the [Native Acceleration](native-nifs.md) guide.

## From Source

Clone the repository:

```bash
git clone https://github.com/rgfaber/faber-tweann.git
cd faber-tweann
rebar3 compile
```

## Requirements

- Erlang/OTP 24 or later
- Mnesia (included with Erlang)
- Rust 1.70+ and Cargo (for the native NIFs; see above to build without)

## Verify Installation

Start an Erlang shell and initialize the database:

```erlang
rebar3 shell

% In the shell:
genotype:init_db().
% => ok
```

If you see `ok`, the installation is successful!

### Verify Native Acceleration

```erlang
% Check which implementation is active
faber_nn_nifs:is_loaded().
% => true when the native path is active

% Check tweann_nif detection
tweann_nif:is_loaded().
% => true (NIFs available)
```

## Next Steps

See the [Quick Start](quickstart.md) guide to create your first evolving neural network.
