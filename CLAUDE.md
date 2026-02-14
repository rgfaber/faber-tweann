# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**faber-tweann** is a Topology and Weight Evolving Artificial Neural Networks library for Erlang, based on DXNN2 by Gene Sher ("Handbook of Neuroevolution through Erlang").

## Build Commands

```bash
rebar3 get-deps        # Fetch dependencies
rebar3 compile         # Compile all modules
rebar3 eunit           # Run unit tests
rebar3 ct              # Run common tests
rebar3 dialyzer        # Run type analysis
rebar3 hex publish     # Publish to hex.pm
```

## Architecture

### Core Modules

- **genotype.erl** - Genotype construction and Mnesia persistence
- **constructor.erl** - Phenotype construction from genotypes
- **morphology.erl** - Sensor/actuator specifications per problem domain
- **records.hrl** - Record definitions (#agent, #cortex, #sensor, #actuator, #neuron)

### Data Storage

- Uses **Mnesia** for persistent genotype storage
- Uses **Erlang records** (not maps) for all data structures
- ID format: `{{LayerCoord, UniqueFloat}, Type}`
- Weight format: `{Weight, DeltaWeight, LearningRate, ParameterList}`

### Morphologies

Each morphology defines sensors and actuators for a problem domain:
- xor_mimic - XOR function learning
- pole_balancing - Classic RL benchmark
- discrete_tmaze - Navigation task
- prey/predator - Flatland simulation
- forex_trader - Financial trading

## EDoc Formatting Rules

**IMPORTANT: EDoc has strict formatting requirements for hex.pm publishing**

- **NO heredoc syntax** - Do not use triple backticks (```)
- **NO HTML tags** - Do not use `<pre>`, `<code>`, etc.
- **NO backticks** - Do not use single backticks for inline code

Instead use:
- Plain text for code examples in documentation
- Proper EDoc tags (@doc, @spec, @param, etc.)
- Simple ASCII formatting for examples

## Testing

```bash
rebar3 eunit           # Run all unit tests
rebar3 ct              # Run common tests
```

Tests require Mnesia initialization:
```erlang
genotype:init_db().
genotype:reset_db().   % Clear between tests
```

## Publishing to Hex

1. Ensure all tests pass: `rebar3 eunit && rebar3 ct`
2. Run dialyzer: `rebar3 dialyzer`
3. Bump version in `src/faber_tweann.app.src`
4. Authenticate: `rebar3 hex user auth`
5. Publish: `rebar3 hex publish --yes`

## Dialyzer Suppressions

Some functions have dialyzer suppressions for:
- Supertype warnings (polymorphic return types)
- No-match warnings (unreachable patterns in guards)

These are intentional and documented in the source files.

---

## Support

If you find this project valuable, consider supporting its development:

**☕ Buy Me a Coffee:** https://buymeacoffee.com/rlefever

