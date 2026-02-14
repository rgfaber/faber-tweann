# Creating Custom Morphologies

This guide explains how to create custom morphologies for faber-tweann v0.9.0+.

## What is a Morphology?

A morphology defines the sensory inputs and motor outputs for a neural network problem domain. It specifies:
- What sensors the network can use to perceive its environment
- What actuators the network can use to affect its environment
- The vector length (number of values) each sensor/actuator provides

## Quick Start

### 1. Create a Module

Create a new Erlang module that implements the `morphology_behaviour`:

```erlang
-module(my_morphology).
-behaviour(morphology_behaviour).
-include_lib("faber_tweann/include/records.hrl").

-export([get_sensors/1, get_actuators/1]).

%% Required callbacks
get_sensors(my_problem) ->
    [#sensor{
        name = my_sensor,
        type = standard,
        scape = {private, my_scape},
        vl = 3,  % 3 input values
        parameters = [],
        fanout_ids = [],
        generation = 0
    }];
get_sensors(_) ->
    error(invalid_morphology).

get_actuators(my_problem) ->
    [#actuator{
        name = my_actuator,
        type = standard,
        scape = {private, my_scape},
        vl = 2,  % 2 output values
        parameters = [],
        fanin_ids = [],
        generation = 0
    }];
get_actuators(_) ->
    error(invalid_morphology).
```

### 2. Register Your Morphology

Before using your morphology, register it with the morphology registry:

```erlang
ok = morphology_registry:register(my_problem, my_morphology).
```

### 3. Use It

Now you can create agents with your custom morphology:

```erlang
Constraint = #constraint{
    morphology = my_problem,
    neural_afs = [tanh],
    neural_pfns = [none],
    neural_aggr_fs = [dot_product]
},
AgentId = genotype:construct_Agent(SpecieId, AgentId, Constraint).
```

## Morphology Behaviour Callbacks

### get_sensors/1

Returns a list of `#sensor{}` records for the given morphology name.

**Sensor Record Fields:**
- `name` (atom) - Unique identifier for this sensor
- `type` (standard | ...) - Sensor type
- `scape` ({private|public, atom()}) - Scape this sensor reads from
- `vl` (integer) - Vector length (number of input values)
- `parameters` (list) - Configuration parameters
- `fanout_ids` ([id()]) - Leave empty, set by genotype
- `generation` (integer) - Set to 0
- `cx_id` (id()) - Leave undefined, set by genotype
- `id` (id()) - Leave undefined, set by genotype

### get_actuators/1

Returns a list of `#actuator{}` records for the given morphology name.

**Actuator Record Fields:**
- `name` (atom) - Unique identifier for this actuator
- `type` (standard | ...) - Actuator type
- `scape` ({private|public, atom()}) - Scape this actuator writes to
- `vl` (integer) - Vector length (number of output values)
- `parameters` (list) - Configuration parameters
- `fanin_ids` ([id()]) - Leave empty, set by genotype
- `generation` (integer) - Set to 0
- `cx_id` (id()) - Leave undefined, set by genotype
- `id` (id()) - Leave undefined, set by genotype

## Examples

### Example 1: Simple Classification

A morphology for binary classification with 10 features:

```erlang
-module(morphology_classifier).
-behaviour(morphology_behaviour).
-include_lib("faber_tweann/include/records.hrl").

-export([get_sensors/1, get_actuators/1]).

get_sensors(binary_classifier) ->
    [#sensor{
        name = classifier_input,
        type = standard,
        scape = {private, classifier_sim},
        vl = 10,  % 10 feature inputs
        parameters = [],
        fanout_ids = [],
        generation = 0
    }];
get_sensors(_) -> error(invalid_morphology).

get_actuators(binary_classifier) ->
    [#actuator{
        name = classifier_output,
        type = standard,
        scape = {private, classifier_sim},
        vl = 1,  % Single binary output
        parameters = [],
        fanin_ids = [],
        generation = 0
    }];
get_actuators(_) -> error(invalid_morphology).
```

### Example 2: Multi-Sensor Morphology

A morphology with multiple sensor types:

```erlang
-module(morphology_robot).
-behaviour(morphology_behaviour).
-include_lib("faber_tweann/include/records.hrl").

-export([get_sensors/1, get_actuators/1]).

get_sensors(robot) ->
    [
        % Distance sensors
        #sensor{
            name = distance_sensors,
            type = standard,
            scape = {private, robot_sim},
            vl = 5,
            parameters = [],
            fanout_ids = [],
            generation = 0
        },
        % Camera input
        #sensor{
            name = camera,
            type = standard,
            scape = {private, robot_sim},
            vl = 100,  % 10x10 pixel grid
            parameters = [10, 10],
            fanout_ids = [],
            generation = 0
        },
        % Battery level
        #sensor{
            name = battery,
            type = standard,
            scape = {private, robot_sim},
            vl = 1,
            parameters = [],
            fanout_ids = [],
            generation = 0
        }
    ];
get_sensors(_) -> error(invalid_morphology).

get_actuators(robot) ->
    [
        % Motor control
        #actuator{
            name = motors,
            type = standard,
            scape = {private, robot_sim},
            vl = 2,  % Left and right motor speeds
            parameters = [],
            fanin_ids = [],
            generation = 0
        }
    ];
get_actuators(_) -> error(invalid_morphology).
```

### Example 3: Supporting Multiple Morphologies

One module can support multiple related morphologies:

```erlang
-module(morphology_game).
-behaviour(morphology_behaviour).
-include_lib("faber_tweann/include/records.hrl").

-export([get_sensors/1, get_actuators/1]).

%% Player morphology
get_sensors(player) ->
    [#sensor{
        name = game_state,
        type = standard,
        scape = {private, game_sim},
        vl = 20,
        parameters = [],
        fanout_ids = [],
        generation = 0
    }];

%% Enemy morphology
get_sensors(enemy) ->
    [#sensor{
        name = game_state,
        type = standard,
        scape = {private, game_sim},
        vl = 15,  % Different perception
        parameters = [],
        fanout_ids = [],
        generation = 0
    }];

get_sensors(_) -> error(invalid_morphology).

%% Same actuators for both
get_actuators(Morphology) when Morphology =:= player; Morphology =:= enemy ->
    [#actuator{
        name = game_action,
        type = standard,
        scape = {private, game_sim},
        vl = 4,  % Up, down, left, right
        parameters = [],
        fanin_ids = [],
        generation = 0
    }];
get_actuators(_) -> error(invalid_morphology).
```

Then register both morphologies:

```erlang
ok = morphology_registry:register(player, morphology_game),
ok = morphology_registry:register(enemy, morphology_game).
```

## Registry API

### morphology_registry:register/2

Register a morphology implementation:

```erlang
ok = morphology_registry:register(MorphologyName, ModuleName).
```

Returns:
- `ok` on success
- `{error, {module_not_loaded, Module}}` if module doesn't exist
- `{error, {missing_callback, Callback, Arity}}` if behaviour not implemented

### morphology_registry:unregister/1

Remove a morphology registration:

```erlang
ok = morphology_registry:unregister(MorphologyName).
```

### morphology_registry:get/1

Get the implementing module for a morphology:

```erlang
{ok, Module} = morphology_registry:get(MorphologyName).
{error, not_found} = morphology_registry:get(unknown).
```

### morphology_registry:list_all/0

List all registered morphologies:

```erlang
Names = morphology_registry:list_all().  % [atom()]
```

### morphology_registry:is_registered/1

Check if a morphology is registered:

```erlang
true = morphology_registry:is_registered(my_problem).
false = morphology_registry:is_registered(unknown).
```

## Scapes

Sensors and actuators connect to "scapes" - the environments they interact with.

### Scape Format

```erlang
{Visibility, ScapeName}
```

Where:
- `Visibility` is `private` or `public`
- `ScapeName` is an atom identifying the scape module/process

### Private Scapes

Private scapes are used for problem-specific simulations:

```erlang
scape = {private, my_sim}
```

The scape name should match your simulation module name.

### Public Scapes

Public scapes are shared across multiple agents:

```erlang
scape = {public, shared_environment}
```

## Application Lifecycle

### In Your Application

```erlang
% In your application start/2
start(_StartType, _StartArgs) ->
    % Ensure faber_tweann is started
    {ok, _} = application:ensure_all_started(faber_tweann),

    % Register your morphologies
    ok = morphology_registry:register(my_problem, my_morphology),
    ok = morphology_registry:register(another_problem, another_morphology),

    % Start your supervisor
    my_sup:start_link().
```

### In Tests

```erlang
setup() ->
    application:ensure_all_started(faber_tweann),
    morphology_registry:register(test_morph, test_morphology),
    genotype:init_db().

teardown(_) ->
    morphology_registry:unregister(test_morph),
    genotype:reset_db().
```

## Example Morphologies

See `examples/` directory for complete working examples:

- `examples/xor/` - XOR function learning
- `examples/pole_balancing/` - Cart-pole balancing
- `examples/forex/` - Forex trading agent
- `examples/flatland/` - Prey/predator simulation

To use example morphologies in your project:

```erlang
% Register the examples you need
ok = morphology_registry:register(xor_mimic, morphology_xor),
ok = morphology_registry:register(pole_balancing, morphology_pole_balancing),
ok = morphology_registry:register(forex_trader, morphology_forex),
ok = morphology_registry:register(prey, morphology_flatland),
ok = morphology_registry:register(predator, morphology_flatland).
```

## Best Practices

1. **One Module Per Domain** - Group related morphologies in one module
2. **Meaningful Names** - Use descriptive sensor/actuator names
3. **Document VL** - Comment why you chose specific vector lengths
4. **Error Handling** - Always handle invalid morphology names
5. **Registration** - Register morphologies at application startup
6. **Testing** - Test your morphology with actual agent construction

## Troubleshooting

### Error: morphology_not_registered

You forgot to register your morphology:

```erlang
ok = morphology_registry:register(my_problem, my_morphology).
```

### Error: missing_callback

Your module doesn't implement the behaviour correctly. Ensure:

```erlang
-behaviour(morphology_behaviour).
-export([get_sensors/1, get_actuators/1]).
```

### Error: module_not_loaded

The module doesn't exist or isn't compiled. Check:
1. Module file exists
2. Module is in code path
3. Module compiles without errors

### Sensors/Actuators Not Found

Make sure your morphology name matches exactly:

```erlang
% Registration
morphology_registry:register(my_problem, my_morphology)

% Usage - MUST MATCH
#constraint{morphology = my_problem, ...}

% Implementation - MUST MATCH
get_sensors(my_problem) -> ...
```

## Migration from Pre-v0.9.0

If you were using the old hardcoded morphology system:

**Before (< v0.9.0):**
```erlang
% Morphologies were hardcoded in morphology.erl
Constraint = #constraint{morphology = xor_mimic, ...}
```

**After (>= v0.9.0):**
```erlang
% Register first
ok = morphology_registry:register(xor_mimic, morphology_xor),

% Then use
Constraint = #constraint{morphology = xor_mimic, ...}
```

## Version History

- **v0.9.0** - Introduced morphology behaviour and registry system
- **v0.8.x and earlier** - Hardcoded morphologies in morphology.erl
