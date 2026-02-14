# DXNN2 Analysis and Migration Guide

This directory contains comprehensive analysis and migration guidance for converting DXNN2 to faber-tweann.

## Documents

### DXNN2_CODEBASE_ANALYSIS.md (37KB, 970 lines)

**Comprehensive deep analysis of DXNN2 codebase covering:**

1. **Architecture Overview** - System hierarchy, data flow, evolutionary cycle
2. **Core Data Structures** - Records, naming issues, improvement recommendations
3. **Core Components Analysis** - 7 major modules with specific issues and line numbers
4. **Readability Issues** - Variable names, function names, complex structures, documentation gaps
5. **Code Smells** - Copy-paste patterns, long functions, poor error handling
6. **Migration Recommendations** - Specific, actionable improvements
7. **Dependency Graph Analysis** - Module dependencies and bottlenecks
8. **Additional Issues** - Typos, deprecated APIs, dangerous patterns
9. **Quality Assessment Summary** - Module rating table
10. **Refactoring Priority** - 4 phases with effort estimates

## Refactoring Principles for faber-tweann

### Core Philosophy: Test-Driven Development (TDD)

All refactoring MUST follow TDD principles:
- **Write tests first** before implementing or refactoring code
- **Red-Green-Refactor cycle**:
  1. Write failing test(s) that specify desired behavior
  2. Implement minimal code to pass tests
  3. Refactor to improve code quality while maintaining test pass
- **Tests are living documentation** of system behavior
- **100% coverage target** for core modules (neuron, cortex, plasticity, signal_aggregator)
- **Integration tests** for process interaction patterns

### Code Quality Standards

#### 1. Declarative, Idiomatic Erlang
**Do this:**
```erlang
%% Pattern matching on function heads (idiomatic)
is_valid_activation_function(tanh) -> true;
is_valid_activation_function(sin) -> true;
is_valid_activation_function(cos) -> true;
is_valid_activation_function(_) -> false.

%% List comprehensions instead of explicit recursion
scale_inputs(InputVector, Scaling) ->
    [Input * Scaling || Input <- InputVector].

%% Guards for preconditions
add_neuron(AgentId, Activation, _) when is_atom(Activation) ->
    % implementation
    ok.
```

**Don't do this:**
```erlang
%% Nested if/case statements
is_valid(Value) ->
    case Value of
        A -> case A of
                 B -> case B of
                      C -> true;
                      _ -> false
                      end;
                 _ -> false
             end;
        _ -> false
    end.

%% Explicit recursion where lists:map would work
scale_inputs([], _Scaling, Acc) ->
    lists:reverse(Acc);
scale_inputs([Input|Inputs], Scaling, Acc) ->
    scale_inputs(Inputs, Scaling, [Input * Scaling | Acc]).
```

**Max nesting: 1 level deep** - if nesting exceeds this, extract to helper function

#### 2. Clear, Descriptive Naming

All names must be **self-documenting** and avoid cryptic abbreviations:

**Module names**: `neuron.erl`, not `n.erl`
**Function names**: `calculate_neuron_output/2`, not `calc_out/2`
**Variable names**: `sensor_process_ids`, not `spids`
**Record fields**: `weighted_inputs`, not `idps`
**Function parameters**: `activation_function`, not `af`

See naming table in Section 6.1 of analysis document for specific conversions.

#### 3. Comprehensive Type Specifications

Every exported function MUST have `-spec`:
```erlang
%% Good:
-spec calculate_activation(ActivationFunction, Signal) -> Output
    when ActivationFunction :: activation_function(),
         Signal :: signal_vector(),
         Output :: float().

calculate_activation(tanh, Signal) ->
    signal_aggregator:dot_product(Signal) |> functions:tanh();
calculate_activation(sin, Signal) ->
    signal_aggregator:dot_product(Signal) |> functions:sin().

%% Also define custom types:
-type activation_function() :: tanh | sin | cos | gaussian | {circuit, spec()}.
-type signal_vector() :: [float()].
-type weight_spec() :: {weight(), delta_weight(), learning_rate(), param_list()}.
```

**Benefit**: Enables dialyzer static analysis, clarifies intent, improves IDE support

#### 4. In-Code Documentation

Every module and function MUST have documentation:

**Module documentation** (at top of file):
```erlang
%% @module neuron
%% @doc Neuron Process - Individual computational unit in neural network
%%
%% A neuron is a process that:
%% - Accumulates signals from multiple input sources (sensors, other neurons)
%% - Applies plasticity learning rules to update weights
%% - Computes activation function output
%% - Forwards output to destination neurons/actuators
%%
%% The neuron maintains multiple versions of weights:
%% - baseline: original weights from initial network
%% - current: working weights (subject to plasticity)
%% - backup: saved weights for hill-climbing restoration
%%
%% Process Lifecycle:
%% 1. gen/2 spawns process
%% 2. prep/1 initializes and waits for configuration
%% 3. loop/6 main receive loop handling signal propagation
%%
%% Implementation Notes:
%% - Uses tail recursion for efficient process loops
%% - Accumulates inputs without blocking (async message passing)
%% - Supports both static and dynamic (learning) weights
%% - Handles both neural and substrate-based processing
```

**Function documentation**:
```erlang
%% @doc Apply plasticity learning rule to update synaptic weights
%%
%% This function modifies weights based on the neuron's learning rule,
%% simulating biological synaptic plasticity. Different plasticity rules
%% (Hebbian, Ojas, etc.) are applied based on the configuration.
%%
%% The weight tuple format is: {Weight, DeltaWeight, LearningRate, ParamList}
%% where:
%%   - Weight: current synaptic weight value
%%   - DeltaWeight: momentum term from previous update
%%   - LearningRate: plasticity rule learning rate
%%   - ParamList: additional parameters specific to learning rule
%%
%% @param LearningRule the plasticity rule to apply (from neuron config)
%% @param InputAccumulator list of {SourceId, Signal} pairs
%% @param WeightedInputs {SourceId, [WeightSpec]} pairs
%% @param NeuronOutput final computed output of neuron
%% @returns UpdatedWeightedInputs with plasticity applied
%% @throws {error, unknown_learning_rule} if learning rule not recognized
apply_plasticity(LearningRule, InputAccumulator, WeightedInputs, NeuronOutput) ->
    % implementation
    ok.
```

**Benefits**:
- Serves as primary documentation (especially for complex algorithms)
- Integrates with edoc for generating HTML docs
- Available in IDE tooltips
- Type information aids in understanding

#### 5. Testability First

Code must be designed for testability:

**Pure functions** instead of side effects:
```erlang
%% Good: Pure function, testable
-spec apply_activation_function(activation_function(), signal_vector()) -> float().
apply_activation_function(tanh, Signal) ->
    functions:tanh(signal_aggregator:dot_product(Signal));
apply_activation_function(sin, Signal) ->
    functions:sin(signal_aggregator:dot_product(Signal)).
```

**Dependency injection** instead of hardcoded dependencies:
```erlang
%% Good: Dependencies passed as parameters
-spec run_simulation(network(), environment(), options()) -> results().
run_simulation(Network, Environment, Options) ->
    % Can test with mock environment
    ok.

%% Bad: Hardcoded environment lookup
-spec run_simulation(network()) -> results().
run_simulation(Network) ->
    Environment = scape:get(default),  % Can't inject for testing!
    % ...
    ok.
```

**Separation of concerns**:
- Data transformations in pure functions (easy to test)
- Behavior/side-effects in gen_server handlers (harder to test, test sparingly)
- Message marshaling/protocol in separate modules (test with mock processes)

## Refactoring Workflow for faber-tweann

### Phase 1: Foundation (2-3 weeks)

**Priority: Enable TDD and establish quality standards**

1. **Create test infrastructure**
   ```
   test/
     unit/
       neuron_test.erl           % Activation, plasticity
       signal_aggregator_test.erl
       genome_mutator_test.erl
       functions_test.erl
     integration/
       network_evaluation_test.erl    % Cortex + neurons
       evolution_test.erl              % Full cycle
   ```

2. **Add type specifications** to all modules
   - Use `dialyzer` for static analysis
   - Fix all dialyzer warnings as they appear

3. **Implement TDD for critical modules** (in priority order):
   - `signal_aggregator.erl` - pure functions, easy to test
   - `functions.erl` - activation functions
   - `plasticity.erl` - learning rules
   - `neuron.erl` core logic

4. **Rename cryptic variables and fields**
   - Use refactoring tools to update systematically
   - Update all tests to use new names
   - Verify no behavioral changes

5. **Add module and function documentation**
   - Write before implementation
   - Use documentation to clarify intent
   - Helps guide TDD test cases

### Phase 2: Structural Improvements (3-4 weeks)

**Priority: Refactor for maintainability while maintaining behavior**

1. **Extract utility modules**
   - `math_utils.erl` - weighted selection, perturbation, saturation
   - `list_utils.erl` - common list operations
   - Test each thoroughly

2. **Break down large functions**
   - `cortex:loop/10` → separate state machine handlers
   - `exoself:prep/3` → modular initialization functions
   - `genome_mutator.erl` duplicate mutations → single parametric version
   - Each step with tests

3. **Implement error handling**
   - Replace `exit()` with structured error returns
   - Add error handling tests
   - Use pattern matching for error cases

4. **Add comprehensive integration tests**
   - Test sense-think-act cycle
   - Test evolution loop
   - Test mutation operators

### Phase 3: Robustness (3-4 weeks)

**Priority: Eliminate bugs and improve reliability**

1. **Fix identified bugs** (from analysis):
   - Line 65 cortex.erl: `termiante` → `terminate`
   - Line 299 neuron.erl: return correct perturbed values
   - Add test for each bug before fixing

2. **Update deprecated APIs**
   - `now()` → `erlang:timestamp()`
   - `random` module → `rand` module
   - Test with Erlang/OTP 25+

3. **Add process safety**
   - Use `spawn_link` instead of `spawn` where appropriate
   - Add monitors for critical processes
   - Test crash recovery scenarios

4. **Add timeouts to receive loops**
   - Prevent hanging on stuck processes
   - Test timeout scenarios
   - Document timeout semantics

### Phase 4: Performance & Polish (2-3 weeks)

**Priority: Optimize and prepare for production**

1. **Profile hot paths**
   - Likely: neuron forward pass, weight updates
   - Use eprof/fprof
   - Add performance benchmarks

2. **Optimize Mnesia access**
   - Consider async/event-sourced design
   - Add caching where beneficial
   - Test for consistency issues

3. **Cleanup and dead code removal**
   - Remove commented-out code
   - Remove unused record fields
   - Update exports to be explicit

## TDD Example: Testing a Neuron's Plasticity Update

```erlang
%% neuron_plasticity_test.erl
-module(neuron_plasticity_test).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% Test that Hebbian plasticity increases weights for correlated input/output
hebbian_plasticity_positive_correlation_test() ->
    % Setup
    InputSignal = [0.5, 0.3],
    SourceId = {input, neuron, 1},
    WeightedInputs = [{SourceId, [{0.1, 0.0, 0.1, [0.1]}, 
                                   {0.2, 0.0, 0.1, [0.1]}]}],
    InputAccumulator = [{SourceId, InputSignal}],
    NeuronOutput = 0.8,
    LearningRate = 0.1,
    
    % Execute
    UpdatedWeights = plasticity:hebbian(
        [LearningRate],
        InputAccumulator,
        WeightedInputs,
        [NeuronOutput]
    ),
    
    % Verify: weights should increase (positive correlation)
    [{SourceId, UpdatedWeightSpecs}] = UpdatedWeights,
    {InitialW1, _, _, _} = lists:nth(1, UpdatedWeightSpecs),
    {UpdatedW1, _, _, _} = lists:nth(1, UpdatedWeightSpecs),
    ?assert(UpdatedW1 > InitialW1),
    
    % Verify: weight increase proportional to input*output
    ExpectedDelta = LearningRate * 0.5 * 0.8,  % input * output * learning_rate
    ActualDelta = UpdatedW1 - InitialW1,
    ?assert(abs(ActualDelta - ExpectedDelta) < 0.001).
```

## Document References

See DXNN2_CODEBASE_ANALYSIS.md for:
- **Section 3**: Detailed analysis of each module
- **Section 4**: Readability issues and specific problems
- **Section 5**: Code smells with examples
- **Section 6**: Specific refactoring recommendations
- **Section 8**: List of identified bugs with line numbers
- **Naming table in Section 6.1**: Complete mapping of old → new names

## Success Criteria

A module is ready for production when it:

1. **Has comprehensive tests**
   - Unit tests for pure functions (100% coverage target)
   - Integration tests for process interaction
   - Error case tests
   - Performance benchmarks for critical functions

2. **Has clear, idiomatic code**
   - Pattern matching on function heads (not nested case/if)
   - Max 1 level of nesting
   - No cryptic abbreviations
   - Declarative style where possible

3. **Has complete documentation**
   - Module-level documentation explaining purpose and behavior
   - Function-level documentation for all public functions
   - Type specifications for all public functions
   - Complex algorithm explanations in comments

4. **Passes quality checks**
   - Dialyzer analysis with zero warnings
   - Code style linting passes
   - All PR code review feedback addressed

5. **Maintains compatibility**
   - All original tests pass
   - No behavioral changes (verified by tests)
   - Backward compatible API (if public)

---

For detailed analysis and specific issues, refer to DXNN2_CODEBASE_ANALYSIS.md.

