%% @doc Unit tests for exoself module.
-module(exoself_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Annealing Calculation Tests
%% ============================================================================

%% Test that perturbation calculation works correctly
calculate_perturbation_first_attempt_test() ->
    %% First attempt (1/10) should have high perturbation
    InitialRange = 1.0,
    AnnealingParam = 1.0,
    MaxAttempts = 10,

    Pert = exoself:calculate_perturbation(InitialRange, 1, MaxAttempts, AnnealingParam),
    %% (1 - 1/10)^1 = 0.9
    ?assertEqual(0.9, Pert).

calculate_perturbation_middle_attempt_test() ->
    %% Middle attempt (5/10) should have moderate perturbation
    InitialRange = 1.0,
    AnnealingParam = 1.0,
    MaxAttempts = 10,

    Pert = exoself:calculate_perturbation(InitialRange, 5, MaxAttempts, AnnealingParam),
    %% (1 - 5/10)^1 = 0.5
    ?assertEqual(0.5, Pert).

calculate_perturbation_last_attempt_test() ->
    %% Last attempt (10/10) should have zero perturbation
    InitialRange = 1.0,
    AnnealingParam = 1.0,
    MaxAttempts = 10,

    Pert = exoself:calculate_perturbation(InitialRange, 10, MaxAttempts, AnnealingParam),
    %% (1 - 10/10)^1 = 0.0
    ?assertEqual(0.0, Pert).

calculate_perturbation_decreases_test() ->
    %% Test that perturbation decreases over attempts
    InitialRange = 1.0,
    AnnealingParam = 1.0,
    MaxAttempts = 10,

    Pert1 = exoself:calculate_perturbation(InitialRange, 1, MaxAttempts, AnnealingParam),
    Pert5 = exoself:calculate_perturbation(InitialRange, 5, MaxAttempts, AnnealingParam),
    Pert9 = exoself:calculate_perturbation(InitialRange, 9, MaxAttempts, AnnealingParam),

    ?assert(Pert1 > Pert5),
    ?assert(Pert5 > Pert9).

calculate_perturbation_with_higher_annealing_test() ->
    %% Higher annealing parameter = faster decay
    InitialRange = 1.0,
    MaxAttempts = 10,

    %% With annealing = 1.0
    Pert_a1 = exoself:calculate_perturbation(InitialRange, 5, MaxAttempts, 1.0),
    %% With annealing = 2.0
    Pert_a2 = exoself:calculate_perturbation(InitialRange, 5, MaxAttempts, 2.0),

    %% Higher annealing should result in lower perturbation at same attempt
    ?assert(Pert_a2 < Pert_a1).

calculate_perturbation_with_custom_range_test() ->
    %% Test with different initial ranges
    AnnealingParam = 1.0,
    MaxAttempts = 10,

    Pert1 = exoself:calculate_perturbation(1.0, 5, MaxAttempts, AnnealingParam),
    Pert2 = exoself:calculate_perturbation(2.0, 5, MaxAttempts, AnnealingParam),

    %% Double range should give double perturbation
    ?assertEqual(Pert1 * 2, Pert2).

%% ============================================================================
%% Exoself State Tests
%% ============================================================================

exoself_exports_test() ->
    %% Test that expected functions are exported
    Exports = exoself:module_info(exports),
    ?assert(lists:member({start, 3}, Exports)),
    ?assert(lists:member({prep, 3}, Exports)),
    ?assert(lists:member({calculate_perturbation, 4}, Exports)),
    ?assert(lists:member({init, 4}, Exports)).

%% ============================================================================
%% Integration Test (requires full network)
%% ============================================================================

%% Note: Full integration tests for exoself spawning networks
%% are deferred to v0.6.0 when we have complete scape integration.
%% These tests would require:
%% - A working scape to evaluate fitness
%% - Complete sensor/actuator implementations
%% - Population monitor for termination signals

%% For now, we test the core algorithm components (annealing, state)
%% and defer network lifecycle tests.
