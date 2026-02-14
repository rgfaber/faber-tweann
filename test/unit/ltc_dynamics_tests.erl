%% @module ltc_dynamics_tests
%% @doc Unit tests for Liquid Time-Constant (LTC) neural dynamics module
%%
%% Tests cover:
%% - CfC closed-form evaluation
%% - ODE-based evaluation
%% - Backbone/head network computations
%% - Time constant dynamics
%% - State management
%% - Utility functions (sigmoid, tanh)
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0

-module(ltc_dynamics_tests).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Utility Function Tests
%%==============================================================================

sigmoid_zero_test() ->
    ?assertEqual(0.5, ltc_dynamics:sigmoid(0.0)).

sigmoid_positive_test() ->
    Result = ltc_dynamics:sigmoid(2.0),
    ?assert(Result > 0.88 andalso Result < 0.89).

sigmoid_negative_test() ->
    Result = ltc_dynamics:sigmoid(-2.0),
    ?assert(Result > 0.11 andalso Result < 0.12).

sigmoid_large_positive_test() ->
    %% Should saturate near 1.0
    Result = ltc_dynamics:sigmoid(10.0),
    ?assert(Result > 0.9999).

sigmoid_large_negative_test() ->
    %% Should saturate near 0.0
    Result = ltc_dynamics:sigmoid(-10.0),
    ?assert(Result < 0.0001).

sigmoid_overflow_protection_test() ->
    %% Very large values should not cause overflow
    Result = ltc_dynamics:sigmoid(1000.0),
    ?assert(Result > 0.9999 andalso Result =< 1.0).

tanh_zero_test() ->
    ?assertEqual(0.0, ltc_dynamics:tanh(0.0)).

tanh_positive_test() ->
    Result = ltc_dynamics:tanh(1.0),
    ?assert(Result > 0.76 andalso Result < 0.77).

tanh_negative_test() ->
    Result = ltc_dynamics:tanh(-1.0),
    ?assert(Result > -0.77 andalso Result < -0.76).

%%==============================================================================
%% State Management Tests
%%==============================================================================

clamp_state_within_bounds_test() ->
    ?assertEqual(0.5, ltc_dynamics:clamp_state(0.5, 1.0)).

clamp_state_positive_overflow_test() ->
    ?assertEqual(1.0, ltc_dynamics:clamp_state(1.5, 1.0)).

clamp_state_negative_overflow_test() ->
    ?assertEqual(-1.0, ltc_dynamics:clamp_state(-1.5, 1.0)).

clamp_state_at_bound_test() ->
    ?assertEqual(1.0, ltc_dynamics:clamp_state(1.0, 1.0)),
    ?assertEqual(-1.0, ltc_dynamics:clamp_state(-1.0, 1.0)).

clamp_state_different_bound_test() ->
    ?assertEqual(2.0, ltc_dynamics:clamp_state(3.0, 2.0)),
    ?assertEqual(-0.5, ltc_dynamics:clamp_state(-1.0, 0.5)).

reset_state_test() ->
    ?assertEqual(0.0, ltc_dynamics:reset_state()).

%%==============================================================================
%% CfC Closed-Form Evaluation Tests
%%==============================================================================

evaluate_cfc_zero_input_test() ->
    %% With zero input, state should decay toward 0
    {NewState, Output} = ltc_dynamics:evaluate_cfc(0.0, 0.5, 1.0, 1.0),
    %% State should move toward h (tanh(0) = 0)
    ?assert(abs(NewState) < 0.5),
    ?assertEqual(NewState, Output).

evaluate_cfc_positive_input_test() ->
    %% With positive input, state should move toward positive h
    {NewState, Output} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 1.0, 1.0),
    ?assert(NewState > 0),
    ?assertEqual(NewState, Output).

evaluate_cfc_negative_input_test() ->
    %% With negative input, state should move toward negative h
    {NewState, Output} = ltc_dynamics:evaluate_cfc(-1.0, 0.0, 1.0, 1.0),
    ?assert(NewState < 0),
    ?assertEqual(NewState, Output).

evaluate_cfc_respects_bounds_test() ->
    %% Even with large inputs, state should be clamped
    {NewState, _} = ltc_dynamics:evaluate_cfc(100.0, 0.0, 1.0, 1.0),
    ?assert(NewState =< 1.0 andalso NewState >= -1.0).

evaluate_cfc_vector_input_test() ->
    %% Should handle list input by summing
    {NewState, _} = ltc_dynamics:evaluate_cfc([0.5, 0.5], 0.0, 1.0, 1.0),
    %% Sum is 1.0, same as scalar 1.0
    {ScalarState, _} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 1.0, 1.0),
    ?assertEqual(NewState, ScalarState).

evaluate_cfc_empty_list_input_test() ->
    %% Empty list should be treated as 0
    {NewState, _} = ltc_dynamics:evaluate_cfc([], 0.0, 1.0, 1.0),
    {ZeroState, _} = ltc_dynamics:evaluate_cfc(0.0, 0.0, 1.0, 1.0),
    ?assertEqual(NewState, ZeroState).

evaluate_cfc_with_params_test() ->
    %% With custom weights
    Params = #{backbone_weights => [0.5], head_weights => [0.5]},
    {NewState, Output} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 1.0, 1.0, Params),
    ?assert(is_float(NewState)),
    ?assertEqual(NewState, Output).

evaluate_cfc_tau_effect_test() ->
    %% Larger tau should result in slower state change
    {State1, _} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 0.5, 1.0),
    {State2, _} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 2.0, 1.0),
    %% With smaller tau (faster), state change should be larger
    ?assert(abs(State1) >= abs(State2)).

%%==============================================================================
%% ODE-Based Evaluation Tests
%%==============================================================================

evaluate_ode_zero_input_test() ->
    %% With zero input, state should decay toward 0
    {NewState, Output} = ltc_dynamics:evaluate_ode(0.0, 0.5, 1.0, 1.0, 0.1),
    ?assert(abs(NewState) < 0.5),
    ?assertEqual(NewState, Output).

evaluate_ode_positive_input_test() ->
    {NewState, Output} = ltc_dynamics:evaluate_ode(1.0, 0.0, 1.0, 1.0, 0.1),
    ?assert(NewState > 0),
    ?assertEqual(NewState, Output).

evaluate_ode_dt_effect_test() ->
    %% Larger dt should result in larger state change per step
    {State1, _} = ltc_dynamics:evaluate_ode(1.0, 0.0, 1.0, 1.0, 0.01),
    {State2, _} = ltc_dynamics:evaluate_ode(1.0, 0.0, 1.0, 1.0, 0.1),
    ?assert(abs(State2) > abs(State1)).

evaluate_ode_respects_bounds_test() ->
    {NewState, _} = ltc_dynamics:evaluate_ode(100.0, 0.0, 1.0, 1.0, 0.1),
    ?assert(NewState =< 1.0 andalso NewState >= -1.0).

evaluate_ode_with_params_test() ->
    Params = #{liquid_weights => [0.5]},
    {NewState, Output} = ltc_dynamics:evaluate_ode(1.0, 0.0, 1.0, 1.0, 0.1, Params),
    ?assert(is_float(NewState)),
    ?assertEqual(NewState, Output).

%%==============================================================================
%% Backbone Network Tests
%%==============================================================================

compute_backbone_simple_test() ->
    %% Simple mode (no weights) should return sigmoid(input/tau)
    Result = ltc_dynamics:compute_backbone(0.0, 1.0, []),
    ?assertEqual(0.5, Result).

compute_backbone_with_weights_test() ->
    %% With weights, should apply weighted nonlinearity
    Result = ltc_dynamics:compute_backbone(1.0, 1.0, [0.5]),
    ?assert(is_float(Result)),
    ?assert(Result >= -1.0 andalso Result =< 1.0).

compute_backbone_list_input_test() ->
    Result = ltc_dynamics:compute_backbone([0.5, 0.5], 1.0, []),
    ScalarResult = ltc_dynamics:compute_backbone(1.0, 1.0, []),
    ?assertEqual(Result, ScalarResult).

%%==============================================================================
%% Head Network Tests
%%==============================================================================

compute_head_simple_test() ->
    %% Simple mode should return tanh(input)
    Result = ltc_dynamics:compute_head(0.0, []),
    ?assertEqual(0.0, Result).

compute_head_positive_test() ->
    Result = ltc_dynamics:compute_head(1.0, []),
    ?assert(Result > 0.76 andalso Result < 0.77).

compute_head_with_weights_test() ->
    Result = ltc_dynamics:compute_head(1.0, [2.0]),
    ?assert(is_float(Result)),
    ?assert(Result >= -1.0 andalso Result =< 1.0).

compute_head_list_input_test() ->
    Result = ltc_dynamics:compute_head([0.5, 0.5], []),
    ScalarResult = ltc_dynamics:compute_head(1.0, []),
    ?assertEqual(Result, ScalarResult).

%%==============================================================================
%% Time Constant Computation Tests
%%==============================================================================

compute_liquid_tau_simple_test() ->
    %% With zero input, modulation is 1 + sigmoid(0) = 1.5
    Tau = ltc_dynamics:compute_liquid_tau(0.0, 0.0, 1.0, #{}),
    ?assertEqual(1.5, Tau).

compute_liquid_tau_positive_input_test() ->
    %% With positive input, tau should increase
    Tau = ltc_dynamics:compute_liquid_tau(2.0, 0.0, 1.0, #{}),
    ?assert(Tau > 1.5).

compute_liquid_tau_negative_input_test() ->
    %% With negative input, tau should decrease (but stay positive)
    Tau = ltc_dynamics:compute_liquid_tau(-2.0, 0.0, 1.0, #{}),
    ?assert(Tau > 0 andalso Tau < 1.5).

compute_liquid_tau_min_bound_test() ->
    %% Tau should never go below 0.001
    Tau = ltc_dynamics:compute_liquid_tau(-100.0, 0.0, 0.001, #{}),
    ?assert(Tau >= 0.001).

compute_liquid_tau_max_bound_test() ->
    %% Tau should never exceed 100.0
    Tau = ltc_dynamics:compute_liquid_tau(100.0, 0.0, 100.0, #{}),
    ?assert(Tau =< 100.0).

compute_liquid_tau_with_weights_test() ->
    Params = #{liquid_weights => [0.5]},
    Tau = ltc_dynamics:compute_liquid_tau(1.0, 0.5, 1.0, Params),
    ?assert(is_float(Tau)),
    ?assert(Tau > 0).

%%==============================================================================
%% Integration Tests (Multi-Step Evaluation)
%%==============================================================================

cfc_convergence_test() ->
    %% With constant input, state should converge
    Steps = 100,
    Input = 1.0,
    Tau = 1.0,
    Bound = 1.0,

    %% Run multiple steps
    FinalState = lists:foldl(
        fun(_, State) ->
            {NewState, _} = ltc_dynamics:evaluate_cfc(Input, State, Tau, Bound),
            NewState
        end,
        0.0,
        lists:seq(1, Steps)
    ),

    %% State should have converged toward tanh(1.0) ≈ 0.76
    ?assert(FinalState > 0.7 andalso FinalState < 0.8).

ode_convergence_test() ->
    %% With constant input, ODE should converge toward target
    Steps = 100,
    Input = 1.0,
    Tau = 1.0,
    Bound = 1.0,
    Dt = 0.1,

    FinalState = lists:foldl(
        fun(_, State) ->
            {NewState, _} = ltc_dynamics:evaluate_ode(Input, State, Tau, Bound, Dt),
            NewState
        end,
        0.0,
        lists:seq(1, Steps)
    ),

    %% Target is Bound * tanh(Input) = 1.0 * tanh(1.0) ≈ 0.76
    ?assert(FinalState > 0.7 andalso FinalState < 0.8).

cfc_vs_ode_consistency_test() ->
    %% CfC and ODE should produce qualitatively similar results
    Input = 1.0,
    InitState = 0.0,
    Tau = 1.0,
    Bound = 1.0,

    {CfcState, _} = ltc_dynamics:evaluate_cfc(Input, InitState, Tau, Bound),
    {OdeState, _} = ltc_dynamics:evaluate_ode(Input, InitState, Tau, Bound, 0.1),

    %% Both should be positive and in same general direction
    ?assert(CfcState > 0),
    ?assert(OdeState > 0).

state_persistence_test() ->
    %% State should persist and affect next computation
    {State1, _} = ltc_dynamics:evaluate_cfc(1.0, 0.0, 1.0, 1.0),
    {State2, _} = ltc_dynamics:evaluate_cfc(0.0, State1, 1.0, 1.0),

    %% With zero input but positive initial state, new state should be positive
    %% but smaller (decaying toward 0)
    ?assert(State2 > 0),
    ?assert(State2 < State1).
