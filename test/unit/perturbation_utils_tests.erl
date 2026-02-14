%% @doc Unit tests for perturbation_utils module.
-module(perturbation_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% Saturation limit (must match module define)
-define(SAT_LIMIT, math:pi() * 10).

%% ============================================================================
%% Weight Perturbation Tests
%% ============================================================================

perturb_weight_changes_value_test() ->
    Original = {0.5, 0.0, 0.1, []},
    Spread = 0.5,
    {NewW, NewDW, LP, LPs} = perturbation_utils:perturb_weight(Original, Spread),

    %% Learning rate and params should be unchanged
    ?assertEqual(0.1, LP),
    ?assertEqual([], LPs),

    %% Delta weight should be non-zero (with very high probability)
    ?assert(NewDW /= 0.0 orelse NewW /= 0.5).

perturb_weight_preserves_params_test() ->
    Original = {0.5, 0.1, 0.2, [param1, param2]},
    Spread = 0.3,
    {_NewW, _NewDW, LP, LPs} = perturbation_utils:perturb_weight(Original, Spread),

    ?assertEqual(0.2, LP),
    ?assertEqual([param1, param2], LPs).

perturb_weight_within_saturation_test() ->
    %% Even with extreme values, should be saturated
    Original = {?SAT_LIMIT, 0.0, 0.1, []},
    Spread = 100.0,
    {NewW, _, _, _} = perturbation_utils:perturb_weight(Original, Spread),

    ?assert(abs(NewW) =< ?SAT_LIMIT).

perturb_weight_negative_saturation_test() ->
    %% Test negative saturation
    Original = {-?SAT_LIMIT, 0.0, 0.1, []},
    Spread = 100.0,
    {NewW, _, _, _} = perturbation_utils:perturb_weight(Original, Spread),

    ?assert(abs(NewW) =< ?SAT_LIMIT).

perturb_weight_momentum_test() ->
    %% Previous delta weight should influence new delta
    WithMomentum = {0.0, 2.0, 0.1, []},  % Large previous DW
    WithoutMomentum = {0.0, 0.0, 0.1, []},
    Spread = 0.1,

    %% Run multiple times and average
    Results1 = [begin
        {_, DW, _, _} = perturbation_utils:perturb_weight(WithMomentum, Spread),
        DW
    end || _ <- lists:seq(1, 100)],

    Results2 = [begin
        {_, DW, _, _} = perturbation_utils:perturb_weight(WithoutMomentum, Spread),
        DW
    end || _ <- lists:seq(1, 100)],

    Avg1 = lists:sum(Results1) / 100,
    Avg2 = lists:sum(Results2) / 100,

    %% With momentum should have higher average DW (2.0 * 0.5 = 1.0 base)
    ?assert(Avg1 > Avg2).

perturb_weight_zero_spread_test() ->
    %% Zero spread should only apply momentum
    Original = {0.5, 0.2, 0.1, []},
    Spread = 0.0,
    {NewW, NewDW, _, _} = perturbation_utils:perturb_weight(Original, Spread),

    %% NewDW should be roughly 0.2 * 0.5 = 0.1
    ?assert(abs(NewDW - 0.1) < 0.01),
    %% NewW should be roughly 0.5 + 0.1 = 0.6
    ?assert(abs(NewW - 0.6) < 0.01).

%% ============================================================================
%% Batch Perturbation Tests
%% ============================================================================

perturb_weights_all_changed_test() ->
    Weights = [
        {0.5, 0.0, 0.1, []},
        {0.3, 0.0, 0.1, []},
        {0.7, 0.0, 0.1, []}
    ],
    Spread = 0.5,

    Perturbed = perturbation_utils:perturb_weights(Weights, Spread),

    ?assertEqual(length(Weights), length(Perturbed)).

perturb_weights_empty_test() ->
    ?assertEqual([], perturbation_utils:perturb_weights([], 0.5)).

perturb_weights_single_test() ->
    Weights = [{0.5, 0.0, 0.1, []}],
    Perturbed = perturbation_utils:perturb_weights(Weights, 0.3),
    ?assertEqual(1, length(Perturbed)).

%% ============================================================================
%% Saturation Tests
%% ============================================================================

sat_within_range_test() ->
    ?assertEqual(0.5, perturbation_utils:sat(0.5, 1.0)).

sat_above_max_test() ->
    ?assertEqual(1.0, perturbation_utils:sat(2.0, 1.0)).

sat_below_min_test() ->
    ?assertEqual(-1.0, perturbation_utils:sat(-2.0, 1.0)).

sat_at_boundary_test() ->
    ?assertEqual(1.0, perturbation_utils:sat(1.0, 1.0)),
    ?assertEqual(-1.0, perturbation_utils:sat(-1.0, 1.0)).

sat_zero_test() ->
    ?assertEqual(0.0, perturbation_utils:sat(0.0, 1.0)).
