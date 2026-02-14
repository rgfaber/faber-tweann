%% @doc Unit tests for activation and utility functions.
-module(functions_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

functions_exports_test() ->
    Exports = functions:module_info(exports),
    %% Activation functions
    ?assert(lists:member({tanh, 1}, Exports)),
    ?assert(lists:member({sin, 1}, Exports)),
    ?assert(lists:member({cos, 1}, Exports)),
    ?assert(lists:member({gaussian, 1}, Exports)),
    ?assert(lists:member({gaussian, 2}, Exports)),
    ?assert(lists:member({sigmoid, 1}, Exports)),
    ?assert(lists:member({sigmoid1, 1}, Exports)),
    ?assert(lists:member({sgn, 1}, Exports)),
    ?assert(lists:member({bin, 1}, Exports)),
    ?assert(lists:member({trinary, 1}, Exports)),
    ?assert(lists:member({multiquadric, 1}, Exports)),
    ?assert(lists:member({absolute, 1}, Exports)),
    ?assert(lists:member({linear, 1}, Exports)),
    ?assert(lists:member({quadratic, 1}, Exports)),
    ?assert(lists:member({cubic, 1}, Exports)),
    ?assert(lists:member({sqrt, 1}, Exports)),
    ?assert(lists:member({log, 1}, Exports)),
    ?assert(lists:member({relu, 1}, Exports)),
    %% Utility functions
    ?assert(lists:member({saturation, 1}, Exports)),
    ?assert(lists:member({saturation, 2}, Exports)),
    ?assert(lists:member({sat, 3}, Exports)),
    ?assert(lists:member({sat_dzone, 5}, Exports)),
    ?assert(lists:member({scale, 3}, Exports)),
    ?assert(lists:member({scale, 5}, Exports)),
    %% Statistics
    ?assert(lists:member({avg, 1}, Exports)),
    ?assert(lists:member({std, 1}, Exports)).

%% ============================================================================
%% Tanh Tests
%% ============================================================================

tanh_zero_test() ->
    ?assertEqual(0.0, functions:tanh(0.0)).

tanh_positive_test() ->
    Result = functions:tanh(1.0),
    ?assert(Result > 0.7),
    ?assert(Result < 0.8).

tanh_negative_test() ->
    Result = functions:tanh(-1.0),
    ?assert(Result < -0.7),
    ?assert(Result > -0.8).

tanh_range_test() ->
    %% tanh should always be in [-1, 1]
    ?assert(functions:tanh(1000.0) =< 1.0),
    ?assert(functions:tanh(-1000.0) >= -1.0).

tanh_symmetry_test() ->
    %% tanh is an odd function: tanh(-x) = -tanh(x)
    ?assertEqual(-functions:tanh(0.5), functions:tanh(-0.5)).

%% ============================================================================
%% Sin/Cos Tests
%% ============================================================================

sin_zero_test() ->
    ?assertEqual(0.0, functions:sin(0.0)).

sin_pi_half_test() ->
    PiHalf = math:pi() / 2,
    ?assert(abs(functions:sin(PiHalf) - 1.0) < 0.0001).

cos_zero_test() ->
    ?assert(abs(functions:cos(0.0) - 1.0) < 0.0001).

cos_pi_test() ->
    Pi = math:pi(),
    ?assert(abs(functions:cos(Pi) - (-1.0)) < 0.0001).

sin_cos_identity_test() ->
    %% sin^2(x) + cos^2(x) = 1
    X = 0.7,
    SinX = functions:sin(X),
    CosX = functions:cos(X),
    ?assert(abs(SinX * SinX + CosX * CosX - 1.0) < 0.0001).

%% ============================================================================
%% Gaussian Tests
%% ============================================================================

gaussian_peak_test() ->
    %% gaussian(0) = 1
    ?assert(abs(functions:gaussian(0.0) - 1.0) < 0.0001).

gaussian_decay_test() ->
    %% gaussian decreases as we move from 0
    G0 = functions:gaussian(0.0),
    G1 = functions:gaussian(1.0),
    G2 = functions:gaussian(2.0),
    ?assert(G0 > G1),
    ?assert(G1 > G2).

gaussian_symmetry_test() ->
    %% gaussian is symmetric
    ?assertEqual(functions:gaussian(1.0), functions:gaussian(-1.0)).

gaussian_custom_base_test() ->
    %% Test with custom base
    Result = functions:gaussian(2.0, 1.0),
    ?assert(Result > 0.0),
    ?assert(Result < 1.0).

gaussian_clamped_test() ->
    %% Very large values should be clamped
    LargeResult = functions:gaussian(100.0),
    ?assert(LargeResult >= 0.0).

%% ============================================================================
%% Sigmoid Tests
%% ============================================================================

sigmoid_midpoint_test() ->
    %% sigmoid(0) = 0.5
    ?assert(abs(functions:sigmoid(0.0) - 0.5) < 0.0001).

sigmoid_positive_test() ->
    %% sigmoid(large) approaches 1
    ?assert(functions:sigmoid(10.0) > 0.99).

sigmoid_negative_test() ->
    %% sigmoid(large negative) approaches 0
    ?assert(functions:sigmoid(-10.0) < 0.01).

sigmoid_range_test() ->
    %% sigmoid is always in (0, 1)
    ?assert(functions:sigmoid(1000.0) > 0.0),
    ?assert(functions:sigmoid(1000.0) < 1.0),
    ?assert(functions:sigmoid(-1000.0) > 0.0),
    ?assert(functions:sigmoid(-1000.0) < 1.0).

sigmoid_monotonic_test() ->
    %% sigmoid is monotonically increasing
    ?assert(functions:sigmoid(-1.0) < functions:sigmoid(0.0)),
    ?assert(functions:sigmoid(0.0) < functions:sigmoid(1.0)).

%% ============================================================================
%% Sigmoid1 Tests
%% ============================================================================

sigmoid1_zero_test() ->
    %% sigmoid1(0) = 0
    ?assertEqual(0.0, functions:sigmoid1(0.0)).

sigmoid1_positive_test() ->
    Result = functions:sigmoid1(1.0),
    ?assertEqual(0.5, Result).

sigmoid1_negative_test() ->
    Result = functions:sigmoid1(-1.0),
    ?assertEqual(-0.5, Result).

sigmoid1_range_test() ->
    %% sigmoid1 is in (-1, 1)
    ?assert(functions:sigmoid1(100.0) < 1.0),
    ?assert(functions:sigmoid1(-100.0) > -1.0).

sigmoid1_antisymmetry_test() ->
    %% sigmoid1 is an odd function
    ?assertEqual(-functions:sigmoid1(2.0), functions:sigmoid1(-2.0)).

%% ============================================================================
%% Sign Function Tests
%% ============================================================================

sgn_zero_test() ->
    ?assertEqual(0, functions:sgn(0)),
    ?assertEqual(0, functions:sgn(0.0)).

sgn_positive_test() ->
    ?assertEqual(1, functions:sgn(1)),
    ?assertEqual(1, functions:sgn(0.5)),
    ?assertEqual(1, functions:sgn(100)).

sgn_negative_test() ->
    ?assertEqual(-1, functions:sgn(-1)),
    ?assertEqual(-1, functions:sgn(-0.5)),
    ?assertEqual(-1, functions:sgn(-100)).

%% ============================================================================
%% Binary Threshold Tests
%% ============================================================================

bin_positive_test() ->
    ?assertEqual(1, functions:bin(0.1)),
    ?assertEqual(1, functions:bin(1.0)),
    ?assertEqual(1, functions:bin(100)).

bin_zero_and_negative_test() ->
    ?assertEqual(0, functions:bin(0)),
    ?assertEqual(0, functions:bin(0.0)),
    ?assertEqual(0, functions:bin(-0.1)),
    ?assertEqual(0, functions:bin(-100)).

%% ============================================================================
%% Trinary Threshold Tests
%% ============================================================================

trinary_zero_zone_test() ->
    ?assertEqual(0, functions:trinary(0.0)),
    ?assertEqual(0, functions:trinary(0.32)),
    ?assertEqual(0, functions:trinary(-0.32)).

trinary_positive_test() ->
    ?assertEqual(1, functions:trinary(0.33)),
    ?assertEqual(1, functions:trinary(0.5)),
    ?assertEqual(1, functions:trinary(1.0)).

trinary_negative_test() ->
    ?assertEqual(-1, functions:trinary(-0.33)),
    ?assertEqual(-1, functions:trinary(-0.5)),
    ?assertEqual(-1, functions:trinary(-1.0)).

%% ============================================================================
%% Multiquadric Tests
%% ============================================================================

multiquadric_zero_test() ->
    %% multiquadric(0) = sqrt(0.01) = 0.1
    ?assert(abs(functions:multiquadric(0.0) - 0.1) < 0.0001).

multiquadric_positive_test() ->
    %% multiquadric(1) = sqrt(1.01) ~ 1.005
    Result = functions:multiquadric(1.0),
    ?assert(Result > 1.0),
    ?assert(Result < 1.01).

multiquadric_always_positive_test() ->
    ?assert(functions:multiquadric(0.0) > 0),
    ?assert(functions:multiquadric(-5.0) > 0),
    ?assert(functions:multiquadric(5.0) > 0).

%% ============================================================================
%% Absolute Value Tests
%% ============================================================================

absolute_positive_test() ->
    ?assertEqual(5.0, functions:absolute(5.0)),
    ?assertEqual(1, functions:absolute(1)).

absolute_negative_test() ->
    ?assertEqual(5.0, functions:absolute(-5.0)),
    ?assertEqual(1, functions:absolute(-1)).

absolute_zero_test() ->
    ?assertEqual(0, functions:absolute(0)),
    ?assertEqual(0.0, functions:absolute(0.0)).

%% ============================================================================
%% Linear Tests
%% ============================================================================

linear_identity_test() ->
    ?assertEqual(0.0, functions:linear(0.0)),
    ?assertEqual(5.0, functions:linear(5.0)),
    ?assertEqual(-3.14, functions:linear(-3.14)).

%% ============================================================================
%% Quadratic Tests
%% ============================================================================

quadratic_zero_test() ->
    ?assertEqual(0.0, functions:quadratic(0.0)).

quadratic_positive_test() ->
    ?assertEqual(4.0, functions:quadratic(2.0)).

quadratic_negative_test() ->
    %% quadratic(-2) = sgn(-2) * (-2)^2 = -1 * 4 = -4
    ?assertEqual(-4.0, functions:quadratic(-2.0)).

%% ============================================================================
%% Cubic Tests
%% ============================================================================

cubic_zero_test() ->
    ?assertEqual(0.0, functions:cubic(0.0)).

cubic_positive_test() ->
    ?assertEqual(8.0, functions:cubic(2.0)).

cubic_negative_test() ->
    ?assertEqual(-8.0, functions:cubic(-2.0)).

%% ============================================================================
%% Square Root Tests
%% ============================================================================

sqrt_zero_test() ->
    ?assertEqual(0.0, functions:sqrt(0.0)).

sqrt_positive_test() ->
    ?assert(abs(functions:sqrt(4.0) - 2.0) < 0.0001).

sqrt_negative_test() ->
    %% sqrt(-4) = sgn(-4) * sqrt(|-4|) = -1 * 2 = -2
    ?assert(abs(functions:sqrt(-4.0) - (-2.0)) < 0.0001).

%% ============================================================================
%% Logarithm Tests
%% ============================================================================

log_zero_test() ->
    ?assertEqual(0.0, functions:log(0)),
    ?assertEqual(0.0, functions:log(0.0)).

log_one_test() ->
    ?assertEqual(0.0, functions:log(1)).

log_e_test() ->
    E = 2.71828183,
    ?assert(abs(functions:log(E) - 1.0) < 0.0001).

log_negative_test() ->
    %% log(-e) = sgn(-e) * ln(|-e|) = -1 * 1 = -1
    E = 2.71828183,
    ?assert(abs(functions:log(-E) - (-1.0)) < 0.0001).

%% ============================================================================
%% ReLU Tests
%% ============================================================================

relu_positive_test() ->
    ?assertEqual(5.0, functions:relu(5.0)),
    ?assertEqual(0.1, functions:relu(0.1)).

relu_zero_test() ->
    ?assertEqual(0.0, functions:relu(0.0)).

relu_negative_test() ->
    ?assertEqual(0.0, functions:relu(-0.1)),
    ?assertEqual(0.0, functions:relu(-100.0)).

%% ============================================================================
%% Saturation Tests
%% ============================================================================

saturation_within_range_test() ->
    ?assertEqual(500, functions:saturation(500)),
    ?assertEqual(-500, functions:saturation(-500)).

saturation_overflow_test() ->
    ?assertEqual(1000, functions:saturation(5000)),
    ?assertEqual(-1000, functions:saturation(-5000)).

saturation_custom_spread_test() ->
    ?assertEqual(10, functions:saturation(50, 10)),
    ?assertEqual(-10, functions:saturation(-50, 10)),
    ?assertEqual(5, functions:saturation(5, 10)).

%% ============================================================================
%% Sat (Clamp) Tests
%% ============================================================================

sat_within_range_test() ->
    ?assertEqual(5, functions:sat(5, 10, -10)).

sat_above_max_test() ->
    ?assertEqual(10, functions:sat(15, 10, -10)).

sat_below_min_test() ->
    ?assertEqual(-10, functions:sat(-15, 10, -10)).

%% ============================================================================
%% Sat Dead Zone Tests
%% ============================================================================

sat_dzone_in_dead_zone_test() ->
    ?assertEqual(0, functions:sat_dzone(0.0, 10, -10, 0.5, -0.5)),
    ?assertEqual(0, functions:sat_dzone(0.3, 10, -10, 0.5, -0.5)),
    ?assertEqual(0, functions:sat_dzone(-0.3, 10, -10, 0.5, -0.5)).

sat_dzone_outside_dead_zone_test() ->
    ?assertEqual(5.0, functions:sat_dzone(5.0, 10, -10, 0.5, -0.5)),
    ?assertEqual(-5.0, functions:sat_dzone(-5.0, 10, -10, 0.5, -0.5)).

sat_dzone_clamped_test() ->
    ?assertEqual(10, functions:sat_dzone(15, 10, -10, 0.5, -0.5)),
    ?assertEqual(-10, functions:sat_dzone(-15, 10, -10, 0.5, -0.5)).

%% ============================================================================
%% Scale Tests
%% ============================================================================

scale_normalize_to_minus_one_to_one_test() ->
    %% Scale 50 from [0, 100] to [-1, 1]
    ?assertEqual(0.0, functions:scale(50, 100, 0)),
    ?assertEqual(1.0, functions:scale(100, 100, 0)),
    ?assertEqual(-1.0, functions:scale(0, 100, 0)).

scale_list_test() ->
    Result = functions:scale([0, 50, 100], 100, 0),
    ?assertEqual([-1.0, 0.0, 1.0], Result).

scale_equal_range_test() ->
    %% When max == min, return 0
    ?assertEqual(0.0, functions:scale(5, 5, 5)).

scale_range_to_range_test() ->
    %% Scale 5 from [0, 10] to [0, 100]
    Result = functions:scale(5, 0, 10, 0, 100),
    ?assertEqual(50.0, Result).

scale_range_to_range_edges_test() ->
    ?assertEqual(0.0, functions:scale(0, 0, 10, 0, 100)),
    ?assertEqual(100.0, functions:scale(10, 0, 10, 0, 100)).

%% ============================================================================
%% Statistics Tests
%% ============================================================================

avg_simple_test() ->
    ?assertEqual(2.0, functions:avg([1, 2, 3])).

avg_single_element_test() ->
    ?assertEqual(5.0, functions:avg([5])).

avg_floats_test() ->
    ?assertEqual(2.5, functions:avg([1.0, 2.0, 3.0, 4.0])).

std_uniform_test() ->
    %% All same values -> std = 0
    ?assertEqual(0.0, functions:std([5, 5, 5, 5])).

std_simple_test() ->
    %% [1, 2, 3] avg = 2
    %% variance = ((1-2)^2 + (2-2)^2 + (3-2)^2) / 3 = (1 + 0 + 1) / 3 = 2/3
    %% std = sqrt(2/3) ~ 0.8165
    Std = functions:std([1, 2, 3]),
    ?assert(abs(Std - 0.8165) < 0.001).
