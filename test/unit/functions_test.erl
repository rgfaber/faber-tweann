%% @module functions_test
%% @doc Unit tests for functions module
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0

-module(functions_test).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% tanh tests
%%==============================================================================

tanh_zero_test() ->
    ?assertEqual(0.0, functions:tanh(0.0)).

tanh_positive_test() ->
    Result = functions:tanh(1.0),
    ?assert(Result > 0.76 andalso Result < 0.77).

tanh_negative_test() ->
    Result = functions:tanh(-1.0),
    ?assert(Result > -0.77 andalso Result < -0.76).

tanh_large_positive_test() ->
    Result = functions:tanh(10.0),
    ?assert(Result > 0.999).

tanh_large_negative_test() ->
    Result = functions:tanh(-10.0),
    ?assert(Result < -0.999).

%%==============================================================================
%% sin tests
%%==============================================================================

sin_zero_test() ->
    ?assertEqual(0.0, functions:sin(0.0)).

sin_pi_half_test() ->
    Result = functions:sin(math:pi() / 2),
    ?assert(abs(Result - 1.0) < 0.0001).

sin_pi_test() ->
    Result = functions:sin(math:pi()),
    ?assert(abs(Result) < 0.0001).

sin_negative_test() ->
    Result = functions:sin(-math:pi() / 2),
    ?assert(abs(Result + 1.0) < 0.0001).

%%==============================================================================
%% cos tests
%%==============================================================================

cos_zero_test() ->
    ?assertEqual(1.0, functions:cos(0.0)).

cos_pi_half_test() ->
    Result = functions:cos(math:pi() / 2),
    ?assert(abs(Result) < 0.0001).

cos_pi_test() ->
    Result = functions:cos(math:pi()),
    ?assert(abs(Result + 1.0) < 0.0001).

%%==============================================================================
%% gaussian tests
%%==============================================================================

gaussian_zero_test() ->
    Result = functions:gaussian(0.0),
    ?assertEqual(1.0, Result).

gaussian_positive_test() ->
    Result = functions:gaussian(1.0),
    ?assert(Result > 0.36 andalso Result < 0.37).

gaussian_negative_test() ->
    Result = functions:gaussian(-1.0),
    %% Gaussian is symmetric
    Expected = functions:gaussian(1.0),
    ?assert(abs(Result - Expected) < 0.0001).

gaussian_large_input_clamped_test() ->
    %% Large inputs should be clamped to prevent underflow
    Result = functions:gaussian(100.0),
    ?assert(Result >= 0.0 andalso Result < 0.0001).

gaussian_custom_base_test() ->
    Result = functions:gaussian(2.0, 1.0),
    Expected = math:pow(2.0, -1.0),
    ?assert(abs(Result - Expected) < 0.0001).

%%==============================================================================
%% sigmoid tests
%%==============================================================================

sigmoid_zero_test() ->
    ?assertEqual(0.5, functions:sigmoid(0.0)).

sigmoid_positive_test() ->
    Result = functions:sigmoid(2.0),
    ?assert(Result > 0.88 andalso Result < 0.89).

sigmoid_negative_test() ->
    Result = functions:sigmoid(-2.0),
    ?assert(Result > 0.11 andalso Result < 0.12).

sigmoid_large_positive_test() ->
    Result = functions:sigmoid(100.0),
    ?assert(Result > 0.9999).

sigmoid_large_negative_test() ->
    Result = functions:sigmoid(-100.0),
    ?assert(Result < 0.0001).

%%==============================================================================
%% sigmoid1 tests
%%==============================================================================

sigmoid1_zero_test() ->
    ?assertEqual(0.0, functions:sigmoid1(0.0)).

sigmoid1_positive_test() ->
    Result = functions:sigmoid1(1.0),
    Expected = 1.0 / 2.0,
    ?assertEqual(Expected, Result).

sigmoid1_negative_test() ->
    Result = functions:sigmoid1(-1.0),
    Expected = -1.0 / 2.0,
    ?assertEqual(Expected, Result).

sigmoid1_large_value_test() ->
    Result = functions:sigmoid1(100.0),
    Expected = 100.0 / 101.0,
    ?assert(abs(Result - Expected) < 0.0001).

%%==============================================================================
%% sgn tests
%%==============================================================================

sgn_positive_test() ->
    ?assertEqual(1, functions:sgn(5.0)).

sgn_negative_test() ->
    ?assertEqual(-1, functions:sgn(-5.0)).

sgn_zero_float_test() ->
    ?assertEqual(0, functions:sgn(0.0)).

sgn_zero_int_test() ->
    ?assertEqual(0, functions:sgn(0)).

sgn_small_positive_test() ->
    ?assertEqual(1, functions:sgn(0.001)).

sgn_small_negative_test() ->
    ?assertEqual(-1, functions:sgn(-0.001)).

%%==============================================================================
%% bin tests
%%==============================================================================

bin_positive_test() ->
    ?assertEqual(1, functions:bin(1.0)).

bin_negative_test() ->
    ?assertEqual(0, functions:bin(-1.0)).

bin_zero_test() ->
    ?assertEqual(0, functions:bin(0.0)).

bin_small_positive_test() ->
    ?assertEqual(1, functions:bin(0.001)).

%%==============================================================================
%% trinary tests
%%==============================================================================

trinary_high_test() ->
    ?assertEqual(1, functions:trinary(0.5)).

trinary_low_test() ->
    ?assertEqual(-1, functions:trinary(-0.5)).

trinary_middle_test() ->
    ?assertEqual(0, functions:trinary(0.0)).

trinary_boundary_high_test() ->
    ?assertEqual(1, functions:trinary(0.33)).

trinary_boundary_low_test() ->
    ?assertEqual(-1, functions:trinary(-0.33)).

trinary_inside_deadzone_test() ->
    ?assertEqual(0, functions:trinary(0.32)).

%%==============================================================================
%% multiquadric tests
%%==============================================================================

multiquadric_zero_test() ->
    Result = functions:multiquadric(0.0),
    Expected = math:sqrt(0.01),
    ?assert(abs(Result - Expected) < 0.0001).

multiquadric_positive_test() ->
    Result = functions:multiquadric(1.0),
    Expected = math:sqrt(1.01),
    ?assert(abs(Result - Expected) < 0.0001).

multiquadric_negative_test() ->
    Result = functions:multiquadric(-1.0),
    Expected = math:sqrt(1.01),
    ?assert(abs(Result - Expected) < 0.0001).

%%==============================================================================
%% absolute tests
%%==============================================================================

absolute_positive_test() ->
    ?assertEqual(5.0, functions:absolute(5.0)).

absolute_negative_test() ->
    ?assertEqual(5.0, functions:absolute(-5.0)).

absolute_zero_test() ->
    ?assertEqual(0.0, functions:absolute(0.0)).

%%==============================================================================
%% linear tests
%%==============================================================================

linear_positive_test() ->
    ?assertEqual(5.0, functions:linear(5.0)).

linear_negative_test() ->
    ?assertEqual(-5.0, functions:linear(-5.0)).

linear_zero_test() ->
    ?assertEqual(0.0, functions:linear(0.0)).

%%==============================================================================
%% quadratic tests
%%==============================================================================

quadratic_positive_test() ->
    Result = functions:quadratic(3.0),
    ?assertEqual(9.0, Result).

quadratic_negative_test() ->
    Result = functions:quadratic(-3.0),
    ?assertEqual(-9.0, Result).

quadratic_zero_test() ->
    ?assertEqual(0.0, functions:quadratic(0.0)).

%%==============================================================================
%% sqrt tests
%%==============================================================================

sqrt_positive_test() ->
    Result = functions:sqrt(4.0),
    ?assertEqual(2.0, Result).

sqrt_negative_test() ->
    Result = functions:sqrt(-4.0),
    ?assertEqual(-2.0, Result).

sqrt_zero_test() ->
    ?assertEqual(0.0, functions:sqrt(0.0)).

%%==============================================================================
%% log tests
%%==============================================================================

log_e_test() ->
    Result = functions:log(math:exp(1)),
    ?assert(abs(Result - 1.0) < 0.0001).

log_negative_test() ->
    Result = functions:log(-math:exp(1)),
    ?assert(abs(Result + 1.0) < 0.0001).

log_zero_float_test() ->
    ?assertEqual(0.0, functions:log(0.0)).

log_zero_int_test() ->
    ?assertEqual(0.0, functions:log(0)).

log_one_test() ->
    ?assertEqual(0.0, functions:log(1.0)).

%%==============================================================================
%% saturation tests
%%==============================================================================

saturation_within_range_test() ->
    ?assertEqual(500.0, functions:saturation(500.0)).

saturation_above_max_test() ->
    ?assertEqual(1000, functions:saturation(2000.0)).

saturation_below_min_test() ->
    ?assertEqual(-1000, functions:saturation(-2000.0)).

saturation_custom_spread_test() ->
    ?assertEqual(50.0, functions:saturation(50.0, 100.0)).

saturation_custom_above_test() ->
    ?assertEqual(100.0, functions:saturation(150.0, 100.0)).

saturation_custom_below_test() ->
    ?assertEqual(-100.0, functions:saturation(-150.0, 100.0)).

%%==============================================================================
%% sat tests
%%==============================================================================

sat_within_range_test() ->
    ?assertEqual(5.0, functions:sat(5.0, 10.0, 0.0)).

sat_above_max_test() ->
    ?assertEqual(10.0, functions:sat(15.0, 10.0, 0.0)).

sat_below_min_test() ->
    ?assertEqual(0.0, functions:sat(-5.0, 10.0, 0.0)).

sat_asymmetric_range_test() ->
    ?assertEqual(-5.0, functions:sat(-10.0, 10.0, -5.0)).

%%==============================================================================
%% sat_dzone tests
%%==============================================================================

sat_dzone_in_deadzone_test() ->
    ?assertEqual(0, functions:sat_dzone(0.1, 1.0, -1.0, 0.2, -0.2)).

sat_dzone_above_deadzone_test() ->
    ?assertEqual(0.5, functions:sat_dzone(0.5, 1.0, -1.0, 0.2, -0.2)).

sat_dzone_below_deadzone_test() ->
    ?assertEqual(-0.5, functions:sat_dzone(-0.5, 1.0, -1.0, 0.2, -0.2)).

sat_dzone_clamped_above_test() ->
    ?assertEqual(1.0, functions:sat_dzone(2.0, 1.0, -1.0, 0.2, -0.2)).

sat_dzone_clamped_below_test() ->
    ?assertEqual(-1.0, functions:sat_dzone(-2.0, 1.0, -1.0, 0.2, -0.2)).

%%==============================================================================
%% scale/3 tests
%%==============================================================================

scale_middle_value_test() ->
    Result = functions:scale(5.0, 10.0, 0.0),
    ?assertEqual(0.0, Result).

scale_min_value_test() ->
    Result = functions:scale(0.0, 10.0, 0.0),
    ?assertEqual(-1.0, Result).

scale_max_value_test() ->
    Result = functions:scale(10.0, 10.0, 0.0),
    ?assertEqual(1.0, Result).

scale_list_test() ->
    Result = functions:scale([0.0, 5.0, 10.0], 10.0, 0.0),
    ?assertEqual([-1.0, 0.0, 1.0], Result).

scale_equal_min_max_test() ->
    %% When min == max, return 0.0
    Result = functions:scale(5.0, 5.0, 5.0),
    ?assertEqual(0.0, Result).

%%==============================================================================
%% scale/5 tests
%%==============================================================================

scale_range_mapping_test() ->
    %% Scale 50 from [0, 100] to [0, 1]
    Result = functions:scale(50.0, 0.0, 100.0, 0.0, 1.0),
    ?assertEqual(0.5, Result).

scale_range_min_test() ->
    Result = functions:scale(0.0, 0.0, 100.0, 0.0, 1.0),
    ?assertEqual(0.0, Result).

scale_range_max_test() ->
    Result = functions:scale(100.0, 0.0, 100.0, 0.0, 1.0),
    ?assertEqual(1.0, Result).

scale_negative_range_test() ->
    %% Scale from [-1, 1] to [0, 100]
    Result = functions:scale(0.0, -1.0, 1.0, 0.0, 100.0),
    ?assertEqual(50.0, Result).

%%==============================================================================
%% avg tests
%%==============================================================================

avg_simple_test() ->
    Result = functions:avg([1.0, 2.0, 3.0]),
    ?assertEqual(2.0, Result).

avg_single_element_test() ->
    Result = functions:avg([5.0]),
    ?assertEqual(5.0, Result).

avg_negative_values_test() ->
    Result = functions:avg([-1.0, 1.0]),
    ?assertEqual(0.0, Result).

%%==============================================================================
%% std tests
%%==============================================================================

std_no_variance_test() ->
    Result = functions:std([5.0, 5.0, 5.0]),
    ?assertEqual(0.0, Result).

std_simple_test() ->
    Result = functions:std([1.0, 2.0, 3.0]),
    %% Variance = ((1-2)^2 + (2-2)^2 + (3-2)^2) / 3 = 2/3
    %% Std = sqrt(2/3) ≈ 0.8165
    ?assert(abs(Result - 0.8165) < 0.001).

std_larger_spread_test() ->
    Result = functions:std([0.0, 10.0]),
    %% Avg = 5, Variance = (25 + 25) / 2 = 25, Std = 5
    ?assertEqual(5.0, Result).
