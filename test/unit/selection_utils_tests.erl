%% @doc Unit tests for selection_utils module.
-module(selection_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Roulette Wheel Tests
%% ============================================================================

roulette_wheel_single_item_test() ->
    Items = [{item1, 1.0}],
    Result = selection_utils:roulette_wheel(Items),
    ?assertEqual(item1, Result).

roulette_wheel_selects_higher_weight_more_often_test() ->
    %% Heavy item should be selected more often
    Items = [{light, 1.0}, {heavy, 99.0}],

    %% Run many selections
    Selections = [selection_utils:roulette_wheel(Items) || _ <- lists:seq(1, 1000)],

    HeavyCount = length([S || S <- Selections, S == heavy]),
    LightCount = length([S || S <- Selections, S == light]),

    %% Heavy should be selected significantly more often
    ?assert(HeavyCount > LightCount * 5).

roulette_wheel_empty_list_test() ->
    ?assertError({selection_failed, empty_list}, selection_utils:roulette_wheel([])).

roulette_wheel_zero_weights_test() ->
    %% Should fall back to random selection
    Items = [{a, 0}, {b, 0}, {c, 0}],
    Result = selection_utils:roulette_wheel(Items),
    ?assert(lists:member(Result, [a, b, c])).

roulette_wheel_mixed_weights_test() ->
    %% Mix of weights
    Items = [{a, 10}, {b, 20}, {c, 70}],
    Result = selection_utils:roulette_wheel(Items),
    ?assert(lists:member(Result, [a, b, c])).

%% ============================================================================
%% Random Select Tests
%% ============================================================================

random_select_single_test() ->
    ?assertEqual(item1, selection_utils:random_select([item1])).

random_select_returns_list_element_test() ->
    Items = [a, b, c, d, e],
    Result = selection_utils:random_select(Items),
    ?assert(lists:member(Result, Items)).

random_select_empty_list_test() ->
    ?assertError({selection_failed, empty_list}, selection_utils:random_select([])).

random_select_distribution_test() ->
    %% Should be roughly uniform
    Items = [a, b, c],
    Selections = [selection_utils:random_select(Items) || _ <- lists:seq(1, 3000)],

    CountA = length([S || S <- Selections, S == a]),
    CountB = length([S || S <- Selections, S == b]),
    CountC = length([S || S <- Selections, S == c]),

    %% Each should be selected roughly 1000 times (+/- 200)
    ?assert(CountA > 700 andalso CountA < 1300),
    ?assert(CountB > 700 andalso CountB < 1300),
    ?assert(CountC > 700 andalso CountC < 1300).

%% ============================================================================
%% Weighted Select Tests
%% ============================================================================

weighted_select_single_test() ->
    Result = selection_utils:weighted_select([item1], fun(_) -> 1.0 end),
    ?assertEqual(item1, Result).

weighted_select_empty_list_test() ->
    ?assertError({selection_failed, empty_list},
                 selection_utils:weighted_select([], fun(_) -> 1.0 end)).

weighted_select_uses_weight_function_test() ->
    %% Items with different computed weights
    Items = [1, 10, 100],
    WeightFun = fun(X) -> X end,

    %% Run selections
    Selections = [selection_utils:weighted_select(Items, WeightFun) || _ <- lists:seq(1, 1000)],

    Count100 = length([S || S <- Selections, S == 100]),
    Count1 = length([S || S <- Selections, S == 1]),

    %% 100 should be selected much more often than 1
    ?assert(Count100 > Count1 * 10).
