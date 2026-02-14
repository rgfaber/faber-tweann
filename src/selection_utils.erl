%% @doc Selection utilities for evolutionary algorithms.
%%
%% Provides selection mechanisms used throughout the evolution process:
%% - Roulette wheel selection (fitness-proportionate)
%% - Random uniform selection
%% - Weighted selection
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(selection_utils).

%% Suppress supertype warnings - specs are intentionally general for API flexibility
-dialyzer({nowarn_function, [random_select/1]}).

-export([
    roulette_wheel/1,
    random_select/1,
    weighted_select/2
]).

%% @doc Roulette wheel selection based on weights.
%%
%% Selects an element with probability proportional to its weight.
%% Higher weight = higher probability of selection.
%%
%% Algorithm:
%% 1. Calculate total weight
%% 2. Generate random value in [0, total)
%% 3. Accumulate weights until random value exceeded
%%
%% @param WeightedItems list of {Item, Weight} tuples
%% @returns selected Item
-spec roulette_wheel([{term(), number()}]) -> term().
roulette_wheel([]) ->
    error({selection_failed, empty_list});
roulette_wheel(WeightedItems) ->
    TotalWeight = lists:sum([W || {_, W} <- WeightedItems]),
    case TotalWeight of
        0 ->
            %% All zero weights - fall back to random selection
            {Item, _} = lists:nth(rand:uniform(length(WeightedItems)), WeightedItems),
            Item;
        _ ->
            RandomValue = rand:uniform() * TotalWeight,
            select_by_accumulated_weight(WeightedItems, RandomValue, 0)
    end.

%% @private Accumulate weights until we exceed target
-spec select_by_accumulated_weight([{term(), number()}], number(), number()) -> term().
select_by_accumulated_weight([{Item, Weight} | Rest], Target, Acc) ->
    NewAcc = Acc + Weight,
    case NewAcc >= Target of
        true -> Item;
        false -> select_by_accumulated_weight(Rest, Target, NewAcc)
    end;
select_by_accumulated_weight([], _, _) ->
    error({selection_failed, accumulation_error}).

%% @doc Uniformly select a random element from a list.
%%
%% Each element has equal probability of selection.
%%
%% @param Items list of items to select from
%% @returns randomly selected Item
-spec random_select([term()]) -> term().
random_select([]) ->
    error({selection_failed, empty_list});
random_select(Items) ->
    Index = rand:uniform(length(Items)),
    lists:nth(Index, Items).

%% @doc Select element with custom weight function.
%%
%% Applies a weight function to each element, then performs
%% roulette wheel selection.
%%
%% @param Items list of items
%% @param WeightFun function that returns weight for an item
%% @returns selected Item
-spec weighted_select([term()], fun((term()) -> number())) -> term().
weighted_select([], _WeightFun) ->
    error({selection_failed, empty_list});
weighted_select(Items, WeightFun) ->
    WeightedItems = [{Item, WeightFun(Item)} || Item <- Items],
    roulette_wheel(WeightedItems).
