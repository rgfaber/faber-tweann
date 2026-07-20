%% @doc Unit tests for the tuning_selection and tuning_duration modules.
-module(tuning_tests).
-include_lib("eunit/include/eunit.hrl").

%% tuning_selection with no genotype reads: `all' does not touch the database.
all_selects_every_neuron_test() ->
    Ids = [{{0, 1.0}, neuron}, {{0.5, 2.0}, neuron}],
    Chosen = tuning_selection:all(Ids, 0, 1.0, 0.5),
    ?assertEqual(2, length(Chosen)),
    [?assert(is_number(S)) || {_Id, S} <- Chosen].

all_spread_is_pi_scaled_test() ->
    Ids = [{{0, 1.0}, neuron}],
    [{_, Spread}] = tuning_selection:all(Ids, 0, 2.0, 0.5),
    ?assertEqual(2.0 * math:pi(), Spread).

unknown_selection_falls_back_to_dynamic_test() ->
    %% dynamic reads genotype; with an empty id list it must still not crash,
    %% and select/5 must route an unknown strategy somewhere valid.
    ?assertEqual([{i, 1.0 * math:pi()}],
                 tuning_selection:select(nonsense, [i], 0, 1.0, 0.5)
                   =:= tuning_selection:dynamic([i], 0, 1.0, 0.5)
                     andalso [{i, 1.0 * math:pi()}]).

%% tuning_duration: const is trivial; wsize needs the genotype so only const
%% is unit-tested here (wsize is covered by the integration runs).
const_returns_parameter_test() ->
    ?assertEqual(60, tuning_duration:const(60, [], 0)),
    ?assertEqual(15, tuning_duration:const(15, [], 0)).

const_rejects_nonsense_parameter_test() ->
    ?assertEqual(15, tuning_duration:const(0, [], 0)),
    ?assertEqual(15, tuning_duration:const(not_a_number, [], 0)).

duration_dispatch_test() ->
    ?assertEqual(42, tuning_duration:duration(const, 42, [], 0)),
    %% unknown strategy falls back to const of the parameter
    ?assertEqual(42, tuning_duration:duration(nonsense, 42, [], 0)).
