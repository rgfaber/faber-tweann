%% @doc Unit tests for plasticity modules.
%%
%% Tests the core plasticity behavior and all plasticity rule implementations.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(plasticity_tests).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Test Setup
%%==============================================================================

%% Helper to create a weight spec
make_weight(W, LR) ->
    {W, 0.0, LR, []}.

make_weight(W, DW, LR, Params) ->
    {W, DW, LR, Params}.

%%==============================================================================
%% Plasticity Core Tests
%%==============================================================================

plasticity_core_test_() ->
    {"Plasticity core functions",
     [
      {"get_weight extracts weight value",
       ?_assertEqual(0.5, plasticity:get_weight({0.5, 0.1, 0.01, []}))},

      {"get_learning_rate extracts learning rate",
       ?_assertEqual(0.01, plasticity:get_learning_rate({0.5, 0.1, 0.01, []}))},

      {"get_delta extracts delta weight",
       ?_assertEqual(0.1, plasticity:get_delta({0.5, 0.1, 0.01, []}))},

      {"set_weight updates weight value",
       ?_assertEqual({0.7, 0.1, 0.01, []},
                     plasticity:set_weight({0.5, 0.1, 0.01, []}, 0.7))},

      {"set_delta updates delta weight",
       ?_assertEqual({0.5, 0.2, 0.01, []},
                     plasticity:set_delta({0.5, 0.1, 0.01, []}, 0.2))},

      {"hebbian_delta calculates correctly from weight spec",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               Delta = plasticity:hebbian_delta(Weight, 0.8, 0.6),
               %% 0.01 * 0.8 * 0.6 = 0.0048
               ?assert(abs(Delta - 0.0048) < 0.0001)
       end},

      {"hebbian_delta calculates correctly with explicit rate",
       fun() ->
               Delta = plasticity:hebbian_delta(0.1, 0.0, 0.8, 0.6),
               %% 0.1 * 0.8 * 0.6 = 0.048
               ?assert(abs(Delta - 0.048) < 0.0001)
       end},

      {"normalize_weight normalizes correctly",
       ?_assertEqual(0.5, plasticity:normalize_weight(1.0, 2.0))},

      {"normalize_weight handles zero magnitude",
       ?_assertEqual(1.0, plasticity:normalize_weight(1.0, 0.0))},

      {"clamp_weight clamps low values",
       ?_assertEqual(-1.0, plasticity:clamp_weight(-2.0, -1.0, 1.0))},

      {"clamp_weight clamps high values",
       ?_assertEqual(1.0, plasticity:clamp_weight(2.0, -1.0, 1.0))},

      {"clamp_weight passes through valid values",
       ?_assertEqual(0.5, plasticity:clamp_weight(0.5, -1.0, 1.0))},

      {"available_rules returns list of rules",
       fun() ->
               Rules = plasticity:available_rules(),
               ?assert(is_list(Rules)),
               ?assert(length(Rules) >= 3),
               Names = [Name || {Name, _Desc} <- Rules],
               ?assert(lists:member(none, Names)),
               ?assert(lists:member(hebbian, Names)),
               ?assert(lists:member(modulated, Names))
       end},

      {"rule_module returns correct modules",
       fun() ->
               ?assertEqual(plasticity_none, plasticity:rule_module(none)),
               ?assertEqual(plasticity_hebbian, plasticity:rule_module(hebbian)),
               ?assertEqual(plasticity_modulated, plasticity:rule_module(modulated))
       end}
     ]}.

%%==============================================================================
%% Plasticity None Tests
%%==============================================================================

plasticity_none_test_() ->
    {"plasticity_none module",
     [
      {"name returns none",
       ?_assertEqual(none, plasticity_none:name())},

      {"description returns binary",
       fun() ->
               Desc = plasticity_none:description(),
               ?assert(is_binary(Desc)),
               ?assert(byte_size(Desc) > 0)
       end},

      {"apply_rule returns weight unchanged",
       fun() ->
               Weight = {0.5, 0.1, 0.01, [{test, true}]},
               Result = plasticity_none:apply_rule(Weight, 0.8, 0.6, 1.0),
               ?assertEqual(Weight, Result)
       end},

      {"apply_rule ignores all inputs",
       fun() ->
               Weight = {0.0, 0.0, 0.0, []},
               Result1 = plasticity_none:apply_rule(Weight, 1.0, 1.0, 1.0),
               Result2 = plasticity_none:apply_rule(Weight, -1.0, -1.0, -1.0),
               ?assertEqual(Weight, Result1),
               ?assertEqual(Weight, Result2)
       end}
     ]}.

%%==============================================================================
%% Plasticity Hebbian Tests
%%==============================================================================

plasticity_hebbian_test_() ->
    {"plasticity_hebbian module",
     [
      {"name returns hebbian",
       ?_assertEqual(hebbian, plasticity_hebbian:name())},

      {"description returns binary",
       fun() ->
               Desc = plasticity_hebbian:description(),
               ?assert(is_binary(Desc)),
               ?assert(byte_size(Desc) > 0)
       end},

      {"init returns undefined (no state needed)",
       ?_assertEqual(undefined, plasticity_hebbian:init(#{}))},

      {"reset returns undefined",
       ?_assertEqual(undefined, plasticity_hebbian:reset(some_state))},

      {"apply_rule increases weight for positive activations",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               {NewW, Delta, LR, _} = plasticity_hebbian:apply_rule(Weight, 0.8, 0.6, 0.0),
               %% Delta = 0.01 * 0.8 * 0.6 = 0.0048
               ?assert(abs(Delta - 0.0048) < 0.0001),
               ?assert(abs(NewW - 0.5048) < 0.0001),
               ?assertEqual(0.01, LR)
       end},

      {"apply_rule decreases weight for mixed activations",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               {NewW, Delta, _, _} = plasticity_hebbian:apply_rule(Weight, 0.8, -0.6, 0.0),
               %% Delta = 0.01 * 0.8 * (-0.6) = -0.0048
               ?assert(abs(Delta - (-0.0048)) < 0.0001),
               ?assert(abs(NewW - 0.4952) < 0.0001)
       end},

      {"apply_rule no change with zero activity",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               {NewW, Delta, _, _} = plasticity_hebbian:apply_rule(Weight, 0.0, 0.6, 0.0),
               ?assertEqual(0.0, Delta),
               ?assertEqual(0.5, NewW)
       end},

      {"apply_bounded clamps weights",
       fun() ->
               Weight = make_weight(0.95, 0.1),
               {NewW, _, _, _} = plasticity_hebbian:apply_bounded(Weight, 1.0, 1.0, -1.0, 1.0),
               %% Without clamping: 0.95 + (0.1 * 1.0 * 1.0) = 1.05
               %% With clamping: 1.0
               ?assertEqual(1.0, NewW)
       end},

      {"apply_oja includes forgetting term",
       fun() ->
               Weight = make_weight(0.5, 0.1),
               {NewW, Delta, _, _} = plasticity_hebbian:apply_oja(Weight, 0.8, 0.6, 0.0),
               %% Oja: Δw = η × post × (pre - post × w)
               %% Δw = 0.1 * 0.6 * (0.8 - 0.6 * 0.5) = 0.1 * 0.6 * 0.5 = 0.03
               ?assert(abs(Delta - 0.03) < 0.0001),
               ?assert(abs(NewW - 0.53) < 0.0001)
       end},

      {"apply_with_decay reduces weights",
       fun() ->
               Weight = make_weight(1.0, 0.01),
               DecayRate = 0.1,
               {NewW, _, _, _} = plasticity_hebbian:apply_with_decay(Weight, 0.0, 0.0, 0.0, DecayRate),
               %% w' = w * (1 - decay) + delta = 1.0 * 0.9 + 0 = 0.9
               ?assertEqual(0.9, NewW)
       end},

      {"oja variant via params",
       fun() ->
               Weight = make_weight(0.5, 0.0, 0.1, [{oja, true}]),
               {NewW, Delta, _, _} = plasticity_hebbian:apply_rule(Weight, 0.8, 0.6, 0.0),
               %% Same calculation as apply_oja test
               ?assert(abs(Delta - 0.03) < 0.0001),
               ?assert(abs(NewW - 0.53) < 0.0001)
       end},

      {"bounded variant via params",
       fun() ->
               Weight = make_weight(0.95, 0.0, 0.1, [{bounded, {-1.0, 1.0}}]),
               {NewW, _, _, _} = plasticity_hebbian:apply_rule(Weight, 1.0, 1.0, 0.0),
               ?assertEqual(1.0, NewW)
       end}
     ]}.

%%==============================================================================
%% Plasticity Modulated Tests
%%==============================================================================

plasticity_modulated_test_() ->
    {"plasticity_modulated module",
     [
      {"name returns modulated",
       ?_assertEqual(modulated, plasticity_modulated:name())},

      {"description returns binary",
       fun() ->
               Desc = plasticity_modulated:description(),
               ?assert(is_binary(Desc)),
               ?assert(byte_size(Desc) > 0)
       end},

      {"init creates state with trace",
       fun() ->
               State = plasticity_modulated:init(#{initial_trace => 0.5}),
               ?assert(is_map(State)),
               ?assertEqual(0.5, maps:get(trace, State))
       end},

      {"reset clears trace",
       fun() ->
               State = plasticity_modulated:init(#{initial_trace => 0.5}),
               ResetState = plasticity_modulated:reset(State),
               ?assertEqual(0.0, maps:get(trace, ResetState))
       end},

      {"apply_rule with positive reward strengthens",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               {NewW, Delta, _, _} = plasticity_modulated:apply_rule(Weight, 0.8, 0.6, 1.0),
               %% Δw = η × pre × post × reward = 0.01 * 0.8 * 0.6 * 1.0 = 0.0048
               ?assert(abs(Delta - 0.0048) < 0.0001),
               ?assert(abs(NewW - 0.5048) < 0.0001)
       end},

      {"apply_rule with negative reward weakens",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               {NewW, Delta, _, _} = plasticity_modulated:apply_rule(Weight, 0.8, 0.6, -1.0),
               %% Δw = η × pre × post × reward = 0.01 * 0.8 * 0.6 * (-1.0) = -0.0048
               ?assert(abs(Delta - (-0.0048)) < 0.0001),
               ?assert(abs(NewW - 0.4952) < 0.0001)
       end},

      {"apply_rule with zero reward no change",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               {NewW, Delta, _, _} = plasticity_modulated:apply_rule(Weight, 0.8, 0.6, 0.0),
               ?assertEqual(0.0, Delta),
               ?assertEqual(0.5, NewW)
       end},

      {"apply_rule with eligibility trace",
       fun() ->
               Weight = make_weight(0.5, 0.0, 0.1, [{trace_decay, 0.9}, {trace, 0.0}]),
               %% First application: trace = 0.9 * 0.0 + 0.8 * 0.6 = 0.48
               {NewW1, _, _, Params1} = plasticity_modulated:apply_rule(Weight, 0.8, 0.6, 0.0),
               Trace1 = proplists:get_value(trace, Params1),
               ?assert(abs(Trace1 - 0.48) < 0.0001),
               %% No reward, so no weight change
               ?assertEqual(0.5, NewW1),

               %% Second application with reward
               Weight2 = {NewW1, 0.0, 0.1, Params1},
               {_NewW2, Delta2, _, Params2} = plasticity_modulated:apply_rule(Weight2, 0.8, 0.6, 1.0),
               Trace2 = proplists:get_value(trace, Params2),
               %% trace = 0.9 * 0.48 + 0.48 = 0.912
               ?assert(abs(Trace2 - 0.912) < 0.0001),
               %% Delta = 0.1 * 0.912 * 1.0 = 0.0912
               ?assert(abs(Delta2 - 0.0912) < 0.0001)
       end},

      {"get_trace extracts trace from params",
       fun() ->
               Weight = make_weight(0.5, 0.0, 0.1, [{trace, 0.42}]),
               ?assert(abs(plasticity_modulated:get_trace(Weight) - 0.42) < 0.0001)
       end},

      {"set_trace updates trace in params",
       fun() ->
               Weight = make_weight(0.5, 0.0, 0.1, [{trace, 0.0}]),
               NewWeight = plasticity_modulated:set_trace(Weight, 0.75),
               ?assert(abs(plasticity_modulated:get_trace(NewWeight) - 0.75) < 0.0001)
       end},

      {"update_trace calculates correctly",
       fun() ->
               NewTrace = plasticity_modulated:update_trace(0.8, 0.6, 0.5, 0.9),
               %% trace = 0.9 * 0.5 + 0.8 * 0.6 = 0.45 + 0.48 = 0.93
               ?assert(abs(NewTrace - 0.93) < 0.0001)
       end},

      {"baseline_reward subtracts from reward",
       fun() ->
               Weight = make_weight(0.5, 0.0, 0.01, [{baseline_reward, 0.5}]),
               {NewW, Delta, _, _} = plasticity_modulated:apply_rule(Weight, 0.8, 0.6, 1.0),
               %% Effective reward = 1.0 - 0.5 = 0.5
               %% Delta = 0.01 * 0.8 * 0.6 * 0.5 = 0.0024
               ?assert(abs(Delta - 0.0024) < 0.0001),
               ?assert(abs(NewW - 0.5024) < 0.0001)
       end},

      {"reward_scale scales reward",
       fun() ->
               Weight = make_weight(0.5, 0.0, 0.01, [{reward_scale, 2.0}]),
               {NewW, Delta, _, _} = plasticity_modulated:apply_rule(Weight, 0.8, 0.6, 0.5),
               %% Effective reward = 0.5 * 2.0 = 1.0
               %% Delta = 0.01 * 0.8 * 0.6 * 1.0 = 0.0048
               ?assert(abs(Delta - 0.0048) < 0.0001),
               ?assert(abs(NewW - 0.5048) < 0.0001)
       end}
     ]}.

%%==============================================================================
%% Integration Tests
%%==============================================================================

integration_test_() ->
    {"Plasticity integration",
     [
      {"apply_to_weights delegates to rule module",
       fun() ->
               Weight = make_weight(0.5, 0.01),
               Result = plasticity:apply_to_weights(plasticity_hebbian, Weight, 0.8, 0.6, 0.0),
               {NewW, Delta, _, _} = Result,
               ?assert(abs(Delta - 0.0048) < 0.0001),
               ?assert(abs(NewW - 0.5048) < 0.0001)
       end},

      {"learning improves over repeated applications",
       fun() ->
               %% Simulate multiple learning steps
               InitWeight = make_weight(0.0, 0.1),
               PreAct = 1.0,
               PostAct = 1.0,

               %% Apply Hebbian learning 10 times
               FinalWeight = lists:foldl(
                   fun(_, {W, DW, LR, P}) ->
                       plasticity_hebbian:apply_rule({W, DW, LR, P}, PreAct, PostAct, 0.0)
                   end,
                   InitWeight,
                   lists:seq(1, 10)
               ),

               {FinalW, _, _, _} = FinalWeight,
               %% Weight should have increased: 0.0 + 10 * 0.1 = 1.0
               %% Use approximate comparison due to floating point precision
               ?assert(abs(FinalW - 1.0) < 0.0001)
       end},

      {"modulated learning converges with consistent reward",
       fun() ->
               InitWeight = make_weight(0.0, 0.1),
               PreAct = 1.0,
               PostAct = 1.0,
               Reward = 1.0,

               FinalWeight = lists:foldl(
                   fun(_, {W, DW, LR, P}) ->
                       plasticity_modulated:apply_rule({W, DW, LR, P}, PreAct, PostAct, Reward)
                   end,
                   InitWeight,
                   lists:seq(1, 10)
               ),

               {FinalW, _, _, _} = FinalWeight,
               %% Same as Hebbian with reward=1.0
               %% Use approximate comparison due to floating point precision
               ?assert(abs(FinalW - 1.0) < 0.0001)
       end}
     ]}.
