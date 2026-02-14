%% @doc No-op plasticity rule (static weights).
%%
%% This module implements a "do nothing" plasticity rule that leaves
%% weights unchanged. It serves as:
%%
%% 1. A baseline for comparing learning vs no learning
%% 2. A way to disable learning for specific connections
%% 3. A template for implementing new plasticity rules
%%
%% @copyright 2024-2026 R.G. Lefever
-module(plasticity_none).
-behaviour(plasticity).

%% Behavior callbacks
-export([
    apply_rule/4,
    name/0,
    description/0
]).

%% Types
-type weight_spec() :: plasticity:weight_spec().

%%==============================================================================
%% Behavior Callbacks
%%==============================================================================

%% @doc Return the rule name.
-spec name() -> atom().
name() -> none.

%% @doc Return a description of this rule.
-spec description() -> binary().
description() ->
    <<"No plasticity - weights remain static">>.

%% @doc Apply the no-op rule (returns weight unchanged).
%%
%% @param Weight The weight specification tuple
%% @param _PreActivity Ignored
%% @param _PostActivity Ignored
%% @param _Reward Ignored
%% @returns Unchanged weight specification
-spec apply_rule(weight_spec(), float(), float(), float()) -> weight_spec().
apply_rule(Weight, _PreActivity, _PostActivity, _Reward) ->
    Weight.
