%% @doc Reward-modulated Hebbian plasticity rule.
%%
%% This module implements a variant of Hebbian learning where the
%% weight update is modulated by a global reward or punishment signal.
%% This bridges unsupervised Hebbian learning with reinforcement learning.
%%
%% == Theory ==
%%
%% Standard Hebbian learning strengthens any co-active connections,
%% which can lead to "runaway" learning of irrelevant correlations.
%% Modulated Hebbian learning solves this by gating updates with
%% a reward signal:
%%
%%   Δw = η × pre × post × reward
%%
%% Where reward ∈ [-1, 1]:
%% - Positive reward: strengthens co-active connections (LTP)
%% - Negative reward: weakens co-active connections (LTD)
%% - Zero reward: no weight change (neutral)
%%
%% This is biologically inspired by dopamine modulation of synaptic
%% plasticity in the basal ganglia and prefrontal cortex.
%%
%% == Eligibility Traces ==
%%
%% For delayed rewards, we maintain an eligibility trace that
%% records which synapses were recently active:
%%
%%   e(t) = γ × e(t-1) + pre × post
%%   Δw = η × e(t) × reward
%%
%% Where γ is the trace decay rate (typically 0.9-0.99).
%%
%% == Usage ==
%%
%%   Weight = {0.5, 0.0, 0.01, []},
%%   PreActivity = 0.8,
%%   PostActivity = 0.6,
%%   Reward = 1.0,  % Positive reward
%%
%%   NewWeight = plasticity_modulated:apply_rule(Weight, PreActivity, PostActivity, Reward).
%%   %% => {0.5048, 0.0048, 0.01, []}
%%
%% == Configuration ==
%%
%% Parameters in the weight_spec param list:
%% - trace_decay: Eligibility trace decay (default: no trace)
%% - trace: Current eligibility trace value
%% - baseline_reward: Baseline to subtract from reward
%% - reward_scale: Scale factor for reward
%%
%% == References ==
%%
%% [1] Schultz, W. (1998). Predictive Reward Signal of Dopamine Neurons.
%%     Journal of Neurophysiology.
%% [2] Izhikevich, E.M. (2007). Solving the Distal Reward Problem through
%%     Linkage of STDP and Dopamine Signaling. Cerebral Cortex.
%% [3] Fremaux, N., Gerstner, W. (2016). Neuromodulated Spike-Timing-
%%     Dependent Plasticity, and Theory of Three-Factor Learning Rules.
%%     Frontiers in Neural Circuits.
%%
%% @copyright 2024-2026 R.G. Lefever
-module(plasticity_modulated).
-behaviour(plasticity).

%% Behavior callbacks
-export([
    apply_rule/4,
    name/0,
    description/0,
    init/1,
    reset/1
]).

%% Additional API
-export([
    apply_with_trace/5,
    update_trace/4,
    get_trace/1,
    set_trace/2
]).

%% Types
-type weight_spec() :: plasticity:weight_spec().

%%==============================================================================
%% Behavior Callbacks
%%==============================================================================

%% @doc Return the rule name.
-spec name() -> atom().
name() -> modulated.

%% @doc Return a description of this rule.
-spec description() -> binary().
description() ->
    <<"Reward-modulated Hebbian: Δw = η × pre × post × reward">>.

%% @doc Initialize state for this rule.
%%
%% Initializes the eligibility trace if configured.
-spec init(map()) -> map().
init(Params) ->
    #{
        trace => maps:get(initial_trace, Params, 0.0),
        trace_decay => maps:get(trace_decay, Params, 0.9)
    }.

%% @doc Reset the rule state.
-spec reset(map()) -> map().
reset(State) ->
    State#{trace => 0.0}.

%% @doc Apply the modulated Hebbian learning rule.
%%
%% The weight change is modulated by the reward signal.
%%
%% @param Weight The weight specification tuple
%% @param PreActivity Presynaptic activity
%% @param PostActivity Postsynaptic activity
%% @param Reward Reward/punishment signal [-1, 1]
%% @returns Updated weight specification
-spec apply_rule(weight_spec(), float(), float(), float()) -> weight_spec().
apply_rule({W, _DW, LR, Params}, PreActivity, PostActivity, Reward) ->
    %% Check for eligibility trace mode
    case proplists:get_value(trace_decay, Params) of
        undefined ->
            %% Simple modulated Hebbian
            apply_simple(W, LR, PreActivity, PostActivity, Reward, Params);
        TraceDecay ->
            %% Eligibility trace version
            apply_with_trace_internal(W, LR, PreActivity, PostActivity, Reward, TraceDecay, Params)
    end.

%%==============================================================================
%% Internal Implementation
%%==============================================================================

%% @private Simple modulated Hebbian
apply_simple(W, LR, PreActivity, PostActivity, Reward, Params) ->
    %% Get optional configurations
    BaselineReward = proplists:get_value(baseline_reward, Params, 0.0),
    RewardScale = proplists:get_value(reward_scale, Params, 1.0),

    %% Compute effective reward
    EffectiveReward = (Reward - BaselineReward) * RewardScale,

    %% Δw = η × pre × post × reward
    Delta = LR * PreActivity * PostActivity * EffectiveReward,
    NewW = W + Delta,

    {NewW, Delta, LR, Params}.

%% @private Eligibility trace version
apply_with_trace_internal(W, LR, PreActivity, PostActivity, Reward, TraceDecay, Params) ->
    %% Get current trace
    OldTrace = proplists:get_value(trace, Params, 0.0),

    %% Update eligibility trace: e(t) = γ × e(t-1) + pre × post
    NewTrace = TraceDecay * OldTrace + PreActivity * PostActivity,

    %% Δw = η × e(t) × reward
    Delta = LR * NewTrace * Reward,
    NewW = W + Delta,

    %% Update trace in params
    NewParams = lists:keystore(trace, 1, Params, {trace, NewTrace}),

    {NewW, Delta, LR, NewParams}.

%%==============================================================================
%% Additional API Functions
%%==============================================================================

%% @doc Apply rule with explicit eligibility trace handling.
%%
%% This variant allows external management of the eligibility trace,
%% useful when the trace needs to be shared across weights.
%%
%% @param Weight Weight specification
%% @param PreActivity Presynaptic activity
%% @param PostActivity Postsynaptic activity
%% @param Reward Reward signal
%% @param TraceDecay Decay rate for eligibility trace
%% @returns {UpdatedWeight, NewTrace}
-spec apply_with_trace(weight_spec(), float(), float(), float(), float()) ->
    {weight_spec(), float()}.
apply_with_trace({W, _DW, LR, Params}, PreActivity, PostActivity, Reward, TraceDecay) ->
    OldTrace = proplists:get_value(trace, Params, 0.0),
    NewTrace = TraceDecay * OldTrace + PreActivity * PostActivity,
    Delta = LR * NewTrace * Reward,
    NewW = W + Delta,
    NewParams = lists:keystore(trace, 1, Params, {trace, NewTrace}),
    {{NewW, Delta, LR, NewParams}, NewTrace}.

%% @doc Update an eligibility trace without applying plasticity.
%%
%% Useful for maintaining traces during periods without reward.
%%
%% @param PreActivity Presynaptic activity
%% @param PostActivity Postsynaptic activity
%% @param OldTrace Previous trace value
%% @param TraceDecay Decay rate
%% @returns New trace value
-spec update_trace(float(), float(), float(), float()) -> float().
update_trace(PreActivity, PostActivity, OldTrace, TraceDecay) ->
    TraceDecay * OldTrace + PreActivity * PostActivity.

%% @doc Get the eligibility trace from a weight spec.
-spec get_trace(weight_spec()) -> float().
get_trace({_W, _DW, _LR, Params}) ->
    proplists:get_value(trace, Params, 0.0).

%% @doc Set the eligibility trace in a weight spec.
-spec set_trace(weight_spec(), float()) -> weight_spec().
set_trace({W, DW, LR, Params}, Trace) ->
    NewParams = lists:keystore(trace, 1, Params, {trace, Trace}),
    {W, DW, LR, NewParams}.
