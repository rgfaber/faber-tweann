%% @doc Basic Hebbian plasticity rule implementation.
%%
%% This module implements the classic Hebbian learning rule, often
%% summarized as "neurons that fire together wire together."
%%
%% == Theory ==
%%
%% Hebbian learning was proposed by Donald Hebb in 1949 as a model
%% for how neural connections are strengthened through experience.
%% The basic rule is:
%%
%%   Δw_ij = η × pre_i × post_j
%%
%% Where:
%% - Δw_ij is the weight change from neuron i to j
%% - η (eta) is the learning rate
%% - pre_i is the presynaptic (input) activation
%% - post_j is the postsynaptic (output) activation
%%
%% == Variants Implemented ==
%%
%% 1. **Basic Hebbian** (default):
%%    Δw = η × pre × post
%%
%% 2. **Bounded Hebbian** (with weight clamping):
%%    Δw = η × pre × post, clamped to [-1, 1]
%%
%% 3. **Oja's Rule** (with normalization):
%%    Δw = η × post × (pre - post × w)
%%    This prevents unbounded weight growth.
%%
%% == Usage ==
%%
%%   Weight = {0.5, 0.0, 0.01, []},  % Initial weight spec
%%   PreActivity = 0.8,
%%   PostActivity = 0.6,
%%   Reward = 0.0,  % Not used in basic Hebbian
%%
%%   NewWeight = plasticity_hebbian:apply_rule(Weight, PreActivity, PostActivity, Reward).
%%   %% => {0.5048, 0.0048, 0.01, []}
%%
%% == Configuration ==
%%
%% The learning rate is stored in the weight_spec tuple (3rd element).
%% Additional parameters can be stored in the parameter list (4th element):
%%
%% - `{bounded, Min, Max}` - Clamp weights to [Min, Max]
%% - `{oja, true}` - Use Oja's normalized rule
%% - `{decay, Rate}` - Apply weight decay: w' = w × (1 - decay)
%%
%% == References ==
%%
%% [1] Hebb, D.O. (1949). The Organization of Behavior. Wiley.
%% [2] Oja, E. (1982). "A simplified neuron model as a principal
%%     component analyzer." Journal of Mathematical Biology.
%%
%% @copyright 2024-2026 R.G. Lefever
-module(plasticity_hebbian).
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
    apply_bounded/5,
    apply_oja/4,
    apply_with_decay/5
]).

%% Types
-type weight_spec() :: plasticity:weight_spec().

%%==============================================================================
%% Behavior Callbacks
%%==============================================================================

%% @doc Return the rule name.
-spec name() -> atom().
name() -> hebbian.

%% @doc Return a description of this rule.
-spec description() -> binary().
description() ->
    <<"Basic Hebbian learning: strengthens connections when pre and post fire together">>.

%% @doc Initialize any state for this rule.
%%
%% For basic Hebbian, no state is needed.
-spec init(map()) -> undefined.
init(_Params) ->
    undefined.

%% @doc Reset the rule state.
-spec reset(term()) -> undefined.
reset(_State) ->
    undefined.

%% @doc Apply the Hebbian learning rule to a weight.
%%
%% This is the main callback that updates a single weight based on
%% the activities of the pre and post neurons.
%%
%% @param Weight The weight specification tuple
%% @param PreActivity Presynaptic (input) activation
%% @param PostActivity Postsynaptic (output) activation
%% @param Reward Reward signal (unused in basic Hebbian)
%% @returns Updated weight specification
-spec apply_rule(weight_spec(), float(), float(), float()) -> weight_spec().
apply_rule({W, _DW, LR, Params}, PreActivity, PostActivity, _Reward) ->
    %% Check for variant modes in parameters
    case extract_variant(Params) of
        oja ->
            apply_oja_internal(W, LR, PreActivity, PostActivity, Params);
        {bounded, Min, Max} ->
            apply_bounded_internal(W, LR, PreActivity, PostActivity, Min, Max, Params);
        basic ->
            apply_basic(W, LR, PreActivity, PostActivity, Params)
    end.

%%==============================================================================
%% Internal Implementation
%%==============================================================================

%% @private Apply basic Hebbian rule
apply_basic(W, LR, PreActivity, PostActivity, Params) ->
    %% Δw = η × pre × post
    Delta = LR * PreActivity * PostActivity,

    %% Apply weight decay if configured
    NewW = case proplists:get_value(decay, Params) of
        undefined -> W + Delta;
        DecayRate -> (W * (1.0 - DecayRate)) + Delta
    end,

    {NewW, Delta, LR, Params}.

%% @private Apply Oja's normalized Hebbian rule
apply_oja_internal(W, LR, PreActivity, PostActivity, Params) ->
    %% Oja's rule: Δw = η × post × (pre - post × w)
    %% This prevents unbounded weight growth by including a forgetting term
    Delta = LR * PostActivity * (PreActivity - PostActivity * W),
    NewW = W + Delta,
    {NewW, Delta, LR, Params}.

%% @private Apply bounded Hebbian rule
apply_bounded_internal(W, LR, PreActivity, PostActivity, Min, Max, Params) ->
    Delta = LR * PreActivity * PostActivity,
    NewW = clamp(W + Delta, Min, Max),
    {NewW, Delta, LR, Params}.

%% @private Extract variant configuration from params
extract_variant(Params) ->
    case proplists:get_value(oja, Params) of
        true -> oja;
        _ ->
            case proplists:get_value(bounded, Params) of
                {Min, Max} -> {bounded, Min, Max};
                _ -> basic
            end
    end.

%% @private Clamp value to range
clamp(X, Min, _Max) when X < Min -> Min;
clamp(X, _Min, Max) when X > Max -> Max;
clamp(X, _Min, _Max) -> X.

%%==============================================================================
%% Additional API Functions
%%==============================================================================

%% @doc Apply bounded Hebbian rule with explicit bounds.
%%
%% @param Weight Weight specification
%% @param PreActivity Presynaptic activity
%% @param PostActivity Postsynaptic activity
%% @param Min Minimum weight value
%% @param Max Maximum weight value
%% @returns Updated weight specification
-spec apply_bounded(weight_spec(), float(), float(), float(), float()) -> weight_spec().
apply_bounded({W, _DW, LR, Params}, PreActivity, PostActivity, Min, Max) ->
    apply_bounded_internal(W, LR, PreActivity, PostActivity, Min, Max, Params).

%% @doc Apply Oja's normalized Hebbian rule.
%%
%% Oja's rule includes a "forgetting" term that prevents unbounded
%% weight growth, making it suitable for self-organizing maps and
%% principal component analysis.
%%
%% @param Weight Weight specification
%% @param PreActivity Presynaptic activity
%% @param PostActivity Postsynaptic activity
%% @returns Updated weight specification
-spec apply_oja(weight_spec(), float(), float(), float()) -> weight_spec().
apply_oja({W, _DW, LR, Params}, PreActivity, PostActivity, _Reward) ->
    apply_oja_internal(W, LR, PreActivity, PostActivity, Params).

%% @doc Apply Hebbian with weight decay.
%%
%% Weight decay prevents weights from growing too large over time
%% by slightly reducing all weights each update.
%%
%% @param Weight Weight specification
%% @param PreActivity Presynaptic activity
%% @param PostActivity Postsynaptic activity
%% @param DecayRate Rate of weight decay (typically 0.001 to 0.01)
%% @returns Updated weight specification
-spec apply_with_decay(weight_spec(), float(), float(), float(), float()) -> weight_spec().
apply_with_decay({W, _DW, LR, Params}, PreActivity, PostActivity, _Reward, DecayRate) ->
    Delta = LR * PreActivity * PostActivity,
    NewW = (W * (1.0 - DecayRate)) + Delta,
    {NewW, Delta, LR, Params}.
