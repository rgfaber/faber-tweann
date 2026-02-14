%% @doc Plasticity behavior module - defines the interface for learning rules.
%%
%% This module provides a behavior (interface) for implementing different
%% plasticity rules that enable neural networks to learn during operation.
%% Unlike evolutionary weight changes, plasticity rules update weights
%% based on neural activity patterns.
%%
%% == Theory ==
%%
%% Plasticity refers to the brain's ability to modify its connections
%% based on experience. The most fundamental rule is Hebbian learning:
%% "neurons that fire together wire together" (Hebb, 1949).
%%
%% Mathematically, basic Hebbian learning is:
%%   Δw_ij = η × pre_i × post_j
%%
%% Where:
%% - Δw_ij is the change in weight from neuron i to j
%% - η is the learning rate
%% - pre_i is the presynaptic (input) activity
%% - post_j is the postsynaptic (output) activity
%%
%% More sophisticated rules include:
%% - Oja's rule: adds weight normalization to prevent unbounded growth
%% - BCM rule: includes a sliding threshold for potentiation/depression
%% - STDP: Spike-Timing Dependent Plasticity, considers timing of spikes
%% - Modulated Hebbian: multiplies by a reward/punishment signal
%%
%% == Usage ==
%%
%% Implement the behavior in a module:
%%
%%   -module(plasticity_hebbian).
%%   -behaviour(plasticity).
%%
%%   -export([apply_rule/4, name/0, description/0]).
%%
%%   name() -> hebbian.
%%   description() -> "Basic Hebbian learning rule" (as binary).
%%
%%   apply_rule(Weight, PreActivity, PostActivity, _Reward) ->
%%       plasticity:hebbian_delta(Weight, PreActivity, PostActivity).
%%
%% Then use the plasticity module to apply rules:
%%
%%   NewWeights = plasticity:apply_to_network(hebbian, Weights, Activations, Reward)
%%
%% == References ==
%%
%% [1] Hebb, D.O. (1949). The Organization of Behavior. Wiley.
%% [2] Oja, E. (1982). A simplified neuron model as a principal component
%%     analyzer. Journal of Mathematical Biology, 15(3).
%% [3] Bi, G., Poo, M. (1998). Synaptic Modifications in Cultured
%%     Hippocampal Neurons. Journal of Neuroscience, 18(24).
%%
%% @copyright 2024-2026 R.G. Lefever
-module(plasticity).

%% Behavior callbacks
-callback apply_rule(Weight :: weight_spec(),
                     PreActivity :: float(),
                     PostActivity :: float(),
                     Reward :: float()) -> weight_spec().

-callback name() -> atom().

-callback description() -> binary().

%% Optional callbacks with defaults
-callback init(Params :: map()) -> State :: term().
-callback reset(State :: term()) -> State :: term().

-optional_callbacks([init/1, reset/1]).

%% API exports
-export([
    %% Core application
    apply_to_weights/5,
    apply_to_layer/4,
    apply_to_network/4,

    %% Weight manipulation
    hebbian_delta/3,
    hebbian_delta/4,
    normalize_weight/2,
    clamp_weight/3,

    %% Weight accessors
    get_weight/1,
    get_learning_rate/1,
    get_delta/1,
    set_weight/2,
    set_delta/2,

    %% Utilities
    available_rules/0,
    rule_module/1
]).

%% Types
-type weight_spec() :: {Weight :: float(), DeltaWeight :: float(),
                        LearningRate :: float(), ParamList :: list()}.
-type layer_weights() :: [{SourceId :: term(), [weight_spec()]}].

-export_type([weight_spec/0, layer_weights/0]).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Apply a plasticity rule to a single weight.
%%
%% Takes a rule module, the current weight spec, pre/post activities,
%% and an optional reward signal. Returns the updated weight spec.
%%
%% @param RuleModule Module implementing the plasticity behavior
%% @param Weight Current weight specification tuple
%% @param PreActivity Presynaptic (input) activation level
%% @param PostActivity Postsynaptic (output) activation level
%% @param Reward Global reward/punishment signal (for modulated rules)
%% @returns Updated weight specification
-spec apply_to_weights(module(), weight_spec(), float(), float(), float()) -> weight_spec().
apply_to_weights(RuleModule, Weight, PreActivity, PostActivity, Reward) ->
    RuleModule:apply_rule(Weight, PreActivity, PostActivity, Reward).

%% @doc Apply a plasticity rule to all weights in a layer.
%%
%% Given a layer's weight structure (list of {SourceId, [weights]}),
%% applies the rule using the corresponding activations.
%%
%% @param RuleModule Module implementing the plasticity behavior
%% @param LayerWeights List of {SourceId, [weight_spec()]}
%% @param PreActivations List of presynaptic activations (indexed by source)
%% @param PostActivity Single postsynaptic activation for this neuron
%% @returns Updated layer weights
-spec apply_to_layer(module(), layer_weights(), [float()], float()) -> layer_weights().
apply_to_layer(RuleModule, LayerWeights, PreActivations, PostActivity) ->
    apply_to_layer(RuleModule, LayerWeights, PreActivations, PostActivity, 0.0).

%% @private Apply rule with reward signal
apply_to_layer(RuleModule, LayerWeights, PreActivations, PostActivity, Reward) ->
    lists:zipwith(
        fun({SourceId, Weights}, PreActs) ->
            UpdatedWeights = lists:zipwith(
                fun(Weight, PreAct) ->
                    apply_to_weights(RuleModule, Weight, PreAct, PostActivity, Reward)
                end,
                Weights,
                PreActs
            ),
            {SourceId, UpdatedWeights}
        end,
        LayerWeights,
        PreActivations
    ).

%% @doc Apply a plasticity rule to an entire network's weights.
%%
%% This is the main entry point for applying learning to a network.
%% It takes all weights organized by layer, activations per layer,
%% and an optional reward signal.
%%
%% @param RuleAtom Atom identifying the plasticity rule (e.g., hebbian)
%% @param AllWeights Network weights: [[{SourceId, [weight_spec()]}]]
%% @param AllActivations Activations per layer: [[float()]]
%% @param Reward Global reward signal
%% @returns Updated network weights
-spec apply_to_network(atom(), [[layer_weights()]], [[float()]], float()) -> [[layer_weights()]].
apply_to_network(RuleAtom, AllWeights, AllActivations, Reward) ->
    RuleModule = rule_module(RuleAtom),
    NumLayers = length(AllWeights),

    %% For each layer, we need pre-activations (previous layer)
    %% and post-activations (current layer output)
    lists:zipwith3(
        fun(LayerIdx, LayerWeights, PostActivations) ->
            PreActivations = case LayerIdx of
                1 -> hd(AllActivations);  %% Input activations for first layer
                _ -> lists:nth(LayerIdx - 1, AllActivations)
            end,

            %% Apply to each neuron in the layer
            lists:zipwith(
                fun(NeuronWeights, PostAct) ->
                    apply_to_layer(RuleModule, NeuronWeights, [PreActivations], PostAct, Reward)
                end,
                LayerWeights,
                PostActivations
            )
        end,
        lists:seq(1, NumLayers),
        AllWeights,
        tl(AllActivations)  %% Skip input layer for output activations
    ).

%%==============================================================================
%% Hebbian Learning Helpers
%%==============================================================================

%% @doc Calculate the Hebbian weight delta.
%%
%% Basic Hebbian rule: Δw = η × pre × post
%%
%% @param Weight Current weight value
%% @param PreActivity Presynaptic activity
%% @param PostActivity Postsynaptic activity
%% @returns Weight change (delta)
-spec hebbian_delta(weight_spec() | float(), float(), float()) -> float().
hebbian_delta({_W, _DW, LearningRate, _Params}, PreActivity, PostActivity) ->
    LearningRate * PreActivity * PostActivity;
hebbian_delta(LearningRate, PreActivity, PostActivity) when is_float(LearningRate) ->
    LearningRate * PreActivity * PostActivity.

%% @doc Calculate Hebbian delta with explicit learning rate.
-spec hebbian_delta(float(), float(), float(), float()) -> float().
hebbian_delta(LearningRate, _CurrentWeight, PreActivity, PostActivity) ->
    LearningRate * PreActivity * PostActivity.

%% @doc Normalize weight to prevent unbounded growth (Oja's modification).
%%
%% Applies: w' = w / ||w||
%%
%% @param Weight Current weight
%% @param Magnitude Normalization magnitude
%% @returns Normalized weight
-spec normalize_weight(float(), float()) -> float().
normalize_weight(Weight, Magnitude) when Magnitude > 0 ->
    Weight / Magnitude;
normalize_weight(Weight, _) ->
    Weight.

%% @doc Clamp weight to stay within bounds.
%%
%% @param Weight Current weight
%% @param Min Minimum allowed value
%% @param Max Maximum allowed value
%% @returns Clamped weight
-spec clamp_weight(float(), float(), float()) -> float().
clamp_weight(Weight, Min, _Max) when Weight < Min -> Min;
clamp_weight(Weight, _Min, Max) when Weight > Max -> Max;
clamp_weight(Weight, _Min, _Max) -> Weight.

%%==============================================================================
%% Weight Accessor Functions
%%==============================================================================

%% @doc Extract the weight value from a weight_spec tuple.
-spec get_weight(weight_spec()) -> float().
get_weight({W, _DW, _LR, _P}) -> W.

%% @doc Extract the learning rate from a weight_spec tuple.
-spec get_learning_rate(weight_spec()) -> float().
get_learning_rate({_W, _DW, LR, _P}) -> LR.

%% @doc Extract the delta weight from a weight_spec tuple.
-spec get_delta(weight_spec()) -> float().
get_delta({_W, DW, _LR, _P}) -> DW.

%% @doc Set the weight value in a weight_spec tuple.
-spec set_weight(weight_spec(), float()) -> weight_spec().
set_weight({_W, DW, LR, P}, NewW) -> {NewW, DW, LR, P}.

%% @doc Set the delta weight in a weight_spec tuple.
-spec set_delta(weight_spec(), float()) -> weight_spec().
set_delta({W, _DW, LR, P}, NewDW) -> {W, NewDW, LR, P}.

%%==============================================================================
%% Rule Registry
%%==============================================================================

%% @doc List available plasticity rules.
%%
%% @returns List of {RuleName, Description} tuples
-spec available_rules() -> [{atom(), binary()}].
available_rules() ->
    [
        {none, <<"No plasticity (static weights)">>},
        {hebbian, <<"Basic Hebbian learning: Δw = η × pre × post">>},
        {modulated, <<"Reward-modulated Hebbian: Δw = η × pre × post × reward">>}
    ].

%% @doc Get the module implementing a plasticity rule.
%%
%% @param RuleAtom Atom identifying the rule
%% @returns Module name
-spec rule_module(atom()) -> module().
rule_module(none) -> plasticity_none;
rule_module(hebbian) -> plasticity_hebbian;
rule_module(modulated) -> plasticity_modulated;
rule_module(RuleAtom) ->
    %% Try to construct module name from atom
    list_to_existing_atom("plasticity_" ++ atom_to_list(RuleAtom)).
