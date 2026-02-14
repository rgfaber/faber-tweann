%% @doc LTC (Liquid Time-Constant) mutation operators for neural network evolution.
%%
%% This module provides mutations specific to LTC/CfC neurons:
%% - mutate_neuron_type: Switch between standard/ltc/cfc
%% - mutate_time_constant: Perturb tau (response speed)
%% - mutate_state_bound: Perturb state bound A
%% - mutate_ltc_weights: Perturb backbone/head network weights
%%
%% LTC neurons have adaptive temporal dynamics that can be evolved.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(ltc_mutations).

-include("records.hrl").

-dialyzer({nowarn_function, [
    mutate_neuron_type/1,
    mutate_time_constant/1,
    mutate_state_bound/1,
    mutate_ltc_weights/1
]}).

-export([
    mutate_neuron_type/1,
    mutate_time_constant/1,
    mutate_state_bound/1,
    mutate_ltc_weights/1
]).

%% Delta multiplier for weight perturbation.
-define(DELTA_MULTIPLIER, math:pi() * 2).

%%==============================================================================
%% LTC Mutations
%%==============================================================================

%% @doc Mutate neuron type of a random neuron.
%%
%% Changes neuron type between standard, ltc, and cfc.
%% This allows networks to evolve which neurons use temporal dynamics.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec mutate_neuron_type(term()) -> ok | {error, term()}.
mutate_neuron_type(AgentId) ->
    case mutation_helpers:select_random_neuron(AgentId) of
        {error, no_neurons} ->
            {error, no_neurons};
        NeuronId ->
            change_neuron_type(NeuronId)
    end.

change_neuron_type(NeuronId) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    CurrentType = Neuron#neuron.neuron_type,
    AvailableTypes = [standard, ltc, cfc],
    Alternatives = AvailableTypes -- [CurrentType],
    NewType = selection_utils:random_select(Alternatives),
    UpdatedNeuron = apply_type_change(Neuron, NewType),
    genotype:write(UpdatedNeuron),
    tweann_logger:debug("Mutated neuron type: ~p -> ~p", [CurrentType, NewType]),
    ok.

apply_type_change(Neuron, standard) ->
    Neuron#neuron{
        neuron_type = standard,
        internal_state = 0.0
    };
apply_type_change(Neuron, NewType) ->
    %% LTC or CfC: ensure tau and bound are reasonable
    Tau = ensure_positive(Neuron#neuron.time_constant, 1.0),
    Bound = ensure_positive(Neuron#neuron.state_bound, 1.0),
    Neuron#neuron{
        neuron_type = NewType,
        time_constant = Tau,
        state_bound = Bound
    }.

ensure_positive(Value, _Default) when Value > 0 -> Value;
ensure_positive(_Value, Default) -> Default.

%% @doc Mutate time constant (tau) of a random LTC/CfC neuron.
%%
%% Perturbs the base time constant which controls response speed.
%% Smaller tau = faster response, larger tau = slower/smoother response.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec mutate_time_constant(term()) -> ok | {error, term()}.
mutate_time_constant(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    PerturbRange = Agent#agent.perturbation_range,
    case mutation_helpers:select_ltc_neuron(AgentId) of
        {error, Reason} ->
            {error, Reason};
        NeuronId ->
            perturb_time_constant(NeuronId, PerturbRange)
    end.

perturb_time_constant(NeuronId, PerturbRange) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    CurrentTau = Neuron#neuron.time_constant,
    %% Perturb multiplicatively to keep positive
    Delta = (rand:uniform() - 0.5) * PerturbRange,
    NewTau = clamp(CurrentTau * (1.0 + Delta), 0.001, 100.0),
    UpdatedNeuron = Neuron#neuron{time_constant = NewTau},
    genotype:write(UpdatedNeuron),
    tweann_logger:debug("Mutated time_constant: ~p -> ~p", [CurrentTau, NewTau]),
    ok.

%% @doc Mutate state bound (A) of a random LTC/CfC neuron.
%%
%% Perturbs the state bound which limits internal state magnitude.
%% Larger bounds allow more dynamic range but may be less stable.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec mutate_state_bound(term()) -> ok | {error, term()}.
mutate_state_bound(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    PerturbRange = Agent#agent.perturbation_range,
    case mutation_helpers:select_ltc_neuron(AgentId) of
        {error, Reason} ->
            {error, Reason};
        NeuronId ->
            perturb_state_bound(NeuronId, PerturbRange)
    end.

perturb_state_bound(NeuronId, PerturbRange) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    CurrentBound = Neuron#neuron.state_bound,
    %% Perturb multiplicatively to keep positive
    Delta = (rand:uniform() - 0.5) * PerturbRange,
    NewBound = clamp(CurrentBound * (1.0 + Delta), 0.1, 10.0),
    UpdatedNeuron = Neuron#neuron{state_bound = NewBound},
    genotype:write(UpdatedNeuron),
    tweann_logger:debug("Mutated state_bound: ~p -> ~p", [CurrentBound, NewBound]),
    ok.

%% @doc Mutate backbone/head weights of a random LTC/CfC neuron.
%%
%% Perturbs the learned weights in the LTC f() backbone and h() head networks.
%% These weights control the adaptive time constant and target state.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec mutate_ltc_weights(term()) -> ok | {error, term()}.
mutate_ltc_weights(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    PerturbRange = Agent#agent.perturbation_range * ?DELTA_MULTIPLIER,
    case mutation_helpers:select_ltc_neuron(AgentId) of
        {error, Reason} ->
            {error, Reason};
        NeuronId ->
            perturb_ltc_weights(NeuronId, PerturbRange)
    end.

perturb_ltc_weights(NeuronId, PerturbRange) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    NewBackboneWeights = mutation_helpers:perturb_ltc_weight_list(
        Neuron#neuron.ltc_backbone_weights, PerturbRange
    ),
    NewHeadWeights = mutation_helpers:perturb_ltc_weight_list(
        Neuron#neuron.ltc_head_weights, PerturbRange
    ),
    UpdatedNeuron = Neuron#neuron{
        ltc_backbone_weights = NewBackboneWeights,
        ltc_head_weights = NewHeadWeights
    },
    genotype:write(UpdatedNeuron),
    ok.

%%==============================================================================
%% Private Helpers
%%==============================================================================

%% @private Clamp value between min and max.
-spec clamp(float(), float(), float()) -> float().
clamp(Value, Min, Max) ->
    max(Min, min(Max, Value)).
