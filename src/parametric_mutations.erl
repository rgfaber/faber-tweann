%% @doc Parametric mutation operators for neural network evolution.
%%
%% This module provides mutations that modify network parameters
%% without changing structure:
%% - mutate_weights: Perturb synaptic weights
%% - mutate_af: Change activation function
%% - mutate_aggr_f: Change aggregation function
%%
%% Also includes evolutionary strategy mutations:
%% - mutate_tuning_selection: Change weight selection strategy
%% - mutate_annealing: Modify simulated annealing schedule
%% - mutate_heredity_type: Switch between darwinian/lamarckian
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(parametric_mutations).

-include("records.hrl").

-dialyzer({nowarn_function, [
    mutate_agent_parameter/3,
    get_agent_field/2,
    set_agent_field/3,
    mutate_af/1,
    mutate_aggr_f/1
]}).

-export([
    %% Network parameter mutations
    mutate_weights/1,
    mutate_af/1,
    mutate_aggr_f/1,

    %% Evolutionary strategy mutations
    mutate_agent_parameter/3,
    mutate_tuning_selection/1,
    mutate_tuning_annealing/1,
    mutate_tot_topological_mutations/1,
    mutate_heredity_type/1
]).

%% Delta multiplier for weight perturbation.
-define(DELTA_MULTIPLIER, math:pi() * 2).

%%==============================================================================
%% Network Parameter Mutations
%%==============================================================================

%% @doc Mutate weights of a random neuron.
%%
%% Selects a random neuron and perturbs its input weights
%% using the agent's perturbation range.
%%
%% @param AgentId the agent to mutate
%% @returns ok
-spec mutate_weights(term()) -> ok.
mutate_weights(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    case mutation_helpers:select_random_neuron(AgentId) of
        {error, no_neurons} ->
            ok;
        NeuronId ->
            perturb_neuron_weights(NeuronId, Agent#agent.perturbation_range)
    end.

perturb_neuron_weights(NeuronId, PerturbRange) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    NewInputIdps = [
        {InputId, perturbation_utils:perturb_weights(Weights, PerturbRange * ?DELTA_MULTIPLIER)}
        || {InputId, Weights} <- Neuron#neuron.input_idps
    ],
    UpdatedNeuron = Neuron#neuron{input_idps = NewInputIdps},
    genotype:write(UpdatedNeuron),
    ok.

%% @doc Mutate activation function of a random neuron.
%%
%% Selects a random neuron and changes its activation function
%% to another available function from the constraint.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, no_alternatives}
-spec mutate_af(term()) -> ok | {error, term()}.
mutate_af(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Constraint = Agent#agent.constraint,
    AvailableAFs = Constraint#constraint.neural_afs,
    case mutation_helpers:select_random_neuron(AgentId) of
        {error, no_neurons} ->
            {error, no_neurons};
        NeuronId ->
            change_neuron_af(NeuronId, AvailableAFs)
    end.

change_neuron_af(NeuronId, AvailableAFs) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    CurrentAF = Neuron#neuron.af,
    Alternatives = AvailableAFs -- [CurrentAF],
    select_and_apply_new_af(Neuron, Alternatives).

select_and_apply_new_af(_Neuron, []) ->
    {error, no_alternatives};
select_and_apply_new_af(Neuron, AFs) ->
    NewAF = selection_utils:random_select(AFs),
    UpdatedNeuron = Neuron#neuron{af = NewAF},
    genotype:write(UpdatedNeuron),
    ok.

%% @doc Mutate aggregation function of a random neuron.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, no_alternatives}
-spec mutate_aggr_f(term()) -> ok | {error, term()}.
mutate_aggr_f(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Constraint = Agent#agent.constraint,
    AvailableAggrFs = Constraint#constraint.neural_aggr_fs,
    case mutation_helpers:select_random_neuron(AgentId) of
        {error, no_neurons} ->
            {error, no_neurons};
        NeuronId ->
            change_neuron_aggr_f(NeuronId, AvailableAggrFs)
    end.

change_neuron_aggr_f(NeuronId, AvailableAggrFs) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    CurrentAggrF = Neuron#neuron.aggr_f,
    Alternatives = AvailableAggrFs -- [CurrentAggrF],
    select_and_apply_new_aggr_f(Neuron, Alternatives).

select_and_apply_new_aggr_f(_Neuron, []) ->
    {error, no_alternatives};
select_and_apply_new_aggr_f(Neuron, AggrFs) ->
    NewAggrF = selection_utils:random_select(AggrFs),
    UpdatedNeuron = Neuron#neuron{aggr_f = NewAggrF},
    genotype:write(UpdatedNeuron),
    ok.

%%==============================================================================
%% Evolutionary Strategy Mutations
%%==============================================================================

%% @doc Generic function to mutate an agent parameter.
%%
%% Reads the current value of a field, gets alternatives from constraint,
%% and selects a new random value.
%%
%% @param AgentId the agent to mutate
%% @param FieldName the agent record field to mutate
%% @param ConstraintField the constraint field with alternatives
%% @returns ok or {error, no_alternatives}
-spec mutate_agent_parameter(term(), atom(), atom()) -> ok | {error, no_alternatives}.
mutate_agent_parameter(AgentId, FieldName, ConstraintField) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Constraint = Agent#agent.constraint,
    CurrentValue = get_agent_field(Agent, FieldName),
    AvailableValues = get_constraint_field(Constraint, ConstraintField),
    Alternatives = AvailableValues -- [CurrentValue],
    apply_new_parameter(Agent, FieldName, Alternatives).

apply_new_parameter(_Agent, _FieldName, []) ->
    {error, no_alternatives};
apply_new_parameter(Agent, FieldName, Values) ->
    NewValue = selection_utils:random_select(Values),
    UpdatedAgent = set_agent_field(Agent, FieldName, NewValue),
    genotype:write(UpdatedAgent),
    ok.

%% @doc Mutate tuning selection function.
-spec mutate_tuning_selection(term()) -> ok | {error, no_alternatives}.
mutate_tuning_selection(AgentId) ->
    mutate_agent_parameter(AgentId, tuning_selection_f, tuning_selection_fs).

%% @doc Mutate annealing parameter.
-spec mutate_tuning_annealing(term()) -> ok | {error, no_alternatives}.
mutate_tuning_annealing(AgentId) ->
    mutate_agent_parameter(AgentId, annealing_parameter, annealing_parameters).

%% @doc Mutate total topological mutations function.
-spec mutate_tot_topological_mutations(term()) -> ok | {error, no_alternatives}.
mutate_tot_topological_mutations(AgentId) ->
    mutate_agent_parameter(AgentId, tot_topological_mutations_f, tot_topological_mutations_fs).

%% @doc Mutate heredity type (darwinian/lamarckian).
-spec mutate_heredity_type(term()) -> ok | {error, no_alternatives}.
mutate_heredity_type(AgentId) ->
    mutate_agent_parameter(AgentId, heredity_type, heredity_types).

%%==============================================================================
%% Private Helpers
%%==============================================================================

%% @private Get field value from agent record
-spec get_agent_field(#agent{}, atom()) -> term().
get_agent_field(Agent, tuning_selection_f) -> Agent#agent.tuning_selection_f;
get_agent_field(Agent, annealing_parameter) -> Agent#agent.annealing_parameter;
get_agent_field(Agent, tot_topological_mutations_f) -> Agent#agent.tot_topological_mutations_f;
get_agent_field(Agent, heredity_type) -> Agent#agent.heredity_type;
get_agent_field(Agent, perturbation_range) -> Agent#agent.perturbation_range.

%% @private Set field value in agent record
-spec set_agent_field(#agent{}, atom(), term()) -> #agent{}.
set_agent_field(Agent, tuning_selection_f, Value) ->
    Agent#agent{tuning_selection_f = Value};
set_agent_field(Agent, annealing_parameter, Value) ->
    Agent#agent{annealing_parameter = Value};
set_agent_field(Agent, tot_topological_mutations_f, Value) ->
    Agent#agent{tot_topological_mutations_f = Value};
set_agent_field(Agent, heredity_type, Value) ->
    Agent#agent{heredity_type = Value};
set_agent_field(Agent, perturbation_range, Value) ->
    Agent#agent{perturbation_range = Value}.

%% @private Get field value from constraint record
-spec get_constraint_field(#constraint{}, atom()) -> list().
get_constraint_field(C, tuning_selection_fs) -> C#constraint.tuning_selection_fs;
get_constraint_field(C, annealing_parameters) -> C#constraint.annealing_parameters;
get_constraint_field(C, tot_topological_mutations_fs) -> C#constraint.tot_topological_mutations_fs;
get_constraint_field(C, heredity_types) -> C#constraint.heredity_types;
get_constraint_field(C, perturbation_ranges) -> C#constraint.perturbation_ranges.
