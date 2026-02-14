%% @doc Genetic mutation operators for neural network evolution.
%%
%% This module is the main entry point for genome mutations. It delegates
%% to specialized modules for different mutation categories:
%%
%% - topological_mutations: Network structure changes
%% - parametric_mutations: Weight and parameter changes
%% - ltc_mutations: LTC neuron-specific mutations
%% - mutation_helpers: Shared utility functions
%%
%% Mutations are selected using roulette wheel selection weighted by
%% mutation probabilities from the agent's constraint.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(genome_mutator).

-include("records.hrl").

-dialyzer({nowarn_function, [
    mutate/1,
    apply_mutation/2
]}).

-export([
    %% Main mutation interface
    mutate/1,
    mutate/2,
    calculate_mutation_count/1,

    %% Re-exports for backwards compatibility
    %% Topological mutations
    add_bias/1,
    add_outlink/1,
    add_inlink/1,
    add_neuron/1,
    outsplice/1,
    add_sensorlink/1,
    add_actuatorlink/1,
    add_sensor/1,
    add_actuator/1,

    %% Parametric mutations
    mutate_weights/1,
    mutate_af/1,
    mutate_aggr_f/1,
    mutate_agent_parameter/3,
    mutate_tuning_selection/1,
    mutate_tuning_annealing/1,
    mutate_tot_topological_mutations/1,
    mutate_heredity_type/1,

    %% LTC mutations
    mutate_neuron_type/1,
    mutate_time_constant/1,
    mutate_state_bound/1,
    mutate_ltc_weights/1,

    %% Utility
    select_random_neuron/1
]).

%%==============================================================================
%% Mutation Dispatch Map
%%==============================================================================

%% @private Map of mutation operators to their implementing functions.
%% Using a function that returns the map to avoid compile-time evaluation issues.
-spec mutation_dispatch() -> #{atom() => fun((term()) -> ok | {error, term()})}.
mutation_dispatch() ->
    #{
        %% Topological mutations
        add_bias => fun topological_mutations:add_bias/1,
        add_outlink => fun topological_mutations:add_outlink/1,
        add_inlink => fun topological_mutations:add_inlink/1,
        add_neuron => fun topological_mutations:add_neuron/1,
        outsplice => fun topological_mutations:outsplice/1,
        add_sensorlink => fun topological_mutations:add_sensorlink/1,
        add_actuatorlink => fun topological_mutations:add_actuatorlink/1,
        add_sensor => fun topological_mutations:add_sensor/1,
        add_actuator => fun topological_mutations:add_actuator/1,

        %% Parametric mutations
        mutate_weights => fun parametric_mutations:mutate_weights/1,
        mutate_af => fun parametric_mutations:mutate_af/1,
        mutate_aggr_f => fun parametric_mutations:mutate_aggr_f/1,

        %% LTC mutations
        mutate_neuron_type => fun ltc_mutations:mutate_neuron_type/1,
        mutate_time_constant => fun ltc_mutations:mutate_time_constant/1,
        mutate_state_bound => fun ltc_mutations:mutate_state_bound/1,
        mutate_ltc_weights => fun ltc_mutations:mutate_ltc_weights/1,

        %% Substrate mutations (HyperNEAT/CPPN) - not supported in current version
        %% These operators require Compositional Pattern Producing Networks
        %% which will be implemented in a future version. Using these operators
        %% will have no effect on the genotype.
        add_cpp => fun(AgentId) ->
            tweann_logger:warning("add_cpp mutation not supported: agent=~p", [AgentId]),
            ok
        end,
        add_cep => fun(AgentId) ->
            tweann_logger:warning("add_cep mutation not supported: agent=~p", [AgentId]),
            ok
        end
    }.

%%==============================================================================
%% Main Mutation Interface
%%==============================================================================

%% @doc Apply mutations to an agent.
%%
%% Selects and applies mutations based on the agent's constraint.
%% The number of mutations is determined by the tot_topological_mutations_f.
%%
%% @param AgentId the agent to mutate
%% @returns ok
-spec mutate(term()) -> ok.
mutate(AgentId) ->
    MutationCount = calculate_mutation_count(AgentId),
    tweann_logger:debug("Starting mutation: agent=~p mutations=~p", [AgentId, MutationCount]),
    mutate(AgentId, MutationCount).

%% @doc Apply a specific number of mutations to an agent.
%%
%% @param AgentId the agent to mutate
%% @param Count number of mutations to apply
%% @returns ok
-spec mutate(term(), non_neg_integer()) -> ok.
mutate(AgentId, Count) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    MutationOperators = Agent#agent.mutation_operators,
    apply_mutations(AgentId, MutationOperators, Count).

apply_mutations(_AgentId, _Operators, 0) ->
    ok;
apply_mutations(AgentId, Operators, Count) ->
    Operator = selection_utils:roulette_wheel(Operators),
    _ = apply_mutation(AgentId, Operator),
    apply_mutations(AgentId, Operators, Count - 1).

%% @doc Calculate number of mutations based on agent's mutation function.
-spec calculate_mutation_count(term()) -> pos_integer().
calculate_mutation_count(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    NeuronCount = length(Cortex#cortex.neuron_ids),
    {MutationF, Param} = Agent#agent.tot_topological_mutations_f,
    calculate_count(MutationF, NeuronCount, Param).

%% @private Calculate count based on mutation function
-spec calculate_count(atom(), non_neg_integer(), float()) -> pos_integer().
calculate_count(ncount_exponential, NeuronCount, Param) ->
    max(1, round(NeuronCount * math:pow(Param, NeuronCount)));
calculate_count(ncount_linear, NeuronCount, Param) ->
    max(1, round(NeuronCount * Param));
calculate_count(_Default, _NeuronCount, _Param) ->
    1.

%% @private Apply a specific mutation operator using map dispatch.
-spec apply_mutation(term(), atom()) -> ok | {error, term()}.
apply_mutation(AgentId, Operator) ->
    tweann_logger:debug("Applying mutation: agent=~p operator=~p", [AgentId, Operator]),
    DispatchMap = mutation_dispatch(),
    Result = dispatch_mutation(AgentId, Operator, DispatchMap),
    log_mutation_result(AgentId, Operator, Result),
    Result.

dispatch_mutation(AgentId, Operator, DispatchMap) ->
    case maps:get(Operator, DispatchMap, undefined) of
        undefined ->
            tweann_logger:warning("Unknown mutation operator: ~p", [Operator]),
            ok;
        MutationFun ->
            MutationFun(AgentId)
    end.

log_mutation_result(AgentId, Operator, {error, Reason}) ->
    tweann_logger:warning("Mutation failed: agent=~p operator=~p reason=~p",
                          [AgentId, Operator, Reason]);
log_mutation_result(AgentId, Operator, ok) ->
    tweann_logger:debug("Mutation succeeded: agent=~p operator=~p", [AgentId, Operator]).

%%==============================================================================
%% Re-exports for Backwards Compatibility
%%==============================================================================

%% Topological mutations
add_bias(AgentId) -> topological_mutations:add_bias(AgentId).
add_outlink(AgentId) -> topological_mutations:add_outlink(AgentId).
add_inlink(AgentId) -> topological_mutations:add_inlink(AgentId).
add_neuron(AgentId) -> topological_mutations:add_neuron(AgentId).
outsplice(AgentId) -> topological_mutations:outsplice(AgentId).
add_sensorlink(AgentId) -> topological_mutations:add_sensorlink(AgentId).
add_actuatorlink(AgentId) -> topological_mutations:add_actuatorlink(AgentId).
add_sensor(AgentId) -> topological_mutations:add_sensor(AgentId).
add_actuator(AgentId) -> topological_mutations:add_actuator(AgentId).

%% Parametric mutations
mutate_weights(AgentId) -> parametric_mutations:mutate_weights(AgentId).
mutate_af(AgentId) -> parametric_mutations:mutate_af(AgentId).
mutate_aggr_f(AgentId) -> parametric_mutations:mutate_aggr_f(AgentId).
mutate_agent_parameter(AgentId, F, C) -> parametric_mutations:mutate_agent_parameter(AgentId, F, C).
mutate_tuning_selection(AgentId) -> parametric_mutations:mutate_tuning_selection(AgentId).
mutate_tuning_annealing(AgentId) -> parametric_mutations:mutate_tuning_annealing(AgentId).
mutate_tot_topological_mutations(AgentId) -> parametric_mutations:mutate_tot_topological_mutations(AgentId).
mutate_heredity_type(AgentId) -> parametric_mutations:mutate_heredity_type(AgentId).

%% LTC mutations
mutate_neuron_type(AgentId) -> ltc_mutations:mutate_neuron_type(AgentId).
mutate_time_constant(AgentId) -> ltc_mutations:mutate_time_constant(AgentId).
mutate_state_bound(AgentId) -> ltc_mutations:mutate_state_bound(AgentId).
mutate_ltc_weights(AgentId) -> ltc_mutations:mutate_ltc_weights(AgentId).

%% Utility
select_random_neuron(AgentId) -> mutation_helpers:select_random_neuron(AgentId).
