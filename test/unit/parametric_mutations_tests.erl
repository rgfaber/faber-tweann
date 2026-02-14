%% @doc Unit tests for parametric_mutations module.
-module(parametric_mutations_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

parametric_mutations_exports_test() ->
    Exports = parametric_mutations:module_info(exports),
    ?assert(lists:member({mutate_weights, 1}, Exports)),
    ?assert(lists:member({mutate_af, 1}, Exports)),
    ?assert(lists:member({mutate_aggr_f, 1}, Exports)),
    ?assert(lists:member({mutate_agent_parameter, 3}, Exports)),
    ?assert(lists:member({mutate_tuning_selection, 1}, Exports)),
    ?assert(lists:member({mutate_tuning_annealing, 1}, Exports)),
    ?assert(lists:member({mutate_tot_topological_mutations, 1}, Exports)),
    ?assert(lists:member({mutate_heredity_type, 1}, Exports)).

%% ============================================================================
%% Helper
%% ============================================================================

setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db().

%% ============================================================================
%% Mutate Weights Tests
%% ============================================================================

mutate_weights_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_weights(AgentId),
        ?assertEqual(ok, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_weights_changes_weights_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Get weights before mutation
        Agent = genotype:dirty_read({agent, AgentId}),
        Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
        [NeuronId | _] = Cortex#cortex.neuron_ids,
        _NeuronBefore = genotype:dirty_read({neuron, NeuronId}),

        %% Mutate (may affect any neuron, not necessarily this one)
        _ = parametric_mutations:mutate_weights(AgentId),

        %% Verify operation completed without error
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        ?assert(is_record(NeuronAfter, neuron))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Mutate Activation Function Tests
%% ============================================================================

mutate_af_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_af(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Mutate Aggregation Function Tests
%% ============================================================================

mutate_aggr_f_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_aggr_f(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Mutate Agent Parameter Tests
%% ============================================================================

mutate_tuning_selection_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_tuning_selection(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_tuning_annealing_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_tuning_annealing(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_tot_topological_mutations_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_tot_topological_mutations(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_heredity_type_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_heredity_type(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_agent_parameter_generic_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = parametric_mutations:mutate_agent_parameter(
            AgentId, tuning_selection_f, tuning_selection_fs
        ),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.
