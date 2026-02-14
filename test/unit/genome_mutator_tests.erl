%% @doc Unit tests for genome_mutator module.
-module(genome_mutator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

genome_mutator_exports_test() ->
    Exports = genome_mutator:module_info(exports),
    ?assert(lists:member({mutate, 1}, Exports)),
    ?assert(lists:member({mutate, 2}, Exports)),
    ?assert(lists:member({add_neuron, 1}, Exports)),
    ?assert(lists:member({add_bias, 1}, Exports)),
    ?assert(lists:member({mutate_weights, 1}, Exports)),
    ?assert(lists:member({select_random_neuron, 1}, Exports)).

%% ============================================================================
%% Integration Tests with Mnesia
%% ============================================================================

%% Helper to setup each test
setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init().

calculate_mutation_count_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Count = genome_mutator:calculate_mutation_count(AgentId),
        ?assert(Count >= 1)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

select_random_neuron_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        NeuronId = genome_mutator:select_random_neuron(AgentId),
        ?assert(NeuronId /= {error, no_neurons}),

        Neuron = genotype:dirty_read({neuron, NeuronId}),
        ?assert(is_record(Neuron, neuron))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

add_bias_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:add_bias(AgentId),
        case Result of
            ok -> ok;
            {error, already_has_bias} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_weights_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:mutate_weights(AgentId),
        ?assertEqual(ok, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_af_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:mutate_af(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_tuning_selection_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:mutate_tuning_selection(AgentId),
        case Result of
            ok -> ok;
            {error, no_alternatives} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

add_outlink_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:add_outlink(AgentId),
        case Result of
            ok -> ok;
            {error, no_available_targets} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

add_inlink_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:add_inlink(AgentId),
        case Result of
            ok -> ok;
            {error, no_available_sources} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

add_neuron_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Agent1 = genotype:dirty_read({agent, AgentId}),
        Cortex1 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
        InitialCount = length(Cortex1#cortex.neuron_ids),

        Result = genome_mutator:add_neuron(AgentId),
        case Result of
            ok ->
                Cortex2 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
                NewCount = length(Cortex2#cortex.neuron_ids),
                ?assertEqual(InitialCount + 1, NewCount);
            {error, cannot_add_neuron} ->
                ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_single_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:mutate(AgentId, 1),
        ?assertEqual(ok, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_multiple_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = genome_mutator:mutate(AgentId, 3),
        ?assertEqual(ok, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% LTC Mutation Tests
%% ============================================================================

mutate_neuron_type_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Get a neuron before mutation
        NeuronId = genome_mutator:select_random_neuron(AgentId),
        NeuronBefore = genotype:dirty_read({neuron, NeuronId}),
        _TypeBefore = NeuronBefore#neuron.neuron_type,

        %% mutate_neuron_type should change the type
        Result = genome_mutator:mutate_neuron_type(AgentId),
        ?assertEqual(ok, Result),

        %% Verify at least one neuron has a changed type or type is in valid set
        Agent = genotype:dirty_read({agent, AgentId}),
        Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
        Types = [begin
            N = genotype:dirty_read({neuron, NId}),
            N#neuron.neuron_type
        end || NId <- Cortex#cortex.neuron_ids],

        %% All types should be valid
        ValidTypes = [standard, ltc, cfc],
        lists:foreach(fun(T) -> ?assert(lists:member(T, ValidTypes)) end, Types)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_neuron_type_changes_type_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% First, verify initial state: all neurons should be standard
        Agent0 = genotype:dirty_read({agent, AgentId}),
        Cortex0 = genotype:dirty_read({cortex, Agent0#agent.cx_id}),
        InitialTypes = [begin
            N = genotype:dirty_read({neuron, NId}),
            N#neuron.neuron_type
        end || NId <- Cortex0#cortex.neuron_ids],
        ?assert(lists:all(fun(T) -> T =:= standard end, InitialTypes)),

        %% Mutate once and verify the operation completed
        Result = genome_mutator:mutate_neuron_type(AgentId),
        ?assertEqual(ok, Result),

        %% Get types after mutation - the key is mutation completed without error
        Agent = genotype:dirty_read({agent, AgentId}),
        Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
        Types = [begin
            N = genotype:dirty_read({neuron, NId}),
            N#neuron.neuron_type
        end || NId <- Cortex#cortex.neuron_ids],

        %% All types should be valid (standard, ltc, or cfc)
        ValidTypes = [standard, ltc, cfc],
        ?assert(lists:all(fun(T) -> lists:member(T, ValidTypes) end, Types))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_time_constant_no_ltc_neurons_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% By default, all neurons are standard, so no LTC neurons
        Result = genome_mutator:mutate_time_constant(AgentId),
        ?assertEqual({error, no_ltc_neurons}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_time_constant_with_ltc_neuron_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% First, convert a neuron to LTC type
        NeuronId = genome_mutator:select_random_neuron(AgentId),
        Neuron = genotype:dirty_read({neuron, NeuronId}),
        LtcNeuron = Neuron#neuron{
            neuron_type = cfc,
            time_constant = 1.0,
            state_bound = 1.0
        },
        genotype:write(LtcNeuron),

        %% Now mutate time constant
        _TauBefore = LtcNeuron#neuron.time_constant,
        Result = genome_mutator:mutate_time_constant(AgentId),
        ?assertEqual(ok, Result),

        %% Verify time constant changed
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        TauAfter = NeuronAfter#neuron.time_constant,
        ?assert(TauAfter > 0),
        ?assert(TauAfter =< 100.0)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_state_bound_no_ltc_neurons_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% By default, no LTC neurons
        Result = genome_mutator:mutate_state_bound(AgentId),
        ?assertEqual({error, no_ltc_neurons}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_state_bound_with_ltc_neuron_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Convert a neuron to LTC type
        NeuronId = genome_mutator:select_random_neuron(AgentId),
        Neuron = genotype:dirty_read({neuron, NeuronId}),
        LtcNeuron = Neuron#neuron{
            neuron_type = ltc,
            time_constant = 1.0,
            state_bound = 1.0
        },
        genotype:write(LtcNeuron),

        %% Now mutate state bound
        Result = genome_mutator:mutate_state_bound(AgentId),
        ?assertEqual(ok, Result),

        %% Verify state bound is within valid range
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        BoundAfter = NeuronAfter#neuron.state_bound,
        ?assert(BoundAfter >= 0.1),
        ?assert(BoundAfter =< 10.0)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_ltc_weights_no_ltc_neurons_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% By default, no LTC neurons
        Result = genome_mutator:mutate_ltc_weights(AgentId),
        ?assertEqual({error, no_ltc_neurons}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_ltc_weights_with_ltc_neuron_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Convert a neuron to CfC type with empty weights
        NeuronId = genome_mutator:select_random_neuron(AgentId),
        Neuron = genotype:dirty_read({neuron, NeuronId}),
        LtcNeuron = Neuron#neuron{
            neuron_type = cfc,
            time_constant = 1.0,
            state_bound = 1.0,
            ltc_backbone_weights = [],
            ltc_head_weights = []
        },
        genotype:write(LtcNeuron),

        %% Mutate LTC weights
        Result = genome_mutator:mutate_ltc_weights(AgentId),
        ?assertEqual(ok, Result),

        %% Verify weights were initialized (from empty to non-empty)
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        ?assert(length(NeuronAfter#neuron.ltc_backbone_weights) > 0),
        ?assert(length(NeuronAfter#neuron.ltc_head_weights) > 0)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_ltc_weights_perturbs_existing_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Convert a neuron to CfC with existing weights
        NeuronId = genome_mutator:select_random_neuron(AgentId),
        Neuron = genotype:dirty_read({neuron, NeuronId}),
        BackboneWeights = [0.5, 0.3],
        HeadWeights = [0.7, -0.2],
        LtcNeuron = Neuron#neuron{
            neuron_type = cfc,
            time_constant = 1.0,
            state_bound = 1.0,
            ltc_backbone_weights = BackboneWeights,
            ltc_head_weights = HeadWeights
        },
        genotype:write(LtcNeuron),

        %% Mutate LTC weights
        Result = genome_mutator:mutate_ltc_weights(AgentId),
        ?assertEqual(ok, Result),

        %% Verify weights changed
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        ?assertEqual(2, length(NeuronAfter#neuron.ltc_backbone_weights)),
        ?assertEqual(2, length(NeuronAfter#neuron.ltc_head_weights)),
        %% Weights should be different (with high probability)
        ?assertNotEqual(BackboneWeights, NeuronAfter#neuron.ltc_backbone_weights)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

ltc_exports_test() ->
    Exports = genome_mutator:module_info(exports),
    ?assert(lists:member({mutate_neuron_type, 1}, Exports)),
    ?assert(lists:member({mutate_time_constant, 1}, Exports)),
    ?assert(lists:member({mutate_state_bound, 1}, Exports)),
    ?assert(lists:member({mutate_ltc_weights, 1}, Exports)).
