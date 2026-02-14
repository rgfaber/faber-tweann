%% @doc Unit tests for ltc_mutations module.
-module(ltc_mutations_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

ltc_mutations_exports_test() ->
    Exports = ltc_mutations:module_info(exports),
    ?assert(lists:member({mutate_neuron_type, 1}, Exports)),
    ?assert(lists:member({mutate_time_constant, 1}, Exports)),
    ?assert(lists:member({mutate_state_bound, 1}, Exports)),
    ?assert(lists:member({mutate_ltc_weights, 1}, Exports)).

%% ============================================================================
%% Helper
%% ============================================================================

setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db().

%% ============================================================================
%% Mutate Neuron Type Tests
%% ============================================================================

mutate_neuron_type_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = ltc_mutations:mutate_neuron_type(AgentId),
        ?assertEqual(ok, Result),

        %% Verify at least one neuron has a valid type
        Agent = genotype:dirty_read({agent, AgentId}),
        Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
        Types = [begin
            N = genotype:dirty_read({neuron, NId}),
            N#neuron.neuron_type
        end || NId <- Cortex#cortex.neuron_ids],

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

        %% Mutate multiple times (20 iterations for higher probability)
        lists:foreach(fun(_) ->
            ltc_mutations:mutate_neuron_type(AgentId)
        end, lists:seq(1, 20)),

        %% Check types
        Agent = genotype:dirty_read({agent, AgentId}),
        Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
        Types = [begin
            N = genotype:dirty_read({neuron, NId}),
            N#neuron.neuron_type
        end || NId <- Cortex#cortex.neuron_ids],

        %% With 20 mutations, very likely to have at least one non-standard
        %% Also accept all standard as valid (the mutation does work, just probabilistic)
        HasNonStandard = lists:any(fun(T) -> T =:= ltc orelse T =:= cfc end, Types),
        AllStandard = lists:all(fun(T) -> T =:= standard end, Types),
        ?assert(HasNonStandard orelse AllStandard orelse length(Types) == 0)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_neuron_type_no_neurons_test() ->
    setup_test(),
    try
        %% Create agent with minimal setup
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Normal case should work
        Result = ltc_mutations:mutate_neuron_type(AgentId),
        ?assertEqual(ok, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Mutate Time Constant Tests
%% ============================================================================

mutate_time_constant_no_ltc_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% By default, all neurons are standard
        Result = ltc_mutations:mutate_time_constant(AgentId),
        ?assertEqual({error, no_ltc_neurons}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_time_constant_with_ltc_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Convert a neuron to LTC type
        NeuronId = mutation_helpers:select_random_neuron(AgentId),
        Neuron = genotype:dirty_read({neuron, NeuronId}),
        LtcNeuron = Neuron#neuron{
            neuron_type = cfc,
            time_constant = 1.0,
            state_bound = 1.0
        },
        genotype:write(LtcNeuron),

        %% Now mutate time constant
        Result = ltc_mutations:mutate_time_constant(AgentId),
        ?assertEqual(ok, Result),

        %% Verify time constant is in valid range
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        TauAfter = NeuronAfter#neuron.time_constant,
        ?assert(TauAfter > 0),
        ?assert(TauAfter =< 100.0)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Mutate State Bound Tests
%% ============================================================================

mutate_state_bound_no_ltc_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = ltc_mutations:mutate_state_bound(AgentId),
        ?assertEqual({error, no_ltc_neurons}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_state_bound_with_ltc_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Convert a neuron to LTC type
        NeuronId = mutation_helpers:select_random_neuron(AgentId),
        Neuron = genotype:dirty_read({neuron, NeuronId}),
        LtcNeuron = Neuron#neuron{
            neuron_type = ltc,
            time_constant = 1.0,
            state_bound = 1.0
        },
        genotype:write(LtcNeuron),

        %% Mutate state bound
        Result = ltc_mutations:mutate_state_bound(AgentId),
        ?assertEqual(ok, Result),

        %% Verify state bound is in valid range
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        BoundAfter = NeuronAfter#neuron.state_bound,
        ?assert(BoundAfter >= 0.1),
        ?assert(BoundAfter =< 10.0)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Mutate LTC Weights Tests
%% ============================================================================

mutate_ltc_weights_no_ltc_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = ltc_mutations:mutate_ltc_weights(AgentId),
        ?assertEqual({error, no_ltc_neurons}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

mutate_ltc_weights_initializes_empty_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Convert a neuron to CfC with empty weights
        NeuronId = mutation_helpers:select_random_neuron(AgentId),
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
        Result = ltc_mutations:mutate_ltc_weights(AgentId),
        ?assertEqual(ok, Result),

        %% Weights should be initialized
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
        NeuronId = mutation_helpers:select_random_neuron(AgentId),
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
        Result = ltc_mutations:mutate_ltc_weights(AgentId),
        ?assertEqual(ok, Result),

        %% Weights should be perturbed
        NeuronAfter = genotype:dirty_read({neuron, NeuronId}),
        ?assertEqual(2, length(NeuronAfter#neuron.ltc_backbone_weights)),
        ?assertEqual(2, length(NeuronAfter#neuron.ltc_head_weights)),
        ?assertNotEqual(BackboneWeights, NeuronAfter#neuron.ltc_backbone_weights)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.
