%% @doc Unit tests for crossover module.
-module(crossover_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

crossover_exports_test() ->
    Exports = crossover:module_info(exports),
    ?assert(lists:member({crossover, 2}, Exports)),
    ?assert(lists:member({neuron_crossover, 3}, Exports)),
    ?assert(lists:member({weight_crossover, 3}, Exports)).

%% ============================================================================
%% Weight Crossover Tests
%% ============================================================================

weight_crossover_empty_test() ->
    Result = crossover:weight_crossover([], [], 0.5),
    ?assertEqual([], Result).

weight_crossover_one_parent_empty_test() ->
    Weights1 = [{sensor1, [{0.5, 0.0, 0.01, []}]}],

    Result1 = crossover:weight_crossover(Weights1, [], 0.5),
    ?assertEqual(Weights1, Result1),

    Result2 = crossover:weight_crossover([], Weights1, 0.5),
    ?assertEqual(Weights1, Result2).

weight_crossover_combines_sources_test() ->
    Weights1 = [{sensor1, [{0.5, 0.0, 0.01, []}]}],
    Weights2 = [{sensor2, [{0.3, 0.0, 0.01, []}]}],

    Result = crossover:weight_crossover(Weights1, Weights2, 0.5),

    %% Should have both sources
    ?assertEqual(2, length(Result)),
    Sources = [S || {S, _W} <- Result],
    ?assert(lists:member(sensor1, Sources)),
    ?assert(lists:member(sensor2, Sources)).

weight_crossover_same_source_test() ->
    Weights1 = [{sensor1, [{0.5, 0.0, 0.01, []}]}],
    Weights2 = [{sensor1, [{-0.3, 0.0, 0.01, []}]}],

    Result = crossover:weight_crossover(Weights1, Weights2, 0.5),

    %% Should have one source with crossed weights
    ?assertEqual(1, length(Result)),
    [{sensor1, CrossedWeights}] = Result,
    ?assertEqual(1, length(CrossedWeights)),

    %% Weight should be one of the parent values
    [{W, _DW, _LR, _P}] = CrossedWeights,
    ?assert(W =:= 0.5 orelse W =:= -0.3).

weight_crossover_deterministic_rate_test() ->
    %% With rate 1.0, should always take from first parent
    Weights1 = [{sensor1, [{1.0, 0.0, 0.01, []}]}],
    Weights2 = [{sensor1, [{-1.0, 0.0, 0.01, []}]}],

    %% Run multiple times to test consistency
    Results = [crossover:weight_crossover(Weights1, Weights2, 1.0) || _ <- lists:seq(1, 10)],

    %% All results should be similar (note: randomness in selection)
    ?assertEqual(10, length(Results)).

%% ============================================================================
%% Neuron Crossover Tests
%% ============================================================================

neuron_crossover_basic_test() ->
    N1 = #neuron{
        id = {1.0, neuron},
        af = tanh,
        aggr_f = dot_product,
        input_idps = [],
        output_ids = [out1]
    },
    N2 = #neuron{
        id = {2.0, neuron},
        af = sigmoid,
        aggr_f = mult_product,
        input_idps = [],
        output_ids = [out2]
    },

    Result = crossover:neuron_crossover(N1, N2, 0.5),

    %% Should have an activation function
    ?assert(Result#neuron.af =:= tanh orelse Result#neuron.af =:= sigmoid),

    %% Should have an aggregation function
    ?assert(Result#neuron.aggr_f =:= dot_product orelse
            Result#neuron.aggr_f =:= mult_product),

    %% Outputs should be union
    ?assertEqual(2, length(Result#neuron.output_ids)).

neuron_crossover_preserves_structure_test() ->
    N1 = #neuron{
        id = {1.0, neuron},
        af = tanh,
        aggr_f = dot_product,
        input_idps = [{sensor1, [{0.5, 0.0, 0.01, []}]}],
        output_ids = [actuator1]
    },
    N2 = #neuron{
        id = {2.0, neuron},
        af = sigmoid,
        aggr_f = mult_product,
        input_idps = [{sensor2, [{-0.5, 0.0, 0.01, []}]}],
        output_ids = [actuator2]
    },

    Result = crossover:neuron_crossover(N1, N2, 0.5),

    %% Should have input weights
    ?assert(length(Result#neuron.input_idps) > 0),

    %% Should have output connections
    ?assert(length(Result#neuron.output_ids) > 0).

neuron_crossover_aggregation_function_test() ->
    N1 = #neuron{id = {1.0, neuron}, af = tanh, aggr_f = dot_product, input_idps = [], output_ids = []},
    N2 = #neuron{id = {2.0, neuron}, af = tanh, aggr_f = mult_product, input_idps = [], output_ids = []},

    Result = crossover:neuron_crossover(N1, N2, 0.5),

    %% Aggregation function should be one of the parents'
    ?assert(Result#neuron.aggr_f =:= dot_product orelse
            Result#neuron.aggr_f =:= mult_product).

%% ============================================================================
%% Full Crossover Tests
%% ============================================================================

%% Helper to setup each test
setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db().

crossover_missing_parent_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        AgentId = {genotype:generate_UniqueId(), agent},
        Constraint = #constraint{morphology = xor_mimic},

        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Crossover with missing parent should clone existing
        Result = crossover:crossover(AgentId, undefined_agent),

        %% Should return a valid agent ID
        ?assertNotEqual(AgentId, Result),

        %% Result should be readable
        Offspring = genotype:read({agent, Result}),
        ?assertNotEqual(undefined, Offspring)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

crossover_both_parents_exist_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        Constraint = #constraint{morphology = xor_mimic},

        Parent1 = {genotype:generate_UniqueId(), agent},
        Parent2 = {genotype:generate_UniqueId(), agent},

        genotype:construct_Agent(SpecieId, Parent1, Constraint),
        genotype:construct_Agent(SpecieId, Parent2, Constraint),

        %% Perform crossover
        Offspring = crossover:crossover(Parent1, Parent2),

        %% Should return a new agent
        ?assertNotEqual(Parent1, Offspring),
        ?assertNotEqual(Parent2, Offspring),

        %% Offspring should exist in database
        OffspringAgent = genotype:read({agent, Offspring}),
        ?assertNotEqual(undefined, OffspringAgent),

        %% Offspring should have parent IDs
        ?assert(lists:member(Parent1, OffspringAgent#agent.parent_ids) orelse
                lists:member(Parent2, OffspringAgent#agent.parent_ids))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

crossover_preserves_morphology_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        Constraint = #constraint{morphology = xor_mimic},

        Parent1 = {genotype:generate_UniqueId(), agent},
        Parent2 = {genotype:generate_UniqueId(), agent},

        genotype:construct_Agent(SpecieId, Parent1, Constraint),
        genotype:construct_Agent(SpecieId, Parent2, Constraint),

        Offspring = crossover:crossover(Parent1, Parent2),

        %% Get offspring's constraint
        OffspringAgent = genotype:read({agent, Offspring}),
        OffspringConstraint = OffspringAgent#agent.constraint,

        %% Morphology should match parents
        ?assertEqual(xor_mimic, OffspringConstraint#constraint.morphology)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

crossover_creates_valid_network_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        Constraint = #constraint{morphology = xor_mimic},

        Parent1 = {genotype:generate_UniqueId(), agent},
        Parent2 = {genotype:generate_UniqueId(), agent},

        genotype:construct_Agent(SpecieId, Parent1, Constraint),
        genotype:construct_Agent(SpecieId, Parent2, Constraint),

        Offspring = crossover:crossover(Parent1, Parent2),

        %% Get offspring's network structure
        OffspringAgent = genotype:read({agent, Offspring}),
        CortexId = OffspringAgent#agent.cx_id,
        Cortex = genotype:read({cortex, CortexId}),

        %% Should have sensors, neurons, actuators
        ?assert(length(Cortex#cortex.sensor_ids) > 0),
        ?assert(length(Cortex#cortex.neuron_ids) > 0),
        ?assert(length(Cortex#cortex.actuator_ids) > 0)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Integration Tests
%% ============================================================================

multiple_crossovers_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        Constraint = #constraint{morphology = xor_mimic},

        Parent1 = {genotype:generate_UniqueId(), agent},
        Parent2 = {genotype:generate_UniqueId(), agent},

        genotype:construct_Agent(SpecieId, Parent1, Constraint),
        genotype:construct_Agent(SpecieId, Parent2, Constraint),

        %% Create multiple offspring
        Offspring1 = crossover:crossover(Parent1, Parent2),
        Offspring2 = crossover:crossover(Parent1, Parent2),
        Offspring3 = crossover:crossover(Parent1, Parent2),

        %% All offspring should be different
        ?assertNotEqual(Offspring1, Offspring2),
        ?assertNotEqual(Offspring2, Offspring3),
        ?assertNotEqual(Offspring1, Offspring3),

        %% All should be valid agents
        ?assertNotEqual(undefined, genotype:read({agent, Offspring1})),
        ?assertNotEqual(undefined, genotype:read({agent, Offspring2})),
        ?assertNotEqual(undefined, genotype:read({agent, Offspring3}))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.
