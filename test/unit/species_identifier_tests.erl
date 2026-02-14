%% @doc Unit tests for species_identifier module.
-module(species_identifier_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

species_identifier_exports_test() ->
    Exports = species_identifier:module_info(exports),
    ?assert(lists:member({identify_species, 3}, Exports)),
    ?assert(lists:member({calculate_distance, 2}, Exports)),
    ?assert(lists:member({create_fingerprint, 1}, Exports)).

%% ============================================================================
%% Distance Calculation Tests
%% ============================================================================

calculate_distance_identical_test() ->
    F1 = [1.0, 2.0, 3.0],
    F2 = [1.0, 2.0, 3.0],
    Distance = species_identifier:calculate_distance(F1, F2),
    ?assertEqual(0.0, Distance).

calculate_distance_different_test() ->
    F1 = [0.0, 0.0, 0.0],
    F2 = [1.0, 1.0, 1.0],
    Distance = species_identifier:calculate_distance(F1, F2),
    %% Distance = sqrt(1^2 + 1^2 + 1^2) = sqrt(3) ≈ 1.732
    Expected = math:sqrt(3.0),
    ?assert(abs(Distance - Expected) < 0.001).

calculate_distance_partial_similarity_test() ->
    F1 = [1.0, 0.0, 0.0],
    F2 = [1.0, 1.0, 0.0],
    Distance = species_identifier:calculate_distance(F1, F2),
    %% Distance = sqrt(0^2 + 1^2 + 0^2) = 1.0
    ?assertEqual(1.0, Distance).

calculate_distance_empty_test() ->
    F1 = [],
    F2 = [],
    Distance = species_identifier:calculate_distance(F1, F2),
    ?assertEqual(0.0, Distance).

calculate_distance_single_dimension_test() ->
    F1 = [5.0],
    F2 = [2.0],
    Distance = species_identifier:calculate_distance(F1, F2),
    ?assertEqual(3.0, Distance).

calculate_distance_symmetric_test() ->
    F1 = [1.0, 2.0, 3.0],
    F2 = [4.0, 5.0, 6.0],
    D1 = species_identifier:calculate_distance(F1, F2),
    D2 = species_identifier:calculate_distance(F2, F1),
    ?assertEqual(D1, D2).

%% ============================================================================
%% Species Identification Tests
%% ============================================================================

identify_species_empty_test() ->
    Result = species_identifier:identify_species([], 1.0, #{}),
    ?assertEqual(#{}, Result).

identify_species_single_agent_test() ->
    AgentFingerprints = [{agent1, [1.0, 2.0, 3.0]}],
    Threshold = 1.0,
    Result = species_identifier:identify_species(AgentFingerprints, Threshold, #{}),

    %% Should create one species with one member
    ?assertEqual(1, map_size(Result)),
    [Members] = maps:values(Result),
    ?assertEqual([agent1], Members).

identify_species_similar_agents_test() ->
    %% Two very similar agents - should be same species
    AgentFingerprints = [
        {agent1, [1.0, 2.0, 3.0]},
        {agent2, [1.1, 2.1, 3.1]}  % Distance ≈ 0.173
    ],
    Threshold = 0.5,  % Generous threshold
    Result = species_identifier:identify_species(AgentFingerprints, Threshold, #{}),

    %% Should create one species with both agents
    ?assertEqual(1, map_size(Result)),
    [Members] = maps:values(Result),
    ?assertEqual(2, length(Members)),
    ?assert(lists:member(agent1, Members)),
    ?assert(lists:member(agent2, Members)).

identify_species_different_agents_test() ->
    %% Two very different agents - should be different species
    AgentFingerprints = [
        {agent1, [0.0, 0.0, 0.0]},
        {agent2, [10.0, 10.0, 10.0]}  % Distance ≈ 17.3
    ],
    Threshold = 1.0,  % Strict threshold
    Result = species_identifier:identify_species(AgentFingerprints, Threshold, #{}),

    %% Should create two species
    ?assertEqual(2, map_size(Result)),
    SpeciesLists = maps:values(Result),
    ?assert(lists:member([agent1], SpeciesLists) orelse
            lists:member([agent2], SpeciesLists)).

identify_species_multiple_species_test() ->
    %% Three agents: two similar, one different
    AgentFingerprints = [
        {agent1, [1.0, 1.0, 1.0]},
        {agent2, [1.1, 1.1, 1.1]},  % Similar to agent1
        {agent3, [10.0, 10.0, 10.0]}  % Very different
    ],
    Threshold = 0.5,
    Result = species_identifier:identify_species(AgentFingerprints, Threshold, #{}),

    %% Should create two species
    ?assertEqual(2, map_size(Result)),

    %% One species should have 2 members, other should have 1
    MemberCounts = lists:sort([length(M) || M <- maps:values(Result)]),
    ?assertEqual([1, 2], MemberCounts).

identify_species_with_existing_species_test() ->
    %% Test adding agents to existing species
    ExistingSpecies = #{
        specie1 => {[1.0, 1.0, 1.0], [existing_agent]}
    },

    %% New agent similar to existing species
    AgentFingerprints = [{agent1, [1.1, 1.1, 1.1]}],
    Threshold = 0.5,

    Result = species_identifier:identify_species(
        AgentFingerprints, Threshold, ExistingSpecies
    ),

    %% Should still have 1 species with 2 members
    ?assertEqual(1, map_size(Result)),
    [Members] = maps:values(Result),
    ?assertEqual(2, length(Members)),
    ?assert(lists:member(existing_agent, Members)),
    ?assert(lists:member(agent1, Members)).

identify_species_threshold_sensitivity_test() ->
    %% Same agents, different thresholds
    AgentFingerprints = [
        {agent1, [1.0, 1.0, 1.0]},
        {agent2, [2.0, 2.0, 2.0]}  % Distance ≈ 1.732
    ],

    %% Strict threshold - should create 2 species
    Result1 = species_identifier:identify_species(AgentFingerprints, 1.0, #{}),
    ?assertEqual(2, map_size(Result1)),

    %% Generous threshold - should create 1 species
    Result2 = species_identifier:identify_species(AgentFingerprints, 5.0, #{}),
    ?assertEqual(1, map_size(Result2)).

%% ============================================================================
%% Fingerprint Creation Tests
%% ============================================================================

create_fingerprint_unknown_agent_test() ->
    %% Unknown agent should return zero fingerprint
    Fingerprint = species_identifier:create_fingerprint(unknown_agent),
    ?assertEqual([0.0], Fingerprint).

%% Helper to setup each test
setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db().

create_fingerprint_xor_agent_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        AgentId = {genotype:generate_UniqueId(), agent},
        Constraint = #constraint{morphology = xor_mimic},

        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Fingerprint = species_identifier:create_fingerprint(AgentId),

        %% XOR should have 4 outputs (for 4 input combinations)
        ?assertEqual(4, length(Fingerprint)),

        %% Each output should be a float
        ?assert(lists:all(fun is_float/1, Fingerprint))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

create_fingerprint_deterministic_test() ->
    genotype:init_db(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        AgentId = {genotype:generate_UniqueId(), agent},
        Constraint = #constraint{morphology = xor_mimic},

        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Create fingerprint twice
        F1 = species_identifier:create_fingerprint(AgentId),
        F2 = species_identifier:create_fingerprint(AgentId),

        %% Should be identical
        ?assertEqual(F1, F2)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

create_fingerprint_different_agents_test() ->
    genotype:init_db(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        Constraint = #constraint{morphology = xor_mimic},

        Agent1 = {genotype:generate_UniqueId(), agent},
        Agent2 = {genotype:generate_UniqueId(), agent},

        genotype:construct_Agent(SpecieId, Agent1, Constraint),
        genotype:construct_Agent(SpecieId, Agent2, Constraint),

        F1 = species_identifier:create_fingerprint(Agent1),
        F2 = species_identifier:create_fingerprint(Agent2),

        %% Different agents should (likely) have different fingerprints
        %% Note: This is probabilistic, but with different agent IDs
        %% the hash-based implementation will differ
        ?assertNotEqual(F1, F2)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Integration Tests
%% ============================================================================

full_speciation_workflow_test() ->
    genotype:init_db(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        Constraint = #constraint{morphology = xor_mimic},

        %% Create 5 agents
        Agents = [
            {genotype:generate_UniqueId(), agent},
            {genotype:generate_UniqueId(), agent},
            {genotype:generate_UniqueId(), agent},
            {genotype:generate_UniqueId(), agent},
            {genotype:generate_UniqueId(), agent}
        ],

        lists:foreach(
            fun(AgentId) ->
                genotype:construct_Agent(SpecieId, AgentId, Constraint)
            end,
            Agents
        ),

        %% Create fingerprints
        Fingerprints = lists:map(
            fun(AgentId) ->
                {AgentId, species_identifier:create_fingerprint(AgentId)}
            end,
            Agents
        ),

        %% Identify species with moderate threshold
        Species = species_identifier:identify_species(Fingerprints, 2.0, #{}),

        %% Verify all agents were assigned
        AllMembers = lists:flatten(maps:values(Species)),
        ?assertEqual(5, length(AllMembers)),

        %% Verify each agent appears exactly once
        SortedAgents = lists:sort(Agents),
        SortedMembers = lists:sort(AllMembers),
        ?assertEqual(SortedAgents, SortedMembers)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

species_stability_test() ->
    %% Test that similar agents consistently group together
    AgentFingerprints = [
        {agent1, [1.0, 1.0]},
        {agent2, [1.1, 1.1]},
        {agent3, [1.05, 1.05]},
        {agent4, [10.0, 10.0]},
        {agent5, [10.1, 10.1]}
    ],

    Threshold = 0.5,

    %% Run speciation multiple times (should be deterministic)
    Result1 = species_identifier:identify_species(AgentFingerprints, Threshold, #{}),
    Result2 = species_identifier:identify_species(AgentFingerprints, Threshold, #{}),

    %% Number of species should be consistent
    ?assertEqual(map_size(Result1), map_size(Result2)),

    %% Should create 2 species (agents 1-3 together, agents 4-5 together)
    ?assertEqual(2, map_size(Result1)).
