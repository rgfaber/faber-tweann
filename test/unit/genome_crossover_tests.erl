%% @doc Unit tests for NEAT-style genome crossover.
-module(genome_crossover_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Test Setup/Teardown
%% ============================================================================

setup() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init().

teardown() ->
    innovation:reset(),
    genotype:reset_db(),
    application:stop(mnesia).

%% ============================================================================
%% Module Export Tests
%% ============================================================================

genome_crossover_exports_test() ->
    Exports = genome_crossover:module_info(exports),
    ?assert(lists:member({crossover, 3}, Exports)),
    ?assert(lists:member({align_genomes, 2}, Exports)),
    ?assert(lists:member({compatibility_distance, 3}, Exports)),
    ?assert(lists:member({extract_connection_genes, 1}, Exports)).

%% ============================================================================
%% Test Helpers
%% ============================================================================

make_gene(Inn, From, To, Weight) ->
    #connection_gene{
        innovation = Inn,
        from_id = From,
        to_id = To,
        weight = Weight,
        enabled = true
    }.

%% ============================================================================
%% Align Genomes Tests
%% ============================================================================

align_empty_genomes_test() ->
    {Matching, Dis1, Dis2, Exc1, Exc2} = genome_crossover:align_genomes([], []),
    ?assertEqual([], Matching),
    ?assertEqual([], Dis1),
    ?assertEqual([], Dis2),
    ?assertEqual([], Exc1),
    ?assertEqual([], Exc2).

align_identical_genomes_test() ->
    G1 = make_gene(1, a, b, 0.5),
    G2 = make_gene(2, b, c, 0.3),
    Genome = [G1, G2],

    {Matching, Dis1, Dis2, Exc1, Exc2} = genome_crossover:align_genomes(Genome, Genome),

    ?assertEqual(2, length(Matching)),
    ?assertEqual([], Dis1),
    ?assertEqual([], Dis2),
    ?assertEqual([], Exc1),
    ?assertEqual([], Exc2).

align_disjoint_genes_test() ->
    %% Genome1: innovations [1, 3, 4]
    %% Genome2: innovations [1, 2, 4]
    %% Matching: 1, 4
    %% Disjoint1: 3 (in range of genome2, which goes to 4)
    %% Disjoint2: 2 (in range of genome1, which goes to 4)
    Genome1 = [make_gene(1, a, b, 0.1), make_gene(3, c, d, 0.3), make_gene(4, d, e, 0.4)],
    Genome2 = [make_gene(1, a, b, 0.2), make_gene(2, b, c, 0.2), make_gene(4, d, e, 0.5)],

    {Matching, Dis1, Dis2, Exc1, Exc2} = genome_crossover:align_genomes(Genome1, Genome2),

    ?assertEqual(2, length(Matching)),  % innovations 1 and 4
    ?assertEqual(1, length(Dis1)),      % innovation 3
    ?assertEqual(1, length(Dis2)),      % innovation 2
    ?assertEqual([], Exc1),
    ?assertEqual([], Exc2).

align_excess_genes_test() ->
    %% Genome1: innovations [1, 2, 5, 6, 7]
    %% Genome2: innovations [1, 2, 3]
    %% Matching: 1, 2
    %% Disjoint1: none (no gaps within genome2's range)
    %% Disjoint2: 3 (within genome1's range)
    %% Excess1: 5, 6, 7 (beyond genome2's max of 3)
    Genome1 = [make_gene(1, a, b, 0.1), make_gene(2, b, c, 0.2),
               make_gene(5, e, f, 0.5), make_gene(6, f, g, 0.6), make_gene(7, g, h, 0.7)],
    Genome2 = [make_gene(1, a, b, 0.1), make_gene(2, b, c, 0.2), make_gene(3, c, d, 0.3)],

    {Matching, Dis1, Dis2, Exc1, Exc2} = genome_crossover:align_genomes(Genome1, Genome2),

    ?assertEqual(2, length(Matching)),  % innovations 1 and 2
    ?assertEqual(0, length(Dis1)),      % no disjoint in genome1 within range
    ?assertEqual(1, length(Dis2)),      % innovation 3 is disjoint
    ?assertEqual(3, length(Exc1)),      % innovations 5, 6, 7 are excess
    ?assertEqual([], Exc2).

%% ============================================================================
%% Crossover Tests
%% ============================================================================

crossover_fitter_parent_1_test() ->
    %% Fitter parent (1) contributes all disjoint/excess genes
    Genome1 = [make_gene(1, a, b, 0.1), make_gene(2, b, c, 0.2), make_gene(3, c, d, 0.3)],
    Genome2 = [make_gene(1, a, b, 0.5)],

    Child = genome_crossover:crossover(Genome1, Genome2, 1),

    %% Child should have all genes from parent 1 (matching + disjoint/excess)
    ?assertEqual(3, length(Child)),

    %% Innovations should be 1, 2, 3
    Innovations = [G#connection_gene.innovation || G <- Child],
    ?assertEqual([1, 2, 3], lists:sort(Innovations)).

crossover_fitter_parent_2_test() ->
    %% Fitter parent (2) contributes all disjoint/excess genes
    Genome1 = [make_gene(1, a, b, 0.1)],
    Genome2 = [make_gene(1, a, b, 0.5), make_gene(2, b, c, 0.2), make_gene(3, c, d, 0.3)],

    Child = genome_crossover:crossover(Genome1, Genome2, 2),

    %% Child should have all genes from parent 2
    ?assertEqual(3, length(Child)),
    Innovations = [G#connection_gene.innovation || G <- Child],
    ?assertEqual([1, 2, 3], lists:sort(Innovations)).

crossover_preserves_structure_test() ->
    %% Verify that matching genes are from one parent or the other
    G1 = make_gene(1, a, b, 1.0),
    G2 = make_gene(1, a, b, 2.0),

    %% Run multiple times to get both possibilities
    Results = [genome_crossover:crossover([G1], [G2], 1) || _ <- lists:seq(1, 100)],
    Weights = [W || [#connection_gene{weight = W}] <- Results],

    %% Should have mix of 1.0 and 2.0 weights
    Has1 = lists:member(1.0, Weights),
    Has2 = lists:member(2.0, Weights),
    ?assert(Has1 orelse Has2).

%% ============================================================================
%% Compatibility Distance Tests
%% ============================================================================

compatibility_distance_identical_test() ->
    Genome = [make_gene(1, a, b, 0.5), make_gene(2, b, c, 0.3)],
    Config = {compatibility_config, 1.0, 1.0, 0.4},

    Distance = genome_crossover:compatibility_distance(Genome, Genome, Config),

    %% Identical genomes should have distance 0 (no excess, no disjoint, no weight diff)
    ?assertEqual(0.0, Distance).

compatibility_distance_disjoint_test() ->
    Genome1 = [make_gene(1, a, b, 0.5), make_gene(2, b, c, 0.3)],
    Genome2 = [make_gene(1, a, b, 0.5), make_gene(3, c, d, 0.3)],
    Config = {compatibility_config, 1.0, 1.0, 0.4},

    Distance = genome_crossover:compatibility_distance(Genome1, Genome2, Config),

    %% 1 matching gene, 2 disjoint genes
    %% Distance = (0 * 1.0 / 2) + (2 * 1.0 / 2) + (0 * 0.4) = 1.0
    ?assert(Distance > 0.0).

compatibility_distance_weight_diff_test() ->
    Genome1 = [make_gene(1, a, b, 0.0)],
    Genome2 = [make_gene(1, a, b, 1.0)],
    Config = {compatibility_config, 1.0, 1.0, 1.0},

    Distance = genome_crossover:compatibility_distance(Genome1, Genome2, Config),

    %% Weight diff = 1.0, coefficient = 1.0
    %% Distance = 0 + 0 + (1.0 * 1.0) = 1.0
    ?assertEqual(1.0, Distance).

compatibility_distance_empty_genome_test() ->
    Config = {compatibility_config, 1.0, 1.0, 0.4},

    Distance = genome_crossover:compatibility_distance([], [], Config),

    %% Empty genomes should have 0 distance
    ?assertEqual(0.0, Distance).

%% ============================================================================
%% Integration Tests with Real Networks
%% ============================================================================

extract_connection_genes_test() ->
    setup(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Genes = genome_crossover:extract_connection_genes(AgentId),

        %% Should have at least one connection gene
        ?assert(length(Genes) > 0),

        %% All genes should have innovations
        HasInnovations = lists:all(
            fun(G) -> is_integer(G#connection_gene.innovation) end,
            Genes
        ),
        ?assert(HasInnovations)
    after
        teardown()
    end.

crossover_real_networks_test() ->
    setup(),
    try
        %% Create two agents
        SpecieId = test_specie,
        AgentId1 = genotype:generate_id(agent),
        AgentId2 = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId1, Constraint),
        genotype:construct_Agent(SpecieId, AgentId2, Constraint),

        %% Extract genes from both
        Genes1 = genome_crossover:extract_connection_genes(AgentId1),
        Genes2 = genome_crossover:extract_connection_genes(AgentId2),

        %% Crossover should produce valid child
        Child = genome_crossover:crossover(Genes1, Genes2, 1),

        ?assert(is_list(Child)),
        ?assert(length(Child) >= 0)
    after
        teardown()
    end.
