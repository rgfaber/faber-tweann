%% @doc Unit tests for fitness_postprocessor module.
-module(fitness_postprocessor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

fitness_postprocessor_exports_test() ->
    Exports = fitness_postprocessor:module_info(exports),
    ?assert(lists:member({size_proportional, 2}, Exports)),
    ?assert(lists:member({normalize, 1}, Exports)),
    ?assert(lists:member({pareto_dominance, 1}, Exports)).

%% ============================================================================
%% Size Proportional Tests
%% ============================================================================

size_proportional_empty_test() ->
    Result = fitness_postprocessor:size_proportional([], 0.1),
    ?assertEqual([], Result).

%% Helper to setup each test
setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db().

size_proportional_applies_penalty_test() ->
    %% Need to setup Mnesia and create test agents
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        AgentId = {genotype:generate_UniqueId(), agent},
        Constraint = #constraint{morphology = xor_mimic},

        %% Create test agent with known network size
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Get the agent to verify it was created
        Agent = genotype:read({agent, AgentId}),
        ?assertNotEqual(undefined, Agent),

        %% Get cortex to count neurons
        CortexId = Agent#agent.cx_id,
        Cortex = genotype:read({cortex, CortexId}),
        NumNeurons = length(Cortex#cortex.neuron_ids),

        %% Apply size-proportional penalty
        Fitnesses = [{AgentId, [100.0]}],
        PenaltyFactor = 0.1,
        Result = fitness_postprocessor:size_proportional(Fitnesses, PenaltyFactor),

        %% Check penalty was applied
        [{ResultId, [ResultFitness]}] = Result,
        ?assertEqual(AgentId, ResultId),

        %% Fitness should be reduced by NumNeurons * PenaltyFactor
        ExpectedFitness = 100.0 - (NumNeurons * PenaltyFactor),
        ?assertEqual(ExpectedFitness, ResultFitness)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

size_proportional_multi_objective_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        AgentId = {genotype:generate_UniqueId(), agent},
        Constraint = #constraint{morphology = xor_mimic},

        %% Create test agent
        genotype:construct_Agent(SpecieId, AgentId, Constraint),
        Agent = genotype:read({agent, AgentId}),
        CortexId = Agent#agent.cx_id,
        Cortex = genotype:read({cortex, CortexId}),
        NumNeurons = length(Cortex#cortex.neuron_ids),

        %% Multi-objective fitness
        Fitnesses = [{AgentId, [100.0, 50.0, 25.0]}],
        PenaltyFactor = 0.5,
        Result = fitness_postprocessor:size_proportional(Fitnesses, PenaltyFactor),

        %% Only first objective should be penalized
        [{ResultId, [F1, F2, F3]}] = Result,
        ?assertEqual(AgentId, ResultId),
        ?assertEqual(100.0 - (NumNeurons * PenaltyFactor), F1),
        ?assertEqual(50.0, F2),  % Unchanged
        ?assertEqual(25.0, F3)   % Unchanged
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

size_proportional_unknown_agent_test() ->
    %% Unknown agent ID should get 0 size (no penalty)
    Fitnesses = [{unknown_agent, [100.0]}],
    PenaltyFactor = 0.1,
    Result = fitness_postprocessor:size_proportional(Fitnesses, PenaltyFactor),

    [{_ResultId, [ResultFitness]}] = Result,
    ?assertEqual(100.0, ResultFitness).  % No penalty if agent not found

%% ============================================================================
%% Normalize Tests
%% ============================================================================

normalize_empty_test() ->
    Result = fitness_postprocessor:normalize([]),
    ?assertEqual([], Result).

normalize_single_agent_test() ->
    %% Single agent should normalize to 0.5 (no variance)
    Fitnesses = [{agent1, [100.0]}],
    Result = fitness_postprocessor:normalize(Fitnesses),

    [{agent1, [NormalizedFitness]}] = Result,
    ?assertEqual(0.5, NormalizedFitness).

normalize_scales_to_01_test() ->
    Fitnesses = [
        {agent1, [100.0]},
        {agent2, [50.0]},
        {agent3, [0.0]}
    ],
    Result = fitness_postprocessor:normalize(Fitnesses),

    %% Extract normalized values
    ResultMap = maps:from_list(Result),

    %% Check normalization
    ?assertEqual([1.0], maps:get(agent1, ResultMap)),   % Max -> 1.0
    ?assertEqual([0.5], maps:get(agent2, ResultMap)),   % Mid -> 0.5
    ?assertEqual([0.0], maps:get(agent3, ResultMap)).   % Min -> 0.0

normalize_multi_objective_test() ->
    Fitnesses = [
        {agent1, [100.0, 10.0]},
        {agent2, [50.0, 20.0]},
        {agent3, [0.0, 15.0]}
    ],
    Result = fitness_postprocessor:normalize(Fitnesses),

    ResultMap = maps:from_list(Result),

    %% Check first objective (range 0-100)
    [F1_agent1, _] = maps:get(agent1, ResultMap),
    [F1_agent2, _] = maps:get(agent2, ResultMap),
    [F1_agent3, _] = maps:get(agent3, ResultMap),

    ?assertEqual(1.0, F1_agent1),
    ?assertEqual(0.5, F1_agent2),
    ?assertEqual(0.0, F1_agent3),

    %% Check second objective (range 10-20)
    [_, F2_agent1] = maps:get(agent1, ResultMap),
    [_, F2_agent2] = maps:get(agent2, ResultMap),
    [_, F2_agent3] = maps:get(agent3, ResultMap),

    ?assertEqual(0.0, F2_agent1),   % Min
    ?assertEqual(1.0, F2_agent2),   % Max
    ?assertEqual(0.5, F2_agent3).   % Mid

normalize_equal_fitness_test() ->
    %% All agents have same fitness -> 0.5 for all
    Fitnesses = [
        {agent1, [50.0]},
        {agent2, [50.0]},
        {agent3, [50.0]}
    ],
    Result = fitness_postprocessor:normalize(Fitnesses),

    %% All should be 0.5
    ?assert(lists:all(
        fun({_Id, [F]}) -> F =:= 0.5 end,
        Result
    )).

%% ============================================================================
%% Pareto Dominance Tests
%% ============================================================================

pareto_empty_test() ->
    Result = fitness_postprocessor:pareto_dominance([]),
    ?assertEqual([], Result).

pareto_single_agent_test() ->
    Fitnesses = [{agent1, [100.0]}],
    Result = fitness_postprocessor:pareto_dominance(Fitnesses),

    %% Single agent is non-dominated (rank 1)
    ?assertEqual([{agent1, [1.0]}], Result).

pareto_non_dominated_front_test() ->
    %% Three non-dominated solutions (no one dominates others)
    Fitnesses = [
        {agent1, [100.0, 10.0]},  % Good on F1, poor on F2
        {agent2, [50.0, 50.0]},   % Balanced
        {agent3, [10.0, 100.0]}   % Poor on F1, good on F2
    ],
    Result = fitness_postprocessor:pareto_dominance(Fitnesses),

    %% All should be rank 1 (non-dominated)
    ResultMap = maps:from_list(Result),
    ?assertEqual([1.0], maps:get(agent1, ResultMap)),
    ?assertEqual([1.0], maps:get(agent2, ResultMap)),
    ?assertEqual([1.0], maps:get(agent3, ResultMap)).

pareto_dominated_solution_test() ->
    %% agent4 is dominated by agent2
    Fitnesses = [
        {agent1, [100.0, 10.0]},
        {agent2, [50.0, 50.0]},
        {agent3, [10.0, 100.0]},
        {agent4, [30.0, 30.0]}    % Dominated by agent2
    ],
    Result = fitness_postprocessor:pareto_dominance(Fitnesses),

    ResultMap = maps:from_list(Result),

    %% agent1, agent2, agent3 should be rank 1
    ?assertEqual([1.0], maps:get(agent1, ResultMap)),
    ?assertEqual([1.0], maps:get(agent2, ResultMap)),
    ?assertEqual([1.0], maps:get(agent3, ResultMap)),

    %% agent4 should be rank 2 (dominated)
    ?assertEqual([2.0], maps:get(agent4, ResultMap)).

pareto_multiple_fronts_test() ->
    %% Multiple Pareto fronts
    Fitnesses = [
        {agent1, [100.0, 100.0]},  % Front 1 - best on both
        {agent2, [90.0, 80.0]},    % Front 2 - dominated by agent1
        {agent3, [80.0, 90.0]},    % Front 2 - dominated by agent1
        {agent4, [70.0, 70.0]}     % Front 3 - dominated by agent2 and agent3
    ],
    Result = fitness_postprocessor:pareto_dominance(Fitnesses),

    ResultMap = maps:from_list(Result),

    %% Check ranks
    ?assertEqual([1.0], maps:get(agent1, ResultMap)),  % Front 1
    ?assertEqual([2.0], maps:get(agent2, ResultMap)),  % Front 2
    ?assertEqual([2.0], maps:get(agent3, ResultMap)),  % Front 2
    ?assertEqual([3.0], maps:get(agent4, ResultMap)).  % Front 3

pareto_single_objective_test() ->
    %% Single objective should work like simple ranking
    Fitnesses = [
        {agent1, [100.0]},
        {agent2, [50.0]},
        {agent3, [75.0]}
    ],
    Result = fitness_postprocessor:pareto_dominance(Fitnesses),

    ResultMap = maps:from_list(Result),

    %% agent1 dominates all others (rank 1)
    ?assertEqual([1.0], maps:get(agent1, ResultMap)),

    %% agent3 dominates agent2 (rank 2)
    ?assertEqual([2.0], maps:get(agent3, ResultMap)),

    %% agent2 is dominated by both (rank 3)
    ?assertEqual([3.0], maps:get(agent2, ResultMap)).

pareto_equal_fitness_test() ->
    %% All agents have equal fitness - none dominate
    Fitnesses = [
        {agent1, [50.0, 50.0]},
        {agent2, [50.0, 50.0]},
        {agent3, [50.0, 50.0]}
    ],
    Result = fitness_postprocessor:pareto_dominance(Fitnesses),

    %% All should be rank 1 (non-dominated)
    ?assert(lists:all(
        fun({_Id, [Rank]}) -> Rank =:= 1.0 end,
        Result
    )).

%% ============================================================================
%% Integration Tests
%% ============================================================================

combined_postprocessing_test() ->
    setup_test(),
    try
        SpecieId = {genotype:generate_UniqueId(), specie},
        Constraint = #constraint{morphology = xor_mimic},

        %% Create test agents
        Agent1 = {genotype:generate_UniqueId(), agent},
        Agent2 = {genotype:generate_UniqueId(), agent},
        Agent3 = {genotype:generate_UniqueId(), agent},

        genotype:construct_Agent(SpecieId, Agent1, Constraint),
        genotype:construct_Agent(SpecieId, Agent2, Constraint),
        genotype:construct_Agent(SpecieId, Agent3, Constraint),

        %% Apply multiple postprocessing steps
        Fitnesses = [
            {Agent1, [100.0, 10.0]},
            {Agent2, [50.0, 50.0]},
            {Agent3, [10.0, 100.0]}
        ],

        %% Step 1: Size-proportional penalty
        AfterSizeP = fitness_postprocessor:size_proportional(Fitnesses, 0.1),

        %% Step 2: Normalize
        AfterNorm = fitness_postprocessor:normalize(AfterSizeP),

        %% Step 3: Pareto dominance
        AfterPareto = fitness_postprocessor:pareto_dominance(AfterNorm),

        %% Verify we got results
        ?assertEqual(3, length(AfterPareto)),

        %% All should be rank 1 (normalized fitness values are on Pareto front)
        ?assert(lists:all(
            fun({_Id, [Rank]}) -> Rank =:= 1.0 end,
            AfterPareto
        ))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.
