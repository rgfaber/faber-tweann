%% @doc Unit tests for selection_algorithm module.
-module(selection_algorithm_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

selection_algorithm_exports_test() ->
    Exports = selection_algorithm:module_info(exports),
    ?assert(lists:member({competition, 2}, Exports)),
    ?assert(lists:member({tournament, 3}, Exports)),
    ?assert(lists:member({steady_state, 2}, Exports)).

%% ============================================================================
%% Competition Selection Tests
%% ============================================================================

competition_empty_test() ->
    Result = selection_algorithm:competition([], 0.5),
    ?assertEqual([], Result).

competition_single_agent_test() ->
    Fitnesses = [{agent1, [5.0]}],
    Result = selection_algorithm:competition(Fitnesses, 0.5),
    ?assertEqual([agent1], Result).

competition_top_performers_test() ->
    Fitnesses = [
        {agent1, [5.0]},
        {agent2, [3.0]},
        {agent3, [4.0]},
        {agent4, [1.0]},
        {agent5, [2.0]}
    ],
    SurvivalRate = 0.4,  % Keep top 40% = 2 agents

    Survivors = selection_algorithm:competition(Fitnesses, SurvivalRate),

    ?assertEqual(2, length(Survivors)),
    %% Top 2 should be agent1 (5.0) and agent3 (4.0)
    ?assertEqual([agent1, agent3], Survivors).

competition_preserves_at_least_one_test() ->
    %% Even with 0% survival rate, should keep at least 1
    Fitnesses = [{agent1, [1.0]}, {agent2, [2.0]}],
    Result = selection_algorithm:competition(Fitnesses, 0.0),
    ?assertEqual(1, length(Result)),
    %% Should be the best agent
    ?assertEqual([agent2], Result).

competition_multi_objective_fitness_test() ->
    %% Multi-objective fitness (sums to compare)
    Fitnesses = [
        {agent1, [3.0, 2.0]},  % Sum = 5.0
        {agent2, [1.0, 1.0]},  % Sum = 2.0
        {agent3, [2.0, 1.0]}   % Sum = 3.0
    ],
    SurvivalRate = 0.5,  % Keep top 50% = 2 agents

    Survivors = selection_algorithm:competition(Fitnesses, SurvivalRate),

    ?assertEqual(2, length(Survivors)),
    %% Top 2 should be agent1 (5.0) and agent3 (3.0)
    ?assertEqual([agent1, agent3], Survivors).

competition_sorted_by_fitness_test() ->
    Fitnesses = [
        {agent1, [1.0]},
        {agent2, [5.0]},
        {agent3, [3.0]},
        {agent4, [7.0]},
        {agent5, [2.0]}
    ],
    SurvivalRate = 0.6,  % Keep top 60% = 3 agents

    Survivors = selection_algorithm:competition(Fitnesses, SurvivalRate),

    %% Should be sorted: agent4 (7.0), agent2 (5.0), agent3 (3.0)
    ?assertEqual([agent4, agent2, agent3], Survivors).

%% ============================================================================
%% Tournament Selection Tests
%% ============================================================================

tournament_empty_test() ->
    Result = selection_algorithm:tournament([], 0.5, 2),
    ?assertEqual([], Result).

tournament_single_agent_test() ->
    Fitnesses = [{agent1, [5.0]}],
    Result = selection_algorithm:tournament(Fitnesses, 0.5, 2),
    ?assertEqual([agent1], Result).

tournament_selects_survivors_test() ->
    Fitnesses = [
        {agent1, [5.0]},
        {agent2, [3.0]},
        {agent3, [7.0]},
        {agent4, [1.0]},
        {agent5, [6.0]}
    ],
    SurvivalRate = 0.4,  % 2 survivors
    TournamentSize = 2,

    Survivors = selection_algorithm:tournament(Fitnesses, SurvivalRate, TournamentSize),

    %% Should have 2 unique survivors
    ?assert(length(Survivors) >= 1),  % At least 1 (could have duplicates removed)
    ?assert(length(Survivors) =< 2),  % At most 2

    %% All survivors should be in original population
    ?assert(lists:all(fun(Id) -> lists:keymember(Id, 1, Fitnesses) end, Survivors)).

tournament_removes_duplicates_test() ->
    %% With small population, same agent can win multiple tournaments
    Fitnesses = [{agent1, [10.0]}, {agent2, [1.0]}],
    SurvivalRate = 1.0,  % 2 tournaments
    TournamentSize = 2,

    Survivors = selection_algorithm:tournament(Fitnesses, SurvivalRate, TournamentSize),

    %% Should have unique survivors only
    ?assertEqual(lists:usort(Survivors), Survivors),
    %% agent1 will likely win both tournaments due to high fitness
    ?assert(lists:member(agent1, Survivors)).

tournament_large_tournament_favors_best_test() ->
    %% Larger tournament size = more selection pressure
    Fitnesses = [
        {agent1, [10.0]},  % Best
        {agent2, [2.0]},
        {agent3, [3.0]},
        {agent4, [1.0]},
        {agent5, [4.0]}
    ],
    SurvivalRate = 0.2,  % 1 survivor
    TournamentSize = 5,  % Tournament includes all agents

    %% Run tournament multiple times to verify consistency
    Results = [selection_algorithm:tournament(Fitnesses, SurvivalRate, TournamentSize)
               || _ <- lists:seq(1, 10)],

    %% With tournament size = population size, agent1 should always win
    ?assert(lists:all(fun(Survivors) -> lists:member(agent1, Survivors) end, Results)).

%% ============================================================================
%% Steady State Selection Tests
%% ============================================================================

steady_state_empty_test() ->
    Result = selection_algorithm:steady_state([], 0.5),
    ?assertEqual([], Result).

steady_state_keeps_top_performers_test() ->
    Fitnesses = [
        {agent1, [5.0]},
        {agent2, [3.0]},
        {agent3, [7.0]},
        {agent4, [1.0]},
        {agent5, [6.0]}
    ],
    SurvivalRate = 0.6,  % Keep top 60% = 3 agents

    Survivors = selection_algorithm:steady_state(Fitnesses, SurvivalRate),

    ?assertEqual(3, length(Survivors)),
    %% Top 3: agent3 (7.0), agent5 (6.0), agent1 (5.0)
    ?assertEqual([agent3, agent5, agent1], Survivors).

steady_state_high_survival_rate_test() ->
    %% Steady state typically uses high survival rates (e.g., 0.9)
    Fitnesses = [
        {agent1, [5.0]},
        {agent2, [3.0]},
        {agent3, [7.0]},
        {agent4, [1.0]},
        {agent5, [6.0]},
        {agent6, [4.0]},
        {agent7, [2.0]},
        {agent8, [8.0]},
        {agent9, [9.0]},
        {agent10, [0.5]}
    ],
    SurvivalRate = 0.9,  % Keep 90% = 9 agents

    Survivors = selection_algorithm:steady_state(Fitnesses, SurvivalRate),

    ?assertEqual(9, length(Survivors)),
    %% Should drop only the worst (agent10 with 0.5)
    ?assertNot(lists:member(agent10, Survivors)),
    ?assert(lists:member(agent9, Survivors)),  % Best should survive
    ?assert(lists:member(agent8, Survivors)).

%% ============================================================================
%% Comparative Tests (Different Strategies)
%% ============================================================================

different_strategies_same_population_test() ->
    Fitnesses = [
        {agent1, [5.0]},
        {agent2, [3.0]},
        {agent3, [7.0]},
        {agent4, [1.0]},
        {agent5, [6.0]}
    ],
    SurvivalRate = 0.4,  % 2 survivors

    CompSurvivors = selection_algorithm:competition(Fitnesses, SurvivalRate),
    SteadySurvivors = selection_algorithm:steady_state(Fitnesses, SurvivalRate),

    %% Competition and steady state should give same results for selection
    ?assertEqual(CompSurvivors, SteadySurvivors),

    %% Both should select top 2
    ?assertEqual(2, length(CompSurvivors)),
    ?assertEqual([agent3, agent5], CompSurvivors).

%% ============================================================================
%% Edge Cases
%% ============================================================================

zero_survival_rate_test() ->
    Fitnesses = [{agent1, [5.0]}, {agent2, [3.0]}],

    CompResult = selection_algorithm:competition(Fitnesses, 0.0),
    TournResult = selection_algorithm:tournament(Fitnesses, 0.0, 2),
    SteadyResult = selection_algorithm:steady_state(Fitnesses, 0.0),

    %% All should preserve at least 1 agent
    ?assertEqual(1, length(CompResult)),
    ?assert(length(TournResult) >= 1),
    ?assertEqual(1, length(SteadyResult)).

full_survival_rate_test() ->
    Fitnesses = [{agent1, [5.0]}, {agent2, [3.0]}, {agent3, [7.0]}],

    CompResult = selection_algorithm:competition(Fitnesses, 1.0),
    SteadyResult = selection_algorithm:steady_state(Fitnesses, 1.0),

    %% Should keep all agents
    ?assertEqual(3, length(CompResult)),
    ?assertEqual(3, length(SteadyResult)),
    ?assertEqual([agent3, agent1, agent2], CompResult).  % Sorted by fitness

equal_fitness_test() ->
    %% All agents have same fitness
    Fitnesses = [
        {agent1, [5.0]},
        {agent2, [5.0]},
        {agent3, [5.0]},
        {agent4, [5.0]}
    ],
    SurvivalRate = 0.5,  % 2 survivors

    CompResult = selection_algorithm:competition(Fitnesses, SurvivalRate),

    %% Should still select 2 agents
    ?assertEqual(2, length(CompResult)),
    %% All are valid survivors (same fitness)
    ?assert(lists:all(fun(Id) -> lists:keymember(Id, 1, Fitnesses) end, CompResult)).
