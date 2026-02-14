%% @doc Unit tests for population_monitor module.
-module(population_monitor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

population_monitor_exports_test() ->
    Exports = population_monitor:module_info(exports),
    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({start_evaluation, 1}, Exports)),
    ?assert(lists:member({agent_terminated, 3}, Exports)),
    ?assert(lists:member({select_survivors, 2}, Exports)),
    ?assert(lists:member({should_terminate, 1}, Exports)).

%% ============================================================================
%% Selection Tests
%% ============================================================================

select_survivors_empty_test() ->
    Result = population_monitor:select_survivors([], 0.5),
    ?assertEqual([], Result).

select_survivors_single_agent_test() ->
    Fitnesses = [{agent1, [5.0]}],
    Result = population_monitor:select_survivors(Fitnesses, 0.5),
    ?assertEqual([agent1], Result).

select_survivors_top_performers_test() ->
    Fitnesses = [
        {agent1, [5.0]},
        {agent2, [3.0]},
        {agent3, [4.0]},
        {agent4, [1.0]},
        {agent5, [2.0]}
    ],
    SurvivalRate = 0.4,  % Keep top 40% = 2 agents

    Survivors = population_monitor:select_survivors(Fitnesses, SurvivalRate),

    ?assertEqual(2, length(Survivors)),
    %% Top 2 should be agent1 (5.0) and agent3 (4.0)
    ?assert(lists:member(agent1, Survivors)),
    ?assert(lists:member(agent3, Survivors)).

select_survivors_preserves_at_least_one_test() ->
    %% Even with 0% survival rate, should keep at least 1
    Fitnesses = [{agent1, [1.0]}, {agent2, [2.0]}],
    Result = population_monitor:select_survivors(Fitnesses, 0.0),
    ?assertEqual(1, length(Result)).

select_survivors_multi_objective_fitness_test() ->
    %% Multi-objective fitness (sums to compare)
    Fitnesses = [
        {agent1, [3.0, 2.0]},  % Sum = 5.0
        {agent2, [1.0, 1.0]},  % Sum = 2.0
        {agent3, [2.0, 1.0]}   % Sum = 3.0
    ],
    SurvivalRate = 0.5,  % Keep top 50% = 1 agent (rounds to 2)

    Survivors = population_monitor:select_survivors(Fitnesses, SurvivalRate),

    ?assertEqual(2, length(Survivors)),
    ?assert(lists:member(agent1, Survivors)).

%% ============================================================================
%% Termination Condition Tests
%% ============================================================================

should_terminate_goal_reached_test() ->
    genotype:init_db(),
    try
        %% Create test population state
        State = create_test_state(#{
            generation_count => 5,
            max_generations => 100,
            fitness_goal => [10.0],
            current_best_fitness => [15.0]  % Exceeds goal
        }),

        ?assertEqual(true, population_monitor:should_terminate(State))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

should_terminate_max_generations_test() ->
    genotype:init_db(),
    try
        State = create_test_state(#{
            generation_count => 100,
            max_generations => 100,
            fitness_goal => [10.0],
            current_best_fitness => [5.0]  % Below goal
        }),

        ?assertEqual(true, population_monitor:should_terminate(State))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

should_not_terminate_test() ->
    genotype:init_db(),
    try
        State = create_test_state(#{
            generation_count => 5,
            max_generations => 100,
            fitness_goal => [10.0],
            current_best_fitness => [5.0]  % Below goal, not at max gen
        }),

        ?assertEqual(false, population_monitor:should_terminate(State))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

should_not_terminate_no_fitness_yet_test() ->
    genotype:init_db(),
    try
        State = create_test_state(#{
            generation_count => 0,
            max_generations => 100,
            fitness_goal => [10.0],
            current_best_fitness => undefined  % No fitness yet
        }),

        ?assertEqual(false, population_monitor:should_terminate(State))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% @private Create test state record with custom fields
create_test_state(Overrides) ->
    Defaults = #{
        operation_mode => gt,
        population_id => test_pop,
        active_agent_processes => [],
        agent_ids => [],
        total_agents => 0,
        remaining_agents => 0,
        operation_tag => undefined,
        generation_count => 0,
        fitness_goal => [100.0],
        max_generations => 100,
        evaluation_limit => 1000,
        selection_algorithm => competition,
        fitness_postprocessor => none,
        evolutionary_strategy => generational,
        current_best_fitness => undefined,
        specie_size_limit => 10,
        species_map => #{},
        timestamp_started => erlang:timestamp(),
        survival_rate => 0.5,
        fitness_acc => []
    },

    Merged = maps:merge(Defaults, Overrides),

    %% Build state record using the population_state record from population_monitor
    %% Note: This is a bit hacky as we're reaching into the module's internal record
    %% A better approach would be to have population_monitor export a constructor
    {population_state,
     maps:get(operation_mode, Merged),
     maps:get(population_id, Merged),
     maps:get(active_agent_processes, Merged),
     maps:get(agent_ids, Merged),
     maps:get(total_agents, Merged),
     maps:get(remaining_agents, Merged),
     maps:get(operation_tag, Merged),
     maps:get(generation_count, Merged),
     maps:get(fitness_goal, Merged),
     maps:get(max_generations, Merged),
     maps:get(evaluation_limit, Merged),
     maps:get(selection_algorithm, Merged),
     maps:get(fitness_postprocessor, Merged),
     maps:get(evolutionary_strategy, Merged),
     maps:get(current_best_fitness, Merged),
     maps:get(specie_size_limit, Merged),
     maps:get(species_map, Merged),
     maps:get(timestamp_started, Merged),
     maps:get(survival_rate, Merged),
     maps:get(fitness_acc, Merged)
    }.
