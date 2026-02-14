%% @doc Selection algorithms for evolutionary processes.
%%
%% This module provides various selection strategies for choosing
%% surviving agents from a population based on fitness. Each strategy
%% implements a different approach to balancing exploration vs exploitation:
%%
%% - Competition: Top performers survive (pure exploitation)
%% - Tournament: Random tournaments with best winner (balanced)
%% - Steady State: Incremental replacement (conservative)
%%
%% == Selection Strategies ==
%%
%% Competition Selection:
%% - Sort population by fitness (descending)
%% - Select top N agents based on survival rate
%% - Guarantees best performers survive
%% - Fast convergence but risks premature convergence
%% - Best for well-understood fitness landscapes
%%
%% Tournament Selection:
%% - Run K tournaments of size TournamentSize
%% - Each tournament selects best from random subset
%% - Provides diversity through randomness
%% - Tournament size controls selection pressure
%% - Good balance of exploration and exploitation
%%
%% Steady State Selection:
%% - Replace worst N agents with offspring
%% - Keep majority of population unchanged
%% - Very conservative, slow convergence
%% - Good for maintaining diversity
%% - Useful when fitness landscape is deceptive
%%
%% == Fitness Comparison ==
%%
%% All strategies use multi-objective fitness vectors [F1, F2, ...].
%% Comparison is by sum of components (weighted sum aggregation).
%% For more sophisticated multi-objective optimization, use
%% fitness_postprocessor module to apply Pareto dominance first.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(selection_algorithm).

-export([
    competition/2,
    tournament/3,
    steady_state/2
]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Competition selection - select top performers.
%%
%% Sorts agents by fitness (descending) and selects the top N performers
%% based on survival rate. This is the most aggressive selection strategy,
%% providing strong selection pressure toward the best solutions.
%%
%% Example:
%%   AgentFitnesses = [{agent1, [5.0]}, {agent2, [3.0]}, {agent3, [7.0]}]
%%   SurvivalRate = 0.5
%%   Result = competition(AgentFitnesses, SurvivalRate)
%%   Result = [agent3, agent1]  % Top 50% by fitness
%%
%% @param AgentFitnesses list of {AgentId, Fitness} tuples where Fitness
%%        is a vector [F1, F2, ...] of floats
%% @param SurvivalRate fraction of population to keep (0.0-1.0)
%% @returns list of surviving AgentIds sorted by fitness (best first)
-spec competition([{term(), [float()]}], float()) -> [term()].
competition([], _SurvivalRate) ->
    [];
competition(AgentFitnesses, SurvivalRate) ->
    %% Sort by fitness (descending - best first)
    Sorted = lists:sort(
        fun({_, F1}, {_, F2}) -> compare_fitness(F1, F2) end,
        AgentFitnesses
    ),

    %% Calculate number of survivors (at least 1)
    NumSurvivors = max(1, round(length(AgentFitnesses) * SurvivalRate)),

    %% Take top N agents
    Survivors = lists:sublist(Sorted, NumSurvivors),
    [AgentId || {AgentId, _Fitness} <- Survivors].

%% @doc Tournament selection - run tournaments to select survivors.
%%
%% Runs K tournaments where each tournament randomly selects TournamentSize
%% agents and picks the best. This provides a good balance between selection
%% pressure (controlled by tournament size) and diversity (through randomness).
%%
%% Larger tournament sizes increase selection pressure (more elitist).
%% Smaller tournament sizes increase diversity (more exploratory).
%%
%% Example:
%%   AgentFitnesses = [{agent1, [5.0]}, {agent2, [3.0]}, {agent3, [7.0]},
%%                     {agent4, [4.0]}, {agent5, [6.0]}]
%%   SurvivalRate = 0.4
%%   TournamentSize = 2
%%   Result = tournament(AgentFitnesses, SurvivalRate, TournamentSize)
%%   Result might be [agent3, agent5] (winners of 2 tournaments)
%%
%% @param AgentFitnesses list of {AgentId, Fitness} tuples
%% @param SurvivalRate fraction of population to keep (0.0-1.0)
%% @param TournamentSize number of agents per tournament (typically 2-5)
%% @returns list of surviving AgentIds (tournament winners)
-spec tournament([{term(), [float()]}], float(), pos_integer()) -> [term()].
tournament([], _SurvivalRate, _TournamentSize) ->
    [];
tournament(AgentFitnesses, SurvivalRate, TournamentSize) ->
    %% Calculate number of tournaments to run
    NumSurvivors = max(1, round(length(AgentFitnesses) * SurvivalRate)),

    %% Run tournaments
    Survivors = lists:map(
        fun(_) ->
            %% Select random contestants
            Contestants = select_random_contestants(AgentFitnesses, TournamentSize),

            %% Find winner (best fitness)
            {WinnerId, _WinnerFitness} = find_best_agent(Contestants),
            WinnerId
        end,
        lists:seq(1, NumSurvivors)
    ),

    %% Remove duplicates (same agent can win multiple tournaments)
    lists:usort(Survivors).

%% @doc Steady state selection - replace worst performers with new agents.
%%
%% This is the most conservative selection strategy. It keeps the majority
%% of the population unchanged and only replaces the worst performers.
%% Good for maintaining diversity and avoiding premature convergence.
%%
%% The survival rate determines what fraction of the worst agents to replace.
%% For example, SurvivalRate=0.8 means keep top 80% and replace bottom 20%.
%%
%% Example:
%%   AgentFitnesses = [{agent1, [5.0]}, {agent2, [3.0]}, {agent3, [7.0]},
%%                     {agent4, [1.0]}, {agent5, [6.0]}]
%%   SurvivalRate = 0.6
%%   Result = steady_state(AgentFitnesses, SurvivalRate)
%%   Result = [agent3, agent5, agent1]  % Top 60% survive
%%
%% @param AgentFitnesses list of {AgentId, Fitness} tuples
%% @param SurvivalRate fraction of population to keep (0.0-1.0)
%% @returns list of surviving AgentIds
-spec steady_state([{term(), [float()]}], float()) -> [term()].
steady_state(AgentFitnesses, SurvivalRate) ->
    %% Steady state is essentially the same as competition selection
    %% The difference is in how offspring are generated (not in this module)
    %% For selection purposes, we keep the top N performers
    competition(AgentFitnesses, SurvivalRate).

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% @private Compare two fitness vectors (higher is better).
-spec compare_fitness([float()], [float()]) -> boolean().
compare_fitness(F1, F2) ->
    sum_fitness(F1) >= sum_fitness(F2).

%% @private Sum fitness vector components.
-spec sum_fitness([float()]) -> float().
sum_fitness(Fitness) ->
    lists:sum(Fitness).

%% @private Select N random agents from population.
-spec select_random_contestants([{term(), [float()]}], pos_integer()) ->
    [{term(), [float()]}].
select_random_contestants(AgentFitnesses, N) ->
    %% Don't select more than available
    ActualN = min(N, length(AgentFitnesses)),

    %% Shuffle and take first N
    Shuffled = shuffle(AgentFitnesses),
    lists:sublist(Shuffled, ActualN).

%% @private Shuffle a list using Fisher-Yates algorithm.
-spec shuffle([T]) -> [T].
shuffle(List) ->
    %% Convert to array for efficient random access
    Array = array:from_list(List),
    Size = array:size(Array),

    %% Shuffle array indices
    ShuffledArray = shuffle_array(Array, Size - 1),

    %% Convert back to list
    array:to_list(ShuffledArray).

%% @private Shuffle array in place (Fisher-Yates).
-spec shuffle_array(array:array(T), integer()) -> array:array(T).
shuffle_array(Array, -1) ->
    Array;
shuffle_array(Array, I) ->
    %% Select random index from 0 to I
    J = rand:uniform(I + 1) - 1,

    %% Swap elements at I and J
    ElemI = array:get(I, Array),
    ElemJ = array:get(J, Array),
    Array1 = array:set(I, ElemJ, Array),
    Array2 = array:set(J, ElemI, Array1),

    %% Continue with next index
    shuffle_array(Array2, I - 1).

%% @private Find agent with best fitness in a list.
-spec find_best_agent([{term(), [float()]}]) -> {term(), [float()]}.
find_best_agent([First | Rest]) ->
    lists:foldl(
        fun({_Id, Fitness} = Agent, {_BestId, BestFitness} = BestSoFar) ->
            case compare_fitness(Fitness, BestFitness) of
                true -> Agent;
                false -> BestSoFar
            end
        end,
        First,
        Rest
    ).
