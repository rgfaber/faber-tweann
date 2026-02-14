%% @doc Fitness postprocessing for multi-objective optimization.
%%
%% This module provides fitness transformation functions to apply
%% domain-specific knowledge or constraints to raw fitness values
%% before selection. Common use cases:
%%
%% - Penalize complex solutions (parsimony pressure)
%% - Normalize fitness across objectives
%% - Apply Pareto dominance for multi-objective optimization
%% - Age-based fitness adjustments
%%
%% == Postprocessing Strategies ==
%%
%% Size Proportional:
%% - Penalize agents with more neurons/connections
%% - Encourages simpler solutions (Occam's Razor)
%% - Helps prevent bloat in network topology
%% - Formula: AdjustedFitness = RawFitness - (Size * Penalty)
%%
%% Normalize:
%% - Scale fitness to [0, 1] range within population
%% - Prevents one objective from dominating in multi-objective case
%% - Useful when objectives have different scales
%% - Formula: Normalized = (F - Min) / (Max - Min)
%%
%% Pareto Dominance:
%% - Assign ranks based on Pareto dominance relationships
%% - Non-dominated solutions get rank 1 (best)
%% - Enables true multi-objective optimization
%% - Returns Pareto fronts as fitness ranks
%%
%% == Multi-Objective Fitness ==
%%
%% All functions work with fitness vectors [F1, F2, ...].
%% Single-objective case is just a vector with one element [F1].
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(fitness_postprocessor).

-include("records.hrl").

-export([
    size_proportional/2,
    normalize/1,
    pareto_dominance/1
]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Apply size-proportional fitness penalty (parsimony pressure).
%%
%% Penalizes agents with larger networks to encourage compact solutions.
%% This helps prevent bloat where networks grow unnecessarily complex
%% without improving performance.
%%
%% The penalty is calculated as: Size * PenaltyFactor
%% where Size is the number of neurons in the agent's network.
%%
%% Example:
%%   AgentFitnesses = [
%%     {agent1, [100.0]},
%%     {agent2, [100.0]},
%%     {agent3, [80.0]}
%%   ]
%%   PenaltyFactor = 0.1
%%
%%   Assuming agent1 has 10 neurons, agent2 has 5 neurons, agent3 has 15 neurons:
%%   Result = [
%%     {agent1, [99.0]},   % 100.0 - (10 * 0.1)
%%     {agent2, [99.5]},   % 100.0 - (5 * 0.1)
%%     {agent3, [78.5]}    % 80.0 - (15 * 0.1)
%%   ]
%%
%% @param AgentFitnesses list of {AgentId, Fitness} tuples
%% @param PenaltyFactor penalty per neuron (typically 0.01 to 0.5)
%% @returns list of {AgentId, AdjustedFitness} tuples
-spec size_proportional([{term(), [float()]}], float()) -> [{term(), [float()]}].
size_proportional(AgentFitnesses, PenaltyFactor) ->
    lists:map(
        fun({AgentId, Fitness}) ->
            %% Get agent's network size
            Size = get_agent_size(AgentId),

            %% Calculate penalty
            Penalty = Size * PenaltyFactor,

            %% Apply penalty to first fitness objective
            %% (assumes first objective is primary performance metric)
            AdjustedFitness = case Fitness of
                [F1 | Rest] -> [F1 - Penalty | Rest];
                [] -> []
            end,

            {AgentId, AdjustedFitness}
        end,
        AgentFitnesses
    ).

%% @doc Normalize fitness values to [0, 1] range.
%%
%% Scales each fitness objective independently to the range [0, 1]
%% based on the min and max values in the population. This prevents
%% objectives with large magnitudes from dominating selection.
%%
%% If all agents have the same fitness for an objective (no variance),
%% that objective is set to 0.5 for all agents.
%%
%% Example:
%%   AgentFitnesses = [
%%     {agent1, [100.0, 10.0]},
%%     {agent2, [50.0, 20.0]},
%%     {agent3, [0.0, 15.0]}
%%   ]
%%   Result = [
%%     {agent1, [1.0, 0.0]},     % Max on F1, min on F2
%%     {agent2, [0.5, 1.0]},     % Mid on F1, max on F2
%%     {agent3, [0.0, 0.5]}      % Min on F1, mid on F2
%%   ]
%%
%% @param AgentFitnesses list of {AgentId, Fitness} tuples
%% @returns list of {AgentId, NormalizedFitness} tuples
-spec normalize([{term(), [float()]}]) -> [{term(), [float()]}].
normalize([]) ->
    [];
normalize(AgentFitnesses) ->
    %% Extract all fitness vectors
    Fitnesses = [F || {_Id, F} <- AgentFitnesses],

    %% Calculate min/max for each objective
    MinMax = calculate_min_max(Fitnesses),

    %% Normalize each agent's fitness
    lists:map(
        fun({AgentId, Fitness}) ->
            NormalizedFitness = normalize_fitness(Fitness, MinMax),
            {AgentId, NormalizedFitness}
        end,
        AgentFitnesses
    ).

%% @doc Assign Pareto ranks based on dominance relationships.
%%
%% Implements fast non-dominated sorting (NSGA-II algorithm) to assign
%% Pareto ranks to agents. Lower rank is better (rank 1 = non-dominated).
%%
%% Agent A dominates Agent B if:
%% - A is no worse than B in all objectives, AND
%% - A is strictly better than B in at least one objective
%%
%% The result is a single-objective fitness [Rank] where Rank indicates
%% which Pareto front the agent belongs to.
%%
%% Example:
%%   AgentFitnesses = [
%%     {agent1, [100.0, 10.0]},  % Good on F1, poor on F2
%%     {agent2, [50.0, 50.0]},   % Balanced
%%     {agent3, [10.0, 100.0]},  % Poor on F1, good on F2
%%     {agent4, [30.0, 30.0]}    % Dominated by agent2
%%   ]
%%   Result = [
%%     {agent1, [1.0]},  % Front 1 (non-dominated)
%%     {agent2, [1.0]},  % Front 1 (non-dominated)
%%     {agent3, [1.0]},  % Front 1 (non-dominated)
%%     {agent4, [2.0]}   % Front 2 (dominated by agent2)
%%   ]
%%
%% @param AgentFitnesses list of {AgentId, Fitness} tuples
%% @returns list of {AgentId, [Rank]} tuples where lower rank is better
-spec pareto_dominance([{term(), [float()]}]) -> [{term(), [float()]}].
pareto_dominance([]) ->
    [];
pareto_dominance(AgentFitnesses) ->
    %% Build dominance relationships
    Dominates = build_dominance_map(AgentFitnesses),

    %% Assign ranks (fronts)
    Ranks = assign_pareto_ranks(AgentFitnesses, Dominates),

    %% Convert ranks to fitness format
    [{AgentId, [float(Rank)]} || {AgentId, Rank} <- Ranks].

%% ============================================================================
%% Internal Functions - Size Proportional
%% ============================================================================

%% @private Get number of neurons in agent's network.
-spec get_agent_size(term()) -> non_neg_integer().
get_agent_size(AgentId) ->
    case genotype:read({agent, AgentId}) of
        undefined ->
            0;  % Agent not found, assume size 0
        Agent ->
            CortexId = Agent#agent.cx_id,
            case genotype:read({cortex, CortexId}) of
                undefined -> 0;
                Cortex -> length(Cortex#cortex.neuron_ids)
            end
    end.

%% ============================================================================
%% Internal Functions - Normalize
%% ============================================================================

%% @private Calculate min and max for each fitness objective.
-spec calculate_min_max([[float()]]) -> [{float(), float()}].
calculate_min_max([]) ->
    [];
calculate_min_max([FirstFitness | _] = Fitnesses) ->
    %% Initialize with first fitness
    NumObjectives = length(FirstFitness),

    %% For each objective index
    lists:map(
        fun(ObjIdx) ->
            %% Extract values for this objective
            Values = [lists:nth(ObjIdx, F) || F <- Fitnesses, length(F) >= ObjIdx],

            %% Find min and max
            case Values of
                [] -> {0.0, 1.0};  % Default range
                _ ->
                    Min = lists:min(Values),
                    Max = lists:max(Values),
                    {Min, Max}
            end
        end,
        lists:seq(1, NumObjectives)
    ).

%% @private Normalize a single fitness vector.
-spec normalize_fitness([float()], [{float(), float()}]) -> [float()].
normalize_fitness(Fitness, MinMax) ->
    lists:zipwith(
        fun(Value, {Min, Max}) ->
            case Max - Min of
                +0.0 -> 0.5;  % No variance, use middle value
                Range -> (Value - Min) / Range
            end
        end,
        Fitness,
        MinMax
    ).

%% ============================================================================
%% Internal Functions - Pareto Dominance
%% ============================================================================

%% @private Build map of which agents dominate which others.
-spec build_dominance_map([{term(), [float()]}]) ->
    #{term() => {[term()], non_neg_integer()}}.
build_dominance_map(AgentFitnesses) ->
    %% For each agent, track:
    %% - List of agents it dominates
    %% - Count of agents that dominate it
    lists:foldl(
        fun({AgentId, Fitness}, Map) ->
            {Dominated, DominatedBy} = find_dominance_relationships(
                AgentId, Fitness, AgentFitnesses
            ),
            maps:put(AgentId, {Dominated, DominatedBy}, Map)
        end,
        #{},
        AgentFitnesses
    ).

%% @private Find which agents this agent dominates and is dominated by.
-spec find_dominance_relationships(term(), [float()], [{term(), [float()]}]) ->
    {[term()], non_neg_integer()}.
find_dominance_relationships(AgentId, Fitness, AllAgents) ->
    lists:foldl(
        fun({OtherId, OtherFitness}, {Dominated, DominatedBy}) ->
            if
                OtherId =:= AgentId ->
                    {Dominated, DominatedBy};
                true ->
                    case dominates(Fitness, OtherFitness) of
                        true ->
                            %% This agent dominates the other
                            {[OtherId | Dominated], DominatedBy};
                        false ->
                            case dominates(OtherFitness, Fitness) of
                                true ->
                                    %% Other agent dominates this one
                                    {Dominated, DominatedBy + 1};
                                false ->
                                    %% No dominance relationship
                                    {Dominated, DominatedBy}
                            end
                    end
            end
        end,
        {[], 0},
        AllAgents
    ).

%% @private Check if fitness A dominates fitness B.
%%
%% A dominates B if:
%% - A is greater than or equal to B for all objectives (no worse)
%% - A is strictly greater than B for at least one objective (strictly better)
-spec dominates([float()], [float()]) -> boolean().
dominates(FitnessA, FitnessB) ->
    %% Check both conditions
    NoWorse = lists:all(
        fun({A, B}) -> A >= B end,
        lists:zip(FitnessA, FitnessB)
    ),

    StrictlyBetter = lists:any(
        fun({A, B}) -> A > B end,
        lists:zip(FitnessA, FitnessB)
    ),

    NoWorse andalso StrictlyBetter.

%% @private Assign Pareto ranks using fast non-dominated sorting.
-spec assign_pareto_ranks([{term(), [float()]}],
                          #{term() => {[term()], non_neg_integer()}}) ->
    [{term(), pos_integer()}].
assign_pareto_ranks(AgentFitnesses, DominanceMap) ->
    %% Find first front (non-dominated agents)
    FirstFront = [Id || {Id, {_Dominated, 0}} <- maps:to_list(DominanceMap)],

    %% Get all agent IDs for verification
    AllAgentIds = [Id || {Id, _F} <- AgentFitnesses],

    %% Assign ranks recursively, tracking processed agents
    Result = assign_ranks_recursive(FirstFront, DominanceMap, 1, [], sets:new()),

    %% Ensure all agents were ranked (safety check)
    RankedIds = [Id || {Id, _Rank} <- Result],
    case length(RankedIds) =:= length(AllAgentIds) of
        true -> Result;
        false ->
            %% Some agents weren't ranked - this shouldn't happen with correct algorithm
            %% But assign remaining agents to final front as fallback
            Unranked = AllAgentIds -- RankedIds,
            MaxRank = case Result of
                [] -> 1;
                _ -> lists:max([R || {_Id, R} <- Result])
            end,
            Result ++ [{Id, MaxRank + 1} || Id <- Unranked]
    end.

%% @private Recursively assign ranks to Pareto fronts.
-spec assign_ranks_recursive([term()],
                             #{term() => {[term()], non_neg_integer()}},
                             pos_integer(),
                             [{term(), pos_integer()}],
                             sets:set(term())) ->
    [{term(), pos_integer()}].
assign_ranks_recursive([], _DominanceMap, _Rank, Acc, _Processed) ->
    lists:reverse(Acc);
assign_ranks_recursive(CurrentFront, DominanceMap, Rank, Acc, Processed) ->
    %% Assign current rank to all agents in this front
    RankedAgents = [{Id, Rank} || Id <- CurrentFront],

    %% Mark current front as processed
    NewProcessed = lists:foldl(fun sets:add_element/2, Processed, CurrentFront),

    %% Find next front (excluding already processed agents)
    NextFront = find_next_front(CurrentFront, DominanceMap, NewProcessed),

    %% Continue with next front
    assign_ranks_recursive(NextFront, DominanceMap, Rank + 1,
                          RankedAgents ++ Acc, NewProcessed).

%% @private Find the next Pareto front.
-spec find_next_front([term()],
                      #{term() => {[term()], non_neg_integer()}},
                      sets:set(term())) ->
    [term()].
find_next_front(CurrentFront, DominanceMap, Processed) ->
    %% For each agent in current front, decrement dominated count
    %% for agents it dominates
    DecrementedMap = lists:foldl(
        fun(AgentId, Map) ->
            case maps:get(AgentId, Map, {[], 0}) of
                {Dominated, _Count} ->
                    %% Decrement count for each dominated agent
                    lists:foldl(
                        fun(DomId, M) ->
                            case maps:get(DomId, M, {[], 0}) of
                                {DomList, DomCount} ->
                                    maps:put(DomId, {DomList, DomCount - 1}, M)
                            end
                        end,
                        Map,
                        Dominated
                    )
            end
        end,
        DominanceMap,
        CurrentFront
    ),

    %% Next front = agents with count 0 (excluding already processed)
    [Id || {Id, {_Dominated, 0}} <- maps:to_list(DecrementedMap),
           not sets:is_element(Id, Processed)].
