%% @doc Population-level evolutionary process manager.
%%
%% This gen_server manages the evolutionary process for a population
%% of neural network agents. It coordinates:
%%
%% - Agent lifecycle (spawn, evaluate, terminate)
%% - Fitness collection and aggregation
%% - Selection and reproduction
%% - Species formation and management
%% - Termination condition checking
%%
%% == Population Hierarchy ==
%% Population contains multiple species.
%% Species contains multiple agents with similar behavior.
%% Agents compete within species for selection.
%%
%% == Evolutionary Generation Loop ==
%% Each generation follows these steps:
%% 1. Spawn Phase: Launch all agent processes in parallel
%% 2. Evaluation Phase: Agents run sense-think-act cycles
%% 3. Collection Phase: Gather fitness results
%% 4. Selection Phase: Select survivors based on fitness
%% 5. Reproduction Phase: Replicate and mutate survivors
%% 6. Speciation Phase: Update species assignments
%% 7. Termination Check: Goal reached or max generations?
%%
%% == Multi-Objective Fitness ==
%% Fitness is a vector [F1, F2, ...] supporting multi-objective
%% optimization with different aggregation strategies.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(population_monitor).
-behaviour(gen_server).

-include("records.hrl").

%% API
-export([
    start_link/1,
    start_evaluation/1,
    agent_terminated/3,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% Internal exports for testing
-export([
    select_survivors/2,
    should_terminate/1
]).

%% State record with clear field names
-record(population_state, {
    operation_mode :: gt | validation | test,
    population_id :: term(),
    active_agent_processes = [] :: [{term(), pid()}],
    agent_ids = [] :: [term()],
    total_agents :: non_neg_integer(),
    remaining_agents :: non_neg_integer(),
    operation_tag :: term(),
    generation_count = 0 :: non_neg_integer(),
    fitness_goal :: [float()],
    max_generations :: pos_integer(),
    evaluation_limit :: pos_integer(),
    selection_algorithm :: atom(),
    fitness_postprocessor :: atom(),
    evolutionary_strategy :: atom(),
    current_best_fitness :: [float()] | undefined,
    specie_size_limit :: pos_integer(),
    species_map = #{} :: #{term() => [term()]},
    timestamp_started :: erlang:timestamp(),
    survival_rate :: float(),
    fitness_acc = [] :: [{term(), [float()]}]
}).

-type population_state() :: #population_state{}.

%% ============================================================================
%% API
%% ============================================================================

%% @doc Start population monitor process.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Begin evaluation phase for current generation.
-spec start_evaluation(pid()) -> ok.
start_evaluation(Pid) ->
    gen_server:cast(Pid, start_evaluation).

%% @doc Report that an agent has completed evaluation.
-spec agent_terminated(pid(), term(), [float()]) -> ok.
agent_terminated(Pid, AgentId, Fitness) ->
    gen_server:cast(Pid, {agent_terminated, AgentId, Fitness}).

%% @doc Stop the population monitor.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% @doc Initialize population monitor state.
-spec init(map()) -> {ok, population_state()}.
init(Config) ->
    State = #population_state{
        operation_mode = maps:get(operation_mode, Config, gt),
        population_id = maps:get(population_id, Config),
        agent_ids = maps:get(agent_ids, Config, []),
        total_agents = length(maps:get(agent_ids, Config, [])),
        remaining_agents = 0,
        operation_tag = maps:get(operation_tag, Config, undefined),
        generation_count = 0,
        fitness_goal = maps:get(fitness_goal, Config, [1000.0]),
        max_generations = maps:get(max_generations, Config, 100),
        evaluation_limit = maps:get(evaluation_limit, Config, 1000),
        selection_algorithm = maps:get(selection_algorithm, Config, competition),
        fitness_postprocessor = maps:get(fitness_postprocessor, Config, none),
        evolutionary_strategy = maps:get(evolutionary_strategy, Config, generational),
        specie_size_limit = maps:get(specie_size_limit, Config, 10),
        survival_rate = maps:get(survival_rate, Config, 0.5),
        timestamp_started = erlang:timestamp()
    },
    {ok, State}.

%% @doc Handle synchronous requests.
-spec handle_call(term(), {pid(), term()}, population_state()) ->
    {reply, term(), population_state()}.
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(get_generation, _From, State) ->
    {reply, State#population_state.generation_count, State};
handle_call(get_best_fitness, _From, State) ->
    {reply, State#population_state.current_best_fitness, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous messages.
-spec handle_cast(term(), population_state()) ->
    {noreply, population_state()} | {stop, normal, population_state()}.
handle_cast(start_evaluation, State) ->
    handle_start_evaluation(State);
handle_cast({agent_terminated, AgentId, Fitness}, State) ->
    handle_agent_termination(AgentId, Fitness, State);
handle_cast(next_generation, State) ->
    handle_generation_advance(State);
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages.
-spec handle_info(term(), population_state()) -> {noreply, population_state()}.
handle_info(Info, State) ->
    tweann_logger:warning("PopMon ~p received unexpected info: ~p",
                         [State#population_state.population_id, Info]),
    {noreply, State}.

%% @doc Cleanup on termination.
-spec terminate(term(), population_state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% ============================================================================
%% Internal Handler Functions
%% ============================================================================

%% @private Start evaluation phase - spawn all agents
-spec handle_start_evaluation(population_state()) ->
    {noreply, population_state()} | {stop, normal, population_state()}.
handle_start_evaluation(State) ->
    %% Check termination conditions before starting new generation
    case should_terminate(State) of
        true ->
            io:format("Evolution complete: Cohort ~p, Best Fitness: ~p~n",
                      [State#population_state.generation_count,
                       State#population_state.current_best_fitness]),
            {stop, normal, State};
        false ->
            %% Spawn all agents for evaluation
            AgentProcesses = spawn_agents(State#population_state.agent_ids, State),
            UpdatedState = State#population_state{
                active_agent_processes = AgentProcesses,
                remaining_agents = length(AgentProcesses),
                fitness_acc = [],
                generation_count = State#population_state.generation_count + 1
            },
            io:format("Cohort ~p started with ~p agents~n",
                      [UpdatedState#population_state.generation_count,
                       UpdatedState#population_state.remaining_agents]),
            {noreply, UpdatedState}
    end.

%% @private Handle agent completing evaluation
-spec handle_agent_termination(term(), [float()], population_state()) ->
    {noreply, population_state()}.
handle_agent_termination(AgentId, Fitness, State) ->
    %% Remove from active processes
    UpdatedActive = lists:keydelete(AgentId, 1, State#population_state.active_agent_processes),

    %% Accumulate fitness
    UpdatedFitnessAcc = [{AgentId, Fitness} | State#population_state.fitness_acc],

    UpdatedState = State#population_state{
        active_agent_processes = UpdatedActive,
        remaining_agents = State#population_state.remaining_agents - 1,
        fitness_acc = UpdatedFitnessAcc
    },

    %% Check if all agents completed
    case UpdatedState#population_state.remaining_agents of
        0 ->
            %% All agents done - trigger selection phase
            handle_generation_complete(UpdatedState);
        _ ->
            {noreply, UpdatedState}
    end.

%% @private Handle generation completion - selection and reproduction
-spec handle_generation_complete(population_state()) -> {noreply, population_state()}.
handle_generation_complete(State) ->
    io:format("Cohort ~p complete, selecting survivors~n",
              [State#population_state.generation_count]),

    %% Select survivors based on fitness
    Survivors = select_survivors(State#population_state.fitness_acc,
                                  State#population_state.survival_rate),

    %% Find best fitness in this generation
    AllFitnesses = [F || {_, F} <- State#population_state.fitness_acc],
    BestFitness = find_best_fitness(AllFitnesses),

    %% Update best fitness if improved
    UpdatedBestFitness = case State#population_state.current_best_fitness of
        undefined -> BestFitness;
        CurrentBest ->
            case fitness_improved(BestFitness, CurrentBest) of
                true -> BestFitness;
                false -> CurrentBest
            end
    end,

    io:format("Best fitness this cohort: ~p (overall: ~p)~n",
              [BestFitness, UpdatedBestFitness]),

    %% Clean up non-survivors to prevent memory accumulation
    NonSurvivors = [AgentId || {AgentId, _Fitness} <- State#population_state.fitness_acc,
                              not lists:member(AgentId, Survivors)],
    cleanup_agents(NonSurvivors),

    %% Reproduce survivors to fill population
    NewAgentIds = reproduce_population(Survivors, State#population_state.total_agents),

    UpdatedState = State#population_state{
        agent_ids = NewAgentIds,
        current_best_fitness = UpdatedBestFitness
    },

    %% Start next generation
    gen_server:cast(self(), start_evaluation),
    {noreply, UpdatedState}.

%% @private Clean up agents that are no longer needed
-spec cleanup_agents([term()]) -> ok.
cleanup_agents([]) ->
    ok;
cleanup_agents(AgentIds) ->
    NumCleaned = length(AgentIds),
    lists:foreach(
        fun(AgentId) ->
            genotype:delete_Agent(AgentId)
        end,
        AgentIds
    ),
    tweann_logger:debug("Cleaned up ~p non-survivor agents", [NumCleaned]),
    ok.

%% @private Advance to next generation
-spec handle_generation_advance(population_state()) -> {noreply, population_state()}.
handle_generation_advance(State) ->
    gen_server:cast(self(), start_evaluation),
    {noreply, State}.

%% ============================================================================
%% Agent Lifecycle
%% ============================================================================

%% @private Spawn all agents for evaluation
-spec spawn_agents([term()], population_state()) -> [{term(), pid()}].
spawn_agents(AgentIds, State) ->
    lists:map(
        fun(AgentId) ->
            Pid = spawn_agent(AgentId, State),
            {AgentId, Pid}
        end,
        AgentIds
    ).

%% @private Spawn a single agent process
-spec spawn_agent(term(), population_state()) -> pid().
spawn_agent(AgentId, State) ->
    MonitorPid = self(),
    OpMode = State#population_state.operation_mode,

    spawn_link(fun() ->
        %% Start exoself for this agent
        {ok, _ExoselfPid} = exoself:start(
            AgentId,
            MonitorPid,
            OpMode
        ),

        %% Wait for agent to complete (5s timeout to prevent hanging agents)
        receive
            {exoself_terminated, Fitness} ->
                population_monitor:agent_terminated(MonitorPid, AgentId, Fitness)
        after 5000 ->
            %% Timeout - use default fitness and log warning
            tweann_logger:warning("Agent ~p evaluation timeout after 5s", [AgentId]),
            population_monitor:agent_terminated(MonitorPid, AgentId, [0.0])
        end
    end).

%% ============================================================================
%% Selection and Reproduction
%% ============================================================================

%% @doc Select survivors based on fitness.
%%
%% Delegates to the selection_algorithm module for configurable selection.
%% Uses the algorithm specified in the population state.
%%
%% @param AgentFitnesses list of {AgentId, Fitness} tuples
%% @param SurvivalRate fraction of population to keep (0.0-1.0)
%% @returns list of surviving AgentIds
-spec select_survivors([{term(), [float()]}], float()) -> [term()].
select_survivors(AgentFitnesses, SurvivalRate) ->
    %% Delegate to selection_algorithm module
    %% Currently using competition selection (top-X)
    %% Could be made configurable via population state in future
    selection_algorithm:competition(AgentFitnesses, SurvivalRate).

%% @private Find best fitness in a list of fitness vectors
-spec find_best_fitness([[float()]]) -> [float()].
find_best_fitness([First | Rest]) ->
    lists:foldl(
        fun(F, BestSoFar) ->
            case lists:sum(F) >= lists:sum(BestSoFar) of
                true -> F;
                false -> BestSoFar
            end
        end,
        First,
        Rest
    ).

%% @private Check if fitness improved
-spec fitness_improved([float()], [float()]) -> boolean().
fitness_improved(NewFitness, OldFitness) ->
    (lists:sum(NewFitness) >= lists:sum(OldFitness)) andalso NewFitness =/= OldFitness.

%% @private Reproduce population from survivors
-spec reproduce_population([term()], pos_integer()) -> [term()].
reproduce_population(Survivors, TotalAgents) ->
    NumToCreate = TotalAgents - length(Survivors),

    %% Clone survivors (with mutation) to fill population
    Offspring = lists:flatmap(
        fun(_) ->
            %% Select random survivor to clone
            Parent = selection_utils:random_select(Survivors),

            %% Clone and mutate
            ClonedId = genotype:clone_Agent(Parent),
            genome_mutator:mutate(ClonedId),

            [ClonedId]
        end,
        lists:seq(1, NumToCreate)
    ),

    %% Return survivors + offspring
    Survivors ++ Offspring.

%% ============================================================================
%% Termination Conditions
%% ============================================================================

%% @doc Check if evolution should terminate.
%%
%% Termination occurs when:
%% - Fitness goal is reached
%% - Maximum generations reached
%%
%% @param State population state
%% @returns true if should terminate, false otherwise
-spec should_terminate(population_state()) -> boolean().
should_terminate(State) ->
    goal_reached(State) orelse max_generations_reached(State).

%% @private Check if fitness goal reached
-spec goal_reached(population_state()) -> boolean().
goal_reached(State) ->
    case State#population_state.current_best_fitness of
        undefined -> false;
        BestFitness ->
            GoalFitness = State#population_state.fitness_goal,
            lists:sum(BestFitness) >= lists:sum(GoalFitness)
    end.

%% @private Check if max generations reached
-spec max_generations_reached(population_state()) -> boolean().
max_generations_reached(State) ->
    State#population_state.generation_count >= State#population_state.max_generations.
