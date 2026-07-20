%% @doc Recurrent-mode evolution runs end to end through the construction path.
%%
%% With connection_architecture = recurrent, topological mutations may add
%% feedback edges (same/lower-layer targets). This smoke test proves the
%% constructor/exoself recurrent-output partition and the first-cycle seeding
%% hold up in a real population: the run completes and produces finite fitness,
%% rather than deadlocking a phenotype. The seeding mechanism itself is proven
%% deterministically in recurrent_neuron_tests.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(recurrent_evolution_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init(),
    ok.

teardown(_) -> ok.

recurrent_evolution_runs_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {timeout, 120, fun recurrent_evolution_runs/0}}.

new_agent() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        connection_architecture = recurrent,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },
    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(SpecieId, AgentId, Constraint),
    AgentId.

recurrent_evolution_runs() ->
    process_flag(trap_exit, true),
    AgentIds = [new_agent() || _ <- lists:seq(1, 10)],
    {ok, Pid} = population_monitor:start_link(#{
        population_id => recurrent_smoke,
        operation_mode => test,
        agent_ids => AgentIds,
        max_generations => 3,
        survival_rate => 0.4,
        fitness_goal => [1.0e12],
        notify_pid => self()
    }),
    timer:sleep(50),
    population_monitor:start_evaluation(Pid),
    receive
        {population_complete, Info} ->
            ?assert(is_number(best(maps:get(best_fitness, Info)))),
            ?assert(best(maps:get(best_fitness, Info)) > 0.0),
            ?assert(maps:get(total_evaluations, Info) > 0);
        {'EXIT', Pid, Reason} ->
            ?assert(false, lists:flatten(
                io_lib:format("recurrent population monitor died: ~p", [Reason])))
    after 115000 ->
        ?assert(false)
    end.

best([F | _]) when is_number(F) -> F;
best(F) when is_number(F) -> F;
best(_) -> 0.0.
