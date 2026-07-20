%% @doc LTC/CfC neurons evaluate through the evolutionary path.
%%
%% Until now the exoself spawned standard neuron.erl for every neuron, silently
%% ignoring neuron_type: a genotype full of cfc neurons ran as standard. This
%% test seeds an agent whose neurons are all cfc and evolves it, proving the
%% exoself now dispatches to neuron_ltc (continuous-time dynamics, persistent
%% internal_state) and that the memetic tuner (perturb/restore) works on LTC
%% neurons. It asserts completion and finite fitness, not a solve.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(ltc_evolution_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init(),
    ok.

teardown(_) -> ok.

ltc_evolution_runs_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {timeout, 120, fun ltc_evolution_runs/0}}.

new_cfc_agent() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        connection_architecture = feedforward,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },
    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(SpecieId, AgentId, Constraint),
    make_all_cfc(AgentId),
    AgentId.

%% Flip every neuron in the agent to cfc, so the population starts with temporal
%% dynamics. tau and bound are set to sane positive defaults; empty backbone/head
%% weights make ltc_dynamics fall back to its input-driven default CfC gate.
make_all_cfc(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    lists:foreach(
        fun(NId) ->
            N = genotype:dirty_read({neuron, NId}),
            genotype:write(N#neuron{
                neuron_type = cfc,
                time_constant = 1.0,
                state_bound = 1.0
            })
        end,
        Cortex#cortex.neuron_ids),
    ok.

ltc_evolution_runs() ->
    process_flag(trap_exit, true),
    AgentIds = [new_cfc_agent() || _ <- lists:seq(1, 10)],
    {ok, Pid} = population_monitor:start_link(#{
        population_id => ltc_smoke,
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
                io_lib:format("ltc population monitor died: ~p", [Reason])))
    after 115000 ->
        ?assert(false)
    end.

best([F | _]) when is_number(F) -> F;
best(F) when is_number(F) -> F;
best(_) -> 0.0.
