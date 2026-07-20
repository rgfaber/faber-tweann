%% @doc Evolution runs end to end on the cart-pole scape (single pole, with
%% velocity) through DXNN2's process-per-neuron path.
%%
%% This is the wiring proof for pole balancing, the analog of the XOR insight-009
%% milestone: it does NOT assert a solve. It asserts that a constructed pole
%% agent senses the cart-pole state, drives the physics through the actuator, the
%% cortex loops sense-think-act until the scape halts (pole falls / cart leaves
%% track / goal), fitness flows back, and the population completes a few
%% generations with a finite best fitness. Whether the default tuner SOLVES pole
%% is the next measurement (and, per insight 014, may need a deeper tuner).
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(pb_evolves_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init(),
    ok.

teardown(_) -> ok.

pole_evolution_runs_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {timeout, 300, fun pole_evolution_runs/0}}.

new_agent() ->
    Constraint = #constraint{
        morphology = pb_1_with_velocity,
        connection_architecture = feedforward,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },
    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(SpecieId, AgentId, Constraint),
    AgentId.

pole_evolution_runs() ->
    process_flag(trap_exit, true),
    AgentIds = [new_agent() || _ <- lists:seq(1, 10)],
    {ok, Pid} = population_monitor:start_link(#{
        population_id => pb_wiring,
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
            BestFitness = maps:get(best_fitness, Info),
            Generation = maps:get(generation, Info),
            TotalEvals = maps:get(total_evaluations, Info),
            ?assert(is_number(best_scalar(BestFitness))),
            ?assert(best_scalar(BestFitness) > 0.0),
            ?assert(Generation >= 1),
            ?assert(TotalEvals > 0);
        {'EXIT', Pid, Reason} ->
            ?assert(false, lists:flatten(
                io_lib:format("pole population monitor died: ~p", [Reason])))
    after 290000 ->
        ?assert(false)
    end.

best_scalar([F | _]) when is_number(F) -> F;
best_scalar(F) when is_number(F) -> F;
best_scalar(_) -> 0.0.
