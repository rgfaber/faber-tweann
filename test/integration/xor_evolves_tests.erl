%% @doc The population_monitor drives Sher-path evaluation across a generation.
%%
%% Phase 3. Insight 009 proved a single agent runs end to end. This proves the
%% population_monitor's generational machinery drives a whole population of
%% agents through that same path, collecting real fitness from each.
%%
%% What it does NOT yet prove: that evolution SOLVES XOR across many
%% generations. Multi-generation evolution currently crashes in the genotype
%% lifecycle (repeated clone/mutate/delete produces topologies the
%% process-per-neuron evaluator cannot link or evaluate). That is a separate
%% subsystem from the scape and fitness channel, recorded in insight 010 and on
%% the roadmap. This test is deliberately scoped to one generation so it proves
%% what works without asserting what does not.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(xor_evolves_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init(),
    ok.

teardown(_) ->
    ok.

population_generation_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {timeout, 120, fun one_generation_evaluates_through_sher_path/0}}.

new_agent() ->
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
    AgentId.

one_generation_evaluates_through_sher_path() ->
    PopSize = 20,
    AgentIds = [new_agent() || _ <- lists:seq(1, PopSize)],

    {ok, Pid} = population_monitor:start_link(#{
        population_id => xor_gen,
        operation_mode => test,
        agent_ids => AgentIds,
        %% One generation: evaluate the whole fresh population, then stop
        %% before reproduction. Multi-generation reproduction is blocked on
        %% the lifecycle bug in insight 010.
        max_generations => 1,
        survival_rate => 0.5,
        fitness_goal => [1.0e6],
        notify_pid => self()
    }),

    population_monitor:start_evaluation(Pid),

    receive
        {population_complete, Info} ->
            ?assertEqual(1, maps:get(generation, Info)),
            BestFitness = maps:get(best_fitness, Info),
            %% Real fitness collected through the Sher path, not the [0.0] that
            %% the old timeout produced when nothing sent exoself_terminated.
            ?assertMatch([F] when is_number(F) andalso F > 0.0, BestFitness)
    after 110000 ->
        ?assert(false)
    end.
