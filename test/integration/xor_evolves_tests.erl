%% @doc Evolution SOLVES XOR through DXNN2's process-per-neuron path.
%%
%% Phase 3, complete. Insight 009 proved a single agent runs end to end; 010
%% proved the population loop drives a generation; 011 fixed the lifecycle and
%% the memetic tuner; 012 found that 60 tuning attempts (up from 15) let good
%% topologies fully tune, so XOR now solves.
%%
%% Every agent is evaluated through the full Sher path: scape, fitness channel,
%% memetic weight tuning, exoself_terminated. The scape's goal_reached signal
%% (all four cases correct) is what stops the run, and stopping BECAUSE the task
%% was solved rather than at the generation cap is the whole point.
%%
%% This test is slow (a full evolutionary run with 60 tuning attempts per
%% agent). It is the proof that the DXNN path works, so it earns its keep.
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

evolution_solves_xor_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {timeout, 300, fun evolution_solves_xor/0}}.

new_agent() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        connection_architecture = feedforward,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product],
        %% Deep, crude tuning (all neurons, 60 attempts). This is the config
        %% proven to solve XOR in 12-22 generations (insight 012). The genotype
        %% default is now DXNN2's dynamic_random + wsize_proportional, which is
        %% far shallower (~12 attempts) and does NOT solve XOR in this
        %% generation budget. That difference is the subject of insight 014, an
        %% open research question; this test proves the CAPABILITY with the
        %% config known to exercise it.
        tuning_selection_fs = [all],
        tuning_duration_f = {const, 60}
    },
    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(SpecieId, AgentId, Constraint),
    AgentId.

evolution_solves_xor() ->
    %% Trap exits: a malformed offspring may crash its evaluation, and the
    %% population must survive it (population_monitor scores it [0.0]).
    process_flag(trap_exit, true),

    PopSize = 30,
    AgentIds = [new_agent() || _ <- lists:seq(1, PopSize)],

    {ok, Pid} = population_monitor:start_link(#{
        population_id => xor_solve,
        operation_mode => test,
        agent_ids => AgentIds,
        %% Observed to solve in 12-22 generations across seeds; 80 is ample.
        max_generations => 80,
        survival_rate => 0.4,
        %% Backstop only. The real stop is the scape's goal_reached signal.
        fitness_goal => [1.0e6],
        notify_pid => self()
    }),
    %% Let the gen_server settle before starting.
    timer:sleep(50),
    population_monitor:start_evaluation(Pid),

    receive
        {population_complete, Info} ->
            Reason = maps:get(reason, Info),
            Generation = maps:get(generation, Info),
            ?assertEqual(solved, Reason,
                lists:flatten(io_lib:format(
                    "evolution did not solve XOR through the Sher path; "
                    "ended by ~p at generation ~p, best fitness ~p",
                    [Reason, Generation, maps:get(best_fitness, Info)]))),
            ?assert(Generation =< 80);
        {'EXIT', Pid, Reason} ->
            ?assert(false, lists:flatten(
                io_lib:format("population monitor died: ~p", [Reason])))
    after 290000 ->
        ?assert(false)
    end.
