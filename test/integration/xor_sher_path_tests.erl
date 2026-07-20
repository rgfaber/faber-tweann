%% @doc End-to-end evolutionary evaluation through DXNN2's process-per-neuron
%% path. This is the Phase 3 / K1 gate.
%%
%% Until now no run had ever completed through this path: there was no scape,
%% no fitness channel, and nothing sent exoself_terminated, so every agent
%% timed out with fitness [0.0]. See insights 001 and 008.
%%
%% This test exercises the whole seam that the 1400 unit tests never touched:
%% genotype -> exoself spawns scape + phenotype -> sense/think/act through the
%% scape -> fitness flows scape -> actuator -> cortex -> exoself ->
%% exoself_terminated back to the caller.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(xor_sher_path_tests).

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

xor_sher_path_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
        {"a single agent runs to completion and reports real fitness",
         {timeout, 30, fun single_agent_completes/0}},
        {"fitness reflects the scape, not the sum of outputs",
         {timeout, 30, fun fitness_is_from_scape/0}}
     ]}.

%% Build one xor_mimic agent and run it through exoself, standing in for the
%% population_monitor's spawn_agent closure: we ARE the caller and block on
%% {exoself_terminated, Fitness}.
run_one_agent() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        %% Feedforward, not the default recurrent. A recurrent neuron expects a
        %% feedback input on its first cycle that nothing has produced yet, so
        %% it waits and times out. Seeding recurrent inputs is a separate
        %% neuron-evaluation concern from the scape and fitness channel this
        %% test exercises; feedforward is the honest way to prove the seam.
        connection_architecture = feedforward,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },
    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    {ok, _Pid} = exoself:start(AgentId, undefined, test),
    receive
        {exoself_terminated, Fitness, _EvalCount} ->
            {ok, Fitness}
    after 20000 ->
        {error, timeout}
    end.

single_agent_completes() ->
    Result = run_one_agent(),
    ?assertMatch({ok, _Fitness}, Result),
    {ok, Fitness} = Result,
    %% xor_sim fitness is 1/(RMSE + epsilon): always a positive float, and
    %% not the degenerate 0.0 that the old timeout path produced.
    ?assert(is_number(Fitness)),
    ?assert(Fitness > 0.0).

%% A random untrained network cannot score perfectly, so fitness must be
%% finite and below the goal_reached ceiling. The old lists:sum(Outputs) path
%% would instead have produced an unbounded value driven by output magnitude.
%%
%% xor_sim's perfect fitness is 1/epsilon = 1.0e5. A random network's RMSE is
%% order 1, giving fitness order 1, far below that ceiling.
fitness_is_from_scape() ->
    {ok, Fitness} = run_one_agent(),
    ?assert(Fitness < 1000.0,
            lists:flatten(io_lib:format(
                "fitness ~p is implausibly high for an untrained network; "
                "it may be coming from output magnitude rather than the scape",
                [Fitness]))).
