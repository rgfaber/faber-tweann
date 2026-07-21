%% @doc EXP_023: budget-to-solution on double-pole-without-velocity.
%%
%% Insight 018 found memory was a TAX on this task at 60 generations (nothing
%% solved). 019 showed memory PAYS on a task solvable within budget. This tests
%% the hypothesis directly: at a HIGH budget (pop 30, 500 generations), does a
%% solve become reachable, and does memory pay when it does? The genuine
%% non-Markov benchmark (double pole, no velocity) needs memory to solve.
%%
%% Three arms: feedforward (delay-line-capable baseline), cfc (neuron memory),
%% recurrent (wiring memory). Reports {Reason, Gen, TotalEvals, BestFitness,
%% WallMs} per run, streamed. Runs on msi00 (8 cores) under tmux. --module only.
-module(exp023_tests).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

-define(POP, 30).
-define(MAXGEN, 500).
-define(RUNS, 10).

experiment_test_() -> {setup, fun s/0, fun(_) -> ok end, {timeout, 40000, fun go/0}}.

s() ->
    {ok, _} = application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init(),
    ok.

mk(Arch, MakeCfc) ->
    C = #constraint{morphology = pb_2_without_velocity, connection_architecture = Arch,
                    neural_afs = [tanh], neural_pfns = [none], neural_aggr_fs = [dot_product]},
    Sp = {genotype:generate_UniqueId(), specie},
    Ag = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(Sp, Ag, C),
    MakeCfc andalso make_all_cfc(Ag),
    Ag.

make_all_cfc(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    [genotype:write((genotype:dirty_read({neuron, NId}))#neuron{
        neuron_type = cfc, time_constant = 1.0, state_bound = 1.0})
     || NId <- Cortex#cortex.neuron_ids],
    ok.

run1(Arch, MakeCfc) ->
    process_flag(trap_exit, true),
    T0 = erlang:monotonic_time(millisecond),
    A = [mk(Arch, MakeCfc) || _ <- lists:seq(1, ?POP)],
    {ok, P} = population_monitor:start_link(#{population_id => exp023,
        operation_mode => test, agent_ids => A, max_generations => ?MAXGEN,
        survival_rate => 0.4, fitness_goal => [1.0e12], notify_pid => self()}),
    receive after 50 -> ok end,
    population_monitor:start_evaluation(P),
    Result = receive
        {population_complete, I} ->
            Ms = erlang:monotonic_time(millisecond) - T0,
            {maps:get(reason, I), maps:get(generation, I),
             maps:get(total_evaluations, I), best(maps:get(best_fitness, I)), Ms};
        {'EXIT', P, Rs} -> {crashed, Rs, 0, 0.0, 0}
    after 3600000 -> {timeout, 0, 0, 0.0, 3600000}
    end,
    _ = unlink(P), _ = exit(P, kill),
    Result.

best([F | _]) when is_number(F) -> F;
best(F) when is_number(F) -> F;
best(_) -> 0.0.

arm(F, Name, Arch, MakeCfc) ->
    io:format(F, "~n=== ~s (pb_2_without_velocity, pop ~p, max_gen ~p) ===~n",
              [Name, ?POP, ?MAXGEN]),
    R = [begin
             X = run1(Arch, MakeCfc),
             io:format(F, "  ~p~n", [X]),
             file:sync(F),
             X
         end || _ <- lists:seq(1, ?RUNS)],
    Solved = [G || {solved, G, _E, _Fit, _Ms} <- R],
    Fits = [Fit || {_R, _G, _E, Fit, _Ms} <- R, is_number(Fit)],
    io:format(F, "  => ~s: solved=~p/~p (gens ~p), best_fitness=~p~n",
              [Name, length(Solved), ?RUNS, Solved, Fits]),
    file:sync(F).

go() ->
    {ok, F} = file:open("/tmp/exp023_results.txt", [write]),
    arm(F, "A: feedforward (delay-line-capable)", feedforward, false),
    arm(F, "B: cfc (neuron memory)", feedforward, true),
    arm(F, "C: recurrent (wiring memory)", recurrent, false),
    io:format(F, "DONE~n", []),
    file:close(F),
    ?assert(true).
