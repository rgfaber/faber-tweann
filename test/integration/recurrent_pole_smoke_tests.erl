%% @doc Smoke: recurrent evaluation no longer stalls under the memetic tuner.
%%
%% Before the FIFO-queue accumulator fix (EXP_020), a recurrent double-pole run
%% produced hundreds of 10s input-timeout stalls (a neuron fired once for two
%% cycles when the cortex over-triggered, starving its recurrent target). This
%% runs a small recurrent pb_2_without_velocity population and asserts it
%% completes quickly, which it cannot do if the stalls remain.
-module(recurrent_pole_smoke_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init(),
    ok.

teardown(_) -> ok.

recurrent_pole_no_stall_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {timeout, 120, fun recurrent_pole_no_stall/0}}.

new_agent() ->
    C = #constraint{morphology = pb_2_without_velocity,
                    connection_architecture = recurrent,
                    neural_afs = [tanh], neural_pfns = [none],
                    neural_aggr_fs = [dot_product]},
    Sp = {genotype:generate_UniqueId(), specie},
    Ag = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(Sp, Ag, C),
    Ag.

recurrent_pole_no_stall() ->
    process_flag(trap_exit, true),
    T0 = erlang:monotonic_time(millisecond),
    AgentIds = [new_agent() || _ <- lists:seq(1, 10)],
    {ok, Pid} = population_monitor:start_link(#{
        population_id => rec_pole_smoke,
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
            Ms = erlang:monotonic_time(millisecond) - T0,
            %% A stalling run takes minutes (10s per stall). A healthy 3-gen,
            %% 10-agent recurrent run finishes in seconds; 60s is a wide margin.
            ?assert(Ms < 60000),
            ?assert(maps:get(total_evaluations, Info) > 0);
        {'EXIT', Pid, Reason} ->
            ?assert(false, lists:flatten(
                io_lib:format("recurrent pole monitor died: ~p", [Reason])))
    after 115000 ->
        ?assert(false)
    end.
