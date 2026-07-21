%% @doc Unit tests for the (mu, lambda)-ES engine, on benchmark functions (no
%% network). Proves the optimizer converges to a known optimum before it is
%% wired to network evaluation (EXP-025).
-module(mu_lambda_es_tests).
-include_lib("eunit/include/eunit.hrl").

%% Negated squared distance to a target: the optimum is the target, fitness 0.
neg_sphere(Target) ->
    fun(W) ->
        -lists:sum([(A - B) * (A - B) || {A, B} <- lists:zip(W, Target)])
    end.

%% The ES finds a small 3-D target to near-zero error.
converges_to_target_test() ->
    rand:seed(exsss, {11, 22, 33}),
    Target = [3.0, -2.0, 1.5],
    R = mu_lambda_es:evolve(neg_sphere(Target), 3,
                            #{mu => 10, lambda => 70, max_generations => 300,
                              fitness_goal => -0.0001}),
    ?assertEqual(solved, maps:get(reason, R)),
    ?assert(maps:get(fitness, R) >= -0.0001),
    %% best vector is close to the target
    Best = maps:get(best, R),
    [?assert(abs(A - B) < 0.02) || {A, B} <- lists:zip(Best, Target)].

%% It scales to higher dimension (10-D) and still drives fitness up sharply from
%% a random start.
improves_in_higher_dim_test() ->
    rand:seed(exsss, {7, 8, 9}),
    Target = [float(N) || N <- lists:seq(1, 10)],
    F = neg_sphere(Target),
    R = mu_lambda_es:evolve(F, 10,
                            #{mu => 15, lambda => 100, max_generations => 400}),
    %% A random 10-D vector scores far below zero; the ES must get close to 0.
    ?assert(maps:get(fitness, R) > -1.0),
    ?assert(maps:get(evaluations, R) > 0).

%% Comma selection and the result contract behave.
result_contract_test() ->
    rand:seed(exsss, {1, 1, 1}),
    R = mu_lambda_es:evolve(neg_sphere([0.0, 0.0]), 2,
                            #{mu => 5, lambda => 35, max_generations => 50}),
    ?assertMatch(#{best := _, fitness := _, generations := _,
                   evaluations := _, reason := _}, R),
    ?assertEqual(50, maps:get(generations, R)),
    ?assertEqual(50 * 35, maps:get(evaluations, R)).
