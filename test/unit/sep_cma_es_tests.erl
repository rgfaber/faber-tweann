%% @doc Unit tests for the sep-CMA-ES engine, on benchmark functions (no
%% network). Proves the optimizer converges — and, on an ill-conditioned
%% ellipsoid, that the PER-COORDINATE variance adaptation actually works (a
%% single-sigma ES needs far more evaluations there) — before it is wired to
%% network evaluation (EXP-028).
-module(sep_cma_es_tests).
-include_lib("eunit/include/eunit.hrl").

%% Negated squared distance to a target: optimum at the target, fitness 0.
neg_sphere(Target) ->
    fun(W) ->
        -lists:sum([(A - B) * (A - B) || {A, B} <- lists:zip(W, Target)])
    end.

%% Axis-aligned ill-conditioned quadratic (the classic ellipsoid, condition
%% 1e6): coordinate j is weighted 10^(6*(j-1)/(n-1)). Diagonal covariance is
%% exactly the structure that solves this; an isotropic single-sigma ES crawls.
neg_ellipsoid(N) ->
    fun(W) ->
        Indexed = lists:zip(lists:seq(0, N - 1), W),
        -lists:sum([math:pow(10.0, 6.0 * I / (N - 1)) * X * X
                    || {I, X} <- Indexed])
    end.

converges_on_sphere_test() ->
    rand:seed(exsss, {11, 22, 33}),
    Target = [3.0, -2.0, 1.5],
    R = sep_cma_es:evolve(neg_sphere(Target), 3,
                          #{max_generations => 400, fitness_goal => -1.0e-8,
                            init_sigma => 1.0}),
    ?assertEqual(solved, maps:get(reason, R)),
    Best = maps:get(best, R),
    [?assert(abs(A - B) < 0.001) || {A, B} <- lists:zip(Best, Target)].

%% The decisive correctness test: sep-CMA-ES must crack a condition-1e6
%% ellipsoid to near-zero. This only works if the per-coordinate variances
%% adapt (they must shrink ~1e-3 on the stiff axis and stay large on the soft
%% one). A bug in the covariance update fails here even though the sphere passes.
solves_ill_conditioned_ellipsoid_test() ->
    rand:seed(exsss, {5, 5, 5}),
    N = 10,
    R = sep_cma_es:evolve(neg_ellipsoid(N), N,
                          #{max_generations => 4000, fitness_goal => -1.0e-8,
                            init_sigma => 1.0}),
    ?assertEqual(solved, maps:get(reason, R)),
    ?assert(maps:get(fitness, R) >= -1.0e-8).

result_contract_test() ->
    rand:seed(exsss, {1, 1, 1}),
    R = sep_cma_es:evolve(neg_sphere([0.0, 0.0]), 2,
                          #{lambda => 20, max_generations => 30}),
    ?assertMatch(#{best := _, fitness := _, generations := _,
                   evaluations := _, reason := _}, R),
    ?assertEqual(30, maps:get(generations, R)),
    ?assertEqual(30 * 20, maps:get(evaluations, R)).
