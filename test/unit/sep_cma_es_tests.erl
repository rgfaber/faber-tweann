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
    ?assertEqual(30 * 20, maps:get(evaluations, R)),
    ?assertEqual(false, maps:is_key(history, R)).

%% trace => true adds a per-generation best-fitness history: one {Gen, Fit}
%% entry per generation, in order, with the fitness monotonically non-decreasing
%% (it is the running best) and ending at the final best fitness.
trace_history_test() ->
    rand:seed(exsss, {2, 2, 2}),
    R = sep_cma_es:evolve(neg_sphere([0.0, 0.0]), 2,
                          #{lambda => 20, max_generations => 25, trace => true}),
    Hist = maps:get(history, R),
    ?assertEqual(25, length(Hist)),
    ?assertEqual(lists:seq(1, 25), [G || {G, _} <- Hist]),
    Fits = [F || {_, F} <- Hist],
    ?assertEqual(Fits, lists:sort(Fits)),
    ?assertEqual(maps:get(fitness, R), lists:last(Fits)).

%% on_generation is called once per generation with the running best fitness, in
%% order; the callback can push live progress to a UI.
on_generation_callback_test() ->
    rand:seed(exsss, {3, 3, 3}),
    Self = self(),
    Fun = fun(Gen, Fit) -> Self ! {gen, Gen, Fit} end,
    _ = sep_cma_es:evolve(neg_sphere([0.0, 0.0]), 2,
                          #{lambda => 10, max_generations => 12, on_generation => Fun}),
    Gens = collect_gens([]),
    ?assertEqual(lists:seq(1, 12), [G || {G, _} <- Gens]),
    Fits = [F || {_, F} <- Gens],
    ?assertEqual(Fits, lists:sort(Fits)).

collect_gens(Acc) ->
    receive
        {gen, G, F} -> collect_gens([{G, F} | Acc])
    after 0 -> lists:reverse(Acc)
    end.
