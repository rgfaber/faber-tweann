%% @doc Self-adaptive (mu, lambda) Evolution Strategy over a real-valued vector.
%%
%% The first optimizer of Programme 2 (Search Strategies). Unlike the DXNN path's
%% truncation-selection GA + memetic tuner, this is a canonical ES: each
%% individual is a parameter vector plus its own mutation step size sigma, sigma
%% self-adapts log-normally, and selection is COMMA (the mu best of the lambda
%% offspring become the next parents; parents do not survive). This is the method
%% family that solves the pole-balancing benchmarks the GA stalled on (insight
%% 024; Igel 2003, Gomez CoSyNE 2008).
%%
%% The fitness function is pluggable: evolve/3 takes fun(([float()]) -> float()),
%% higher is better. That lets the ES be validated on a benchmark function
%% (mu_lambda_es_tests) independently of network evaluation; wiring it to a
%% fixed-topology network's weight vector is EXP-025.
%%
%% Reference: Beyer & Schwefel 2002, "Evolution strategies - a comprehensive
%% introduction"; Rechenberg / Schwefel.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(mu_lambda_es).

-export([evolve/3]).

-type vector() :: [float()].
-type fitness_fun() :: fun((vector()) -> float()).

%% Each individual carries its parameter vector and its self-adapting step size.
-type individual() :: {vector(), float()}.

-define(NEG_INF, -1.0e308).

%% @doc Run a self-adaptive (mu, lambda)-ES.
%%
%% Opts:
%%   mu               parents (default 10)
%%   lambda           offspring per generation, >= mu (default 70; ~7*mu is usual)
%%   max_generations  cap (default 500)
%%   fitness_goal     stop when best fitness >= this (default infinity)
%%   init_sigma       initial step size (default 1.0)
%%
%% Returns a map: best (vector), fitness, generations, evaluations, reason
%% (solved | max_generations).
-spec evolve(fitness_fun(), pos_integer(), map()) -> map().
evolve(FitnessFun, Dim, Opts) ->
    Mu = maps:get(mu, Opts, 10),
    Lambda = max(Mu, maps:get(lambda, Opts, 70)),
    MaxGen = maps:get(max_generations, Opts, 500),
    Goal = maps:get(fitness_goal, Opts, infinity),
    InitSigma = maps:get(init_sigma, Opts, 1.0),
    %% Global step-size learning rate, the standard 1/sqrt(dim).
    Tau = 1.0 / math:sqrt(Dim),
    Parents = [{random_vector(Dim), InitSigma} || _ <- lists:seq(1, Mu)],
    loop(FitnessFun, Dim, Mu, Lambda, MaxGen, Goal, Tau, Parents, 0, 0,
         {undefined, ?NEG_INF}).

%%%============================================================================
%%% Internal
%%%============================================================================

loop(_F, _Dim, _Mu, _Lambda, 0, _Goal, _Tau, _Parents, Gen, Evals, {BestW, BestF}) ->
    result(BestW, BestF, Gen, Evals, max_generations);
loop(F, Dim, Mu, Lambda, GenLeft, Goal, Tau, Parents, Gen, Evals, Best) ->
    %% Generate lambda offspring, each from a random parent, mutated.
    Offspring = [mutate(random_parent(Parents), Tau) || _ <- lists:seq(1, Lambda)],
    Evaluated = [{W, S, F(W)} || {W, S} <- Offspring],
    NewEvals = Evals + Lambda,
    %% Comma selection: the mu best offspring become the next parents.
    Sorted = lists:sort(fun({_, _, Fa}, {_, _, Fb}) -> Fa >= Fb end, Evaluated),
    {TopW, _TopS, TopF} = hd(Sorted),
    NewParents = [{W, S} || {W, S, _} <- lists:sublist(Sorted, Mu)],
    NewBest = case TopF > element(2, Best) of
                  true -> {TopW, TopF};
                  false -> Best
              end,
    case reached_goal(TopF, Goal) of
        true ->
            result(TopW, TopF, Gen + 1, NewEvals, solved);
        false ->
            loop(F, Dim, Mu, Lambda, GenLeft - 1, Goal, Tau, NewParents,
                 Gen + 1, NewEvals, NewBest)
    end.

%% Self-adaptive mutation: perturb sigma log-normally, then the vector by N(0,sigma).
-spec mutate(individual(), float()) -> individual().
mutate({W, Sigma}, Tau) ->
    NewSigma = Sigma * math:exp(Tau * rand:normal()),
    NewW = [Wi + NewSigma * rand:normal() || Wi <- W],
    {NewW, NewSigma}.

random_parent(Parents) ->
    lists:nth(rand:uniform(length(Parents)), Parents).

random_vector(Dim) ->
    [rand:normal() || _ <- lists:seq(1, Dim)].

reached_goal(_F, infinity) -> false;
reached_goal(F, Goal) -> F >= Goal.

result(W, F, Gen, Evals, Reason) ->
    #{best => W, fitness => F, generations => Gen, evaluations => Evals,
      reason => Reason}.
