%% @doc Separable (diagonal) CMA-ES over a real-valued vector.
%%
%% The second optimizer of Programme 2 (Search Strategies). Where mu_lambda_es
%% carries a single global step size sigma (isotropic search), sep-CMA-ES adapts
%% a PER-COORDINATE variance (a diagonal covariance) plus a cumulative
%% step-size path, so it can scale each dimension independently. It is the
%% diagonal restriction of CMA-ES (Ros and Hansen 2008, "A Simple Modification
%% in CMA-ES Achieving Linear Time and Space Complexity"): O(N) per generation,
%% no eigendecomposition. It captures the anisotropic-scaling half of full
%% CMA-ES's power, but NOT correlations/rotations (the covariance stays
%% diagonal). If a problem needs correlated steps, full CMA-ES or CoSyNE is the
%% next rung.
%%
%% Same pluggable-fitness contract as mu_lambda_es: evolve/3 takes
%% fun(([float()]) -> float()), higher is better, and returns
%% #{best, fitness, generations, evaluations, reason}. That makes it a drop-in
%% swap in the EXP-025/026 evaluation harness (EXP-028).
%%
%% Reference: Hansen, "The CMA Evolution Strategy: A Tutorial" (arXiv 1604.00772)
%% for the update equations; Ros and Hansen 2008 for the diagonal learning-rate
%% scaling ((N+2)/3 on c1 and cmu).
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(sep_cma_es).

-export([evolve/3]).

-type vector() :: [float()].
-type fitness_fun() :: fun((vector()) -> float()).

-define(NEG_INF, -1.0e308).

%% @doc Run a separable (diagonal) CMA-ES.
%%
%% Opts:
%%   lambda           offspring per generation (default 4 + floor(3*ln N))
%%   mu               parents (default lambda div 2)
%%   max_generations  cap (default 1000)
%%   fitness_goal     stop when best fitness >= this (default infinity)
%%   init_sigma       initial step size (default 1.0)
%%   x0               initial mean vector (default zeros)
%%
%% Returns #{best, fitness, generations, evaluations, reason}
%% (reason = solved | max_generations).
-spec evolve(fitness_fun(), pos_integer(), map()) -> map().
evolve(FitnessFun, N, Opts) ->
    Lambda = maps:get(lambda, Opts, 4 + trunc(3.0 * math:log(N))),
    Mu = maps:get(mu, Opts, Lambda div 2),
    MaxGen = maps:get(max_generations, Opts, 1000),
    Goal = maps:get(fitness_goal, Opts, infinity),
    Sigma0 = maps:get(init_sigma, Opts, 1.0),
    M0 = maps:get(x0, Opts, lists:duplicate(N, 0.0)),
    {Weights, MuEff} = recomb_weights(Mu, Lambda),
    P = params(N, MuEff),
    State = #{m => M0, sigma => Sigma0, c => lists:duplicate(N, 1.0),
              ps => lists:duplicate(N, 0.0), pc => lists:duplicate(N, 0.0),
              gen => 0, evals => 0, best => {undefined, ?NEG_INF}},
    loop(FitnessFun, N, Lambda, Mu, MaxGen, Goal, Weights, MuEff, P, State).

%%%============================================================================
%%% Strategy parameters (Hansen tutorial; sep-CMA (N+2)/3 scaling on c1/cmu)
%%%============================================================================

recomb_weights(Mu, Lambda) ->
    Raw = [math:log((Lambda + 1) / 2) - math:log(I) || I <- lists:seq(1, Mu)],
    Sum = lists:sum(Raw),
    W = [R / Sum || R <- Raw],
    MuEff = 1.0 / lists:sum([Wi * Wi || Wi <- W]),
    {W, MuEff}.

params(N, MuEff) ->
    Cs = (MuEff + 2) / (N + MuEff + 5),
    Ds = 1 + Cs + 2 * max(0.0, math:sqrt((MuEff - 1) / (N + 1)) - 1),
    Cc = (4 + MuEff / N) / (N + 4 + 2 * MuEff / N),
    C1f = 2 / (math:pow(N + 1.3, 2) + MuEff),
    Cmuf = 2 * (MuEff - 2 + 1 / MuEff) / (math:pow(N + 2, 2) + MuEff),
    Sep = (N + 2) / 3,
    C1 = min(1.0, C1f * Sep),
    Cmu = min(1.0 - C1, Cmuf * Sep),
    ChiN = math:sqrt(N) * (1 - 1 / (4 * N) + 1 / (21 * N * N)),
    #{cs => Cs, ds => Ds, cc => Cc, c1 => C1, cmu => Cmu, chiN => ChiN}.

%%%============================================================================
%%% Main loop
%%%============================================================================

loop(_F, _N, _L, _Mu, 0, _Goal, _W, _MuEff, _P, S) ->
    result(S, max_generations);
loop(F, N, Lambda, Mu, GenLeft, Goal, W, MuEff, P, S) ->
    #{m := M, sigma := Sigma, c := C, ps := Ps, pc := Pc,
      gen := Gen, evals := Evals, best := Best} = S,
    Sqc = [math:sqrt(Cj) || Cj <- C],
    %% Sample and evaluate lambda offspring.
    Offspring = [sample(M, Sigma, Sqc) || _ <- lists:seq(1, Lambda)],
    Evaluated = [{X, F(X)} || X <- Offspring],
    Sorted = lists:sort(fun({_, Fa}, {_, Fb}) -> Fa >= Fb end, Evaluated),
    {TopX, TopF} = hd(Sorted),
    TopMu = [X || {X, _} <- lists:sublist(Sorted, Mu)],
    %% Recombination: weighted mean of the mu best.
    Mnew = weighted_vsum(W, TopMu),
    Yw = vscale(1 / Sigma, vsub(Mnew, M)),
    Gen1 = Gen + 1,
    %% Step-size evolution path (C^-1/2 y = y ./ sqrt(c) for diagonal C).
    Cs = maps:get(cs, P),
    Ps1 = vadd(vscale(1 - Cs, Ps),
               vscale(math:sqrt(Cs * (2 - Cs) * MuEff),
                      [Yj / Sqcj || {Yj, Sqcj} <- lists:zip(Yw, Sqc)])),
    PsNorm = vnorm(Ps1),
    Hsig = hsig(PsNorm, Cs, Gen1, N, maps:get(chiN, P)),
    %% Covariance evolution path.
    Cc = maps:get(cc, P),
    Pc1 = vadd(vscale(1 - Cc, Pc),
               vscale(Hsig * math:sqrt(Cc * (2 - Cc) * MuEff), Yw)),
    %% Diagonal covariance update (rank-one + rank-mu, elementwise).
    Ys = [vscale(1 / Sigma, vsub(X, M)) || X <- TopMu],
    RankMu = weighted_vsum(W, [[Yj * Yj || Yj <- Y] || Y <- Ys]),
    DeltaH = (1 - Hsig) * Cc * (2 - Cc),
    C1v = maps:get(c1, P),
    Cmuv = maps:get(cmu, P),
    Cnew = [(1 - C1v - Cmuv) * Cj + C1v * (Pcj * Pcj + DeltaH * Cj) + Cmuv * Rj
            || {Cj, Pcj, Rj} <- lists:zip3(C, Pc1, RankMu)],
    %% Step-size update.
    Sigma1 = Sigma * math:exp((Cs / maps:get(ds, P)) * (PsNorm / maps:get(chiN, P) - 1)),
    NewBest = case TopF > element(2, Best) of
                  true -> {TopX, TopF};
                  false -> Best
              end,
    S1 = S#{m := Mnew, sigma := Sigma1, c := Cnew, ps := Ps1, pc := Pc1,
            gen := Gen1, evals := Evals + Lambda, best := NewBest},
    case reached(TopF, Goal) of
        true -> result(S1, solved);
        false -> loop(F, N, Lambda, Mu, GenLeft - 1, Goal, W, MuEff, P, S1)
    end.

%% h_sigma: stall the rank-one update when the step-size path is over-long.
hsig(PsNorm, Cs, Gen1, N, ChiN) ->
    Denom = math:sqrt(1 - math:pow(1 - Cs, 2 * Gen1)) * ChiN,
    case PsNorm / Denom < 1.4 + 2 / (N + 1) of
        true -> 1.0;
        false -> 0.0
    end.

sample(M, Sigma, Sqc) ->
    [Mj + Sigma * Sqcj * rand:normal() || {Mj, Sqcj} <- lists:zip(M, Sqc)].

reached(_F, infinity) -> false;
reached(F, Goal) -> F >= Goal.

result(S, Reason) ->
    {BestW, BestF} = maps:get(best, S),
    #{best => BestW, fitness => BestF, generations => maps:get(gen, S),
      evaluations => maps:get(evals, S), reason => Reason}.

%%%============================================================================
%%% Vector helpers
%%%============================================================================

vadd(A, B) -> [X + Y || {X, Y} <- lists:zip(A, B)].
vsub(A, B) -> [X - Y || {X, Y} <- lists:zip(A, B)].
vscale(S, A) -> [S * X || X <- A].
vnorm(A) -> math:sqrt(lists:sum([X * X || X <- A])).

%% sum_i w_i * V_i, elementwise over a list of equal-length vectors.
weighted_vsum(Ws, Vs) ->
    Zero = lists:duplicate(length(hd(Vs)), 0.0),
    lists:foldl(fun({W, V}, Acc) -> vadd(Acc, vscale(W, V)) end,
                Zero, lists:zip(Ws, Vs)).
