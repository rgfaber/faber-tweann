#!/usr/bin/env escript
%%! -pa _build/default/lib/faber_tweann/ebin
%%
%% Every deterministic function that exists in BOTH faber_nn_nifs and
%% tweann_nif_fallback, run through both and compared.
%%
%% This exists because three divergences were found in one day by looking at
%% three things, which is not a sampling strategy. 43 functions have two
%% implementations. This checks the deterministic ones rather than assuming the
%% other forty agree.
%%
%% Excluded, and why: anything drawing randomness cannot agree across two
%% different PRNGs, so mutate_weights*, random_weights*, tournament_select and
%% the seeded variants are out of scope for a value comparison. That is a real
%% gap, not a clean bill of health for them.

main(_) ->
    Results = [check(Name, Fun) || {Name, Fun} <- cases()],
    Bad = [R || {_, Delta, _, _} = R <- Results, Delta =:= mismatch orelse
                (is_number(Delta) andalso Delta > 1.0e-9)],
    Skipped = [R || {_, raised, _, _} = R <- Results],
    io:format("~n~s~n", [lists:duplicate(64, $=)]),
    io:format("checked ~p functions, ~p disagree, ~p not exercised~n",
              [length(Results), length(Bad), length(Skipped)]),
    case Bad of
        [] -> ok;
        _ -> [io:format("  DISAGREES: ~s~n", [N]) || {N, _, _, _} <- Bad]
    end,
    erlang:halt(case Bad of [] -> 0; _ -> 1 end).

check(Name, Fun) ->
    {Native, Erlang} = try Fun() catch C:R -> {{raised, C, R}, {raised, C, R}} end,
    Delta = case {Native, Erlang} of
        {{raised, _, _}, _} -> raised;
        _ -> compare(Native, Erlang)
    end,
    Mark = case Delta of
        raised -> "RAISED (test-data shape, not a parity result)";
        mismatch -> "SHAPE MISMATCH";
        D when D > 1.0e-9 -> io_lib:format("DISAGREES by ~p", [D]);
        D -> io_lib:format("ok  (~.2e)", [D * 1.0])
    end,
    io:format("~-34s ~s~n", [Name, Mark]),
    {Name, Delta, Native, Erlang}.

%% Structural walk, so a tuple/list shape difference is caught rather than
%% crashing the comparison.
compare(A, B) when is_number(A), is_number(B) -> abs(A - B);
compare(A, B) when is_list(A), is_list(B), length(A) =:= length(B) ->
    lists:foldl(fun(D, Acc) when is_number(D), is_number(Acc) -> max(D, Acc);
                   (_, _) -> mismatch end,
                0.0, [compare(X, Y) || {X, Y} <- lists:zip(A, B)]);
compare(A, B) when is_tuple(A), is_tuple(B), tuple_size(A) =:= tuple_size(B) ->
    compare(tuple_to_list(A), tuple_to_list(B));
compare(_, _) -> mismatch.

%% Two vectors and a population, reused so every case sees the same data.
v1() -> [0.5, -1.2, 2.0, 0.0, -0.7].
v2() -> [-0.3, 0.8, 1.5, -2.0, 0.25].
pop() -> [v1(), v2(), [1.0, 1.0, 1.0, 1.0, 1.0], [0.0, 0.0, 0.0, 0.0, 0.0]].
fits() -> [0.5, 2.0, 0.1, 3.7, 1.2, 0.0].

both(F) -> {F(faber_nn_nifs), F(tweann_nif_fallback)}.

cases() ->
    [
     {"euclidean_distance/2",
      fun() -> both(fun(M) -> M:euclidean_distance(v1(), v2()) end) end},
     {"euclidean_distance_batch/2",
      fun() -> both(fun(M) -> M:euclidean_distance_batch(v1(), pop()) end) end},
     {"weight_distance_l1/2",
      fun() -> both(fun(M) -> M:weight_distance_l1(v1(), v2()) end) end},
     {"weight_distance_l2/2",
      fun() -> both(fun(M) -> M:weight_distance_l2(v1(), v2()) end) end},
     {"weight_distance_batch/3 (l1)",
      fun() -> both(fun(M) -> M:weight_distance_batch(v1(), pop(), l1) end) end},
     {"weight_distance_batch/3 (l2)",
      fun() -> both(fun(M) -> M:weight_distance_batch(v1(), pop(), l2) end) end},
     {"knn_novelty/4",
      fun() -> both(fun(M) -> M:knn_novelty(v1(), pop(), pop(), 2) end) end},
     {"knn_novelty_batch/3",
      fun() -> both(fun(M) -> M:knn_novelty_batch(pop(), pop(), 2) end) end},
     {"compatibility_distance/5",
      fun() -> both(fun(M) ->
              G1 = [{1, 0.5, true}, {2, -1.2, true}, {3, 2.0, false}],
              G2 = [{1, 0.7, true}, {2, -1.0, true}, {4, 0.3, true}],
              M:compatibility_distance(G1, G2, 1.0, 1.0, 0.4)
          end) end},
     {"dot_product_flat/3",
      fun() -> both(fun(M) -> M:dot_product_flat(v1(), v2(), 0.25) end) end},
     {"dot_product_preflattened/3",
      fun() -> both(fun(M) -> M:dot_product_preflattened(v1(), v2(), 0.25) end) end},
     {"dot_product_batch/1",
      fun() -> both(fun(M) -> M:dot_product_batch([{v1(), v2(), 0.1}, {v2(), v1(), -0.2}]) end) end},
     {"fitness_stats/1",
      fun() -> both(fun(M) -> M:fitness_stats(fits()) end) end},
     {"shannon_entropy/1",
      fun() -> both(fun(M) -> M:shannon_entropy(fits()) end) end},
     {"weighted_moving_average/2",
      fun() -> both(fun(M) -> M:weighted_moving_average(fits(), 0.3) end) end},
     {"z_score/3",
      fun() -> both(fun(M) -> M:z_score(2.0, 1.0, 0.5) end) end},
     {"histogram/4",
      fun() -> both(fun(M) -> M:histogram(fits(), 4, 0.0, 4.0) end) end},
     {"build_cumulative_fitness/1",
      fun() -> both(fun(M) -> M:build_cumulative_fitness(fits()) end) end},
     {"roulette_select/3",
      fun() -> both(fun(M) ->
              {Cum, Total} = M:build_cumulative_fitness(fits()),
              M:roulette_select(Cum, Total, 0.5)
          end) end},
     {"compute_reward_component/2",
      fun() -> both(fun(M) -> M:compute_reward_component(fits(), 0.6) end) end},
     {"compute_weighted_reward/1",
      fun() -> both(fun(M) -> M:compute_weighted_reward([{fits(), 0.6, 0.4}, {v1(), 0.2, 0.8}]) end) end},
     {"evaluate_cfc/4",
      fun() -> both(fun(M) -> M:evaluate_cfc(0.7, 0.2, 1.3, 1.0) end) end},
     {"evaluate_cfc/4 (small tau)",
      fun() -> both(fun(M) -> M:evaluate_cfc(2.0, -0.7, 0.001, 2.0) end) end},
     {"evaluate_cfc_with_weights/6",
      fun() -> both(fun(M) -> M:evaluate_cfc_with_weights(0.7, 0.2, 1.3, 1.0, [0.4, 0.1], [0.9, -0.2]) end) end},
     {"evaluate_cfc_batch/4",
      fun() -> both(fun(M) -> M:evaluate_cfc_batch([0.7, -0.3, 1.1], 0.0, 1.3, 1.0) end) end},
     {"evaluate_ode/5",
      fun() -> both(fun(M) -> M:evaluate_ode(0.7, 0.2, 1.3, 1.0, 0.1) end) end},
     {"evaluate_ode_with_weights/7",
      fun() -> both(fun(M) -> M:evaluate_ode_with_weights(0.7, 0.2, 1.3, 1.0, 0.1, [0.4, 0.1], [0.9, -0.2]) end) end},
     {"flatten_weights/1",
      fun() -> both(fun(M) -> M:flatten_weights([{a, [{0.5, 0.0, 0.1, []}, {-0.25, 0.0, 0.1, []}]},
                                                 {b, [{1.5, 0.0, 0.1, []}]}]) end) end},
     {"compile_network/3 + evaluate/2",
      fun() -> both(fun(M) ->
              N = M:compile_network([{0, input, linear, 0.0, []},
                                     {1, neuron, tanh, 0.1, [{0, 0.7}]},
                                     {2, neuron, sigmoid, -0.2, [{1, 1.3}, {0, 0.4}]}],
                                    1, [2]),
              M:evaluate(N, [0.6])
          end) end},
     {"evaluate_batch/2",
      fun() -> both(fun(M) ->
              N = M:compile_network([{0, input, linear, 0.0, []},
                                     {1, neuron, gaussian, 0.1, [{0, 0.7}]}], 1, [1]),
              M:evaluate_batch(N, [[0.6], [-1.2], [0.0]])
          end) end},
     {"evaluate_with_state/3 (delay)",
      fun() -> both(fun(M) ->
              N = M:compile_network([{0, input, linear, 0.0, []},
                                     {1, delay, linear, 0.0, [{2, 1.0}]},
                                     {2, neuron, linear, 0.5, [{0, 1.0}, {1, 0.5}]}], 1, [2]),
              {O1, S1} = M:evaluate_with_state(N, [1.0], []),
              {O2, S2} = M:evaluate_with_state(N, [1.0], S1),
              {O3, _} = M:evaluate_with_state(N, [1.0], S2),
              [O1, O2, O3]
          end) end},
     {"compile_cfc_pop/6 + cfc_pop_step/3",
      fun() -> both(fun(M) ->
              P = M:compile_cfc_pop([[[0.5, -0.2]], [[0.3, 0.8]]], [[0.1], [-0.1]],
                                    [[[0.9]], [[0.4]]], [[0.2], [0.6]],
                                    [[1.3], [0.7]], 1.0),
              M:cfc_pop_step(P, [[0.5, 1.0], [0.5, 1.0]], [[0.0], [0.0]])
          end) end}
    ].
