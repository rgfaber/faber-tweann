%% @doc Differential tests: the Rust NIFs and the pure Erlang fallback must
%% agree.
%%
%% faber_tweann carries two implementations of the same numeric surface:
%% faber_nn_nifs (Rust) and tweann_nif_fallback (Erlang). Before the NIFs were
%% absorbed into this package in v2.0.0 they lived in a separate repository,
%% each side had its own test suite, and neither suite ever compared them.
%%
%% They had silently diverged:
%%
%%   - weight_distance_l1/2 divided by vector length in Rust and did not in
%%     Erlang, disagreeing by a factor of the vector length while both
%%     claimed to compute Manhattan distance.
%%   - weight_distance_l2/2 likewise divided by sqrt(length).
%%   - weight_distance_batch/3 took a boolean and returned sorted
%%     {Index, Distance} pairs in Rust, against an atom and a plain distance
%%     list in Erlang.
%%   - random_weights_batch/1 took a bare size list in Rust and silently
%%     discarded the requested mean and standard deviation, against
%%     {Count, Mean, StdDev} specs in Erlang.
%%
%% Each suite asserted its own side, so all of it passed. This module exists
%% so that cannot happen again. Add a case here for every function that has
%% both a native and a fallback implementation.
%%
%% Only deterministic functions are compared directly. Functions returning
%% random values are compared on shape and distribution properties.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(nif_fallback_conformance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EPS, 1.0e-9).

%%==============================================================================
%% Helpers
%%==============================================================================

nif_available() ->
    code:which(faber_nn_nifs) =/= non_existing andalso faber_nn_nifs:is_loaded().

%% Assert both implementations produce the same float.
agree_float(Fun, Args) ->
    Native = erlang:apply(faber_nn_nifs, Fun, Args),
    Fallback = erlang:apply(tweann_nif_fallback, Fun, Args),
    ?assert(
        abs(Native - Fallback) < ?EPS,
        lists:flatten(
            io_lib:format(
                "~p/~p disagreed: native=~p fallback=~p args=~p",
                [Fun, length(Args), Native, Fallback, Args]
            )
        )
    ).

%% Assert both implementations produce the same list of floats.
agree_float_list(Fun, Args) ->
    Native = erlang:apply(faber_nn_nifs, Fun, Args),
    Fallback = erlang:apply(tweann_nif_fallback, Fun, Args),
    ?assertEqual(
        length(Fallback),
        length(Native),
        lists:flatten(
            io_lib:format("~p/~p length disagreed", [Fun, length(Args)])
        )
    ),
    lists:foreach(
        fun({N, F}) -> ?assert(abs(N - F) < ?EPS) end,
        lists:zip(Native, Fallback)
    ).

%%==============================================================================
%% Tests
%%==============================================================================

conformance_test_() ->
    case nif_available() of
        false ->
            {"native NIFs unavailable, differential tests skipped", fun() -> ok end};
        true ->
            [
                {"weight_distance_l1 agrees", fun weight_distance_l1_agrees/0},
                {"weight_distance_l2 agrees", fun weight_distance_l2_agrees/0},
                {"weight_distance_batch agrees", fun weight_distance_batch_agrees/0},
                {"euclidean_distance agrees", fun euclidean_distance_agrees/0},
                {"random_weights_batch shape agrees", fun random_weights_batch_shape_agrees/0}
            ]
    end.

weight_distance_l1_agrees() ->
    Cases = [
        {[0.0, 0.0, 0.0], [1.0, 1.0, 1.0]},
        {[0.1, 0.2, 0.3], [0.1, 0.2, 0.3]},
        {[-1.0, 2.5], [1.0, -2.5]},
        {[], []}
    ],
    [agree_float(weight_distance_l1, [W1, W2]) || {W1, W2} <- Cases].

weight_distance_l2_agrees() ->
    Cases = [
        {[0.0, 0.0], [3.0, 4.0]},
        {[0.1, 0.2, 0.3], [0.1, 0.2, 0.3]},
        {[-1.0, 2.5], [1.0, -2.5]},
        {[], []}
    ],
    [agree_float(weight_distance_l2, [W1, W2]) || {W1, W2} <- Cases].

weight_distance_batch_agrees() ->
    Target = [0.0, 0.0],
    Others = [[1.0, 0.0], [0.5, 0.0], [2.0, 0.0], [3.0, 4.0]],
    agree_float_list(weight_distance_batch, [Target, Others, l1]),
    agree_float_list(weight_distance_batch, [Target, Others, l2]).

euclidean_distance_agrees() ->
    Cases = [
        {[0.0, 0.0], [3.0, 4.0]},
        {[1.0, 2.0, 3.0], [1.0, 2.0, 3.0]}
    ],
    [agree_float(euclidean_distance, [A, B]) || {A, B} <- Cases].

%% Random output cannot be compared elementwise. Compare the contract: one
%% vector per spec, each of the requested length.
random_weights_batch_shape_agrees() ->
    Specs = [{5, 0.0, 1.0}, {10, 0.5, 0.2}, {0, 0.0, 1.0}],
    Native = faber_nn_nifs:random_weights_batch(Specs),
    Fallback = tweann_nif_fallback:random_weights_batch(Specs),
    ?assertEqual(length(Specs), length(Native)),
    ?assertEqual(length(Specs), length(Fallback)),
    Expected = [N || {N, _, _} <- Specs],
    ?assertEqual(Expected, [length(W) || W <- Native]),
    ?assertEqual(Expected, [length(W) || W <- Fallback]).
