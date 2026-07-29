%% @doc Tests for the deterministic integer forward pass (PLAN_ROBO_RUMBLE phase 0).
%%
%% AUDIT FIX. These did not exist, while robo_net's own docs asserted that the
%% tanh table "is verified by recomputation rather than trusted, which is the
%% standard the front applies to match results". It was not: nothing recomputed
%% it. A single mistyped table entry would have changed every match on the front,
%% identically on every machine, so no cross-machine replay diff would ever have
%% caught it.
%%
%% The forward pass is the determinism hazard the plan names in section 3, and it
%% was the one part of the match path with no golden vector. GOLDEN_FORWARD below
%% closes that. Changing it is a rules change and therefore a season boundary,
%% exactly like robo_sim_tests:GOLDEN_HASH; it is never a way to make a test pass.
%%
%% Floats appear in this file and only in this file, in the recomputation oracle.
%% That is the point: the table is CHECKED against libm here, at test time, and
%% never consults libm at match time.
%% @end
-module(robo_net_tests).

-include_lib("eunit/include/eunit.hrl").

-define(GOLDEN_FORWARD,
        <<"49520202A1B9B8E15D3CCF01586A3212421FF0A2D90458856316A75ADAA09456">>).

%%==============================================================================
%% The table, recomputed rather than trusted
%%==============================================================================

%% The claim in the module header, now actually executed: every entry equals
%% round(4096 * tanh(I/32)). This is the test whose absence the audit called a
%% claim of verification that was "a comment, not a fact".
tanh_table_recomputes_test() ->
    [?assertEqual(round(4096 * math:tanh(I / 32)), robo_net:tanh(I * 128))
     || I <- lists:seq(0, 156)].

tanh_is_odd_test() ->
    [?assertEqual(-robo_net:tanh(X), robo_net:tanh(-X))
     || X <- lists:seq(-40000, 40000, 137)].

tanh_is_monotone_and_bounded_test() ->
    Vals = [robo_net:tanh(X) || X <- lists:seq(-24576, 24576, 7)],
    ?assertEqual([], [P || {A, B} = P <- lists:zip(lists:droplast(Vals), tl(Vals)),
                           B < A]),
    ?assert(lists:all(fun(V) -> V >= -4096 andalso V =< 4096 end, Vals)).

tanh_saturates_test() ->
    ?assertEqual(4096, robo_net:tanh(1000000)),
    ?assertEqual(-4096, robo_net:tanh(-1000000)),
    ?assertEqual(0, robo_net:tanh(0)).

%% activate/1 must be exactly odd too, not merely tanh/1. The audit found a
%% competing design that lost this by shifting a signed accumulator directly.
activate_is_odd_test() ->
    [?assertEqual(-robo_net:activate(A), robo_net:activate(-A))
     || A <- lists:seq(-40000000, 40000000, 271831)].

%%==============================================================================
%% The mappings
%%==============================================================================

mappings_are_odd_test() ->
    [?assertEqual(-robo_net:narrow(A), robo_net:narrow(-A))
     || A <- lists:seq(-5000000, 5000000, 31337)],
    [?assertEqual(-robo_net:to_arena(A), robo_net:to_arena(-A))
     || A <- lists:seq(-40000, 40000, 337)],
    [?assertEqual(-robo_net:to_range(A, 32), robo_net:to_range(-A, 32))
     || A <- lists:seq(-4096, 4096, 17)].

%% AUDIT FIX, guarded: a negative Max used to route a negative product into bsr,
%% which floors, falsifying the module's central claim that no right shift here
%% ever sees a negative operand.
to_range_rejects_negative_max_test() ->
    ?assertEqual(0, robo_net:to_range(4095, -4)),
    ?assertEqual(0, robo_net:to_range(-4095, -4)),
    ?assertEqual(0, robo_net:to_range(4095, 0)).

to_range_hits_its_bounds_test() ->
    ?assertEqual(7, robo_net:to_range(4096, 7)),
    ?assertEqual(-7, robo_net:to_range(-4096, 7)),
    ?assertEqual(0, robo_net:to_range(0, 512)).

%%==============================================================================
%% The accumulator
%%==============================================================================

%% dot/3 accumulates exactly in Q24 and rounds exactly once, so summation order
%% cannot change a result. If this ever fails, a reordered input list changes a
%% match and replay is dead.
dot_is_order_invariant_test() ->
    Ws = [123, -456, 789, -1011, 1213, -1415],
    Xs = [11, -22, 33, -44, 55, -66],
    Forward = robo_net:dot(Ws, Xs, 7),
    Reversed = robo_net:dot(lists:reverse(Ws), lists:reverse(Xs), 7),
    ?assertEqual(Forward, Reversed).

weight_count_test() ->
    ?assertEqual(145, robo_net:weight_count([8, 10, 5])),
    ?assertEqual(0, robo_net:weight_count([4])),
    ?assertEqual(0, robo_net:weight_count([])).

%%==============================================================================
%% eval/3 contracts
%%==============================================================================

%% AUDIT FIX: a topology of fewer than two layers has no weight layer, and used
%% to return the RAW inputs, 800x outside the documented output range. That
%% silently broke the guarantee to_range/2's totality is documented to rest on.
degenerate_topology_stays_in_range_test() ->
    ?assertEqual([0, 0, 0, 0], robo_net:eval([4], [], [204800, 1, 2, 3])),
    ?assertEqual([], robo_net:eval([], [], [1, 2, 3])).

%% Output arity follows the declared topology even when the genome is too short,
%% because a malformed genome should lose a match rather than crash one.
short_genome_still_has_right_arity_test() ->
    ?assertEqual(3, length(robo_net:eval([2, 3], [], [100, 200]))),
    ?assertEqual(3, length(robo_net:eval([2, 3], [1, 2], [100, 200]))).

%% Wrong sensor count is padded or truncated rather than crashing.
input_arity_is_forgiving_test() ->
    W = lists:duplicate(robo_net:weight_count([3, 2]), 64),
    ?assertEqual(2, length(robo_net:eval([3, 2], W, []))),
    ?assertEqual(2, length(robo_net:eval([3, 2], W, [1, 2, 3, 4, 5]))).

output_range_holds_test() ->
    W = [((X * 2654435761) rem 2048) - 1024 || X <- lists:seq(1, robo_net:weight_count([5, 7, 4]))],
    Out = robo_net:eval([5, 7, 4], W, [204800, -204800, 0, 12345, -999]),
    ?assertEqual(4, length(Out)),
    ?assert(lists:all(fun(V) -> V >= -256 andalso V =< 256 end, Out)).

%%==============================================================================
%% Determinism harness
%%==============================================================================

forward_is_bit_identical_test() ->
    ?assertEqual(forward_trace(), forward_trace()).

golden_forward_vector_test() ->
    ?assertEqual(?GOLDEN_FORWARD, binary:encode_hex(forward_trace())).

%% A fixed genome driven over a fixed input sweep, hashed. This is the forward
%% pass's counterpart to the engine's golden replay vector, and the vector the
%% cross-machine CI job diffs.
%%
%% Rows is a list of lists of integers, with no map anywhere, and the
%% deterministic option is set as insurance rather than necessity. The reason is
%% recorded because it cost real time to find: robo_match's vector once hashed
%% result maps directly, term_to_binary is not canonical for maps, and the digest
%% then differed between VMs while the terms themselves compared identical. Keep
%% maps out of anything that gets hashed on this front.
forward_trace() ->
    Layers = [5, 7, 4],
    N = robo_net:weight_count(Layers),
    W = [((X * 2654435761) rem 4096) - 2048 || X <- lists:seq(1, N)],
    Rows = [robo_net:eval_q12(Layers, W, [X, -X, X div 2, 1000 - X, X * 3])
            || X <- lists:seq(-2000, 2000, 13)],
    crypto:hash(sha256, term_to_binary(Rows, [deterministic, {minor_version, 2}])).
