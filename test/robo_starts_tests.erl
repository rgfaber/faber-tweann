%% @doc Tests for the start set, against the values the research actually ran on.
%%
%% THIS IS AN EXTRACTION, SO THE ONLY QUESTION IS WHETHER IT MOVED FAITHFULLY.
%% The generator was written inside the phase 0 experiment runner and every number
%% on this front was measured through it. If this copy differs by one angle unit
%% on one index, every future result silently stops being comparable with the
%% archive, and nothing anywhere would fail.
%%
%% GOLDEN_SHA IS NOT MY ARITHMETIC. It was taken by running the RUNNER's own
%% starts/0 at the pinned engine, in faber-programmes, and hashing the result. The
%% first two values below were printed from that same run. So this file checks the
%% extraction against the research, not against itself, which is the difference
%% between a test and a tautology.
%%
%% Changing GOLDEN_SHA is a RULES CHANGE and a season boundary, like the golden
%% match vector and the golden channel vector. It is never a way to make a test
%% pass.
%% @end
-module(robo_starts_tests).

-include_lib("eunit/include/eunit.hrl").

%% sha256 over term_to_binary of all 120 starts, deterministic, minor_version 2.
-define(GOLDEN_SHA,
        <<"F198D94DB622A8FC51CB26D2A0F0F6D0B21B0E5258A34DBD6DD5697090CEDCA5">>).

%%==============================================================================
%% The extraction is faithful
%%==============================================================================

golden_start_set_test() ->
    Bin = term_to_binary(robo_starts:all(), [deterministic, {minor_version, 2}]),
    ?assertEqual(?GOLDEN_SHA, binary:encode_hex(crypto:hash(sha256, Bin))).

%% Printed from the runner alongside the hash, so a failure says WHICH end moved
%% rather than only that something did.
golden_endpoints_test() ->
    ?assertEqual({197, 251, 225, 311, 373, 153}, robo_starts:at(1)),
    ?assertEqual({156, 373, 105, 216, 102, 41}, robo_starts:at(120)),
    ?assertEqual(120, length(robo_starts:all())),
    ?assertEqual(120, robo_starts:count()).

%% The splits are part of the contract: a result claiming comparability with the
%% phase 0 endpoint has to be measured on the same 80 geometries.
golden_splits_test() ->
    ?assertEqual(6, length(robo_starts:split(train))),
    ?assertEqual(80, length(robo_starts:split(heldout))),
    ?assertEqual(30, length(robo_starts:split(calibration))),
    ?assertEqual({197, 251, 225, 311, 373, 153}, hd(robo_starts:split(train))),
    ?assertEqual({338, 435, 66, 455, 327, 90}, hd(robo_starts:split(heldout))),
    ?assertEqual({402, 323, 220, 105, 355, 28}, hd(robo_starts:split(calibration))).

splits_are_disjoint_test() ->
    T = robo_starts:split(train),
    H = robo_starts:split(heldout),
    C = robo_starts:split(calibration),
    ?assertEqual([], [X || X <- T, lists:member(X, H)]),
    ?assertEqual([], [X || X <- H, lists:member(X, C)]),
    ?assertEqual([], [X || X <- T, lists:member(X, C)]).

%%==============================================================================
%% The properties the offset exists to guarantee
%%==============================================================================

%% THE POINT OF THE WHOLE GENERATOR. An all-mutually-facing start set drew 106 of
%% 160 with 70 percent of matches censored at the turn cap. Every start must be
%% off the exact facing angle by at least 8 units, or "fire straight ahead on turn
%% one" clears the first rung by itself and stalemates dominate.
%%
%% This is the property a naive circle placement violates: two entrants placed
%% opposite each other facing the centre differ by exactly 128, which is exactly
%% facing, which is the geometry this generator was built to avoid.
no_start_is_exactly_facing_test() ->
    Bad = [S || {AX, AY, AH, BX, BY, BH} = S <- robo_starts:all(),
                exactly_facing(AX, AY, AH, BX, BY, BH)],
    ?assertEqual([], Bad).

exactly_facing(AX, AY, AH, BX, BY, BH) ->
    ToB = robo_starts:angle_of({BX - AX, BY - AY}),
    ToA = robo_starts:angle_of({AX - BX, AY - BY}),
    off_by(AH, ToB) < 8 andalso off_by(BH, ToA) < 8.

off_by(A, B) -> min(abs(A - B), 256 - abs(A - B)).

%% No match may open inside ram range, or the opening move decides it.
every_pair_is_separated_test() ->
    Close = [S || {AX, AY, _AH, BX, BY, _BH} = S <- robo_starts:all(),
                  robo_sim:dist({AX, AY}, {BX, BY}) < 150],
    ?assertEqual([], Close).

%% Every tank starts inside the arena with clearance. The arena reports FIXED
%% POINT while these are whole units, and conflating the two is the error that put
%% an earlier placement 256x outside the world.
every_start_is_inside_the_arena_test() ->
    {W, H} = robo_sim:arena_size(),
    {WU, HU} = {W div 256, H div 256},
    [begin
         ?assert(AX > 0 andalso AX < WU),
         ?assert(AY > 0 andalso AY < HU),
         ?assert(BX > 0 andalso BX < WU),
         ?assert(BY > 0 andalso BY < HU)
     end || {AX, AY, _, BX, BY, _} <- robo_starts:all()],
    ok.

%% Angles are binary angles, and a start built with an out-of-range heading would
%% be silently wrapped by the engine into a different geometry.
every_heading_is_a_binary_angle_test() ->
    [begin
         ?assert(AH >= 0 andalso AH =< 255),
         ?assert(BH >= 0 andalso BH =< 255)
     end || {_, _, AH, _, _, BH} <- robo_starts:all()],
    ok.

%%==============================================================================
%% Determinism
%%==============================================================================

%% A start is a pure function of its index, with no rand, no clock and no libm,
%% which is what lets a published result name its start set by identity rather
%% than by shipping 120 tuples.
generation_is_deterministic_test() ->
    ?assertEqual(robo_starts:all(), robo_starts:all()),
    ?assertEqual(robo_starts:at(57), robo_starts:at(57)).
