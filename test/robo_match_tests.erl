%% @doc Kill gate 0a: the gauntlet ranks in the pre-registered order.
%%
%% AUDIT FIX. This is the test whose absence was the audit's first FATAL: the
%% gauntlet asserted its own difficulty order in a docstring, nothing in the repo
%% drove robo_gauntlet:act/4, and no scoring rule existed. PLAN_ROBO_RUMBLE.md
%% section 6 gates the entire front on this, so until now the gate was a claim
%% rather than a measurement.
%%
%% The ordering assertion is the load-bearing one. If it fails, the competence
%% floor is not calibrated and no later result about evolution clearing it means
%% anything, so a failure here should stop work rather than be relaxed.
%% @end
-module(robo_match_tests).

-include_lib("eunit/include/eunit.hrl").
-include("robo_sim.hrl").

-define(GOLDEN_MATCH,
        <<"DFCD8106EDC9AE214F6AE99BB5F4988FE441243284A6D3769634D778B3895E88">>).

%%==============================================================================
%% Kill gate 0a
%%==============================================================================

%% THE GATE. Damage decides, survival breaks ties, ranked against the whole
%% field over the whole pre-registered start ensemble in both seats.
gauntlet_ranks_in_preregistered_order_test_() ->
    {timeout, 300, fun() ->
        ?assertEqual([predictive_gun, circle_strafer, rammer, spinner, sitting_duck],
                     robo_match:rank())
    end}.

%% The top three must separate on DAMAGE alone, not on the tiebreak. If they ever
%% collapse onto survival, the ladder is being decided by attrition rather than
%% by marksmanship and the rungs stop meaning what they claim.
top_three_separate_on_damage_test_() ->
    {timeout, 300, fun() ->
        Field = robo_match:field(),
        D = fun(K) -> {K, Dmg, _S} = lists:keyfind(K, 1, Field), Dmg end,
        ?assert(D(predictive_gun) > D(circle_strafer)),
        ?assert(D(circle_strafer) > D(rammer)),
        ?assert(D(rammer) > 0)
    end}.

%% AUDIT FIX, rungs 1 and 2. Neither can hurt the other, so the pair is
%% degenerate head to head and only separates against the field: the duck stands
%% still and dies to the tracking bots, the spinner moves and does not. Both must
%% deal exactly zero damage, or the spinner has grown a gun again and the reason
%% this rung was disarmed has been forgotten.
bottom_two_separate_on_survival_only_test_() ->
    {timeout, 300, fun() ->
        Field = robo_match:field(),
        {sitting_duck, DD, DS} = lists:keyfind(sitting_duck, 1, Field),
        {spinner, SD, SS} = lists:keyfind(spinner, 1, Field),
        ?assertEqual(0, DD),
        ?assertEqual(0, SD),
        ?assert(SS > DS)
    end}.

%%==============================================================================
%% AUDIT FIX: walls
%%==============================================================================

%% Every bot used to be able to kill itself on a wall with NO OPPONENT PRESENT,
%% because hunt/0 carried no wall term and wall_push/1 was reachable only once a
%% contact was held, so avoidance was missing in exactly the state that has no
%% target to look at. Measured then: three of the five died outright from three of
%% six starts, with up to 1591 wall-contact turns out of 2000.
%%
%% This must be run SOLO, in an empty arena. An earlier version of this test put
%% the bot against a sitting duck and failed, but not for the reason it claimed:
%% the spinner had driven into the duck and the two ground each other down on ram
%% damage at 0.6 a turn, which is the engine working correctly and has nothing to
%% do with walls. Testing the property in the presence of an opponent cannot
%% separate the two causes.
no_bot_kills_itself_on_walls_solo_test_() ->
    {timeout, 300, fun() ->
        [?assertEqual({100, false}, solo(K, S))
         || K <- robo_gauntlet:kinds(), S <- robo_match:starts()]
    end}.

%% One bot alone for 2000 turns. robo_sim:finished/1 is true immediately with a
%% single tank, so the arena is stepped directly rather than through the match
%% runner. Returns {WholeEnergy, Dead}; anything but {100, false} means the bot
%% touched a wall, since nothing else in an empty arena can cost it energy.
solo(Kind, {X, Y, H, _BX, _BY, _BH}) ->
    Arena = robo_sim:new([{a, X, Y, H}]),
    solo_loop(Arena, robo_gauntlet:init(Kind), Kind, 2000).

solo_loop(Arena, _S, _Kind, 0) ->
    T = lists:keyfind(a, #tank.id, Arena#arena.tanks),
    {T#tank.energy div 256, T#tank.dead};
solo_loop(Arena, S, Kind, N) ->
    T = lists:keyfind(a, #tank.id, Arena#arena.tanks),
    solo_next(Arena, S, Kind, N, T, T#tank.dead).

solo_next(_Arena, _S, _Kind, _N, T, true) -> {T#tank.energy div 256, true};
solo_next(Arena, S, Kind, N, T, false) ->
    {Intent, S2} = robo_gauntlet:act(Kind, S, T, Arena),
    solo_loop(robo_sim:step(Arena, [{a, Intent}]), S2, Kind, N - 1).

%%==============================================================================
%% AUDIT FIX: the perception contract
%%==============================================================================

%% A bot must be handed the arena whose scans were produced by the same step that
%% produced its own tank. A runner that stepped first would hand every bot a
%% one-turn-stale world, silently, and nothing would fail. This pins it: acting
%% on a freshly stepped arena and acting on the previous one must differ, so the
%% mistake is detectable rather than invisible.
%% Probed on the bot STATE rather than the intent. Two consecutive arenas can
%% carry identical scans when nothing has moved, so comparing intents does not
%% reliably separate a fresh world from a stale one; comparing what the bot
%% RECORDED does, because a scan present is a contact held and a scan absent is
%% an aged track. An earlier version of this test compared intents and passed
%% vacuously for exactly that reason.
stale_arena_is_detectable_test() ->
    Arena = robo_sim:step(robo_sim:new([{a, 100, 300, 0}, {b, 400, 300, 128}]), []),
    Blind = Arena#arena{scans = []},
    T = lists:keyfind(a, #tank.id, Arena#arena.tanks),
    S0 = robo_gauntlet:init(predictive_gun),
    ?assertNotEqual([], Arena#arena.scans),
    {_, Fresh} = robo_gauntlet:act(predictive_gun, S0, T, Arena),
    {_, Stale} = robo_gauntlet:act(predictive_gun, S0, T, Blind),
    ?assertNotEqual(Fresh, Stale).

%% The duck really does nothing, which is what makes it a usable smoke test.
sitting_duck_is_inert_test() ->
    Arena = robo_sim:new([{a, 100, 300, 0}, {b, 400, 300, 128}]),
    T = lists:keyfind(a, #tank.id, Arena#arena.tanks),
    {Intent, _} = robo_gauntlet:act(sitting_duck, robo_gauntlet:init(sitting_duck), T, Arena),
    ?assertEqual(#intent{}, Intent).

%%==============================================================================
%% angle_of: the AUDIT FIX for the one-sided rounding bias
%%==============================================================================

%% It used to floor, so every aim, radar lock and steer carried a systematic lean
%% in one rotational direction, measured at mean -0.48 angle units and never once
%% positive. Rounding to nearest bounds the error symmetrically. The oracle uses
%% floats deliberately: this is a test, not the match path.
angle_of_is_unbiased_test() ->
    Errs = [err(X, Y) || X <- lists:seq(-40, 40), Y <- lists:seq(-40, 40),
                         {X, Y} =/= {0, 0}],
    ?assert(lists:max(Errs) =< 0.5),
    ?assert(lists:min(Errs) >= -0.5),
    Mean = lists:sum(Errs) / length(Errs),
    ?assert(abs(Mean) < 0.01).

err(X, Y) ->
    A = robo_gauntlet:angle_of({X, Y}),
    T = math:atan2(Y, X) * 256 / (2 * math:pi()),
    D = A - T,
    D - round(D / 256) * 256.

%%==============================================================================
%% Determinism
%%==============================================================================

match_is_bit_identical_test() ->
    ?assertEqual(match_trace(), match_trace()).

golden_match_vector_test() ->
    ?assertEqual(?GOLDEN_MATCH, binary:encode_hex(match_trace())).

%% A fixed pairing at a fixed start, hashed. The cross-machine CI job diffs this
%% alongside the engine's and the forward pass's vectors, so all three layers of
%% the match path are covered rather than only the physics.
%%
%% MAPS ARE CANONICALISED OUT BEFORE HASHING, and this is not fastidiousness.
%% term_to_binary is NOT canonical for maps: the first version of this vector
%% hashed robo_match:match/3's result maps directly and produced a different
%% digest under eunit than under a plain erl, from byte 14 onward, while the
%% terms themselves compared byte-identical when printed. A golden vector whose
%% value depends on which VM serialised it is worse than no vector, because it
%% would fail the cross-machine CI job for a reason that has nothing to do with
%% the simulation and would train everyone to re-pin it. So the result is
%% flattened to plain tuples in a fixed field order, and the deterministic option
%% is set as well. robo_sim and robo_net hash only tuples and lists, which is
%% why their vectors were stable throughout.
match_trace() ->
    Results = [canon(robo_match:match(A, B, S))
               || {A, B} <- [{predictive_gun, circle_strafer},
                             {rammer, spinner},
                             {circle_strafer, sitting_duck}],
                  S <- robo_match:starts()],
    crypto:hash(sha256, term_to_binary(Results, [deterministic, {minor_version, 2}])).

canon(#{a := A, b := B, turns := T}) -> {T, outcome_tuple(A), outcome_tuple(B)}.

outcome_tuple(#{damage := D, survived := S, alive := L}) -> {D, S, L}.
