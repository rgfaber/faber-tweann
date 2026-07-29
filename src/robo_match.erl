%% @doc Robo Rumble: the match runner, the scoring rule and the start ensemble.
%%
%% AUDIT FIX. This module exists because an adversarial audit found the kill gate
%% was not executable: the gauntlet asserted its own difficulty order in a
%% docstring, nothing in the repo drove robo_gauntlet:act/4 at all, and no
%% scoring rule existed anywhere. PLAN_ROBO_RUMBLE.md section 6 gates the whole
%% front on kill gate 0a, so an unrunnable gate meant every later result on this
%% front rested on a claim nobody had ever measured.
%%
%% THE PERCEPTION CONTRACT, PINNED HERE. A bot must be handed the arena whose
%% scans field was produced by the same step that produced its own tank. Step
%% first and you hand every bot a world one turn stale, silently, and nothing
%% fails. loop/4 therefore acts on the CURRENT arena and only then steps, and
%% robo_match_tests asserts that a runner which steps first is detectably wrong.
%%
%% THE SCORING RULE IS DAMAGE-PRIMARY, SURVIVAL AS TIEBREAK, AND NEVER ENERGY.
%% This is a measured decision, not a taste. robo_gauntlet budgets every gun so
%% that a bot which is missing decays toward its fire floor while a bot that
%% never fires keeps a full bar, so ANY energy-derived score structurally rewards
%% not shooting. Measured, energy scoring ranked the sitting duck above a bot
%% that moves and shoots. It would also hand a future fitness function a strong
%% local optimum reading "never fire", which is worth knowing before that
%% function is written rather than after.
%%
%% There are no magic weights here. Damage and survival are reported and ordered
%% separately, lexicographically, so no arbitrary exchange rate between them is
%% ever invented. A weighted sum would have to justify its constants and could
%% not.
%%
%% RANK AGAINST THE FIELD, NOT PAIRWISE. sitting_duck versus spinner is
%% degenerate on purpose: neither can hurt the other, so that pair cannot rank
%% robustly under any rule and reading the ladder off adjacent pairs is
%% meaningless. Against the field the two separate cleanly, because the duck
%% stands still and dies to the tracking bots while the spinner moves and does
%% not. That is exactly the difference rung 2 is supposed to encode.
%%
%% DETERMINISM. The engine has no randomness, so a start configuration is the
%% ONLY free variable in a match. The ensemble below is therefore pre-registered
%% in code rather than chosen per experiment, and both seats are always played,
%% because a gate evaluated on starts picked after seeing the results is not a
%% gate.
%% @end
-module(robo_match).

-include("robo_sim.hrl").

-export([starts/0, match/3, duel/3, field/0, rank/0, score_of/2]).
-export([run/4]).

-define(FP, 256).

-type outcome() :: #{damage := integer(), survived := non_neg_integer(),
                     alive := boolean()}.

%%==============================================================================
%% The pre-registered start ensemble
%%==============================================================================

%% Six starts, fixed. Chosen before any result was looked at, to span the
%% distances and geometries a controller can meet: long open range, short range,
%% a corner (where wall damage is reachable), an edge, and two off-axis
%% diagonals so no match is decided purely along a cardinal direction.
%%
%% Each entry is {AX, AY, AHeading, BX, BY, BHeading} in WHOLE units and binary
%% angles. Headings face the two tanks roughly at each other, so a match does not
%% begin with both bots hunting empty arena.
-spec starts() -> [{integer(), integer(), integer(), integer(), integer(), integer()}].
starts() ->
    [{100, 300, 0, 700, 300, 128},      %% long, facing, horizontal
     {330, 300, 0, 470, 300, 128},      %% short, facing
     {60, 60, 32, 740, 540, 160},       %% corner to corner, diagonal
     {400, 60, 64, 400, 540, 192},      %% vertical, facing
     {120, 480, 224, 680, 120, 96},     %% anti-diagonal
     {400, 300, 0, 700, 500, 160}].     %% centre versus off-axis

%%==============================================================================
%% Running
%%==============================================================================

%% One match: KindA in seat a, KindB in seat b, at one start.
-spec match(robo_gauntlet:kind(), robo_gauntlet:kind(), tuple()) ->
          #{a := outcome(), b := outcome(), turns := non_neg_integer()}.
match(KindA, KindB, {AX, AY, AH, BX, BY, BH}) ->
    Arena = robo_sim:new([{a, AX, AY, AH}, {b, BX, BY, BH}]),
    States = [{a, robo_gauntlet:init(KindA)}, {b, robo_gauntlet:init(KindB)}],
    run(Arena, [{a, KindA}, {b, KindB}], States, #{}).

%% Both seats at one start, which is what a fair pairing means when the start
%% geometry is not symmetric.
-spec duel(robo_gauntlet:kind(), robo_gauntlet:kind(), tuple()) -> [map()].
duel(KindA, KindB, Start) -> [match(KindA, KindB, Start), match(KindB, KindA, Start)].

%% The whole round robin over the whole ensemble, both seats. One row per kind:
%% {Kind, TotalDamage, TotalSurvivalTurns}, accumulated over every match played.
-spec field() -> [{robo_gauntlet:kind(), integer(), non_neg_integer()}].
field() ->
    [row(K, score_of(K, starts())) || K <- robo_gauntlet:kinds()].

row(K, {D, S}) -> {K, D, S}.

%% The ranked ladder, strongest first. Damage decides; survival breaks ties.
-spec rank() -> [robo_gauntlet:kind()].
rank() ->
    Scored = [{score_of(K, starts()), K} || K <- robo_gauntlet:kinds()],
    [K || {_S, K} <- lists:reverse(lists:sort(Scored))].

%% A kind's total score against the whole field over the given starts, as
%% {Damage, SurvivalTurns}. Ordered lexicographically by Erlang's own term
%% order, which is why no weighting constant is needed.
-spec score_of(robo_gauntlet:kind(), [tuple()]) -> {integer(), non_neg_integer()}.
score_of(Kind, Starts) ->
    Others = [K || K <- robo_gauntlet:kinds(), K =/= Kind],
    Outcomes = [seat(Kind, Other, Start) || Other <- Others, Start <- Starts],
    lists:foldl(fun({D, S}, {AD, AS}) -> {AD + D, AS + S} end, {0, 0},
                lists:flatten(Outcomes)).

%% Kind plays Other at Start in both seats; return both of Kind's outcomes.
seat(Kind, Other, Start) ->
    #{a := A} = match(Kind, Other, Start),
    #{b := B} = match(Other, Kind, Start),
    [{maps:get(damage, A), maps:get(survived, A)},
     {maps:get(damage, B), maps:get(survived, B)}].

%%==============================================================================
%% The loop
%%==============================================================================

%% THE PERCEPTION CONTRACT LIVES HERE. Bots act on Arena, whose scans were
%% produced by the step that produced the tanks in Arena, and only then is the
%% arena stepped. Reversing those two lines is the silent bug this module exists
%% to prevent.
-spec run(#arena{}, [{term(), robo_gauntlet:kind()}], [{term(), term()}], map()) -> map().
run(Arena, Kinds, States, Deaths) ->
    run_step(Arena, Kinds, States, note_deaths(Arena, Deaths), robo_sim:finished(Arena)).

run_step(Arena, _Kinds, _States, Deaths, true) -> report(Arena, Deaths);
run_step(Arena, Kinds, States, Deaths, false) ->
    Live = robo_sim:alive(Arena),
    Acted = [act_one(T, Kinds, States, Arena) || T <- Live],
    Intents = [{Id, I} || {Id, I, _S} <- Acted],
    States2 = merge_states(Acted, States),
    run(robo_sim:step(Arena, Intents), Kinds, States2, Deaths).

act_one(#tank{id = Id} = T, Kinds, States, Arena) ->
    {Id, Kind} = lists:keyfind(Id, 1, Kinds),
    {Id, S} = lists:keyfind(Id, 1, States),
    {Intent, S2} = robo_gauntlet:act(Kind, S, T, Arena),
    {Id, Intent, S2}.

merge_states(Acted, States) ->
    lists:foldl(fun({Id, _I, S}, Acc) -> lists:keystore(Id, 1, Acc, {Id, S}) end,
                States, Acted).

%% Record the turn each tank died on, once. A tank still alive at the end scores
%% the full match length, which is what makes survival a usable tiebreak rather
%% than a boolean.
note_deaths(#arena{turn = Turn, tanks = Ts}, Deaths) ->
    lists:foldl(fun(T, Acc) -> note_one(T, Turn, Acc) end, Deaths, Ts).

note_one(#tank{id = Id, dead = true}, Turn, Deaths) ->
    maps:put(Id, maps:get(Id, Deaths, Turn), Deaths);
note_one(_T, _Turn, Deaths) -> Deaths.

report(#arena{turn = Turn, tanks = Ts}, Deaths) ->
    #{a => outcome(lists:keyfind(a, #tank.id, Ts), Turn, Deaths),
      b => outcome(lists:keyfind(b, #tank.id, Ts), Turn, Deaths),
      turns => Turn}.

outcome(#tank{id = Id, dead = Dead, damage_dealt = D}, Turn, Deaths) ->
    #{damage => D div ?FP,
      survived => maps:get(Id, Deaths, Turn),
      alive => not Dead}.
