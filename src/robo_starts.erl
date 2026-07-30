%% @doc Robo Rumble: where a match begins. The start set is a rule of the game.
%%
%% THIS EXISTS SO TWO HOSTS AGREE ON WHAT A FIGHT WAS. Two machines running the
%% same tanks from different starting positions produce results that cannot be
%% compared, so a start set is not a test fixture, it is part of the contract a
%% visitor and a host both submit to.
%%
%% WHY IT WAS NOT HERE. This generator was written inside the phase 0 experiment
%% runner in faber-programmes and stayed there, the same way the controller did.
%% The engine meanwhile exported robo_match:starts/0, SIX hand-written starts,
%% which the research superseded and which nothing updated. So the library shipped
%% one start set while every measured result on this front used a different one.
%% A host building a match from the library would have used the wrong geometry and
%% produced numbers comparable to nothing. Those six are kept, unchanged, for the
%% scripted-ladder work that used them; this is the set the science ran on.
%%
%% THE HEADING OFFSET IS THE PART THAT MATTERS, AND IT WAS MEASURED RATHER THAN
%% REASONED. Under an all-mutually-facing generator the floor bot against its own
%% clone drew 106 of 160 with 70 percent of matches hitting the turn cap. With the
%% per-index offset the same 80 geometries give 67 wins, 67 losses and 26 draws.
%% Facing tanks exactly at each other manufactures stalemates, and it would have
%% left the phase 0 endpoint 70 percent censored.
%%
%% It kills a second degeneracy too. With at least 8 angle units of offset on
%% every start, no tank is ever bore-sighted at turn 1, so "fire straight ahead
%% immediately" cannot clear the first rung by itself.
%%
%% Because the offset is ONE RULE applied to all 120 indices, the splits remain
%% exchangeable draws from a single distribution rather than three differently
%% shaped populations.
%%
%% INTEGER ONLY. No rand, no libm, no clock. A start is a pure function of its
%% index, so a start set is reproducible from nothing but this module.
-module(robo_starts).

-export([all/0, count/0, at/1, split/1, angle_of/1]).

-define(COUNT, 120).

%% Whole units. robo_sim:new/1 takes whole units and applies its own fixed-point
%% conversion, which is the unit confusion that put an earlier placement 256x
%% outside the arena.
-define(MIN_SEPARATION, 150).
-define(WALK_CAP, 64).

-type start() :: {integer(), integer(), 0..255, integer(), integer(), 0..255}.
-export_type([start/0]).

%%==============================================================================
%% The set
%%==============================================================================

-spec all() -> [start()].
all() -> [at(I) || I <- lists:seq(1, ?COUNT)].

-spec count() -> pos_integer().
count() -> ?COUNT.

-spec at(pos_integer()) -> start().
at(I) ->
    AX = 60 + (I * 137) rem 681,
    AY = 60 + (I * 191) rem 481,
    {BX, BY} = separate(I, AX, AY, 0),
    Face = robo_gauntlet:angle_of({BX - AX, BY - AY}),
    {AX, AY, robo_sim:wrap(Face + off(I)),
     BX, BY, robo_sim:wrap(Face + 128 + off(I div 8 + 3))}.

%% B is walked by a fixed stride until the pair is at least MIN_SEPARATION apart,
%% so no match opens inside ram range. The walk IS BOUNDED, with a deterministic
%% reflection fallback, so it cannot fail to terminate on some future index.
separate(_I, AX, AY, K) when K > ?WALK_CAP -> {800 - AX, 600 - AY};
separate(I, AX, AY, K) ->
    BX = 60 + ((I * 251) + K * 97) rem 681,
    BY = 60 + ((I * 313) + K * 89) rem 481,
    far_enough(I, AX, AY, K, BX, BY,
               robo_sim:dist({AX, AY}, {BX, BY}) >= ?MIN_SEPARATION).

far_enough(_I, _AX, _AY, _K, BX, BY, true) -> {BX, BY};
far_enough(I, AX, AY, K, _BX, _BY, false) -> separate(I, AX, AY, K + 1).

off(I) -> element(1 + (I rem 8), {-96, -64, -32, -8, 8, 32, 64, 96}).

%%==============================================================================
%% The splits
%%
%% THESE ARE A RULE OF THE GAME, NOT AN EXPERIMENT'S PREFERENCE, and that is why
%% they travel with the generator. Phase 0's held-out endpoint is defined on
%% indices 7 to 86, so a later result claiming to be comparable with it has to use
%% the same 80. Indices 117 to 120 are generated and deliberately unused.
%%==============================================================================

-spec split(train | heldout | calibration) -> [start()].
split(train) -> lists:sublist(all(), 1, 6);
split(heldout) -> lists:sublist(all(), 7, 80);
split(calibration) -> lists:sublist(all(), 87, 30).

%%==============================================================================
%% Geometry
%%
%% THE BEARING COMES FROM robo_gauntlet:angle_of/1 AND IS NOT REIMPLEMENTED HERE.
%% The first version of this module carried its own bisection routine, which is
%% the copy-against-a-copy hazard: two implementations of the same rule drift, and
%% the drift would show up as a start set that no longer matches the one every
%% phase 0 number was measured on. The experiment runner calls that function, so
%% this calls the same function.
%%
%% It recovers a binary angle by bisection on the sign of a cross product, with NO
%% atan2 anywhere, because atan2 is libm and libm is not bit-identical across libc
%% versions. A match would then replay differently on two honest boxes, which is
%% worse than cheating because nobody could tell.
%%==============================================================================

-spec angle_of({integer(), integer()}) -> 0..255.
angle_of(V) -> robo_gauntlet:angle_of(V).
