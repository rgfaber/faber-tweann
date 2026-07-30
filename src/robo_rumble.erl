%% @doc Robo Rumble: a battle between N entrants, on one machine, publishable.
%%
%% THIS EXISTS SO A TANK CAN FIGHT ON A STRANGER'S MACHINE. A leaf node accepts
%% entrants, runs ONE battle, and publishes what happened. Nothing recomputes it
%% and no second machine has to agree, because only one ever runs the fight.
%%
%% BUILD, NOT CLAIM. Tests and a commit, no pre-registration and no gate.
%%
%% THE ENGINE WAS ALREADY N-TANK AND NOBODY HAD NOTICED. robo_sim:new/1 takes a
%% list of any length, robo_sim:finished/1 is "one or fewer still alive", which is
%% last-tank-standing, and robo_match's loop already acts on every live tank. What
%% was duel-shaped was the SCORING: robo_match:report/2 reads exactly two tanks
%% named a and b, and its act_one/4 assumes every entrant is a robo_gauntlet kind.
%% So this module is a new head on an existing loop, not a new engine.
%%
%% TWO KINDS OF ENTRANT, because a battle royale full of strangers is not the
%% first thing anyone will run. A house bot makes a one-tank test meaningful and
%% gives a visiting genome something to be measured against.
%%
%%     {genome, robo_genome:genome()}   driven by robo_pilot
%%     {script, robo_gauntlet:kind()}   driven by robo_gauntlet
%%
%% EVERY FOREIGN GENOME IS VALIDATED BEFORE THE BATTLE STARTS, never during. A
%% genome that fails validation makes the whole battle refuse rather than one
%% entrant silently misbehave, because robo_net:fit/2 pads a wrong-width input
%% layer in silence and robo_pilot falls back to a null intent on a short output
%% vector. A mismatched genome therefore does not crash: it fights badly and
%% produces a result that looks real.
%%
%% PLACEMENT IS A RULE OF THE GAME AND IT IS HERE. Entrants are spaced evenly on
%% a circle and face the centre, so no entrant gets a better seat than another and
%% adding one more competitor does not quietly change the geometry for everybody.
%% The circle is derived from the arena rather than chosen, and it is INTEGER, so
%% a placement is reproducible from the entrant count alone.
%%
%% WHAT A RESULT CARRIES, and it is chosen so a battle can be replayed from it
%% rather than streamed. The genome ids, the placement, the engine fingerprint and
%% the outcome. A spectator does not receive frames; they receive this and
%% regenerate the fight locally.
-module(robo_rumble).

-include("robo_sim.hrl").

-export([battle/1, battle/2, place/1, engine_id/0]).

-define(FP, 256).

-type entrant() :: {term(), {genome, robo_genome:genome()} | {script, atom()}}.
-export_type([entrant/0]).

%%==============================================================================
%% The battle
%%==============================================================================

-spec battle([entrant()]) -> {ok, map()} | {error, term()}.
battle(Entrants) -> battle(Entrants, #{}).

%% Options today are only the placement radius, kept so a caller can reproduce a
%% published placement exactly rather than trusting this module's default to be
%% stable forever.
-spec battle([entrant()], map()) -> {ok, map()} | {error, term()}.
battle(Entrants, Opts) when is_list(Entrants) ->
    start(Entrants, Opts, admit(Entrants));
battle(_Entrants, _Opts) -> {error, entrants_not_a_list}.

start(_Entrants, _Opts, {error, _} = E) -> E;
start(Entrants, Opts, ok) ->
    Ids = [Id || {Id, _} <- Entrants],
    Placed = place(length(Entrants), maps:get(radius, Opts, default_radius())),
    Arena = robo_sim:new([{Id, X, Y, H} || {Id, {X, Y, H}} <- lists:zip(Ids, Placed)]),
    States = [{Id, init_one(Spec)} || {Id, Spec} <- Entrants],
    Result = loop(Arena, Entrants, States, #{}),
    {ok, Result#{entrants => manifest(Entrants, Placed),
                 engine => engine_id(),
                 placement_radius => maps:get(radius, Opts, default_radius())}}.

%% ADMISSION. Every reason a battle refuses to start, checked before a turn is
%% simulated. A duplicate id is in here because the arena keys tanks by id and two
%% entrants sharing one would silently become one tank.
admit(Entrants) ->
    admit_count(length(Entrants), Entrants).

admit_count(N, _E) when N < 2 -> {error, {too_few_entrants, N}};
admit_count(N, E) -> admit_ids(N, E, lists:usort([Id || {Id, _} <- E])).

admit_ids(N, E, Uniq) when length(Uniq) =:= N -> admit_specs(E);
admit_ids(_N, _E, _Uniq) -> {error, duplicate_entrant_id}.

admit_specs([]) -> ok;
admit_specs([{Id, Spec} | Rest]) -> admit_one(Id, spec_ok(Spec), Rest).

admit_one(_Id, ok, Rest) -> admit_specs(Rest);
admit_one(Id, {error, Why}, _Rest) -> {error, {rejected, Id, Why}}.

spec_ok({genome, G}) -> spec_genome(robo_genome:validate(G));
spec_ok({script, Kind}) -> spec_script(lists:member(Kind, robo_gauntlet:kinds()), Kind);
spec_ok(_Other) -> {error, unknown_entrant_kind}.

spec_genome({ok, _G}) -> ok;
spec_genome({error, _} = E) -> E.

spec_script(true, _Kind) -> ok;
spec_script(false, Kind) -> {error, {unknown_script, Kind}}.

init_one({genome, _G}) -> {pilot, robo_pilot:init()};
init_one({script, Kind}) -> {script, robo_gauntlet:init(Kind)}.

%%==============================================================================
%% The loop
%%
%% THE PERCEPTION CONTRACT, carried over from robo_match unchanged and for the
%% same reason: every entrant acts on the arena whose scans were produced by the
%% step that produced its own tank, and the arena is stepped only afterwards.
%% Stepping first hands everyone a world one turn stale, silently, and nothing
%% fails.
%%==============================================================================

loop(Arena, Specs, States, Deaths) ->
    step(Arena, Specs, States, note_deaths(Arena, Deaths), robo_sim:finished(Arena)).

step(Arena, Specs, _States, Deaths, true) -> report(Arena, Specs, Deaths);
step(Arena, Specs, States, Deaths, false) ->
    Acted = [act_one(T, Specs, States, Arena) || T <- robo_sim:alive(Arena)],
    Intents = [{Id, I} || {Id, I, _S} <- Acted],
    loop(robo_sim:step(Arena, Intents), Specs, merge(Acted, States), Deaths).

act_one(#tank{id = Id} = T, Specs, States, Arena) ->
    {Id, Spec} = lists:keyfind(Id, 1, Specs),
    {Id, S} = lists:keyfind(Id, 1, States),
    {Intent, S2} = drive(Spec, S, T, Arena),
    {Id, Intent, S2}.

drive({genome, G}, {pilot, P}, T, Arena) ->
    {I, P2} = robo_pilot:act(G, P, T, Arena),
    {I, {pilot, P2}};
drive({script, Kind}, {script, S}, T, Arena) ->
    {I, S2} = robo_gauntlet:act(Kind, S, T, Arena),
    {I, {script, S2}}.

merge(Acted, States) ->
    lists:foldl(fun({Id, _I, S}, Acc) -> lists:keystore(Id, 1, Acc, {Id, S}) end,
                States, Acted).

%% The turn each tank died on, recorded once. A tank still alive at the end scores
%% the full battle length, which is what makes survival a usable tiebreak rather
%% than a boolean.
note_deaths(#arena{turn = Turn, tanks = Ts}, Deaths) ->
    lists:foldl(fun(T, Acc) -> note_one(T, Turn, Acc) end, Deaths, Ts).

note_one(#tank{id = Id, dead = true}, Turn, Deaths) ->
    maps:put(Id, maps:get(Id, Deaths, Turn), Deaths);
note_one(_T, _Turn, Deaths) -> Deaths.

%%==============================================================================
%% The result
%%==============================================================================

%% Every tank, not two named seats. THE SURVIVOR MAY BE NOBODY: a battle that
%% reaches the turn cap with several alive has no winner, and a battle where the
%% last two kill each other on the same turn has none either. Reporting a winner
%% in those cases would be inventing one, so `winner` is explicitly `none`.
report(#arena{turn = Turn, tanks = Ts} = A, Specs, Deaths) ->
    Standings = [outcome(T, Turn, Deaths, Specs) || T <- Ts],
    #{turns => Turn,
      standings => lists:sort(fun rank/2, Standings),
      winner => winner(robo_sim:alive(A)),
      trace => robo_sim:trace_hash(A)}.

outcome(#tank{id = Id, dead = Dead, damage_dealt = D}, Turn, Deaths, Specs) ->
    {Id, Spec} = lists:keyfind(Id, 1, Specs),
    #{id => Id,
      kind => kind_of(Spec),
      damage => D div ?FP,
      survived => maps:get(Id, Deaths, Turn),
      alive => not Dead}.

kind_of({script, Kind}) -> {script, Kind};
kind_of({genome, G}) -> {genome, robo_genome:id(G)}.

%% Survival first, damage breaks ties. Deliberately NOT damage-first: this is a
%% last-tank-standing battle, so outliving the field is the thing it is about.
rank(#{survived := S1, damage := D1}, #{survived := S2, damage := D2}) ->
    {S1, D1} >= {S2, D2}.

winner([#tank{id = Id}]) -> Id;
winner(_None_or_many) -> none.

%%==============================================================================
%% Placement and provenance
%%==============================================================================

-spec place(pos_integer()) -> [{integer(), integer(), 0..255}].
place(N) -> place(N, default_radius()).

%% Evenly spaced on a circle about the arena centre, each facing inward. INTEGER
%% throughout, using the engine's own sine table, so a placement is reproducible
%% from the entrant count and radius alone and needs no float and no libm.
%%
%% Angles are binary: 256 is a full turn. Entrant I sits at I * 256 / N and faces
%% the centre, which is its own angle plus half a turn.
place(N, Radius) ->
    {W, H} = arena_units(),
    [point(I * 256 div N, Radius, W div 2, H div 2) || I <- lists:seq(0, N - 1)].

point(A, R, CX, CY) ->
    {CX + R * robo_sim:cos(A) div 32768,
     CY + R * robo_sim:sin(A) div 32768,
     robo_sim:wrap(A + 128)}.

%% THE ARENA IN WHOLE UNITS, AND THIS CONVERSION IS NOT OPTIONAL.
%% robo_sim:arena_size/0 reports FIXED POINT (204800 x 153600), while
%% robo_sim:new/1 takes WHOLE units and applies fp/1 to them itself. A placement
%% computed straight from the reported size is therefore 256 times too far out,
%% and every tank starts outside the arena.
%%
%% The first version of this module did exactly that. It was caught by a test
%% asserting placements land inside the arena, and by nothing else: the battle
%% still ran to completion and still produced a full result, because tanks that
%% begin outside simply pile into the bounds and fight there. Silently wrong
%% rather than broken, which is the failure mode this whole front is built to
%% avoid.
arena_units() ->
    {W, H} = robo_sim:arena_size(),
    {W div ?FP, H div ?FP}.

%% A quarter of the shorter arena dimension, so the ring fits with room to
%% manoeuvre at any entrant count and nobody starts inside a wall.
default_radius() ->
    {W, H} = arena_units(),
    min(W, H) div 4.

%% WHICH PHYSICS RAN. A result is meaningless without it: a rules change makes old
%% results incomparable rather than merely old, so this is what a season is keyed
%% on. Hashing the loaded modules' own beam checksums means it moves when the code
%% moves and cannot be forgotten at the point of a change.
%% HAND-ROLLED for the same reason robo_genome:pack/1 is: term_to_binary's bytes
%% are only guaranteed stable WITHIN an OTP release, so hashing its output would
%% make two honest hosts on different releases disagree about which physics they
%% are running. Here the bytes are a fixed concatenation with an explicit
%% separator, in a fixed module order, and nothing about the encoding is left to
%% the runtime.
-spec engine_id() -> binary().
engine_id() ->
    Mods = [robo_sim, robo_net, robo_gauntlet, robo_pilot, robo_rumble, robo_genome],
    crypto:hash(sha256,
                iolist_to_binary([[atom_to_binary(M, utf8), 0, module_md5(M), 0]
                                  || M <- Mods])).

%% module_info(md5) DIRECTLY, and not via the attributes list. The first version
%% looked the key up in attributes first and fell back to this, which is
%% wrong-shaped: a module that ever gained a literal -md5(...) attribute would
%% have that attribute's value, a list-wrapped term, shadow the real beam
%% checksum. Nothing would fail, and the engine fingerprint would quietly stop
%% naming the engine.
module_md5(M) ->
    _ = code:ensure_loaded(M),
    M:module_info(md5).

%% A battle's public manifest: who fought, as what, and from where.
manifest(Entrants, Placed) ->
    [#{id => Id, kind => kind_of(Spec), start => Start}
     || {{Id, Spec}, Start} <- lists:zip(Entrants, Placed)].
