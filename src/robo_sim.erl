%% @doc Robo Rumble: a deterministic 2D tank-arena engine (pure, process-free).
%%
%% The instrument for the Robo Rumble front (faber-ecosystem PLAN_ROBO_RUMBLE.md,
%% phase 0). Successor substrate to flatland_sim, chosen because it differs from the
%% exhausted pursuit-evasion grid on three axes: PARTIAL OBSERVABILITY (a radar that
%% sweeps, so you see only what you scan), an ENERGY ECONOMY WITH A REAL DECISION
%% (firing costs energy, hitting returns more than it cost), and a high-dimensional
%% action space (body, gun and radar steer independently).
%%
%% DETERMINISM IS THE POINT, and it is a day-one design decision rather than a
%% property to retrofit. Every quantity here is an INTEGER. No float, no libm, no
%% wall clock, no process, no randomness. A match is therefore a pure function of
%% its inputs and replays bit-identically on any machine, which is what makes a
%% result verifiable by recomputation. See the sine table note below.
%%
%% UNITS
%%   Distance/velocity/energy: fixed-point, scale 256 (8 fractional bits).
%%   Angles: BINARY ANGLE, 0..255 where 256 is a full turn (1 unit = 1.40625 deg).
%%           Wrap is a band, never a rem, so negative angles cost nothing.
%%   Bullet power: TENTHS, 1..30, meaning 0.1 .. 3.0.
%%   Sine: scaled by 32768.
%%
%% TURN ORDER is fixed and load-bearing (a reordering is a rules change, so it
%% changes the engine hash and starts a new season):
%%   1 rotate  2 accelerate and move and resolve walls  3 fire
%%   4 advance bullets and resolve hits  5 scan  6 cool guns
%% @end
-module(robo_sim).

-include("robo_sim.hrl").

%% Arena
-export([new/1, step/2, alive/1, finished/1, trace_hash/1]).
%% Units and trigonometry (pure, table-driven)
-export([fp/1, unfp/1, sin/1, cos/1, wrap/1, arena_size/0]).
%% Sensing helpers for controllers
-export([delta/2, dist/2, in_arc/4]).

-define(FP, 256).
-define(SIN_SCALE, 32768).
-define(ARENA_W, 800).
-define(ARENA_H, 600).
-define(MAX_VEL, 2048).          %% 8 * FP
-define(ACCEL, 256).             %% 1 * FP
-define(DECEL, 512).             %% 2 * FP
-define(TURN_BODY_MAX, 7).       %% ~10 deg
-define(TURN_GUN_MAX, 14).       %% ~20 deg
-define(TURN_RADAR_MAX, 32).     %% ~45 deg
-define(BEAM_MIN, 2).            %% radar always sees a sliver straight ahead
-define(TANK_R, 4608).           %% 18 * FP
-define(HIT_R, 5120).            %% 20 * FP, bullet-tank hit radius
-define(START_ENERGY, 25600).    %% 100 * FP
-define(GUN_COOL, 26).           %% ~0.1 * FP per turn
-define(RAM_DAMAGE, 154).        %% ~0.6 * FP
-define(WALL_DAMAGE, 256).       %% 1 * FP
-define(MAX_TURNS, 2000).

%% Sine for the 256 binary angles, scaled by 32768. GENERATED ONCE and checked in
%% as a literal on purpose: computing it at load would put libm back in the match
%% path and reintroduce cross-machine divergence, which is the exact hazard this
%% engine exists to avoid.
-define(SIN_TABLE, {
     0, 804, 1608, 2411, 3212, 4011, 4808, 5602,
     6393, 7180, 7962, 8740, 9512, 10279, 11039, 11793,
     12540, 13279, 14010, 14733, 15447, 16151, 16846, 17531,
     18205, 18868, 19520, 20160, 20788, 21403, 22006, 22595,
     23170, 23732, 24279, 24812, 25330, 25833, 26320, 26791,
     27246, 27684, 28106, 28511, 28899, 29269, 29622, 29957,
     30274, 30572, 30853, 31114, 31357, 31581, 31786, 31972,
     32138, 32286, 32413, 32522, 32610, 32679, 32729, 32758,
     32768, 32758, 32729, 32679, 32610, 32522, 32413, 32286,
     32138, 31972, 31786, 31581, 31357, 31114, 30853, 30572,
     30274, 29957, 29622, 29269, 28899, 28511, 28106, 27684,
     27246, 26791, 26320, 25833, 25330, 24812, 24279, 23732,
     23170, 22595, 22006, 21403, 20788, 20160, 19520, 18868,
     18205, 17531, 16846, 16151, 15447, 14733, 14010, 13279,
     12540, 11793, 11039, 10279, 9512, 8740, 7962, 7180,
     6393, 5602, 4808, 4011, 3212, 2411, 1608, 804,
     0, -804, -1608, -2411, -3212, -4011, -4808, -5602,
     -6393, -7180, -7962, -8740, -9512, -10279, -11039, -11793,
     -12540, -13279, -14010, -14733, -15447, -16151, -16846, -17531,
     -18205, -18868, -19520, -20160, -20788, -21403, -22006, -22595,
     -23170, -23732, -24279, -24812, -25330, -25833, -26320, -26791,
     -27246, -27684, -28106, -28511, -28899, -29269, -29622, -29957,
     -30274, -30572, -30853, -31114, -31357, -31581, -31786, -31972,
     -32138, -32286, -32413, -32522, -32610, -32679, -32729, -32758,
     -32768, -32758, -32729, -32679, -32610, -32522, -32413, -32286,
     -32138, -31972, -31786, -31581, -31357, -31114, -30853, -30572,
     -30274, -29957, -29622, -29269, -28899, -28511, -28106, -27684,
     -27246, -26791, -26320, -25833, -25330, -24812, -24279, -23732,
     -23170, -22595, -22006, -21403, -20788, -20160, -19520, -18868,
     -18205, -17531, -16846, -16151, -15447, -14733, -14010, -13279,
     -12540, -11793, -11039, -10279, -9512, -8740, -7962, -7180,
     -6393, -5602, -4808, -4011, -3212, -2411, -1608, -804
}).

%%==============================================================================
%% Units and trigonometry
%%==============================================================================

%% Whole units to fixed-point.
-spec fp(integer()) -> integer().
fp(N) -> N * ?FP.

%% Fixed-point to whole units. Erlang's div TRUNCATES TOWARD ZERO; it does not
%% floor. So unfp(-1) is 0, not -1. Stated precisely because both robo_net and
%% robo_gauntlet reason about this trap, and a reader calibrating on a wrong
%% comment here would mis-audit every div site in the match path. Not reachable
%% with a negative operand today, since positions are wall-clamped to TANK_R.
-spec unfp(integer()) -> integer().
unfp(N) -> N div ?FP.

%% Wrap a binary angle onto 0..255.
-spec wrap(integer()) -> 0..255.
wrap(A) -> A band 255.

-spec sin(integer()) -> integer().
sin(A) -> element(wrap(A) + 1, ?SIN_TABLE).

-spec cos(integer()) -> integer().
cos(A) -> sin(A + 64).

-spec arena_size() -> {integer(), integer()}.
arena_size() -> {fp(?ARENA_W), fp(?ARENA_H)}.

%%==============================================================================
%% Sensing helpers (for controllers; no atan2 anywhere, hence no libm)
%%==============================================================================

%% Signed fixed-point delta from A to B.
-spec delta(#tank{}, #tank{}) -> {integer(), integer()}.
delta(#tank{x = X1, y = Y1}, #tank{x = X2, y = Y2}) -> {X2 - X1, Y2 - Y1}.

%% Octagonal distance approximation: max + 3/8 * min. Integer only, and monotone
%% in true distance, which is all any controller or hit test needs here.
-spec dist({integer(), integer()}, {integer(), integer()}) -> integer().
dist({X1, Y1}, {X2, Y2}) ->
    DX = abs(X2 - X1),
    DY = abs(Y2 - Y1),
    max(DX, DY) + (3 * min(DX, DY)) div 8.

%% Does the direction {DX,DY} lie in the arc swept from angle A0 by signed D,
%% widened by BeamMin so a motionless radar still sees straight ahead? Decided by
%% cross products against the two edge directions, so no angle of the target is
%% ever computed.
-spec in_arc({integer(), integer()}, integer(), integer(), integer()) -> boolean().
in_arc({0, 0}, _A0, _D, _BeamMin) -> true;
in_arc({DX, DY}, A0, D, BeamMin) ->
    Lo = wrap(A0 - BeamMin + min(D, 0)),
    Hi = wrap(A0 + BeamMin + max(D, 0)),
    Span = wrap(Hi - Lo),
    Off = angle_offset({DX, DY}, Lo),
    Off =< Span.

%% How far the direction {DX,DY} sits counter-clockwise of angle From, in angle
%% units, found by bisection on cross-product sign. 8 steps, fully integer.
angle_offset(V, From) -> bisect(V, From, 0, 256, 8).

bisect(_V, _From, Lo, _Hi, 0) -> Lo;
bisect(V, From, Lo, Hi, N) ->
    Mid = (Lo + Hi) div 2,
    bisect_pick(V, From, Lo, Hi, Mid, N, side(V, From + Mid)).

bisect_pick(V, From, _Lo, Hi, Mid, N, true) -> bisect(V, From, Mid, Hi, N - 1);
bisect_pick(V, From, Lo, _Hi, Mid, N, false) -> bisect(V, From, Lo, Mid, N - 1).

%% Is {DX,DY} counter-clockwise of (or on) direction A? cross(dirA, V) >= 0.
side({DX, DY}, A) -> cos(A) * DY - sin(A) * DX >= 0.

%%==============================================================================
%% Arena
%%==============================================================================

%% Build a starting arena from [{Id, X, Y, Heading}] in WHOLE units.
-spec new([{term(), integer(), integer(), integer()}]) -> #arena{}.
new(Starts) ->
    Tanks = [#tank{id = Id, x = fp(X), y = fp(Y), heading = wrap(H), gun = wrap(H),
                   radar = wrap(H), energy = ?START_ENERGY} || {Id, X, Y, H} <- Starts],
    #arena{tanks = Tanks}.

-spec alive(#arena{}) -> [#tank{}].
alive(#arena{tanks = Ts}) -> [T || T <- Ts, not T#tank.dead].

-spec finished(#arena{}) -> boolean().
finished(#arena{turn = Turn}) when Turn >= ?MAX_TURNS -> true;
finished(A) -> length(alive(A)) =< 1.

%% Advance the arena one turn. Intents is [{Id, #intent{}}]; a tank with no intent
%% holds. This is THE function that must be bit-reproducible.
-spec step(#arena{}, [{term(), #intent{}}]) -> #arena{}.
step(#arena{tanks = Ts, bullets = Bs, turn = Turn}, Intents) ->
    Rotated = [rotate(T, intent_for(T, Intents)) || T <- Ts],
    Moved = [move(T, intent_for(T, Intents)) || T <- Rotated],
    Bumped = resolve_rams(Moved),
    {Fired, New} = fire_all(Bumped, Intents),
    {Advanced, Hit} = advance_bullets(Bs ++ New, Fired),
    Scans = scan_all(Hit, Ts),
    Cooled = [cool(T) || T <- Hit],
    #arena{turn = Turn + 1, tanks = Cooled, bullets = Advanced, scans = Scans}.

intent_for(#tank{id = Id}, Intents) ->
    case lists:keyfind(Id, 1, Intents) of
        {Id, I} -> I;
        false -> #intent{}
    end.

%%------------------------------------------------------------------ 1. rotate

rotate(#tank{dead = true} = T, _I) -> T;
rotate(T, #intent{turn_body = B, turn_gun = G, turn_radar = R}) ->
    Body = clamp(B, ?TURN_BODY_MAX),
    T#tank{heading = wrap(T#tank.heading + Body),
           gun = wrap(T#tank.gun + clamp(G, ?TURN_GUN_MAX)),
           radar = wrap(T#tank.radar + clamp(R, ?TURN_RADAR_MAX))}.

clamp(V, Max) -> max(-Max, min(Max, V)).

%%------------------------------------------- 2. accelerate, move, resolve walls

move(#tank{dead = true} = T, _I) -> T;
move(#tank{vel = V, heading = H, x = X, y = Y} = T, #intent{accel = A}) ->
    NewV = clamp(V + clamp(A, ?DECEL), ?MAX_VEL),
    NX = X + (NewV * cos(H)) div ?SIN_SCALE,
    NY = Y + (NewV * sin(H)) div ?SIN_SCALE,
    wall(T#tank{vel = NewV}, NX, NY).

wall(T, NX, NY) ->
    {W, H} = arena_size(),
    CX = min(max(NX, ?TANK_R), W - ?TANK_R),
    CY = min(max(NY, ?TANK_R), H - ?TANK_R),
    wall_damage(T#tank{x = CX, y = CY}, CX =:= NX andalso CY =:= NY).

wall_damage(T, true) -> T;
wall_damage(T, false) -> hurt(T#tank{vel = 0}, ?WALL_DAMAGE).

hurt(#tank{energy = E} = T, D) ->
    NewE = E - D,
    T#tank{energy = max(0, NewE), dead = T#tank.dead orelse NewE =< 0}.

%% Tanks that end the turn overlapping each other both take ram damage and stop.
resolve_rams(Ts) -> [ram_one(T, Ts) || T <- Ts].

ram_one(#tank{dead = true} = T, _Ts) -> T;
ram_one(T, Ts) ->
    Touching = [O || O <- Ts, O#tank.id =/= T#tank.id, not O#tank.dead,
                     dist({T#tank.x, T#tank.y}, {O#tank.x, O#tank.y}) < 2 * ?TANK_R],
    ram_apply(T, Touching).

ram_apply(T, []) -> T;
ram_apply(T, _) -> hurt(T#tank{vel = 0}, ?RAM_DAMAGE).

%%-------------------------------------------------------------------- 3. fire

fire_all(Ts, Intents) -> lists:foldl(fun(T, Acc) -> fire_one(T, Intents, Acc) end,
                                     {[], []}, Ts).

fire_one(T, Intents, {Ts, Bs}) ->
    #intent{fire = P} = intent_for(T, Intents),
    {T2, New} = maybe_fire(T, clamp_power(P)),
    {Ts ++ [T2], Bs ++ New}.

clamp_power(P) when P =< 0 -> 0;
clamp_power(P) -> min(30, max(1, P)).

maybe_fire(#tank{dead = true} = T, _P) -> {T, []};
maybe_fire(T, 0) -> {T, []};
maybe_fire(#tank{gun_heat = GH} = T, _P) when GH > 0 -> {T, []};
maybe_fire(#tank{energy = E} = T, P) when E =< P * ?FP div 10 -> {T, []};
maybe_fire(#tank{x = X, y = Y, gun = G} = T, P) ->
    Cost = P * ?FP div 10,
    Heat = ?FP + (?FP * P) div 50,
    {T#tank{energy = T#tank.energy - Cost, gun_heat = Heat},
     [#bullet{owner = T#tank.id, x = X, y = Y, heading = G, power = P}]}.

%%----------------------------------------------- 4. advance bullets, hits, walls

advance_bullets(Bs, Ts) -> lists:foldl(fun advance_one/2, {[], Ts}, Bs).

advance_one(#bullet{x = X, y = Y, heading = H, power = P} = B, {Keep, Ts}) ->
    Speed = fp(20) - (3 * ?FP * P) div 10,
    NX = X + (Speed * cos(H)) div ?SIN_SCALE,
    NY = Y + (Speed * sin(H)) div ?SIN_SCALE,
    resolve_bullet(B#bullet{x = NX, y = NY}, Keep, Ts).

resolve_bullet(B, Keep, Ts) ->
    case first_hit(B, Ts) of
        none -> {keep_if_inside(B, Keep), Ts};
        Victim -> {Keep, apply_hit(B, Victim, Ts)}
    end.

first_hit(#bullet{owner = O, x = X, y = Y}, Ts) ->
    Hits = [T || T <- Ts, T#tank.id =/= O, not T#tank.dead,
                 dist({X, Y}, {T#tank.x, T#tank.y}) < ?HIT_R],
    first_or_none(Hits).

first_or_none([]) -> none;
first_or_none([T | _]) -> T.

keep_if_inside(#bullet{x = X, y = Y} = B, Keep) ->
    {W, H} = arena_size(),
    keep_if(B, Keep, X >= 0 andalso X =< W andalso Y >= 0 andalso Y =< H).

keep_if(B, Keep, true) -> Keep ++ [B];
keep_if(_B, Keep, false) -> Keep.

apply_hit(#bullet{owner = O, power = P}, Victim, Ts) ->
    Damage = damage(P),
    Reward = (3 * ?FP * P) div 10,
    [hit_one(T, O, Victim#tank.id, Damage, Reward) || T <- Ts].

hit_one(#tank{id = Id} = T, _O, Id, Damage, _R) -> hurt(T, Damage);
hit_one(#tank{id = Id} = T, Id, _V, Damage, Reward) ->
    T#tank{energy = T#tank.energy + Reward,
           damage_dealt = T#tank.damage_dealt + Damage};
hit_one(T, _O, _V, _D, _R) -> T.

damage(P) when P =< 10 -> (4 * ?FP * P) div 10;
damage(P) -> (4 * ?FP * P) div 10 + (2 * ?FP * (P - 10)) div 10.

%%--------------------------------------------------------------------- 5. scan

%% A tank scans every live opponent whose direction fell in the arc its radar
%% swept this turn. This is the partial-observability mechanism: what a controller
%% does not sweep, it does not see.
scan_all(Ts, Prev) -> lists:flatten([scan_one(T, Ts, Prev) || T <- Ts]).

scan_one(#tank{dead = true}, _Ts, _Prev) -> [];
scan_one(#tank{id = Id, radar = R} = T, Ts, Prev) ->
    D = wrap_signed(R - prev_radar(Id, Prev, R)),
    [{Id, O#tank.id, dist({T#tank.x, T#tank.y}, {O#tank.x, O#tank.y}), delta(T, O)}
     || O <- Ts, O#tank.id =/= Id, not O#tank.dead,
        in_arc(delta(T, O), wrap(R - D), D, ?BEAM_MIN)].

prev_radar(Id, Prev, Default) ->
    case lists:keyfind(Id, #tank.id, Prev) of
        #tank{radar = R} -> R;
        false -> Default
    end.

%% Signed shortest difference, in -128..127.
wrap_signed(A) -> signed(wrap(A)).

signed(A) when A > 128 -> A - 256;
signed(A) -> A.

%%---------------------------------------------------------------- 6. cool guns

cool(#tank{gun_heat = GH} = T) -> T#tank{gun_heat = max(0, GH - ?GUN_COOL)}.

%%==============================================================================
%% Determinism harness
%%==============================================================================

%% A hash of the full arena state. The cross-machine CI job replays a fixed match
%% on two boxes and diffs this; any divergence is a determinism defect, and a
%% determinism defect makes every downstream verification claim false.
-spec trace_hash(#arena{}) -> binary().
trace_hash(#arena{turn = Turn, tanks = Ts, bullets = Bs}) ->
    Terms = [Turn,
             [{T#tank.id, T#tank.x, T#tank.y, T#tank.heading, T#tank.gun,
               T#tank.radar, T#tank.vel, T#tank.energy, T#tank.gun_heat,
               T#tank.dead, T#tank.damage_dealt} || T <- Ts],
             [{B#bullet.owner, B#bullet.x, B#bullet.y, B#bullet.heading,
               B#bullet.power} || B <- Bs]],
    crypto:hash(sha256, term_to_binary(Terms, [{minor_version, 2}])).
