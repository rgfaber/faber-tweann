%% @doc Instrument tests for the Robo Rumble arena (PLAN_ROBO_RUMBLE.md, phase 0).
%%
%% Two jobs. First, kill gate 0a: the arena must behave, and every assertion below
%% is a hand-computed expected value rather than a value read off a run, so a
%% physics regression fails loudly instead of silently rebaselining.
%%
%% Second, the DETERMINISM harness. GOLDEN_HASH is a checked-in replay vector. The
%% cross-machine CI job runs this suite on two different boxes and compares; a
%% divergence there is not a test failure, it is proof that verification-by-
%% recomputation is broken, which would invalidate every downstream claim about
%% results being checkable. That is why the vector is pinned in the repo.
%% @end
-module(robo_sim_tests).

-include_lib("eunit/include/eunit.hrl").
-include("robo_sim.hrl").

%% Pinned 2026-07-29 on the initial engine. Changing it is a RULES CHANGE and
%% therefore a new season: never edit it to make a test pass.
-define(GOLDEN_HASH,
        <<"1A0B8A6D740ADE5E2C97BC172F4CC84A814CD3CFDCC96AE55E050509C9079AEE">>).

%%==============================================================================
%% Units and trigonometry
%%==============================================================================

trig_identities_test() ->
    ?assertEqual(0, robo_sim:sin(0)),
    ?assertEqual(32768, robo_sim:cos(0)),
    ?assertEqual(32768, robo_sim:sin(64)),
    ?assertEqual(0, robo_sim:cos(64)),
    ?assertEqual(0, robo_sim:sin(128)),
    ?assertEqual(-32768, robo_sim:cos(128)).

%% Every angle must be in range and the table must be symmetric, which is what
%% lets cos/1 be a shifted sin/1 rather than a second table.
trig_table_wellformed_test() ->
    [?assert(abs(robo_sim:sin(A)) =< 32768) || A <- lists:seq(0, 255)],
    [?assertEqual(robo_sim:sin(A), -robo_sim:sin(A + 128)) || A <- lists:seq(0, 127)].

wrap_test() ->
    ?assertEqual(255, robo_sim:wrap(-1)),
    ?assertEqual(0, robo_sim:wrap(256)),
    ?assertEqual(44, robo_sim:wrap(300)),
    ?assertEqual(0, robo_sim:wrap(-256)).

%%==============================================================================
%% Kill gate 0a: the arena behaves
%%==============================================================================

%% Full throttle from rest accelerates 1 unit/turn to a cap of 8, so 20 turns
%% travel 1+2+..+8 then 12 turns at 8 = 36 + 96 = 132 whole units. Hand-computed.
acceleration_and_cap_test() ->
    A = drive(20),
    T = tank(a, A),
    ?assertEqual(2048, T#tank.vel),
    ?assertEqual(100 + 132, robo_sim:unfp(T#tank.x)),
    ?assertEqual(300, robo_sim:unfp(T#tank.y)).

%% A tank cannot leave the arena, and hitting the wall stops it.
wall_stops_tank_test() ->
    A = drive(400),
    T = tank(a, A),
    {W, _H} = robo_sim:arena_size(),
    ?assertEqual(0, T#tank.vel),
    ?assert(T#tank.x =< W),
    ?assertEqual(800 - 18, robo_sim:unfp(T#tank.x)).

%% Firing at power 3.0 costs exactly 3.0 energy and heats the gun to 1 + 3/5,
%% less one turn of cooling because firing precedes cooling in the turn order.
fire_costs_energy_and_heats_gun_test() ->
    A0 = robo_sim:new([{a, 100, 300, 0}, {b, 700, 300, 128}]),
    A1 = robo_sim:step(A0, [{a, #intent{fire = 30}}]),
    T = tank(a, A1),
    ?assertEqual(1, length(A1#arena.bullets)),
    ?assertEqual(25600 - 768, T#tank.energy),
    ?assertEqual(256 + 153 - 26, T#tank.gun_heat).

%% The gun cannot fire again while hot, so a repeated fire order yields one bullet.
gun_heat_gates_fire_test() ->
    A0 = robo_sim:new([{a, 100, 300, 0}, {b, 700, 300, 128}]),
    Fire = [{a, #intent{fire = 30}}],
    A = lists:foldl(fun(_, Acc) -> robo_sim:step(Acc, Fire) end, A0, lists:seq(1, 5)),
    ?assertEqual(1, length(A#arena.bullets)).

%% A power-3.0 hit deals 4*3 + 2*(3-1) = 16 energy and returns 3*3 = 9 to the
%% shooter. Both tanks are held still so nothing else can touch the numbers.
bullet_damage_and_reward_test() ->
    A0 = robo_sim:new([{a, 100, 300, 0}, {b, 400, 300, 128}]),
    A1 = robo_sim:step(A0, [{a, #intent{fire = 30}}]),
    A = lists:foldl(fun(_, Acc) -> robo_sim:step(Acc, []) end, A1, lists:seq(1, 40)),
    B = tank(b, A),
    Shooter = tank(a, A),
    ?assertEqual(25600 - 4096, B#tank.energy),
    ?assertEqual(4096, Shooter#tank.damage_dealt),
    ?assertEqual(25600 - 768 + 2304, Shooter#tank.energy).

%% Turn rates are clamped per part: body slowest, radar fastest.
turn_rates_are_clamped_test() ->
    A0 = robo_sim:new([{a, 400, 300, 0}]),
    A = robo_sim:step(A0, [{a, #intent{turn_body = 99, turn_gun = 99, turn_radar = 99}}]),
    T = tank(a, A),
    ?assertEqual(7, T#tank.heading),
    ?assertEqual(14, T#tank.gun),
    ?assertEqual(32, T#tank.radar).

%% Partial observability: a radar pointed away from the opponent sees nothing,
%% and one pointed at it does. This is the property that distinguishes this
%% substrate from the fully-observed grid the previous programme exhausted.
radar_is_directional_test() ->
    Away = robo_sim:new([{a, 100, 300, 0}, {b, 700, 300, 128}]),
    Blind = robo_sim:step(set_radar(Away, a, 128), []),
    ?assertEqual([], [S || {a, _, _, _} = S <- Blind#arena.scans]),
    Facing = robo_sim:step(set_radar(Away, a, 0), []),
    ?assertMatch([{a, b, _, _}], [S || {a, _, _, _} = S <- Facing#arena.scans]).

%% A round ends when one tank is left standing or the turn cap is reached.
finished_when_one_left_test() ->
    A = robo_sim:new([{a, 100, 300, 0}, {b, 700, 300, 128}]),
    ?assertNot(robo_sim:finished(A)),
    ?assertEqual(1, length(robo_sim:alive(kill(A, b)))),
    ?assert(robo_sim:finished(kill(A, b))).

%%==============================================================================
%% Determinism harness
%%==============================================================================

%% Same inputs, same bytes. If this ever fails on one machine it is a bug; if it
%% passes here and fails on another machine, the engine is not verifiable.
replay_is_bit_identical_test() ->
    ?assertEqual(replay(), replay()).

golden_replay_vector_test() ->
    ?assertEqual(?GOLDEN_HASH, binary:encode_hex(replay())).

%%==============================================================================
%% Helpers
%%==============================================================================

replay() ->
    A0 = robo_sim:new([{a, 100, 300, 0}, {b, 700, 300, 128}]),
    Seq = [{a, #intent{accel = 256, turn_radar = 32, fire = 15}},
           {b, #intent{accel = 256, turn_gun = 5, fire = 20}}],
    A = lists:foldl(fun(_, Acc) -> robo_sim:step(Acc, Seq) end, A0, lists:seq(1, 300)),
    robo_sim:trace_hash(A).

drive(N) ->
    A0 = robo_sim:new([{a, 100, 300, 0}]),
    lists:foldl(fun(_, A) -> robo_sim:step(A, [{a, #intent{accel = 256}}]) end,
                A0, lists:seq(1, N)).

tank(Id, #arena{tanks = Ts}) -> lists:keyfind(Id, #tank.id, Ts).

set_radar(#arena{tanks = Ts} = A, Id, R) ->
    T = lists:keyfind(Id, #tank.id, Ts),
    A#arena{tanks = lists:keyreplace(Id, #tank.id, Ts, T#tank{radar = R})}.

kill(#arena{tanks = Ts} = A, Id) ->
    T = lists:keyfind(Id, #tank.id, Ts),
    A#arena{tanks = lists:keyreplace(Id, #tank.id, Ts, T#tank{dead = true})}.
