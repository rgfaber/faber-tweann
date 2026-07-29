%% @doc Robo Rumble: the five scripted opponents that fix the competence floor.
%%
%% This module is a KILL GATE, not a convenience. PLAN_ROBO_RUMBLE.md section 2
%% states the reason plainly: absence of cycling is uninterpretable unless the
%% population has crossed a competence floor at which counter-strategies are
%% possible at all. At low competence a tank arena is transitive, so a no-cycling
%% result below the floor is a statement about the optimiser rather than about
%% the substrate, which is exactly the search under-convergence confound that the
%% closed programme left unresolved. These five bots ARE the floor. If they are
%% wrong, every later null result on this front means nothing.
%%
%% THE FIVE, weakest first, matching the plan's table:
%%
%%   sitting_duck    does nothing at all. Engine smoke test. Anything beats it.
%%   spinner         rotates constantly, drives in a circle, fires on a fixed
%%                   period. Deterministic, never random. Tests basic tracking
%%                   of a target that moves.
%%   rammer          sweeps radar to acquire, then drives at the contact and
%%                   shoots at close range. Punishes passivity and wall-hugging.
%%   circle_strafer  holds the contact at a roughly constant range while
%%                   orbiting it, aiming straight at where the contact WAS.
%%                   Punishes non-predictive aim.
%%   predictive_gun  THE FLOOR. Same orbiting movement as the strafer, but the
%%                   gun estimates the contact's velocity from successive scans
%%                   and leads the shot by the bullet's flight time. Beating it
%%                   requires genuine evasion, not merely being in motion.
%%
%% PARTIAL OBSERVABILITY IS ENFORCED BY STRUCTURE, NOT BY GOOD INTENTIONS. A bot
%% here may read its own tank record and the scans addressed to it, and nothing
%% else. act/4 is the only function that touches the arena, it destructures only
%% the scans field, it filters that list down to the entries whose observer id is
%% the acting tank, and it then calls decide/3, which receives the bot state and
%% the bot's OWN tank and has no way to reach an opponent record. Reading the
%% opponent's tank directly would silently delete the property that this whole
%% substrate was chosen for, and it would do so without failing any test, so the
%% defence is a shape the compiler can check rather than a rule in a comment.
%%
%% DETERMINISM. Every quantity below is an integer. There is no float, no libm
%% call, no clock, no randomness, no ets, no process dictionary and no message
%% passing anywhere in this module. State is carried by value through act/4, so a
%% bot is a pure function of its history and a match replays bit-identically on
%% any machine. See robo_sim for the unit scheme, which this module obeys without
%% exception: distance and velocity in fixed-point at scale 256, angles as binary
%% angles on 0 to 255 with 256 a full turn, bullet power in tenths on 1 to 30.
%%
%% NO ATAN2, AND NONE ADDED. A gun needs the direction to a contact as a binary
%% angle, but the scans deliberately carry a vector and never a bearing, because
%% a bearing means atan2 and atan2 means libm. angle_of/1 below recovers the
%% angle by bisection on the sign of a cross product, which is the same technique
%% robo_sim uses in in_arc/4. robo_sim keeps that machinery private, so it is
%% reimplemented here rather than exported from there, on the principle that a
%% gauntlet should not force a change to the engine it is meant to test.
%%
%% ONE CONVENTION FOR AIMING. A bot commands the raw signed rotation it wants and
%% lets the engine clamp it, because the engine clamps in exactly one place and
%% duplicating those limits here would let the two drift apart. What a bot must
%% therefore reason about is the RESIDUAL: the part of the wanted rotation that
%% the clamp will refuse this turn. residual/1 computes it, and a bot holds fire
%% until the residual is small, so it never spends energy on a shot the gun has
%% not finished turning onto.
%%
%% THE ENERGY ECONOMY IS A REAL CONSTRAINT AND THE BOTS RESPECT IT. The plan
%% counts firing cost against hit reward as one of the three material differences
%% from the exhausted grid, and the arithmetic bites: a shot costs a tenth of its
%% power and a hit returns three tenths, so a bot must land better than one shot
%% in three merely to break even. A bot that fires whenever its gun is cool and
%% roughly on target burns its whole energy bar inside the turn cap and loses to
%% a target that does nothing at all, which was measured, not imagined. Every gun
%% here is therefore budgeted by budget/2, which never commits more than a
%% twentieth of remaining energy to one shot, so power decays as the bar does and
%% no bot can shoot itself to death.
%%
%% WALLS ARE THE OTHER SILENT KILLER. Wall contact costs a full unit of energy per
%% turn and zeroes velocity, so a bot that orbits a contact parked near an edge
%% will grind itself flat while its gun works perfectly. The two orbiting bots
%% bend their heading away from a nearby wall, and they do it in vector space
%% rather than by adding angles, because angles do not blend under addition. This
%% is not the rammer's business: punishing wall-hugging is what the rammer is
%% for, and a rammer that flinched from walls would not be one.
%% @end
-module(robo_gauntlet).

-include("robo_sim.hrl").

%% The gauntlet
-export([kinds/0, init/1, act/4]).
%% Exposed for the kill gate: the one piece of non-obvious arithmetic here is
%% the atan2-free vector to angle conversion, so it is testable on its own.
-export([angle_of/1]).

-export_type([kind/0, state/0]).

-define(FP, 256).              %% arena fixed-point scale, mirrors robo_sim
-define(FULL, 256).            %% one unit per turn of acceleration
-define(SWEEP, 32).            %% full radar rate: a revolution every 8 turns
-define(WOBBLE, 4).            %% lock oscillation, keeps the beam crossing
-define(BODY_MAX, 7).          %% body clamp, for the spinner's constant turn
-define(GUN_MAX, 14).          %% gun clamp, needed to compute the residual
-define(ORBIT, 51200).         %% 200 whole units: the range a strafer holds
-define(RANGE_GAIN, 1024).     %% fixed-point of range error per angle unit
-define(RANGE_MAX, 48).        %% how far off tangent a range correction may pull
-define(WALL_MARGIN, 30720).   %% 120 whole units: how near a wall counts as near
-define(WALL_PUSH, 49152).     %% inward push, one and a half times the sine scale
-define(LOST, 16).             %% turns of silence after which a bot re-acquires
-define(STALE, 3).             %% turns of silence after which a lock is dropped
-define(MAX_LEAD, 60).         %% cap on predicted flight time, in turns
-define(VEL_GAP, 8).           %% widest scan gap a velocity estimate trusts
-define(AIM_TOL, 2).           %% residual the gun may carry and still fire
-define(SHOT_BUDGET, 512).     %% fixed-point energy per tenth of power committed
-define(FIRE_FLOOR, 2560).     %% 10 whole units of energy kept back from the gun
-define(HIT_R, 5120).          %% 20 whole units: the engine's bullet hit radius
-define(SIN_SCALE, 32768).     %% the engine's sine scale, for vector arithmetic

%% What a bot remembers between turns. All integer, carried by value.
%%
%% Two contacts are kept, not one, because a velocity estimate is a difference of
%% successive positions and there is nothing else in this substrate to difference.
%% The tick of each contact is stored alongside it so the gap between them is
%% known: scans arrive only when the radar happens to sweep the contact, so
%% successive observations are NOT one turn apart in general and dividing by a
%% presumed gap of one would report a velocity several times too large.
-record(bot, {
    tick = 0 :: non_neg_integer(),   %% turns this bot has acted
    seen = 0 :: 0..2,                %% contacts held: 0 none, 1 position, 2 also velocity
    age = 0 :: non_neg_integer(),    %% turns since the latest contact
    dist = 0 :: integer(),           %% range at the latest contact, fixed-point
    ex = 0 :: integer(),             %% latest contact position, fixed-point
    ey = 0 :: integer(),
    etick = 0 :: non_neg_integer(),  %% tick of the latest contact
    px = 0 :: integer(),             %% previous contact position, fixed-point
    py = 0 :: integer(),
    ptick = 0 :: non_neg_integer(),  %% tick of the previous contact
    target = none :: term()          %% id of the tracked contact; see track/5
}).

-type kind() :: sitting_duck | spinner | rammer | circle_strafer | predictive_gun.
-type state() :: #bot{}.

%%==============================================================================
%% The gauntlet
%%==============================================================================

%% The five, in the intended difficulty order. Kill gate 0a asserts that a round
%% robin ranks them in this order; the list is the pre-registered prediction, so
%% it is written down here once and read by the tests rather than restated there.
-spec kinds() -> [kind()].
kinds() -> [sitting_duck, spinner, rammer, circle_strafer, predictive_gun].

%% A fresh bot. Enumerated rather than catch-all so a misspelled kind fails at
%% once instead of quietly behaving like a sitting duck for a whole tournament.
-spec init(kind()) -> state().
init(sitting_duck) -> #bot{};
init(spinner) -> #bot{};
init(rammer) -> #bot{};
init(circle_strafer) -> #bot{};
init(predictive_gun) -> #bot{}.

%% One turn of a scripted bot.
%%
%% THIS IS THE PERCEPTION BOUNDARY. The arena is destructured to its scans field
%% and to nothing else, so the tanks and bullets lists are not in scope below
%% this line, and the scans are then narrowed to the entries this tank observed.
%% decide/3 receives only the bot's own tank, which is why no bot in this module
%% can read an opponent's record even by accident.
-spec act(kind(), state(), #tank{}, #arena{}) -> {#intent{}, state()}.
act(Kind, State, #tank{id = Id} = Me, #arena{scans = Scans}) ->
    Mine = [S || {Observer, _T, _D, _V} = S <- Scans, Observer =:= Id],
    Next = observe(State, Me, Mine),
    {decide(Kind, Next, Me), Next}.

%%==============================================================================
%% Perception
%%==============================================================================

%% Fold this turn's scans into memory. A turn with no scan ages the track, which
%% is the whole cost of pointing a radar at the wrong part of the arena.
observe(#bot{tick = T, age = A} = S, _Me, []) ->
    S#bot{tick = T + 1, age = A + 1};
observe(#bot{tick = T} = S, #tank{x = MX, y = MY}, Scans) ->
    {_O, Target, D, {DX, DY}} = nearest(Scans),
    track(S#bot{tick = T + 1, age = 0, dist = D}, MX + DX, MY + DY, T, Target).

%% Absolute contact position is own position plus the scan's delta. That is a
%% derivation from two things the bot is allowed to know, not a peek at the
%% opponent's record.
%% AUDIT FIX: the tracked contact's id is now carried, and switching contact
%% resets the history to a single sighting. Without that, enemy_vel/1 could
%% difference two DIFFERENT opponents' positions and feed the result straight
%% into the lead solver. Unreachable in a duel, so it costs nothing today, but
%% melee is listed as undecided in the plan and the day a third tank enters the
%% arena this would produce an arbitrary lead with no crash and no failing test.
track(#bot{target = Target} = S, X, Y, T, Target) ->
    #bot{ex = EX, ey = EY, etick = ET, seen = N} = S,
    S#bot{px = EX, py = EY, ptick = ET, ex = X, ey = Y, etick = T,
          seen = min(2, N + 1)};
track(S, X, Y, T, Target) ->
    S#bot{px = X, py = Y, ptick = T, ex = X, ey = Y, etick = T,
          seen = 1, target = Target}.

%% The closest contact wins. Strictly closer, so ties keep the earlier entry and
%% the choice does not depend on anything but list order, which the engine fixes.
nearest([H | Rest]) -> lists:foldl(fun closer/2, H, Rest).

closer({_O, _T, D, _V} = Candidate, {_BO, _BT, Best, _BV}) when D < Best -> Candidate;
closer(_Candidate, Best) -> Best.

%% Contact velocity in fixed-point per turn, by differencing two contacts.
%% Reported as standing still whenever the estimate would be untrustworthy:
%% before a second contact exists, and after a gap so long that the contact could
%% have turned, accelerated and stopped between the two sightings.
enemy_vel(#bot{seen = N}) when N < 2 -> {0, 0};
enemy_vel(#bot{etick = T, ptick = P}) when T =< P -> {0, 0};
enemy_vel(#bot{etick = T, ptick = P}) when T - P > ?VEL_GAP -> {0, 0};
enemy_vel(#bot{ex = X, ey = Y, px = PX, py = PY, etick = T, ptick = P}) ->
    Gap = T - P,
    {(X - PX) div Gap, (Y - PY) div Gap}.

%%==============================================================================
%% The five
%%==============================================================================

decide(sitting_duck, _S, _Me) -> #intent{};
decide(spinner, _S, Me) -> spin(Me);
decide(rammer, S, Me) -> ram(S, Me);
decide(circle_strafer, S, Me) -> strafe(S, Me);
decide(predictive_gun, S, Me) -> snipe(S, Me).

%%-------------------------------------------------------------- 1. sitting duck
%% Handled entirely by the decide/3 clause above: the default intent is all
%% zeros, so the duck does not even turn its radar. It exists so that a controller
%% which cannot beat it is known to be broken rather than merely weak.

%%------------------------------------------------------------------- 2. spinner
%% Constant rotation of body, gun and radar at full throttle, and NO GUN. It
%% ignores its own scans on purpose: it is an honest moving target, and the
%% constant body turn traces the tight circle that a linear-lead gun finds
%% awkward. That is the entire content of this rung, and the whole difference
%% from rung 1 is that this one moves.
%%
%% AUDIT FIX, and it is a deliberate change to the pre-registered spec. The
%% spinner used to carry a blind gun on a fixed period. Measured, that gun landed
%% 4 of 214 shots at a mean firing range of 428 units, so it was expected-NEGATIVE
%% at any fire rate: lowering the period changes the magnitude of the loss and
%% never its sign. The consequence was that rungs 1 and 2 inverted, with the
%% spinner finishing on 80 energy against a sitting duck's 96 having achieved
%% nothing, and the duck outright winning two matches when the spinner drove into
%% a wall. A rung that ranks below the bot which does literally nothing is not a
%% floor. Removing the gun makes the rung mean something precise instead: rung 1
%% does not move, rung 2 does, and a tracker must handle the difference.
%%
%% It also gets the wall term, for the same reason hunt/2 does.
spin(Me) -> spin_wall(Me, wall_push(Me)).

spin_wall(_Me, {0, 0}) ->
    #intent{turn_body = ?BODY_MAX, turn_gun = ?GUN_MAX, turn_radar = ?SWEEP,
            accel = ?FULL};
%% MEASURED, not reasoned. Braking while turning was tried first, on the theory
%% that a bot with a 47 unit turning circle needs to shed speed to come about
%% inside the margin. It made things WORSE, 4 solo deaths against 1: the engine
%% has no drag, so a sustained negative command does not stop the tank, it drives
%% it backwards along its heading and straight into the wall it was fleeing.
%% Coasting is the correct command here, and full throttle is better still
%% because it carries the bot out of the margin fastest once it has come about.
spin_wall(#tank{heading = H}, {PX, PY}) ->
    Want = angle_of({robo_sim:cos(H) + PX, robo_sim:sin(H) + PY}),
    #intent{turn_body = turn_to(Want, H), turn_gun = ?GUN_MAX,
            turn_radar = ?SWEEP, accel = ?FULL}.

%%-------------------------------------------------------------------- 3. rammer
%% Acquire, then drive straight at the contact with the gun bore-sighted along
%% the same line, and shoot only once close enough that a straight shot is likely
%% to connect. Holding fire at long range is deliberate: the rammer's threat is
%% the collision and the point-blank shot, so its energy belongs in the charge.
ram(#bot{seen = 0}, Me) -> hunt(Me);
ram(#bot{age = A}, Me) when A > ?LOST -> hunt(Me);
ram(S, Me) ->
    Aim = direct_aim(S, Me),
    #intent{turn_body = turn_to(Aim, Me#tank.heading),
            turn_gun = turn_to(Aim, Me#tank.gun),
            turn_radar = lock(S, Aim, Me#tank.radar),
            accel = ?FULL,
            fire = shoot(S, Me, Aim, ram_power(S#bot.dist, Me#tank.energy))}.

ram_power(D, E) -> budget(ram_range(D), E).

ram_range(D) when D < ?FP * 120 -> 30;
ram_range(D) when D < ?FP * 300 -> 15;
ram_range(_D) -> 0.

%%------------------------------------------------------------- 4. circle strafer
%% Steer along the tangent to the contact, pulled off tangent in proportion to
%% the range error, which holds the range near ORBIT without any control loop
%% worth the name. The gun aims straight at the last known position, with no
%% lead at all, which is precisely the weakness a predictive opponent exploits
%% and the reason this bot sits one rung below the floor.
strafe(#bot{seen = 0}, Me) -> hunt(Me);
strafe(#bot{age = A}, Me) when A > ?LOST -> hunt(Me);
strafe(S, Me) ->
    Aim = direct_aim(S, Me),
    #intent{turn_body = turn_to(steer(Aim, S#bot.dist, Me), Me#tank.heading),
            turn_gun = turn_to(Aim, Me#tank.gun),
            turn_radar = lock(S, Aim, Me#tank.radar),
            accel = ?FULL,
            fire = shoot(S, Me, Aim, power_for(S#bot.dist, Me#tank.energy))}.

%% Body heading for the two orbiting bots.
%%
%% The tangent is the contact direction plus a quarter turn, pulled off tangent
%% in proportion to the range error, which holds the range near ORBIT without any
%% control loop worth the name. That heading is then bent away from a nearby
%% wall, and the bend happens in VECTOR space rather than by adding angles,
%% because two headings do not blend by addition: the tangent is turned back into
%% a unit vector at the engine's own sine scale, the inward push is added to it,
%% and the angle is read off the sum. The push is half again as long as the
%% tangent, so the sum can never be the zero vector however the two oppose.
steer(Aim, Dist, Me) ->
    Tangent = robo_sim:wrap(Aim + 64 - range_error(Dist)),
    {PX, PY} = wall_push(Me),
    angle_of({robo_sim:cos(Tangent) + PX, robo_sim:sin(Tangent) + PY}).

range_error(Dist) -> clamp((Dist - ?ORBIT) div ?RANGE_GAIN, ?RANGE_MAX).

%% Own position against the arena bounds. The arena size is an engine constant,
%% not opponent state, so consulting it breaks no perception rule.
wall_push(#tank{x = X, y = Y}) ->
    {W, H} = robo_sim:arena_size(),
    {axis_push(X, W), axis_push(Y, H)}.

axis_push(V, _Limit) when V < ?WALL_MARGIN -> ?WALL_PUSH;
axis_push(V, Limit) when V > Limit - ?WALL_MARGIN -> -?WALL_PUSH;
axis_push(_V, _Limit) -> 0.

%%------------------------------------------------------------ 5. predictive gun
%% THE FLOOR. Movement is the strafer's, unchanged and deliberately so: the two
%% bots differ in exactly one thing, the gun, so a controller that beats the
%% strafer and loses to this one has been beaten by prediction and not by some
%% incidental difference in how the opponent drives.
%%
%% The gun solves for where the contact will be when the bullet arrives. Flight
%% time depends on range, range depends on where the contact will be, and that
%% depends on flight time, so settle/5 iterates the fixed point twice from a
%% standing start. Two passes are enough at these speeds and, being a fixed
%% count, cost the same on every turn and can never fail to terminate.
%%
%% The radar keeps tracking the contact's CURRENT direction, not the lead point.
%% Pointing the radar where the bullet is going would lose the track.
snipe(#bot{seen = 0}, Me) -> hunt(Me);
snipe(#bot{age = A}, Me) when A > ?LOST -> hunt(Me);
snipe(S, Me) ->
    Power = power_for(S#bot.dist, Me#tank.energy),
    Track = direct_aim(S, Me),
    Aim = lead_aim(S, Me, Power),
    #intent{turn_body = turn_to(steer(Track, S#bot.dist, Me), Me#tank.heading),
            turn_gun = turn_to(Aim, Me#tank.gun),
            turn_radar = lock(S, Track, Me#tank.radar),
            accel = ?FULL,
            fire = shoot(S, Me, Aim, Power)}.

%% AUDIT FIX, two of the three biases that were shaving accuracy off the floor.
%%
%% The shot is solved from the MUZZLE, not from where the tank is standing.
%% robo_sim:step/2 moves before it fires, so a bullet spawns at the shooter's
%% POST-move position, up to 8 units away and almost entirely sideways while
%% orbiting. Solving from the pre-move position aimed the gun from a place the
%% bullet never leaves.
lead_aim(S, Me, Power) ->
    {MX, MY} = muzzle(Me),
    Flight = settle(S, {MX, MY}, bullet_speed(Power), 0, 2),
    {PX, PY} = predict(S, Flight),
    angle_of({PX - MX, PY - MY}).

%% Where this tank's gun will be when the bullet spawns: one turn of its current
%% velocity along its current heading.
muzzle(#tank{x = X, y = Y, vel = V, heading = H}) ->
    {X + (V * robo_sim:cos(H)) div ?SIN_SCALE,
     Y + (V * robo_sim:sin(H)) div ?SIN_SCALE}.

%% The engine's bullet speed, restated: fp(20) minus 3/10 of a unit per tenth of
%% power. Always positive, since power never exceeds 30, so the division below
%% cannot be by zero.
bullet_speed(P) -> ?FP * 20 - (3 * ?FP * P) div 10.

%% AUDIT FIX, the third bias. Flight time is to CONTACT, not to the target's
%% centre: the engine scores a hit at dist < HIT_R, so a bullet stops one hit
%% radius short of the point being aimed at. Dividing the full centre range by
%% the bullet speed over-estimated the flight by exactly one turn at every range
%% and power measured, which at power 2.0 against a target at the speed cap is
%% 11.4 units of over-lead into a 20 unit window: over half the margin, spent
%% systematically.
settle(_S, _Pos, _Speed, Flight, 0) -> Flight;
settle(S, {MX, MY} = Pos, Speed, Flight, N) ->
    {PX, PY} = predict(S, Flight),
    Range = robo_sim:dist({MX, MY}, {PX, PY}),
    settle(S, Pos, Speed, min(?MAX_LEAD, max(0, Range - ?HIT_R) div Speed), N - 1).

%% Where the contact will be Flight turns from now, allowing for the track being
%% Age turns old already. Clamped to the arena, because a contact cannot leave it
%% and an unclamped extrapolation would aim at a wall the contact is about to
%% stop against.
predict(#bot{ex = X, ey = Y, age = A} = S, Flight) ->
    {VX, VY} = enemy_vel(S),
    Ahead = A + Flight,
    inside({X + VX * Ahead, Y + VY * Ahead}).

inside({X, Y}) ->
    {W, H} = robo_sim:arena_size(),
    {min(max(X, 0), W), min(max(Y, 0), H)}.

%%==============================================================================
%% Shared behaviour
%%==============================================================================

%% No contact, or a contact too old to act on: sweep the radar at full rate,
%% which covers the whole circle in eight turns, and keep moving on a slow curve.
%%
%% AUDIT FIX. This used to ignore walls entirely, and wall_push/1 was reachable
%% only from steer/3, that is only once a contact was held, so avoidance was
%% absent in exactly the state that has no target to look at. Measured alone in
%% an empty arena over 2000 turns, that cost the three tracking bots up to 1591
%% wall-contact turns and killed them outright from three of six starts, while
%% the sitting duck finished untouched on 100 energy. It also contaminated live
%% matches: the rammer lost to a gunless circler having dealt no damage at all,
%% so any fitness computed from those matches was grading wall avoidance rather
%% than marksmanship. The bend is the same vector-space blend steer/3 uses.
hunt(Me) -> hunt_wall(Me, wall_push(Me)).

hunt_wall(_Me, {0, 0}) ->
    #intent{turn_body = 2, turn_radar = ?SWEEP, accel = ?FULL};
hunt_wall(#tank{heading = H}, {PX, PY}) ->
    Want = angle_of({robo_sim:cos(H) + PX, robo_sim:sin(H) + PY}),
    #intent{turn_body = turn_to(Want, H), turn_radar = ?SWEEP, accel = ?FULL}.

%% Direction to the last known contact position, as a binary angle.
direct_aim(#bot{ex = X, ey = Y}, #tank{x = MX, y = MY}) -> angle_of({X - MX, Y - MY}).

%% Radar command. A stale track is abandoned in favour of sweeping. A live track
%% is followed, with an alternating overshoot added so the beam CROSSES the
%% contact every turn instead of settling onto it: a radar that stops moving
%% sweeps only the two angle units the engine grants a motionless beam, and a
%% contact that drifts one unit further than that is lost by a bot that thought
%% it had a lock.
lock(#bot{age = A}, _Aim, _Radar) when A > ?STALE -> ?SWEEP;
lock(#bot{tick = T}, Aim, Radar) -> turn_to(Aim, Radar) + wobble(T).

wobble(T) when T band 1 =:= 0 -> ?WOBBLE;
wobble(_T) -> -?WOBBLE.

%% Firing decision. Four reasons to hold, checked in order: the track is not from
%% this turn, the gun is still hot, the energy reserve is spent, or the gun has
%% not finished turning onto the aim point.
shoot(#bot{age = A}, _Me, _Aim, _P) when A > 0 -> 0;
shoot(_S, #tank{gun_heat = GH}, _Aim, _P) when GH > 0 -> 0;
shoot(_S, #tank{energy = E}, _Aim, _P) when E =< ?FIRE_FLOOR -> 0;
shoot(_S, #tank{gun = G}, Aim, P) -> aligned(residual(turn_to(Aim, G)), P).

aligned(R, P) when R >= -?AIM_TOL, R =< ?AIM_TOL -> P;
aligned(_R, _P) -> 0.

%% How much of a wanted gun rotation the engine's clamp will refuse this turn.
residual(D) -> D - clamp(D, ?GUN_MAX).

%% Shot power by range: heavy up close where it will connect and the damage bonus
%% is worth having, light at range where it mostly will not.
power_for(D, E) -> budget(range_power(D), E).

range_power(D) when D < ?FP * 150 -> 30;
range_power(D) when D < ?FP * 350 -> 20;
range_power(_D) -> 10.

%% Never commit more than a twentieth of the remaining bar to one shot. This is
%% what stops a gun that is missing from emptying itself: the power it can afford
%% decays with the energy it has left, so the spend is a decaying series and the
%% bar approaches the fire floor instead of crossing it.
budget(P, Energy) -> min(P, Energy div ?SHOT_BUDGET).

%% Signed shortest rotation from Current onto Target, in angle units.
turn_to(Target, Current) -> signed(robo_sim:wrap(Target - Current)).

signed(A) when A > 128 -> A - 256;
signed(A) -> A.

clamp(V, Max) -> max(-Max, min(Max, V)).

%%==============================================================================
%% Direction to angle, without atan2
%%==============================================================================

%% The binary angle of a vector, found by bisection on the sign of a cross
%% product. Eight steps, integer throughout, no libm, no float, no table beyond
%% the engine's own sine table.
%%
%% The invariant is that ccw/2 is true for angle A exactly when the vector lies
%% within half a turn counter-clockwise of A, so the true-set is a half-circle
%% and bisection over the 256 angles converges on its upper edge, which is the
%% vector's own angle rounded down. The one direction where that argument needs
%% help is a vector with no cross component at all: there the very first test
%% sits exactly on the antipode, where the cross product is zero, and the
%% inclusive comparison sends the search a full turn the wrong way to land one
%% unit short. The positive x axis is that direction and it is answered directly.
%% AUDIT FIX: this used to return the angle rounded DOWN, which put a systematic
%% one-sided bias on every aim, radar lock and steer command in the module.
%% Measured against atan2 over every integer vector in a 40 unit box, the error
%% was in [-0.999, 0] units with mean -0.48, never once positive: a 0.67 degree
%% clockwise lean worth about 4 units of lateral miss at 350 range. It also made
%% the bots CHIRAL, so a mirrored match was not the mirror of the original, which
%% sits badly beside an arena whose starts are mirrored. Rounding to nearest
%% halves the error and removes the lean.
angle_of({0, 0}) -> 0;
angle_of({DX, 0}) when DX > 0 -> 0;
angle_of(V) -> nearest_angle(V, bisect(V, 0, 256, 8)).

%% cross(V, A) is |V| times the sine of the angle from A to V, so of the two
%% angles bracketing the true direction the nearer one is whichever has the
%% smaller absolute cross product. Ties keep the lower angle, which keeps the
%% function total and its choice independent of anything but the vector.
nearest_angle(V, Lo) -> closer_angle(Lo, robo_sim:wrap(Lo + 1),
                                     abs(cross(V, Lo)) =< abs(cross(V, robo_sim:wrap(Lo + 1)))).

closer_angle(Lo, _Hi, true) -> Lo;
closer_angle(_Lo, Hi, false) -> Hi.

bisect(_V, Lo, _Hi, 0) -> robo_sim:wrap(Lo);
bisect(V, Lo, Hi, N) ->
    Mid = (Lo + Hi) div 2,
    bisect_pick(V, Lo, Hi, Mid, N, ccw(V, Mid)).

bisect_pick(V, _Lo, Hi, Mid, N, true) -> bisect(V, Mid, Hi, N - 1);
bisect_pick(V, Lo, _Hi, Mid, N, false) -> bisect(V, Lo, Mid, N - 1).

%% Is the vector counter-clockwise of, or exactly on, direction A?
ccw(V, A) -> cross(V, A) >= 0.

cross({DX, DY}, A) -> robo_sim:cos(A) * DY - robo_sim:sin(A) * DX.
