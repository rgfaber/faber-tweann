%% @doc Robo Rumble: the controller contract. 17 sensor channels in, 5 intents out.
%%
%% WHY THIS MODULE EXISTS, AND WHY IT DID NOT UNTIL NOW. The phase 0
%% pre-registration placed the controller in a faber-tweann module named
%% robo_pilot. No such module existed at the pinned engine commit, and the pin IS
%% the provenance of every phase 0 number, so the controller was written inside
%% the experiment runner instead, with the deviation recorded there rather than
%% hidden. That runner's own note says the fix is "a pin bump plus a file move,
%% and nothing about the measurement changes". This is that move.
%%
%% It is not tidying. The engine was a library that could simulate a match and
%% could not DRIVE one: the code turning arena state into a genome's inputs, and
%% a genome's outputs into engine intents, lived in a research runner in a
%% different repository. A host machine handed a foreign genome had no way to run
%% it. Every rule of the game a visitor must agree to was therefore unavailable
%% to the only component that needs it. That, and not the wire format, is what
%% blocked the mesh half of this front.
%%
%% BEHAVIOUR-IDENTICAL BY CONSTRUCTION. Every function body below is the runner's
%% body character for character. Only the names lost their pilot_ prefix, which is
%% redundant in a module called robo_pilot, and act/4 now matches
%% robo_gauntlet:act/4 exactly so a host can drive a genome and a scripted bot
%% through one call shape. robo_pilot_tests proves the equivalence against the
%% archived phase 0 champions rather than asserting it.
%%
%% THE PERCEPTION BOUNDARY IS A SHAPE, NOT A COMMENT. act/4 destructures the arena
%% to its scans field and to NOTHING ELSE, so #arena.tanks and #arena.bullets are
%% out of scope below that line and an opponent's #tank{} cannot be reached from
%% decide/3 even by accident. The defence is the destructuring; a comment would
%% not survive review, a shape the compiler can check does.
%%
%% TWO LIMITATIONS THAT ARE PRE-REGISTERED AS LIMITATIONS, not defects, and which
%% any claim about this controller must respect:
%%
%%   The controller CANNOT SEE BULLETS. There is no incoming-fire channel. The
%%   only proprioception of being shot is channel 4, an energy delta that also
%%   conflates hitting a wall, being rammed, paying to fire and being paid for a
%%   hit.
%%
%%   The controller DOES NOT PREDICT. Channels 9 to 14 report the contact's
%%   estimated position NOW, by dead reckoning, never where it will be. Solving
%%   flight time is strategy and is withheld. So "learned to track" and "learned
%%   to handle intermittent observation" are claims this controller cannot
%%   support, by design.
%%
%% NO FLOAT AND NO LIBM, the same rule the rest of the engine keeps. Every
%% quantity is an integer at the arena's scale of 256, and bearings are produced
%% by rotating a scan's vector against robo_sim's own sine table, never by atan2.
%% A scan carries a vector, never an angle, precisely so no inverse trig is
%% needed anywhere in the match path.
-module(robo_pilot).

-include("robo_sim.hrl").

%% The controller contract a host must satisfy to drive a foreign genome
-export([inputs/0, outputs/0, init/0, act/4]).
%% The pieces, exported for tests, probes and diagnostics rather than for the loop
-export([channels/2, decide/3, contact_vel/1]).

-define(INPUTS, 17).
-define(OUTPUTS, 5).
-define(SIN_SCALE, 32768).       %% the engine's sine scale
-define(BAR, 25600).             %% robo_sim START_ENERGY: the death floor
-define(TANK_R, 4608).           %% 18 whole units
-define(ORBIT, 51200).           %% 200 whole units, the gauntlet's own orbit range
-define(WALL_SPAN, 32768).       %% 128 whole units of clearance reads danger 0

%% One contact track, plus the proprioception channel 4 needs. Positions are
%% ABSOLUTE and are derived from own position plus a scan delta, which is a
%% derivation from two permitted facts and not a peek at the opponent's record.
-record(pilot, {
    tick = 0 :: non_neg_integer(),
    seen = 0 :: 0..2,                %% 0 none, 1 position, 2 also velocity
    age = 0 :: non_neg_integer(),    %% turns since the latest contact
    dist = 0 :: integer(),           %% range at the latest contact, fixed point
    ex = 0 :: integer(),             %% latest contact position, absolute
    ey = 0 :: integer(),
    etick = 0 :: non_neg_integer(),
    px = 0 :: integer(),             %% previous contact position, absolute
    py = 0 :: integer(),
    ptick = 0 :: non_neg_integer(),
    prev_e = ?BAR :: integer(),      %% own energy last turn
    d_e = 0 :: integer(),            %% own energy change since last turn
    target = none :: term()
}).

-opaque state() :: #pilot{}.
-export_type([state/0]).

%%==============================================================================
%% The contract
%%==============================================================================

%% How many sensor channels the pilot emits. A genome's FIRST layer width must
%% equal this. robo_net:fit/2 silently pads or truncates a mismatch, so a host
%% that does not check this will run a foreign genome against a differently
%% shaped sensor vector and report a result that means nothing.
-spec inputs() -> pos_integer().
inputs() -> ?INPUTS.

%% How many intents the pilot consumes. A genome's LAST layer width must equal
%% this. intent/1 tolerates a short vector by falling back to a null intent, so
%% again the mismatch is silent and the host must check rather than discover.
-spec outputs() -> pos_integer().
outputs() -> ?OUTPUTS.

-spec init() -> state().
init() -> #pilot{}.

%% THE PERCEPTION BOUNDARY. The arena is destructured to scans and to nothing
%% else. The scans are then narrowed to the entries THIS tank observed.
-spec act({[non_neg_integer()], [integer()]}, state(), #tank{}, #arena{}) ->
          {#intent{}, state()}.
act(Net, State, #tank{id = Id} = Me, #arena{scans = Scans}) ->
    Mine = [S || {Observer, _T, _D, _V} = S <- Scans, Observer =:= Id],
    Next = observe(State, Me, Mine),
    {decide(Net, Next, Me), Next}.

%%==============================================================================
%% Memory
%%==============================================================================

%% Fold this turn's scans into memory. A silent turn ages the track, which is the
%% whole cost of pointing a radar at the wrong part of the arena.
observe(#pilot{tick = T, age = A} = S, Me, []) ->
    own_energy(S#pilot{tick = T + 1, age = A + 1}, Me);
observe(#pilot{tick = T} = S, #tank{x = MX, y = MY} = Me, Scans) ->
    {_O, Target, D, {DX, DY}} = nearest(Scans),
    own_energy(latch(S#pilot{tick = T + 1, age = 0, dist = D},
                     MX + DX, MY + DY, T, Target), Me).

%% Channel 4's source, and the ONLY proprioception of incoming fire that exists.
own_energy(#pilot{prev_e = P} = S, #tank{energy = E}) ->
    S#pilot{prev_e = E, d_e = E - P}.

%% Absolute contact position is own position plus the scan delta. Switching
%% target id resets to a single sighting, so a velocity estimate can never
%% difference two DIFFERENT opponents.
latch(#pilot{target = Target} = S, X, Y, T, Target) ->
    #pilot{ex = EX, ey = EY, etick = ET, seen = N} = S,
    S#pilot{px = EX, py = EY, ptick = ET, ex = X, ey = Y, etick = T,
            seen = min(2, N + 1)};
latch(S, X, Y, T, Target) ->
    S#pilot{px = X, py = Y, ptick = T, ex = X, ey = Y, etick = T,
            seen = 1, target = Target}.

%% Nearest contact, strictly closer so ties keep list order, which the engine
%% fixes.
nearest([H | Rest]) -> lists:foldl(fun closer/2, H, Rest).

closer({_O, _T, D, _V} = C, {_BO, _BT, Best, _BV}) when D < Best -> C;
closer(_C, Best) -> Best.

%% Contact velocity per turn, by differencing the two latched positions.
%% Dividing by the GAP is arithmetic, not strategy, and it is mandatory. There is
%% NO trust window and NO gating, unlike robo_gauntlet:enemy_vel/1: a stale
%% estimate attenuates because the gap divides it, and channel 8 tells the net
%% how much to believe the track. That call belongs to evolution.
-spec contact_vel(state()) -> {integer(), integer()}.
contact_vel(#pilot{seen = N}) when N < 2 -> {0, 0};
contact_vel(#pilot{ex = X, ey = Y, px = PX, py = PY, etick = T, ptick = P}) ->
    Gap = max(1, T - P),
    {(X - PX) div Gap, (Y - PY) div Gap}.

%%==============================================================================
%% The 17 channels. Arena scale 256, every channel inside -256 to 256.
%%==============================================================================

-spec channels(state(), #tank{}) -> [integer()].
channels(S, Me) -> own(S, Me) ++ place(Me) ++ contact(S, Me).

%% 1 own_speed, 2 own_energy, 3 gun_heat, 4 energy_delta.
%%
%% Engine velocity is always along the heading, so one scalar is all of own
%% motion. MAX_VEL 2048 maps to 256 exactly; START_ENERGY 25600 maps to 256; the
%% hottest reachable gun (power 30) reads 204, so channel 3's clamp never bites.
%% Channel 4 conflates being shot, hitting a wall, being rammed, paying to fire
%% and being paid for a hit: irreducible, since separating the causes needs
%% engine internals the boundary forbids.
own(#pilot{d_e = DE}, #tank{vel = V, energy = E, gun_heat = GH}) ->
    [V div 8, min(256, E div 100), min(256, GH div 2), clamp(DE div 16, 256)].

%% 5 pos_fwd, 6 pos_port, 7 wall_danger.
%%
%% Arena size is a rule of the game, not opponent state, so consulting it breaks
%% no perception rule (robo_gauntlet:wall_push/1 already does). Position is given
%% in BODY frame because acting on a world-frame position requires a rotation,
%% and a rotation is a product of activations that a fixed-topology MLP cannot
%% form. wall_danger is DANGER rather than clearance so the channel is SILENT in
%% the common case and does not perturb an untrained baseline.
place(#tank{x = X, y = Y, heading = H}) ->
    {W, A} = robo_sim:arena_size(),
    {FX, FY} = rotate(X - W div 2, Y - A div 2, H),
    [FX div 512, FY div 512, wall_danger(X, Y, W, A)].

wall_danger(X, Y, W, H) ->
    C = lists:min([X - ?TANK_R, W - ?TANK_R - X, Y - ?TANK_R, H - ?TANK_R - Y]),
    256 - min(256, max(0, C) * 256 div ?WALL_SPAN).

%% 8 contact_fresh, 9-14 the three bearing frames, 15 contact_prox,
%% 16 target_lateral, 17 target_range_rate.
%%
%% ALL EXACTLY ZERO when no contact has ever been seen, which combined with
%% robo_net's per-neuron bias makes "behave sensibly while blind" a learnable
%% bias term rather than a special case.
%%
%% THREE FRAMES, DELIBERATELY REDUNDANT. Recovering one frame from another is a
%% rotation, a rotation is a product of activations, and a fixed-topology MLP
%% cannot form products. Spending two channels to remove a multiplication the
%% architecture cannot perform is the correct trade every time: aiming, orbiting
%% and radar tracking each collapse to a single weight. This is also why a
%% linear map over this encoding clears the phase 0 floor, and why the honest
%% report of that result is that the nonlinear work is HERE and not in the net.
contact(#pilot{seen = 0}, _Me) -> lists:duplicate(10, 0);
contact(#pilot{age = A, dist = D} = S, #tank{heading = H, gun = G, radar = R} = Me) ->
    {UX, UY} = sight(S, Me),
    {BC, BS} = unit(UX, UY, H),
    {GC, GS} = unit(UX, UY, G),
    {RC, RS} = unit(UX, UY, R),
    {WC, WS} = unit(UX, UY, 0),
    {VX, VY} = contact_vel(S),
    [fresh(A), BC, BS, GC, GS, RC, RS,
     (256 * ?ORBIT) div (?ORBIT + D),
     clamp((WC * VY - WS * VX) div 2048, 256),
     clamp((WC * VX + WS * VY) div 2048, 256)].

%% 256 on a scan turn, 128 at eight turns, floor 1 at the cap. Exactly 0 if never
%% seen, and that zero is a structural sentinel, which is why this is only ever
%% reached from the seen-nonzero clause above.
fresh(A) -> 2048 div (8 + min(A, 1024)).

%% DEAD RECKONING IS IN, PREDICTION IS OUT. The bearing channels report the
%% contact's ESTIMATED POSITION NOW: latched position plus estimated velocity
%% times age, clamped to arena bounds. Keeping the sensor's meaning constant
%% across turns is what makes a radar track a track rather than a snapshot.
sight(#pilot{ex = EX, ey = EY, age = A} = S, #tank{x = MX, y = MY}) ->
    {VX, VY} = contact_vel(S),
    {PX, PY} = inside(EX + VX * A, EY + VY * A),
    {PX - MX, PY - MY}.

inside(X, Y) ->
    {W, H} = robo_sim:arena_size(),
    {min(max(X, 0), W), min(max(Y, 0), H)}.

%% THE BEARING TRICK. No atan2 and none added: a scan carries a vector, never a
%% bearing. The delta is rotated into a part's frame against robo_sim's own sine
%% table, then BOTH components are divided by the octagonal norm OF THE ROTATED
%% VECTOR, recomputed rather than reused from the scan's Distance field, because
%% the octagonal metric is not rotation invariant. Max component is exactly 256,
%% so axis-aligned deltas sit exactly ON robo_net's contract edge rather than
%% inside it; the pair norm wobbles between 0.9318 and 1.0275, which is a bounded
%% deterministic gain modulation identical for every controller, not a direction
%% error. The scan's own Distance is used for channel 15, so the range the net
%% sees is the number the engine reported.
unit(UX, UY, A) ->
    {RX, RY} = rotate(UX, UY, A),
    scale_pair(RX, RY, robo_sim:dist({0, 0}, {RX, RY})).

scale_pair(_RX, _RY, 0) -> {0, 0};
scale_pair(RX, RY, N) -> {RX * 256 div N, RY * 256 div N}.

%% A world vector into a part's frame. Positive Y is to the LEFT of the part,
%% matching the engine's counter-clockwise-positive rotations.
rotate(DX, DY, A) ->
    C = robo_sim:cos(A),
    S = robo_sim:sin(A),
    {(DX * C + DY * S) div ?SIN_SCALE, (DY * C - DX * S) div ?SIN_SCALE}.

%%==============================================================================
%% The 5 outputs
%%
%% From robo_net:eval_q12/3 so the last four bits are not thrown away, each
%% through robo_net:to_range/2 with THE ENGINE'S OWN CLAMP as Max, so the
%% network's reachable set is exactly the legal set and no output is wasted on
%% values the engine will refuse.
%%
%% FIRING IS ONE CONTINUOUS OUTPUT AND THE THRESHOLD IS THE ENGINE'S.
%% robo_sim:clamp_power/1 already reads anything at or below zero as hold and
%% anything above as a power in tenths, so to_range(A, 30) covers hold and every
%% power on one monotone axis with NO hand-chosen constant and no second output.
%% Do NOT add a hold-biased encoding or a separate fire-decision output: the
%% recorded hazard on this front runs the other way, "never fire" is the strong
%% local optimum, and a neutral untrained prior is a mild counterweight.
%%==============================================================================

-spec decide({[non_neg_integer()], [integer()]}, state(), #tank{}) -> #intent{}.
decide({Layers, Ws}, S, Me) ->
    intent(robo_net:eval_q12(Layers, Ws, channels(S, Me))).

intent([A, B, C, D, E | _]) ->
    #intent{turn_body = robo_net:to_range(A, 7),
            turn_gun = robo_net:to_range(B, 14),
            turn_radar = robo_net:to_range(C, 32),
            accel = robo_net:to_range(D, 512),
            fire = robo_net:to_range(E, 30)};
intent(_Short) -> #intent{}.

clamp(V, Max) -> max(-Max, min(Max, V)).
