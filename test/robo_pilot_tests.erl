%% @doc Tests for the controller contract (PLAN_ROBO_RUMBLE phase 0, extracted).
%%
%% WHAT THESE ARE FOR. robo_pilot is a MOVE, not new code: every body in it is the
%% phase 0 experiment runner's body character for character. So the risk is not
%% that the design is wrong, it is that the move corrupted something silently. A
%% mistyped constant, a swapped shift, a channel in the wrong order or a lost
%% clamp would change every match on this front IDENTICALLY on every machine, so
%% no cross-machine replay diff would ever catch it. That is exactly the hazard
%% robo_net_tests was written to close for the forward pass, and GOLDEN_CHANNELS
%% below closes it for the sensor vector.
%%
%% Changing GOLDEN_CHANNELS is a RULES CHANGE and therefore a season boundary,
%% like robo_sim_tests' GOLDEN_HASH and robo_net_tests' GOLDEN_FORWARD. It is
%% never a way to make a test pass.
%%
%% WHAT THESE CANNOT DO, stated so nobody mistakes a green suite for the real
%% proof. The decisive equivalence test is to replay the ARCHIVED phase 0
%% champions through this module and reproduce their held-out win rates exactly.
%% Those champions live in faber-programmes beside the experiment that produced
%% them, and the engine must not depend on the research repository, so that test
%% belongs there and is named in this front's records as owed. What is here is the
%% contract, the boundary and the golden vector.
%% @end
-module(robo_pilot_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("faber_tweann/include/robo_sim.hrl").

%% The sensor vector for the fixture below. DERIVED, not observed: the first
%% version of this constant was guessed at plausible-looking values, the test
%% failed, and the values below were then worked out one channel at a time from
%% robo_pilot's own arithmetic against robo_sim's own exports before being frozen.
%% Pasting an observed value into a golden vector is how a golden vector becomes
%% worthless, so the derivation is recorded here rather than trusted.
%%
%%   1  own_speed       vel div 8 = 512 div 8                          =  64
%%   2  own_energy      min(256, 25600 div 100)                        = 256
%%   3  gun_heat        min(256, 0 div 2)                              =   0
%%   4  energy_delta    a fresh pilot's prev_e equals a fresh tank's
%%                      energy, so the first delta is exactly          =   0
%%   5  pos_fwd         arena_size is FIXED POINT {204800, 153600}, so
%%                      (51200 - 102400) div 512                       = -100
%%   6  pos_port        (38400 - 76800) div 512                        = -75
%%   7  wall_danger     clearance 33792 exceeds WALL_SPAN 32768, so the
%%                      channel is silent                              =   0
%%   8  contact_fresh   scanned this turn: 2048 div (8 + 0)            = 256
%%  9-14 three bearing frames. The contact is due east and body, gun and
%%       radar all sit at angle 0, so each frame gives the same unit pair
%%       and the cosine component lands EXACTLY on robo_net's contract
%%       edge                                                 = 256, 0 (x3)
%%  15  contact_prox    range equals ORBIT exactly, so
%%                      (256 * 51200) div (51200 + 51200)              = 128
%%  16  target_lateral  two identical scans, so the velocity estimate
%%  17  target_range_rate  is (0, 0) and both channels are              =   0
%%
%% Changing this is a RULES CHANGE and therefore a season boundary. It is never a
%% way to make a test pass.
-define(GOLDEN_CHANNELS,
        [64, 256, 0, 0,
         -100, -75, 0,
         256, 256, 0, 256, 0, 256, 0, 128, 0, 0]).

%%==============================================================================
%% The contract a host must satisfy
%%==============================================================================

%% A genome's first layer must be exactly this wide and its last layer exactly
%% that wide. robo_net:fit/2 pads or truncates a mismatch in silence and
%% robo_pilot's intent/1 falls back to a null intent on a short vector, so a host
%% that does not check these will run a foreign genome against a differently
%% shaped world and report a number that means nothing.
contract_arity_test() ->
    ?assertEqual(17, robo_pilot:inputs()),
    ?assertEqual(5, robo_pilot:outputs()).

%% The channel vector's LENGTH is part of the contract independently of its
%% contents, because a host validates a topology against inputs/0 before it ever
%% evaluates anything.
channel_count_matches_declared_inputs_test() ->
    Me = fixture_tank(),
    ?assertEqual(robo_pilot:inputs(),
                 length(robo_pilot:channels(robo_pilot:init(), Me))).

%%==============================================================================
%% The golden sensor vector
%%==============================================================================

golden_channels_test() ->
    ?assertEqual(?GOLDEN_CHANNELS, robo_pilot:channels(seen_state(), fixture_tank())).

%% Every channel must sit inside robo_net's stated input contract of -256 to 256.
%% Out-of-range inputs are legal and merely saturate, so a drift here costs
%% behaviour rather than correctness and would never fail anything else.
all_channels_within_contract_test() ->
    Cs = robo_pilot:channels(seen_state(), fixture_tank()),
    [?assert(C >= -256 andalso C =< 256) || C <- Cs],
    ok.

%% ADDED AFTER A RED CHECK FOUND THIS SUITE BLIND. Perturbing the golden vector's
%% inputs one at a time showed that swapping the channel blocks fails 5 tests and
%% mistyping the own_speed shift fails 1, but mistyping TANK_R from 4608 to 4680
%% passed everything. The reason is that the fixture above stands in open ground,
%% where clearance exceeds WALL_SPAN and wall_danger saturates to a SILENT zero,
%% so the radius cancels out entirely. A tank near a wall is the only place that
%% constant is observable.
%%
%% Derived, not observed. At x = 10000 the clearance is
%% min(10000 - 4608, 204800 - 4608 - 10000, 38400 - 4608, 153600 - 4608 - 38400)
%% = 5392, and 256 - (5392 * 256 div 32768) = 256 - 42 = 214. With TANK_R at 4680
%% the clearance is 5320, the quotient is 41, and the channel reads 215, so a
%% digit transposition in the radius now moves this number.
wall_danger_is_observable_near_a_wall_test() ->
    Near = (fixture_tank())#tank{x = 10000},
    ?assertEqual(214, lists:nth(7, robo_pilot:channels(robo_pilot:init(), Near))),
    %% And it is still silent in the open, which is the property the blind
    %% sentinel argument depends on.
    ?assertEqual(0, lists:nth(7, robo_pilot:channels(robo_pilot:init(), fixture_tank()))).

%%==============================================================================
%% The blind case, which is structural rather than incidental
%%==============================================================================

%% With no contact ever seen, the last ten channels are EXACTLY zero. That zero is
%% a sentinel: combined with robo_net's per-neuron bias it makes "behave sensibly
%% while blind" a learnable bias term instead of a special case in the loop. A
%% drift to near-zero would still pass a looser test and would quietly remove
%% that property.
blind_contact_block_is_exactly_zero_test() ->
    Cs = robo_pilot:channels(robo_pilot:init(), fixture_tank()),
    ?assertEqual(lists:duplicate(10, 0), lists:nthtail(7, Cs)).

%% A track with one sighting has a position but no velocity, so the two
%% velocity-derived channels stay zero while the bearings do not.
one_sighting_gives_no_velocity_test() ->
    ?assertEqual({0, 0}, robo_pilot:contact_vel(one_sighting_state())),
    Cs = robo_pilot:channels(one_sighting_state(), fixture_tank()),
    ?assertEqual([0, 0], lists:nthtail(15, Cs)).

%%==============================================================================
%% The perception boundary
%%==============================================================================

%% act/4 narrows the arena's scans to the acting tank's own observer id. A scan
%% belonging to a DIFFERENT observer must not reach the track, or a controller
%% would see through another tank's radar.
foreign_observer_scans_are_ignored_test() ->
    Me = fixture_tank(),
    Mine = #arena{scans = [{a, b, 25600, {6400, 0}}]},
    Theirs = #arena{scans = [{other, b, 25600, {6400, 0}}]},
    {_I1, S1} = robo_pilot:act(null_net(), robo_pilot:init(), Me, Mine),
    {_I2, S2} = robo_pilot:act(null_net(), robo_pilot:init(), Me, Theirs),
    %% The tank's own scan produces a contact; the foreign one produces none, so
    %% the blind sentinel survives.
    ?assertNotEqual(lists:duplicate(10, 0), lists:nthtail(7, robo_pilot:channels(S1, Me))),
    ?assertEqual(lists:duplicate(10, 0), lists:nthtail(7, robo_pilot:channels(S2, Me))).

%% A silent turn AGES the track rather than dropping it, which is the entire cost
%% of pointing a radar at the wrong part of the arena. Channel 8 must fall.
silence_ages_the_track_test() ->
    Me = fixture_tank(),
    Seen = #arena{scans = [{a, b, 25600, {6400, 0}}]},
    Quiet = #arena{scans = []},
    {_I1, S1} = robo_pilot:act(null_net(), robo_pilot:init(), Me, Seen),
    {_I2, S2} = robo_pilot:act(null_net(), S1, Me, Quiet),
    Fresh1 = lists:nth(8, robo_pilot:channels(S1, Me)),
    Fresh2 = lists:nth(8, robo_pilot:channels(S2, Me)),
    ?assert(Fresh2 < Fresh1),
    ?assert(Fresh2 > 0).

%% Switching target id must reset the track to a single sighting, so a velocity
%% estimate can never difference the positions of two DIFFERENT opponents.
target_switch_resets_velocity_test() ->
    Me = fixture_tank(),
    A1 = #arena{scans = [{a, first, 25600, {6400, 0}}]},
    A2 = #arena{scans = [{a, first, 25600, {7680, 0}}]},
    A3 = #arena{scans = [{a, second, 25600, {2560, 2560}}]},
    {_, S1} = robo_pilot:act(null_net(), robo_pilot:init(), Me, A1),
    {_, S2} = robo_pilot:act(null_net(), S1, Me, A2),
    ?assertNotEqual({0, 0}, robo_pilot:contact_vel(S2)),
    {_, S3} = robo_pilot:act(null_net(), S2, Me, A3),
    ?assertEqual({0, 0}, robo_pilot:contact_vel(S3)).

%%==============================================================================
%% Constants that must track the engine rather than drift from it
%%==============================================================================

%% robo_pilot carries the death floor as its own constant, because robo_sim does
%% not export START_ENERGY. That duplication is the risk: if the engine's starting
%% energy ever changes, channel 4 would report a spurious delta on turn one and
%% NOTHING else would fail. This test is the only thing standing between that
%% change and a silently different sensor.
death_floor_tracks_engine_start_energy_test() ->
    #arena{tanks = [T | _]} = robo_sim:new([{a, 100, 100, 0}, {b, 400, 400, 128}]),
    Blind = robo_pilot:channels(robo_pilot:init(), T),
    %% Channel 4 is the energy delta. A fresh pilot's prev_e must equal a fresh
    %% tank's energy, so the delta on the very first read is exactly zero.
    {_I, S} = robo_pilot:act(null_net(), robo_pilot:init(), T, #arena{scans = []}),
    ?assertEqual(0, lists:nth(4, robo_pilot:channels(S, T))),
    ?assertEqual(17, length(Blind)).

%%==============================================================================
%% Output decoding
%%==============================================================================

%% A short output vector must fall back to a NULL intent rather than crash, and
%% the fallback must be genuinely null: a topology whose last layer is too narrow
%% is a host-side validation failure, not something that should quietly steer.
short_output_falls_back_to_null_intent_test() ->
    Null = #intent{},
    ?assertEqual(Null, robo_pilot:decide({[17, 1], zeros(18)}, robo_pilot:init(),
                                         fixture_tank())).

%% Every intent field must land inside the engine's own clamp, because the
%% ranges in decide/3 are the engine's limits and not hand-chosen constants.
intent_fields_within_engine_clamps_test() ->
    #intent{turn_body = TB, turn_gun = TG, turn_radar = TR, accel = Ac, fire = F} =
        robo_pilot:decide(saturating_net(), seen_state(), fixture_tank()),
    ?assert(abs(TB) =< 7),
    ?assert(abs(TG) =< 14),
    ?assert(abs(TR) =< 32),
    ?assert(abs(Ac) =< 512),
    ?assert(abs(F) =< 30).

%%==============================================================================
%% Fixtures. Fixed integers only, so every expectation above is reproducible.
%%==============================================================================

fixture_tank() ->
    #tank{id = a, x = 51200, y = 38400, heading = 0, gun = 0, radar = 0,
          vel = 512, energy = 25600, gun_heat = 0}.

%% A pilot that has seen the same target twice, at a known range, moving. Built
%% through act/4 rather than by poking the record, so the fixture exercises the
%% real latch path and cannot encode a state the module could not reach.
seen_state() ->
    Me = fixture_tank(),
    A1 = #arena{scans = [{a, b, 51200, {51200, 0}}]},
    A2 = #arena{scans = [{a, b, 51200, {51200, 0}}]},
    {_, S1} = robo_pilot:act(null_net(), robo_pilot:init(), Me, A1),
    {_, S2} = robo_pilot:act(null_net(), S1, Me, A2),
    S2.

one_sighting_state() ->
    {_, S} = robo_pilot:act(null_net(), robo_pilot:init(), fixture_tank(),
                            #arena{scans = [{a, b, 51200, {51200, 0}}]}),
    S.

%% A net that outputs nothing useful. Weights all zero, so every activation is the
%% bias, which is zero: the intent is null and the pilot's own arithmetic is what
%% the channel tests are reading.
null_net() -> {[17, 5], zeros(robo_net:weight_count([17, 5]))}.

%% Weights at the cap, so every output saturates and the clamp tests bite.
saturating_net() ->
    N = robo_net:weight_count([17, 5]),
    {[17, 5], lists:duplicate(N, robo_net:weight_limit())}.

zeros(N) -> lists:duplicate(N, 0).
