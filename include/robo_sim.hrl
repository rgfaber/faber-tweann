%% Robo Rumble arena records (faber-ecosystem PLAN_ROBO_RUMBLE.md, phase 0).
%%
%% EVERY field is an INTEGER. There is no float anywhere in the match path, by
%% design: replay must be bit-identical across machines, and libm transcendentals
%% are not bit-identical across libc versions. See robo_sim for the unit scheme.

%% A tank. Positions/velocity/energy are fixed-point (scale 256); headings are
%% binary angles (0..255, 256 = full turn); gun_heat is fixed-point.
-record(tank, {
    id :: term(),
    x = 0 :: integer(),          %% fixed-point
    y = 0 :: integer(),          %% fixed-point
    heading = 0 :: 0..255,       %% body, absolute
    gun = 0 :: 0..255,           %% gun, absolute
    radar = 0 :: 0..255,         %% radar, absolute
    vel = 0 :: integer(),        %% fixed-point, signed
    energy = 0 :: integer(),     %% fixed-point
    gun_heat = 0 :: integer(),   %% fixed-point
    dead = false :: boolean(),
    damage_dealt = 0 :: integer()
}).

%% An in-flight bullet. Power is in tenths (1..30 = 0.1..3.0).
-record(bullet, {
    owner :: term(),
    x = 0 :: integer(),
    y = 0 :: integer(),
    heading = 0 :: 0..255,
    power = 0 :: 1..30
}).

%% One turn's commanded action from a controller. Rotations are in angle units
%% and are clamped by robo_sim to the per-part limits; accel is fixed-point;
%% fire is 0 (hold) or a power in tenths.
-record(intent, {
    turn_body = 0 :: integer(),
    turn_gun = 0 :: integer(),
    turn_radar = 0 :: integer(),
    accel = 0 :: integer(),
    fire = 0 :: integer()
}).

%% The whole world at one turn. Everything needed to reproduce the next turn.
-record(arena, {
    turn = 0 :: non_neg_integer(),
    tanks = [] :: list(),
    bullets = [] :: list(),
    scans = [] :: list()         %% [{ObserverId, TargetId, Distance, Bearing}]
}).
