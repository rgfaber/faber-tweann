%% @doc Cart-pole balancing scape: single and double pole, with or without velocity.
%%
%% Faithful port of Gene Sher's DXNN2 pole-balancing scape (Handbook Ch 14),
%% adapted to faber-tweann's scape behaviour (one process, State threaded by the
%% generic loop, no process dictionary).
%%
%% One physics engine drives four problem variants, selected by the SENSOR's
%% parameters (the variant tag) and the ACTUATOR's parameters (single vs double,
%% goal length):
%%
%%   sensor [4] -> [CPos, CVel, PAngle1, PVel1]                single pole, WITH velocity
%%   sensor [2] -> [CPos, PAngle1]                             single pole, NO velocity
%%   sensor [6] -> [CPos, CVel, PAngle1, PAngle2, PVel1, PVel2] double pole, WITH velocity
%%   sensor [3] -> [CPos, PAngle1, PAngle2]                    double pole, NO velocity
%%
%% The no-velocity variants are only Markov-solvable with recurrence (the network
%% must integrate velocity itself), so they need working recurrent evaluation.
%% The with-velocity variants are feedforward-solvable: full state is observed.
%%
%% Actuator parameters are [Damping_Flag, DPB_Flag | GoalSteps]:
%%   DPB_Flag = 0 disables the second-pole fall check (single pole)
%%   DPB_Flag = 1 enables it (double pole)
%%   Damping_Flag = without_damping -> +1 fitness per surviving step (pure
%%     timesteps-balanced, the Table 14.1 metric); with_damping -> the shaped
%%     reward that also rewards keeping the cart and pole near centre.
%%
%% The integrator (sm_double_pole/3) uses semi-implicit Euler at dt=0.01s, two
%% sub-steps per action, force applied on the first sub-step only. Even single-
%% pole variants integrate pole 2: its mass (0.01) barely perturbs the cart and
%% its fall check is disabled, exactly as in DXNN2.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(pb_sim).

-behaviour(scape).

-export([init/1, sense/3, act/4]).

%% 36 degrees, the pole-fall threshold; 2.4m, the track half-length.
-define(ANGLE_LIMIT, (2 * math:pi() * (36 / 360))).
-define(TRACK_LIMIT, 2.4).
%% Balance this many control steps and the task is solved.
-define(DEFAULT_GOAL_STEPS, 100000).
%% Pole 1 starts 3.6 degrees off vertical, as in DXNN2.
-define(INIT_ANGLE1, (3.6 * (2 * math:pi() / 360))).

-record(state, {
    cpos = 0.0 :: float(),
    cvel = 0.0 :: float(),
    p1_angle = ?INIT_ANGLE1 :: float(),
    p1_vel = 0.0 :: float(),
    p2_angle = 0.0 :: float(),
    p2_vel = 0.0 :: float(),
    time_step = 0 :: non_neg_integer(),
    fitness_acc = 0.0 :: float()
}).

%%%============================================================================
%%% scape behaviour
%%%============================================================================

%% init([]) starts from the standard benign state. A proplist of field overrides
%% sets initial conditions (for generalisation testing over varied starting
%% velocities/angles): [{p1_vel, V}, {p1_angle, A}, ...]. Unrecognised entries and
%% non-list params fall through to the default, so existing callers are unchanged.
init(Overrides) when is_list(Overrides) ->
    lists:foldl(fun apply_override/2, #state{}, Overrides);
init(_Params) ->
    #state{}.

apply_override({cpos, V}, S) -> S#state{cpos = V};
apply_override({cvel, V}, S) -> S#state{cvel = V};
apply_override({p1_angle, V}, S) -> S#state{p1_angle = V};
apply_override({p1_vel, V}, S) -> S#state{p1_vel = V};
apply_override({p2_angle, V}, S) -> S#state{p2_angle = V};
apply_override({p2_vel, V}, S) -> S#state{p2_vel = V};
apply_override(_Other, S) -> S.

%% Sense reads the current cart-pole state; it does not advance the physics.
%% The physics step happens in act/4, once the network has decided a force.
sense(_SensorName, [Variant], S) ->
    {sense_vector(Variant, S), S}.

%% Act applies force F = Output * 10 (network output range +/-1 -> +/-10 N),
%% integrates one control step, scores it, and decides whether to halt.
%%
%% Output is the actuator's fan-in vector. A vl=1 actuator usually carries one
%% value, but evolution can wire several neurons into it (add_outlink), so the
%% vector may hold more than one; combine them by summing into a single force.
%% Matching only [Output] crashed the scape with function_clause once a
%% multi-fan-in actuator appeared (EXP_023, deep evolution).
act(_ActuatorName, Params, Output, S) when is_list(Output) ->
    {DampingFlag, DPBFlag, Goal} = params(Params),
    Force = lists:sum(Output),
    S1 = sm_double_pole(Force * 10, S, 2),
    Failed = (abs(S1#state.p1_angle) > ?ANGLE_LIMIT)
        orelse (abs(S1#state.p2_angle) * DPBFlag > ?ANGLE_LIMIT)
        orelse (abs(S1#state.cpos) > ?TRACK_LIMIT),
    Reached = S1#state.time_step > Goal,
    step_fitness(Failed, Reached, DampingFlag, S1).

%%%============================================================================
%%% Sensing
%%%============================================================================

sense_vector(2, S) ->
    [scale_cpos(S), scale_angle(S#state.p1_angle)];
sense_vector(3, S) ->
    [scale_cpos(S), scale_angle(S#state.p1_angle), scale_angle(S#state.p2_angle)];
sense_vector(4, S) ->
    [scale_cpos(S), scale_cvel(S), scale_angle(S#state.p1_angle), S#state.p1_vel];
sense_vector(6, S) ->
    [scale_cpos(S), scale_cvel(S), scale_angle(S#state.p1_angle),
     scale_angle(S#state.p2_angle), S#state.p1_vel, S#state.p2_vel].

scale_cpos(S) -> functions:scale(S#state.cpos, ?TRACK_LIMIT, -?TRACK_LIMIT).
scale_cvel(S) -> functions:scale(S#state.cvel, 10.0, -10.0).
scale_angle(A) -> functions:scale(A, ?ANGLE_LIMIT, -?ANGLE_LIMIT).

%%%============================================================================
%%% Fitness and termination
%%%============================================================================

%% goal_reached wins over a same-step fall, matching DXNN2's ordering.
step_fitness(_Failed, true, DampingFlag, S) ->
    {fitness(DampingFlag, S), goal_reached, S};
step_fitness(false, false, DampingFlag, S) ->
    F = fitness(DampingFlag, S),
    {F, 0, S#state{fitness_acc = S#state.fitness_acc + F}};
step_fitness(true, false, _DampingFlag, S) ->
    {0.0, 1, S}.

fitness(without_damping, _S) ->
    1.0;
fitness(with_damping, S) ->
    TimeStep = S#state.time_step,
    F1 = TimeStep / 1000,
    F2 = case TimeStep < 100 of
        true -> 0.0;
        false -> 0.75 / (abs(S#state.cpos) + abs(S#state.cvel)
                         + abs(S#state.p1_angle) + abs(S#state.p1_vel))
    end,
    F1 * 0.1 + F2 * 0.9.

params([Damping, DPB]) -> {Damping, DPB, ?DEFAULT_GOAL_STEPS};
params([Damping, DPB, Goal]) -> {Damping, DPB, Goal};
params([Damping]) -> {Damping, 0, ?DEFAULT_GOAL_STEPS}.

%%%============================================================================
%%% Physics: two-pole cart, semi-implicit Euler, force on the first sub-step
%%%============================================================================

sm_double_pole(_F, S, 0) ->
    S#state{time_step = S#state.time_step + 1};
sm_double_pole(F, S, N) ->
    CVel = S#state.cvel,
    PAngle1 = S#state.p1_angle,
    PAngle2 = S#state.p2_angle,
    PVel1 = S#state.p1_vel,
    PVel2 = S#state.p2_vel,
    PHalfLength1 = 0.5,
    PHalfLength2 = 0.05,
    M = 1.0,
    PMass1 = 0.1,
    PMass2 = 0.01,
    MUc = 0.0005,
    MUp = 0.000002,
    G = -9.81,
    Delta = 0.01,
    EM1 = PMass1 * (1 - (3 / 4) * math:pow(math:cos(PAngle1), 2)),
    EM2 = PMass2 * (1 - (3 / 4) * math:pow(math:cos(PAngle2), 2)),
    EF1 = PMass1 * PHalfLength1 * math:pow(PVel1, 2) * math:sin(PAngle1)
        + (3 / 4) * PMass1 * math:cos(PAngle1)
          * (((MUp * PVel1) / (PMass1 * PHalfLength1)) + G * math:sin(PAngle1)),
    EF2 = PMass2 * PHalfLength2 * math:pow(PVel2, 2) * math:sin(PAngle2)
        + (3 / 4) * PMass2 * math:cos(PAngle2)
          * (((MUp * PVel2) / (PMass1 * PHalfLength2)) + G * math:sin(PAngle2)),
    NextCAccel = (F - MUc * functions:sgn(CVel) + EF1 + EF2) / (M + EM1 + EM2),
    NextPAccel1 = -(3 / (4 * PHalfLength1))
        * ((NextCAccel * math:cos(PAngle1)) + (G * math:sin(PAngle1))
           + ((MUp * PVel1) / (PMass1 * PHalfLength1))),
    NextPAccel2 = -(3 / (4 * PHalfLength2))
        * ((NextCAccel * math:cos(PAngle2)) + (G * math:sin(PAngle2))
           + ((MUp * PVel2) / (PMass2 * PHalfLength2))),
    NextCVel = CVel + (Delta * NextCAccel),
    NextCPos = S#state.cpos + (Delta * CVel),
    NextPVel1 = PVel1 + (Delta * NextPAccel1),
    NextPAngle1 = PAngle1 + (Delta * NextPVel1),
    NextPVel2 = PVel2 + (Delta * NextPAccel2),
    NextPAngle2 = PAngle2 + (Delta * NextPVel2),
    U_S = S#state{
        cpos = NextCPos, cvel = NextCVel,
        p1_angle = NextPAngle1, p1_vel = NextPVel1,
        p2_angle = NextPAngle2, p2_vel = NextPVel2
    },
    sm_double_pole(0, U_S, N - 1).
