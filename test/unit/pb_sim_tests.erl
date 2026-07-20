%% @doc Unit tests for the cart-pole scape (pb_sim), driving its pure callbacks
%% directly. No processes, no evolution: this proves the physics port and the
%% sense/act protocol in isolation before any agent runs through it.
-module(pb_sim_tests).
-include_lib("eunit/include/eunit.hrl").

%% init: pole 1 starts ~3.6 degrees off vertical, everything else at rest.
init_state_test() ->
    S = pb_sim:init(undefined),
    {V, S} = pb_sim:sense(pb_GetInput, [4], S),
    ?assertEqual(4, length(V)),
    %% scaled cart position and cart velocity both start at 0.
    [ScaledCPos, ScaledCVel, ScaledA1, PVel1] = V,
    ?assertEqual(0.0, ScaledCPos),
    ?assertEqual(0.0, ScaledCVel),
    ?assertEqual(0.0, PVel1),
    %% pole angle is 3.6/36 of the limit -> 0.1 after scaling.
    ?assert(abs(ScaledA1 - 0.1) < 1.0e-9).

%% Each sensor variant returns a vector of its declared length.
sense_variant_lengths_test() ->
    S = pb_sim:init(undefined),
    ?assertEqual(2, length(element(1, pb_sim:sense(pb_GetInput, [2], S)))),
    ?assertEqual(3, length(element(1, pb_sim:sense(pb_GetInput, [3], S)))),
    ?assertEqual(4, length(element(1, pb_sim:sense(pb_GetInput, [4], S)))),
    ?assertEqual(6, length(element(1, pb_sim:sense(pb_GetInput, [6], S)))).

%% With no control force the pole falls: the run halts (flag 1, not goal) within
%% a small number of steps, and fitness accumulates while it is still up.
uncontrolled_pole_falls_test() ->
    S = pb_sim:init(undefined),
    {Halt, Steps, LastFitness} = run_zero_force(S, 0, 100000),
    ?assertEqual(1, Halt),
    ?assert(Steps > 0),
    ?assert(Steps < 1000),
    ?assert(LastFitness >= 0.0).

%% A steady positive force drives the cart in the positive direction.
positive_force_moves_cart_right_test() ->
    S0 = pb_sim:init(undefined),
    S1 = step_force(S0, 1.0, 5),
    ?assert(cpos(S1) > 0.0).

%% A steady negative force drives it the other way.
negative_force_moves_cart_left_test() ->
    S0 = pb_sim:init(undefined),
    S1 = step_force(S0, -1.0, 5),
    ?assert(cpos(S1) < 0.0).

%% goal_reached fires when the balanced-step count passes the goal. A short goal
%% (via the third actuator parameter) lets us reach it with a naive controller:
%% push toward the falling direction. We only need SOME run to reach the goal to
%% prove the goal_reached path; a proportional nudge suffices for a few steps.
goal_reached_path_test() ->
    S = pb_sim:init(undefined),
    Reached = run_proportional(S, [with_damping, 0, 3], 0, 100000),
    ?assertEqual(goal_reached, Reached).

%%%============================================================================
%%% Helpers (drive act/4 directly, threading state)
%%%============================================================================

run_zero_force(S, Steps, Cap) when Steps < Cap ->
    case pb_sim:act(pb_SendOutput, [with_damping, 0], [0.0], S) of
        {_F, 0, S1} -> run_zero_force(S1, Steps + 1, Cap);
        {F, Halt, _S1} -> {Halt, Steps + 1, F}
    end;
run_zero_force(_S, Steps, _Cap) -> {timeout, Steps, 0.0}.

%% Apply a fixed force for N control steps, return the resulting state.
step_force(S, _Force, 0) -> S;
step_force(S, Force, N) ->
    {_F, _Halt, S1} = pb_sim:act(pb_SendOutput, [with_damping, 0], [Force], S),
    step_force(S1, Force, N - 1).

%% Naive proportional controller: push in the direction the pole is leaning.
run_proportional(S, Params, Steps, Cap) when Steps < Cap ->
    Force = proportional_force(S),
    case pb_sim:act(pb_SendOutput, Params, [Force], S) of
        {_F, 0, S1} -> run_proportional(S1, Params, Steps + 1, Cap);
        {_F, Halt, _S1} -> Halt
    end;
run_proportional(_S, _Params, _Steps, _Cap) -> timeout.

%% Sense the scaled pole angle and push proportionally toward upright.
proportional_force(S) ->
    {[_CPos, _CVel, ScaledAngle, _PVel], S} = pb_sim:sense(pb_GetInput, [4], S),
    %% ScaledAngle > 0 means leaning positive; push positive to catch it.
    max(-1.0, min(1.0, ScaledAngle * 10.0)).

cpos(S) ->
    {[ScaledCPos | _], S} = pb_sim:sense(pb_GetInput, [4], S),
    ScaledCPos.
