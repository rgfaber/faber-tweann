%% @doc Unit tests for the cue-memory T-maze scape, driving its pure callbacks.
-module(tmaze_sim_tests).
-include_lib("eunit/include/eunit.hrl").

-define(P, [2]).

%% Sense structure: cue at step 0, junction flag at the decision step, blank in
%% the corridor.
sense_structure_test() ->
    S0 = tmaze_sim:init(undefined),
    {[Cue, J0], S0} = tmaze_sim:sense(n, ?P, S0),
    ?assert(Cue =:= 1.0 orelse Cue =:= -1.0),
    ?assertEqual(0.0, J0),
    %% Advance one corridor step.
    {_, _, S1} = tmaze_sim:act(n, ?P, [0.0], S0),
    ?assertEqual([0.0, 0.0], element(1, tmaze_sim:sense(n, ?P, S1))),
    %% Second corridor step -> now at the junction (delay 2).
    {_, _, S2} = tmaze_sim:act(n, ?P, [0.0], S1),
    ?assertEqual([0.0, 0.0], element(1, tmaze_sim:sense(n, ?P, S2))),
    {_, _, S3} = tmaze_sim:act(n, ?P, [0.0], S2),
    ?assertEqual([0.0, 1.0], element(1, tmaze_sim:sense(n, ?P, S3))).

%% A perfect-memory policy (turn the way the cue pointed) solves the maze:
%% goal_reached fires. Deterministic regardless of the random cue sequence.
perfect_memory_solves_test() ->
    Halt = run(fun(Cue) -> Cue end),
    ?assertEqual(goal_reached, Halt).

%% The opposite policy is always wrong: 0 correct, so it halts without solving.
wrong_policy_fails_test() ->
    Halt = run(fun(Cue) -> -Cue end),
    ?assertEqual(1, Halt).

%%%============================================================================
%%% A driver that plays whole episodes, choosing the junction output via Policy.
%%%============================================================================

run(Policy) ->
    step(tmaze_sim:init(undefined), 0.0, Policy, 100000).

step(_S, _Cue, _Policy, 0) -> erlang:error(too_many_cycles);
step(S, Remembered, Policy, Budget) ->
    {Vec, S1} = tmaze_sim:sense(n, ?P, S),
    {Cue1, Output} = decide(Vec, Remembered, Policy),
    case tmaze_sim:act(n, ?P, [Output], S1) of
        {_R, 0, S2} -> step(S2, Cue1, Policy, Budget - 1);
        {_R, Halt, _S2} -> Halt
    end.

%% Remember the cue when it is shown; act on it at the junction. Guards avoid
%% matching on the float 0.0 (OTP 27 -0.0 semantics).
decide([Cue, _J], _Remembered, _Policy) when Cue > 0.5; Cue < -0.5 ->
    {Cue, 0.0};                       % step 0: cue shown (+/-1), corridor move
decide([_Cue, J], Remembered, Policy) when J > 0.5 ->
    {Remembered, Policy(Remembered)}; % junction: decide from memory
decide([_Cue, _J], Remembered, _Policy) ->
    {Remembered, 0.0}.                % corridor
