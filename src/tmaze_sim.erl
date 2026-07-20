%% @doc Cue-memory T-maze: a clean memory benchmark solvable in few generations.
%%
%% Each trial: a CUE (left/right, drawn at random) is shown at step 0, then a
%% corridor of `delay' steps shows nothing, then at the junction the agent must
%% decide which way to turn. Reward iff the decision matches the cue. The cue is
%% only visible at step 0, so the network must CARRY it across the corridor to
%% the junction — that is the memory the task demands.
%%
%% Sensor (vl 2): [Cue, JunctionFlag].
%%   step 0        -> [cue, 0]   cue shown (+1 left / -1 right)
%%   corridor      -> [0, 0]     nothing
%%   junction      -> [0, 1]     decide now
%% Actuator (vl 1): Output >= 0 turns left, < 0 turns right.
%%
%% Why it is the right instrument: with random cues, a MEMORYLESS network sees
%% [0,1] at the junction with no cue information, so its best fixed policy is to
%% always turn one way — capped at ~50% (10/20 trials). A network that can hold
%% the cue (recurrent wiring, or an LTC neuron's internal_state) can reach 100%.
%% The fitness (count of correct decisions) cleanly separates the two. Unlike
%% DXNN2's discrete T-maze, this tests static memory, not lifetime plasticity, so
%% it needs no neuromodulation.
%%
%% Actuator/sensor parameters [Delay | Trials] override the corridor length and
%% trial count (defaults 2 and 20).
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(tmaze_sim).

-behaviour(scape).

-export([init/1, sense/3, act/4]).

-define(DEFAULT_DELAY, 2).
-define(DEFAULT_TRIALS, 20).
%% Solved when at least this fraction of trials are correct.
-define(SOLVE_RATIO, 0.9).

-record(state, {
    delay = ?DEFAULT_DELAY :: non_neg_integer(),
    trials = ?DEFAULT_TRIALS :: pos_integer(),
    trial_index = 0 :: non_neg_integer(),
    step = 0 :: non_neg_integer(),
    cue = 1.0 :: float(),
    correct = 0 :: non_neg_integer()
}).

init(_Params) ->
    #state{cue = draw_cue()}.

%% Sense: cue at step 0, junction flag at the decision step, nothing in between.
sense(_SensorName, Params, S) ->
    Junction = delay(Params, S) + 1,
    Vec = sense_vector(S#state.step, Junction, S#state.cue),
    {Vec, S}.

sense_vector(0, _Junction, Cue) -> [Cue, 0.0];
sense_vector(Step, Junction, _Cue) when Step =:= Junction -> [0.0, 1.0];
sense_vector(_Step, _Junction, _Cue) -> [0.0, 0.0].

%% Act: advance through the corridor; at the junction, score the decision and
%% either start the next trial or end the episode.
act(_ActuatorName, Params, [Output], S) ->
    Junction = delay(Params, S) + 1,
    act_step(S#state.step < Junction, Output, S).

act_step(true, _Output, S) ->
    {0.0, 0, S#state{step = S#state.step + 1}};
act_step(false, Output, S) ->
    Reward = reward(same_sign(Output, S#state.cue)),
    NewCorrect = S#state.correct + round(Reward),
    NewIndex = S#state.trial_index + 1,
    finish_or_continue(NewIndex >= S#state.trials, Reward, NewCorrect, NewIndex, S).

finish_or_continue(true, Reward, NewCorrect, NewIndex, S) ->
    Halt = solved_flag(NewCorrect >= round(S#state.trials * ?SOLVE_RATIO)),
    {Reward, Halt, S#state{correct = NewCorrect, trial_index = NewIndex}};
finish_or_continue(false, Reward, NewCorrect, NewIndex, S) ->
    {Reward, 0, S#state{step = 0, cue = draw_cue(),
                        trial_index = NewIndex, correct = NewCorrect}}.

%%%============================================================================
%%% Internal
%%%============================================================================

reward(true) -> 1.0;
reward(false) -> 0.0.

solved_flag(true) -> goal_reached;
solved_flag(false) -> 1.

same_sign(A, B) -> (A >= 0) =:= (B >= 0).

draw_cue() ->
    case rand:uniform(2) of
        1 -> 1.0;
        2 -> -1.0
    end.

delay([D | _], _S) when is_integer(D) -> D;
delay(_Params, S) -> S#state.delay.
