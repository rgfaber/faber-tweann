%% @doc Multi-cue T-maze: a memory-CAPACITY benchmark (two held bits).
%%
%% Where tmaze_sim tests memory DURATION (hold one cue across a corridor), this
%% tests memory CAPACITY (hold TWO independent cues at once). Each trial shows
%% cue A on its own channel, then cue B on its own channel, then a corridor of
%% `delay' empty steps, then TWO junctions: junction 1 asks for cue A, junction 2
%% asks for cue B. Both cues are shown early and recalled late, so both must be
%% carried; getting both right needs room for two bits, not just a longer memory.
%%
%% Sensor (vl 4): [CueA, CueB, Junction1, Junction2].
%%   step 0            -> [a, 0, 0, 0]   cue A shown (+1 / -1), its OWN channel
%%   step 1            -> [0, b, 0, 0]   cue B shown (+1 / -1), its OWN channel
%%   corridor          -> [0, 0, 0, 0]   nothing
%%   step delay+2 (J1) -> [0, 0, 1, 0]   recall A
%%   step delay+3 (J2) -> [0, 0, 0, 1]   recall B
%% Cue A is on channel 0 ONLY at step 0, so a consumer can detect trial-start
%% (and reset a plastic net) without mistaking cue B for a new trial.
%% Actuator (vl 1): Output >= 0 decides +, < 0 decides -, at each junction.
%%
%% Baselines: trials use all four (A,B) combinations in equal number, so at each
%% junction a MEMORYLESS "always one way" policy scores exactly half -- fitness 50
%% with zero variance. A one-bit memory (hold A only, or B only) caps at 75. Only
%% a genuine two-bit memory reaches the 90 solve line. Fitness is scaled to 0-100
%% (each of the 2*trials decisions worth 50/trials), so it reuses the same solve
%% goal and ES options as tmaze_sim.
%%
%% Actuator/sensor parameters [Delay | Trials] override the corridor length and
%% trial count (defaults 4 and 100).
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(multi_cue_tmaze_sim).

-behaviour(scape).

-export([init/1, sense/3, act/4]).

-define(DEFAULT_DELAY, 4).
-define(DEFAULT_TRIALS, 100).
-define(SOLVE_RATIO, 0.9).

-record(state, {
    delay = ?DEFAULT_DELAY :: non_neg_integer(),
    trials = ?DEFAULT_TRIALS :: pos_integer(),
    pairs = [] :: [{float(), float()}],
    a = 1.0 :: float(),
    b = 1.0 :: float(),
    step = 0 :: non_neg_integer(),
    correct = 0 :: non_neg_integer()
}).

init(_Params) ->
    [{A, B} | Rest] = balanced_pairs(?DEFAULT_TRIALS),
    #state{trials = ?DEFAULT_TRIALS, pairs = Rest, a = A, b = B}.

%% Sense: cue A (step 0), cue B (step 1), then two junction flags after the corridor.
sense(_SensorName, Params, S) ->
    D = delay(Params, S),
    Vec = sense_vector(S#state.step, D, S#state.a, S#state.b),
    {Vec, S}.

sense_vector(0, _D, A, _B) -> [A, 0.0, 0.0, 0.0];
sense_vector(1, _D, _A, B) -> [0.0, B, 0.0, 0.0];
sense_vector(Step, D, _A, _B) when Step =:= D + 2 -> [0.0, 0.0, 1.0, 0.0];
sense_vector(Step, D, _A, _B) when Step =:= D + 3 -> [0.0, 0.0, 0.0, 1.0];
sense_vector(_Step, _D, _A, _B) -> [0.0, 0.0, 0.0, 0.0].

%% Act: advance the corridor; at junction 1 score against A, at junction 2 score
%% against B and start the next trial. Output is the actuator fan-in (summed).
act(_ActuatorName, Params, Output, S) when is_list(Output) ->
    D = delay(Params, S),
    act_phase(phase(S#state.step, D), lists:sum(Output), S).

phase(Step, D) when Step =:= D + 2 -> junction1;
phase(Step, D) when Step =:= D + 3 -> junction2;
phase(_Step, _D) -> corridor.

act_phase(corridor, _Output, S) ->
    {0.0, 0, S#state{step = S#state.step + 1}};
act_phase(junction1, Output, S) ->
    Reward = decision_reward(same_sign(Output, S#state.a), S#state.trials),
    {Reward, 0, S#state{step = S#state.step + 1,
                        correct = S#state.correct + correct(Output, S#state.a)}};
act_phase(junction2, Output, S) ->
    Reward = decision_reward(same_sign(Output, S#state.b), S#state.trials),
    NewCorrect = S#state.correct + correct(Output, S#state.b),
    next_trial(S#state.pairs, Reward, NewCorrect, S).

%% No pairs left: episode over. Solved if enough of the 2*trials decisions correct.
next_trial([], Reward, NewCorrect, S) ->
    Halt = solved_flag(NewCorrect >= round(2 * S#state.trials * ?SOLVE_RATIO)),
    {Reward, Halt, S#state{correct = NewCorrect}};
next_trial([{A, B} | Rest], Reward, NewCorrect, S) ->
    {Reward, 0, S#state{step = 0, a = A, b = B, pairs = Rest, correct = NewCorrect}}.

%%%============================================================================
%%% Internal
%%%============================================================================

%% Each of the 2*trials decisions is worth 50/trials, so a perfect run scores 100
%% and a memoryless run (half correct) scores exactly 50.
decision_reward(true, Trials) -> 50.0 / Trials;
decision_reward(false, _Trials) -> 0.0.

correct(Output, Cue) -> round(reward(same_sign(Output, Cue))).

reward(true) -> 1.0;
reward(false) -> 0.0.

solved_flag(true) -> goal_reached;
solved_flag(false) -> 1.

same_sign(A, B) -> (A >= 0) =:= (B >= 0).

%% All four (A,B) sign combinations in equal number, shuffled. Balance pins the
%% memoryless ceiling at exactly 50 (each junction sees + and - equally often).
balanced_pairs(N) ->
    Quarter = N div 4,
    Combos = [{1.0, 1.0}, {1.0, -1.0}, {-1.0, 1.0}, {-1.0, -1.0}],
    shuffle(lists:append([lists:duplicate(Quarter, C) || C <- Combos])).

shuffle(List) ->
    [X || {_K, X} <- lists:sort([{rand:uniform(), E} || E <- List])].

delay([D | _], _S) when is_integer(D) -> D;
delay(_Params, S) -> S#state.delay.
