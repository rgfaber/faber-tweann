%% @doc Probabilistic reversal bandit: the FAIR lifetime-learning contest.
%%
%% Like reversal_bandit_sim, but (a) reward is NOISY -- the good arm pays +1 with
%% probability PHi (e.g. 0.8) and -1 otherwise, the bad arm the reverse -- so a
%% single trial is uninformative and the agent must INTEGRATE evidence over trials;
%% and (b) the previous trial's reward and action are fed back as SENSOR inputs, so a
%% recurrent STATE can compete (RL^2-style meta-learning) on equal footing with
%% reward-gated plasticity. The noise defeats a reactive win-stay-lose-shift, so this
%% genuinely tests learning, not a one-step reflex.
%%
%% Sensor (vl 3): [LastReward, LastAction, 1.0].
%%   LastReward  -1.0 / +1.0 (0.0 on the first trial)
%%   LastAction  +1.0 if the last choice was arm 0, -1.0 if arm 1 (0.0 first trial)
%%   1.0         constant bias.
%% Actuator (vl 1): Output >= 0 chooses arm 0, < 0 chooses arm 1.
%% Reward / modulator: +1.0 / -1.0 as above (also the neuromodulator for plasticity).
%%
%% Reward is drawn deterministically from a hash of (FlipSeed, Trial, Arm), so the
%% environment's "coin flips" are FIXED per instance: two agents that make the same
%% choice at the same trial get the same reward (common random numbers), and the draw
%% never touches the global RNG the evolver uses. Fully determined by its init params
%% [GoodArm, ReversalAt, Lifetime, FlipSeed, PHi].
%%
%% Baselines: a fixed policy scores chance across a balanced instance set; an agent
%% that integrates reward and adapts (in state or in weights), and re-adapts after the
%% reversal, approaches the optimal mean reward 2*PHi - 1 per trial.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(prob_reversal_bandit_sim).

-behaviour(scape).

-export([init/1, sense/3, act/4]).

-record(state, {
    good_arm = 0 :: 0 | 1,
    reversal_at = 30 :: non_neg_integer(),
    lifetime = 60 :: pos_integer(),
    p_hi = 0.8 :: float(),
    flip_seed = 0 :: integer(),
    trial = 0 :: non_neg_integer(),
    last_reward = 0.0 :: float(),
    last_action = 0.0 :: float()
}).

init([GoodArm, ReversalAt, Lifetime, FlipSeed, PHi]) ->
    #state{good_arm = GoodArm, reversal_at = ReversalAt, lifetime = Lifetime,
           flip_seed = FlipSeed, p_hi = PHi};
init(_Params) ->
    #state{}.

%% Sense: the previous trial's reward and action, plus a bias. This feedback is what
%% lets a recurrent state (or a plastic rule) learn which arm is currently paying.
sense(_SensorName, _Params, S) ->
    {[S#state.last_reward, S#state.last_action, 1.0], S}.

%% Act: choose an arm, draw its (fixed) noisy reward against the currently-good arm,
%% remember the reward and action for the next observation, and advance.
act(_ActuatorName, _Params, Output, S) when is_list(Output) ->
    Arm = action_of(lists:sum(Output)),
    Paid = paid(S#state.flip_seed, S#state.trial, Arm, Arm =:= current_good(S), S#state.p_hi),
    Reward = reward(Paid),
    S1 = S#state{last_reward = Reward, last_action = action_signed(Arm)},
    advance(S#state.trial + 1, S#state.lifetime, Reward, S1).

%%%============================================================================
%%% Internal
%%%============================================================================

action_of(X) when X >= 0 -> 0;
action_of(_X) -> 1.

action_signed(0) -> 1.0;
action_signed(1) -> -1.0.

reward(true) -> 1.0;
reward(false) -> -1.0.

current_good(#state{good_arm = G, reversal_at = R, trial = T}) when T >= R -> 1 - G;
current_good(#state{good_arm = G}) -> G.

%% Deterministic noisy payout: the chosen arm pays iff a fixed hashed uniform draw
%% falls under this arm's pay probability (PHi if currently good, 1-PHi if bad).
paid(FlipSeed, Trial, Arm, IsGood, PHi) ->
    U = (erlang:phash2({FlipSeed, Trial, Arm}) rem 1000000) / 1000000,
    U < pay_prob(IsGood, PHi).

pay_prob(true, PHi) -> PHi;
pay_prob(false, PHi) -> 1.0 - PHi.

advance(Trial, Lifetime, Reward, S) when Trial >= Lifetime ->
    {Reward, done, S#state{trial = Trial}};
advance(Trial, _Lifetime, Reward, S) ->
    {Reward, 0, S#state{trial = Trial}}.
