%% @doc Reversal bandit: a lifetime-LEARNING benchmark (learning to learn).
%%
%% Where tmaze_sim / multi_cue_tmaze_sim test MEMORY within one episode (hold a cue
%% you were shown), this tests LEARNING across an episode (discover, from reward, a
%% mapping you were never shown, and re-discover it when it changes). Two arms; one
%% is "good". The agent is told NOTHING about which -- its only signal is the reward
%% it receives after each choice. Partway through the lifetime the good arm SWAPS, so
%% no fixed policy wins: the agent must learn which arm pays, then RE-learn after the
%% reversal.
%%
%% Sensor (vl 1): a constant [1.0] -- there is no cue to remember; the information
%%   lives entirely in the reward stream.
%% Actuator (vl 1): Output >= 0 chooses arm 0, < 0 chooses arm 1.
%% Reward / modulator: +1.0 if the chosen arm is currently good, -1.0 otherwise. The
%%   SAME value is the fitness contribution AND the neuromodulator a reward-gated
%%   plasticity rule uses to adapt (see network_evaluator:evaluate_with_neuromod/4).
%%
%% Why it is the right instrument: with the good arm hidden and reversing, a FIXED
%% network emits a constant action and is right only half the time across a balanced
%% set of instances -- a lifetime reward sum near 0 (chance). An agent that reads the
%% reward and adapts its weights within the lifetime reaches near +Lifetime, losing
%% only the few exploratory trials at the start and just after the reversal. Storage
%% (a recurrent state) can only compete if the reward is fed as an INPUT; here it is
%% not, so this scape isolates reward-GATED plasticity as the learning mechanism.
%%
%% Fully deterministic given its init params [GoodArm, ReversalAt, Lifetime], so a
%% runner can enumerate a fixed set of instances (common random numbers) and average
%% -- no fitness noise. The probabilistic variant (which also admits a storage
%% competitor) is a separate scape.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(reversal_bandit_sim).

-behaviour(scape).

-export([init/1, sense/3, act/4]).

-record(state, {
    good_arm = 0 :: 0 | 1,
    reversal_at = 20 :: non_neg_integer(),
    lifetime = 40 :: pos_integer(),
    trial = 0 :: non_neg_integer()
}).

init([GoodArm, ReversalAt, Lifetime]) ->
    #state{good_arm = GoodArm, reversal_at = ReversalAt, lifetime = Lifetime};
init(_Params) ->
    #state{}.

%% Sense: a constant input. The agent has no cue; it must learn from reward.
sense(_SensorName, _Params, S) ->
    {[1.0], S}.

%% Act: choose an arm, score it against the currently-good arm (which has flipped
%% iff the reversal has been reached), and advance. The returned value is both the
%% fitness contribution and the neuromodulator for a reward-gated plasticity rule.
act(_ActuatorName, _Params, Output, S) when is_list(Output) ->
    Reward = reward(action_of(lists:sum(Output)) =:= current_good(S)),
    advance(S#state.trial + 1, S#state.lifetime, Reward, S).

%%%============================================================================
%%% Internal
%%%============================================================================

action_of(X) when X >= 0 -> 0;
action_of(_X) -> 1.

reward(true) -> 1.0;
reward(false) -> -1.0.

%% The good arm flips once the reversal trial is reached.
current_good(#state{good_arm = G, reversal_at = R, trial = T}) when T >= R -> 1 - G;
current_good(#state{good_arm = G}) -> G.

advance(Trial, Lifetime, Reward, S) when Trial >= Lifetime ->
    {Reward, done, S#state{trial = Trial}};
advance(Trial, _Lifetime, Reward, S) ->
    {Reward, 0, S#state{trial = Trial}}.
