%% @doc Brain system facade - unified API for neural network inference and learning.
%%
%% This module provides a high-level API for working with brain systems.
%% A brain system consists of:
%% - An inference process (brain.erl) for forward propagation
%% - An optional learner process (brain_learner.erl) for weight adaptation
%%
%% Both processes are supervised by brain_sup.erl.
%%
%% == Starting a Brain System ==
%%
%% Simple inference-only brain:
%%   Network = network_evaluator:create_feedforward(42, [16, 8], 6),
%%   {ok, Pid} = brain_system:start_link(#{network => Network})
%%
%% Brain with learning enabled:
%%   {ok, Pid} = brain_system:start_link(#{
%%       network => Network,
%%       learning_enabled => true,
%%       plasticity_rule => modulated
%%   })
%%
%% == Evaluation ==
%%
%% Evaluate with recording for later learning:
%%   Outputs = brain_system:evaluate(Pid, Inputs)
%%
%% == Learning ==
%%
%% Online learning (immediate weight updates):
%%   brain_system:reward(Pid, 1.0)  %% Positive reward
%%   brain_system:learn_step(Pid)   %% Apply learning
%%
%% Batch learning (from experience buffer):
%%   brain_system:start_recording(Pid)
%%   %% ... run episode ...
%%   brain_system:learn_from_experience(Pid, FinalReward)
%%
%% == Architecture ==
%%
%% brain_system (this module - facade)
%%     |
%%     v
%% brain_sup (supervisor)
%%     |
%%     +-- brain (inference)
%%     +-- brain_learner (learning, optional)
%%
%% @copyright 2024-2026 R.G. Lefever
-module(brain_system).

%% Lifecycle API
-export([
    start_link/1,
    start_link/2,
    stop/1
]).

%% Inference API (delegates to brain)
-export([
    evaluate/2,
    get_viz/1,
    get_topology/1,
    get_network/1,
    set_network/2,
    subscribe/1,
    subscribe/2,
    unsubscribe/1,
    unsubscribe/2
]).

%% Learning API (delegates to brain_learner)
-export([
    enable_learning/1,
    disable_learning/1,
    is_learning_enabled/1,
    reward/2,
    learn_step/1,
    learn_from_experience/1,
    learn_from_experience/2,
    start_recording/1,
    stop_recording/1,
    clear_experience/1,
    set_plasticity_rule/2,
    get_plasticity_rule/1,
    set_learning_rate/2,
    get_learning_rate/1
]).

%% System introspection
-export([
    get_inference_pid/1,
    get_learner_pid/1,
    get_brain_id/1,
    has_learner/1
]).

-record(brain_system, {
    sup_pid :: pid(),
    brain_id :: term(),
    inference_pid :: pid(),
    learner_pid :: pid() | undefined
}).

-opaque brain_system() :: #brain_system{}.
-export_type([brain_system/0]).

%%==============================================================================
%% Lifecycle API
%%==============================================================================

%% @doc Start a brain system.
%%
%% This starts the supervisor and all child processes, then connects
%% the learner to the inference process if learning is enabled.
%%
%% Options:
%% - `network' - The neural network (required)
%% - `learning_enabled' - Start learner process (default: false)
%% - `plasticity_rule' - Rule for learning (default: modulated)
%% - `learning_rate' - Learning rate (default: 0.01)
%% - `input_labels' - Labels for visualization
%% - `viz_enabled' - Enable visualization (default: true)
%%
%% @returns {ok, BrainSystem} where BrainSystem is an opaque handle
-spec start_link(map()) -> {ok, brain_system()} | {error, term()}.
start_link(Opts) ->
    case brain_sup:start_link(Opts) of
        {ok, SupPid} ->
            init_brain_system(SupPid, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start a named brain system.
-spec start_link(term(), map()) -> {ok, brain_system()} | {error, term()}.
start_link(Name, Opts) ->
    case brain_sup:start_link(Name, Opts) of
        {ok, SupPid} ->
            init_brain_system(SupPid, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop a brain system.
-spec stop(brain_system()) -> ok.
stop(#brain_system{sup_pid = SupPid}) ->
    _ = supervisor:terminate_child(SupPid, brain_learner),
    _ = supervisor:terminate_child(SupPid, brain),
    exit(SupPid, shutdown),
    ok.

%%==============================================================================
%% Inference API
%%==============================================================================

%% @doc Evaluate the brain with sensor inputs.
%%
%% The brain publishes an 'evaluated' event via pubsub after each evaluation.
%% If the learner has auto_record enabled, it will automatically record
%% experiences from these events.
-spec evaluate(brain_system(), [float()]) -> [float()].
evaluate(#brain_system{inference_pid = InfPid}, Inputs) ->
    brain:evaluate(InfPid, Inputs).

%% @doc Get visualization data.
-spec get_viz(brain_system()) -> map() | undefined.
get_viz(#brain_system{inference_pid = Pid}) ->
    brain:get_viz(Pid).

%% @doc Get network topology.
-spec get_topology(brain_system()) -> map().
get_topology(#brain_system{inference_pid = Pid}) ->
    brain:get_topology(Pid).

%% @doc Get the current network.
-spec get_network(brain_system()) -> network_evaluator:network().
get_network(#brain_system{inference_pid = Pid}) ->
    brain:get_network(Pid).

%% @doc Replace the network.
-spec set_network(brain_system(), network_evaluator:network()) -> ok.
set_network(#brain_system{inference_pid = Pid}, Network) ->
    brain:set_network(Pid, Network).

%% @doc Subscribe to visualization updates.
-spec subscribe(brain_system()) -> ok.
subscribe(#brain_system{inference_pid = Pid}) ->
    brain:subscribe(Pid).

%% @doc Subscribe a specific process to visualization updates.
-spec subscribe(brain_system(), pid()) -> ok.
subscribe(#brain_system{inference_pid = Pid}, SubscriberPid) ->
    brain:subscribe(Pid, SubscriberPid).

%% @doc Unsubscribe from visualization updates.
-spec unsubscribe(brain_system()) -> ok.
unsubscribe(#brain_system{inference_pid = Pid}) ->
    brain:unsubscribe(Pid).

%% @doc Unsubscribe a specific process.
-spec unsubscribe(brain_system(), pid()) -> ok.
unsubscribe(#brain_system{inference_pid = Pid}, SubscriberPid) ->
    brain:unsubscribe(Pid, SubscriberPid).

%%==============================================================================
%% Learning API
%%==============================================================================

%% @doc Enable learning.
-spec enable_learning(brain_system()) -> ok | {error, no_learner}.
enable_learning(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
enable_learning(#brain_system{learner_pid = Pid}) ->
    brain_learner:enable(Pid).

%% @doc Disable learning.
-spec disable_learning(brain_system()) -> ok | {error, no_learner}.
disable_learning(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
disable_learning(#brain_system{learner_pid = Pid}) ->
    brain_learner:disable(Pid).

%% @doc Check if learning is enabled.
-spec is_learning_enabled(brain_system()) -> boolean().
is_learning_enabled(#brain_system{learner_pid = undefined}) ->
    false;
is_learning_enabled(#brain_system{learner_pid = Pid}) ->
    brain_learner:is_enabled(Pid).

%% @doc Provide a reward signal.
%%
%% For modulated learning, positive rewards strengthen active connections,
%% negative rewards weaken them.
%%
%% This publishes a 'reward_received' event via pubsub, which the learner
%% receives and uses for future weight updates.
-spec reward(brain_system(), float()) -> ok | {error, no_learner}.
reward(#brain_system{learner_pid = undefined}, _Reward) ->
    {error, no_learner};
reward(#brain_system{brain_id = BrainId}, Reward) ->
    brain_pubsub:publish(BrainId, reward_received, #{reward => Reward}),
    ok.

%% @doc Perform a learning step using current reward.
%%
%% This applies the plasticity rule to update weights based on
%% the most recent experience and current reward.
-spec learn_step(brain_system()) -> {ok, non_neg_integer()} | {error, no_learner}.
learn_step(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
learn_step(#brain_system{learner_pid = Pid}) ->
    brain_learner:learn_from_experience(Pid).

%% @doc Learn from buffered experiences with current reward.
-spec learn_from_experience(brain_system()) -> {ok, non_neg_integer()} | {error, no_learner}.
learn_from_experience(System) ->
    learn_step(System).

%% @doc Learn from buffered experiences with a specific reward.
-spec learn_from_experience(brain_system(), float()) -> {ok, non_neg_integer()} | {error, no_learner}.
learn_from_experience(#brain_system{learner_pid = undefined}, _Reward) ->
    {error, no_learner};
learn_from_experience(#brain_system{learner_pid = Pid}, FinalReward) ->
    brain_learner:learn_from_experience(Pid, FinalReward).

%% @doc Start recording experiences for batch learning.
%%
%% This enables auto_record in the learner, which will automatically
%% record experiences from 'evaluated' events published by the brain.
-spec start_recording(brain_system()) -> ok | {error, no_learner}.
start_recording(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
start_recording(#brain_system{learner_pid = Pid}) ->
    brain_learner:set_auto_record(Pid, true).

%% @doc Stop recording experiences.
%%
%% This disables auto_record in the learner.
-spec stop_recording(brain_system()) -> ok | {error, no_learner}.
stop_recording(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
stop_recording(#brain_system{learner_pid = Pid}) ->
    brain_learner:set_auto_record(Pid, false).

%% @doc Clear the experience buffer.
-spec clear_experience(brain_system()) -> ok | {error, no_learner}.
clear_experience(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
clear_experience(#brain_system{learner_pid = Pid}) ->
    brain_learner:clear_experience(Pid).

%% @doc Set the plasticity rule.
-spec set_plasticity_rule(brain_system(), atom()) -> ok | {error, no_learner}.
set_plasticity_rule(#brain_system{learner_pid = undefined}, _Rule) ->
    {error, no_learner};
set_plasticity_rule(#brain_system{learner_pid = Pid}, Rule) ->
    brain_learner:set_plasticity_rule(Pid, Rule).

%% @doc Get the current plasticity rule.
-spec get_plasticity_rule(brain_system()) -> atom() | {error, no_learner}.
get_plasticity_rule(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
get_plasticity_rule(#brain_system{learner_pid = Pid}) ->
    brain_learner:get_plasticity_rule(Pid).

%% @doc Set the learning rate.
-spec set_learning_rate(brain_system(), float()) -> ok | {error, no_learner}.
set_learning_rate(#brain_system{learner_pid = undefined}, _Rate) ->
    {error, no_learner};
set_learning_rate(#brain_system{learner_pid = Pid}, Rate) ->
    brain_learner:set_learning_rate(Pid, Rate).

%% @doc Get the current learning rate.
-spec get_learning_rate(brain_system()) -> float() | {error, no_learner}.
get_learning_rate(#brain_system{learner_pid = undefined}) ->
    {error, no_learner};
get_learning_rate(#brain_system{learner_pid = Pid}) ->
    brain_learner:get_learning_rate(Pid).

%%==============================================================================
%% System Introspection
%%==============================================================================

%% @doc Get the inference process PID.
-spec get_inference_pid(brain_system()) -> pid().
get_inference_pid(#brain_system{inference_pid = Pid}) ->
    Pid.

%% @doc Get the learner process PID.
-spec get_learner_pid(brain_system()) -> pid() | undefined.
get_learner_pid(#brain_system{learner_pid = Pid}) ->
    Pid.

%% @doc Get the brain ID used for pubsub communication.
-spec get_brain_id(brain_system()) -> term().
get_brain_id(#brain_system{brain_id = BrainId}) ->
    BrainId.

%% @doc Check if the system has a learner.
-spec has_learner(brain_system()) -> boolean().
has_learner(#brain_system{learner_pid = Pid}) ->
    Pid =/= undefined.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Initialize the brain system after supervisor starts
%%
%% The learner and inference processes communicate via pubsub (brain_pubsub),
%% using the brain_id as the namespace. No direct connection is needed.
init_brain_system(SupPid, Opts) ->
    %% Get child PIDs
    InferencePid = brain_sup:get_inference_pid(SupPid),
    LearnerPid = brain_sup:get_learner_pid(SupPid),

    %% Get the brain_id that was used by the supervisor
    BrainId = maps:get(id, Opts, make_ref()),

    System = #brain_system{
        sup_pid = SupPid,
        brain_id = BrainId,
        inference_pid = InferencePid,
        learner_pid = LearnerPid
    },

    {ok, System}.
