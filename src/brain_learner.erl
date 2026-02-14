%% @doc Brain learner process for weight adaptation via plasticity.
%%
%% This GenServer manages the learning aspects of a brain system:
%% - Applies plasticity rules to update weights based on neural activity
%% - Maintains an experience buffer for batch learning
%% - Handles reward signals for reinforcement-style learning
%%
%% == Online Learning ==
%%
%% When online learning is enabled, the learner receives activation
%% data after each inference and applies plasticity rules:
%%
%%   Inference → Activations → Learner → Weight Updates → Back to Inference
%%
%% == Batch Learning ==
%%
%% For delayed rewards (e.g., end of game), the learner buffers
%% experiences and applies learning when a reward is received:
%%
%%   1. Record experiences during episode
%%   2. Receive final reward
%%   3. Apply learning with reward propagation (eligibility traces)
%%
%% == Theory ==
%%
%% This module implements reward-modulated Hebbian learning, where
%% weight changes depend on:
%%   - Pre-synaptic activity (input to connection)
%%   - Post-synaptic activity (output from connection)
%%   - Global reward signal (from environment)
%%
%% The basic rule: delta_w = learning_rate * pre * post * reward
%%
%% For delayed rewards, eligibility traces track which synapses
%% were recently active, allowing credit assignment across time.
%%
%% @see plasticity
%% @see plasticity_modulated
%% @copyright 2024-2026 R.G. Lefever
-module(brain_learner).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/1,
    %% Learning control
    enable/1,
    disable/1,
    is_enabled/1,
    %% Reward handling
    reward/2,
    set_baseline_reward/2,
    %% Experience buffer
    record_experience/3,
    clear_experience/1,
    learn_from_experience/1,
    learn_from_experience/2,
    get_experience_count/1,
    set_auto_record/2,
    get_auto_record/1,
    %% Configuration
    set_plasticity_rule/2,
    get_plasticity_rule/1,
    set_learning_rate/2,
    get_learning_rate/1,
    %% Weight access
    get_weight_deltas/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    %% Brain ID for pubsub communication
    brain_id :: term(),
    %% Current network weights (updated via pubsub)
    current_weights :: [float()] | undefined,
    %% Learning configuration
    enabled :: boolean(),
    plasticity_rule :: atom(),
    learning_rate :: float(),
    baseline_reward :: float(),
    %% Experience buffer for batch learning
    experience_buffer :: [experience()],
    max_buffer_size :: pos_integer(),
    %% Current reward (for online learning)
    current_reward :: float(),
    %% Accumulated weight deltas (for inspection/debugging)
    weight_deltas :: [float()],
    %% Auto-record experiences from pubsub evaluated events
    auto_record :: boolean()
}).

-type experience() :: #{
    inputs := [float()],
    activations := [[float()]],
    outputs := [float()],
    timestamp := integer()
}.

-export_type([experience/0]).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Start a brain learner process.
%%
%% Options:
%% - `inference_pid' - PID of the brain inference process (required for weight updates)
%% - `enabled' - Whether learning is enabled (default: true)
%% - `plasticity_rule' - Atom identifying the rule (default: modulated)
%% - `learning_rate' - Learning rate (default: 0.01)
%% - `baseline_reward' - Baseline to subtract from rewards (default: 0.0)
%% - `max_buffer_size' - Max experiences to buffer (default: 1000)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the learner process.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Enable learning.
-spec enable(pid()) -> ok.
enable(Pid) ->
    gen_server:call(Pid, enable).

%% @doc Disable learning.
-spec disable(pid()) -> ok.
disable(Pid) ->
    gen_server:call(Pid, disable).

%% @doc Check if learning is enabled.
-spec is_enabled(pid()) -> boolean().
is_enabled(Pid) ->
    gen_server:call(Pid, is_enabled).

%% @doc Provide a reward signal.
%%
%% For online learning, this affects the next weight update.
%% Positive rewards strengthen active connections, negative weaken them.
%%
%% @param Pid Learner process
%% @param Reward Reward value (typically -1.0 to 1.0)
-spec reward(pid(), float()) -> ok.
reward(Pid, Reward) ->
    gen_server:cast(Pid, {reward, Reward}).

%% @doc Set the baseline reward for comparison.
%%
%% Effective reward = actual_reward - baseline_reward.
%% This helps with reward normalization.
-spec set_baseline_reward(pid(), float()) -> ok.
set_baseline_reward(Pid, Baseline) ->
    gen_server:call(Pid, {set_baseline_reward, Baseline}).

%% @doc Record an experience for batch learning.
%%
%% @param Pid Learner process
%% @param Inputs The sensor inputs
%% @param Activations All layer activations from the inference
-spec record_experience(pid(), [float()], [[float()]]) -> ok.
record_experience(Pid, Inputs, Activations) ->
    gen_server:cast(Pid, {record_experience, Inputs, Activations}).

%% @doc Clear the experience buffer.
-spec clear_experience(pid()) -> ok.
clear_experience(Pid) ->
    gen_server:call(Pid, clear_experience).

%% @doc Learn from buffered experiences using current reward.
-spec learn_from_experience(pid()) -> {ok, non_neg_integer()}.
learn_from_experience(Pid) ->
    gen_server:call(Pid, learn_from_experience).

%% @doc Learn from buffered experiences with a specific reward.
%%
%% @param Pid Learner process
%% @param FinalReward Reward to apply (e.g., end-of-episode reward)
%% @returns {ok, NumExperiencesProcessed}
-spec learn_from_experience(pid(), float()) -> {ok, non_neg_integer()}.
learn_from_experience(Pid, FinalReward) ->
    gen_server:call(Pid, {learn_from_experience, FinalReward}).

%% @doc Get the number of buffered experiences.
-spec get_experience_count(pid()) -> non_neg_integer().
get_experience_count(Pid) ->
    gen_server:call(Pid, get_experience_count).

%% @doc Enable or disable automatic experience recording.
%%
%% When enabled, the learner automatically records experiences from
%% 'evaluated' events published by the brain via pubsub.
-spec set_auto_record(pid(), boolean()) -> ok.
set_auto_record(Pid, Enabled) ->
    gen_server:call(Pid, {set_auto_record, Enabled}).

%% @doc Check if automatic experience recording is enabled.
-spec get_auto_record(pid()) -> boolean().
get_auto_record(Pid) ->
    gen_server:call(Pid, get_auto_record).

%% @doc Set the plasticity rule.
%%
%% Available rules: none, hebbian, modulated
-spec set_plasticity_rule(pid(), atom()) -> ok.
set_plasticity_rule(Pid, Rule) ->
    gen_server:call(Pid, {set_plasticity_rule, Rule}).

%% @doc Get the current plasticity rule.
-spec get_plasticity_rule(pid()) -> atom().
get_plasticity_rule(Pid) ->
    gen_server:call(Pid, get_plasticity_rule).

%% @doc Set the learning rate.
-spec set_learning_rate(pid(), float()) -> ok.
set_learning_rate(Pid, Rate) ->
    gen_server:call(Pid, {set_learning_rate, Rate}).

%% @doc Get the current learning rate.
-spec get_learning_rate(pid()) -> float().
get_learning_rate(Pid) ->
    gen_server:call(Pid, get_learning_rate).

%% @doc Get accumulated weight deltas from last learning step.
%%
%% Useful for debugging and visualization.
-spec get_weight_deltas(pid()) -> [float()].
get_weight_deltas(Pid) ->
    gen_server:call(Pid, get_weight_deltas).

%%==============================================================================
%% gen_server Callbacks
%%==============================================================================

init(Opts) ->
    BrainId = maps:get(brain_id, Opts),
    Enabled = maps:get(enabled, Opts, true),
    PlasticityRule = maps:get(plasticity_rule, Opts, modulated),
    LearningRate = maps:get(learning_rate, Opts, 0.01),
    BaselineReward = maps:get(baseline_reward, Opts, 0.0),
    MaxBufferSize = maps:get(max_buffer_size, Opts, 1000),
    AutoRecord = maps:get(auto_record, Opts, true),

    %% Subscribe to evaluated events from brain
    brain_pubsub:subscribe(BrainId, evaluated),
    %% Subscribe to reward events from brain_system
    brain_pubsub:subscribe(BrainId, reward_received),
    %% Request current weights from brain
    brain_pubsub:subscribe(BrainId, weights_response),
    brain_pubsub:publish(BrainId, weights_requested, #{}),

    State = #state{
        brain_id = BrainId,
        current_weights = undefined,
        enabled = Enabled,
        plasticity_rule = PlasticityRule,
        learning_rate = LearningRate,
        baseline_reward = BaselineReward,
        experience_buffer = [],
        max_buffer_size = MaxBufferSize,
        current_reward = 0.0,
        weight_deltas = [],
        auto_record = AutoRecord
    },

    {ok, State}.

handle_call(enable, _From, State) ->
    {reply, ok, State#state{enabled = true}};

handle_call(disable, _From, State) ->
    {reply, ok, State#state{enabled = false}};

handle_call(is_enabled, _From, State) ->
    {reply, State#state.enabled, State};

handle_call({set_baseline_reward, Baseline}, _From, State) ->
    {reply, ok, State#state{baseline_reward = Baseline}};

handle_call(clear_experience, _From, State) ->
    {reply, ok, State#state{experience_buffer = []}};

handle_call(learn_from_experience, _From, State) ->
    {NumProcessed, NewState} = do_learn_from_experience(State, State#state.current_reward),
    {reply, {ok, NumProcessed}, NewState};

handle_call({learn_from_experience, FinalReward}, _From, State) ->
    {NumProcessed, NewState} = do_learn_from_experience(State, FinalReward),
    {reply, {ok, NumProcessed}, NewState};

handle_call(get_experience_count, _From, State) ->
    {reply, length(State#state.experience_buffer), State};

handle_call({set_plasticity_rule, Rule}, _From, State) ->
    {reply, ok, State#state{plasticity_rule = Rule}};

handle_call(get_plasticity_rule, _From, State) ->
    {reply, State#state.plasticity_rule, State};

handle_call({set_learning_rate, Rate}, _From, State) ->
    {reply, ok, State#state{learning_rate = Rate}};

handle_call(get_learning_rate, _From, State) ->
    {reply, State#state.learning_rate, State};

handle_call(get_weight_deltas, _From, State) ->
    {reply, State#state.weight_deltas, State};

handle_call({set_auto_record, Enabled}, _From, State) ->
    {reply, ok, State#state{auto_record = Enabled}};

handle_call(get_auto_record, _From, State) ->
    {reply, State#state.auto_record, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({reward, Reward}, State) ->
    {noreply, State#state{current_reward = Reward}};

handle_cast({record_experience, Inputs, Activations}, State) ->
    #state{
        experience_buffer = Buffer,
        max_buffer_size = MaxSize
    } = State,

    Experience = #{
        inputs => Inputs,
        activations => Activations,
        outputs => lists:last(Activations),
        timestamp => erlang:system_time(millisecond)
    },

    %% Add to buffer, trimming if necessary
    NewBuffer = case length(Buffer) >= MaxSize of
        true -> [Experience | lists:droplast(Buffer)];
        false -> [Experience | Buffer]
    end,

    {noreply, State#state{experience_buffer = NewBuffer}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle evaluated events from brain (auto-record experiences)
handle_info({brain_event, evaluated, EventData}, State) ->
    #state{
        auto_record = AutoRecord,
        experience_buffer = Buffer,
        max_buffer_size = MaxSize
    } = State,

    NewState = case AutoRecord of
        true ->
            #{inputs := Inputs, activations := Activations, outputs := Outputs} = EventData,
            Experience = #{
                inputs => Inputs,
                activations => Activations,
                outputs => Outputs,
                timestamp => erlang:system_time(millisecond)
            },
            NewBuffer = case length(Buffer) >= MaxSize of
                true -> [Experience | lists:droplast(Buffer)];
                false -> [Experience | Buffer]
            end,
            State#state{experience_buffer = NewBuffer};
        false ->
            State
    end,
    {noreply, NewState};

%% Handle reward events from brain_system
handle_info({brain_event, reward_received, #{reward := Reward}}, State) ->
    {noreply, State#state{current_reward = Reward}};

%% Handle weights response (for initial sync)
handle_info({brain_event, weights_response, #{weights := Weights}}, State) ->
    {noreply, State#state{current_weights = Weights}};

%% Ignore other pubsub events
handle_info({brain_event, _Topic, _Data}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup pubsub subscriptions
    #state{brain_id = BrainId} = State,
    brain_pubsub:cleanup(BrainId),
    ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Process buffered experiences and apply learning
do_learn_from_experience(State, _Reward) when State#state.enabled == false ->
    {0, State};
do_learn_from_experience(State, _Reward) when State#state.experience_buffer == [] ->
    {0, State};
do_learn_from_experience(State, _Reward) when State#state.current_weights == undefined ->
    %% No weights available yet, can't learn
    {0, State};
do_learn_from_experience(State, Reward) ->
    #state{
        brain_id = BrainId,
        current_weights = Weights,
        experience_buffer = Buffer,
        plasticity_rule = Rule,
        learning_rate = LearningRate,
        baseline_reward = Baseline
    } = State,

    %% Calculate effective reward
    EffectiveReward = Reward - Baseline,

    %% Convert flat weights to weight_spec format for plasticity
    WeightSpecs = weights_to_specs(Weights, LearningRate),

    %% Apply learning from each experience
    {UpdatedSpecs, Deltas} = apply_learning_to_experiences(
        WeightSpecs,
        lists:reverse(Buffer),  %% Oldest first
        Rule,
        EffectiveReward
    ),

    %% Convert back to flat weights
    NewWeights = specs_to_weights(UpdatedSpecs),

    %% Publish weights_updated event for brain to apply
    brain_pubsub:publish(BrainId, weights_updated, #{
        weights => NewWeights,
        deltas => Deltas
    }),

    NumProcessed = length(Buffer),
    NewState = State#state{
        experience_buffer = [],
        current_weights = NewWeights,  %% Update our copy too
        weight_deltas = Deltas
    },
    {NumProcessed, NewState}.

%% @private Convert flat weights to weight_spec tuples
weights_to_specs(Weights, LearningRate) ->
    [{W, 0.0, LearningRate, []} || W <- Weights].

%% @private Convert weight_spec tuples back to flat weights
specs_to_weights(Specs) ->
    [plasticity:get_weight(Spec) || Spec <- Specs].

%% @private Apply learning across all experiences
apply_learning_to_experiences(WeightSpecs, [], _Rule, _Reward) ->
    Deltas = [plasticity:get_delta(Spec) || Spec <- WeightSpecs],
    {WeightSpecs, Deltas};
apply_learning_to_experiences(WeightSpecs, [Experience | Rest], Rule, Reward) ->
    #{activations := Activations} = Experience,

    %% Apply plasticity rule to each weight based on activations
    UpdatedSpecs = apply_plasticity_to_weights(WeightSpecs, Activations, Rule, Reward),

    apply_learning_to_experiences(UpdatedSpecs, Rest, Rule, Reward).

%% @private Apply plasticity rule to weights based on layer activations
%%
%% This maps the flat weight list to the layer structure and applies
%% the plasticity rule based on pre/post activations.
apply_plasticity_to_weights(WeightSpecs, Activations, Rule, Reward) ->
    RuleModule = plasticity:rule_module(Rule),

    %% For a feedforward network, weights connect adjacent layers
    %% We need to pair pre-activations (layer N) with post-activations (layer N+1)
    NumLayers = length(Activations),

    case NumLayers < 2 of
        true ->
            %% Not enough layers for learning
            WeightSpecs;
        false ->
            %% Build list of {PreActivations, PostActivations} pairs
            LayerPairs = lists:zip(
                lists:droplast(Activations),  %% Pre: all but last
                tl(Activations)               %% Post: all but first
            ),

            %% Apply plasticity to each weight
            %% Weights are organized: [layer1_weights..., layer2_weights..., ...]
            %% Each layer has: [w11, w12, ..., w1n, w21, ..., bias1, bias2, ...]
            apply_plasticity_by_layer(WeightSpecs, LayerPairs, RuleModule, Reward)
    end.

%% @private Apply plasticity layer by layer
apply_plasticity_by_layer(WeightSpecs, [], _RuleModule, _Reward) ->
    WeightSpecs;
apply_plasticity_by_layer(WeightSpecs, [{PreActs, PostActs} | RestPairs], RuleModule, Reward) ->
    PreSize = length(PreActs),
    PostSize = length(PostActs),

    %% Number of weights in this layer (excluding biases)
    WeightCount = PreSize * PostSize,
    BiasCount = PostSize,
    TotalCount = WeightCount + BiasCount,

    %% Split weights for this layer
    {LayerSpecs, RestSpecs} = lists:split(TotalCount, WeightSpecs),
    {ConnectionSpecs, BiasSpecs} = lists:split(WeightCount, LayerSpecs),

    %% Apply plasticity to connection weights
    %% Weights are stored as: [w(1,1), w(1,2), ..., w(1,post), w(2,1), ...]
    %% where w(pre_idx, post_idx)
    UpdatedConnections = apply_plasticity_to_connections(
        ConnectionSpecs, PreActs, PostActs, RuleModule, Reward
    ),

    %% Biases: use post-activation only (no pre-synaptic activity)
    UpdatedBiases = lists:zipwith(
        fun(BiasSpec, PostAct) ->
            RuleModule:apply_rule(BiasSpec, 1.0, PostAct, Reward)
        end,
        BiasSpecs,
        PostActs
    ),

    %% Combine and continue with next layer
    UpdatedLayerSpecs = UpdatedConnections ++ UpdatedBiases,
    apply_plasticity_by_layer(UpdatedLayerSpecs ++ RestSpecs, RestPairs, RuleModule, Reward).

%% @private Apply plasticity to connection weights
apply_plasticity_to_connections(ConnectionSpecs, PreActs, PostActs, RuleModule, Reward) ->
    PreSize = length(PreActs),
    PostSize = length(PostActs),

    %% Map over all connections
    lists:zipwith(
        fun(Spec, Idx) ->
            %% Calculate which pre/post neurons this weight connects
            PreIdx = ((Idx - 1) rem PreSize) + 1,
            PostIdx = ((Idx - 1) div PreSize) + 1,
            PreAct = lists:nth(PreIdx, PreActs),
            PostAct = lists:nth(PostIdx, PostActs),
            RuleModule:apply_rule(Spec, PreAct, PostAct, Reward)
        end,
        ConnectionSpecs,
        lists:seq(1, PreSize * PostSize)
    ).
