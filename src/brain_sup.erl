%% @doc Supervisor for brain system components.
%%
%% This supervisor manages the inference and learning processes that
%% together form a brain system. The supervision strategy is one_for_all
%% because learning depends on inference state.
%%
%% == Architecture ==
%%
%% brain_sup (one_for_all)
%%     |
%%     +-- brain (inference GenServer)
%%     |       Forward propagation
%%     |       Visualization data
%%     |       Network state management
%%     |       Publishes: evaluated
%%     |
%%     +-- brain_learner (learning GenServer) [optional]
%%             Plasticity rule application
%%             Experience buffer
%%             Weight updates
%%             Subscribes to: evaluated, reward_received
%%             Publishes: weights_updated
%%
%% == Communication ==
%%
%% Subsystems communicate via brain_pubsub (built on OTP pg):
%% - brain publishes evaluated after each forward pass
%% - brain_learner subscribes to receive activations
%% - brain_learner publishes weights_updated after learning
%% - brain subscribes to update its network weights
%%
%% == Future Expansion ==
%%
%% This supervisor is designed to accommodate future brain subsystems:
%% - brain_memory: Episodic memory storage
%% - brain_attention: Focus and salience control
%% - brain_prediction: Forward model / world model
%%
%% @copyright 2024-2026 R.G. Lefever
-module(brain_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/1,
    start_link/2,
    get_inference_pid/1,
    get_learner_pid/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP_FLAGS, #{
    strategy => one_for_all,
    intensity => 3,
    period => 5
}).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Start an unnamed brain supervisor.
%%
%% Options:
%% - `network' - The neural network (required)
%% - `learning_enabled' - Start learner process (default: false)
%% - `plasticity_rule' - Rule for learning (default: modulated)
%% - `learning_rate' - Learning rate (default: 0.01)
%% - `input_labels' - Labels for visualization
%% - `viz_enabled' - Enable visualization (default: true)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

%% @doc Start a named brain supervisor.
-spec start_link(term(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) ->
    supervisor:start_link(Name, ?MODULE, Opts).

%% @doc Get the PID of the inference process.
-spec get_inference_pid(pid()) -> pid() | undefined.
get_inference_pid(SupPid) ->
    find_child(SupPid, brain).

%% @doc Get the PID of the learner process.
-spec get_learner_pid(pid()) -> pid() | undefined.
get_learner_pid(SupPid) ->
    find_child(SupPid, brain_learner).

%%==============================================================================
%% Supervisor Callbacks
%%==============================================================================

init(Opts) ->
    %% Extract options
    Network = maps:get(network, Opts),
    LearningEnabled = maps:get(learning_enabled, Opts, false),
    InputLabels = maps:get(input_labels, Opts, []),
    VizEnabled = maps:get(viz_enabled, Opts, true),
    BrainId = maps:get(id, Opts, make_ref()),

    %% Initialize pubsub for this brain
    brain_pubsub:init(BrainId),

    %% Build inference child spec
    %% brain_id is passed so inference can publish/subscribe to events
    InferenceOpts = #{
        network => Network,
        id => BrainId,
        brain_id => BrainId,
        input_labels => InputLabels,
        viz_enabled => VizEnabled
    },

    InferenceSpec = #{
        id => brain,
        start => {brain, start_link, [InferenceOpts]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [brain]
    },

    %% Optionally add learner child spec
    ChildSpecs = case LearningEnabled of
        true ->
            PlasticityRule = maps:get(plasticity_rule, Opts, modulated),
            LearningRate = maps:get(learning_rate, Opts, 0.01),
            BaselineReward = maps:get(baseline_reward, Opts, 0.0),
            MaxBufferSize = maps:get(max_buffer_size, Opts, 1000),

            %% Learner uses pubsub for communication with inference
            LearnerOpts = #{
                brain_id => BrainId,
                enabled => true,
                plasticity_rule => PlasticityRule,
                learning_rate => LearningRate,
                baseline_reward => BaselineReward,
                max_buffer_size => MaxBufferSize
            },

            LearnerSpec = #{
                id => brain_learner,
                start => {brain_learner, start_link, [LearnerOpts]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [brain_learner]
            },

            [InferenceSpec, LearnerSpec];
        false ->
            [InferenceSpec]
    end,

    {ok, {?SUP_FLAGS, ChildSpecs}}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Find a child process by ID
find_child(SupPid, ChildId) ->
    Children = supervisor:which_children(SupPid),
    case lists:keyfind(ChildId, 1, Children) of
        {ChildId, Pid, _Type, _Modules} when is_pid(Pid) ->
            Pid;
        _ ->
            undefined
    end.
