%% @doc Brain process for real-time neural network inference.
%%
%% A brain is a running neural network that accepts sensor inputs and
%% produces actuator outputs. Unlike the process-per-neuron architecture
%% used during training, this is a single GenServer optimized for
%% real-time inference in applications like games and robotics.
%%
%% == Features ==
%%
%% - Synchronous evaluation via evaluate/2
%% - Internal state persistence for LTC neurons
%% - Visualization data for UI rendering
%% - PubSub notifications for state changes
%%
%% == Usage ==
%%
%% Start a brain with a network:
%%   Network = network_evaluator:create_feedforward(42, [16, 8], 6),
%%   {ok, Pid} = brain:start_link(#{network => Network})
%%
%% Evaluate with sensor inputs:
%%   Outputs = brain:evaluate(Pid, Inputs)
%%
%% Get visualization data:
%%   VizData = brain:get_viz(Pid)
%%
%% Subscribe to viz updates (Erlang):
%%   brain:subscribe(Pid)
%%   receive {brain_viz, Pid, VizData} -> ... end
%%
%% @copyright 2024-2026 R.G. Lefever
-module(brain).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    evaluate/2,
    evaluate_with_activations/2,
    get_viz/1,
    get_topology/1,
    get_network/1,
    set_network/2,
    subscribe/1,
    subscribe/2,
    unsubscribe/1,
    unsubscribe/2
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
    id :: term(),
    brain_id :: term(),  %% For pubsub communication
    network :: network_evaluator:network(),
    last_inputs :: [float()] | undefined,
    last_outputs :: [float()] | undefined,
    last_activations :: [[float()]] | undefined,
    subscribers :: #{pid() => reference()},
    input_labels :: [binary()],
    viz_enabled :: boolean()
}).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Start a brain process.
%%
%% Options:
%% - `network' - The neural network (required, from network_evaluator)
%% - `id' - Optional identifier for this brain
%% - `input_labels' - Labels for input neurons (for visualization)
%% - `viz_enabled' - Enable visualization data (default: true)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start a named brain process.
-spec start_link(term(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) ->
    gen_server:start_link(Name, ?MODULE, Opts, []).

%% @doc Stop the brain process.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Evaluate the brain with sensor inputs.
%%
%% Returns the output values from the neural network.
%% Also updates internal state and notifies subscribers.
-spec evaluate(pid(), [float()]) -> [float()].
evaluate(Pid, Inputs) ->
    gen_server:call(Pid, {evaluate, Inputs}).

%% @doc Evaluate and return both outputs and all layer activations.
%%
%% This is used by the learning system to record experiences.
%% Returns {Outputs, Activations} where Activations includes all layers.
-spec evaluate_with_activations(pid(), [float()]) -> {[float()], [[float()]]}.
evaluate_with_activations(Pid, Inputs) ->
    gen_server:call(Pid, {evaluate_with_activations, Inputs}).

%% @doc Get current visualization data.
%%
%% Returns a map with nodes, connections, and activation levels.
-spec get_viz(pid()) -> map() | undefined.
get_viz(Pid) ->
    gen_server:call(Pid, get_viz).

%% @doc Get network topology information.
-spec get_topology(pid()) -> map().
get_topology(Pid) ->
    gen_server:call(Pid, get_topology).

%% @doc Get the current network.
-spec get_network(pid()) -> network_evaluator:network().
get_network(Pid) ->
    gen_server:call(Pid, get_network).

%% @doc Replace the network (e.g., after mutation).
-spec set_network(pid(), network_evaluator:network()) -> ok.
set_network(Pid, Network) ->
    gen_server:call(Pid, {set_network, Network}).

%% @doc Subscribe to visualization updates.
%%
%% The calling process will receive {brain_viz, Pid, VizData} messages
%% after each evaluation.
-spec subscribe(pid()) -> ok.
subscribe(BrainPid) ->
    subscribe(BrainPid, self()).

%% @doc Subscribe a specific process to visualization updates.
-spec subscribe(pid(), pid()) -> ok.
subscribe(BrainPid, SubscriberPid) ->
    gen_server:cast(BrainPid, {subscribe, SubscriberPid}).

%% @doc Unsubscribe from visualization updates.
-spec unsubscribe(pid()) -> ok.
unsubscribe(BrainPid) ->
    unsubscribe(BrainPid, self()).

%% @doc Unsubscribe a specific process from visualization updates.
-spec unsubscribe(pid(), pid()) -> ok.
unsubscribe(BrainPid, SubscriberPid) ->
    gen_server:cast(BrainPid, {unsubscribe, SubscriberPid}).

%%==============================================================================
%% gen_server Callbacks
%%==============================================================================

init(Opts) ->
    Network = maps:get(network, Opts),
    Id = maps:get(id, Opts, make_ref()),
    BrainId = maps:get(brain_id, Opts, Id),
    InputLabels = maps:get(input_labels, Opts, []),
    VizEnabled = maps:get(viz_enabled, Opts, true),

    %% Ensure pubsub scope is initialized (idempotent)
    brain_pubsub:init(BrainId),

    %% Subscribe to events from other brain subsystems
    brain_pubsub:subscribe(BrainId, weights_updated),
    brain_pubsub:subscribe(BrainId, weights_requested),

    State = #state{
        id = Id,
        brain_id = BrainId,
        network = Network,
        last_inputs = undefined,
        last_outputs = undefined,
        last_activations = undefined,
        subscribers = #{},
        input_labels = InputLabels,
        viz_enabled = VizEnabled
    },

    {ok, State}.

handle_call({evaluate, Inputs}, _From, State) ->
    #state{network = Network, viz_enabled = VizEnabled, brain_id = BrainId} = State,

    %% Evaluate and get activations if viz is enabled
    {Outputs, Activations, NewState} = case VizEnabled of
        true ->
            {Out, Acts} = network_evaluator:evaluate_with_activations(Network, Inputs),
            S = State#state{
                last_inputs = Inputs,
                last_outputs = Out,
                last_activations = Acts
            },
            {Out, Acts, S};
        false ->
            Out = network_evaluator:evaluate(Network, Inputs),
            S = State#state{
                last_inputs = Inputs,
                last_outputs = Out
            },
            {Out, undefined, S}
    end,

    %% Publish evaluated event for learner and other subsystems
    publish_evaluated(BrainId, Inputs, Outputs, Activations),

    %% Notify viz subscribers (legacy)
    notify_subscribers(NewState),

    {reply, Outputs, NewState};

handle_call({evaluate_with_activations, Inputs}, _From, State) ->
    #state{network = Network, brain_id = BrainId} = State,

    %% Always get activations for this call
    {Outputs, Activations} = network_evaluator:evaluate_with_activations(Network, Inputs),

    NewState = State#state{
        last_inputs = Inputs,
        last_outputs = Outputs,
        last_activations = Activations
    },

    %% Publish evaluated event for learner and other subsystems
    publish_evaluated(BrainId, Inputs, Outputs, Activations),

    %% Notify viz subscribers (legacy)
    notify_subscribers(NewState),

    {reply, {Outputs, Activations}, NewState};

handle_call(get_viz, _From, State) ->
    VizData = build_viz_data(State),
    {reply, VizData, State};

handle_call(get_topology, _From, State) ->
    Topology = network_evaluator:get_topology(State#state.network),
    {reply, Topology, State};

handle_call(get_network, _From, State) ->
    {reply, State#state.network, State};

handle_call({set_network, Network}, _From, State) ->
    NewState = State#state{
        network = Network,
        last_inputs = undefined,
        last_outputs = undefined,
        last_activations = undefined
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({subscribe, Pid}, State) ->
    #state{subscribers = Subs} = State,
    case maps:is_key(Pid, Subs) of
        true ->
            {noreply, State};
        false ->
            MonRef = monitor(process, Pid),
            NewSubs = maps:put(Pid, MonRef, Subs),
            {noreply, State#state{subscribers = NewSubs}}
    end;

handle_cast({unsubscribe, Pid}, State) ->
    #state{subscribers = Subs} = State,
    case maps:take(Pid, Subs) of
        {MonRef, NewSubs} ->
            demonitor(MonRef, [flush]),
            {noreply, State#state{subscribers = NewSubs}};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonRef, process, Pid, _Reason}, State) ->
    #state{subscribers = Subs} = State,
    NewSubs = maps:remove(Pid, Subs),
    {noreply, State#state{subscribers = NewSubs}};

%% Handle weight updates from brain_learner via pubsub
handle_info({brain_event, weights_updated, #{weights := NewWeights}}, State) ->
    #state{network = Network} = State,
    NewNetwork = network_evaluator:set_weights(Network, NewWeights),
    {noreply, State#state{network = NewNetwork}};

%% Handle weight requests from brain_learner (for initial sync)
handle_info({brain_event, weights_requested, _}, State) ->
    #state{network = Network, brain_id = BrainId} = State,
    Weights = network_evaluator:get_weights(Network),
    brain_pubsub:publish(BrainId, weights_response, #{weights => Weights}),
    {noreply, State};

handle_info({brain_event, _Topic, _Data}, State) ->
    %% Ignore other pubsub events
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

%% @private Build visualization data from current state
build_viz_data(#state{last_activations = undefined}) ->
    undefined;
build_viz_data(State) ->
    #state{
        network = Network,
        last_inputs = Inputs,
        last_outputs = Outputs,
        last_activations = Activations,
        input_labels = InputLabels
    } = State,

    %% Get topology
    #{layer_sizes := LayerSizes} = network_evaluator:get_topology(Network),

    %% Build nodes with activations
    Nodes = build_viz_nodes(LayerSizes, Activations, InputLabels),

    %% Build connections with weights
    Connections = build_viz_connections(Network, LayerSizes),

    #{
        nodes => Nodes,
        connections => Connections,
        layer_sizes => LayerSizes,
        inputs => Inputs,
        outputs => Outputs
    }.

%% @private Build node visualization data
build_viz_nodes(LayerSizes, Activations, InputLabels) ->
    NumLayers = length(LayerSizes),
    lists:flatten(
        lists:zipwith3(
            fun(LayerIdx, LayerSize, LayerActivations) ->
                Labels = get_layer_labels(LayerIdx, LayerSize, NumLayers, InputLabels),
                build_layer_nodes(LayerIdx, LayerActivations, Labels, NumLayers)
            end,
            lists:seq(1, NumLayers),
            LayerSizes,
            Activations
        )
    ).

%% @private Build nodes for a single layer
build_layer_nodes(LayerIdx, Activations, Labels, NumLayers) ->
    lists:zipwith3(
        fun(NodeIdx, Activation, Label) ->
            #{
                id => {LayerIdx, NodeIdx},
                layer => LayerIdx,
                index => NodeIdx,
                activation => Activation,
                label => Label,
                type => layer_type(LayerIdx, NumLayers)
            }
        end,
        lists:seq(1, length(Activations)),
        Activations,
        Labels
    ).

%% @private Get labels for a layer
get_layer_labels(1, Size, _NumLayers, InputLabels) ->
    pad_labels(InputLabels, Size);
get_layer_labels(LayerIdx, Size, NumLayers, _InputLabels) when LayerIdx == NumLayers ->
    output_labels(Size);
get_layer_labels(_LayerIdx, Size, _NumLayers, _InputLabels) ->
    hidden_labels(Size).

%% @private Build connection visualization data
build_viz_connections(Network, LayerSizes) ->
    Weights = network_evaluator:get_weights(Network),
    build_connections_from_weights(Weights, LayerSizes, 1, []).

build_connections_from_weights([], _LayerSizes, _LayerIdx, Acc) ->
    lists:reverse(lists:flatten(Acc));
build_connections_from_weights(Weights, LayerSizes, LayerIdx, Acc) ->
    FromSize = lists:nth(LayerIdx, LayerSizes),
    ToSize = lists:nth(LayerIdx + 1, LayerSizes),
    ConnectionCount = FromSize * ToSize,
    BiasCount = ToSize,

    {LayerWeights, Rest1} = lists:split(ConnectionCount, Weights),
    {_Biases, Rest2} = lists:split(BiasCount, Rest1),

    %% Build connections for this layer
    LayerConns = [
        #{
            from => {LayerIdx, FromIdx},
            to => {LayerIdx + 1, ToIdx},
            weight => lists:nth((ToIdx - 1) * FromSize + FromIdx, LayerWeights)
        }
        || ToIdx <- lists:seq(1, ToSize),
           FromIdx <- lists:seq(1, FromSize)
    ],

    build_connections_from_weights(Rest2, LayerSizes, LayerIdx + 1, [LayerConns | Acc]).

%% @private Notify all subscribers of viz update
notify_subscribers(#state{viz_enabled = false}) ->
    ok;
notify_subscribers(#state{subscribers = Subs}) when map_size(Subs) == 0 ->
    ok;
notify_subscribers(State) ->
    #state{subscribers = Subs} = State,
    VizData = build_viz_data(State),
    BrainPid = self(),
    maps:foreach(
        fun(Pid, _MonRef) ->
            Pid ! {brain_viz, BrainPid, VizData}
        end,
        Subs
    ).

%% @private Publish evaluated event via pubsub
%%
%% This notifies brain_learner and other subsystems that an evaluation
%% has completed. The learner uses this to record experiences.
publish_evaluated(BrainId, Inputs, Outputs, Activations) ->
    EventData = #{
        inputs => Inputs,
        outputs => Outputs,
        activations => Activations,
        timestamp => erlang:system_time(millisecond)
    },
    brain_pubsub:publish(BrainId, evaluated, EventData).

%% @private Determine layer type
layer_type(1, _NumLayers) -> input;
layer_type(N, N) -> output;
layer_type(_, _) -> hidden.

%% @private Pad labels to match layer size
pad_labels(Labels, Size) when length(Labels) >= Size ->
    lists:sublist(Labels, Size);
pad_labels(Labels, Size) ->
    Labels ++ lists:duplicate(Size - length(Labels), <<"">>).

%% @private Generate output labels
output_labels(6) ->
    [<<"L">>, <<"R">>, <<"F">>, <<"Spd">>, <<"Conf">>, <<"Aggr">>];
output_labels(Size) ->
    [list_to_binary("O" ++ integer_to_list(I)) || I <- lists:seq(1, Size)].

%% @private Generate hidden layer labels
hidden_labels(Size) ->
    [list_to_binary("H" ++ integer_to_list(I)) || I <- lists:seq(1, Size)].
