%% @doc Internal pub/sub for neural network component communication using pg.
%%
%% This module provides a thin wrapper around OTP's pg (process groups)
%% for communication between network components (cortex, sensors, neurons,
%% actuators). It uses pg's built-in group management with network-specific
%% naming conventions.
%%
%% == Design Philosophy ==
%%
%% Network components communicate through events rather than direct calls:
%% - Publishers don't need to know subscriber PIDs
%% - Subscribers don't need to know publisher PIDs
%% - New observers can be added without modifying existing code
%% - Events form a clear contract between components
%%
%% == Event Types ==
%%
%% | Event | Publisher | Description |
%% |-------|-----------|-------------|
%% | evaluation_cycle_started | cortex | When sync is triggered |
%% | sensor_output_ready | sensor | When sensor produces output |
%% | neuron_output_ready | neuron | When neuron fires |
%% | actuator_output_ready | actuator | When actuator produces output |
%% | backup_requested | cortex | When weight backup is needed |
%% | network_terminating | cortex | When network is shutting down |
%%
%% == Usage ==
%%
%% Initialize pubsub for a network (typically in cortex):
%%   network_pubsub:init(NetworkId)
%%
%% Subscribe to events:
%%   network_pubsub:subscribe(NetworkId, evaluation_cycle_started)
%%   network_pubsub:subscribe(NetworkId, [sensor_output_ready, neuron_output_ready])
%%
%% Publish events:
%%   network_pubsub:publish(NetworkId, evaluation_cycle_started, #{cycle => 1})
%%   network_pubsub:publish(NetworkId, sensor_output_ready, #{from => SensorPid, signal => Signal})
%%
%% Receive events in subscriber:
%%   receive
%%       {network_event, evaluation_cycle_started, Data} ->
%%           %% Handle evaluation start
%%           ...
%%   end
%%
%% == Implementation ==
%%
%% Uses OTP pg (process groups) with group names of the form:
%%   {network_pubsub, NetworkId, Topic}
%%
%% This allows multiple network instances to have independent pubsub.
%%
%% @copyright 2024-2026 R.G. Lefever
-module(network_pubsub).

%% API
-export([
    %% Lifecycle
    init/1,
    cleanup/1,
    %% Subscription management
    subscribe/2,
    subscribe/3,
    unsubscribe/2,
    unsubscribe/3,
    get_subscribers/2,
    %% Publishing
    publish/3,
    %% Introspection
    list_topics/1
]).

%% Event types (for documentation)
-type event_type() :: evaluation_cycle_started
                    | sensor_output_ready
                    | neuron_output_ready
                    | actuator_output_ready
                    | backup_requested
                    | weights_backed_up
                    | network_terminating
                    | atom().  %% Allow custom events

-type event_data() :: map().
-type network_id() :: term().

-export_type([event_type/0, event_data/0, network_id/0]).

%% pg scope for network pubsub
-define(SCOPE, network_pubsub_scope).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Initialize pubsub for a network instance.
%%
%% This ensures the pg scope is started. Safe to call multiple times.
%% Typically called from cortex during initialization.
-spec init(network_id()) -> ok.
init(_NetworkId) ->
    %% Ensure pg scope is started
    case pg:start(?SCOPE) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

%% @doc Cleanup pubsub for a network instance.
%%
%% Removes all subscriptions for the current process related to this network.
%% Typically called during network shutdown.
-spec cleanup(network_id()) -> ok.
cleanup(NetworkId) ->
    %% Get all groups this process belongs to
    Groups = pg:which_groups(?SCOPE),

    %% Leave any groups that belong to this network
    lists:foreach(
        fun(Group) ->
            case Group of
                {network_pubsub, NetworkId, _Topic} ->
                    pg:leave(?SCOPE, Group, self());
                _ ->
                    ok
            end
        end,
        Groups
    ),
    ok.

%% @doc Subscribe calling process to event type(s).
-spec subscribe(network_id(), event_type() | [event_type()]) -> ok.
subscribe(NetworkId, Topics) when is_list(Topics) ->
    lists:foreach(fun(Topic) -> subscribe(NetworkId, Topic, self()) end, Topics);
subscribe(NetworkId, Topic) ->
    subscribe(NetworkId, Topic, self()).

%% @doc Subscribe a specific process to an event type.
-spec subscribe(network_id(), event_type(), pid()) -> ok.
subscribe(NetworkId, Topic, Pid) ->
    Group = make_group(NetworkId, Topic),
    pg:join(?SCOPE, Group, Pid).

%% @doc Unsubscribe calling process from event type(s).
-spec unsubscribe(network_id(), event_type() | [event_type()]) -> ok.
unsubscribe(NetworkId, Topics) when is_list(Topics) ->
    lists:foreach(fun(Topic) -> unsubscribe(NetworkId, Topic, self()) end, Topics);
unsubscribe(NetworkId, Topic) ->
    unsubscribe(NetworkId, Topic, self()).

%% @doc Unsubscribe a specific process from an event type.
-spec unsubscribe(network_id(), event_type(), pid()) -> ok.
unsubscribe(NetworkId, Topic, Pid) ->
    Group = make_group(NetworkId, Topic),
    pg:leave(?SCOPE, Group, Pid).

%% @doc Get list of subscribers for a topic.
-spec get_subscribers(network_id(), event_type()) -> [pid()].
get_subscribers(NetworkId, Topic) ->
    Group = make_group(NetworkId, Topic),
    pg:get_members(?SCOPE, Group).

%% @doc Publish an event to all subscribers.
%%
%% Sends {network_event, Topic, Data} to all processes subscribed to this topic.
%% This is asynchronous - returns immediately after sending.
-spec publish(network_id(), event_type(), event_data()) -> ok.
publish(NetworkId, Topic, Data) ->
    Group = make_group(NetworkId, Topic),
    Subscribers = pg:get_members(?SCOPE, Group),

    Message = {network_event, Topic, Data},
    lists:foreach(
        fun(Pid) -> Pid ! Message end,
        Subscribers
    ),
    ok.

%% @doc List all topics with active subscriptions for a network.
-spec list_topics(network_id()) -> [event_type()].
list_topics(NetworkId) ->
    Groups = pg:which_groups(?SCOPE),

    %% Filter to this network's groups and extract topics
    lists:filtermap(
        fun(Group) ->
            case Group of
                {network_pubsub, NetworkId, Topic} -> {true, Topic};
                _ -> false
            end
        end,
        Groups
    ).

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Create a pg group name for a network topic
-spec make_group(network_id(), event_type()) -> term().
make_group(NetworkId, Topic) ->
    {network_pubsub, NetworkId, Topic}.
