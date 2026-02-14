%% @doc Internal pub/sub for brain subsystem communication using pg.
%%
%% This module provides a thin wrapper around OTP's pg (process groups)
%% for communication between brain subsystems. It uses pg's built-in
%% group management with brain-specific naming conventions.
%%
%% == Design Philosophy ==
%%
%% Brain subsystems communicate through events rather than direct calls:
%% - Publishers don't know about subscribers
%% - Subscribers don't know about publishers
%% - New subsystems can be added without modifying existing code
%% - Events form a clear contract between subsystems
%%
%% == Event Types ==
%%
%% | Event | Publisher | Description |
%% |-------|-----------|-------------|
%% | evaluated | brain | After each forward propagation |
%% | reward_received | brain_system | When reward signal arrives |
%% | weights_updated | brain_learner | After plasticity applied |
%% | learning_toggled | brain_system | Learning enabled/disabled |
%%
%% == Usage ==
%%
%% Initialize pubsub for a brain (typically in brain_sup):
%%   brain_pubsub:init(BrainId)
%%
%% Subscribe to events:
%%   brain_pubsub:subscribe(BrainId, evaluated)
%%   brain_pubsub:subscribe(BrainId, [evaluated, reward_received])
%%
%% Publish events:
%%   brain_pubsub:publish(BrainId, evaluated, #{activations => Acts})
%%
%% Receive events in subscriber:
%%   receive
%%       {brain_event, evaluated, Data} ->
%%           %% Handle evaluation event
%%           ...
%%   end
%%
%% == Implementation ==
%%
%% Uses OTP pg (process groups) with group names of the form:
%%   {brain_pubsub, BrainId, Topic}
%%
%% This allows multiple brain instances to have independent pubsub.
%%
%% @copyright 2024-2026 R.G. Lefever
-module(brain_pubsub).

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
-type event_type() :: evaluated
                    | reward_received
                    | weights_updated
                    | learning_toggled
                    | experience_recorded
                    | weights_requested
                    | weights_response
                    | atom().  %% Allow custom events

-type event_data() :: map().
-type brain_id() :: term().

-export_type([event_type/0, event_data/0, brain_id/0]).

%% pg scope for brain pubsub
-define(SCOPE, brain_pubsub_scope).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Initialize pubsub for a brain instance.
%%
%% This ensures the pg scope is started. Safe to call multiple times.
%% Typically called from brain_sup during initialization.
-spec init(brain_id()) -> ok.
init(_BrainId) ->
    %% Ensure pg scope is started
    case pg:start(?SCOPE) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

%% @doc Cleanup pubsub for a brain instance.
%%
%% Removes all subscriptions for the current process related to this brain.
%% Typically called during brain shutdown.
-spec cleanup(brain_id()) -> ok.
cleanup(BrainId) ->
    %% Get all groups this process belongs to
    Groups = pg:which_groups(?SCOPE),

    %% Leave any groups that belong to this brain
    lists:foreach(
        fun(Group) ->
            case Group of
                {brain_pubsub, BrainId, _Topic} ->
                    pg:leave(?SCOPE, Group, self());
                _ ->
                    ok
            end
        end,
        Groups
    ),
    ok.

%% @doc Subscribe calling process to event type(s).
-spec subscribe(brain_id(), event_type() | [event_type()]) -> ok.
subscribe(BrainId, Topics) when is_list(Topics) ->
    lists:foreach(fun(Topic) -> subscribe(BrainId, Topic, self()) end, Topics);
subscribe(BrainId, Topic) ->
    subscribe(BrainId, Topic, self()).

%% @doc Subscribe a specific process to an event type.
-spec subscribe(brain_id(), event_type(), pid()) -> ok.
subscribe(BrainId, Topic, Pid) ->
    Group = make_group(BrainId, Topic),
    pg:join(?SCOPE, Group, Pid).

%% @doc Unsubscribe calling process from event type(s).
-spec unsubscribe(brain_id(), event_type() | [event_type()]) -> ok.
unsubscribe(BrainId, Topics) when is_list(Topics) ->
    lists:foreach(fun(Topic) -> unsubscribe(BrainId, Topic, self()) end, Topics);
unsubscribe(BrainId, Topic) ->
    unsubscribe(BrainId, Topic, self()).

%% @doc Unsubscribe a specific process from an event type.
-spec unsubscribe(brain_id(), event_type(), pid()) -> ok.
unsubscribe(BrainId, Topic, Pid) ->
    Group = make_group(BrainId, Topic),
    pg:leave(?SCOPE, Group, Pid).

%% @doc Get list of subscribers for a topic.
-spec get_subscribers(brain_id(), event_type()) -> [pid()].
get_subscribers(BrainId, Topic) ->
    Group = make_group(BrainId, Topic),
    pg:get_members(?SCOPE, Group).

%% @doc Publish an event to all subscribers.
%%
%% Sends {brain_event, Topic, Data} to all processes subscribed to this topic.
%% This is asynchronous - returns immediately after sending.
-spec publish(brain_id(), event_type(), event_data()) -> ok.
publish(BrainId, Topic, Data) ->
    Group = make_group(BrainId, Topic),
    Subscribers = pg:get_members(?SCOPE, Group),

    Message = {brain_event, Topic, Data},
    lists:foreach(
        fun(Pid) -> Pid ! Message end,
        Subscribers
    ),
    ok.

%% @doc List all topics with active subscriptions for a brain.
-spec list_topics(brain_id()) -> [event_type()].
list_topics(BrainId) ->
    Groups = pg:which_groups(?SCOPE),

    %% Filter to this brain's groups and extract topics
    lists:filtermap(
        fun(Group) ->
            case Group of
                {brain_pubsub, BrainId, Topic} -> {true, Topic};
                _ -> false
            end
        end,
        Groups
    ).

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Create a pg group name for a brain topic
-spec make_group(brain_id(), event_type()) -> term().
make_group(BrainId, Topic) ->
    {brain_pubsub, BrainId, Topic}.
