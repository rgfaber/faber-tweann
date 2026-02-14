%% @doc Innovation number tracking for NEAT-style evolution.
%%
%% This module manages innovation numbers that uniquely identify structural
%% changes in neural networks. Innovation numbers enable:
%% - Meaningful crossover between networks with different topologies
%% - Historical alignment of genes during reproduction
%% - Tracking which structural changes are the "same" across lineages
%%
%% Key concepts from NEAT (Stanley and Miikkulainen, 2002):
%% - Each new link gets a unique innovation number
%% - When a link is split to add a neuron, the new node and its links get tracked
%% - Same structural change (same from/to) always gets the same innovation
%%
%% Storage: Uses ETS for innovation tracking and the counters module for
%% atomic counter increments (faster than Mnesia dirty_update_counter).
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(innovation).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([
    init/0,
    reset/0,
    next_innovation/0,
    get_or_create_link_innovation/2,
    get_or_create_node_innovation/2,
    get_innovation_info/1,
    get_link_innovation/2,
    get_node_innovation/2
]).

%% Internal records for ETS storage
-record(link_innovation, {
    key :: {term(), term()},         % {FromId, ToId}
    innovation :: pos_integer()      % Unique innovation number
}).

-record(node_innovation, {
    key :: {term(), term()},         % {FromId, ToId} - the link being split
    node_innovation :: pos_integer(),    % Innovation for the new node
    in_innovation :: pos_integer(),      % Innovation for link to new node
    out_innovation :: pos_integer()      % Innovation for link from new node
}).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Initialize innovation tracking with ETS and atomic counter.
%%
%% Creates ETS tables for innovation tracking and initializes the
%% atomic counter. Should be called after genotype:init_db().
-spec init() -> ok.
init() ->
    %% Create ETS tables for innovation tracking
    Tables = [link_innovation, node_innovation],

    lists:foreach(
        fun(Name) ->
            case ets:whereis(Name) of
                undefined ->
                    ets:new(Name, [set, public, named_table, {keypos, 2},
                                   {read_concurrency, true}]);
                _Tid ->
                    ok
            end
        end,
        Tables
    ),

    %% Initialize atomic counter using persistent_term
    %% The counter starts at 0 and is incremented atomically
    case persistent_term:get(innovation_counter, undefined) of
        undefined ->
            CounterRef = counters:new(1, [atomics]),
            persistent_term:put(innovation_counter, CounterRef);
        _Existing ->
            ok
    end,
    ok.

%% @doc Reset all innovation tracking.
%%
%% Clears all innovation tables and resets the counter.
%% Useful for starting a fresh evolutionary run.
-spec reset() -> ok.
reset() ->
    %% Clear ETS tables
    lists:foreach(
        fun(Table) ->
            case ets:whereis(Table) of
                undefined -> ok;
                _Tid -> ets:delete_all_objects(Table)
            end
        end,
        [link_innovation, node_innovation]
    ),
    %% Reset counter to 0
    case persistent_term:get(innovation_counter, undefined) of
        undefined ->
            %% Counter not initialized, initialize it
            CounterRef = counters:new(1, [atomics]),
            persistent_term:put(innovation_counter, CounterRef);
        CounterRef ->
            %% Reset existing counter to 0
            Current = counters:get(CounterRef, 1),
            counters:sub(CounterRef, 1, Current)
    end,
    ok.

%% @doc Get the next innovation number.
%%
%% Atomically increments and returns the global innovation counter.
-spec next_innovation() -> pos_integer().
next_innovation() ->
    CounterRef = persistent_term:get(innovation_counter),
    counters:add(CounterRef, 1, 1),
    counters:get(CounterRef, 1).

%% @doc Get or create innovation number for a link.
%%
%% If this exact link (from -> to) was seen before, returns the same
%% innovation number. Otherwise, creates a new one.
%% This ensures that the same structural change in different lineages
%% gets the same historical marker.
-spec get_or_create_link_innovation(FromId :: term(), ToId :: term()) -> pos_integer().
get_or_create_link_innovation(FromId, ToId) ->
    Key = {FromId, ToId},
    case ets:lookup(link_innovation, Key) of
        [#link_innovation{innovation = Inn}] ->
            Inn;
        [] ->
            Inn = next_innovation(),
            true = ets:insert(link_innovation, #link_innovation{key = Key, innovation = Inn}),
            Inn
    end.

%% @doc Get or create innovation numbers for a node split.
%%
%% When a link is split to add a neuron, we need three innovation numbers:
%% 1. For the new node itself
%% 2. For the new link from the original source to the new node
%% 3. For the new link from the new node to the original target
%%
%% Returns {NodeInnovation, InLinkInnovation, OutLinkInnovation}
-spec get_or_create_node_innovation(FromId :: term(), ToId :: term()) ->
    {pos_integer(), pos_integer(), pos_integer()}.
get_or_create_node_innovation(FromId, ToId) ->
    Key = {FromId, ToId},
    case ets:lookup(node_innovation, Key) of
        [#node_innovation{node_innovation = NodeInn,
                          in_innovation = InInn,
                          out_innovation = OutInn}] ->
            {NodeInn, InInn, OutInn};
        [] ->
            NodeInn = next_innovation(),
            InInn = next_innovation(),
            OutInn = next_innovation(),
            true = ets:insert(node_innovation, #node_innovation{
                key = Key,
                node_innovation = NodeInn,
                in_innovation = InInn,
                out_innovation = OutInn
            }),
            {NodeInn, InInn, OutInn}
    end.

%% @doc Get innovation info for a specific innovation number.
%%
%% Returns {link, FromId, ToId} or {node, FromId, ToId, InInn, OutInn}
%% or not_found if the innovation doesn't exist.
-spec get_innovation_info(pos_integer()) ->
    {link, term(), term()} |
    {node, term(), term(), pos_integer(), pos_integer()} |
    not_found.
get_innovation_info(InnovationNum) ->
    %% Check link innovations
    LinkMatch = ets:fun2ms(
        fun(#link_innovation{key = Key, innovation = Inn})
            when Inn =:= InnovationNum -> Key
        end
    ),
    case ets:select(link_innovation, LinkMatch) of
        [{FromId, ToId}] ->
            {link, FromId, ToId};
        [] ->
            %% Check node innovations
            NodeMatch = ets:fun2ms(
                fun(#node_innovation{key = Key,
                                     node_innovation = NodeInn,
                                     in_innovation = InInn,
                                     out_innovation = OutInn})
                    when NodeInn =:= InnovationNum -> {Key, InInn, OutInn}
                end
            ),
            case ets:select(node_innovation, NodeMatch) of
                [{{FromId, ToId}, InInn, OutInn}] ->
                    {node, FromId, ToId, InInn, OutInn};
                [] ->
                    not_found
            end
    end.

%% @doc Get existing link innovation without creating one.
%%
%% Returns the innovation number if the link exists, undefined otherwise.
-spec get_link_innovation(FromId :: term(), ToId :: term()) -> pos_integer() | undefined.
get_link_innovation(FromId, ToId) ->
    case ets:lookup(link_innovation, {FromId, ToId}) of
        [#link_innovation{innovation = Inn}] -> Inn;
        [] -> undefined
    end.

%% @doc Get existing node innovation without creating one.
%%
%% Returns {NodeInn, InInn, OutInn} if the node split exists, undefined otherwise.
-spec get_node_innovation(FromId :: term(), ToId :: term()) ->
    {pos_integer(), pos_integer(), pos_integer()} | undefined.
get_node_innovation(FromId, ToId) ->
    case ets:lookup(node_innovation, {FromId, ToId}) of
        [#node_innovation{node_innovation = NodeInn,
                          in_innovation = InInn,
                          out_innovation = OutInn}] ->
            {NodeInn, InInn, OutInn};
        [] ->
            undefined
    end.
