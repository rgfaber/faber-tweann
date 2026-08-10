%%%-----------------------------------------------------------------------------
%%% @doc Convert an evolved genotype into the flat node list the DAG evaluator
%%% takes, so an arbitrary evolved topology can be flown at inference speed.
%%%
%%% This is the counterpart to genotype_to_network. That one targets
%%% network_evaluator, which is a stack of dense layers, so it refuses any
%%% genotype whose connections skip or cross a layer. This one targets
%%% tweann_nif:compile_network/3, which imposes no layer structure at all: any
%%% acyclic connection pattern converts.
%%%
%%% ==========================================================================
%%% WHAT THIS BUYS, AND WHAT IT DOES NOT
%%% ==========================================================================
%%%
%%% Buys: arbitrary feedforward topology, evaluated synchronously, in Rust when
%%% the native path is loaded. That is the only route by which a topology
%%% produced by genome_mutator can be flown at the rate a simulation needs.
%%% The process-per-neuron phenotype is the alternative and it is orders of
%%% magnitude slower.
%%%
%%% ⚠ Does NOT buy temporal memory. compile_network/3 carries no per-node state,
%%% so a CfC or LTC neuron cannot be represented and is refused. A caller that
%%% needs memory AND arbitrary topology has neither path today.
%%%
%%% ==========================================================================
%%% THE CONTRACT, WHICH IS TIGHTER THAN THE SPEC SUGGESTS
%%% ==========================================================================
%%%
%%% The node tuple carries an index, and the two implementations do not treat it
%%% the same way. The native compile_network discards it and pushes nodes into a
%%% vector, so a node's LIST POSITION is its index. The Erlang fallback builds a
%%% map keyed on the index it was given. The two agree only when index equals
%%% position, and nothing checks.
%%%
%%% They diverge again on a source index that does not exist: the native
%%% evaluator indexes a vector and would panic, the fallback reads a map with a
%%% default of 0.0 and carries on.
%%%
%%% And neither sorts. The native loop iterates the vector once in order, so a
%%% connection whose source appears later reads whatever that slot held, which
%%% is 0.0. A caller passing an unsorted list gets a silently wrong answer from
%%% both.
%%%
%%% So this module emits, and asserts, all four:
%%%
%%% 1. index equals list position, over one contiguous run from zero
%%% 2. the first InputCount nodes are the inputs
%%% 3. topological order, every source strictly earlier than its consumer
%%% 4. every source index in range
%%%
%%% A genotype that cannot satisfy 3 is cyclic, which is recurrence, and is
%%% refused rather than evaluated into nonsense.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(genotype_to_dag).

%% nodes/1 shadows the auto-imported erlang:nodes/1. The name is the right one
%% here (genotype_to_dag:nodes/1 reads as what it is) and this module has no use
%% for the BIF, so the clash is resolved rather than the name given up.
-compile({no_auto_import, [nodes/1]}).

-include("records.hrl").

-export([nodes/1, compile/1]).

-type node_tuple() :: {non_neg_integer(), atom(), atom(), float(),
                       [{non_neg_integer(), float()}]}.
-type dag() :: {[node_tuple()], non_neg_integer(), [non_neg_integer()]}.

-type why() ::
    cyclic
    | {unsupported_neuron_type, atom()}
    | {unknown_source, term(), term()}
    | {weight_count_mismatch, term(), non_neg_integer(), non_neg_integer()}
    | no_neurons.

-type reason() :: {not_convertible, why()} | {missing, atom(), term()}.

-export_type([node_tuple/0, dag/0, reason/0]).

%%==============================================================================
%% Public API
%%==============================================================================

%% @doc The flat node list, the input count and the output indices.
%%
%% Pure, so it can be inspected and tested without loading a NIF.
-spec nodes(term()) -> {ok, dag()} | {error, reason()}.
nodes(AgentId) ->
    try
        Agent = required(agent, AgentId),
        Cx = required(cortex, Agent#agent.cx_id),
        Sensors = [required(sensor, I) || I <- Cx#cortex.sensor_ids],
        Neurons = [required(neuron, I) || I <- Cx#cortex.neuron_ids],
        Actuators = [required(actuator, I) || I <- Cx#cortex.actuator_ids],
        {ok, build(Sensors, Neurons, Actuators)}
    catch
        throw:{dag, Reason} -> {error, Reason}
    end.

%% @doc Convert and hand straight to the evaluator.
-spec compile(term()) -> {ok, reference() | map()} | {error, reason()}.
compile(AgentId) ->
    case nodes(AgentId) of
        {ok, {Nodes, InputCount, OutputIndices}} ->
            {ok, tweann_nif:compile_network(Nodes, InputCount, OutputIndices)};
        {error, _} = E ->
            E
    end.

%%==============================================================================
%% Building
%%==============================================================================

build(_Sensors, [], _Actuators) -> refuse(no_neurons);
build(Sensors, Neurons, Actuators) ->
    ok = every_type_supported(Neurons),
    InputSlots = sensor_slots(Sensors),
    InputCount = length(InputSlots),
    Ordered = topological(Neurons),
    Index = index_of(InputSlots, Ordered, InputCount),
    InputNodes = [{I, input, linear, 0.0, []} || I <- lists:seq(0, InputCount - 1)],
    NeuronNodes = [neuron_node(N, Index) || N <- Ordered],
    Nodes = InputNodes ++ NeuronNodes,
    ok = well_formed(Nodes, InputCount),
    {Nodes, InputCount, outputs(Actuators, Index)}.

%% Each sensor contributes vl slots to the input vector, concatenated in the
%% cortex's order. A slot is one index of the evaluator's input.
sensor_slots(Sensors) ->
    lists:append([[{S#sensor.id, K} || K <- lists:seq(1, S#sensor.vl)] || S <- Sensors]).

%% Inputs occupy 0..InputCount-1; neurons follow in topological order.
index_of(InputSlots, Ordered, InputCount) ->
    Slots = maps:from_list(lists:zip(InputSlots, lists:seq(0, InputCount - 1))),
    lists:foldl(
        fun({N, I}, Acc) -> Acc#{N#neuron.id => I} end,
        Slots,
        lists:zip(Ordered, lists:seq(InputCount, InputCount + length(Ordered) - 1))
    ).

neuron_node(#neuron{input_idps = Idps, af = Af} = N, Index) ->
    Self = maps:get(N#neuron.id, Index),
    Conns = lists:append([connections(Idp, N, Index) || Idp <- Idps]),
    {Self, neuron, Af, bias_of(Idps), Conns}.

connections({bias, _}, _N, _Index) ->
    [];
connections({SourceId, Weights}, N, Index) ->
    Slots = source_slots(SourceId, Index, N),
    length(Slots) =:= length(Weights)
        orelse refuse({weight_count_mismatch, SourceId, length(Slots), length(Weights)}),
    [{Slot, weight(W)} || {Slot, W} <- lists:zip(Slots, Weights)].

%% A neuron source is one index. A sensor source is vl consecutive indices, and
%% its weight list runs over them in order.
source_slots(SourceId, Index, N) ->
    case maps:get(SourceId, Index, undefined) of
        undefined -> sensor_run(SourceId, Index, N);
        I -> [I]
    end.

sensor_run(SourceId, Index, N) ->
    case lists:sort([I || {{Id, _K}, I} <- maps:to_list(Index), Id =:= SourceId]) of
        [] -> refuse({unknown_source, N#neuron.id, SourceId});
        Run -> Run
    end.

weight({W, _Delta, _Lr, _Params}) -> float(W);
weight(W) when is_number(W) -> float(W).

bias_of(Idps) ->
    case lists:keyfind(bias, 1, Idps) of
        {bias, [W | _]} -> weight(W);
        {bias, W} when is_number(W) -> float(W);
        false -> 0.0
    end.

%% The evaluator reads output values out of the same array it filled, so an
%% output is the index of the neuron feeding an actuator, in cortex order.
outputs(Actuators, Index) ->
    [maps:get(Id, Index)
     || A <- Actuators, Id <- A#actuator.fanin_ids, maps:is_key(Id, Index)].

%%==============================================================================
%% Topological order, and the refusal when there is none
%%==============================================================================

%% Kahn's algorithm over neuron-to-neuron edges only; sensor sources are already
%% earlier than every neuron by construction. Anything left over when no node
%% has zero remaining dependencies is part of a cycle.
topological(Neurons) ->
    ById = maps:from_list([{N#neuron.id, N} || N <- Neurons]),
    Deps = maps:from_list([{N#neuron.id, neuron_deps(N, ById)} || N <- Neurons]),
    kahn(Deps, ById, [N#neuron.id || N <- Neurons], []).

neuron_deps(#neuron{input_idps = Idps}, ById) ->
    [Id || {Id, _} <- Idps, Id =/= bias, maps:is_key(Id, ById)].

kahn(_Deps, _ById, [], Acc) ->
    lists:reverse(Acc);
kahn(Deps, ById, Pending, Acc) ->
    Placed = fun(Id) -> not lists:member(Id, Pending) end,
    Ready = [Id || Id <- Pending, lists:all(Placed, maps:get(Id, Deps))],
    Ready =/= [] orelse refuse(cyclic),
    kahn(Deps, ById,
         [Id || Id <- Pending, not lists:member(Id, Ready)],
         [maps:get(Id, ById) || Id <- Ready] ++ Acc).

%%==============================================================================
%% Refusals and the well-formedness assertion
%%==============================================================================

%% compile_network/3 has no per-node state, so a neuron whose behaviour depends
%% on carrying state between evaluations cannot be represented here.
every_type_supported(Neurons) ->
    case lists:usort([N#neuron.neuron_type || N <- Neurons]) -- [standard] of
        [] -> ok;
        [Bad | _] -> refuse({unsupported_neuron_type, Bad})
    end.

%% The four properties both evaluators assume and neither checks. Asserted here
%% rather than trusted, because getting any of them wrong is silent on one
%% implementation and a panic on the other.
well_formed(Nodes, InputCount) ->
    Count = length(Nodes),
    Indices = [I || {I, _, _, _, _} <- Nodes],
    Indices =:= lists:seq(0, Count - 1) orelse erlang:error({dag_not_positional, Indices}),
    lists:all(fun({I, Type, _, _, _}) -> (Type =:= input) =:= (I < InputCount) end, Nodes)
        orelse erlang:error(dag_inputs_not_first),
    lists:all(
        fun({I, _, _, _, Conns}) ->
            lists:all(fun({From, _}) -> From >= 0 andalso From < I end, Conns)
        end,
        Nodes
    ) orelse erlang:error(dag_not_topological),
    ok.

required(Tag, Id) ->
    case genotype:dirty_read({Tag, Id}) of
        undefined -> throw({dag, {missing, Tag, Id}});
        Rec -> Rec
    end.

-spec refuse(why()) -> no_return().
refuse(Why) -> throw({dag, {not_convertible, Why}}).
