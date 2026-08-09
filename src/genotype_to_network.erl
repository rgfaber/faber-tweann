%%%-----------------------------------------------------------------------------
%%% @doc Convert an evolved genotype into a network_evaluator network, carrying
%%% the weights, or REFUSE when the topology cannot be represented.
%%%
%%% ROADMAP item 8a. The function this replaces reported success and handed back
%%% a brain-dead network: it counted the neurons, invented a layer shape and
%%% filled it with random weights, under a public doc claiming it read the
%%% structure "and weights" from Mnesia. There is no Mnesia, and there were no
%%% weights. An evolved champion came back the right size and knowing nothing,
%%% and nothing raised.
%%%
%%% ==========================================================================
%%% REFUSE, NEVER APPROXIMATE
%%% ==========================================================================
%%%
%%% A genotype is an arbitrary graph. A network_evaluator network is a stack of
%%% dense layers. Most graphs are not stacks, and the only honest answers are a
%%% faithful conversion or an error naming what stopped it. An approximation
%%% behind an ok tuple is the defect this module exists to remove, so it is not
%%% reintroduced here in a tidier form.
%%%
%%% Missing connections are NOT an approximation and are filled with 0.0. A
%%% weight of zero contributes nothing to a dot product, so a sparse layer
%%% expressed densely computes the same function exactly.
%%%
%%% ==========================================================================
%%% THE FIVE THINGS THAT MAKE A GENOTYPE UNCONVERTIBLE
%%% ==========================================================================
%%%
%%% 1. RECURRENCE. Any neuron with a recurrent output edge. A dense stack has no
%%%    cycle to put it in.
%%% 2. A CONNECTION THAT SKIPS OR CROSSES A LAYER. Every input to a neuron in
%%%    layer N must come from layer N-1, or from the sensors when N is the first
%%%    layer. Skip connections and lateral connections have no matrix position.
%%% 3. MIXED ACTIVATION FUNCTIONS. The network record carries ONE activation for
%%%    every hidden layer and one for the output layer, not one per neuron.
%%% 4. AN ACTIVATION network_evaluator DOES NOT IMPLEMENT. It handles exactly
%%%    tanh, sigmoid, relu and linear, and its private apply_activation/2 ends in
%%%    a catch-all that silently returns math:tanh/1. So a genotype carrying
%%%    gaussian, sin, cos, absolute, sgn, quadratic, cubic, sqrt, log, bin,
%%%    trinary, multiquadric or sigmoid1 would convert to something that computes
%%%    a different function without saying so. Refused by name instead.
%%% 5. AN LTC NEURON. The evaluator's neuron metadata admits standard and cfc.
%%%    There is no ltc there, and mapping one onto the other would be a guess.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(genotype_to_network).

-include("records.hrl").

-export([convert/1, supported_activations/0]).

%% Exactly the clause list of network_evaluator's private apply_activation/2,
%% minus its catch-all. Kept here because the catch-all is what makes an
%% unsupported activation silent, and a list that drifts from it would restore
%% the silence.
-define(SUPPORTED_AFS, [tanh, sigmoid, relu, linear]).

-type why() ::
    recurrent
    | {unsupported_activation, atom()}
    | {mixed_activations, hidden | output, [atom()]}
    | {unsupported_neuron_type, atom()}
    | {source_not_in_previous_layer, term(), term()}
    | {weight_count_mismatch, term(), non_neg_integer(), non_neg_integer()}
    | no_neurons.

-type reason() :: {not_layerable, why()} | {missing, atom(), term()}.

-export_type([why/0, reason/0]).

%%==============================================================================
%% Public API
%%==============================================================================

%% @doc The activations a genotype may carry and still be convertible.
-spec supported_activations() -> [atom()].
supported_activations() -> ?SUPPORTED_AFS.

%% @doc Convert an agent's genotype into an equivalent network, weights included.
-spec convert(term()) -> {ok, network_evaluator:network()} | {error, reason()}.
convert(AgentId) ->
    try
        Agent = required(agent, AgentId),
        Cx = required(cortex, Agent#agent.cx_id),
        Sensors = [required(sensor, I) || I <- Cx#cortex.sensor_ids],
        Neurons = [required(neuron, I) || I <- Cx#cortex.neuron_ids],
        {ok, build(Sensors, Neurons)}
    catch
        throw:{convert, Reason} -> {error, Reason}
    end.

%%==============================================================================
%% Building
%%==============================================================================

build(_Sensors, []) -> refuse(no_neurons);
build(Sensors, Neurons) ->
    ok = no_recurrence(Neurons),
    ok = every_type_supported(Neurons),
    Layers = layered(Neurons),
    %% lists:split over the LAYERS, so HiddenLayers is a list of layers and the
    %% tail is the single output layer. Binding [OutputLayer] rather than
    %% OutputLayer is the whole point: an earlier version passed the one-element
    %% list of layers where a list of neurons was expected, and every activation
    %% lookup hit a list instead of a record.
    {HiddenLayers, [OutputLayer]} = lists:split(length(Layers) - 1, Layers),
    HiddenNeurons = lists:append(HiddenLayers),
    Activation = one_activation(hidden, HiddenNeurons, OutputLayer),
    OutputActivation = one_activation(output, OutputLayer, OutputLayer),
    InputSlots = sensor_slots(Sensors),
    {Rows, _} = lists:mapfoldl(fun rows_for/2, InputSlots, Layers),
    Net = network_evaluator:create_feedforward(
        length(InputSlots),
        [length(L) || L <- HiddenLayers],
        length(OutputLayer),
        Activation,
        OutputActivation
    ),
    with_meta(network_evaluator:set_weights(Net, flatten(Rows)), Layers).

%% The flat layout network_evaluator:get_weights/1 produces and set_weights/2
%% consumes: layer by layer, all of a layer's weights neuron-major in input
%% order, then all of that layer's biases.
flatten(Rows) ->
    lists:append([lists:append([W || {W, _B} <- Layer]) ++ [B || {_W, B} <- Layer]
                  || Layer <- Rows]).

%% Sensors are the input vector, concatenated in cortex order, each contributing
%% vl slots. A slot is what one column of the first weight matrix corresponds to.
sensor_slots(Sensors) ->
    lists:append([[{S#sensor.id, K} || K <- lists:seq(1, S#sensor.vl)] || S <- Sensors]).

%% Neurons carry their layer in their own id, as {{LayerCoord, _}, neuron}.
%%
%% ⚠ WITHIN a layer the order is the CORTEX'S order, which is why Neurons is
%% read in cortex.neuron_ids order and that order is preserved here rather than
%% sorted. An earlier version sorted the neuron records, which orders them by
%% the random UniqueFloat inside each id. The converted network computed the
%% same function either way, because permuting a layer permutes its rows and the
%% next layer's columns together, so an arithmetic test cannot see it. What it
%% breaks is that the weight vector stops being predictable from the genome as
%% declared, and two structurally identical genotypes minted at different times
%% lay out differently. Found when a test asserting the vector passed and then
%% failed on a rerun, with nothing changed but the ids.
layered(Neurons) ->
    Coords = lists:usort([layer_of(N) || N <- Neurons]),
    [[N || N <- Neurons, layer_of(N) =:= Coord] || Coord <- Coords].

layer_of(#neuron{id = {{Coord, _}, neuron}}) -> Coord.

%% Each layer's rows are built against the previous layer's slots, and the
%% layer's own neurons become the slots for the next one.
rows_for(Layer, PrevSlots) ->
    Rows = [row(N, PrevSlots) || N <- Layer],
    {Rows, [{N#neuron.id, 1} || N <- Layer]}.

%% One neuron becomes one dense row plus a bias. Every slot the neuron does not
%% connect to is 0.0, which is exact rather than approximate.
row(#neuron{input_idps = Idps} = N, PrevSlots) ->
    Bias = bias_of(Idps),
    Placed = lists:foldl(fun(Idp, Acc) -> place(Idp, PrevSlots, N, Acc) end, #{}, Idps),
    {[maps:get(Slot, Placed, 0.0) || Slot <- PrevSlots], Bias}.

place({bias, _}, _PrevSlots, _N, Acc) ->
    Acc;
place({SourceId, Weights}, PrevSlots, N, Acc) ->
    Slots = [S || {Id, _} = S <- PrevSlots, Id =:= SourceId],
    Slots =/= [] orelse refuse({source_not_in_previous_layer, N#neuron.id, SourceId}),
    length(Slots) =:= length(Weights)
        orelse refuse({weight_count_mismatch, SourceId, length(Slots), length(Weights)}),
    lists:foldl(fun({Slot, W}, A) -> A#{Slot => weight(W)} end,
                Acc,
                lists:zip(Slots, Weights)).

%% A weight is {Weight, DeltaWeight, LearningRate, ParameterList}. Only the first
%% element is the synaptic weight; the rest is tuning state the evaluator has no
%% place for, and dropping it is a documented consequence rather than a silent
%% one: a converted network is an inference artifact, not a resumable genotype.
weight({W, _Delta, _Lr, _Params}) -> float(W);
weight(W) when is_number(W) -> float(W).

bias_of(Idps) ->
    case lists:keyfind(bias, 1, Idps) of
        {bias, [W | _]} -> weight(W);
        {bias, W} when is_number(W) -> float(W);
        false -> 0.0
    end.

%%==============================================================================
%% The refusals
%%==============================================================================

no_recurrence(Neurons) ->
    case [N#neuron.id || N <- Neurons, N#neuron.ro_ids =/= []] of
        [] -> ok;
        [_ | _] -> refuse(recurrent)
    end.

every_type_supported(Neurons) ->
    case lists:usort([N#neuron.neuron_type || N <- Neurons]) -- [standard, cfc] of
        [] -> ok;
        [Bad | _] -> refuse({unsupported_neuron_type, Bad})
    end.

%% The network record holds one activation for every hidden layer and one for the
%% output layer. A genotype may hold one per neuron, so disagreement is refused
%% rather than resolved by picking a winner.
one_activation(_Which, [], Fallback) ->
    one_activation(output, Fallback, Fallback);
one_activation(Which, Neurons, _Fallback) ->
    case lists:usort([N#neuron.af || N <- Neurons]) of
        [Af] ->
            lists:member(Af, ?SUPPORTED_AFS) orelse refuse({unsupported_activation, Af}),
            Af;
        Many ->
            refuse({mixed_activations, Which, Many})
    end.

%%==============================================================================
%% CfC metadata
%%==============================================================================

%% Only set when a genotype actually carries a cfc neuron, so a plain network is
%% left with undefined metadata and keeps network_evaluator's stateless path.
with_meta(Net, Layers) ->
    Meta = [[meta(N) || N <- Layer] || Layer <- Layers],
    case lists:any(fun(L) -> lists:any(fun(#{neuron_type := T}) -> T =:= cfc end, L) end, Meta) of
        true -> network_evaluator:set_neuron_meta(Net, Meta);
        false -> Net
    end.

meta(#neuron{neuron_type = T, time_constant = Tau, state_bound = Bound}) ->
    #{neuron_type => T, tau => float(Tau), state_bound => float(Bound)}.

%%==============================================================================
%% Reading and failing
%%==============================================================================

required(Tag, Id) ->
    case genotype:dirty_read({Tag, Id}) of
        undefined -> throw({convert, {missing, Tag, Id}});
        Rec -> Rec
    end.

-spec refuse(why()) -> no_return().
refuse(Why) -> throw({convert, {not_layerable, Why}}).
