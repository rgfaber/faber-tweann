%% @doc Synchronous neural network evaluator for inference.
%%
%% This module provides synchronous (blocking) forward propagation
%% for neural networks. Unlike the process-based cortex/neuron approach
%% used during training, this is designed for fast inference in
%% real-time applications like games.
%%
%% == Usage ==
%%
%% Create a network from a genotype:
%%   {ok, Network} = network_evaluator:from_genotype(AgentId)
%%
%% Or create a simple feedforward network:
%%   Network = network_evaluator:create_feedforward(42, [16, 8], 6)
%%
%% Evaluate:
%%   Outputs = network_evaluator:evaluate(Network, Inputs)
%%
%% @copyright 2024-2026 R.G. Lefever
-module(network_evaluator).

-export([
    create_feedforward/3,
    create_feedforward/4,
    create_feedforward/5,
    evaluate/2,
    evaluate_with_activations/2,
    from_genotype/1,
    get_weights/1,
    set_weights/2,
    get_topology/1,
    get_viz_data/3,
    %% Memory management
    strip_compiled_ref/1,
    %% NIF compilation (use sparingly - can cause memory leaks if networks accumulate)
    compile_for_nif/1,
    %% Serialization
    to_json/1,
    from_json/1,
    to_binary/1,
    from_binary/1
]).

-record(network, {
    layers :: [layer()],
    activation :: atom(),
    %% Optional: separate activation for output layer (undefined = same as activation)
    output_activation :: atom() | undefined,
    %% Optional compiled NIF reference for fast evaluation
    compiled_ref :: reference() | undefined
}).

-type layer() :: {Weights :: [[float()]], Biases :: [float()]}.
-type network() :: #network{}.

-export_type([network/0]).

%% @doc Create a feedforward network with random weights.
%%
%% @param InputSize Number of inputs
%% @param HiddenSizes List of hidden layer sizes
%% @param OutputSize Number of outputs
%% @returns Network record
-spec create_feedforward(pos_integer(), [pos_integer()], pos_integer()) -> network().
create_feedforward(InputSize, HiddenSizes, OutputSize) ->
    create_feedforward(InputSize, HiddenSizes, OutputSize, tanh, undefined).

%% @doc Create a feedforward network with specified activation.
-spec create_feedforward(pos_integer(), [pos_integer()], pos_integer(), atom()) -> network().
create_feedforward(InputSize, HiddenSizes, OutputSize, Activation) ->
    create_feedforward(InputSize, HiddenSizes, OutputSize, Activation, undefined).

%% @doc Create a feedforward network with separate output activation.
%%
%% Hidden layers use Activation, output layer uses OutputActivation.
%% If OutputActivation is undefined, all layers use Activation.
-spec create_feedforward(pos_integer(), [pos_integer()], pos_integer(), atom(), atom() | undefined) -> network().
create_feedforward(InputSize, HiddenSizes, OutputSize, Activation, OutputActivation) ->
    LayerSizes = [InputSize | HiddenSizes] ++ [OutputSize],
    Layers = create_layers(LayerSizes),
    %% NOTE: Do NOT compile for NIF here - lazy compilation prevents memory leaks.
    %% The compiled_ref holds a Rust ResourceArc that keeps native memory alive.
    %% Eager compilation causes memory to accumulate unboundedly across generations.
    %% Networks will use pure Erlang evaluation (fallback in evaluate/2).
    #network{layers = Layers, activation = Activation,
             output_activation = OutputActivation, compiled_ref = undefined}.

%% @doc Evaluate the network with given inputs.
%%
%% Performs synchronous forward propagation through all layers.
%% Uses NIF acceleration if available and network was compiled.
%%
%% @param Network The network record
%% @param Inputs List of input values (must match input size)
%% @returns List of output values
-spec evaluate(network(), [float()]) -> [float()].
evaluate(#network{compiled_ref = CompiledRef}, Inputs) when CompiledRef =/= undefined ->
    %% Fast path: use NIF-compiled network
    tweann_nif:evaluate(CompiledRef, Inputs);
evaluate(#network{layers = Layers, activation = Activation,
                  output_activation = OutputActivation}, Inputs) ->
    %% Fallback: pure Erlang evaluation
    OA = resolve_output_activation(OutputActivation, Activation),
    forward_propagate(Layers, Inputs, Activation, OA).

%% @doc Load a network from a genotype stored in Mnesia.
%%
%% Reads the agent's neural network structure and weights from Mnesia
%% and creates an evaluator network.
%%
%% @param AgentId The agent identifier
%% @returns {ok, Network} | {error, Reason}
-spec from_genotype(term()) -> {ok, network()} | {error, term()}.
from_genotype(AgentId) ->
    case load_genotype_structure(AgentId) of
        {ok, Structure} ->
            Network = build_network_from_structure(Structure),
            {ok, Network};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get all weights from the network as a flat list.
%%
%% Useful for evolution - can be mutated and set back.
-spec get_weights(network()) -> [float()].
get_weights(#network{layers = Layers}) ->
    lists:flatmap(
        fun({Weights, Biases}) ->
            lists:flatten(Weights) ++ Biases
        end,
        Layers
    ).

%% @doc Set weights from a flat list.
%%
%% The list must have the same number of elements as returned by get_weights/1.
%% NOTE: Does NOT compile for NIF - this prevents memory leaks during evolution.
-spec set_weights(network(), [float()]) -> network().
set_weights(Network = #network{layers = Layers}, FlatWeights) ->
    {NewLayers, []} = lists:mapfoldl(
        fun({Weights, Biases}, Remaining) ->
            WeightCount = length(Weights) * length(hd(Weights)),
            BiasCount = length(Biases),
            {WeightVals, Rest1} = lists:split(WeightCount, Remaining),
            {BiasVals, Rest2} = lists:split(BiasCount, Rest1),
            NewWeights = reshape_weights(WeightVals, length(hd(Weights))),
            {{NewWeights, BiasVals}, Rest2}
        end,
        FlatWeights,
        Layers
    ),
    %% NOTE: Do NOT compile for NIF here - lazy compilation prevents memory leaks.
    %% The compiled_ref holds a Rust ResourceArc that keeps native memory alive.
    %% During breeding, set_weights is called for EVERY offspring - eager compilation
    %% would create millions of compiled_ref references that accumulate unboundedly.
    %% Networks will use pure Erlang evaluation (fallback in evaluate/2).
    Network#network{layers = NewLayers, compiled_ref = undefined}.

%% @doc Strip the compiled_ref from a network to release NIF memory.
%%
%% IMPORTANT: Call this before storing networks long-term (archives, events)
%% to prevent NIF ResourceArc references from accumulating and causing
%% memory leaks. The compiled_ref is a Rust ResourceArc that holds native
%% memory - keeping references alive prevents the memory from being freed.
%%
%% The network can be recompiled on-demand when needed for evaluation.
-spec strip_compiled_ref(Network :: network() | map() | term()) -> network() | map() | term().
strip_compiled_ref(#network{} = Network) ->
    Network#network{compiled_ref = undefined};
strip_compiled_ref(#{compiled_ref := _} = NetworkMap) ->
    maps:put(compiled_ref, undefined, NetworkMap);
strip_compiled_ref(Other) ->
    %% Unknown format or already stripped - return as-is
    Other.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Create layer weight matrices
create_layers(LayerSizes) ->
    Pairs = lists:zip(
        lists:droplast(LayerSizes),
        tl(LayerSizes)
    ),
    [create_layer(FromSize, ToSize) || {FromSize, ToSize} <- Pairs].

%% @private Create a single layer with random weights
create_layer(FromSize, ToSize) ->
    %% Xavier initialization: scale by sqrt(2 / (fan_in + fan_out))
    Scale = math:sqrt(2.0 / (FromSize + ToSize)),
    Weights = [
        [(rand:uniform() * 2 - 1) * Scale || _ <- lists:seq(1, FromSize)]
        || _ <- lists:seq(1, ToSize)
    ],
    Biases = [(rand:uniform() * 0.2 - 0.1) || _ <- lists:seq(1, ToSize)],
    {Weights, Biases}.

%% @private Forward propagate with separate output layer activation.
%% Hidden layers use Activation, the final layer uses OutputActivation.
forward_propagate([], Activations, _Activation, _OutputActivation) ->
    Activations;
forward_propagate([{Weights, Biases}], Inputs, _Activation, OutputActivation) ->
    %% Last layer — use output activation
    lists:zipwith(
        fun(NeuronWeights, Bias) ->
            Sum = dot_product(NeuronWeights, Inputs) + Bias,
            apply_activation(Sum, OutputActivation)
        end,
        Weights,
        Biases
    );
forward_propagate([{Weights, Biases} | RestLayers], Inputs, Activation, OutputActivation) ->
    %% Hidden layer — use regular activation
    Outputs = lists:zipwith(
        fun(NeuronWeights, Bias) ->
            Sum = dot_product(NeuronWeights, Inputs) + Bias,
            apply_activation(Sum, Activation)
        end,
        Weights,
        Biases
    ),
    forward_propagate(RestLayers, Outputs, Activation, OutputActivation).

%% @private Dot product of two vectors
dot_product(Weights, Inputs) ->
    lists:sum(lists:zipwith(fun(W, I) -> W * I end, Weights, Inputs)).

%% @private Apply activation function
apply_activation(X, tanh) ->
    math:tanh(X);
apply_activation(X, sigmoid) ->
    1.0 / (1.0 + math:exp(-X));
apply_activation(X, relu) ->
    max(0.0, X);
apply_activation(X, linear) ->
    X;
apply_activation(X, _) ->
    math:tanh(X).

%% @private Resolve output activation: undefined means same as hidden activation
resolve_output_activation(undefined, Activation) -> Activation;
resolve_output_activation(OutputActivation, _Activation) -> OutputActivation.

%% @private Reshape flat weights into matrix
reshape_weights(FlatWeights, RowSize) ->
    reshape_weights(FlatWeights, RowSize, []).

reshape_weights([], _RowSize, Acc) ->
    lists:reverse(Acc);
reshape_weights(Weights, RowSize, Acc) ->
    {Row, Rest} = lists:split(RowSize, Weights),
    reshape_weights(Rest, RowSize, [Row | Acc]).

%%==============================================================================
%% NIF Compilation
%%==============================================================================

%% @doc Compile network for NIF acceleration.
%%
%% If the NIF is loaded, compiles the network to a flat representation
%% that can be evaluated much faster. Falls back to Erlang evaluation
%% if NIF is not available.
%%
%% WARNING: Use sparingly! Each compiled network holds a Rust ResourceArc
%% reference that keeps native memory alive. During neuroevolution, do NOT
%% compile networks automatically (especially in create_feedforward or
%% set_weights) as this causes massive memory leaks - one compiled_ref
%% per offspring per generation accumulates unboundedly.
%%
%% Only call this when you need maximum performance for a specific network
%% that will be evaluated many times (e.g., the final champion network).
-spec compile_for_nif(network()) -> network().
compile_for_nif(Network = #network{layers = Layers, activation = Activation,
                                   output_activation = OutputActivation}) ->
    case tweann_nif:is_loaded() of
        true ->
            try
                OA = resolve_output_activation(OutputActivation, Activation),
                {Nodes, InputCount, OutputIndices} = build_nif_network(Layers, Activation, OA),
                CompiledRef = tweann_nif:compile_network(Nodes, InputCount, OutputIndices),
                Network#network{compiled_ref = CompiledRef}
            catch
                _:_ ->
                    %% Compilation failed, use Erlang fallback
                    Network
            end;
        false ->
            Network
    end.

%% @private Build the flat node representation for NIF compilation.
%%
%% Converts the layer-based structure to a flat list of nodes in
%% topological order (inputs first, then hidden layers, then outputs).
%%
%% Node format: {Index, Type, Activation, Bias, [{FromIndex, Weight}, ...]}
-spec build_nif_network([layer()], atom(), atom()) ->
    {Nodes :: list(), InputCount :: non_neg_integer(), OutputIndices :: [non_neg_integer()]}.
build_nif_network(Layers, Activation, OutputActivation) ->
    LayerSizes = extract_layer_sizes(Layers),
    InputCount = hd(LayerSizes),
    NumLayers = length(Layers),

    %% Build input nodes (no connections, linear activation)
    InputNodes = [{I, input, linear, 0.0, []} || I <- lists:seq(0, InputCount - 1)],

    %% Build hidden and output nodes layer by layer
    {HiddenOutputNodes, _} = lists:foldl(
        fun({WeightMatrix, Biases}, {Acc, {PrevLayerStart, LayerNum}}) ->
            PrevLayerSize = length(hd(WeightMatrix)),
            CurrentLayerStart = PrevLayerStart + PrevLayerSize,

            %% Use output activation for the last layer
            LayerActivation = case LayerNum of
                NumLayers -> OutputActivation;
                _ -> Activation
            end,

            %% Each row in WeightMatrix is weights for one neuron
            %% WeightMatrix[neuron][input] = weight from input to neuron
            NewNodes = lists:zipwith(
                fun(NeuronWeights, Bias) ->
                    NeuronIdx = CurrentLayerStart + length(Acc) - length(InputNodes),
                    Connections = [{PrevLayerStart + I, W}
                                   || {I, W} <- lists:zip(
                                        lists:seq(0, length(NeuronWeights) - 1),
                                        NeuronWeights)],
                    {NeuronIdx, hidden, LayerActivation, Bias, Connections}
                end,
                WeightMatrix,
                Biases
            ),
            {Acc ++ NewNodes, {CurrentLayerStart, LayerNum + 1}}
        end,
        {[], {0, 1}},
        Layers
    ),

    %% Combine all nodes
    AllNodes = InputNodes ++ HiddenOutputNodes,

    %% Re-index all nodes sequentially from 0
    {FinalNodes, _} = lists:mapfoldl(
        fun({_OldIdx, Type, Act, Bias, Conns}, NewIdx) ->
            {{NewIdx, Type, Act, Bias, Conns}, NewIdx + 1}
        end,
        0,
        AllNodes
    ),

    %% Calculate output indices (last layer nodes)
    TotalNodes = length(FinalNodes),
    OutputCount = length(element(2, lists:last(Layers))),
    OutputIndices = lists:seq(TotalNodes - OutputCount, TotalNodes - 1),

    {FinalNodes, InputCount, OutputIndices}.

%% @private Load genotype structure from ETS
load_genotype_structure(AgentId) ->
    case genotype:dirty_read({agent, AgentId}) of
        undefined ->
            {error, agent_not_found};
        Agent ->
            CxId = element(3, Agent), %% #agent.cx_id
            case genotype:dirty_read({cortex, CxId}) of
                undefined ->
                    {error, cortex_not_found};
                Cortex ->
                    %% Load neurons
                    NeuronIds = element(5, Cortex), %% #cortex.neuron_ids
                    Neurons = [genotype:dirty_read({neuron, NId})
                               || NId <- NeuronIds],

                    %% Load sensors for input count
                    SensorIds = element(6, Cortex), %% #cortex.sensor_ids
                    Sensors = [genotype:dirty_read({sensor, SId})
                               || SId <- SensorIds],

                    %% Load actuators for output count
                    ActuatorIds = element(7, Cortex), %% #cortex.actuator_ids
                    Actuators = [genotype:dirty_read({actuator, AId})
                                 || AId <- ActuatorIds],

                    {ok, {Sensors, Neurons, Actuators}}
            end
    end.

%% @private Build network from genotype structure
%%
%% NOTE: This creates a feedforward approximation of the evolved topology.
%% TWEANN genotypes have arbitrary topology (including recurrent connections)
%% which cannot be represented in the matrix-based feedforward format used
%% by network_evaluator. For exact topology evaluation, use:
%%   tweann_nif:compile_network/3 + tweann_nif:evaluate/2
%% which supports arbitrary DAG and recurrent topologies.
%%
%% This function is primarily useful for visualization and analysis where
%% a feedforward approximation is acceptable.
build_network_from_structure({Sensors, Neurons, Actuators}) ->
    %% Calculate sizes
    InputSize = lists:sum([element(8, S) || S <- Sensors]), %% #sensor.vl
    OutputSize = lists:sum([element(7, A) || A <- Actuators]), %% #actuator.vl
    HiddenCount = length(Neurons),

    %% Create a feedforward approximation with similar structure
    %% Cannot preserve exact topology due to format limitations
    HiddenSizes = case HiddenCount of
        0 -> [];
        N when N < 10 -> [N];
        N -> [N div 2, N div 2]
    end,

    %% Create network with random weights (topology approximation only)
    %% For exact weight reproduction, use tweann_nif:compile_network/3
    create_feedforward(InputSize, HiddenSizes, OutputSize).

%%==============================================================================
%% Visualization Functions
%%==============================================================================

%% @doc Evaluate network and return all layer activations.
%%
%% Returns {Outputs, AllActivations} where AllActivations is a list of
%% activation vectors for each layer (including input and output).
-spec evaluate_with_activations(network(), [float()]) ->
    {Outputs :: [float()], Activations :: [[float()]]}.
evaluate_with_activations(#network{layers = Layers, activation = Activation,
                                   output_activation = OutputActivation}, Inputs) ->
    OA = resolve_output_activation(OutputActivation, Activation),
    {Outputs, Activations} = forward_propagate_with_activations(Layers, Inputs, Activation, OA, [Inputs]),
    {Outputs, lists:reverse(Activations)}.

%% @doc Get network topology information for visualization.
%%
%% Returns a map with layer sizes for rendering the network structure.
-spec get_topology(network()) -> map().
get_topology(#network{layers = Layers}) ->
    %% Extract layer sizes from weight matrices
    LayerSizes = extract_layer_sizes(Layers),
    #{
        layer_sizes => LayerSizes,
        num_layers => length(LayerSizes),
        total_neurons => lists:sum(LayerSizes),
        total_connections => count_connections(Layers)
    }.

%% @doc Get visualization data for rendering the network.
%%
%% Combines topology, weights, and activations into a format suitable
%% for frontend visualization.
%%
%% @param Network The network record
%% @param Inputs Current input values (for activation display)
%% @param InputLabels Optional labels for input neurons
%% @returns Map with nodes, connections, and metadata
-spec get_viz_data(network(), [float()], [binary()]) -> map().
get_viz_data(Network = #network{layers = Layers}, Inputs, InputLabels) ->
    %% Get activations for current inputs
    {Outputs, AllActivations} = evaluate_with_activations(Network, Inputs),

    %% Build layer sizes
    LayerSizes = extract_layer_sizes(Layers),

    %% Build node data with positions and activations
    Nodes = build_viz_nodes(LayerSizes, AllActivations, InputLabels),

    %% Build connection data with weights
    Connections = build_viz_connections(Layers, LayerSizes),

    #{
        nodes => Nodes,
        connections => Connections,
        layer_sizes => LayerSizes,
        outputs => Outputs
    }.

%% @private Forward propagate and collect all activations with separate output activation.
forward_propagate_with_activations([], Activations, _Activation, _OutputActivation, AllActivations) ->
    {Activations, AllActivations};
forward_propagate_with_activations([{Weights, Biases}], Inputs, _Activation, OutputActivation, AllActivations) ->
    %% Last layer — use output activation
    Outputs = lists:zipwith(
        fun(NeuronWeights, Bias) ->
            Sum = dot_product(NeuronWeights, Inputs) + Bias,
            apply_activation(Sum, OutputActivation)
        end,
        Weights,
        Biases
    ),
    {Outputs, [Outputs | AllActivations]};
forward_propagate_with_activations([{Weights, Biases} | RestLayers], Inputs, Activation, OutputActivation, AllActivations) ->
    %% Hidden layer — use regular activation
    Outputs = lists:zipwith(
        fun(NeuronWeights, Bias) ->
            Sum = dot_product(NeuronWeights, Inputs) + Bias,
            apply_activation(Sum, Activation)
        end,
        Weights,
        Biases
    ),
    forward_propagate_with_activations(RestLayers, Outputs, Activation, OutputActivation, [Outputs | AllActivations]).

%% @private Extract layer sizes from weight matrices
extract_layer_sizes([]) ->
    [];
extract_layer_sizes([{Weights, _Biases} | Rest]) ->
    %% First layer: input size is the width of weight matrix
    InputSize = length(hd(Weights)),
    %% All layers: output size is the height of weight matrix
    OutputSizes = [length(W) || {W, _} <- [{Weights, undefined} | Rest]],
    [InputSize | OutputSizes].

%% @private Count total connections
count_connections(Layers) ->
    lists:sum([length(Weights) * length(hd(Weights)) || {Weights, _} <- Layers]).

%% @private Build node data for visualization
build_viz_nodes(LayerSizes, AllActivations, InputLabels) ->
    NumLayers = length(LayerSizes),
    lists:flatten(
        lists:zipwith3(
            fun(LayerIdx, LayerSize, Activations) ->
                Labels = case LayerIdx of
                    1 -> pad_labels(InputLabels, LayerSize);
                    N when N == NumLayers -> output_labels(LayerSize);
                    _ -> hidden_labels(LayerSize)
                end,
                build_layer_nodes(LayerIdx, LayerSize, Activations, Labels, NumLayers)
            end,
            lists:seq(1, NumLayers),
            LayerSizes,
            AllActivations
        )
    ).

%% @private Build nodes for a single layer
build_layer_nodes(LayerIdx, LayerSize, Activations, Labels, NumLayers) ->
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
        lists:seq(1, LayerSize),
        Activations,
        Labels
    ).

%% @private Build connection data for visualization
build_viz_connections(Layers, LayerSizes) ->
    {Connections, _} = lists:foldl(
        fun({Weights, _Biases}, {Acc, LayerIdx}) ->
            FromSize = lists:nth(LayerIdx, LayerSizes),
            ToSize = lists:nth(LayerIdx + 1, LayerSizes),
            LayerConns = [
                #{
                    from => {LayerIdx, FromIdx},
                    to => {LayerIdx + 1, ToIdx},
                    weight => lists:nth(FromIdx, lists:nth(ToIdx, Weights))
                }
                || ToIdx <- lists:seq(1, ToSize),
                   FromIdx <- lists:seq(1, FromSize)
            ],
            {Acc ++ LayerConns, LayerIdx + 1}
        end,
        {[], 1},
        Layers
    ),
    Connections.

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

%%==============================================================================
%% Serialization Functions
%%==============================================================================

%% @doc Serialize a network to a JSON-compatible map.
%%
%% The output format is suitable for JSON encoding and can be loaded
%% in other runtimes (Python, JavaScript, etc.) for inference.
%%
%% Format: A map with keys "version", "activation", and "layers".
%% The layers list contains maps with "weights" and "biases" keys.
%%
%% @param Network The network record
%% @returns Map suitable for JSON encoding
-spec to_json(network()) -> map().
to_json(#network{layers = Layers, activation = Activation, output_activation = OA}) ->
    LayerList = [
        #{<<"weights">> => Weights, <<"biases">> => Biases}
        || {Weights, Biases} <- Layers
    ],
    Base = #{
        <<"activation">> => atom_to_binary(Activation, utf8),
        <<"layers">> => LayerList
    },
    case OA of
        undefined ->
            Base#{<<"version">> => 1};
        _ ->
            Base#{<<"version">> => 2,
                  <<"output_activation">> => atom_to_binary(OA, utf8)}
    end.

%% @doc Deserialize a network from a JSON-compatible map.
%%
%% Accepts the format produced by to_json/1.
%%
%% @param JsonMap Map from JSON decoding
%% @returns {ok, Network} | {error, Reason}
-spec from_json(map()) -> {ok, network()} | {error, term()}.
from_json(#{<<"version">> := 1, <<"activation">> := ActivationBin, <<"layers">> := LayerMaps}) ->
    try
        Activation = binary_to_atom(ActivationBin, utf8),
        Layers = [
            {maps:get(<<"weights">>, L), maps:get(<<"biases">>, L)}
            || L <- LayerMaps
        ],
        {ok, #network{layers = Layers, activation = Activation,
                      output_activation = undefined}}
    catch
        _:Reason ->
            {error, {invalid_network_format, Reason}}
    end;
from_json(#{<<"version">> := 2, <<"activation">> := ActivationBin, <<"layers">> := LayerMaps} = Map) ->
    try
        Activation = binary_to_atom(ActivationBin, utf8),
        OA = case maps:get(<<"output_activation">>, Map, undefined) of
            undefined -> undefined;
            OABin -> binary_to_atom(OABin, utf8)
        end,
        Layers = [
            {maps:get(<<"weights">>, L), maps:get(<<"biases">>, L)}
            || L <- LayerMaps
        ],
        {ok, #network{layers = Layers, activation = Activation,
                      output_activation = OA}}
    catch
        _:Reason ->
            {error, {invalid_network_format, Reason}}
    end;
from_json(_) ->
    {error, unsupported_version}.

%% @doc Serialize a network to binary using Erlang term format.
%%
%% This is more compact than JSON and preserves exact floating point values.
%% Use this for Erlang-to-Erlang transfer or storage.
%%
%% @param Network The network record
%% @returns Binary representation
-spec to_binary(network()) -> binary().
to_binary(Network) ->
    term_to_binary(Network, [compressed]).

%% @doc Deserialize a network from binary.
%%
%% @param Binary Binary from to_binary/1
%% @returns {ok, Network} | {error, Reason}
-spec from_binary(binary()) -> {ok, network()} | {error, term()}.
from_binary(Binary) when is_binary(Binary) ->
    try
        case binary_to_term(Binary) of
            #network{} = Network ->
                {ok, Network};
            _ ->
                {error, invalid_network}
        end
    catch
        _:Reason ->
            {error, {deserialize_failed, Reason}}
    end;
from_binary(_) ->
    {error, invalid_binary}.
