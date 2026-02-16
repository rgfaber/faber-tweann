%% @doc ONNX export for neural networks.
%%
%% Exports networks to ONNX (Open Neural Network Exchange) format for
%% cross-platform inference in Python, JavaScript, C++, mobile, and edge devices.
%%
%% == Usage ==
%%
%% Export a trained network:
%%   {ok, OnnxBinary} = network_onnx:to_onnx(Network)
%%   file:write_file("model.onnx", OnnxBinary)
%%
%% Load in Python:
%%   import onnxruntime as ort
%%   session = ort.InferenceSession("model.onnx")
%%   outputs = session.run(None, {"input": inputs})
%%
%% Load in JavaScript:
%%   const session = await ort.InferenceSession.create("model.onnx")
%%   const results = await session.run({input: tensor})
%%
%% @copyright 2024-2026 R.G. Lefever
-module(network_onnx).

-export([to_onnx/1, to_onnx/2]).

%% ONNX IR version (9 = ONNX 1.14+)
-define(IR_VERSION, 9).
%% Default opset version (18 = latest stable)
-define(OPSET_VERSION, 18).
%% TensorProto.DataType.FLOAT
-define(FLOAT_TYPE, 1).

%% Protobuf wire types
-define(WIRE_VARINT, 0).
-define(WIRE_64BIT, 1).
-define(WIRE_LENGTH_DELIMITED, 2).
-define(WIRE_32BIT, 5).

%%==============================================================================
%% Public API
%%==============================================================================

%% @doc Export network to ONNX format.
%%
%% @param Network The network record from network_evaluator
%% @returns {ok, Binary} | {error, Reason}
-spec to_onnx(network_evaluator:network()) -> {ok, binary()} | {error, term()}.
to_onnx(Network) ->
    to_onnx(Network, #{}).

%% @doc Export network to ONNX format with options.
%%
%% Options:
%%   model_name - Name of the model (default: "macula_network")
%%   producer - Producer name (default: "faber-tweann")
%%
%% @param Network The network record
%% @param Opts Options map
%% @returns {ok, Binary} | {error, Reason}
-spec to_onnx(network_evaluator:network(), map()) -> {ok, binary()} | {error, term()}.
to_onnx(Network, Opts) ->
    try
        ModelProto = build_model(Network, Opts),
        {ok, ModelProto}
    catch
        error:Reason:Stack ->
            {error, {onnx_export_failed, Reason, Stack}}
    end.

%%==============================================================================
%% Model Building
%%==============================================================================

%% @private Build the complete ONNX ModelProto
build_model(Network, Opts) ->
    {Layers, Activation} = get_network_data(Network),
    LayerSizes = get_layer_sizes(Layers),

    ModelName = maps:get(model_name, Opts, <<"macula_network">>),
    Producer = maps:get(producer, Opts, <<"faber-tweann">>),

    %% Build graph
    GraphProto = build_graph(Layers, Activation, LayerSizes, ModelName),

    %% Build model
    encode_model_proto(#{
        ir_version => ?IR_VERSION,
        producer_name => Producer,
        producer_version => <<"1.0.0">>,
        model_version => 1,
        opset_import => [#{domain => <<>>, version => ?OPSET_VERSION}],
        graph => GraphProto
    }).

%% @private Extract network data
get_network_data(Network) when is_tuple(Network) ->
    %% Record: #network{layers, activation, output_activation, compiled_ref}
    %% Handle old and new record formats
    case Network of
        {network, Layers, Activation, _OutputActivation, _CompiledRef} ->
            {Layers, Activation};
        {network, Layers, Activation, _CompiledRef} ->
            {Layers, Activation};
        {network, Layers, Activation} ->
            {Layers, Activation}
    end;
get_network_data(#{layers := Layers, activation := Activation}) ->
    {Layers, Activation}.

%% @private Calculate layer sizes from weight matrices
get_layer_sizes([]) -> [];
get_layer_sizes([{Weights, _Biases} | Rest]) ->
    InputSize = length(hd(Weights)),
    OutputSizes = [length(W) || {W, _} <- [{Weights, undefined} | Rest]],
    [InputSize | OutputSizes].

%%==============================================================================
%% Graph Building
%%==============================================================================

%% @private Build the ONNX GraphProto
build_graph(Layers, Activation, LayerSizes, Name) ->
    InputSize = hd(LayerSizes),
    OutputSize = lists:last(LayerSizes),

    %% Build nodes (operations)
    {Nodes, _} = lists:foldl(
        fun({Weights, Biases}, {Acc, LayerIdx}) ->
            LayerNodes = build_layer_nodes(LayerIdx, Weights, Biases, Activation, length(Layers)),
            {Acc ++ LayerNodes, LayerIdx + 1}
        end,
        {[], 1},
        Layers
    ),

    %% Build initializers (weights and biases as tensors)
    Initializers = build_initializers(Layers),

    %% Build input/output value info
    InputInfo = build_value_info(<<"input">>, [1, InputSize]),
    OutputInfo = build_value_info(<<"output">>, [1, OutputSize]),

    encode_graph_proto(#{
        name => Name,
        node => Nodes,
        initializer => Initializers,
        input => [InputInfo],
        output => [OutputInfo]
    }).

%% @private Build nodes for a single layer
build_layer_nodes(LayerIdx, _Weights, _Biases, Activation, NumLayers) ->
    IdxStr = integer_to_binary(LayerIdx),

    %% Input name
    InputName = case LayerIdx of
        1 -> <<"input">>;
        _ -> <<"relu_", (integer_to_binary(LayerIdx - 1))/binary>>
    end,

    %% MatMul node
    WeightName = <<"weight_", IdxStr/binary>>,
    MatMulOut = <<"matmul_", IdxStr/binary>>,
    MatMulNode = encode_node_proto(#{
        input => [InputName, WeightName],
        output => [MatMulOut],
        op_type => <<"MatMul">>
    }),

    %% Add (bias) node
    BiasName = <<"bias_", IdxStr/binary>>,
    AddOut = <<"add_", IdxStr/binary>>,
    AddNode = encode_node_proto(#{
        input => [MatMulOut, BiasName],
        output => [AddOut],
        op_type => <<"Add">>
    }),

    %% Activation node (skip on last layer for some use cases, but we'll apply it)
    ActOut = case LayerIdx of
        NumLayers -> <<"output">>;
        _ -> <<"relu_", IdxStr/binary>>
    end,
    ActNode = encode_node_proto(#{
        input => [AddOut],
        output => [ActOut],
        op_type => activation_to_onnx(Activation)
    }),

    [MatMulNode, AddNode, ActNode].

%% @private Map activation function to ONNX op type
activation_to_onnx(tanh) -> <<"Tanh">>;
activation_to_onnx(sigmoid) -> <<"Sigmoid">>;
activation_to_onnx(relu) -> <<"Relu">>;
activation_to_onnx(linear) -> <<"Identity">>;
activation_to_onnx(_) -> <<"Tanh">>.

%% @private Build initializer tensors for all weights and biases
build_initializers(Layers) ->
    {Initializers, _} = lists:foldl(
        fun({Weights, Biases}, {Acc, LayerIdx}) ->
            IdxStr = integer_to_binary(LayerIdx),

            %% Weight tensor (transposed for MatMul: [in, out] -> [in, out])
            %% ONNX MatMul: [M, K] x [K, N] = [M, N]
            %% Our weights are [out, in], need to transpose to [in, out]
            TransposedWeights = transpose(Weights),
            InSize = length(hd(Weights)),
            OutSize = length(Weights),
            WeightTensor = encode_tensor_proto(
                <<"weight_", IdxStr/binary>>,
                [InSize, OutSize],
                lists:flatten(TransposedWeights)
            ),

            %% Bias tensor
            BiasTensor = encode_tensor_proto(
                <<"bias_", IdxStr/binary>>,
                [OutSize],
                Biases
            ),

            {Acc ++ [WeightTensor, BiasTensor], LayerIdx + 1}
        end,
        {[], 1},
        Layers
    ),
    Initializers.

%% @private Transpose a 2D list (matrix)
transpose([]) -> [];
transpose([[] | _]) -> [];
transpose(Matrix) ->
    [[lists:nth(I, Row) || Row <- Matrix] || I <- lists:seq(1, length(hd(Matrix)))].

%% @private Build ValueInfoProto for input/output
build_value_info(Name, Shape) ->
    encode_value_info_proto(Name, Shape).

%%==============================================================================
%% Protobuf Encoding
%%==============================================================================

%% @private Encode ModelProto
encode_model_proto(Model) ->
    Fields = [
        encode_field(1, varint, maps:get(ir_version, Model)),
        encode_field(2, bytes, maps:get(producer_name, Model)),
        encode_field(3, bytes, maps:get(producer_version, Model)),
        encode_field(5, varint, maps:get(model_version, Model)),
        encode_field(7, bytes, maps:get(graph, Model))
    ] ++ [encode_field(8, bytes, encode_opset(O)) || O <- maps:get(opset_import, Model, [])],
    iolist_to_binary(Fields).

%% @private Encode OperatorSetIdProto
encode_opset(#{domain := Domain, version := Version}) ->
    iolist_to_binary([
        encode_field(1, bytes, Domain),
        encode_field(2, varint, Version)
    ]).

%% @private Encode GraphProto
encode_graph_proto(Graph) ->
    Fields = [
        encode_field(2, bytes, maps:get(name, Graph, <<>>))
    ] ++
    [encode_field(1, bytes, N) || N <- maps:get(node, Graph, [])] ++
    [encode_field(5, bytes, I) || I <- maps:get(initializer, Graph, [])] ++
    [encode_field(11, bytes, I) || I <- maps:get(input, Graph, [])] ++
    [encode_field(12, bytes, O) || O <- maps:get(output, Graph, [])],
    iolist_to_binary(Fields).

%% @private Encode NodeProto
encode_node_proto(Node) ->
    Fields =
        [encode_field(1, bytes, I) || I <- maps:get(input, Node, [])] ++
        [encode_field(2, bytes, O) || O <- maps:get(output, Node, [])] ++
        [encode_field(4, bytes, maps:get(op_type, Node))],
    iolist_to_binary(Fields).

%% @private Encode TensorProto with float data
encode_tensor_proto(Name, Dims, FloatData) ->
    %% Use raw_data for efficiency (little-endian floats)
    RawData = iolist_to_binary([<<F:32/float-little>> || F <- FloatData]),
    Fields = [
        encode_packed_field(1, varint, Dims),
        encode_field(2, varint, ?FLOAT_TYPE),
        encode_field(8, bytes, Name),
        encode_field(9, bytes, RawData)
    ],
    iolist_to_binary(Fields).

%% @private Encode ValueInfoProto
encode_value_info_proto(Name, Shape) ->
    TypeProto = encode_type_proto(Shape),
    Fields = [
        encode_field(1, bytes, Name),
        encode_field(2, bytes, TypeProto)
    ],
    iolist_to_binary(Fields).

%% @private Encode TypeProto for tensor
encode_type_proto(Shape) ->
    TensorType = encode_tensor_type(?FLOAT_TYPE, Shape),
    encode_field(1, bytes, TensorType).

%% @private Encode TypeProto.Tensor
encode_tensor_type(ElemType, Shape) ->
    ShapeProto = encode_shape_proto(Shape),
    Fields = [
        encode_field(1, varint, ElemType),
        encode_field(2, bytes, ShapeProto)
    ],
    iolist_to_binary(Fields).

%% @private Encode TensorShapeProto
encode_shape_proto(Dims) ->
    DimProtos = [encode_dim_proto(D) || D <- Dims],
    iolist_to_binary([encode_field(1, bytes, D) || D <- DimProtos]).

%% @private Encode TensorShapeProto.Dimension
encode_dim_proto(DimValue) ->
    encode_field(1, varint, DimValue).

%%==============================================================================
%% Low-level Protobuf Encoding
%%==============================================================================

%% @private Encode a protobuf field
encode_field(FieldNum, varint, Value) ->
    Tag = (FieldNum bsl 3) bor ?WIRE_VARINT,
    [encode_varint(Tag), encode_varint(Value)];
encode_field(FieldNum, bytes, Value) when is_binary(Value) ->
    Tag = (FieldNum bsl 3) bor ?WIRE_LENGTH_DELIMITED,
    [encode_varint(Tag), encode_varint(byte_size(Value)), Value];
encode_field(FieldNum, bytes, Value) when is_list(Value) ->
    encode_field(FieldNum, bytes, iolist_to_binary(Value)).

%% @private Encode a packed repeated field
encode_packed_field(FieldNum, varint, Values) ->
    Encoded = iolist_to_binary([encode_varint(V) || V <- Values]),
    encode_field(FieldNum, bytes, Encoded).

%% @private Encode a varint (variable-length integer)
encode_varint(N) when N < 128 ->
    [<<N>>];
encode_varint(N) ->
    [<<1:1, (N band 127):7>> | encode_varint(N bsr 7)].
