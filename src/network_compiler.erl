%% @doc Network compiler for NIF-accelerated evaluation.
%%
%% This module compiles TWEANN genotypes from Mnesia records into
%% a flat representation suitable for the Rust NIF evaluator.
%%
%% == Compilation Process ==
%%
%% 1. Load genotype records from Mnesia (cortex, neurons, sensors, actuators)
%% 2. Build a node graph with connections
%% 3. Topologically sort nodes (inputs -> hidden -> outputs)
%% 4. Convert to flat indexed representation
%% 5. Call tweann_nif:compile_network/3
%%
%% == Usage ==
%%
%% Compile a genotype for fast evaluation:
%%   {ok, Network} = network_compiler:compile(AgentId)
%%   Outputs = tweann_nif:evaluate(Network, Inputs)
%%
%% Or compile from in-memory records:
%%   {ok, Network} = network_compiler:compile_from_records(Cortex, Neurons, Sensors, Actuators)
%%
%% @copyright 2024-2026 R.G. Lefever
-module(network_compiler).

-include("records.hrl").

-export([
    compile/1,
    compile_from_records/4,
    compile_simple/3
]).

%% @doc Compile a genotype from Mnesia for NIF evaluation.
%%
%% Loads the agent's neural network from Mnesia and compiles it
%% to a format suitable for the Rust NIF.
%%
%% @param AgentId The agent identifier
%% @returns {ok, NetworkRef} | {error, Reason}
-spec compile(AgentId :: term()) ->
    {ok, reference()} |
    {error, {mnesia_error, term()}} |
    {error, {compilation_failed, term(), [{atom(), atom(), non_neg_integer(), term()}]}}.
compile(AgentId) ->
    case load_genotype(AgentId) of
        {ok, Cortex, Neurons, Sensors, Actuators} ->
            compile_from_records(Cortex, Neurons, Sensors, Actuators);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compile from in-memory records.
%%
%% Use this when you already have the network records loaded.
%%
%% @param Cortex The cortex record
%% @param Neurons List of neuron records
%% @param Sensors List of sensor records
%% @param Actuators List of actuator records
%% @returns {ok, NetworkRef} | {error, Reason}
-spec compile_from_records(
    Cortex :: #cortex{},
    Neurons :: [#neuron{}],
    Sensors :: [#sensor{}],
    Actuators :: [#actuator{}]
) -> {ok, reference()} | {error, term()}.
compile_from_records(_Cortex, Neurons, Sensors, Actuators) ->
    try
        %% Build ID to index mapping
        {IdToIndex, InputCount, OutputIndices} =
            build_index_mapping(Sensors, Neurons, Actuators),

        %% Build node list in topological order
        Nodes = build_node_list(Sensors, Neurons, Actuators, IdToIndex),

        %% Compile via NIF
        Network = tweann_nif:compile_network(Nodes, InputCount, OutputIndices),
        {ok, Network}
    catch
        error:Reason:Stack ->
            {error, {compilation_failed, Reason, Stack}}
    end.

%% @doc Compile a simple feedforward network.
%%
%% Convenience function for creating simple networks without Mnesia.
%% Useful for testing and examples.
%%
%% @param InputCount Number of input nodes
%% @param HiddenLayers List of hidden layer sizes, e.g., [4, 3] for 2 layers
%% @param OutputCount Number of output nodes
%% @returns {ok, NetworkRef, Weights} where Weights can be used to set weights
-spec compile_simple(
    InputCount :: pos_integer(),
    HiddenLayers :: [pos_integer()],
    OutputCount :: pos_integer()
) -> {ok, reference(), [{non_neg_integer(), non_neg_integer(), float()}]}.
compile_simple(InputCount, HiddenLayers, OutputCount) ->
    %% Generate nodes and random weights
    {Nodes, Weights, TotalNodes} =
        generate_simple_network(InputCount, HiddenLayers, OutputCount),

    %% Output indices are the last OutputCount nodes
    OutputIndices = lists:seq(TotalNodes - OutputCount, TotalNodes - 1),

    %% Compile
    Network = tweann_nif:compile_network(Nodes, InputCount, OutputIndices),
    {ok, Network, Weights}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private Load genotype from ETS
load_genotype(AgentId) ->
    case genotype:dirty_read({agent, AgentId}) of
        undefined ->
            {error, {agent_not_found, AgentId}};
        Agent ->
            CxId = Agent#agent.cx_id,
            case genotype:dirty_read({cortex, CxId}) of
                undefined ->
                    {error, {cortex_not_found, CxId}};
                Cortex ->
                    Neurons = [genotype:dirty_read({neuron, NId})
                               || NId <- Cortex#cortex.neuron_ids],
                    Sensors = [genotype:dirty_read({sensor, SId})
                               || SId <- Cortex#cortex.sensor_ids],
                    Actuators = [genotype:dirty_read({actuator, AId})
                                 || AId <- Cortex#cortex.actuator_ids],
                    {ok, Cortex, Neurons, Sensors, Actuators}
            end
    end.

%% @private Build mapping from record IDs to flat indices
build_index_mapping(Sensors, Neurons, Actuators) ->
    %% Sensors become inputs (index 0..N-1)
    SensorIds = [S#sensor.id || S <- Sensors],
    InputCount = length(SensorIds),

    %% Sort neurons by layer coordinate for topological order
    SortedNeurons = lists:sort(
        fun(N1, N2) ->
            {{L1, _}, _} = N1#neuron.id,
            {{L2, _}, _} = N2#neuron.id,
            L1 =< L2
        end,
        Neurons
    ),
    NeuronIds = [N#neuron.id || N <- SortedNeurons],

    %% Actuators become outputs (last indices)
    ActuatorIds = [A#actuator.id || A <- Actuators],

    %% Build ID -> Index map
    AllIds = SensorIds ++ NeuronIds ++ ActuatorIds,
    IdToIndex = maps:from_list(
        [{Id, Idx} || {Id, Idx} <- lists:zip(AllIds, lists:seq(0, length(AllIds) - 1))]
    ),

    %% Output indices
    OutputIndices = [maps:get(Id, IdToIndex) || Id <- ActuatorIds],

    {IdToIndex, InputCount, OutputIndices}.

%% @private Build node list in format expected by NIF
build_node_list(Sensors, Neurons, Actuators, IdToIndex) ->
    %% Sensors (inputs) - no connections, linear activation
    SensorNodes = [
        {maps:get(S#sensor.id, IdToIndex), input, linear, 0.0, []}
        || S <- Sensors
    ],

    %% Sort neurons by layer for topological order
    SortedNeurons = lists:sort(
        fun(N1, N2) ->
            {{L1, _}, _} = N1#neuron.id,
            {{L2, _}, _} = N2#neuron.id,
            L1 =< L2
        end,
        Neurons
    ),

    %% Neurons (hidden)
    NeuronNodes = [
        begin
            Idx = maps:get(N#neuron.id, IdToIndex),
            Activation = N#neuron.af,
            %% Extract bias from weights (first weight with bias marker or 0.0)
            Bias = extract_bias(N#neuron.input_idps),
            %% Convert input connections
            Connections = convert_connections(N#neuron.input_idps, IdToIndex),
            {Idx, hidden, Activation, Bias, Connections}
        end
        || N <- SortedNeurons
    ],

    %% Actuators (outputs) - receive from neurons, linear activation
    ActuatorNodes = [
        begin
            Idx = maps:get(A#actuator.id, IdToIndex),
            %% Actuators receive from their fanin neurons with weight 1.0
            Connections = [
                {maps:get(FromId, IdToIndex), 1.0}
                || FromId <- A#actuator.fanin_ids,
                   maps:is_key(FromId, IdToIndex)
            ],
            {Idx, output, linear, 0.0, Connections}
        end
        || A <- Actuators
    ],

    SensorNodes ++ NeuronNodes ++ ActuatorNodes.

%% @private Extract bias from weighted inputs
extract_bias(InputIdps) ->
    %% Look for bias input (special marker) or return 0.0
    case lists:keyfind(bias, 1, InputIdps) of
        {bias, [{W, _, _, _} | _]} -> W;
        {bias, W} when is_number(W) -> W;
        false -> 0.0
    end.

%% @private Convert input_idps to connection list
convert_connections(InputIdps, IdToIndex) ->
    lists:flatmap(
        fun
            ({bias, _}) ->
                %% Skip bias - handled separately
                [];
            ({FromId, WeightList}) when is_list(WeightList) ->
                case maps:find(FromId, IdToIndex) of
                    {ok, FromIdx} ->
                        %% Sum weights if multiple (shouldn't happen normally)
                        TotalWeight = lists:sum([W || {W, _, _, _} <- WeightList]),
                        [{FromIdx, TotalWeight}];
                    error ->
                        []
                end;
            ({FromId, Weight}) when is_number(Weight) ->
                case maps:find(FromId, IdToIndex) of
                    {ok, FromIdx} -> [{FromIdx, Weight}];
                    error -> []
                end
        end,
        InputIdps
    ).

%% @private Generate a simple feedforward network
generate_simple_network(InputCount, HiddenLayers, OutputCount) ->
    %% Calculate total nodes
    HiddenTotal = lists:sum(HiddenLayers),
    TotalNodes = InputCount + HiddenTotal + OutputCount,

    %% Generate input nodes
    InputNodes = [
        {I, input, linear, 0.0, []}
        || I <- lists:seq(0, InputCount - 1)
    ],

    %% Generate hidden layers with random weights
    {HiddenNodes, Weights1, NextIdx1, PrevLayerStart, PrevLayerEnd} =
        generate_hidden_layers(InputCount, HiddenLayers),

    %% Generate output layer - connects to last hidden layer, or inputs if no hidden
    {OutputNodes, Weights2} =
        generate_output_layer(NextIdx1, OutputCount, PrevLayerStart, PrevLayerEnd),

    AllNodes = InputNodes ++ HiddenNodes ++ OutputNodes,
    AllWeights = Weights1 ++ Weights2,

    {AllNodes, AllWeights, TotalNodes}.

%% @private Generate hidden layers
generate_hidden_layers(InputCount, []) ->
    %% No hidden layers - outputs will connect directly to inputs
    {[], [], InputCount, 0, InputCount - 1};
generate_hidden_layers(InputCount, HiddenLayers) ->
    generate_hidden_layers_acc(HiddenLayers, 0, InputCount - 1, InputCount, [], []).

generate_hidden_layers_acc([], PrevStart, PrevEnd, NextIdx, NodesAcc, WeightsAcc) ->
    {lists:reverse(NodesAcc), lists:flatten(lists:reverse(WeightsAcc)), NextIdx, PrevStart, PrevEnd};
generate_hidden_layers_acc([LayerSize | Rest], PrevStart, PrevEnd, StartIdx, NodesAcc, WeightsAcc) ->
    %% Generate nodes for this layer
    {LayerNodes, LayerWeights} = lists:unzip([
        begin
            Idx = StartIdx + I,
            %% Connect to all nodes in previous layer
            Connections = [
                {PrevIdx, random_weight()}
                || PrevIdx <- lists:seq(PrevStart, PrevEnd)
            ],
            Bias = random_weight() * 0.1,
            Node = {Idx, hidden, tanh, Bias, Connections},
            Weights = [{Idx, PrevIdx, W} || {PrevIdx, W} <- Connections],
            {Node, Weights}
        end
        || I <- lists:seq(0, LayerSize - 1)
    ]),

    NewPrevStart = StartIdx,
    NewPrevEnd = StartIdx + LayerSize - 1,
    generate_hidden_layers_acc(Rest, NewPrevStart, NewPrevEnd, StartIdx + LayerSize,
                               LayerNodes ++ NodesAcc, [LayerWeights | WeightsAcc]).

%% @private Generate output layer
generate_output_layer(StartIdx, OutputCount, PrevStart, PrevEnd) ->
    {Nodes, NestedWeights} = lists:unzip([
        begin
            Idx = StartIdx + I,
            Connections = [
                {PrevIdx, random_weight()}
                || PrevIdx <- lists:seq(PrevStart, PrevEnd)
            ],
            Node = {Idx, output, tanh, 0.0, Connections},
            Weights = [{Idx, PrevIdx, W} || {PrevIdx, W} <- Connections],
            {Node, Weights}
        end
        || I <- lists:seq(0, OutputCount - 1)
    ]),
    {Nodes, lists:flatten(NestedWeights)}.

%% @private Generate random weight in [-1, 1]
random_weight() ->
    rand:uniform() * 2 - 1.
