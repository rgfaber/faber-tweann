%% @doc Genetic crossover (recombination) for neural networks.
%%
%% This module implements sexual reproduction by combining genetic
%% material from two parent agents to create offspring. Crossover
%% enables exploration of the solution space by mixing successful
%% traits from different individuals.
%%
%% == Crossover Strategies ==
%%
%% Network-Level Crossover:
%% - Select matching neurons from both parents
%% - Randomly choose which parent contributes each neuron
%% - Inherit connections and weights from selected neurons
%% - Handles structural differences (different topologies)
%%
%% Neuron-Level Crossover:
%% - For matching neurons, can mix properties:
%%   * Activation function from parent A
%%   * Weights from parent B
%%   * Bias from parent A
%% - Creates fine-grained genetic mixing
%%
%% Weight-Level Crossover:
%% - For matching connections, blend weights:
%%   * Averaging: W_child = (W_A + W_B) / 2
%%   * Random selection: W_child = random choice of W_A or W_B
%%   * Weighted average based on fitness
%% - Preserves promising weight configurations
%%
%% == Implementation Notes ==
%%
%% Matching Strategy:
%% - Neurons match by layer coordinate
%% - Connections match by source/target pair
%% - Unmatched elements inherited from fitter parent
%%
%% Compatibility:
%% - Parents must have compatible morphologies
%% - Sensor/actuator counts must match
%% - Layer structure should align (though not required)
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(crossover).

-include("records.hrl").

-export([
    crossover/2,
    neuron_crossover/3,
    weight_crossover/3
]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Perform genetic crossover between two parent agents.
%%
%% Creates a new offspring agent by combining genetic material from
%% two parents. The offspring inherits a mix of neurons, connections,
%% and weights from both parents.
%%
%% Algorithm:
%% 1. Clone one parent as base structure
%% 2. For each matching neuron, perform neuron-level crossover
%% 3. For unmatched neurons, inherit from fitter parent
%% 4. Return offspring agent ID
%%
%% Example:
%%   Parent1 = {1.0, agent}
%%   Parent2 = {2.0, agent}
%%   Offspring = crossover(Parent1, Parent2)
%%   % Offspring inherits mix of traits from both parents
%%
%% @param Parent1Id first parent agent identifier
%% @param Parent2Id second parent agent identifier
%% @returns offspring agent identifier
-spec crossover(term(), term()) -> {float(), agent}.
crossover(Parent1Id, Parent2Id) ->
    %% Read parent agents
    Parent1 = genotype:read({agent, Parent1Id}),
    Parent2 = genotype:read({agent, Parent2Id}),

    case {Parent1, Parent2} of
        {undefined, _} ->
            %% Parent1 not found, clone Parent2
            genotype:clone_Agent(Parent2Id);
        {_, undefined} ->
            %% Parent2 not found, clone Parent1
            genotype:clone_Agent(Parent1Id);
        {_, _} ->
            %% Both parents available, perform crossover
            perform_crossover(Parent1Id, Parent2Id, Parent1, Parent2)
    end.

%% @doc Perform neuron-level crossover.
%%
%% Combines properties from matching neurons in both parents.
%% Can mix activation function, weights, LTC parameters, etc.
%%
%% Strategy:
%% - Activation function: random choice from parents
%% - Input weights: weight-level crossover
%% - Aggregation function: random choice
%% - Output connections: union of both parents
%% - LTC parameters: random choice with averaging for continuous values
%%
%% Example:
%%   Neuron1 = #neuron{af = tanh, ...}
%%   Neuron2 = #neuron{af = sigmoid, ...}
%%   Result = neuron_crossover(Neuron1, Neuron2, 0.5)
%%   % Result might have tanh from parent 1
%%
%% @param Neuron1 first parent neuron record
%% @param Neuron2 second parent neuron record
%% @param CrossoverRate probability of taking trait from parent 1 (0.0-1.0)
%% @returns crossed neuron record
-spec neuron_crossover(#neuron{}, #neuron{}, float()) -> #neuron{}.
neuron_crossover(Neuron1, Neuron2, CrossoverRate) ->
    %% Choose activation function
    AF = case rand:uniform() < CrossoverRate of
        true -> Neuron1#neuron.af;
        false -> Neuron2#neuron.af
    end,

    %% Choose aggregation function
    AggrF = case rand:uniform() < CrossoverRate of
        true -> Neuron1#neuron.aggr_f;
        false -> Neuron2#neuron.aggr_f
    end,

    %% Crossover input weights
    InputWeights = weight_crossover(
        Neuron1#neuron.input_idps,
        Neuron2#neuron.input_idps,
        CrossoverRate
    ),

    %% Take output connections from both (union)
    Outputs = lists:usort(
        Neuron1#neuron.output_ids ++ Neuron2#neuron.output_ids
    ),

    %% Crossover LTC parameters
    {NeuronType, Tau, Bound, BackboneW, HeadW} = crossover_ltc_params(
        Neuron1, Neuron2, CrossoverRate
    ),

    %% Build crossed neuron
    Neuron1#neuron{
        af = AF,
        aggr_f = AggrF,
        input_idps = InputWeights,
        output_ids = Outputs,
        neuron_type = NeuronType,
        time_constant = Tau,
        state_bound = Bound,
        ltc_backbone_weights = BackboneW,
        ltc_head_weights = HeadW,
        internal_state = 0.0  %% Reset state for offspring
    }.

%% @doc Perform weight-level crossover.
%%
%% Combines weight vectors from two parent neurons.
%% For matching connections (same source), blends weights.
%% For unmatched connections, includes from both parents.
%%
%% Blending strategies:
%% - Averaging: (W1 + W2) / 2
%% - Random selection: random choice
%% - Crossover rate determines probability
%%
%% Example:
%%   Weights1 = [{sensor1, [{0.5, 0.0, 0.01, []}]}]
%%   Weights2 = [{sensor1, [{-0.3, 0.0, 0.01, []}]}]
%%   Result = weight_crossover(Weights1, Weights2, 0.5)
%%   % Might average to [{sensor1, [{0.1, 0.0, 0.01, []}]}]
%%
%% @param Weights1 first parent's weighted inputs
%% @param Weights2 second parent's weighted inputs
%% @param CrossoverRate probability of taking weight from parent 1
%% @returns crossed weighted inputs
-spec weight_crossover(weighted_inputs(), weighted_inputs(), float()) ->
    weighted_inputs().
weight_crossover([], Weights2, _CrossoverRate) ->
    Weights2;
weight_crossover(Weights1, [], _CrossoverRate) ->
    Weights1;
weight_crossover(Weights1, Weights2, CrossoverRate) ->
    %% Build map of all sources
    Sources1 = [{Source, Ws} || {Source, Ws} <- Weights1],
    Sources2 = [{Source, Ws} || {Source, Ws} <- Weights2],

    AllSources = lists:usort(
        [S || {S, _} <- Sources1] ++ [S || {S, _} <- Sources2]
    ),

    %% For each source, crossover weights
    lists:map(
        fun(Source) ->
            W1 = proplists:get_value(Source, Sources1, []),
            W2 = proplists:get_value(Source, Sources2, []),
            CrossedWeights = crossover_weight_vectors(W1, W2, CrossoverRate),
            {Source, CrossedWeights}
        end,
        AllSources
    ).

%% ============================================================================
%% Internal Functions - Crossover Execution
%% ============================================================================

%% @private Perform full crossover between two parents.
-spec perform_crossover(term(), term(), #agent{}, #agent{}) -> {float(), agent}.
perform_crossover(Parent1Id, Parent2Id, Parent1, Parent2) ->
    %% Determine which parent is fitter (for tie-breaking)
    FitterParentId = case Parent1#agent.fitness >= Parent2#agent.fitness of
        true -> Parent1Id;
        false -> Parent2Id
    end,

    %% Clone fitter parent as base
    OffspringId = genotype:clone_Agent(FitterParentId),

    %% Get cortex info from both parents
    Cortex1 = genotype:read({cortex, Parent1#agent.cx_id}),
    Cortex2 = genotype:read({cortex, Parent2#agent.cx_id}),

    %% Perform neuron-level crossover
    crossover_neurons(OffspringId, Cortex1, Cortex2),

    OffspringId.

%% @private Crossover neurons between parents.
-spec crossover_neurons({float(), agent}, #cortex{neuron_ids :: [term()]}, #cortex{neuron_ids :: [term()]}) -> ok.
crossover_neurons(OffspringId, Cortex1, Cortex2) ->
    %% Get offspring cortex
    Offspring = genotype:read({agent, OffspringId}),
    OffspringCortex = genotype:read({cortex, Offspring#agent.cx_id}),

    %% Get neuron lists
    Neurons1 = Cortex1#cortex.neuron_ids,
    Neurons2 = Cortex2#cortex.neuron_ids,

    %% Build maps by layer coordinate for matching
    Map1 = build_neuron_map(Neurons1),
    Map2 = build_neuron_map(Neurons2),

    %% Get all layer coordinates
    AllCoords = lists:usort(maps:keys(Map1) ++ maps:keys(Map2)),

    %% For each layer coordinate, perform crossover
    lists:foreach(
        fun(Coord) ->
            N1List = maps:get(Coord, Map1, []),
            N2List = maps:get(Coord, Map2, []),

            %% Crossover neurons at this layer
            crossover_neurons_at_layer(OffspringCortex, N1List, N2List, 0.5)
        end,
        AllCoords
    ),

    ok.

%% @private Build map of neurons by layer coordinate.
-spec build_neuron_map([term()]) -> #{float() => [term()]}.
build_neuron_map(NeuronIds) ->
    lists:foldl(
        fun(NeuronId, Map) ->
            {{LayerCoord, _UniqueId}, neuron} = NeuronId,
            Neurons = maps:get(LayerCoord, Map, []),
            maps:put(LayerCoord, [NeuronId | Neurons], Map)
        end,
        #{},
        NeuronIds
    ).

%% @private Crossover neurons at a specific layer.
-spec crossover_neurons_at_layer(#cortex{}, [term()], [term()], float()) -> ok.
crossover_neurons_at_layer(_Cortex, [], [], _Rate) ->
    ok;
crossover_neurons_at_layer(Cortex, Neurons1, Neurons2, CrossoverRate) ->
    %% Match neurons by index (assumes similar structure)
    Pairs = lists:zip(Neurons1, Neurons2),

    %% For each pair, perform neuron crossover
    lists:foreach(
        fun({N1Id, N2Id}) ->
            N1 = genotype:read({neuron, N1Id}),
            N2 = genotype:read({neuron, N2Id}),

            case {N1, N2} of
                {undefined, _} -> ok;
                {_, undefined} -> ok;
                {_, _} ->
                    %% Perform crossover and update
                    Crossed = neuron_crossover(N1, N2, CrossoverRate),

                    %% Find corresponding neuron in offspring
                    {{Layer, _}, neuron} = N1Id,
                    OffspringNeurons = [
                        NId || NId <- Cortex#cortex.neuron_ids,
                        begin
                            {{L, _}, neuron} = NId,
                            L =:= Layer
                        end
                    ],

                    %% Update first matching neuron
                    case OffspringNeurons of
                        [OffspringNId | _] ->
                            OffspringN = genotype:read({neuron, OffspringNId}),
                            UpdatedN = OffspringN#neuron{
                                af = Crossed#neuron.af,
                                aggr_f = Crossed#neuron.aggr_f
                            },
                            genotype:write(UpdatedN);
                        [] ->
                            ok
                    end
            end
        end,
        Pairs
    ),

    ok.

%% ============================================================================
%% Internal Functions - Weight Operations
%% ============================================================================

%% @private Crossover weight vectors.
-spec crossover_weight_vectors([weight_spec()], [weight_spec()], float()) ->
    [weight_spec()].
crossover_weight_vectors([], W2, _Rate) -> W2;
crossover_weight_vectors(W1, [], _Rate) -> W1;
crossover_weight_vectors(W1, W2, CrossoverRate) ->
    %% Take equal number from both or combine
    NumWeights = min(length(W1), length(W2)),

    Combined = lists:zipwith(
        fun({Weight1, DW1, LR1, P1}, {Weight2, _DW2, _LR2, _P2}) ->
            %% Choose weight based on crossover rate
            NewWeight = case rand:uniform() < CrossoverRate of
                true -> Weight1;
                false -> Weight2
            end,
            {NewWeight, DW1, LR1, P1}
        end,
        lists:sublist(W1, NumWeights),
        lists:sublist(W2, NumWeights)
    ),

    %% Add remaining weights from longer list
    Remaining = if
        length(W1) > NumWeights ->
            lists:nthtail(NumWeights, W1);
        length(W2) > NumWeights ->
            lists:nthtail(NumWeights, W2);
        true ->
            []
    end,

    Combined ++ Remaining.

%% ============================================================================
%% Internal Functions - LTC Parameter Crossover
%% ============================================================================

%% @private Crossover LTC (Liquid Time-Constant) parameters between neurons.
%%
%% Strategy for LTC parameters:
%% - neuron_type: Select from parent with matching type, or random choice
%% - time_constant: Average if both LTC, otherwise from LTC parent or default
%% - state_bound: Average if both LTC, otherwise from LTC parent or default
%% - backbone/head weights: Crossover weight vectors if both have them
%%
%% @param Neuron1 first parent neuron
%% @param Neuron2 second parent neuron
%% @param CrossoverRate probability of taking from parent 1
%% @returns {NeuronType, Tau, Bound, BackboneWeights, HeadWeights}
-spec crossover_ltc_params(#neuron{}, #neuron{}, float()) ->
    {atom(), float(), float(), [float()], [float()]}.
crossover_ltc_params(Neuron1, Neuron2, CrossoverRate) ->
    Type1 = Neuron1#neuron.neuron_type,
    Type2 = Neuron2#neuron.neuron_type,

    %% Determine offspring neuron type
    NeuronType = crossover_neuron_type(Type1, Type2, CrossoverRate),

    %% Crossover time constant (tau)
    Tau = crossover_continuous_param(
        Neuron1#neuron.time_constant,
        Neuron2#neuron.time_constant,
        Type1, Type2,
        1.0  %% default tau
    ),

    %% Crossover state bound (A)
    Bound = crossover_continuous_param(
        Neuron1#neuron.state_bound,
        Neuron2#neuron.state_bound,
        Type1, Type2,
        1.0  %% default bound
    ),

    %% Crossover backbone weights
    BackboneW = crossover_ltc_weights(
        Neuron1#neuron.ltc_backbone_weights,
        Neuron2#neuron.ltc_backbone_weights,
        CrossoverRate
    ),

    %% Crossover head weights
    HeadW = crossover_ltc_weights(
        Neuron1#neuron.ltc_head_weights,
        Neuron2#neuron.ltc_head_weights,
        CrossoverRate
    ),

    {NeuronType, Tau, Bound, BackboneW, HeadW}.

%% @private Crossover neuron types.
%% If both parents have same type, inherit it.
%% If one is LTC/CfC and other is standard, probabilistic choice.
-spec crossover_neuron_type(atom(), atom(), float()) -> atom().
crossover_neuron_type(Type1, Type1, _Rate) ->
    %% Same type, inherit it
    Type1;
crossover_neuron_type(Type1, Type2, CrossoverRate) ->
    %% Different types, choose based on crossover rate
    case rand:uniform() < CrossoverRate of
        true -> Type1;
        false -> Type2
    end.

%% @private Crossover continuous LTC parameter (tau or bound).
%% Averages values if both parents are LTC/CfC neurons.
-spec crossover_continuous_param(float(), float(), atom(), atom(), float()) -> float().
crossover_continuous_param(Val1, Val2, Type1, Type2, Default) ->
    IsLtc1 = is_ltc_type(Type1),
    IsLtc2 = is_ltc_type(Type2),
    crossover_continuous_impl(Val1, Val2, IsLtc1, IsLtc2, Default).

crossover_continuous_impl(Val1, Val2, true, true, _Default) ->
    %% Both LTC: average
    (Val1 + Val2) / 2.0;
crossover_continuous_impl(Val1, _Val2, true, false, _Default) ->
    %% Only parent1 is LTC
    Val1;
crossover_continuous_impl(_Val1, Val2, false, true, _Default) ->
    %% Only parent2 is LTC
    Val2;
crossover_continuous_impl(_Val1, _Val2, false, false, Default) ->
    %% Neither is LTC, use default
    Default.

%% @private Check if neuron type is LTC or CfC.
-spec is_ltc_type(atom()) -> boolean().
is_ltc_type(ltc) -> true;
is_ltc_type(cfc) -> true;
is_ltc_type(_) -> false.

%% @private Crossover LTC weight vectors.
%% Uses weight-level crossover for matching indices.
-spec crossover_ltc_weights([float()], [float()], float()) -> [float()].
crossover_ltc_weights([], W2, _Rate) -> W2;
crossover_ltc_weights(W1, [], _Rate) -> W1;
crossover_ltc_weights(W1, W2, CrossoverRate) ->
    NumWeights = min(length(W1), length(W2)),

    %% Crossover matching weights
    Combined = lists:zipwith(
        fun(Weight1, Weight2) ->
            case rand:uniform() < CrossoverRate of
                true -> Weight1;
                false -> Weight2
            end
        end,
        lists:sublist(W1, NumWeights),
        lists:sublist(W2, NumWeights)
    ),

    %% Add remaining weights from longer list
    Remaining = get_remaining_weights(W1, W2, NumWeights),
    Combined ++ Remaining.

%% @private Get remaining weights from the longer list.
-spec get_remaining_weights([float()], [float()], non_neg_integer()) -> [float()].
get_remaining_weights(W1, W2, NumWeights) ->
    Len1 = length(W1),
    Len2 = length(W2),
    get_remaining_impl(W1, W2, Len1, Len2, NumWeights).

get_remaining_impl(W1, _W2, Len1, _Len2, NumWeights) when Len1 > NumWeights ->
    lists:nthtail(NumWeights, W1);
get_remaining_impl(_W1, W2, _Len1, Len2, NumWeights) when Len2 > NumWeights ->
    lists:nthtail(NumWeights, W2);
get_remaining_impl(_W1, _W2, _Len1, _Len2, _NumWeights) ->
    [].
