%% @module sample_genotypes
%% @doc Sample genotypes for testing Faber TWEANN
%%
%% Provides pre-built genotype configurations for testing purposes.
%% These include simple networks, XOR-solving topologies, and
%% networks with plasticity enabled.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0

-module(sample_genotypes).

-include("records.hrl").

-export([
    xor_network/0,
    feedforward_3layer/0,
    plasticity_network/0,
    minimal_network/0
]).

%%==============================================================================
%% Sample Networks
%%==============================================================================

%% @doc Create a simple XOR solving network genotype
%%
%% Topology: 2 inputs -> 2 hidden -> 1 output
%% This is a classic benchmark for neuroevolution.
%%
%% @returns Map containing all network components
-spec xor_network() -> map().
xor_network() ->
    CortexId = {{0.0, 0.001}, cortex},
    AgentId = {0.001, agent},

    %% Sensors for XOR inputs
    Sensor1Id = {{-1.0, 0.101}, sensor},
    Sensor2Id = {{-1.0, 0.102}, sensor},

    %% Hidden layer neurons
    Hidden1Id = {{0.5, 0.201}, neuron},
    Hidden2Id = {{0.5, 0.202}, neuron},

    %% Output actuator
    ActuatorId = {{1.0, 0.301}, actuator},

    %% Build sensors
    Sensor1 = #sensor{
        id = Sensor1Id,
        name = xor_input_1,
        type = standard,
        cx_id = CortexId,
        scape = {private, xor_mimic},
        vl = 1,
        fanout_ids = [Hidden1Id, Hidden2Id],
        generation = 0,
        format = {no_geo, []},
        parameters = []
    },

    Sensor2 = #sensor{
        id = Sensor2Id,
        name = xor_input_2,
        type = standard,
        cx_id = CortexId,
        scape = {private, xor_mimic},
        vl = 1,
        fanout_ids = [Hidden1Id, Hidden2Id],
        generation = 0,
        format = {no_geo, []},
        parameters = []
    },

    %% Build hidden neurons with initial weights
    Hidden1 = #neuron{
        id = Hidden1Id,
        generation = 0,
        cx_id = CortexId,
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product,
        input_idps = [
            {Sensor1Id, [{1.0, 0.0, 0.1, []}]},
            {Sensor2Id, [{1.0, 0.0, 0.1, []}]}
        ],
        output_ids = [ActuatorId],
        ro_ids = []
    },

    Hidden2 = #neuron{
        id = Hidden2Id,
        generation = 0,
        cx_id = CortexId,
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product,
        input_idps = [
            {Sensor1Id, [{-1.0, 0.0, 0.1, []}]},
            {Sensor2Id, [{-1.0, 0.0, 0.1, []}]}
        ],
        output_ids = [ActuatorId],
        ro_ids = []
    },

    %% Build actuator
    Actuator = #actuator{
        id = ActuatorId,
        name = xor_output,
        type = standard,
        cx_id = CortexId,
        scape = {private, xor_mimic},
        vl = 2,
        fanin_ids = [Hidden1Id, Hidden2Id],
        generation = 0,
        format = {no_geo, []},
        parameters = []
    },

    %% Build cortex
    Cortex = #cortex{
        id = CortexId,
        agent_id = AgentId,
        neuron_ids = [Hidden1Id, Hidden2Id],
        sensor_ids = [Sensor1Id, Sensor2Id],
        actuator_ids = [ActuatorId]
    },

    %% Build agent
    Agent = #agent{
        id = AgentId,
        encoding_type = neural,
        generation = 0,
        cx_id = CortexId,
        pattern = [{0.5, [Hidden1Id, Hidden2Id]}],
        mutation_operators = [
            {mutate_weights, 1},
            {add_bias, 1},
            {add_neuron, 1}
        ]
    },

    #{
        agent => Agent,
        cortex => Cortex,
        sensors => [Sensor1, Sensor2],
        neurons => [Hidden1, Hidden2],
        actuators => [Actuator]
    }.

%% @doc Create a basic 3-layer feedforward network
%%
%% Topology: 3 inputs -> 4 hidden -> 2 outputs
%% Useful for testing forward propagation without recurrence.
%%
%% @returns Map containing all network components
-spec feedforward_3layer() -> map().
feedforward_3layer() ->
    CortexId = {{0.0, 0.002}, cortex},
    AgentId = {0.002, agent},

    %% Create 3 sensors
    SensorIds = [{{-1.0, 0.111 + I/1000}, sensor} || I <- lists:seq(1, 3)],

    %% Create 4 hidden neurons
    HiddenIds = [{{0.5, 0.211 + I/1000}, neuron} || I <- lists:seq(1, 4)],

    %% Create 2 actuators
    ActuatorIds = [{{1.0, 0.311 + I/1000}, actuator} || I <- lists:seq(1, 2)],

    %% Build sensors
    Sensors = [
        #sensor{
            id = SId,
            name = input,
            type = standard,
            cx_id = CortexId,
            vl = 1,
            fanout_ids = HiddenIds,
            generation = 0
        }
        || SId <- SensorIds
    ],

    %% Build hidden neurons
    Neurons = [
        #neuron{
            id = NId,
            generation = 0,
            cx_id = CortexId,
            af = tanh,
            pf = {none, []},
            aggr_f = dot_product,
            input_idps = [{SId, [{0.5, 0.0, 0.1, []}]} || SId <- SensorIds],
            output_ids = ActuatorIds,
            ro_ids = []
        }
        || NId <- HiddenIds
    ],

    %% Build actuators
    Actuators = [
        #actuator{
            id = AId,
            name = output,
            type = standard,
            cx_id = CortexId,
            vl = 4,
            fanin_ids = HiddenIds,
            generation = 0
        }
        || AId <- ActuatorIds
    ],

    %% Build cortex
    Cortex = #cortex{
        id = CortexId,
        agent_id = AgentId,
        neuron_ids = HiddenIds,
        sensor_ids = SensorIds,
        actuator_ids = ActuatorIds
    },

    #{
        cortex => Cortex,
        sensors => Sensors,
        neurons => Neurons,
        actuators => Actuators
    }.

%% @doc Create a network with plasticity enabled
%%
%% Same topology as XOR but with Hebbian plasticity on hidden neurons.
%% Useful for testing weight modification during operation.
%%
%% @returns Map containing all network components
-spec plasticity_network() -> map().
plasticity_network() ->
    %% Start with XOR network
    Base = xor_network(),
    #{neurons := Neurons} = Base,

    %% Enable Hebbian plasticity on all neurons
    PlasticNeurons = [
        N#neuron{
            pf = {hebbian, [0.1]},  %% Learning rate 0.1
            input_idps = update_weight_params(N#neuron.input_idps)
        }
        || N <- Neurons
    ],

    Base#{neurons := PlasticNeurons}.

%% @private
%% Update weight parameters for plasticity
update_weight_params(WeightedInputs) ->
    [{SourceId, update_weights(Weights)} || {SourceId, Weights} <- WeightedInputs].

update_weights(Weights) ->
    %% Add plasticity parameters to each weight
    [{W, DW, 0.1, [0.1, 0.1]} || {W, DW, _LR, _Params} <- Weights].

%% @doc Create a minimal network for basic testing
%%
%% Simplest possible network: 1 sensor -> 1 neuron -> 1 actuator
%%
%% @returns Map containing all network components
-spec minimal_network() -> map().
minimal_network() ->
    CortexId = {{0.0, 0.003}, cortex},
    SensorId = {{-1.0, 0.103}, sensor},
    NeuronId = {{0.5, 0.203}, neuron},
    ActuatorId = {{1.0, 0.303}, actuator},

    Sensor = #sensor{
        id = SensorId,
        name = single_input,
        cx_id = CortexId,
        vl = 1,
        fanout_ids = [NeuronId],
        generation = 0
    },

    Neuron = #neuron{
        id = NeuronId,
        generation = 0,
        cx_id = CortexId,
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product,
        input_idps = [{SensorId, [{1.0, 0.0, 0.1, []}]}],
        output_ids = [ActuatorId],
        ro_ids = []
    },

    Actuator = #actuator{
        id = ActuatorId,
        name = single_output,
        cx_id = CortexId,
        vl = 1,
        fanin_ids = [NeuronId],
        generation = 0
    },

    Cortex = #cortex{
        id = CortexId,
        neuron_ids = [NeuronId],
        sensor_ids = [SensorId],
        actuator_ids = [ActuatorId]
    },

    #{
        cortex => Cortex,
        sensors => [Sensor],
        neurons => [Neuron],
        actuators => [Actuator]
    }.
