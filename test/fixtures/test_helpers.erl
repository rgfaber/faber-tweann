%% @module test_helpers
%% @doc Test helper functions for Faber TWEANN
%%
%% Provides utilities for setting up test environments, creating test
%% neurons, sensors, actuators, and complete test networks.
%%
%% == Usage ==
%% ```
%% test_helpers:setup_test_env(),
%% Neuron = test_helpers:create_test_neuron(default),
%% test_helpers:teardown_test_env().
%% ```
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0

-module(test_helpers).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

-export([
    setup_test_env/0,
    teardown_test_env/0,
    create_test_neuron/1,
    create_test_sensor/1,
    create_test_actuator/1,
    create_test_cortex/1,
    create_test_network/1,
    generate_unique_id/0
]).

%%==============================================================================
%% Environment Setup
%%==============================================================================

%% @doc Set up test environment with ETS tables
%%
%% Creates in-memory ETS tables for testing. Should be called
%% in test setup functions.
%%
%% @returns ok on success
-spec setup_test_env() -> ok.
setup_test_env() ->
    %% Initialize genotype ETS tables
    genotype:init_db(),

    %% Initialize innovation tracking
    innovation:init(),
    ok.

%% @doc Tear down test environment
%%
%% Clears all ETS tables. Should be called in test teardown functions.
%%
%% @returns ok on success
-spec teardown_test_env() -> ok.
teardown_test_env() ->
    %% Clear genotype tables
    genotype:reset_db(),

    %% Clear innovation tables
    innovation:reset(),
    ok.

%%==============================================================================
%% Entity Creation
%%==============================================================================

%% @doc Create a test neuron with specified configuration
%%
%% @param Config Configuration atom or proplist
%%   - default: Basic neuron with tanh activation
%%   - with_plasticity: Neuron with hebbian plasticity
%%   - with_inputs: Neuron with sample weighted inputs
%%
%% @returns Neuron record
-spec create_test_neuron(atom() | list()) -> #neuron{}.
create_test_neuron(default) ->
    NeuronId = {{0.5, generate_unique_id()}, neuron},
    CortexId = {{0.0, generate_unique_id()}, cortex},
    #neuron{
        id = NeuronId,
        generation = 1,
        cx_id = CortexId,
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product,
        input_idps = [],
        output_ids = [],
        ro_ids = []
    };

create_test_neuron(with_plasticity) ->
    Neuron = create_test_neuron(default),
    Neuron#neuron{
        pf = {hebbian, [0.1]}
    };

create_test_neuron(with_inputs) ->
    Neuron = create_test_neuron(default),
    SensorId = {{-1.0, generate_unique_id()}, sensor},
    %% Weight format: {Weight, DeltaWeight, LearningRate, ParamList}
    WeightedInputs = [{SensorId, [{0.5, 0.0, 0.1, [0.1]}]}],
    Neuron#neuron{
        input_idps = WeightedInputs
    };

create_test_neuron(Config) when is_list(Config) ->
    Base = create_test_neuron(default),
    apply_neuron_config(Base, Config).

%% @private
apply_neuron_config(Neuron, []) ->
    Neuron;
apply_neuron_config(Neuron, [{af, AF} | Rest]) ->
    apply_neuron_config(Neuron#neuron{af = AF}, Rest);
apply_neuron_config(Neuron, [{pf, PF} | Rest]) ->
    apply_neuron_config(Neuron#neuron{pf = PF}, Rest);
apply_neuron_config(Neuron, [{aggr_f, AggrF} | Rest]) ->
    apply_neuron_config(Neuron#neuron{aggr_f = AggrF}, Rest);
apply_neuron_config(Neuron, [_ | Rest]) ->
    apply_neuron_config(Neuron, Rest).

%% @doc Create a test sensor with specified configuration
%%
%% @param Config Configuration atom
%%   - default: Basic sensor with vector length 2
%%
%% @returns Sensor record
-spec create_test_sensor(atom()) -> #sensor{}.
create_test_sensor(default) ->
    SensorId = {{-1.0, generate_unique_id()}, sensor},
    CortexId = {{0.0, generate_unique_id()}, cortex},
    #sensor{
        id = SensorId,
        name = test_sensor,
        type = standard,
        cx_id = CortexId,
        scape = {private, test_scape},
        vl = 2,
        fanout_ids = [],
        generation = 1,
        format = {no_geo, []},
        parameters = []
    }.

%% @doc Create a test actuator with specified configuration
%%
%% @param Config Configuration atom
%%   - default: Basic actuator with vector length 1
%%
%% @returns Actuator record
-spec create_test_actuator(atom()) -> #actuator{}.
create_test_actuator(default) ->
    ActuatorId = {{1.0, generate_unique_id()}, actuator},
    CortexId = {{0.0, generate_unique_id()}, cortex},
    #actuator{
        id = ActuatorId,
        name = test_actuator,
        type = standard,
        cx_id = CortexId,
        scape = {private, test_scape},
        vl = 1,
        fanin_ids = [],
        generation = 1,
        format = {no_geo, []},
        parameters = []
    }.

%% @doc Create a test cortex with specified configuration
%%
%% @param Config Configuration atom
%%   - default: Empty cortex
%%
%% @returns Cortex record
-spec create_test_cortex(atom()) -> #cortex{}.
create_test_cortex(default) ->
    CortexId = {{0.0, generate_unique_id()}, cortex},
    AgentId = {generate_unique_id(), agent},
    #cortex{
        id = CortexId,
        agent_id = AgentId,
        neuron_ids = [],
        sensor_ids = [],
        actuator_ids = []
    }.

%% @doc Create a complete test network
%%
%% @param Config Configuration atom
%%   - simple: 1 sensor -> 1 neuron -> 1 actuator
%%   - xor: 2 sensors -> 2 hidden -> 1 actuator (XOR topology)
%%
%% @returns Map with cortex, sensors, neurons, actuators
-spec create_test_network(atom()) -> map().
create_test_network(simple) ->
    CortexId = {{0.0, generate_unique_id()}, cortex},
    AgentId = {generate_unique_id(), agent},

    %% Create sensor
    SensorId = {{-1.0, generate_unique_id()}, sensor},

    %% Create neuron
    NeuronId = {{0.5, generate_unique_id()}, neuron},

    %% Create actuator
    ActuatorId = {{1.0, generate_unique_id()}, actuator},

    %% Build components with connections
    Sensor = #sensor{
        id = SensorId,
        name = rng,
        type = standard,
        cx_id = CortexId,
        scape = {private, test_scape},
        vl = 1,
        fanout_ids = [NeuronId],
        generation = 1,
        format = {no_geo, []}
    },

    Neuron = #neuron{
        id = NeuronId,
        generation = 1,
        cx_id = CortexId,
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product,
        input_idps = [{SensorId, [{0.5, 0.0, 0.1, []}]}],
        output_ids = [ActuatorId],
        ro_ids = []
    },

    Actuator = #actuator{
        id = ActuatorId,
        name = pts,
        type = standard,
        cx_id = CortexId,
        scape = {private, test_scape},
        vl = 1,
        fanin_ids = [NeuronId],
        generation = 1,
        format = {no_geo, []}
    },

    Cortex = #cortex{
        id = CortexId,
        agent_id = AgentId,
        neuron_ids = [NeuronId],
        sensor_ids = [SensorId],
        actuator_ids = [ActuatorId]
    },

    #{
        cortex => Cortex,
        sensors => [Sensor],
        neurons => [Neuron],
        actuators => [Actuator]
    };

create_test_network(xor_gate) ->
    CortexId = {{0.0, generate_unique_id()}, cortex},
    AgentId = {generate_unique_id(), agent},

    %% Create 2 sensors for XOR inputs
    Sensor1Id = {{-1.0, generate_unique_id()}, sensor},
    Sensor2Id = {{-1.0, generate_unique_id()}, sensor},

    %% Create 2 hidden neurons
    Hidden1Id = {{0.5, generate_unique_id()}, neuron},
    Hidden2Id = {{0.5, generate_unique_id()}, neuron},

    %% Create output actuator
    ActuatorId = {{1.0, generate_unique_id()}, actuator},

    %% Build sensors
    Sensor1 = #sensor{
        id = Sensor1Id,
        name = xor_input,
        cx_id = CortexId,
        vl = 1,
        fanout_ids = [Hidden1Id, Hidden2Id],
        generation = 1
    },

    Sensor2 = #sensor{
        id = Sensor2Id,
        name = xor_input,
        cx_id = CortexId,
        vl = 1,
        fanout_ids = [Hidden1Id, Hidden2Id],
        generation = 1
    },

    %% Build hidden neurons
    Hidden1 = #neuron{
        id = Hidden1Id,
        generation = 1,
        cx_id = CortexId,
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product,
        input_idps = [
            {Sensor1Id, [{0.5, 0.0, 0.1, []}]},
            {Sensor2Id, [{0.5, 0.0, 0.1, []}]}
        ],
        output_ids = [ActuatorId],
        ro_ids = []
    },

    Hidden2 = #neuron{
        id = Hidden2Id,
        generation = 1,
        cx_id = CortexId,
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product,
        input_idps = [
            {Sensor1Id, [{0.5, 0.0, 0.1, []}]},
            {Sensor2Id, [{0.5, 0.0, 0.1, []}]}
        ],
        output_ids = [ActuatorId],
        ro_ids = []
    },

    %% Build actuator
    Actuator = #actuator{
        id = ActuatorId,
        name = xor_output,
        cx_id = CortexId,
        vl = 2,
        fanin_ids = [Hidden1Id, Hidden2Id],
        generation = 1
    },

    %% Build cortex
    Cortex = #cortex{
        id = CortexId,
        agent_id = AgentId,
        neuron_ids = [Hidden1Id, Hidden2Id],
        sensor_ids = [Sensor1Id, Sensor2Id],
        actuator_ids = [ActuatorId]
    },

    #{
        cortex => Cortex,
        sensors => [Sensor1, Sensor2],
        neurons => [Hidden1, Hidden2],
        actuators => [Actuator]
    }.

%%==============================================================================
%% Utility Functions
%%==============================================================================

%% @doc Generate a unique identifier component
%%
%% Uses erlang:unique_integer for better uniqueness than rand.
%%
%% @returns Unique float value
-spec generate_unique_id() -> float().
generate_unique_id() ->
    erlang:unique_integer([positive]) / 1000000.0.
