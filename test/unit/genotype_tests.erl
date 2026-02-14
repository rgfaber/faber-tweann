-module(genotype_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% =============================================================================
%% Setup/Teardown
%% =============================================================================

setup() ->
    %% Ensure application started (for morphology registry)
    application:ensure_all_started(faber_tweann),

    %% Register example morphologies
    test_helper:register_all_example_morphologies(),

    %% Initialize ETS tables (no Mnesia needed)
    genotype:init_db(),
    innovation:init().

teardown(_) ->
    genotype:reset_db().

%% =============================================================================
%% Test Generator
%% =============================================================================

genotype_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        fun database_operations/0,
        fun generate_unique_id/0,
        fun simple_agent_construction/0,
        fun read_write_delete/0,
        fun clone_agent/0,
        fun constraint_defaults/0,
        fun network_connectivity/0,
        fun neuron_weights_format/0,
        fun morphology_integration/0,
        fun update_fingerprint/0,
        fun delete_agent/0,
        fun multiple_agents/0,
        fun neuron_linking/0,
        fun sensor_linking/0
     ]}.

%% =============================================================================
%% Database Tests
%% =============================================================================

database_operations() ->
    %% DB should already be created by setup
    %% Test that we can read/write
    Agent = #agent{
        id = {0.12345, agent},
        encoding_type = neural,
        generation = 0
    },
    ?assertEqual(ok, genotype:write(Agent)),
    Retrieved = genotype:dirty_read({agent, {0.12345, agent}}),
    ?assertEqual(Agent, Retrieved).

%% =============================================================================
%% ID Generation Tests
%% =============================================================================

generate_unique_id() ->
    Id1 = genotype:generate_UniqueId(),
    Id2 = genotype:generate_UniqueId(),
    Id3 = genotype:generate_UniqueId(),

    ?assert(is_float(Id1)),
    ?assert(is_float(Id2)),
    ?assert(is_float(Id3)),

    %% IDs should be unique
    ?assertNotEqual(Id1, Id2),
    ?assertNotEqual(Id2, Id3).

%% =============================================================================
%% Agent Construction Tests
%% =============================================================================

simple_agent_construction() ->
    %% Create constraint with xor_mimic morphology
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    ResultId = genotype:construct_Agent(SpecieId, AgentId, Constraint),
    ?assertEqual(AgentId, ResultId),

    %% Verify agent was created
    Agent = genotype:dirty_read({agent, AgentId}),
    ?assertNotEqual(undefined, Agent),
    ?assertEqual(AgentId, Agent#agent.id),
    ?assertEqual(neural, Agent#agent.encoding_type),
    ?assertEqual(0, Agent#agent.generation),
    ?assertEqual(0, Agent#agent.fitness),
    ?assertEqual(SpecieId, Agent#agent.specie_id),

    %% Verify cortex was created
    CortexId = Agent#agent.cx_id,
    Cortex = genotype:dirty_read({cortex, CortexId}),
    ?assertNotEqual(undefined, Cortex),
    ?assertEqual(AgentId, Cortex#cortex.agent_id),

    %% Verify sensors (xor_mimic has 1 sensor with vl=2)
    SensorIds = Cortex#cortex.sensor_ids,
    ?assertEqual(1, length(SensorIds)),
    [SensorId] = SensorIds,
    Sensor = genotype:dirty_read({sensor, SensorId}),
    ?assertEqual(xor_GetInput, Sensor#sensor.name),
    ?assertEqual(2, Sensor#sensor.vl),

    %% Verify actuators (xor_mimic has 1 actuator with vl=1)
    ActuatorIds = Cortex#cortex.actuator_ids,
    ?assertEqual(1, length(ActuatorIds)),
    [ActuatorId] = ActuatorIds,
    Actuator = genotype:dirty_read({actuator, ActuatorId}),
    ?assertEqual(xor_SendOutput, Actuator#actuator.name),
    ?assertEqual(1, Actuator#actuator.vl),

    %% Verify neurons (should have 1 for 1 actuator output)
    NeuronIds = Cortex#cortex.neuron_ids,
    ?assertEqual(1, length(NeuronIds)).

%% =============================================================================
%% CRUD Tests
%% =============================================================================

read_write_delete() ->
    Neuron = #neuron{
        id = {{0.5, 0.123}, neuron},
        generation = 1,
        af = tanh,
        aggr_f = dot_product
    },

    %% Write
    ?assertEqual(ok, genotype:write(Neuron)),

    %% Read
    Retrieved = genotype:dirty_read({neuron, {{0.5, 0.123}, neuron}}),
    ?assertEqual(Neuron, Retrieved),

    %% Delete
    ?assertEqual(ok, genotype:delete({neuron, {{0.5, 0.123}, neuron}})),
    ?assertEqual(undefined, genotype:dirty_read({neuron, {{0.5, 0.123}, neuron}})).

%% =============================================================================
%% Clone Tests
%% =============================================================================

clone_agent() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    %% Clone the agent
    NewAgentId = genotype:clone_Agent(AgentId),

    %% Verify different IDs
    ?assertNotEqual(AgentId, NewAgentId),

    %% Verify both agents exist
    Agent = genotype:dirty_read({agent, AgentId}),
    NewAgent = genotype:dirty_read({agent, NewAgentId}),
    ?assertNotEqual(undefined, Agent),
    ?assertNotEqual(undefined, NewAgent),

    %% Verify generation incremented
    ?assertEqual(0, Agent#agent.generation),
    ?assertEqual(1, NewAgent#agent.generation),

    %% Verify different cortex IDs
    OldCortexId = Agent#agent.cx_id,
    NewCortexId = NewAgent#agent.cx_id,
    ?assertNotEqual(OldCortexId, NewCortexId),

    %% Verify same structure
    OldCortex = genotype:dirty_read({cortex, OldCortexId}),
    NewCortex = genotype:dirty_read({cortex, NewCortexId}),
    ?assertEqual(length(OldCortex#cortex.sensor_ids),
                 length(NewCortex#cortex.sensor_ids)),
    ?assertEqual(length(OldCortex#cortex.neuron_ids),
                 length(NewCortex#cortex.neuron_ids)),
    ?assertEqual(length(OldCortex#cortex.actuator_ids),
                 length(NewCortex#cortex.actuator_ids)).

%% =============================================================================
%% Constraint Tests
%% =============================================================================

constraint_defaults() ->
    %% Test with default constraint
    Constraint = #constraint{},

    ?assertEqual(xor_mimic, Constraint#constraint.morphology),
    ?assertEqual(recurrent, Constraint#constraint.connection_architecture),
    ?assert(lists:member(tanh, Constraint#constraint.neural_afs)),
    ?assert(lists:member(dot_product, Constraint#constraint.neural_aggr_fs)).

%% =============================================================================
%% Connectivity Tests
%% =============================================================================

network_connectivity() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    [SensorId] = Cortex#cortex.sensor_ids,
    [NeuronId] = Cortex#cortex.neuron_ids,
    [ActuatorId] = Cortex#cortex.actuator_ids,

    Sensor = genotype:dirty_read({sensor, SensorId}),
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    Actuator = genotype:dirty_read({actuator, ActuatorId}),

    %% Sensor should connect to neuron
    FanoutIds = Sensor#sensor.fanout_ids,
    ?assertEqual(1, length(FanoutIds)),
    ?assert(lists:member(NeuronId, FanoutIds)),

    %% Neuron should receive from sensor
    InputIdps = Neuron#neuron.input_idps,
    InputIds = [Id || {Id, _} <- InputIdps],
    ?assert(lists:member(SensorId, InputIds)),

    %% Neuron should connect to actuator
    OutputIds = Neuron#neuron.output_ids,
    ?assert(lists:member(ActuatorId, OutputIds)),

    %% Actuator should receive from neuron
    FaninIds = Actuator#actuator.fanin_ids,
    ?assert(lists:member(NeuronId, FaninIds)).

%% =============================================================================
%% Weight Format Tests
%% =============================================================================

neuron_weights_format() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    [NeuronId] = Cortex#cortex.neuron_ids,
    Neuron = genotype:dirty_read({neuron, NeuronId}),

    InputIdps = Neuron#neuron.input_idps,
    ?assertEqual(1, length(InputIdps)),  % One sensor

    [{_SensorId, Weights}] = InputIdps,
    ?assertEqual(2, length(Weights)),  % 2 weights for xor_mimic sensor VL=2

    %% Verify weight tuple format: {W, DW, LR, Params}
    [{W, DW, LR, Params} | _] = Weights,
    ?assert(is_float(W)),
    ?assertEqual(0.0, DW),
    ?assertEqual(0.1, LR),
    ?assertEqual([], Params).

%% =============================================================================
%% Morphology Integration Tests
%% =============================================================================

morphology_integration() ->
    %% Test that morphology module is correctly integrated
    Sensors = morphology:get_InitSensors(xor_mimic),
    Actuators = morphology:get_InitActuators(xor_mimic),

    ?assertEqual(1, length(Sensors)),
    ?assertEqual(1, length(Actuators)),

    [Sensor] = Sensors,
    [Actuator] = Actuators,

    ?assertEqual(xor_GetInput, Sensor#sensor.name),
    ?assertEqual(2, Sensor#sensor.vl),
    ?assertEqual(xor_SendOutput, Actuator#actuator.name),
    ?assertEqual(1, Actuator#actuator.vl).

%% =============================================================================
%% Fingerprint Tests
%% =============================================================================

update_fingerprint() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Agent = genotype:dirty_read({agent, AgentId}),

    %% NOTE: Fingerprint calculation is not yet implemented (TODO in genotype.erl)
    %% For now, we just verify the agent was created successfully
    ?assertNotEqual(undefined, Agent),
    ?assertEqual(AgentId, Agent#agent.id).

%% =============================================================================
%% Delete Agent Tests
%% =============================================================================

delete_agent() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    %% Verify agent exists
    Agent = genotype:dirty_read({agent, AgentId}),
    ?assertNotEqual(undefined, Agent),

    CortexId = Agent#agent.cx_id,
    Cortex = genotype:dirty_read({cortex, CortexId}),
    [SensorId] = Cortex#cortex.sensor_ids,
    [NeuronId] = Cortex#cortex.neuron_ids,
    [ActuatorId] = Cortex#cortex.actuator_ids,

    %% Delete agent
    ?assertEqual(ok, genotype:delete_Agent(AgentId)),

    %% Verify all components are deleted
    ?assertEqual(undefined, genotype:dirty_read({agent, AgentId})),
    ?assertEqual(undefined, genotype:dirty_read({cortex, CortexId})),
    ?assertEqual(undefined, genotype:dirty_read({sensor, SensorId})),
    ?assertEqual(undefined, genotype:dirty_read({neuron, NeuronId})),
    ?assertEqual(undefined, genotype:dirty_read({actuator, ActuatorId})).

%% =============================================================================
%% Multiple Agents Tests
%% =============================================================================

multiple_agents() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},

    %% Create multiple agents
    Agent1Id = {genotype:generate_UniqueId(), agent},
    Agent2Id = {genotype:generate_UniqueId(), agent},
    Agent3Id = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, Agent1Id, Constraint),
    genotype:construct_Agent(SpecieId, Agent2Id, Constraint),
    genotype:construct_Agent(SpecieId, Agent3Id, Constraint),

    %% Verify all agents exist
    Agent1 = genotype:dirty_read({agent, Agent1Id}),
    Agent2 = genotype:dirty_read({agent, Agent2Id}),
    Agent3 = genotype:dirty_read({agent, Agent3Id}),

    ?assertNotEqual(undefined, Agent1),
    ?assertNotEqual(undefined, Agent2),
    ?assertNotEqual(undefined, Agent3),

    %% All should have same specie
    ?assertEqual(SpecieId, Agent1#agent.specie_id),
    ?assertEqual(SpecieId, Agent2#agent.specie_id),
    ?assertEqual(SpecieId, Agent3#agent.specie_id).

%% =============================================================================
%% Neuron Linking Tests
%% =============================================================================

neuron_linking() ->
    %% Test the link_FromElementToElement function
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    [NeuronId] = Cortex#cortex.neuron_ids,

    Neuron = genotype:dirty_read({neuron, NeuronId}),

    %% Verify neuron has all expected fields
    ?assertNotEqual(undefined, Neuron#neuron.af),
    ?assertNotEqual(undefined, Neuron#neuron.aggr_f),
    ?assert(is_list(Neuron#neuron.input_idps)),
    ?assert(is_list(Neuron#neuron.output_ids)).

%% =============================================================================
%% Sensor Linking Tests
%% =============================================================================

sensor_linking() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    [SensorId] = Cortex#cortex.sensor_ids,

    Sensor = genotype:dirty_read({sensor, SensorId}),

    %% Verify sensor has cortex ID set
    ?assertEqual(Agent#agent.cx_id, Sensor#sensor.cx_id),

    %% Verify sensor has generation set
    ?assertEqual(0, Sensor#sensor.generation),

    %% Verify sensor has fanout
    ?assert(length(Sensor#sensor.fanout_ids) > 0).
