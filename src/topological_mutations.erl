%% @doc Topological mutation operators for neural network evolution.
%%
%% This module provides mutations that modify network structure:
%% - add_neuron: Insert neuron into existing connection
%% - add_outlink: Add output connection from neuron
%% - add_inlink: Add input connection to neuron
%% - add_sensorlink: Connect sensor to neuron
%% - add_actuatorlink: Connect neuron to actuator
%% - outsplice: Split output connection with new neuron
%% - add_bias: Add bias connection to neuron
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(topological_mutations).

-include("records.hrl").

-dialyzer({nowarn_function, [
    add_bias/1,
    add_outlink/1,
    add_inlink/1,
    add_neuron/1,
    outsplice/1,
    add_sensorlink/1,
    add_actuatorlink/1,
    add_sensor/1,
    add_actuator/1
]}).

-export([
    add_bias/1,
    add_outlink/1,
    add_inlink/1,
    add_neuron/1,
    outsplice/1,
    add_sensorlink/1,
    add_actuatorlink/1,
    add_sensor/1,
    add_actuator/1
]).

%%==============================================================================
%% Topological Mutations
%%==============================================================================

%% @doc Add bias input to a random neuron.
%%
%% Adds a bias connection (self-connection) to a neuron that
%% doesn't already have one.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_bias(term()) -> ok | {error, term()}.
add_bias(AgentId) ->
    case mutation_helpers:select_random_neuron(AgentId) of
        {error, no_neurons} ->
            {error, no_neurons};
        NeuronId ->
            add_bias_to_neuron(NeuronId)
    end.

add_bias_to_neuron(NeuronId) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    HasBias = lists:any(fun({InputId, _}) -> InputId == bias end, Neuron#neuron.input_idps),
    do_add_bias(HasBias, Neuron).

do_add_bias(true, _Neuron) ->
    {error, already_has_bias};
do_add_bias(false, Neuron) ->
    BiasWeight = mutation_helpers:create_random_weight(),
    NewInputIdps = [{bias, [BiasWeight]} | Neuron#neuron.input_idps],
    UpdatedNeuron = Neuron#neuron{input_idps = NewInputIdps},
    genotype:write(UpdatedNeuron),
    ok.

%% @doc Add output link from a random neuron.
%%
%% Connects a neuron to another neuron or actuator that it's
%% not currently connected to.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_outlink(term()) -> ok | {error, term()}.
add_outlink(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    case mutation_helpers:select_random_neuron(AgentId) of
        {error, no_neurons} ->
            {error, no_neurons};
        NeuronId ->
            add_outlink_from_neuron(NeuronId, Cortex)
    end.

add_outlink_from_neuron(NeuronId, Cortex) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    AllTargets = Cortex#cortex.neuron_ids ++ Cortex#cortex.actuator_ids,
    CurrentOutputs = Neuron#neuron.output_ids,
    AvailableTargets = AllTargets -- CurrentOutputs -- [NeuronId],
    connect_to_target(NeuronId, Neuron, AvailableTargets).

connect_to_target(_NeuronId, _Neuron, []) ->
    {error, no_available_targets};
connect_to_target(NeuronId, Neuron, Targets) ->
    TargetId = selection_utils:random_select(Targets),
    mutation_helpers:link_neuron_to_target(NeuronId, Neuron, TargetId),
    ok.

%% @doc Add input link to a random neuron.
%%
%% Connects a sensor or another neuron to a neuron that it's
%% not currently connected to.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_inlink(term()) -> ok | {error, term()}.
add_inlink(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    case mutation_helpers:select_random_neuron(AgentId) of
        {error, no_neurons} ->
            {error, no_neurons};
        NeuronId ->
            add_inlink_to_neuron(NeuronId, Cortex)
    end.

add_inlink_to_neuron(NeuronId, Cortex) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    AllSources = Cortex#cortex.sensor_ids ++ Cortex#cortex.neuron_ids,
    CurrentInputIds = [InputId || {InputId, _} <- Neuron#neuron.input_idps],
    AvailableSources = AllSources -- CurrentInputIds -- [NeuronId],
    connect_from_source(NeuronId, Neuron, AvailableSources).

connect_from_source(_NeuronId, _Neuron, []) ->
    {error, no_available_sources};
connect_from_source(NeuronId, Neuron, Sources) ->
    SourceId = selection_utils:random_select(Sources),
    mutation_helpers:link_source_to_neuron(SourceId, NeuronId, Neuron),
    ok.

%% @doc Add a new neuron by splitting a connection.
%%
%% Selects a random connection, removes it, and inserts a new
%% neuron in the middle.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_neuron(term()) -> ok | {error, term()}.
add_neuron(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    case mutation_helpers:find_splittable_link(AgentId) of
        {error, no_links} ->
            {error, cannot_add_neuron};
        {FromId, ToId, Weight} ->
            insert_neuron(Agent, Cortex, FromId, ToId, Weight)
    end.

insert_neuron(Agent, Cortex, FromId, ToId, Weight) ->
    NewNeuronId = genotype:generate_id(neuron),
    Constraint = Agent#agent.constraint,
    AF = selection_utils:random_select(Constraint#constraint.neural_afs),
    AggrF = selection_utils:random_select(Constraint#constraint.neural_aggr_fs),

    %% Get innovation number for this node split (NEAT tracking)
    %% Same FromId->ToId split always gets the same innovation
    {NodeInnovation, _InLinkInn, _OutLinkInn} =
        innovation:get_or_create_node_innovation(FromId, ToId),

    NewNeuron = #neuron{
        id = NewNeuronId,
        generation = Agent#agent.generation,
        cx_id = Agent#agent.cx_id,
        af = AF,
        aggr_f = AggrF,
        input_idps = [{FromId, [Weight]}],
        output_ids = [ToId],
        ro_ids = [],
        innovation = NodeInnovation
    },

    %% Update connections
    mutation_helpers:update_source_output(FromId, ToId, NewNeuronId),
    mutation_helpers:update_target_input(ToId, FromId, NewNeuronId, Weight),

    %% Write new neuron and update cortex
    genotype:write(NewNeuron),
    NewNeuronIds = [NewNeuronId | Cortex#cortex.neuron_ids],
    UpdatedCortex = Cortex#cortex{neuron_ids = NewNeuronIds},
    genotype:write(UpdatedCortex),
    ok.

%% @doc Add neuron by outsplicing (split output connection).
%%
%% Similar to add_neuron but specifically targets output connections.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec outsplice(term()) -> ok | {error, term()}.
outsplice(AgentId) ->
    add_neuron(AgentId).

%% @doc Add link from a sensor to a neuron.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_sensorlink(term()) -> ok | {error, term()}.
add_sensorlink(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    select_sensor_and_link(Cortex).

select_sensor_and_link(#cortex{sensor_ids = []}) ->
    {error, no_sensors};
select_sensor_and_link(Cortex) ->
    SensorId = selection_utils:random_select(Cortex#cortex.sensor_ids),
    Sensor = genotype:dirty_read({sensor, SensorId}),
    AvailableNeurons = Cortex#cortex.neuron_ids -- Sensor#sensor.fanout_ids,
    link_sensor_to_available_neuron(SensorId, Sensor, AvailableNeurons).

link_sensor_to_available_neuron(_SensorId, _Sensor, []) ->
    {error, no_available_neurons};
link_sensor_to_available_neuron(SensorId, Sensor, Neurons) ->
    NeuronId = selection_utils:random_select(Neurons),
    mutation_helpers:link_sensor_to_neuron(SensorId, Sensor, NeuronId),
    ok.

%% @doc Add link from a neuron to an actuator.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_actuatorlink(term()) -> ok | {error, term()}.
add_actuatorlink(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    select_actuator_and_link(Cortex).

select_actuator_and_link(#cortex{actuator_ids = []}) ->
    {error, no_actuators};
select_actuator_and_link(Cortex) ->
    ActuatorId = selection_utils:random_select(Cortex#cortex.actuator_ids),
    Actuator = genotype:dirty_read({actuator, ActuatorId}),
    AvailableNeurons = Cortex#cortex.neuron_ids -- Actuator#actuator.fanin_ids,
    link_neuron_to_available_actuator(ActuatorId, Actuator, AvailableNeurons).

link_neuron_to_available_actuator(_ActuatorId, _Actuator, []) ->
    {error, no_available_neurons};
link_neuron_to_available_actuator(ActuatorId, Actuator, Neurons) ->
    NeuronId = selection_utils:random_select(Neurons),
    mutation_helpers:link_neuron_to_actuator(NeuronId, ActuatorId, Actuator),
    ok.

%% @doc Add a new sensor to the network.
%%
%% Selects a sensor type from the morphology that isn't already
%% in the network, creates it, and connects it to a random neuron.
%% Enables networks to evolve new perception capabilities.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_sensor(term()) -> ok | {error, term()}.
add_sensor(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    Constraint = Agent#agent.constraint,
    Morphology = Constraint#constraint.morphology,

    %% Get all available sensors from morphology
    AllSensors = morphology:get_Sensors(Morphology),

    %% Get sensor names already in network
    CurrentSensorNames = get_current_sensor_names(Cortex#cortex.sensor_ids),

    %% Find sensors not yet in network
    CandidateSensors = [S || S <- AllSensors,
                             not lists:member(S#sensor.name, CurrentSensorNames)],

    add_sensor_from_candidates(Agent, Cortex, CandidateSensors).

add_sensor_from_candidates(_Agent, _Cortex, []) ->
    {error, no_available_sensors};
add_sensor_from_candidates(Agent, Cortex, Candidates) ->
    %% Select random sensor template
    SensorTemplate = selection_utils:random_select(Candidates),

    %% Get innovation number for adding this sensor type
    %% Same sensor type always gets the same innovation (NEAT tracking)
    SensorInnovation = innovation:get_or_create_link_innovation(
        {add_sensor, SensorTemplate#sensor.name}, Agent#agent.cx_id),

    %% Create sensor with unique ID
    NewSensorId = genotype:generate_id(sensor),
    NewSensor = SensorTemplate#sensor{
        id = NewSensorId,
        cx_id = Agent#agent.cx_id,
        generation = Agent#agent.generation,
        fanout_ids = [],
        innovation = SensorInnovation
    },

    %% Write sensor to Mnesia
    genotype:write(NewSensor),

    %% Update cortex with new sensor
    NewSensorIds = [NewSensorId | Cortex#cortex.sensor_ids],
    UpdatedCortex = Cortex#cortex{sensor_ids = NewSensorIds},
    genotype:write(UpdatedCortex),

    %% Connect to a random neuron
    connect_sensor_to_random_neuron(NewSensorId, NewSensor, Cortex#cortex.neuron_ids).

get_current_sensor_names(SensorIds) ->
    [begin
        Sensor = genotype:dirty_read({sensor, SId}),
        Sensor#sensor.name
     end || SId <- SensorIds].

connect_sensor_to_random_neuron(_SensorId, _Sensor, []) ->
    %% No neurons - sensor added but unconnected (will connect when neurons added)
    ok;
connect_sensor_to_random_neuron(SensorId, Sensor, NeuronIds) ->
    NeuronId = selection_utils:random_select(NeuronIds),
    mutation_helpers:link_sensor_to_neuron(SensorId, Sensor, NeuronId),
    ok.

%% @doc Add a new actuator to the network.
%%
%% Selects an actuator type from the morphology that isn't already
%% in the network, creates it, and connects a random neuron to it.
%% Enables networks to evolve new action capabilities.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_actuator(term()) -> ok | {error, term()}.
add_actuator(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    Constraint = Agent#agent.constraint,
    Morphology = Constraint#constraint.morphology,

    %% Get all available actuators from morphology
    AllActuators = morphology:get_Actuators(Morphology),

    %% Get actuator names already in network
    CurrentActuatorNames = get_current_actuator_names(Cortex#cortex.actuator_ids),

    %% Find actuators not yet in network
    CandidateActuators = [A || A <- AllActuators,
                               not lists:member(A#actuator.name, CurrentActuatorNames)],

    add_actuator_from_candidates(Agent, Cortex, CandidateActuators).

add_actuator_from_candidates(_Agent, _Cortex, []) ->
    {error, no_available_actuators};
add_actuator_from_candidates(Agent, Cortex, Candidates) ->
    %% Select random actuator template
    ActuatorTemplate = selection_utils:random_select(Candidates),

    %% Get innovation number for adding this actuator type
    %% Same actuator type always gets the same innovation (NEAT tracking)
    ActuatorInnovation = innovation:get_or_create_link_innovation(
        {add_actuator, ActuatorTemplate#actuator.name}, Agent#agent.cx_id),

    %% Create actuator with unique ID
    NewActuatorId = genotype:generate_id(actuator),
    NewActuator = ActuatorTemplate#actuator{
        id = NewActuatorId,
        cx_id = Agent#agent.cx_id,
        generation = Agent#agent.generation,
        fanin_ids = [],
        innovation = ActuatorInnovation
    },

    %% Write actuator to Mnesia
    genotype:write(NewActuator),

    %% Update cortex with new actuator
    NewActuatorIds = [NewActuatorId | Cortex#cortex.actuator_ids],
    UpdatedCortex = Cortex#cortex{actuator_ids = NewActuatorIds},
    genotype:write(UpdatedCortex),

    %% Connect a random neuron to this actuator
    connect_random_neuron_to_actuator(NewActuatorId, NewActuator, Cortex#cortex.neuron_ids).

get_current_actuator_names(ActuatorIds) ->
    [begin
        Actuator = genotype:dirty_read({actuator, AId}),
        Actuator#actuator.name
     end || AId <- ActuatorIds].

connect_random_neuron_to_actuator(_ActuatorId, _Actuator, []) ->
    %% No neurons - actuator added but unconnected (will connect when neurons added)
    ok;
connect_random_neuron_to_actuator(ActuatorId, Actuator, NeuronIds) ->
    NeuronId = selection_utils:random_select(NeuronIds),
    mutation_helpers:link_neuron_to_actuator(NeuronId, ActuatorId, Actuator),
    ok.
