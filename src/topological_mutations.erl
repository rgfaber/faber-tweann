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
    add_delay/1,
    add_leaky/1,
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
    add_delay/1,
    add_leaky/1,
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
    AvailableTargets0 = AllTargets -- CurrentOutputs -- [NeuronId],
    %% In feedforward mode, only connect to a strictly higher layer, so the
    %% network stays acyclic and the single-pass evaluator never waits on a
    %% feedback signal. See layer_of/1.
    AvailableTargets = forward_only(NeuronId, AvailableTargets0, Neuron),
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
    AvailableSources0 = AllSources -- CurrentInputIds -- [NeuronId],
    %% Feedforward: only accept input from a strictly lower layer.
    AvailableSources = backward_only(NeuronId, AvailableSources0, Neuron),
    connect_from_source(NeuronId, Neuron, AvailableSources).

%% @private Keep only targets at a strictly higher layer than the source
%% neuron, so an added output link runs forward.
forward_only(NeuronId, Targets, Neuron) ->
    case is_feedforward(Neuron) of
        false -> Targets;
        true ->
            L = layer_of(NeuronId),
            [T || T <- Targets, layer_of(T) > L]
    end.

%% @private Keep only sources at a strictly lower layer than the target
%% neuron, so an added input link runs forward.
backward_only(NeuronId, Sources, Neuron) ->
    case is_feedforward(Neuron) of
        false -> Sources;
        true ->
            L = layer_of(NeuronId),
            [S || S <- Sources, layer_of(S) < L]
    end.

%% @private A neuron belongs to a feedforward agent when its cortex's agent
%% declares connection_architecture = feedforward. Defaults to feedforward
%% when unknown, which is the safe direction: it never introduces a cycle the
%% evaluator cannot handle.
is_feedforward(Neuron) ->
    case genotype:dirty_read({cortex, Neuron#neuron.cx_id}) of
        undefined -> true;
        Cortex -> agent_is_feedforward(Cortex)
    end.

%% @private An agent is feedforward unless its constraint explicitly declares a
%% recurrent connection_architecture. A missing agent defaults to feedforward.
agent_is_feedforward(Cortex) ->
    case genotype:dirty_read({agent, Cortex#cortex.agent_id}) of
        undefined -> true;
        Agent ->
            (Agent#agent.constraint)#constraint.connection_architecture
                =/= recurrent
    end.

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

%% @private The layer coordinate of any element id.
%%
%% Ids are {{Layer, Unique}, Type}. Sensors sit at -1, actuators at +1,
%% neurons between. A spliced neuron takes a fractional layer strictly between
%% its endpoints, which is what keeps a feedforward network acyclic: every
%% connection then runs from a lower layer to a higher one. genotype's
%% generate_id(neuron) hardcodes layer 0, so every neuron used to land on the
%% same layer, turning same-layer connections into self/feedback loops that the
%% single-pass evaluator waits on forever (roadmap 2b, insight 010).
layer_of({{Layer, _Unique}, _Type}) when is_number(Layer) ->
    Layer.

insert_neuron(Agent, Cortex, FromId, ToId, Weight) ->
    %% Place the new neuron between its endpoints so feedforward order holds.
    NewLayer = (layer_of(FromId) + layer_of(ToId)) / 2,
    NewNeuronId = {{NewLayer, genotype:generate_UniqueId()}, neuron},
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

%% @doc Splice a MEMORY ORGANELLE into an existing connection.
%%
%% A to B becomes A to D to B, where D is a neuron whose neuron_type is delay:
%% it emits what it captured last tick and applies no activation. Chained, these
%% are a delay line, which is the structure insight 023 found when it dumped the
%% wiring of a network that had actually solved a memory task: a pure linear
%% chain, every neuron with exactly one input.
%%
%% ==========================================================================
%% WHY THE WEIGHTS GO WHERE THEY DO
%% ==========================================================================
%%
%% add_neuron/1 splices with the original weight on the new neuron's input and
%% again on the link out, which squares the path gain. For a delay that would
%% make the organelle a gain change as well as a delay, and the two effects
%% would be inseparable afterwards.
%%
%% So the delay's input weight is 1.0 and the original weight moves to the link
%% out. The path gain is preserved exactly and the mutation does one thing: it
%% costs the signal a tick.
%%
%% ==========================================================================
%% NOT IN THE DEFAULT OPERATOR LIST, AND THIS IS NOT AN OVERSIGHT
%% ==========================================================================
%%
%% A delay is evaluated by genotype_to_dag and by nothing else. The
%% process-per-neuron phenotype has no delay process, so exoself and constructor
%% now RAISE on one rather than spawning a standard neuron that would silently
%% ignore the type. A population driven by population_monitor would therefore
%% crash the moment this operator fired.
%%
%% Add it to a constraint's mutation_operators deliberately, for a population
%% evaluated through the DAG path. It is a capability the substrate offers and
%% the process path does not, and pretending otherwise is what the raise
%% prevents.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_delay(term()) -> ok | {error, term()}.
add_delay(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    case mutation_helpers:find_splittable_link(AgentId) of
        {error, no_links} ->
            {error, cannot_add_delay};
        {FromId, ToId, Weight} ->
            insert_organelle(Agent, Cortex, FromId, ToId, Weight, delay, 1.0)
    end.

insert_organelle(Agent, Cortex, FromId, ToId, Weight, Type, Tau) ->
    NewLayer = (layer_of(FromId) + layer_of(ToId)) / 2,
    OrganelleId = {{NewLayer, genotype:generate_UniqueId()}, neuron},
    {NodeInnovation, _In, _Out} = innovation:get_or_create_node_innovation(FromId, ToId),
    Unity = unity_like(Weight),
    Organelle = #neuron{
        id = OrganelleId,
        generation = Agent#agent.generation,
        cx_id = Agent#agent.cx_id,
        %% linear, because the evaluator applies no activation to a delay and a
        %% stored af would suggest otherwise to anything reading the genotype.
        af = linear,
        aggr_f = dot_product,
        neuron_type = Type,
        time_constant = Tau,
        input_idps = [{FromId, [Unity]}],
        output_ids = [ToId],
        ro_ids = [],
        innovation = NodeInnovation
    },
    mutation_helpers:update_source_output(FromId, ToId, OrganelleId),
    mutation_helpers:update_target_input(ToId, FromId, OrganelleId, Weight),
    genotype:write(Organelle),
    genotype:write(Cortex#cortex{neuron_ids = [OrganelleId | Cortex#cortex.neuron_ids]}),
    ok.

%% A weight is {W, DeltaWeight, LearningRate, ParamList}, always. Build a unit
%% weight carrying the same tuning fields as the one it replaces, so the
%% organelle is perturbed by the same machinery as everything else rather than
%% being frozen. No catch-all clause: dialyzer proves the shape, and a fallback
%% would be defensive code for a state that cannot occur.
unity_like({_W, Delta, Lr, Params}) -> {1.0, Delta, Lr, Params}.

%% @doc Splice a LEAKY INTEGRATOR into an existing connection.
%%
%% The other organelle. Where a delay holds a value for exactly one tick, a
%% leaky integrator moves its state toward its input by one part in tau each
%% tick, so it carries a decaying trace rather than a discrete memory. It reads
%% this tick's inputs, so unlike a delay it is ordered normally and does NOT
%% break a cycle.
%%
%% The time constant is drawn from the same range create_cfc_feedforward uses
%% for tau, [0.1, 2.0], and it is an ordinary genotype field, so
%% mutate_time_constant perturbs it like any other. That is what makes this an
%% EVOLVABLE organelle rather than a fixed one: placement is chosen by this
%% operator and the constant is then tuned by the machinery that already exists.
%%
%% ⚠ tau below 1.0 OVERSHOOTS rather than smoothing, since the update moves the
%% state more than the whole way to its input. That is a real part of the
%% parameter's range and is deliberately not clamped away; what is refused, in
%% genotype_to_dag, is a tau of zero or less, which the update cannot evaluate
%% at all.
%%
%% Weights follow add_delay's reasoning, not add_neuron's: unity in, the
%% original weight on the link out, so the mutation adds a dynamic and does not
%% also change the path gain.
%%
%% Like add_delay, deliberately absent from the default operator list. The
%% process phenotype has no leaky process and raises rather than running one as
%% an ordinary neuron.
%%
%% @param AgentId the agent to mutate
%% @returns ok or {error, term()}
-spec add_leaky(term()) -> ok | {error, term()}.
add_leaky(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    case mutation_helpers:find_splittable_link(AgentId) of
        {error, no_links} ->
            {error, cannot_add_leaky};
        {FromId, ToId, Weight} ->
            insert_organelle(Agent, Cortex, FromId, ToId, Weight, leaky, drawn_tau())
    end.

%% Same range as network_evaluator:create_cfc_feedforward/5 draws for tau, so an
%% organelle and a CfC neuron start life in the same regime.
drawn_tau() -> 0.1 + genotype_rand:uniform() * 1.9.

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
