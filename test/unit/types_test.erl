%% @module types_test
%% @doc Unit tests for type specifications
%%
%% Validates that the type specifications in types.hrl and records.hrl
%% are correctly defined and can be used as expected.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0

-module(types_test).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%%==============================================================================
%% Weight Specification Tests
%%==============================================================================

%% @doc Test that weight_spec type is correctly structured
weight_spec_structure_test() ->
    %% Weight format: {Weight, DeltaWeight, LearningRate, ParamList}
    WeightSpec = {0.5, 0.0, 0.1, [0.1, 0.2]},
    {W, DW, LR, Params} = WeightSpec,

    ?assert(is_float(W)),
    ?assert(is_float(DW)),
    ?assert(is_float(LR)),
    ?assert(is_list(Params)),
    ?assertEqual(0.5, W),
    ?assertEqual(0.0, DW),
    ?assertEqual(0.1, LR),
    ?assertEqual([0.1, 0.2], Params).

%% @doc Test weight_spec with empty parameter list
weight_spec_empty_params_test() ->
    WeightSpec = {0.3, 0.01, 0.05, []},
    {_W, _DW, _LR, Params} = WeightSpec,
    ?assertEqual([], Params).

%%==============================================================================
%% Entity Identifier Tests
%%==============================================================================

%% @doc Test neuron_id structure
neuron_id_structure_test() ->
    NeuronId = {{0.5, 0.123}, neuron},
    {{Layer, UniqueId}, Type} = NeuronId,

    ?assert(is_float(Layer)),
    ?assert(is_float(UniqueId)),
    ?assertEqual(neuron, Type),
    %% Layer should be between 0 and 1 for hidden neurons
    ?assert(Layer >= 0.0 andalso Layer =< 1.0).

%% @doc Test sensor_id structure
sensor_id_structure_test() ->
    SensorId = {{-1.0, 0.456}, sensor},
    {{Layer, _UniqueId}, Type} = SensorId,

    ?assertEqual(-1.0, Layer),
    ?assertEqual(sensor, Type).

%% @doc Test actuator_id structure
actuator_id_structure_test() ->
    ActuatorId = {{1.0, 0.789}, actuator},
    {{Layer, _UniqueId}, Type} = ActuatorId,

    ?assertEqual(1.0, Layer),
    ?assertEqual(actuator, Type).

%% @doc Test cortex_id structure
cortex_id_structure_test() ->
    CortexId = {{0.0, 0.111}, cortex},
    {{Layer, _UniqueId}, Type} = CortexId,

    ?assertEqual(0.0, Layer),
    ?assertEqual(cortex, Type).

%%==============================================================================
%% Record Construction Tests
%%==============================================================================

%% @doc Test neuron record construction
neuron_record_test() ->
    N = #neuron{
        id = {{0.5, 0.5}, neuron},
        generation = 1,
        cx_id = {{0.0, 0.1}, cortex},
        af = tanh,
        pf = {none, []},
        aggr_f = dot_product
    },

    ?assertEqual(1, N#neuron.generation),
    ?assertEqual(tanh, N#neuron.af),
    ?assertEqual({none, []}, N#neuron.pf),
    ?assertEqual(dot_product, N#neuron.aggr_f),
    ?assertEqual([], N#neuron.input_idps),
    ?assertEqual([], N#neuron.output_ids),
    ?assertMatch({{_, _}, cortex}, N#neuron.cx_id).

%% @doc Test sensor record construction
sensor_record_test() ->
    S = #sensor{
        id = {{-1.0, 0.5}, sensor},
        name = test_sensor,
        type = standard,
        cx_id = {{0.0, 0.1}, cortex},
        vl = 3,
        generation = 0
    },

    ?assertEqual(test_sensor, S#sensor.name),
    ?assertEqual(standard, S#sensor.type),
    ?assertEqual(3, S#sensor.vl),
    ?assertEqual([], S#sensor.fanout_ids).

%% @doc Test actuator record construction
actuator_record_test() ->
    A = #actuator{
        id = {{1.0, 0.5}, actuator},
        name = test_actuator,
        type = standard,
        cx_id = {{0.0, 0.1}, cortex},
        vl = 2,
        generation = 0
    },

    ?assertEqual(test_actuator, A#actuator.name),
    ?assertEqual(2, A#actuator.vl),
    ?assertEqual([], A#actuator.fanin_ids).

%% @doc Test cortex record construction
cortex_record_test() ->
    C = #cortex{
        id = {{0.0, 0.1}, cortex},
        agent_id = {0.1, agent},
        neuron_ids = [{{0.5, 0.2}, neuron}],
        sensor_ids = [{{-1.0, 0.3}, sensor}],
        actuator_ids = [{{1.0, 0.4}, actuator}]
    },

    ?assertEqual(1, length(C#cortex.neuron_ids)),
    ?assertEqual(1, length(C#cortex.sensor_ids)),
    ?assertEqual(1, length(C#cortex.actuator_ids)).

%%==============================================================================
%% Weighted Inputs Tests
%%==============================================================================

%% @doc Test weighted_inputs structure
weighted_inputs_structure_test() ->
    SensorId = {{-1.0, 0.5}, sensor},
    WeightedInputs = [
        {SensorId, [{0.5, 0.0, 0.1, []}, {0.3, 0.0, 0.1, []}]}
    ],

    [{SourceId, Weights}] = WeightedInputs,
    ?assertEqual(SensorId, SourceId),
    ?assertEqual(2, length(Weights)),

    [{W1, DW1, LR1, P1}, {W2, _DW2, _LR2, _P2}] = Weights,
    ?assertEqual(0.5, W1),
    ?assertEqual(0.0, DW1),
    ?assertEqual(0.1, LR1),
    ?assertEqual([], P1),
    ?assertEqual(0.3, W2).

%% @doc Test multiple source weighted inputs
multiple_sources_weighted_inputs_test() ->
    Sensor1Id = {{-1.0, 0.1}, sensor},
    Sensor2Id = {{-1.0, 0.2}, sensor},

    WeightedInputs = [
        {Sensor1Id, [{0.5, 0.0, 0.1, []}]},
        {Sensor2Id, [{0.3, 0.0, 0.1, []}]}
    ],

    ?assertEqual(2, length(WeightedInputs)),
    [{S1, W1}, {S2, W2}] = WeightedInputs,
    ?assertEqual(Sensor1Id, S1),
    ?assertEqual(Sensor2Id, S2),
    ?assertEqual(1, length(W1)),
    ?assertEqual(1, length(W2)).

%%==============================================================================
%% Constraint Record Tests
%%==============================================================================

%% @doc Test constraint record defaults
constraint_defaults_test() ->
    C = #constraint{},

    ?assertEqual(xor_mimic, C#constraint.morphology),
    ?assertEqual(recurrent, C#constraint.connection_architecture),
    ?assertEqual([tanh, cos, gaussian], C#constraint.neural_afs),
    ?assertEqual([none], C#constraint.neural_pfns),
    ?assertEqual([dot_product], C#constraint.neural_aggr_fs),
    ?assertEqual(generational, C#constraint.population_evo_alg_f).

%% @doc Test constraint with custom values
constraint_custom_test() ->
    C = #constraint{
        morphology = custom_morph,
        connection_architecture = feedforward,
        neural_afs = [tanh, sin]
    },

    ?assertEqual(custom_morph, C#constraint.morphology),
    ?assertEqual(feedforward, C#constraint.connection_architecture),
    ?assertEqual([tanh, sin], C#constraint.neural_afs).

%%==============================================================================
%% Plasticity Function Tests
%%==============================================================================

%% @doc Test plasticity function specification
plasticity_spec_test() ->
    %% No plasticity
    NoPlasticity = {none, []},
    ?assertEqual(none, element(1, NoPlasticity)),

    %% Hebbian plasticity
    Hebbian = {hebbian, [0.1]},
    ?assertEqual(hebbian, element(1, Hebbian)),
    ?assertEqual([0.1], element(2, Hebbian)),

    %% Ojas plasticity with parameters
    Ojas = {ojas, [0.1, 0.5]},
    ?assertEqual(ojas, element(1, Ojas)),
    ?assertEqual([0.1, 0.5], element(2, Ojas)).

%%==============================================================================
%% Agent Record Tests
%%==============================================================================

%% @doc Test agent record construction
agent_record_test() ->
    A = #agent{
        id = {0.1, agent},
        encoding_type = neural,
        generation = 5,
        fitness = 0.85,
        heredity_type = darwinian
    },

    ?assertEqual({0.1, agent}, A#agent.id),
    ?assertEqual(neural, A#agent.encoding_type),
    ?assertEqual(5, A#agent.generation),
    ?assertEqual(0.85, A#agent.fitness),
    ?assertEqual(darwinian, A#agent.heredity_type),
    ?assertEqual([], A#agent.evo_hist),
    ?assertEqual([false], A#agent.champion_flag).

%%==============================================================================
%% Trace and Stat Record Tests
%%==============================================================================

%% @doc Test trace record defaults
trace_record_test() ->
    T = #trace{},

    ?assertEqual([], T#trace.stats),
    ?assertEqual(0, T#trace.tot_evaluations),
    ?assertEqual(500, T#trace.step_size).

%% @doc Test stat record construction
stat_record_test() ->
    S = #stat{
        morphology = xor_mimic,
        avg_fitness = 0.75,
        max_fitness = 0.95,
        min_fitness = 0.50,
        evaluations = 1000
    },

    ?assertEqual(xor_mimic, S#stat.morphology),
    ?assertEqual(0.75, S#stat.avg_fitness),
    ?assertEqual(0.95, S#stat.max_fitness),
    ?assertEqual(0.50, S#stat.min_fitness),
    ?assertEqual(1000, S#stat.evaluations).
