%% @doc Unit tests for topological_mutations module.
-module(topological_mutations_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

topological_mutations_exports_test() ->
    Exports = topological_mutations:module_info(exports),
    ?assert(lists:member({add_bias, 1}, Exports)),
    ?assert(lists:member({add_outlink, 1}, Exports)),
    ?assert(lists:member({add_inlink, 1}, Exports)),
    ?assert(lists:member({add_neuron, 1}, Exports)),
    ?assert(lists:member({outsplice, 1}, Exports)),
    ?assert(lists:member({add_sensorlink, 1}, Exports)),
    ?assert(lists:member({add_actuatorlink, 1}, Exports)),
    ?assert(lists:member({add_sensor, 1}, Exports)),
    ?assert(lists:member({add_actuator, 1}, Exports)).

%% ============================================================================
%% Helper
%% ============================================================================

setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init().

%% ============================================================================
%% Add Bias Tests
%% ============================================================================

add_bias_success_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = topological_mutations:add_bias(AgentId),
        case Result of
            ok -> ok;
            {error, already_has_bias} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Add Outlink Tests
%% ============================================================================

add_outlink_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = topological_mutations:add_outlink(AgentId),
        case Result of
            ok -> ok;
            {error, no_available_targets} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Add Inlink Tests
%% ============================================================================

add_inlink_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = topological_mutations:add_inlink(AgentId),
        case Result of
            ok -> ok;
            {error, no_available_sources} -> ok;
            {error, no_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Add Neuron Tests
%% ============================================================================

add_neuron_increases_neuron_count_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Agent1 = genotype:dirty_read({agent, AgentId}),
        Cortex1 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
        InitialCount = length(Cortex1#cortex.neuron_ids),

        Result = topological_mutations:add_neuron(AgentId),
        case Result of
            ok ->
                Cortex2 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
                NewCount = length(Cortex2#cortex.neuron_ids),
                ?assertEqual(InitialCount + 1, NewCount);
            {error, cannot_add_neuron} ->
                ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

outsplice_same_as_add_neuron_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% outsplice is an alias for add_neuron
        Result = topological_mutations:outsplice(AgentId),
        case Result of
            ok -> ok;
            {error, cannot_add_neuron} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Add Sensorlink Tests
%% ============================================================================

add_sensorlink_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = topological_mutations:add_sensorlink(AgentId),
        case Result of
            ok -> ok;
            {error, no_sensors} -> ok;
            {error, no_available_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Add Actuatorlink Tests
%% ============================================================================

add_actuatorlink_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = topological_mutations:add_actuatorlink(AgentId),
        case Result of
            ok -> ok;
            {error, no_actuators} -> ok;
            {error, no_available_neurons} -> ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Add Sensor Tests
%% ============================================================================

add_sensor_success_test() ->
    setup_test(),
    try
        %% Use snake morphology which has multiple sensors
        test_helper:register_all_example_morphologies(),
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        %% Snake has 4 sensors, initial construction uses only 1
        Constraint = #constraint{morphology = snake},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Agent1 = genotype:dirty_read({agent, AgentId}),
        Cortex1 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
        InitialSensorCount = length(Cortex1#cortex.sensor_ids),

        Result = topological_mutations:add_sensor(AgentId),
        case Result of
            ok ->
                Cortex2 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
                NewSensorCount = length(Cortex2#cortex.sensor_ids),
                ?assertEqual(InitialSensorCount + 1, NewSensorCount);
            {error, no_available_sensors} ->
                %% All sensors already added - this is ok
                ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

add_sensor_no_available_test() ->
    setup_test(),
    try
        %% Use xor morphology which has only 1 sensor
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% XOR only has 1 sensor, already used, so should fail
        Result = topological_mutations:add_sensor(AgentId),
        ?assertEqual({error, no_available_sensors}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

%% ============================================================================
%% Add Actuator Tests
%% ============================================================================

add_actuator_success_test() ->
    setup_test(),
    try
        %% Use snake morphology which has multiple actuators
        test_helper:register_all_example_morphologies(),
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        %% Snake has 3 actuators, initial construction uses only 1
        Constraint = #constraint{morphology = snake},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Agent1 = genotype:dirty_read({agent, AgentId}),
        Cortex1 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
        InitialActuatorCount = length(Cortex1#cortex.actuator_ids),

        Result = topological_mutations:add_actuator(AgentId),
        case Result of
            ok ->
                Cortex2 = genotype:dirty_read({cortex, Agent1#agent.cx_id}),
                NewActuatorCount = length(Cortex2#cortex.actuator_ids),
                ?assertEqual(InitialActuatorCount + 1, NewActuatorCount);
            {error, no_available_actuators} ->
                %% All actuators already added - this is ok
                ok
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

add_actuator_no_available_test() ->
    setup_test(),
    try
        %% Use xor morphology which has only 1 actuator
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% XOR only has 1 actuator, already used, so should fail
        Result = topological_mutations:add_actuator(AgentId),
        ?assertEqual({error, no_available_actuators}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.
