%% @doc Unit tests for morphology and morphology_registry modules.
-module(morphology_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Test Setup/Teardown
%% ============================================================================

setup() ->
    application:ensure_all_started(faber_tweann),
    ok.

teardown() ->
    %% Unregister any morphologies registered during tests
    catch morphology_registry:unregister(test_morphology),
    catch morphology_registry:unregister(incomplete_morphology),
    ok.

%% ============================================================================
%% morphology_registry Export Tests
%% ============================================================================

morphology_registry_exports_test() ->
    Exports = morphology_registry:module_info(exports),
    ?assert(lists:member({start_link, 0}, Exports)),
    ?assert(lists:member({register, 2}, Exports)),
    ?assert(lists:member({unregister, 1}, Exports)),
    ?assert(lists:member({get, 1}, Exports)),
    ?assert(lists:member({list_all, 0}, Exports)),
    ?assert(lists:member({is_registered, 1}, Exports)).

%% ============================================================================
%% morphology Export Tests
%% ============================================================================

morphology_exports_test() ->
    Exports = morphology:module_info(exports),
    ?assert(lists:member({get_InitSensors, 1}, Exports)),
    ?assert(lists:member({get_InitActuators, 1}, Exports)),
    ?assert(lists:member({get_Sensors, 1}, Exports)),
    ?assert(lists:member({get_Actuators, 1}, Exports)).

%% ============================================================================
%% morphology_registry:register/2 Tests
%% ============================================================================

register_valid_morphology_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        %% xor_mimic should be registered
        ?assertEqual({ok, morphology_xor}, morphology_registry:get(xor_mimic)),
        %% Verify we can look it up
        ?assert(morphology_registry:is_registered(xor_mimic))
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

register_duplicate_overwrites_test() ->
    setup(),
    try
        %% Register xor_mimic with one module
        ok = morphology_registry:register(xor_mimic, morphology_xor),
        ?assertEqual({ok, morphology_xor}, morphology_registry:get(xor_mimic)),

        %% Re-register with different module should overwrite
        ok = morphology_registry:register(xor_mimic, morphology_snake),
        ?assertEqual({ok, morphology_snake}, morphology_registry:get(xor_mimic))
    after
        morphology_registry:unregister(xor_mimic),
        teardown()
    end.

register_missing_callback_test() ->
    setup(),
    try
        %% Try to register a module that doesn't implement morphology_behaviour
        %% Using 'lists' module which doesn't have get_sensors/1
        Result = morphology_registry:register(bad_morph, lists),
        ?assertMatch({error, {missing_callback, _, _}}, Result)
    after
        teardown()
    end.

register_nonexistent_module_test() ->
    setup(),
    try
        Result = morphology_registry:register(fake_morph, nonexistent_module_xyz),
        ?assertMatch({error, {module_not_loaded, _}}, Result)
    after
        teardown()
    end.

%% ============================================================================
%% morphology_registry:unregister/1 Tests
%% ============================================================================

unregister_registered_morphology_test() ->
    setup(),
    try
        ok = morphology_registry:register(xor_mimic, morphology_xor),
        ?assert(morphology_registry:is_registered(xor_mimic)),

        ok = morphology_registry:unregister(xor_mimic),
        ?assertNot(morphology_registry:is_registered(xor_mimic))
    after
        teardown()
    end.

unregister_nonexistent_morphology_test() ->
    setup(),
    try
        %% Unregistering a non-existent morphology should succeed silently
        Result = morphology_registry:unregister(never_registered_morph),
        ?assertEqual(ok, Result)
    after
        teardown()
    end.

%% ============================================================================
%% morphology_registry:get/1 Tests
%% ============================================================================

get_registered_morphology_test() ->
    setup(),
    try
        ok = morphology_registry:register(xor_mimic, morphology_xor),
        ?assertEqual({ok, morphology_xor}, morphology_registry:get(xor_mimic))
    after
        morphology_registry:unregister(xor_mimic),
        teardown()
    end.

get_unregistered_morphology_test() ->
    setup(),
    try
        Result = morphology_registry:get(unregistered_morphology),
        ?assertEqual({error, not_found}, Result)
    after
        teardown()
    end.

%% ============================================================================
%% morphology_registry:list_all/0 Tests
%% ============================================================================

list_all_empty_test() ->
    setup(),
    try
        %% Make sure nothing is registered
        lists:foreach(
            fun(M) -> morphology_registry:unregister(M) end,
            morphology_registry:list_all()
        ),
        ?assertEqual([], morphology_registry:list_all())
    after
        teardown()
    end.

list_all_with_registrations_test() ->
    setup(),
    try
        ok = morphology_registry:register(xor_mimic, morphology_xor),
        ok = morphology_registry:register(snake, morphology_snake),

        All = morphology_registry:list_all(),
        ?assert(lists:member(xor_mimic, All)),
        ?assert(lists:member(snake, All))
    after
        morphology_registry:unregister(xor_mimic),
        morphology_registry:unregister(snake),
        teardown()
    end.

%% ============================================================================
%% morphology_registry:is_registered/1 Tests
%% ============================================================================

is_registered_true_test() ->
    setup(),
    try
        ok = morphology_registry:register(xor_mimic, morphology_xor),
        ?assert(morphology_registry:is_registered(xor_mimic))
    after
        morphology_registry:unregister(xor_mimic),
        teardown()
    end.

is_registered_false_test() ->
    setup(),
    try
        ?assertNot(morphology_registry:is_registered(never_registered_morph))
    after
        teardown()
    end.

%% ============================================================================
%% morphology:get_Sensors/1 Tests
%% ============================================================================

get_sensors_registered_morphology_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        Sensors = morphology:get_Sensors(xor_mimic),

        %% Should return a list of sensor records
        ?assert(is_list(Sensors)),
        ?assert(length(Sensors) >= 1),

        %% Each element should be a sensor record
        lists:foreach(
            fun(S) -> ?assert(is_record(S, sensor)) end,
            Sensors
        )
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

get_sensors_unregistered_morphology_test() ->
    setup(),
    try
        %% Should throw error for unregistered morphology
        ?assertError(
            {morphology_not_registered, unregistered_morph, _},
            morphology:get_Sensors(unregistered_morph)
        )
    after
        teardown()
    end.

%% Note: The legacy {Module, Function} tuple format tests are omitted
%% because the current morphology modules use get_sensors/1 and get_actuators/1
%% with the morphology name as the argument, not the old M:F(sensors) pattern.

%% ============================================================================
%% morphology:get_Actuators/1 Tests
%% ============================================================================

get_actuators_registered_morphology_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        Actuators = morphology:get_Actuators(xor_mimic),

        %% Should return a list of actuator records
        ?assert(is_list(Actuators)),
        ?assert(length(Actuators) >= 1),

        %% Each element should be an actuator record
        lists:foreach(
            fun(A) -> ?assert(is_record(A, actuator)) end,
            Actuators
        )
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

get_actuators_unregistered_morphology_test() ->
    setup(),
    try
        %% Should throw error for unregistered morphology
        ?assertError(
            {morphology_not_registered, unregistered_morph, _},
            morphology:get_Actuators(unregistered_morph)
        )
    after
        teardown()
    end.

%% Legacy {Module, Function} tuple format test omitted - see note above.

%% ============================================================================
%% morphology:get_InitSensors/1 Tests
%% ============================================================================

get_init_sensors_returns_single_sensor_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        InitSensors = morphology:get_InitSensors(xor_mimic),

        %% Should return exactly one sensor (the first one)
        ?assertEqual(1, length(InitSensors)),
        [FirstSensor] = InitSensors,
        ?assert(is_record(FirstSensor, sensor))
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

%% Legacy {Module, Function} tuple format test omitted - see note above.

%% ============================================================================
%% morphology:get_InitActuators/1 Tests
%% ============================================================================

get_init_actuators_returns_single_actuator_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        InitActuators = morphology:get_InitActuators(xor_mimic),

        %% Should return exactly one actuator (the first one)
        ?assertEqual(1, length(InitActuators)),
        [FirstActuator] = InitActuators,
        ?assert(is_record(FirstActuator, actuator))
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

%% Legacy {Module, Function} tuple format test omitted - see note above.

%% ============================================================================
%% Multiple Morphology Tests
%% ============================================================================

multiple_morphologies_isolated_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),

        %% Each morphology should return different sensors/actuators
        XorSensors = morphology:get_Sensors(xor_mimic),
        SnakeSensors = morphology:get_Sensors(snake),

        %% Sensors from different morphologies may differ
        %% At minimum they should both be valid
        ?assert(is_list(XorSensors)),
        ?assert(is_list(SnakeSensors)),
        ?assert(length(XorSensors) >= 1),
        ?assert(length(SnakeSensors) >= 1)
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

%% ============================================================================
%% Morphology Module Tests (Example Morphologies)
%% ============================================================================

xor_morphology_sensors_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        Sensors = morphology:get_Sensors(xor_mimic),

        %% XOR morphology should have specific sensor configuration
        ?assert(length(Sensors) >= 1),
        [FirstSensor | _] = Sensors,
        ?assert(is_record(FirstSensor, sensor)),
        %% XOR input sensor typically has vl=2 (two inputs)
        ?assert(is_integer(FirstSensor#sensor.vl))
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

xor_morphology_actuators_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        Actuators = morphology:get_Actuators(xor_mimic),

        %% XOR morphology should have specific actuator configuration
        ?assert(length(Actuators) >= 1),
        [FirstActuator | _] = Actuators,
        ?assert(is_record(FirstActuator, actuator)),
        %% XOR output actuator typically has vl=1 (single output)
        ?assert(is_integer(FirstActuator#actuator.vl))
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

snake_morphology_has_multiple_sensors_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        Sensors = morphology:get_Sensors(snake),

        %% Snake morphology should have multiple sensors
        ?assert(length(Sensors) >= 2)
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

snake_morphology_has_multiple_actuators_test() ->
    setup(),
    try
        test_helper:register_all_example_morphologies(),
        Actuators = morphology:get_Actuators(snake),

        %% Snake morphology should have multiple actuators
        ?assert(length(Actuators) >= 2)
    after
        test_helper:unregister_all_example_morphologies(),
        teardown()
    end.

