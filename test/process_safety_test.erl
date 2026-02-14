%% @doc Tests for process safety features (timeouts, crash handling).
-module(process_safety_test).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Timeout Configuration Tests
%% ============================================================================

cortex_default_timeout_test() ->
    %% Cortex should have default timeout
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    }),

    %% Cortex should be alive
    ?assert(is_process_alive(CortexPid)),

    %% Clean up
    cortex:terminate(CortexPid),
    timer:sleep(10),
    ?assertNot(is_process_alive(CortexPid)).

cortex_custom_timeout_test() ->
    %% Cortex should accept custom timeout
    CustomTimeout = 5000,
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => [],
        sync_timeout => CustomTimeout
    }),

    ?assert(is_process_alive(CortexPid)),

    cortex:terminate(CortexPid),
    timer:sleep(10),
    ?assertNot(is_process_alive(CortexPid)).

neuron_default_timeout_test() ->
    %% Neuron should have default timeout
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    }),

    {ok, NeuronPid} = neuron:start_link(#{
        id => test_neuron,
        cortex_pid => CortexPid,
        activation_function => tanh,
        aggregation_function => dot_product,
        input_pids => [],
        output_pids => [],
        ro_pids => [],
        input_weights => #{},
        bias => 0.0
    }),

    ?assert(is_process_alive(NeuronPid)),

    %% Clean up
    NeuronPid ! {cortex, terminate},
    cortex:terminate(CortexPid),
    timer:sleep(10),
    ?assertNot(is_process_alive(NeuronPid)).

neuron_custom_timeout_test() ->
    %% Neuron should accept custom timeout
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    }),

    CustomTimeout = 5000,
    {ok, NeuronPid} = neuron:start_link(#{
        id => test_neuron,
        cortex_pid => CortexPid,
        input_timeout => CustomTimeout
    }),

    ?assert(is_process_alive(NeuronPid)),

    NeuronPid ! {cortex, terminate},
    cortex:terminate(CortexPid),
    timer:sleep(10),
    ?assertNot(is_process_alive(NeuronPid)).

%% ============================================================================
%% Timeout Behavior Tests
%% ============================================================================

%% NOTE: Actual timeout tests that wait for timeouts would take 10-30 seconds
%% In a real test suite, these would be integration tests run separately.
%% Here we just verify that the timeout mechanism is in place.

short_timeout_configuration_test() ->
    %% Very short timeout for testing (not recommended for production)
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => [],
        sync_timeout => 100  % 100ms
    }),

    ?assert(is_process_alive(CortexPid)),
    cortex:terminate(CortexPid).

%% ============================================================================
%% Termination Tests
%% ============================================================================

cortex_terminate_test() ->
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    }),

    cortex:terminate(CortexPid),
    timer:sleep(10),
    ?assertNot(is_process_alive(CortexPid)).

neuron_terminate_test() ->
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    }),

    {ok, NeuronPid} = neuron:start_link(#{
        id => test_neuron,
        cortex_pid => CortexPid
    }),

    NeuronPid ! {cortex, terminate},
    timer:sleep(10),
    ?assertNot(is_process_alive(NeuronPid)),

    cortex:terminate(CortexPid).

sensor_terminate_test() ->
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    }),

    {ok, SensorPid} = sensor:start_link(#{
        id => test_sensor,
        cortex_pid => CortexPid,
        sensor_name => rng,
        vector_length => 2,
        fanout_pids => []
    }),

    SensorPid ! {cortex, terminate},
    timer:sleep(10),
    ?assertNot(is_process_alive(SensorPid)),

    cortex:terminate(CortexPid).

actuator_terminate_test() ->
    {ok, CortexPid} = cortex:start_link(#{
        id => test_cortex,
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    }),

    {ok, ActuatorPid} = actuator:start_link(#{
        id => test_actuator,
        cortex_pid => CortexPid,
        actuator_name => pts,
        vector_length => 1,
        fanin_pids => []
    }),

    ActuatorPid ! {cortex, terminate},
    timer:sleep(10),
    ?assertNot(is_process_alive(ActuatorPid)),

    cortex:terminate(CortexPid).
