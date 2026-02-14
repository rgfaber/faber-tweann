-module(cortex_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper to create a simple cortex
simple_cortex_opts() ->
    #{
        id => test_cortex,
        exoself_pid => self(),
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => [],
        max_cycles => infinity
    }.


%% =============================================================================
%% Start/Init Tests
%% =============================================================================

start_link_test() ->
    {ok, Pid} = cortex:start_link(simple_cortex_opts()),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    cortex:terminate(Pid).

start_with_max_cycles_test() ->
    Opts = (simple_cortex_opts())#{max_cycles => 10},
    {ok, Pid} = cortex:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    cortex:terminate(Pid).

%% =============================================================================
%% Sync Tests
%% =============================================================================

sync_triggers_sensors_test() ->
    Self = self(),

    %% Create a simple sensor that reports when synced
    Sensor = spawn(fun() ->
        receive
            {cortex, sync} ->
                Self ! sensor_synced
        end
    end),

    Opts = (simple_cortex_opts())#{
        sensor_pids => [Sensor]
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    cortex:sync(CortexPid),

    receive
        sensor_synced -> ok
    after 1000 ->
        ?assert(false)
    end,

    cortex:terminate(CortexPid).

sync_multiple_sensors_test() ->
    Self = self(),

    %% Create multiple sensors
    Sensors = [spawn(fun() ->
        receive
            {cortex, sync} ->
                Self ! {sensor_synced, I}
        end
    end) || I <- [1, 2, 3]],

    Opts = (simple_cortex_opts())#{
        sensor_pids => Sensors
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    cortex:sync(CortexPid),

    %% All sensors should be synced
    lists:foreach(fun(I) ->
        receive
            {sensor_synced, I} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, [1, 2, 3]),

    cortex:terminate(CortexPid).

%% =============================================================================
%% Actuator Output Tests
%% =============================================================================

single_actuator_output_test() ->
    {ok, CortexPid} = cortex:start_link(simple_cortex_opts()),

    %% Simulate single actuator reporting
    Opts = (simple_cortex_opts())#{
        actuator_pids => [self()]  % Dummy, just need count
    },

    {ok, CortexPid2} = cortex:start_link(Opts),

    %% Send actuator output
    CortexPid2 ! {actuator_output, self(), [1.0, 2.0]},

    receive
        {cortex, test_cortex, evaluation_complete, Outputs} ->
            ?assertEqual([1.0, 2.0], Outputs)
    after 1000 ->
        ?assert(false)
    end,

    cortex:terminate(CortexPid),
    cortex:terminate(CortexPid2).

multiple_actuator_outputs_test() ->
    %% Create actuator PIDs (just for counting)
    Act1 = spawn(fun() -> receive _ -> ok end end),
    Act2 = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_cortex_opts())#{
        actuator_pids => [Act1, Act2]
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    %% First actuator reports - should not complete yet
    CortexPid ! {actuator_output, Act1, [1.0]},

    receive
        {cortex, test_cortex, evaluation_complete, _} ->
            ?assert(false)  % Should not receive yet
    after 100 ->
        ok
    end,

    %% Second actuator reports - now should complete
    CortexPid ! {actuator_output, Act2, [2.0]},

    receive
        {cortex, test_cortex, evaluation_complete, Outputs} ->
            ?assertEqual([1.0, 2.0], Outputs)
    after 1000 ->
        ?assert(false)
    end,

    cortex:terminate(CortexPid),
    Act1 ! done,
    Act2 ! done.

%% =============================================================================
%% Full Cycle Tests
%% =============================================================================

simple_network_cycle_test() ->
    %% Create a simple network: sensor -> actuator
    %% The actuator needs to know the cortex PID to report back
    %% Use a two-phase approach: spawn actors that wait for cortex PID

    %% Spawn actuator that waits for cortex PID then loops
    Actuator = spawn(fun() ->
        receive
            {set_cortex, CortexPid} ->
                actuator_loop(CortexPid)
        end
    end),

    %% Spawn sensor that waits for cortex PID then loops
    Sensor = spawn(fun() ->
        receive
            {set_cortex, _CortexPid} ->
                sensor_loop(Actuator, [3.14])
        end
    end),

    Opts = (simple_cortex_opts())#{
        sensor_pids => [Sensor],
        actuator_pids => [Actuator]
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    %% Tell components about cortex
    Actuator ! {set_cortex, CortexPid},
    Sensor ! {set_cortex, CortexPid},

    %% Small delay to let messages arrive
    timer:sleep(10),

    %% Trigger sync
    cortex:sync(CortexPid),

    %% Should receive evaluation complete
    receive
        {cortex, test_cortex, evaluation_complete, Outputs} ->
            ?assertEqual([3.14], Outputs)
    after 1000 ->
        ?assert(false)
    end,

    cortex:terminate(CortexPid).

sensor_loop(OutputPid, Signal) ->
    receive
        {cortex, sync} ->
            OutputPid ! {forward, self(), Signal},
            sensor_loop(OutputPid, Signal);
        {cortex, terminate} ->
            ok
    end.

actuator_loop(CortexPid) ->
    receive
        {forward, _From, Signal} ->
            CortexPid ! {actuator_output, self(), Signal},
            actuator_loop(CortexPid);
        {cortex, terminate} ->
            ok
    end.

multiple_cycles_test() ->
    %% Spawn actuator that waits for cortex PID
    Actuator = spawn(fun() ->
        receive
            {set_cortex, CortexPid} ->
                actuator_loop(CortexPid)
        end
    end),

    %% Spawn sensor
    Sensor = spawn(fun() ->
        receive
            {set_cortex, _CortexPid} ->
                sensor_loop(Actuator, [1.0])
        end
    end),

    Opts = (simple_cortex_opts())#{
        sensor_pids => [Sensor],
        actuator_pids => [Actuator]
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    %% Tell components about cortex
    Actuator ! {set_cortex, CortexPid},
    Sensor ! {set_cortex, CortexPid},
    timer:sleep(10),

    %% Run multiple cycles
    lists:foreach(fun(_) ->
        cortex:sync(CortexPid),
        receive
            {cortex, test_cortex, evaluation_complete, [1.0]} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, [1, 2, 3]),

    cortex:terminate(CortexPid).

%% =============================================================================
%% Max Cycles Tests
%% =============================================================================

max_cycles_reached_test() ->
    Act = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_cortex_opts())#{
        actuator_pids => [Act],
        max_cycles => 2
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    %% First cycle
    cortex:sync(CortexPid),
    CortexPid ! {actuator_output, Act, [1.0]},
    receive {cortex, test_cortex, evaluation_complete, _} -> ok after 1000 -> ?assert(false) end,

    %% Second cycle - should trigger max_cycles_reached
    cortex:sync(CortexPid),
    CortexPid ! {actuator_output, Act, [2.0]},

    %% Should receive both completion and max_cycles_reached
    receive {cortex, test_cortex, evaluation_complete, _} -> ok after 1000 -> ?assert(false) end,
    receive {cortex, test_cortex, max_cycles_reached, 2} -> ok after 1000 -> ?assert(false) end,

    cortex:terminate(CortexPid),
    Act ! done.

%% =============================================================================
%% Backup Tests
%% =============================================================================

backup_requests_neurons_test() ->
    Self = self(),

    %% Create mock neurons that report when backup is requested
    Neurons = [spawn(fun() ->
        receive
            backup ->
                Self ! {neuron_backup_requested, I}
        end
    end) || I <- [1, 2]],

    Opts = (simple_cortex_opts())#{
        neuron_pids => Neurons
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    cortex:backup(CortexPid),

    %% All neurons should receive backup request
    lists:foreach(fun(I) ->
        receive
            {neuron_backup_requested, I} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, [1, 2]),

    cortex:terminate(CortexPid).

neuron_backup_forwarding_test() ->
    Opts = simple_cortex_opts(),
    {ok, CortexPid} = cortex:start_link(Opts),

    %% Simulate neuron sending backup
    Weights = #{some_pid => [{0.5, 0.0, 0.1, []}]},
    Bias = 0.3,
    CortexPid ! {backup, neuron_1, Weights, Bias},

    receive
        {cortex, test_cortex, neuron_backup, neuron_1, Weights, Bias} ->
            ok
    after 1000 ->
        ?assert(false)
    end,

    cortex:terminate(CortexPid).

%% =============================================================================
%% Terminate Tests
%% =============================================================================

terminate_all_components_test() ->
    Self = self(),

    %% Create components that report termination
    Sensor = spawn(fun() ->
        receive {cortex, terminate} -> Self ! sensor_terminated end
    end),
    Neuron = spawn(fun() ->
        receive {cortex, terminate} -> Self ! neuron_terminated end
    end),
    Actuator = spawn(fun() ->
        receive {cortex, terminate} -> Self ! actuator_terminated end
    end),

    Opts = (simple_cortex_opts())#{
        sensor_pids => [Sensor],
        neuron_pids => [Neuron],
        actuator_pids => [Actuator]
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    cortex:terminate(CortexPid),

    %% All components should receive terminate
    receive sensor_terminated -> ok after 1000 -> ?assert(false) end,
    receive neuron_terminated -> ok after 1000 -> ?assert(false) end,
    receive actuator_terminated -> ok after 1000 -> ?assert(false) end.

exoself_stop_terminates_test() ->
    Self = self(),

    Neuron = spawn(fun() ->
        receive {cortex, terminate} -> Self ! neuron_terminated end
    end),

    Opts = (simple_cortex_opts())#{
        neuron_pids => [Neuron]
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    %% Send stop from exoself
    CortexPid ! {exoself, stop},

    receive neuron_terminated -> ok after 1000 -> ?assert(false) end.

%% =============================================================================
%% Edge Cases
%% =============================================================================

no_exoself_test() ->
    %% Cortex without exoself should not crash
    Opts = (simple_cortex_opts())#{
        exoself_pid => undefined,
        actuator_pids => [self()]
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    %% Should handle output without exoself
    CortexPid ! {actuator_output, self(), [1.0]},

    %% Should not receive anything (no exoself)
    receive
        {cortex, _, _, _} ->
            ?assert(false)
    after 100 ->
        ok
    end,

    cortex:terminate(CortexPid).

empty_network_test() ->
    %% Network with no components
    Opts = (simple_cortex_opts())#{
        sensor_pids => [],
        neuron_pids => [],
        actuator_pids => []
    },

    {ok, CortexPid} = cortex:start_link(Opts),

    %% Sync should work but not produce output
    cortex:sync(CortexPid),

    receive
        {cortex, _, evaluation_complete, _} ->
            ?assert(false)  % No actuators means no completion
    after 100 ->
        ok
    end,

    cortex:terminate(CortexPid).
