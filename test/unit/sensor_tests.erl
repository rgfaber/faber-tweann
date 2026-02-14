-module(sensor_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper to create a simple sensor
simple_sensor_opts() ->
    #{
        id => test_sensor,
        cortex_pid => self(),
        sensor_name => rng,
        vector_length => 3,
        fanout_pids => [],
        scape_pid => undefined,
        parameters => []
    }.

%% =============================================================================
%% Start/Init Tests
%% =============================================================================

start_link_test() ->
    {ok, Pid} = sensor:start_link(simple_sensor_opts()),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

start_with_custom_sensor_test() ->
    Opts = (simple_sensor_opts())#{sensor_name => ones},
    {ok, Pid} = sensor:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

start_with_fanout_test() ->
    Receiver = spawn(fun() -> receive _ -> ok end end),
    Opts = (simple_sensor_opts())#{fanout_pids => [Receiver]},
    {ok, Pid} = sensor:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate},
    Receiver ! done.

%% =============================================================================
%% Sync Tests
%% =============================================================================

sync_forwards_to_fanout_test() ->
    Opts = (simple_sensor_opts())#{
        sensor_name => ones,
        vector_length => 2,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual([1.0, 1.0], Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

sync_multiple_fanout_test() ->
    Self = self(),
    Receiver1 = spawn(fun() ->
        receive {forward, _, S} -> Self ! {recv1, S} end
    end),
    Receiver2 = spawn(fun() ->
        receive {forward, _, S} -> Self ! {recv2, S} end
    end),

    Opts = (simple_sensor_opts())#{
        sensor_name => ones,
        vector_length => 1,
        fanout_pids => [Receiver1, Receiver2]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive {recv1, [1.0]} -> ok after 1000 -> ?assert(false) end,
    receive {recv2, [1.0]} -> ok after 1000 -> ?assert(false) end,

    SensorPid ! {cortex, terminate}.

%% =============================================================================
%% Sensor Function Tests
%% =============================================================================

sensor_rng_test() ->
    Opts = (simple_sensor_opts())#{
        sensor_name => rng,
        vector_length => 5,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual(5, length(Signal)),
            %% Values should be in [-1, 1]
            lists:foreach(fun(V) ->
                ?assert(V >= -1.0 andalso V =< 1.0)
            end, Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

sensor_ones_test() ->
    Opts = (simple_sensor_opts())#{
        sensor_name => ones,
        vector_length => 3,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual([1.0, 1.0, 1.0], Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

sensor_zeros_test() ->
    Opts = (simple_sensor_opts())#{
        sensor_name => zeros,
        vector_length => 4,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual([0.0, 0.0, 0.0, 0.0], Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

sensor_counter_test() ->
    Opts = (simple_sensor_opts())#{
        sensor_name => counter,
        vector_length => 4,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual([0.25, 0.5, 0.75, 1.0], Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

sensor_unknown_returns_zeros_test() ->
    Opts = (simple_sensor_opts())#{
        sensor_name => unknown_sensor,
        vector_length => 2,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual([0.0, 0.0], Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

%% =============================================================================
%% Multiple Cycles Tests
%% =============================================================================

multiple_sync_cycles_test() ->
    Opts = (simple_sensor_opts())#{
        sensor_name => ones,
        vector_length => 1,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    %% Multiple sync cycles
    lists:foreach(fun(_) ->
        SensorPid ! {cortex, sync},
        receive
            {forward, SensorPid, [1.0]} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, [1, 2, 3]),

    SensorPid ! {cortex, terminate}.

%% =============================================================================
%% Scape Tests
%% =============================================================================

scape_signal_test() ->
    Opts = (simple_sensor_opts())#{
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    %% Send signal from scape
    SensorPid ! {scape, [0.5, 0.6, 0.7]},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual([0.5, 0.6, 0.7], Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

scape_request_test() ->
    %% Create mock scape
    Scape = spawn(fun() ->
        receive
            {From, sense, _SensorName, _Params} ->
                From ! {self(), sensory_signal, [1.0, 2.0, 3.0]}
        end
    end),

    Opts = (simple_sensor_opts())#{
        scape_pid => Scape,
        vector_length => 3,
        fanout_pids => [self()]
    },

    {ok, SensorPid} = sensor:start_link(Opts),

    SensorPid ! {cortex, sync},

    receive
        {forward, SensorPid, Signal} ->
            ?assertEqual([1.0, 2.0, 3.0], Signal)
    after 1000 ->
        ?assert(false)
    end,

    SensorPid ! {cortex, terminate}.

%% =============================================================================
%% Terminate Tests
%% =============================================================================

terminate_test() ->
    {ok, SensorPid} = sensor:start_link(simple_sensor_opts()),

    ?assert(is_process_alive(SensorPid)),

    SensorPid ! {cortex, terminate},
    timer:sleep(50),

    ?assertNot(is_process_alive(SensorPid)).
