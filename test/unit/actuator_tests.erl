-module(actuator_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper to create a simple actuator
simple_actuator_opts() ->
    #{
        id => test_actuator,
        cortex_pid => self(),
        actuator_name => pts,
        vector_length => 1,
        fanin_pids => [],
        scape_pid => undefined,
        parameters => []
    }.

%% =============================================================================
%% Start/Init Tests
%% =============================================================================

start_link_test() ->
    {ok, Pid} = actuator:start_link(simple_actuator_opts()),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

start_with_custom_actuator_test() ->
    Opts = (simple_actuator_opts())#{actuator_name => identity},
    {ok, Pid} = actuator:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

start_with_fanin_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),
    Opts = (simple_actuator_opts())#{fanin_pids => [Sender]},
    {ok, Pid} = actuator:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate},
    Sender ! done.

%% =============================================================================
%% Forward/Output Tests
%% =============================================================================

single_input_forward_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        fanin_pids => [Sender]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [0.5]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([0.5], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

multiple_inputs_forward_test() ->
    Sender1 = spawn(fun() -> receive _ -> ok end end),
    Sender2 = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        fanin_pids => [Sender1, Sender2]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    %% First input - should not complete yet
    ActuatorPid ! {forward, Sender1, [1.0]},

    receive
        {actuator_output, _, _} ->
            ?assert(false)
    after 100 ->
        ok
    end,

    %% Second input - now should complete
    ActuatorPid ! {forward, Sender2, [2.0]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([1.0, 2.0], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender1 ! done,
    Sender2 ! done.

vector_input_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        fanin_pids => [Sender],
        vector_length => 3
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [1.0, 2.0, 3.0]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([1.0, 2.0, 3.0], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

%% =============================================================================
%% Actuator Function Tests
%% =============================================================================

actuator_pts_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        actuator_name => pts,
        fanin_pids => [Sender]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [3.14]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([3.14], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

actuator_identity_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        actuator_name => identity,
        fanin_pids => [Sender]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [1.0, 2.0]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([1.0, 2.0], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

actuator_threshold_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        actuator_name => threshold,
        fanin_pids => [Sender],
        parameters => [{threshold, 0.5}]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [0.3, 0.7, 0.5]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([0.0, 1.0, 0.0], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

actuator_softmax_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        actuator_name => softmax,
        fanin_pids => [Sender]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [1.0, 2.0, 3.0]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            %% Should sum to 1.0
            Sum = lists:sum(Output),
            ?assert(abs(Sum - 1.0) < 0.0001),
            %% Larger inputs should have larger probabilities
            [P1, P2, P3] = Output,
            ?assert(P1 < P2),
            ?assert(P2 < P3)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

actuator_argmax_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        actuator_name => argmax,
        fanin_pids => [Sender]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [0.1, 0.9, 0.5]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([1.0], Output)  % Index 1 has max value
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

actuator_unknown_passthrough_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        actuator_name => unknown_actuator,
        fanin_pids => [Sender]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [1.0, 2.0]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([1.0, 2.0], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

%% =============================================================================
%% Multiple Cycles Tests
%% =============================================================================

multiple_forward_cycles_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_actuator_opts())#{
        fanin_pids => [Sender]
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    %% Multiple cycles
    lists:foreach(fun(I) ->
        ActuatorPid ! {forward, Sender, [float(I)]},
        receive
            {actuator_output, ActuatorPid, [V]} ->
                ?assertEqual(float(I), V)
        after 1000 ->
            ?assert(false)
        end
    end, [1, 2, 3]),

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

%% =============================================================================
%% Scape Tests
%% =============================================================================

scape_actuate_test() ->
    Sender = spawn(fun() -> receive _ -> ok end end),

    %% Create mock scape
    Scape = spawn(fun() ->
        receive
            {From, actuate, Input, _Params} ->
                %% Return modified result
                From ! {self(), result, [hd(Input) * 2]}
        end
    end),

    Opts = (simple_actuator_opts())#{
        actuator_name => scape,
        fanin_pids => [Sender],
        scape_pid => Scape
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),

    ActuatorPid ! {forward, Sender, [5.0]},

    receive
        {actuator_output, ActuatorPid, Output} ->
            ?assertEqual([10.0], Output)
    after 1000 ->
        ?assert(false)
    end,

    ActuatorPid ! {cortex, terminate},
    Sender ! done.

%% =============================================================================
%% Terminate Tests
%% =============================================================================

terminate_test() ->
    {ok, ActuatorPid} = actuator:start_link(simple_actuator_opts()),

    ?assert(is_process_alive(ActuatorPid)),

    ActuatorPid ! {cortex, terminate},
    timer:sleep(50),

    ?assertNot(is_process_alive(ActuatorPid)).

%% =============================================================================
%% Edge Cases
%% =============================================================================

empty_fanin_test() ->
    %% Actuator with no inputs should still work (0 expected inputs)
    Opts = (simple_actuator_opts())#{
        fanin_pids => []
    },

    {ok, ActuatorPid} = actuator:start_link(Opts),
    ?assert(is_process_alive(ActuatorPid)),
    ActuatorPid ! {cortex, terminate}.
