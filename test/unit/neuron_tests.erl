-module(neuron_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(CORTEX_PID, self()).

%% Helper to create a simple neuron
simple_neuron_opts() ->
    #{
        id => test_neuron,
        cortex_pid => ?CORTEX_PID,
        activation_function => tanh,
        aggregation_function => dot_product,
        input_pids => [],
        output_pids => [],
        ro_pids => [],
        input_weights => #{},
        bias => 0.0
    }.

%% =============================================================================
%% Start/Init Tests
%% =============================================================================

start_link_test() ->
    {ok, Pid} = neuron:start_link(simple_neuron_opts()),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

start_with_custom_activation_test() ->
    Opts = (simple_neuron_opts())#{activation_function => sigmoid},
    {ok, Pid} = neuron:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

start_with_custom_aggregation_test() ->
    Opts = (simple_neuron_opts())#{aggregation_function => mult_product},
    {ok, Pid} = neuron:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

start_with_bias_test() ->
    Opts = (simple_neuron_opts())#{bias => 0.5},
    {ok, Pid} = neuron:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

%% =============================================================================
%% Forward/Signal Processing Tests
%% =============================================================================

forward_single_input_test() ->
    %% Create a neuron that receives input from a fake sensor
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]},
        bias => 0.0
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    %% Send a signal
    neuron:forward(NeuronPid, SensorPid, [0.5]),

    %% Should receive output
    receive
        {forward, NeuronPid, [Output]} ->
            %% tanh(0.5 * 1.0 + 0.0) = tanh(0.5) ≈ 0.4621
            ?assert(abs(Output - 0.4621171572600098) < 0.0001)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

forward_multiple_inputs_test() ->
    %% Create fake sensors
    Sensor1 = spawn(fun() -> receive _ -> ok end end),
    Sensor2 = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        input_pids => [Sensor1, Sensor2],
        output_pids => [self()],
        input_weights => #{
            Sensor1 => [{0.5, 0.0, 0.1, []}],
            Sensor2 => [{0.5, 0.0, 0.1, []}]
        },
        bias => 0.0
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    %% Send signals from both sensors
    neuron:forward(NeuronPid, Sensor1, [1.0]),
    neuron:forward(NeuronPid, Sensor2, [1.0]),

    %% Should receive output after both inputs
    receive
        {forward, NeuronPid, [Output]} ->
            %% tanh(1.0*0.5 + 1.0*0.5) = tanh(1.0) ≈ 0.7616
            ?assert(abs(Output - 0.7615941559557649) < 0.0001)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    Sensor1 ! done,
    Sensor2 ! done.

forward_waits_for_all_inputs_test() ->
    %% Verify neuron waits for all inputs before processing
    Sensor1 = spawn(fun() -> receive _ -> ok end end),
    Sensor2 = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        input_pids => [Sensor1, Sensor2],
        output_pids => [self()],
        input_weights => #{
            Sensor1 => [{1.0, 0.0, 0.1, []}],
            Sensor2 => [{1.0, 0.0, 0.1, []}]
        }
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    %% Send only first signal
    neuron:forward(NeuronPid, Sensor1, [0.5]),

    %% Should NOT receive output yet
    receive
        {forward, NeuronPid, _} ->
            ?assert(false)
    after 100 ->
        ok
    end,

    %% Send second signal
    neuron:forward(NeuronPid, Sensor2, [0.5]),

    %% Now should receive output
    receive
        {forward, NeuronPid, [_Output]} ->
            ok
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    Sensor1 ! done,
    Sensor2 ! done.

forward_with_bias_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]},
        bias => 0.5
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:forward(NeuronPid, SensorPid, [0.5]),

    receive
        {forward, NeuronPid, [Output]} ->
            %% tanh(0.5 * 1.0 + 0.5) = tanh(1.0) ≈ 0.7616
            ?assert(abs(Output - 0.7615941559557649) < 0.0001)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

%% =============================================================================
%% Activation Function Tests
%% =============================================================================

activation_sigmoid_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        activation_function => sigmoid,
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]}
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:forward(NeuronPid, SensorPid, [0.0]),

    receive
        {forward, NeuronPid, [Output]} ->
            %% sigmoid(0) = 0.5
            ?assert(abs(Output - 0.5) < 0.0001)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

activation_relu_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        activation_function => relu,
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]}
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    %% Test positive input
    neuron:forward(NeuronPid, SensorPid, [2.0]),

    receive
        {forward, NeuronPid, [Output1]} ->
            ?assertEqual(2.0, Output1)
    after 1000 ->
        ?assert(false)
    end,

    %% Test negative input
    neuron:forward(NeuronPid, SensorPid, [-2.0]),

    receive
        {forward, NeuronPid, [Output2]} ->
            ?assertEqual(0.0, Output2)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

activation_linear_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        activation_function => linear,
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [{2.0, 0.0, 0.1, []}]},
        bias => 1.0
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:forward(NeuronPid, SensorPid, [3.0]),

    receive
        {forward, NeuronPid, [Output]} ->
            %% linear(3.0 * 2.0 + 1.0) = 7.0
            ?assertEqual(7.0, Output)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

%% =============================================================================
%% Aggregation Function Tests
%% =============================================================================

aggregation_mult_product_test() ->
    Sensor1 = spawn(fun() -> receive _ -> ok end end),
    Sensor2 = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        activation_function => linear,
        aggregation_function => mult_product,
        input_pids => [Sensor1, Sensor2],
        output_pids => [self()],
        input_weights => #{
            Sensor1 => [{2.0, 0.0, 0.1, []}],
            Sensor2 => [{3.0, 0.0, 0.1, []}]
        }
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:forward(NeuronPid, Sensor1, [1.0]),
    neuron:forward(NeuronPid, Sensor2, [1.0]),

    receive
        {forward, NeuronPid, [Output]} ->
            %% mult_product: (1.0*2.0) * (1.0*3.0) = 6.0
            ?assertEqual(6.0, Output)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    Sensor1 ! done,
    Sensor2 ! done.

%% =============================================================================
%% Output Routing Tests
%% =============================================================================

forward_to_multiple_outputs_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    %% Create two receiver processes
    Self = self(),
    Receiver1 = spawn(fun() ->
        receive {forward, _, Signal} -> Self ! {recv1, Signal} end
    end),
    Receiver2 = spawn(fun() ->
        receive {forward, _, Signal} -> Self ! {recv2, Signal} end
    end),

    Opts = (simple_neuron_opts())#{
        activation_function => linear,
        input_pids => [SensorPid],
        output_pids => [Receiver1, Receiver2],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]}
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:forward(NeuronPid, SensorPid, [5.0]),

    %% Both receivers should get the output
    receive {recv1, [5.0]} -> ok after 1000 -> ?assert(false) end,
    receive {recv2, [5.0]} -> ok after 1000 -> ?assert(false) end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

forward_to_recurrent_outputs_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    %% Recurrent output receiver
    Self = self(),
    RecurrentReceiver = spawn(fun() ->
        receive {forward, _, Signal} -> Self ! {recurrent, Signal} end
    end),

    Opts = (simple_neuron_opts())#{
        activation_function => linear,
        input_pids => [SensorPid],
        output_pids => [self()],
        ro_pids => [RecurrentReceiver],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]}
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:forward(NeuronPid, SensorPid, [3.0]),

    %% Regular output
    receive {forward, NeuronPid, [3.0]} -> ok after 1000 -> ?assert(false) end,

    %% Recurrent output
    receive {recurrent, [3.0]} -> ok after 1000 -> ?assert(false) end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

%% =============================================================================
%% Backup Tests
%% =============================================================================

backup_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    InputWeights = #{SensorPid => [{0.5, 0.1, 0.01, []}]},
    Bias = 0.3,

    Opts = (simple_neuron_opts())#{
        id => backup_test_neuron,
        input_pids => [SensorPid],
        input_weights => InputWeights,
        bias => Bias
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:backup(NeuronPid),

    receive
        {backup, backup_test_neuron, ReturnedWeights, ReturnedBias} ->
            ?assertEqual(InputWeights, ReturnedWeights),
            ?assertEqual(Bias, ReturnedBias)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

%% =============================================================================
%% Weight Update Tests
%% =============================================================================

update_weights_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        activation_function => linear,
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]},
        bias => 0.0
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    %% First forward with original weights
    neuron:forward(NeuronPid, SensorPid, [2.0]),
    receive {forward, NeuronPid, [2.0]} -> ok after 1000 -> ?assert(false) end,

    %% Update weights
    NewWeights = #{SensorPid => [{2.0, 0.0, 0.1, []}]},
    NewBias = 1.0,
    NeuronPid ! {update_weights, NewWeights, NewBias},

    %% Forward with new weights
    neuron:forward(NeuronPid, SensorPid, [2.0]),
    receive
        {forward, NeuronPid, [Output]} ->
            %% 2.0 * 2.0 + 1.0 = 5.0
            ?assertEqual(5.0, Output)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

%% =============================================================================
%% Multiple Cycles Tests
%% =============================================================================

multiple_forward_cycles_test() ->
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        activation_function => linear,
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [{1.0, 0.0, 0.1, []}]}
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    %% First cycle
    neuron:forward(NeuronPid, SensorPid, [1.0]),
    receive {forward, NeuronPid, [1.0]} -> ok after 1000 -> ?assert(false) end,

    %% Second cycle
    neuron:forward(NeuronPid, SensorPid, [2.0]),
    receive {forward, NeuronPid, [2.0]} -> ok after 1000 -> ?assert(false) end,

    %% Third cycle
    neuron:forward(NeuronPid, SensorPid, [3.0]),
    receive {forward, NeuronPid, [3.0]} -> ok after 1000 -> ?assert(false) end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.

%% =============================================================================
%% Edge Cases
%% =============================================================================

empty_input_list_test() ->
    %% Neuron with no inputs (useful for bias-only neurons)
    Opts = (simple_neuron_opts())#{
        input_pids => [],
        output_pids => [self()]
    },

    {ok, Pid} = neuron:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    Pid ! {cortex, terminate}.

vector_input_test() ->
    %% Test with multi-element input vector
    SensorPid = spawn(fun() -> receive _ -> ok end end),

    Opts = (simple_neuron_opts())#{
        activation_function => linear,
        input_pids => [SensorPid],
        output_pids => [self()],
        input_weights => #{SensorPid => [
            {1.0, 0.0, 0.1, []},
            {2.0, 0.0, 0.1, []},
            {3.0, 0.0, 0.1, []}
        ]}
    },

    {ok, NeuronPid} = neuron:start_link(Opts),

    neuron:forward(NeuronPid, SensorPid, [1.0, 1.0, 1.0]),

    receive
        {forward, NeuronPid, [Output]} ->
            %% 1.0*1.0 + 1.0*2.0 + 1.0*3.0 = 6.0
            ?assertEqual(6.0, Output)
    after 1000 ->
        ?assert(false)
    end,

    NeuronPid ! {cortex, terminate},
    SensorPid ! done.
