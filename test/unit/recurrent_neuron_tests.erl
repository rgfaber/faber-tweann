%% @doc Recurrent-network evaluation: first-cycle seeding of feedback inputs.
%%
%% A recurrent (feedback) edge points from a higher layer to the same or a lower
%% layer, so its target fires BEFORE the source each cycle and would deadlock on
%% cycle 0 waiting for a signal the source has not produced. The fix (faithful to
%% DXNN2 neuron:prep/1) seeds each recurrent target with a default [0.0] when the
%% source learns its recurrent outputs, so the target has all its inputs on
%% cycle 0. From cycle 1 the real feedback (the previous cycle's output) flows.
%%
%% These tests drive neuron processes directly, no genotype or cortex, so the
%% mechanism is proven in isolation.
-module(recurrent_neuron_tests).
-include_lib("eunit/include/eunit.hrl").

opts() ->
    #{
        id => test_neuron,
        cortex_pid => self(),
        activation_function => linear,
        aggregation_function => dot_product,
        input_pids => [],
        output_pids => [],
        ro_pids => [],
        input_weights => #{},
        bias => 0.0
    }.

%% Linking a recurrent-output target fires a [0.0] seed to it immediately.
recurrent_seed_on_link_test() ->
    Self = self(),
    Receiver = spawn(fun() ->
        receive {forward, _From, Signal} -> Self ! {seeded, Signal} end
    end),
    {ok, NeuronPid} = neuron:start_link(opts()),
    NeuronPid ! {link, ro_pids, [Receiver]},
    receive
        {seeded, Signal} -> ?assertEqual([0.0], Signal)
    after 1000 -> ?assert(false)
    end.

%% A neuron with a SELF recurrent edge: it must not deadlock on cycle 0 (the
%% seed unblocks it), and its output must depend on its own previous output
%% (state carried across cycles). With linear activation and no bias the maths
%% is exact: out = 1.0*sensor + 0.5*prev_out.
self_recurrent_carries_state_test() ->
    Self = self(),
    %% A stand-in "sensor" pid, used only as the source tag on forward messages.
    Sensor = spawn(fun() -> receive stop -> ok end end),
    %% Collector relays the neuron's feedforward output back to the test.
    Collector = spawn(fun Loop() ->
        receive {forward, _From, Signal} -> Self ! {out, Signal}, Loop() end
    end),

    {ok, N} = neuron:start_link(opts()),
    W = fun(X) -> [{X, 0.0, 0.1, []}] end,
    %% Inputs: the sensor and the neuron itself. ro = itself (the feedback edge).
    N ! {link, input_pids, [Sensor, N]},
    N ! {link, ro_pids, [N]},
    N ! {link, output_pids, [Collector]},
    N ! {link, input_weights, #{Sensor => W(1.0), N => W(0.5)}},

    %% Cycle 1: sensor = 2.0. Feedback seed is 0.0, so out = 2.0.
    N ! {forward, Sensor, [2.0]},
    V1 = recv_out(),
    ?assert(close(V1, 2.0)),

    %% Cycle 2: sensor = 2.0 again. Feedback is now the previous output (2.0),
    %% so out = 1.0*2.0 + 0.5*2.0 = 3.0. Same input, different output: the
    %% network remembers. This is impossible for a feedforward net.
    N ! {forward, Sensor, [2.0]},
    V2 = recv_out(),
    ?assert(close(V2, 3.0)),

    Sensor ! stop.

recv_out() ->
    receive {out, [V]} -> V after 1000 -> erlang:error(no_output_deadlock) end.

close(A, B) -> abs(A - B) < 1.0e-9.
