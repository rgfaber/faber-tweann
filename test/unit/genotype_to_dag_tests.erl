%%% @doc Tests for genotype to DAG conversion.
%%%
%%% The load-bearing test is the skip connection: a topology that
%%% genotype_to_network REFUSES, converting here and computing the right number.
%%% That is the whole reason this module exists.
-module(genotype_to_dag_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    application:ensure_all_started(faber_tweann),
    genotype:init_db().

teardown(_) ->
    genotype:reset_db().

genotype_to_dag_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun converts_a_plain_layered_net/0,
        fun converts_a_skip_connection_the_layered_path_refuses/0,
        fun orders_nodes_so_every_source_comes_first/0,
        fun compiles_and_evaluates_through_the_evaluator/0,
        fun refuses_a_cycle/0,
        fun refuses_an_ltc_neuron/0,
        fun a_cfc_neuron_matches_the_standalone_cfc/0,
        fun refuses_an_unknown_source/0,
        fun a_delay_emits_last_ticks_value/0,
        fun delays_chain_into_a_longer_delay/0,
        fun a_feedback_path_through_a_delay_is_not_a_cycle/0,
        fun refuses_a_cycle_with_no_delay_in_it/0,
        fun the_state_is_one_slot_per_organelle/0,
        fun refuses_a_state_of_the_wrong_length/0,
        fun both_implementations_agree_exactly_on_linear_feedback/0,
        fun both_implementations_agree_within_an_ulp_on_tanh_feedback/0,
        fun a_leaky_integrator_approaches_its_input/0,
        fun a_leaky_integrator_does_not_break_a_cycle/0,
        fun refuses_a_non_positive_time_constant/0,
        fun both_implementations_agree_on_a_mixed_state_vector/0
    ]}.

%%==============================================================================
%% Fixtures
%%==============================================================================

u() -> genotype:generate_UniqueId().
w(X) -> {X, 0.0, 0.0, []}.

%% Two inputs, two hidden, one output, all linear so the arithmetic is checkable
%% by hand. Hidden neurons are deliberately written into the cortex AFTER the
%% output neuron, so a converter that trusted cortex order would emit an
%% unsorted list and read zeros.
layered() ->
    S = {{-1.0, u()}, sensor},
    H1 = {{0.0, u()}, neuron},
    H2 = {{0.0, u()}, neuron},
    O = {{0.5, u()}, neuron},
    A = {{1.0, u()}, actuator},
    Cx = {{origin, u()}, cortex},
    Ag = {{origin, u()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = in, vl = 2, fanout_ids = [H1, H2]}),
    genotype:write(#neuron{id = H1, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(1.0), w(2.0)]}, {bias, [w(0.5)]}],
                           output_ids = [O]}),
    genotype:write(#neuron{id = H2, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(3.0), w(4.0)]}], output_ids = [O]}),
    genotype:write(#neuron{id = O, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{H1, [w(10.0)]}, {H2, [w(-1.0)]}, {bias, [w(0.25)]}],
                           output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = out, vl = 1, fanin_ids = [O]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [O, H1, H2],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {Ag, S, H1, H2, O}.

%%==============================================================================
%% Conversion
%%==============================================================================

%% h1 = 1*1 + 2*2 + 0.5 = 5.5 ; h2 = 3*1 + 4*2 = 11.0
%% o  = 10*5.5 - 1*11.0 + 0.25 = 44.25
converts_a_plain_layered_net() ->
    {Ag, _S, _H1, _H2, _O} = layered(),
    {ok, {Nodes, InputCount, Outputs}} = genotype_to_dag:nodes(Ag),
    ?assertEqual(2, InputCount),
    ?assertEqual(5, length(Nodes)),
    ?assertEqual(1, length(Outputs)),
    {ok, Net} = genotype_to_dag:compile(Ag),
    ?assertEqual([44.25], tweann_nif:evaluate(Net, [1.0, 2.0])).

%% The output neuron reaches PAST the hidden layer straight to the sensor.
%% genotype_to_network refuses exactly this, and it is the ordinary shape of an
%% evolved topology, so it is the reason this module exists.
converts_a_skip_connection_the_layered_path_refuses() ->
    {Ag, S, _H1, _H2, O} = layered(),
    N = genotype:dirty_read({neuron, O}),
    genotype:write(N#neuron{input_idps = [{S, [w(100.0), w(0.0)]} | N#neuron.input_idps]}),
    ?assertMatch({error, {not_layerable, {source_not_in_previous_layer, _, _}}},
                 network_evaluator:from_genotype(Ag)),
    {ok, Net} = genotype_to_dag:compile(Ag),
    %% 44.25 as before, plus 100 * the first input
    ?assertEqual([144.25], tweann_nif:evaluate(Net, [1.0, 2.0])).

%% Both evaluators walk the list once in order, so an unsorted list silently
%% reads zeros. The cortex above lists the output neuron first on purpose.
orders_nodes_so_every_source_comes_first() ->
    {Ag, _S, _H1, _H2, _O} = layered(),
    {ok, {Nodes, InputCount, _}} = genotype_to_dag:nodes(Ag),
    %% index equals position, one contiguous run
    ?assertEqual(lists:seq(0, length(Nodes) - 1), [I || {I, _, _, _, _} <- Nodes]),
    %% inputs first, and nothing else marked input
    ?assertEqual(InputCount, length([I || {I, input, _, _, _} <- Nodes, I < InputCount])),
    %% every source strictly earlier than its consumer
    ?assert(lists:all(fun({I, _, _, _, Conns}) ->
                          lists:all(fun({From, _}) -> From < I end, Conns)
                      end, Nodes)).

compiles_and_evaluates_through_the_evaluator() ->
    {Ag, _S, _H1, _H2, _O} = layered(),
    {ok, Net} = genotype_to_dag:compile(Ag),
    ?assertEqual([44.25], tweann_nif:evaluate(Net, [1.0, 2.0])),
    %% and the layered path agrees, on a genotype both can take
    {ok, Layered} = network_evaluator:from_genotype(Ag),
    ?assertEqual(network_evaluator:evaluate(Layered, [1.0, 2.0]),
                 tweann_nif:evaluate(Net, [1.0, 2.0])).

%%==============================================================================
%% Refusals
%%==============================================================================

%% A cycle has no topological order. Evaluated anyway it would read 0.0 for the
%% back edge and report a number, which is the failure this refuses.
refuses_a_cycle() ->
    {Ag, _S, H1, _H2, O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{input_idps = [{O, [w(1.0)]} | N#neuron.input_idps]}),
    ?assertMatch({error, {not_convertible, {cyclic, _}}}, genotype_to_dag:nodes(Ag)).

%% LTC proper is an Euler step needing a dt that no genotype field carries, so
%% converting one would mean inventing a value.
refuses_an_ltc_neuron() ->
    {Ag, _S, H1, _H2, _O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{neuron_type = ltc, time_constant = 1.0}),
    ?assertEqual({error, {not_convertible, {unsupported_neuron_type, ltc}}},
                 genotype_to_dag:nodes(Ag)).

%% A CfC neuron on this path must compute exactly what the standalone
%% evaluate_cfc/4 computes, or "CfC" would mean two things again depending on
%% how it was reached. That is the failure this whole line of work removed.
a_cfc_neuron_matches_the_standalone_cfc() ->
    S = {{-1.0, u()}, sensor},
    C = {{0.0, u()}, neuron},
    A = {{1.0, u()}, actuator},
    Cx = {{origin, u()}, cortex},
    Ag = {{origin, u()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = in, vl = 1, fanout_ids = [C]}),
    genotype:write(#neuron{id = C, cx_id = Cx, af = linear, aggr_f = dot_product,
                           neuron_type = cfc, time_constant = 1.3, state_bound = 1.0,
                           input_idps = [{S, [w(1.0)]}], output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = out, vl = 1, fanin_ids = [C]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [C],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {ok, Net} = genotype_to_dag:compile(Ag),
    Inputs = [0.7, -0.4, 1.1],
    {ByGraph, _} = lists:foldl(
        fun(X, {Acc, St}) ->
            {[Out], St2} = tweann_nif:evaluate_with_state(Net, [X], St),
            {[Out | Acc], St2}
        end, {[], []}, Inputs),
    {ByHand, _} = lists:foldl(
        fun(X, {Acc, St}) ->
            {NewSt, Out} = tweann_nif:evaluate_cfc(X, St, 1.3, 1.0),
            {[Out | Acc], NewSt}
        end, {[], 0.0}, Inputs),
    ?assertEqual(lists:reverse(ByHand), lists:reverse(ByGraph)).

refuses_an_unknown_source() ->
    {Ag, _S, H1, _H2, _O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    Ghost = {{0.0, u()}, neuron},
    genotype:write(N#neuron{input_idps = [{Ghost, [w(1.0)]} | N#neuron.input_idps]}),
    ?assertMatch({error, {not_convertible, {unknown_source, _, _}}},
                 genotype_to_dag:nodes(Ag)).

%%==============================================================================
%% The memory organelle
%%
%% Insight 018 compared three places to put memory on this engine and ranked
%% them none > wiring > neuron, with CfC last. Insight 023 dumped the wiring of
%% an actual evolved memory solver and found a pure linear chain, every neuron
%% with exactly one input: a delay line. This is that, as a thing the substrate
%% offers rather than something evolution has to grow out of ordinary neurons.
%%==============================================================================

%% One sensor straight through a delay to the output. Tick 1 emits the initial
%% zero, tick 2 emits what tick 1 captured, and so on: a unit delay.
a_delay_emits_last_ticks_value() ->
    S = {{-1.0, u()}, sensor},
    D = {{0.0, u()}, neuron},
    O = {{0.5, u()}, neuron},
    A = {{1.0, u()}, actuator},
    Cx = {{origin, u()}, cortex},
    Ag = {{origin, u()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = in, vl = 1, fanout_ids = [D]}),
    genotype:write(#neuron{id = D, cx_id = Cx, af = linear, aggr_f = dot_product,
                           neuron_type = delay,
                           input_idps = [{S, [w(1.0)]}], output_ids = [O]}),
    genotype:write(#neuron{id = O, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{D, [w(1.0)]}], output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = out, vl = 1, fanin_ids = [O]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [D, O],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {ok, Net} = genotype_to_dag:compile(Ag),
    {Out1, St1} = tweann_nif:evaluate_with_state(Net, [5.0], []),
    {Out2, St2} = tweann_nif:evaluate_with_state(Net, [7.0], St1),
    {Out3, _} = tweann_nif:evaluate_with_state(Net, [9.0], St2),
    ?assertEqual([0.0], Out1),
    ?assertEqual([5.0], Out2),
    ?assertEqual([7.0], Out3).

%% Two delays in series give a delay of two, which is the chain insight 023
%% found evolution building for the T-maze.
delays_chain_into_a_longer_delay() ->
    S = {{-1.0, u()}, sensor},
    D1 = {{0.0, u()}, neuron},
    D2 = {{0.25, u()}, neuron},
    O = {{0.5, u()}, neuron},
    A = {{1.0, u()}, actuator},
    Cx = {{origin, u()}, cortex},
    Ag = {{origin, u()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = in, vl = 1, fanout_ids = [D1]}),
    genotype:write(#neuron{id = D1, cx_id = Cx, af = linear, aggr_f = dot_product,
                           neuron_type = delay,
                           input_idps = [{S, [w(1.0)]}], output_ids = [D2]}),
    genotype:write(#neuron{id = D2, cx_id = Cx, af = linear, aggr_f = dot_product,
                           neuron_type = delay,
                           input_idps = [{D1, [w(1.0)]}], output_ids = [O]}),
    genotype:write(#neuron{id = O, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{D2, [w(1.0)]}], output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = out, vl = 1, fanin_ids = [O]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [D1, D2, O],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {ok, Net} = genotype_to_dag:compile(Ag),
    Fed = lists:foldl(
        fun(X, {Acc, St}) ->
            {Out, St2} = tweann_nif:evaluate_with_state(Net, [X], St),
            {[Out | Acc], St2}
        end,
        {[], []},
        [1.0, 2.0, 3.0, 4.0, 5.0]),
    {Outs, _} = Fed,
    ?assertEqual([[0.0], [0.0], [1.0], [2.0], [3.0]], lists:reverse(Outs)).

%% ⚠ THE POINT. A neuron feeding a delay that feeds back into that neuron is a
%% loop in the genotype, and without an organelle it is refused as cyclic. The
%% delay reads a tick late, so it contributes no ordering constraint and the
%% whole thing sorts. This is recurrence without a cycle.
a_feedback_path_through_a_delay_is_not_a_cycle() ->
    S = {{-1.0, u()}, sensor},
    N = {{0.0, u()}, neuron},
    D = {{0.25, u()}, neuron},
    A = {{1.0, u()}, actuator},
    Cx = {{origin, u()}, cortex},
    Ag = {{origin, u()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = in, vl = 1, fanout_ids = [N]}),
    %% N sums the input and its own previous output, held by D.
    genotype:write(#neuron{id = N, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(1.0)]}, {D, [w(1.0)]}],
                           output_ids = [D, A]}),
    genotype:write(#neuron{id = D, cx_id = Cx, af = linear, aggr_f = dot_product,
                           neuron_type = delay,
                           input_idps = [{N, [w(1.0)]}], output_ids = [N]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = out, vl = 1, fanin_ids = [N]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [N, D],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {ok, Net} = genotype_to_dag:compile(Ag),
    %% A running sum: 1, 1+1, 2+1, 3+1 ...
    {O1, S1} = tweann_nif:evaluate_with_state(Net, [1.0], []),
    {O2, S2} = tweann_nif:evaluate_with_state(Net, [1.0], S1),
    {O3, S3} = tweann_nif:evaluate_with_state(Net, [1.0], S2),
    {O4, _} = tweann_nif:evaluate_with_state(Net, [1.0], S3),
    ?assertEqual([[1.0], [2.0], [3.0], [4.0]], [O1, O2, O3, O4]).

%% A loop with no organelle in it genuinely has no order and is still refused.
refuses_a_cycle_with_no_delay_in_it() ->
    {Ag, _S, H1, _H2, O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{input_idps = [{O, [w(1.0)]} | N#neuron.input_idps]}),
    ?assertMatch({error, {not_convertible, {cyclic, _}}}, genotype_to_dag:nodes(Ag)).

%% The state vector is one float per organelle, not one per neuron, and its
%% length is discoverable by evaluating once from empty.
the_state_is_one_slot_per_organelle() ->
    {Ag, _, _, _, _} = layered(),
    {ok, Plain} = genotype_to_dag:compile(Ag),
    {_, NoState} = tweann_nif:evaluate_with_state(Plain, [1.0, 2.0], []),
    ?assertEqual([], NoState).

%% A state of the wrong length is refused rather than padded, on both paths.
refuses_a_state_of_the_wrong_length() ->
    {Ag, _, _, _, _} = layered(),
    {ok, Net} = genotype_to_dag:compile(Ag),
    ?assertEqual({[], []}, tweann_nif:evaluate_with_state(Net, [1.0, 2.0], [0.0, 0.0])).

%% ⚠ THE GUARD THAT MATTERS, and it comes in two halves because two different
%% things can go wrong and they need different assertions.
%%
%% Today's session found three places where the native and Erlang evaluators
%% computed different things and nothing said so: the node index convention, the
%% out-of-range source, and ten of the seventeen activations. The organelle is a
%% fourth opportunity, so both implementations are driven from one node list and
%% compared rather than trusted.
%%
%% EXACT half: linear activations only, so every value is representable and the
%% arithmetic is exact. Any difference at all is a logic difference, in the three
%% passes, the state layout or the ordering. Asserted to the bit.
%%
%% TOLERANT half: a transcendental, where Rust's f64::tanh and Erlang's libm
%% math:tanh legitimately differ by about a unit in the last place. This half
%% catches a dispatch or wiring difference and deliberately does not chase the
%% last bit; hecate-dronex reached the same conclusion and wrote it down as a
%% decision rather than a defect. Asserting equality here is what the first
%% version of this test did, and it failed on ...62 against ...61.

%% Exact. A feedback loop, so any disagreement compounds instead of cancelling.
both_implementations_agree_exactly_on_linear_feedback() ->
    Nodes = [{0, input, linear, 0.0, []},
             {1, delay, linear, 0.0, [{2, 1.0}]},
             {2, neuron, linear, 0.5, [{0, 1.0}, {1, 0.5}]}],
    ?assertEqual(trace(fun faber_nn_nifs:compile_network/3,
                       fun faber_nn_nifs:evaluate_with_state/3, Nodes),
                 trace(fun tweann_nif_fallback:compile_network/3,
                       fun tweann_nif_fallback:evaluate_with_state/3, Nodes)).

%% Tolerant. Same loop shape, transcendental activation.
both_implementations_agree_within_an_ulp_on_tanh_feedback() ->
    Nodes = [{0, input, linear, 0.0, []},
             {1, delay, linear, 0.0, [{2, 1.0}]},
             {2, neuron, tanh, 0.1, [{0, 0.7}, {1, 0.4}]}],
    Native = trace(fun faber_nn_nifs:compile_network/3,
                   fun faber_nn_nifs:evaluate_with_state/3, Nodes),
    Erlang = trace(fun tweann_nif_fallback:compile_network/3,
                   fun tweann_nif_fallback:evaluate_with_state/3, Nodes),
    ?assertEqual(length(Native), length(Erlang)),
    ?assertNotEqual([], Native),
    lists:foreach(
        fun({{[A], [SA]}, {[B], [SB]}}) ->
            ?assert(abs(A - B) < 1.0e-12),
            ?assert(abs(SA - SB) < 1.0e-12)
        end,
        lists:zip(Native, Erlang)).

%% Drive one node list through one implementation for five ticks, carrying the
%% state, and return the whole trace of outputs and states.
trace(Compile, Eval, Nodes) ->
    Compiled = Compile(Nodes, 1, [2]),
    {Trace, _} = lists:foldl(
        fun(X, {Acc, St}) ->
            {Out, St2} = Eval(Compiled, [X], St),
            {[{Out, St2} | Acc], St2}
        end,
        {[], []},
        [1.0, -0.5, 0.25, 2.0, -1.0]),
    lists:reverse(Trace).

%%==============================================================================
%% The leaky integrator, the second organelle
%%
%% Where a delay holds a value for exactly one tick, a leaky integrator moves
%% its state toward its input by one part in tau each tick. It READS this tick's
%% inputs, so unlike a delay it is ordered normally and does not break a cycle.
%%==============================================================================

%% tau = 2 halves the remaining distance each tick, from a standing start:
%% 0 -> 0.5 -> 0.75 -> 0.875 for a constant input of 1.0.
a_leaky_integrator_approaches_its_input() ->
    S = {{-1.0, u()}, sensor},
    L = {{0.0, u()}, neuron},
    A = {{1.0, u()}, actuator},
    Cx = {{origin, u()}, cortex},
    Ag = {{origin, u()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = in, vl = 1, fanout_ids = [L]}),
    genotype:write(#neuron{id = L, cx_id = Cx, af = linear, aggr_f = dot_product,
                           neuron_type = leaky, time_constant = 2.0,
                           input_idps = [{S, [w(1.0)]}], output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = out, vl = 1, fanin_ids = [L]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [L],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {ok, Net} = genotype_to_dag:compile(Ag),
    {O1, St1} = tweann_nif:evaluate_with_state(Net, [1.0], []),
    {O2, St2} = tweann_nif:evaluate_with_state(Net, [1.0], St1),
    {O3, _} = tweann_nif:evaluate_with_state(Net, [1.0], St2),
    ?assertEqual([[0.5], [0.75], [0.875]], [O1, O2, O3]).

%% The distinction that matters between the two organelles. A delay adds no
%% ordering constraint, so a loop through one converts. A leaky integrator reads
%% this tick's inputs, so a loop through one is a genuine cycle and is refused.
a_leaky_integrator_does_not_break_a_cycle() ->
    {Ag, _S, H1, _H2, O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{input_idps = [{O, [w(1.0)]} | N#neuron.input_idps]}),
    Out = genotype:dirty_read({neuron, O}),
    genotype:write(Out#neuron{neuron_type = leaky, time_constant = 2.0}),
    ?assertMatch({error, {not_convertible, {cyclic, _}}}, genotype_to_dag:nodes(Ag)).

%% The update divides by tau, so zero or less cannot be evaluated. Refused with
%% a reason here rather than surfacing as a bad argument out of a NIF.
refuses_a_non_positive_time_constant() ->
    {Ag, _S, H1, _H2, _O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{neuron_type = leaky, time_constant = 0.0}),
    ?assertMatch({error, {not_convertible, {non_positive_time_constant, _}}},
                 genotype_to_dag:nodes(Ag)).

%% Both organelles at once, sharing one state vector, and both implementations
%% agreeing on it tick by tick. Linear throughout so the arithmetic is exact and
%% any difference is a logic difference.
both_implementations_agree_on_a_mixed_state_vector() ->
    Nodes = [{0, input, linear, 0.0, []},
             {1, delay, linear, 0.0, [{3, 1.0}]},
             {2, {leaky, 2.0}, linear, 0.0, [{0, 1.0}]},
             {3, neuron, linear, 0.0, [{0, 0.5}, {1, 0.25}, {2, 0.5}]}],
    Run = fun(Compile, Eval) ->
        C = Compile(Nodes, 1, [3]),
        {T, _} = lists:foldl(
            fun(X, {Acc, St}) ->
                {Out, St2} = Eval(C, [X], St),
                {[{Out, St2} | Acc], St2}
            end, {[], []}, [1.0, 1.0, 2.0, -1.0, 0.5]),
        lists:reverse(T)
    end,
    Native = Run(fun faber_nn_nifs:compile_network/3, fun faber_nn_nifs:evaluate_with_state/3),
    Erlang = Run(fun tweann_nif_fallback:compile_network/3, fun tweann_nif_fallback:evaluate_with_state/3),
    %% Two slots, one per organelle, in node-index order.
    [{_, FirstState} | _] = Native,
    ?assertEqual(2, length(FirstState)),
    ?assertEqual(Native, Erlang).
