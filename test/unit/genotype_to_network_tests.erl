%%% @doc Tests for genotype to network conversion (ROADMAP 8a).
%%%
%%% The load-bearing test is equivalence: the converted network must compute
%%% what the genotype's own weights say it should, on real inputs. Everything
%%% else is a refusal, and refusals matter here more than usual, because the
%%% function being replaced answered every one of them with a cheerful ok and a
%%% network full of random numbers.
-module(genotype_to_network_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init().

teardown(_) ->
    genotype:reset_db().

genotype_to_network_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun carries_the_weights_rather_than_inventing_them/0,
        fun computes_what_the_genotype_computes/0,
        fun a_missing_connection_is_a_zero_not_a_guess/0,
        fun refuses_recurrence/0,
        fun refuses_an_activation_the_evaluator_would_silently_replace/0,
        fun refuses_mixed_activations/0,
        fun refuses_an_ltc_neuron/0,
        fun refuses_a_connection_that_skips_a_layer/0,
        fun refuses_a_missing_agent/0,
        fun the_supported_activation_list_matches_the_evaluator/0,
        fun layer_order_follows_the_cortex_not_the_random_ids/0
    ]}.

%%==============================================================================
%% Helpers: a hand-built two-layer genotype, so the expected arithmetic is
%% known independently of anything the engine did.
%%==============================================================================

sid() -> {{-1.0, genotype:generate_UniqueId()}, sensor}.
nid(L) -> {{L, genotype:generate_UniqueId()}, neuron}.
aid() -> {{1.0, genotype:generate_UniqueId()}, actuator}.
cid() -> {{origin, genotype:generate_UniqueId()}, cortex}.
agid() -> {{origin, genotype:generate_UniqueId()}, agent}.

w(X) -> {X, 0.0, 0.0, []}.

%% Two inputs, two hidden (layer 0.0), one output (layer 0.5), all linear so the
%% expected value is plain arithmetic rather than a transcendental.
a_small_genotype() ->
    S = sid(),
    [H1, H2] = [nid(0.0), nid(0.0)],
    O = nid(0.5),
    A = aid(),
    Cx = cid(),
    Ag = agid(),
    genotype:write(#sensor{id = S, cx_id = Cx, name = test_in, vl = 2, fanout_ids = [H1, H2]}),
    genotype:write(#neuron{id = H1, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(1.0), w(2.0)]}, {bias, [w(0.5)]}],
                           output_ids = [O]}),
    genotype:write(#neuron{id = H2, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(3.0), w(4.0)]}],
                           output_ids = [O]}),
    genotype:write(#neuron{id = O, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{H1, [w(10.0)]}, {H2, [w(-1.0)]}, {bias, [w(0.25)]}],
                           output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = test_out, vl = 1, fanin_ids = [O]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [H1, H2, O],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {Ag, S, H1, H2, O}.

%%==============================================================================
%% Equivalence
%%==============================================================================

%% The specific regression 8a is about. The old implementation produced a
%% correctly shaped network of random numbers, so a shape assertion passes on a
%% broken conversion and only the values catch it.
carries_the_weights_rather_than_inventing_them() ->
    {Ag, _S, _H1, _H2, _O} = a_small_genotype(),
    {ok, Net} = network_evaluator:from_genotype(Ag),
    %% layer 1: 2 rows of 2 weights then 2 biases; layer 2: 1 row of 2 then 1 bias
    ?assertEqual([1.0, 2.0, 3.0, 4.0, 0.5, 0.0, 10.0, -1.0, 0.25],
                 network_evaluator:get_weights(Net)).

%% Worked by hand from the genotype above, with linear activations throughout.
%%   h1 = 1*1 + 2*2 + 0.5   = 5.5
%%   h2 = 3*1 + 4*2 + 0.0   = 11.0
%%   o  = 10*5.5 - 1*11.0 + 0.25 = 44.25
computes_what_the_genotype_computes() ->
    {Ag, _S, _H1, _H2, _O} = a_small_genotype(),
    {ok, Net} = network_evaluator:from_genotype(Ag),
    ?assertEqual([44.25], network_evaluator:evaluate(Net, [1.0, 2.0])).

%% A sparse genotype is expressed densely with zeros, which computes the same
%% function exactly. It is the one thing here that is filled in rather than
%% carried, and it is exact rather than approximate.
a_missing_connection_is_a_zero_not_a_guess() ->
    S = sid(),
    H = nid(0.0),
    O = nid(0.5),
    A = aid(),
    Cx = cid(),
    Ag = agid(),
    genotype:write(#sensor{id = S, cx_id = Cx, name = test_in, vl = 2, fanout_ids = [H]}),
    %% Connected to the FIRST input slot only. The second column must be 0.0.
    genotype:write(#neuron{id = H, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(1.0), w(0.0)]}], output_ids = [O]}),
    genotype:write(#neuron{id = O, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{H, [w(1.0)]}], output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = test_out, vl = 1, fanin_ids = [O]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [H, O],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {ok, Net} = network_evaluator:from_genotype(Ag),
    ?assertEqual([7.0], network_evaluator:evaluate(Net, [7.0, 999.0])).

%%==============================================================================
%% Refusals
%%==============================================================================

refuses_recurrence() ->
    {Ag, _S, H1, _H2, O} = a_small_genotype(),
    N = genotype:dirty_read({neuron, O}),
    genotype:write(N#neuron{ro_ids = [H1]}),
    ?assertEqual({error, {not_layerable, recurrent}}, network_evaluator:from_genotype(Ag)).

%% network_evaluator's private apply_activation/2 ends in a catch-all returning
%% math:tanh/1, so an unsupported activation would convert into a network that
%% computes a different function and says nothing. This is the refusal that
%% keeps that from happening.
refuses_an_activation_the_evaluator_would_silently_replace() ->
    {Ag, _S, H1, H2, O} = a_small_genotype(),
    [begin
         N = genotype:dirty_read({neuron, Id}),
         genotype:write(N#neuron{af = gaussian})
     end || Id <- [H1, H2, O]],
    ?assertEqual({error, {not_layerable, {unsupported_activation, gaussian}}},
                 network_evaluator:from_genotype(Ag)).

refuses_mixed_activations() ->
    {Ag, _S, H1, _H2, _O} = a_small_genotype(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{af = relu}),
    ?assertMatch({error, {not_layerable, {mixed_activations, hidden, _}}},
                 network_evaluator:from_genotype(Ag)).

refuses_an_ltc_neuron() ->
    {Ag, _S, H1, _H2, _O} = a_small_genotype(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{neuron_type = ltc}),
    ?assertEqual({error, {not_layerable, {unsupported_neuron_type, ltc}}},
                 network_evaluator:from_genotype(Ag)).

refuses_a_connection_that_skips_a_layer() ->
    {Ag, S, _H1, _H2, O} = a_small_genotype(),
    N = genotype:dirty_read({neuron, O}),
    %% The output neuron reaching back past the hidden layer to the sensor.
    genotype:write(N#neuron{input_idps = [{S, [w(1.0), w(1.0)]} | N#neuron.input_idps]}),
    ?assertMatch({error, {not_layerable, {source_not_in_previous_layer, _, _}}},
                 network_evaluator:from_genotype(Ag)).

refuses_a_missing_agent() ->
    ?assertMatch({error, {missing, agent, _}},
                 network_evaluator:from_genotype({{origin, 0.123}, agent})).

%% If the evaluator ever learns a new activation, this fails and the list is
%% updated deliberately rather than drifting until the catch-all hides it again.
the_supported_activation_list_matches_the_evaluator() ->
    ?assertEqual([linear, relu, sigmoid, tanh],
                 lists:sort(genotype_to_network:supported_activations())).

%% Swapping the two hidden neurons in the cortex's list must swap their rows,
%% and must swap the next layer's columns to match, so the function is unchanged
%% while the layout follows what the genome declares. Sorting the records
%% instead would order by the random float inside each id, and the vector would
%% depend on when the neurons happened to be minted.
layer_order_follows_the_cortex_not_the_random_ids() ->
    {Ag, _S, H1, H2, O} = a_small_genotype(),
    {ok, A} = network_evaluator:from_genotype(Ag),
    ?assertEqual([1.0, 2.0, 3.0, 4.0, 0.5, 0.0, 10.0, -1.0, 0.25],
                 network_evaluator:get_weights(A)),
    Cx = genotype:dirty_read({cortex, (genotype:dirty_read({agent, Ag}))#agent.cx_id}),
    genotype:write(Cx#cortex{neuron_ids = [H2, H1, O]}),
    {ok, B} = network_evaluator:from_genotype(Ag),
    ?assertEqual([3.0, 4.0, 1.0, 2.0, 0.0, 0.5, -1.0, 10.0, 0.25],
                 network_evaluator:get_weights(B)),
    %% Same function, different layout. That is the whole point.
    ?assertEqual(network_evaluator:evaluate(A, [1.0, 2.0]),
                 network_evaluator:evaluate(B, [1.0, 2.0])).
