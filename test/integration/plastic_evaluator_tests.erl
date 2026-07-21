%% @doc Unit tests for the plastic-neuron path (network_evaluator:
%% evaluate_with_plasticity/3), the memory-by-learning mechanism (Programme 3).
%%
%% Uses a demonstrative Hebbian rule (A=1, Eta=0.3) to show the mechanism: at a
%% cue the weights move from baseline (the cue is latched); during zero-input the
%% cue layer's update is exactly zero (held); different cues produce different
%% weight states (that divergence IS the stored memory).
-module(plastic_evaluator_tests).

-include_lib("eunit/include/eunit.hrl").

%% {A, B, C, D, Eta} — pure Hebbian pre*post at learning rate 0.3.
-define(RULE, {1.0, 0.0, 0.0, 0.0, 0.3}).

plasticity_moves_weights_test() ->
    rand:seed(exsss, {31, 1, 1}),
    Net0 = network_evaluator:create_feedforward(2, [6], 1, tanh, tanh),
    W0 = network_evaluator:get_weights(Net0),
    {_Out, Net1} = network_evaluator:evaluate_with_plasticity(Net0, [1.0, 0.0], ?RULE),
    ?assert(W0 =/= network_evaluator:get_weights(Net1)).

%% The cue-storing (input->hidden) layer holds exactly during zero input: its
%% pre-synaptic activity is the [0,0] input, so the Hebbian delta is zero. (The
%% output layer drifts slightly because ITS pre is the bias-driven hidden
%% activity, not zero -- a real property of the rule, not asserted here.)
cue_layer_holds_during_zero_input_test() ->
    rand:seed(exsss, {31, 3, 3}),
    Net0 = network_evaluator:create_feedforward(2, [6], 1, tanh, tanh),
    Init = network_evaluator:get_weights(Net0),
    InputLayer = 6 * 2,
    {_O1, N1} = network_evaluator:evaluate_with_plasticity(
                  network_evaluator:set_weights(Net0, Init), [1.0, 0.0], ?RULE),
    Wc = lists:sublist(network_evaluator:get_weights(N1), InputLayer),
    {_O2, N2} = network_evaluator:evaluate_with_plasticity(N1, [0.0, 0.0], ?RULE),
    Wh = lists:sublist(network_evaluator:get_weights(N2), InputLayer),
    ?assertEqual(Wc, Wh).

opposite_cues_store_different_memory_test() ->
    rand:seed(exsss, {31, 2, 2}),
    Net0 = network_evaluator:create_feedforward(2, [6], 1, tanh, tanh),
    Init = network_evaluator:get_weights(Net0),
    MemP = mem_after_cue(Net0, Init, 1.0),
    MemM = mem_after_cue(Net0, Init, -1.0),
    ?assert(abs(MemP - MemM) > 0.0).

%% Oja's rule also moves weights, and (being self-normalising) keeps them bounded.
oja_rule_moves_and_bounds_weights_test() ->
    rand:seed(exsss, {31, 9, 9}),
    Net0 = network_evaluator:create_feedforward(2, [6], 1, tanh, tanh),
    W0 = network_evaluator:get_weights(Net0),
    Net1 = lists:foldl(
             fun(_, N) ->
                 {_, N2} = network_evaluator:evaluate_with_plasticity(N, [1.0, 0.5], {oja, 0.2}),
                 N2
             end, Net0, lists:seq(1, 50)),
    W1 = network_evaluator:get_weights(Net1),
    ?assert(W0 =/= W1),
    ?assert(lists:all(fun(X) -> abs(X) =< 10.0 end, W1)).

mem_after_cue(Net0, Init, Cue) ->
    {_Out, Net1} = network_evaluator:evaluate_with_plasticity(
                     network_evaluator:set_weights(Net0, Init), [Cue, 0.0], ?RULE),
    W = network_evaluator:get_weights(Net1),
    math:sqrt(lists:sum([(A - B) * (A - B) || {A, B} <- lists:zip(W, Init)])).
