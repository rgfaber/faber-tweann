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

%% Per-connection ABCD ({pc, CoeffLayers, Eta}): a rule with a distinct {A,B,C,D}
%% per synapse moves weights, and connections with different coefficients move by
%% different amounts (that per-synapse freedom is the whole point of the variant).
per_connection_rule_moves_weights_per_synapse_test() ->
    rand:seed(exsss, {31, 7, 7}),
    Net0 = network_evaluator:create_feedforward(2, [3], 1, tanh, tanh),
    W0 = network_evaluator:get_weights(Net0),
    %% Layer 1: 3 neurons x 2 conns; neuron 1 learns (A=1), neurons 2,3 are inert.
    L1 = [[{1.0, 0.0, 0.0, 0.0}, {1.0, 0.0, 0.0, 0.0}],
          [{0.0, 0.0, 0.0, 0.0}, {0.0, 0.0, 0.0, 0.0}],
          [{0.0, 0.0, 0.0, 0.0}, {0.0, 0.0, 0.0, 0.0}]],
    %% Layer 2: 1 neuron x 3 conns, all inert.
    L2 = [[{0.0, 0.0, 0.0, 0.0}, {0.0, 0.0, 0.0, 0.0}, {0.0, 0.0, 0.0, 0.0}]],
    {_Out, Net1} = network_evaluator:evaluate_with_plasticity(
                     Net0, [1.0, 1.0], {pc, [L1, L2], 0.3}),
    W1 = network_evaluator:get_weights(Net1),
    ?assert(W0 =/= W1),
    %% Neuron 1's two input weights moved; the other input weights held exactly.
    Moved = lists:sublist(W1, 2),
    Held = lists:sublist(W1, 3, 4),
    Moved0 = lists:sublist(W0, 2),
    Held0 = lists:sublist(W0, 3, 4),
    ?assert(Moved =/= Moved0),
    ?assertEqual(Held0, Held).

%% A per-connection rule of all-zero coefficients is inert: weights are unchanged
%% (the memoryless special case), confirming the plasticity is fully rule-driven.
per_connection_zero_rule_is_inert_test() ->
    rand:seed(exsss, {31, 8, 8}),
    Net0 = network_evaluator:create_feedforward(2, [3], 1, tanh, tanh),
    W0 = network_evaluator:get_weights(Net0),
    Zero = {0.0, 0.0, 0.0, 0.0},
    L1 = [[Zero, Zero], [Zero, Zero], [Zero, Zero]],
    L2 = [[Zero, Zero, Zero]],
    {_Out, Net1} = network_evaluator:evaluate_with_plasticity(
                     Net0, [1.0, 1.0], {pc, [L1, L2], 0.5}),
    ?assertEqual(W0, network_evaluator:get_weights(Net1)).

mem_after_cue(Net0, Init, Cue) ->
    {_Out, Net1} = network_evaluator:evaluate_with_plasticity(
                     network_evaluator:set_weights(Net0, Init), [Cue, 0.0], ?RULE),
    W = network_evaluator:get_weights(Net1),
    math:sqrt(lists:sum([(A - B) * (A - B) || {A, B} <- lists:zip(W, Init)])).
