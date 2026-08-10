%%% @doc The two CfC implementations must agree.
%%%
%%% They did not. faber-tweann carried THREE implementations of the CfC update,
%%% disagreeing by up to 0.36 on the same inputs: ltc_dynamics used by the
%%% process phenotype, the native NIF, and this fallback, which discarded the
%%% time constant entirely. network_evaluator:evaluate_with_state/2 routes CfC
%%% through tweann_nif:evaluate_cfc/4, so a CfC network computed a different
%%% function depending on whether the native library had loaded, silently.
%%%
%%% The native implementation is the reference. This holds the fallback to it.
-module(cfc_reference_tests).

-include_lib("eunit/include/eunit.hrl").

%% Deliberately spans the extremes, because that is where they diverged. A tau
%% at the native floor of 0.001 with an ordinary input drives the backbone to
%% the thousands, which overflowed the fallback's sigmoid before it was made
%% stable, and reaches the native sigmoid's clamp.
cases() ->
    [{I, S, T, B}
     || I <- [0.0, 0.5, -1.2, 2.0, -5.0, 12.0],
        S <- [0.0, 0.3, -0.7, 0.99],
        T <- [0.001, 0.1, 0.5, 1.5, 10.0],
        B <- [1.0, 2.0]].

the_fallback_matches_the_native_reference_test() ->
    Diffs = [begin
                 {NS1, O1} = faber_nn_nifs:evaluate_cfc(I, S, T, B),
                 {NS2, O2} = tweann_nif_fallback:evaluate_cfc(I, S, T, B),
                 max(abs(NS1 - NS2), abs(O1 - O2))
             end || {I, S, T, B} <- cases()],
    ?assert(length(Diffs) > 100),
    ?assert(lists:max(Diffs) < 1.0e-12).

%% The specific defect: tau was bound as _Tau and never read, so a liquid
%% TIME-CONSTANT neuron's time constant did nothing on this path.
the_fallback_actually_reads_tau_test() ->
    {A, _} = tweann_nif_fallback:evaluate_cfc(1.0, 0.5, 0.2, 1.0),
    {B, _} = tweann_nif_fallback:evaluate_cfc(1.0, 0.5, 5.0, 1.0),
    ?assertNotEqual(A, B).

%% The output is the state on both sides. The fallback used to return
%% tanh(state) instead, so even its shape differed.
the_output_is_the_state_on_both_sides_test() ->
    [begin
         {NS1, O1} = faber_nn_nifs:evaluate_cfc(I, S, T, B),
         {NS2, O2} = tweann_nif_fallback:evaluate_cfc(I, S, T, B),
         ?assertEqual(NS1, O1),
         ?assertEqual(NS2, O2)
     end || {I, S, T, B} <- lists:sublist(cases(), 20)],
    ok.

%% A backbone of input/tau reaches the thousands at a small tau, where the naive
%% sigmoid raises on this VM and the native one returns a value.
the_fallback_survives_an_extreme_backbone_test() ->
    ?assertMatch({_, _}, tweann_nif_fallback:evaluate_cfc(2.0, -0.7, 0.001, 2.0)),
    ?assertMatch({_, _}, tweann_nif_fallback:evaluate_cfc(-2.0, 0.7, 0.001, 2.0)).
