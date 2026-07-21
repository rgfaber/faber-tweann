%% @doc Validation for the batched CfC-population NIF (compile_cfc_pop/cfc_pop_step).
%%
%% (1) Numerical equivalence: N nets run through the per-individual path
%%     (network_evaluator:evaluate_with_state) and through the batched NIF
%%     (lockstep) must give IDENTICAL DPNV fitnesses (same trajectory -> same
%%     death step). (2) Speedup: time lambda=100 both ways.
%%
%% Invoke: rebar3 eunit --module=valcfcpop_tests
-module(valcfcpop_tests).

-include_lib("eunit/include/eunit.hrl").

-define(IN, 3).
-define(H, 12).
-define(OUT, 1).
-define(BOUND, 1.0).
-define(VARIANT, 3).
-define(ACT_PARAMS, [without_damping, 1, 1000]).
-define(MAX_STEPS, 1000).

equivalence_test_() ->
    {timeout, 120, fun equivalence/0}.

%% Per-net fitness from the batched path must equal the per-individual path.
equivalence() ->
    rand:seed(exsss, {99, 1, 1}),
    N = 6,
    Nw = layer_weight_count(),
    Pop = [{[rand:normal() || _ <- lists:seq(1, Nw)],
            [tau_of(rand:normal()) || _ <- lists:seq(1, ?H)]} || _ <- lists:seq(1, N)],
    Solo = [solo_fitness(W, T) || {W, T} <- Pop],
    Batched = batched_fitnesses(Pop),
    io:format("VALCFC equivalence: solo=~p batched=~p~n", [Solo, Batched]),
    ?assertEqual(Solo, Batched).

%%%============================================================================
%%% Per-individual path (network_evaluator)
%%%============================================================================

solo_fitness(Weights, Taus) ->
    Net0 = network_evaluator:create_cfc_feedforward(?IN, [?H], ?OUT, tanh, tanh),
    Net1 = network_evaluator:set_weights(Net0, Weights),
    Net2 = set_taus(Net1, Taus),
    solo_episode(network_evaluator:reset_internal_state(Net2), pb_sim:init([]), 0).

solo_episode(_Net, _S, Steps) when Steps >= ?MAX_STEPS -> Steps;
solo_episode(Net, S, Steps) ->
    {In, S1} = pb_sim:sense(v, [?VARIANT], S),
    {Out, Net1} = network_evaluator:evaluate_with_state(Net, In),
    {_F, Halt, S2} = pb_sim:act(v, ?ACT_PARAMS, Out, S1),
    case Halt of
        0 -> solo_episode(Net1, S2, Steps + 1);
        _ -> Steps
    end.

%%%============================================================================
%%% Batched path (lockstep over the population via the NIF)
%%%============================================================================

batched_fitnesses(Pop) ->
    Weights = [W || {W, _} <- Pop],
    Taus = [T || {_, T} <- Pop],
    Ref = tweann_nif:compile_cfc_pop(Weights, Taus, ?IN, ?H, ?OUT, ?BOUND),
    L = length(Pop),
    States = [lists:duplicate(?H, 0.0) || _ <- lists:seq(1, L)],
    Sims = [pb_sim:init([]) || _ <- lists:seq(1, L)],
    Alive = [true || _ <- lists:seq(1, L)],
    Steps = [0 || _ <- lists:seq(1, L)],
    batched_loop(Ref, States, Sims, Alive, Steps, 0).

batched_loop(_Ref, _States, _Sims, Alive, Steps, Step) when Step >= ?MAX_STEPS ->
    finalize(Alive, Steps, Step);
batched_loop(Ref, States, Sims, Alive, Steps, Step) ->
    case lists:any(fun(A) -> A end, Alive) of
        false ->
            Steps;
        true ->
            Inputs = [sense_or_zero(A, S) || {A, S} <- lists:zip(Alive, Sims)],
            {Outputs, NewStates} = tweann_nif:cfc_pop_step(Ref, States, Inputs),
            {Sims1, Alive1, Steps1} = advance(Alive, Sims, Outputs, Steps),
            batched_loop(Ref, NewStates, Sims1, Alive1, Steps1, Step + 1)
    end.

%% For nets that reached MAX_STEPS still alive, their step count is MAX_STEPS.
finalize(Alive, Steps, Max) ->
    [case A of true -> Max; false -> St end || {A, St} <- lists:zip(Alive, Steps)].

sense_or_zero(true, Sim) ->
    {In, _} = pb_sim:sense(v, [?VARIANT], Sim),
    In;
sense_or_zero(false, _Sim) ->
    lists:duplicate(?IN, 0.0).

advance(Alive, Sims, Outputs, Steps) ->
    Zipped = zip4(Alive, Sims, Outputs, Steps),
    Advanced = [advance_one(A, Sim, Out, St) || {A, Sim, Out, St} <- Zipped],
    {[Sim || {_, Sim, _} <- Advanced],
     [Al || {Al, _, _} <- Advanced],
     [St || {_, _, St} <- Advanced]}.

advance_one(false, Sim, _Out, St) ->
    {false, Sim, St};
advance_one(true, Sim, Out, St) ->
    {_F, Halt, Sim1} = pb_sim:act(v, ?ACT_PARAMS, Out, Sim),
    case Halt of
        0 -> {true, Sim1, St + 1};
        _ -> {false, Sim1, St}
    end.

%%%============================================================================
%%% Shared
%%%============================================================================

layer_weight_count() ->
    Net = network_evaluator:create_cfc_feedforward(?IN, [?H], ?OUT, tanh, tanh),
    length(network_evaluator:get_weights(Net)).

tau_of(X) -> 0.05 + math:log(1.0 + math:exp(min(X, 30.0))).

zip4([A | As], [B | Bs], [C | Cs], [D | Ds]) -> [{A, B, C, D} | zip4(As, Bs, Cs, Ds)];
zip4([], [], [], []) -> [].

set_taus(Net, Taus) ->
    Meta = network_evaluator:get_neuron_meta(Net),
    {NewMeta, []} = lists:mapfoldl(fun set_layer_taus/2, Taus, Meta),
    network_evaluator:set_neuron_meta(Net, NewMeta).

set_layer_taus(LayerMeta, Params) ->
    lists:mapfoldl(fun set_neuron_tau/2, Params, LayerMeta).

set_neuron_tau(#{neuron_type := cfc} = M, [P | Rest]) -> {M#{tau => P}, Rest};
set_neuron_tau(#{neuron_type := standard} = M, Params) -> {M, Params}.
