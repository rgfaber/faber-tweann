%%% @doc Tests for the genotype layer's own generator.
%%%
%%% Two properties, and the second is the one that was actually broken.
%%%
%%% A run seeded the same way produces the same genotype, so a surprising one
%%% can be built again. And building a genotype does not move the CALLER's
%%% generator, so a caller who seeded deliberately still gets the sequence they
%%% seeded for. The second is hecate-dronex register D.5, which cost that
%%% project an irreproducible benchmark.
-module(genotype_rand_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init().

teardown(_) ->
    genotype:reset_db().

genotype_rand_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun the_same_seed_builds_the_same_genotype/0,
        fun different_seeds_build_different_genotypes/0,
        fun building_a_genotype_does_not_move_the_callers_generator/0,
        fun mutating_does_not_move_the_callers_generator/0,
        fun a_state_can_be_carried_across_a_call/0,
        fun an_unseeded_process_still_gets_randomness/0
    ]}.

%%==============================================================================
%% Helpers
%%==============================================================================

an_agent() ->
    Id = {{origin, genotype:generate_UniqueId()}, agent},
    genotype:construct_Agent(test_specie, Id, #constraint{morphology = xor_mimic}),
    Id.

%% The packed genotype, which is a function of every draw that built it.
packed(AgentId) ->
    {ok, Bin} = genotype:to_binary(AgentId),
    Bin.

%% Everything about a genotype except the identities, which are themselves
%% draws. Two runs from one seed should agree on all of it.
shape(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cx = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    Neurons = [genotype:dirty_read({neuron, Id}) || Id <- Cx#cortex.neuron_ids],
    [{N#neuron.af, N#neuron.aggr_f, N#neuron.neuron_type,
      [{length(Ws), [W || {W, _, _, _} <- Ws]} || {_Src, Ws} <- N#neuron.input_idps]}
     || N <- Neurons].

%%==============================================================================
%% Reproducibility
%%==============================================================================

the_same_seed_builds_the_same_genotype() ->
    ok = genotype_rand:seed(4242),
    A = an_agent(),
    ShapeA = shape(A),
    PackedA = packed(A),
    genotype:reset_db(),
    ok = genotype_rand:seed(4242),
    B = an_agent(),
    ?assertEqual(ShapeA, shape(B)),
    %% Identities are draws too, so from one seed even the bytes agree.
    ?assertEqual(PackedA, packed(B)).

different_seeds_build_different_genotypes() ->
    ok = genotype_rand:seed(1),
    A = packed(an_agent()),
    genotype:reset_db(),
    ok = genotype_rand:seed(2),
    B = packed(an_agent()),
    ?assertNotEqual(A, B).

%%==============================================================================
%% Not touching the caller's stream. This is D.5.
%%==============================================================================

%% The caller seeds, records what it expects to draw, then builds a genotype in
%% between. Before this module existed, construct_Agent advanced the caller's
%% generator by an unpredictable number of steps and the second draw differed.
building_a_genotype_does_not_move_the_callers_generator() ->
    rand:seed(exsss, {7, 7, 7}),
    Expected = [rand:uniform() || _ <- lists:seq(1, 5)],
    rand:seed(exsss, {7, 7, 7}),
    First = rand:uniform(),
    _ = an_agent(),
    Rest = [rand:uniform() || _ <- lists:seq(1, 4)],
    ?assertEqual(Expected, [First | Rest]).

mutating_does_not_move_the_callers_generator() ->
    AgentId = an_agent(),
    rand:seed(exsss, {11, 11, 11}),
    Expected = [rand:uniform() || _ <- lists:seq(1, 5)],
    rand:seed(exsss, {11, 11, 11}),
    First = rand:uniform(),
    _ = genome_mutator:mutate(AgentId),
    Rest = [rand:uniform() || _ <- lists:seq(1, 4)],
    ?assertEqual(Expected, [First | Rest]).

%%==============================================================================
%% Threading at the call boundary, which is how a caller regains purity
%%==============================================================================

a_state_can_be_carried_across_a_call() ->
    Carried = rand:seed_s(exsss, {3, 1, 4}),
    ok = genotype_rand:set_state(Carried),
    A = packed(an_agent()),
    After = genotype_rand:state(),
    ?assertNotEqual(Carried, After),
    %% Put the same state back and the same genotype comes out again.
    genotype:reset_db(),
    ok = genotype_rand:set_state(Carried),
    B = packed(an_agent()),
    ?assertEqual(A, B),
    ?assertEqual(After, genotype_rand:state()).

%% A caller that never seeds must still get ordinary randomness, not a fixed
%% sequence, or every unseeded run would build the identical genotype.
an_unseeded_process_still_gets_randomness() ->
    Draws = [genotype_rand:uniform() || _ <- lists:seq(1, 20)],
    ?assertEqual(20, length(lists:usort(Draws))),
    ?assert(lists:all(fun(V) -> V >= 0.0 andalso V < 1.0 end, Draws)).
