%%% @doc Tests for canonical genotype serialisation (ROADMAP 8b).
%%%
%%% Two things are being defended here and they are different.
%%%
%%% That a genotype round-trips, which is what makes persistence and a wire
%%% possible at all. And that the SAME genotype produces the SAME bytes, which
%%% is what makes genome_id an address rather than a hint. The second is the one
%%% that costs, and hecate-dronex REGISTER I.12 is the record of what it costs
%%% when it is absent: two identical images computed different fingerprints from
%%% term_to_binary/1, each filtered the other out as incompatible, and nothing
%%% was logged anywhere.
-module(genotype_codec_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init().

teardown(_) ->
    genotype:reset_db().

genotype_codec_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun round_trip_preserves_the_whole_subgraph/0,
        fun a_pack_is_a_snapshot_not_a_view/0,
        fun the_same_genotype_packs_to_the_same_bytes/0,
        fun the_address_changes_when_the_genome_changes/0,
        fun atoms_travel_by_name_not_by_table_position/0,
        fun zero_has_exactly_one_representation/0,
        fun refuses_a_shape_it_cannot_encode/0,
        fun refuses_a_non_finite_float_on_the_wire/0,
        fun refuses_an_atom_this_build_does_not_know/0,
        fun refuses_to_overwrite_a_living_agent/0,
        fun refuses_bad_magic_at_byte_four/0,
        fun refuses_trailing_bytes/0,
        fun refuses_truncation/0,
        fun limits_are_reportable_before_they_are_hit/0
    ]}.

%%==============================================================================
%% Helpers
%%==============================================================================

an_agent() ->
    Id = {{origin, genotype:generate_UniqueId()}, agent},
    genotype:construct_Agent(test_specie, Id, #constraint{morphology = xor_mimic}),
    Id.

subgraph(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cx = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    Get = fun(Tag, Ids) -> [genotype:dirty_read({Tag, I}) || I <- Ids] end,
    {Agent, Cx,
     lists:sort(Get(neuron, Cx#cortex.neuron_ids)),
     lists:sort(Get(sensor, Cx#cortex.sensor_ids)),
     lists:sort(Get(actuator, Cx#cortex.actuator_ids))}.

a_neuron_of(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cx = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    genotype:dirty_read({neuron, hd(Cx#cortex.neuron_ids)}).

a_sensor_of(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cx = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    genotype:dirty_read({sensor, hd(Cx#cortex.sensor_ids)}).

%%==============================================================================
%% Round trip
%%==============================================================================

round_trip_preserves_the_whole_subgraph() ->
    AgentId = an_agent(),
    Before = subgraph(AgentId),
    {ok, Bin} = genotype:to_binary(AgentId),
    genotype:reset_db(),
    ?assertEqual(undefined, genotype:dirty_read({agent, AgentId})),
    ?assertEqual({ok, AgentId}, genotype:from_binary(Bin)),
    ?assertEqual(Before, subgraph(AgentId)).

%% The point of packing is to hold a genotype still while the live one moves on.
%% If the bytes tracked later mutation, an archived champion would silently
%% become whatever its lineage became.
a_pack_is_a_snapshot_not_a_view() ->
    AgentId = an_agent(),
    {ok, Bin} = genotype:to_binary(AgentId),
    Original = a_neuron_of(AgentId),
    Moved = Original#neuron{time_constant = 1.7734, state_bound = 2.5},
    genotype:write(Moved),
    ?assertNotEqual(Original, a_neuron_of(AgentId)),
    genotype:reset_db(),
    {ok, AgentId} = genotype:from_binary(Bin),
    ?assertEqual(Original, a_neuron_of(AgentId)).

%%==============================================================================
%% Canonicality: the property that makes the address an address
%%==============================================================================

the_same_genotype_packs_to_the_same_bytes() ->
    AgentId = an_agent(),
    {ok, First} = genotype:to_binary(AgentId),
    %% Through a full reset and restore, so the second pack reads out of freshly
    %% populated tables rather than out of the ones construction happened to
    %% leave behind.
    genotype:reset_db(),
    {ok, AgentId} = genotype:from_binary(First),
    {ok, Second} = genotype:to_binary(AgentId),
    ?assertEqual(First, Second),
    ?assertEqual(genotype:genome_id(First), genotype:genome_id(Second)).

the_address_changes_when_the_genome_changes() ->
    AgentId = an_agent(),
    {ok, IdBefore} = genotype:genome_id(AgentId),
    N = a_neuron_of(AgentId),
    genotype:write(N#neuron{time_constant = N#neuron.time_constant + 0.5}),
    {ok, IdAfter} = genotype:genome_id(AgentId),
    ?assertNotEqual(IdBefore, IdAfter),
    ?assertEqual(32, byte_size(IdBefore)).

%% I.12 was possible because term_to_binary/1 let a node's atom table decide the
%% bytes. Here an atom is its NAME, so two nodes cannot disagree. A local test
%% cannot produce a second atom table, so the mechanism is asserted directly
%% rather than the symptom.
atoms_travel_by_name_not_by_table_position() ->
    AgentId = an_agent(),
    {ok, Bin} = genotype:to_binary(AgentId),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"xor_mimic">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"tanh">>)).

%% Zero is encoded sign-positive with an empty magnitude, so it has exactly one
%% byte form. Without the normalisation it falls through to the negative clause
%% and becomes a "negative zero", which is a second representation of one value.
%% Both forms round-trip and both are deterministic, so an equality or
%% round-trip assertion CANNOT see the difference. This test was written that
%% way first, and an injected regression sailed straight through it. It asserts
%% the bytes instead.
zero_has_exactly_one_representation() ->
    AgentId = an_agent(),
    N = a_neuron_of(AgentId),
    genotype:write(N#neuron{generation = 0}),
    {ok, Bin} = genotype:to_binary(AgentId),
    PositiveZero = <<2, 0, 0, 1, 0>>,
    NegativeZero = <<2, 1, 0, 1, 0>>,
    ?assertNotEqual(nomatch, binary:match(Bin, PositiveZero)),
    ?assertEqual(nomatch, binary:match(Bin, NegativeZero)),
    genotype:reset_db(),
    {ok, AgentId} = genotype:from_binary(Bin),
    ?assertEqual(0, (a_neuron_of(AgentId))#neuron.generation).

%%==============================================================================
%% Refusal. Every one of these must refuse rather than clamp, approximate or
%% succeed quietly, because the module beside this one does the opposite and
%% that is the defect it is recorded as.
%%==============================================================================

%% A map is the specific shape that made I.12 possible, so it is the one refused
%% by name here. It cannot be encoded canonically by this codec and it is not
%% quietly dropped either.
refuses_a_shape_it_cannot_encode() ->
    AgentId = an_agent(),
    S = a_sensor_of(AgentId),
    genotype:write(S#sensor{parameters = #{a => 1}}),
    ?assertMatch({error, {unsupported_term, _}}, genotype:to_binary(AgentId)).

%% Only the DECODE path can meet one of these. Erlang arithmetic raises badarith
%% rather than overflowing to an infinity, and the bit syntax refuses to match
%% the bit patterns, so nothing running in this VM can produce one to encode. A
%% check on the encode side would be defending an impossible state; a check here
%% is defending against bytes that came from somewhere else.
refuses_a_non_finite_float_on_the_wire() ->
    Inf = <<127, 240, 0, 0, 0, 0, 0, 0>>,
    ?assertMatch({error, {non_finite_float, Inf}},
                 genotype:from_binary(<<"FTG1", 3, Inf/binary>>)),
    Nan = <<127, 248, 0, 0, 0, 0, 0, 0>>,
    ?assertMatch({error, {non_finite_float, Nan}},
                 genotype:from_binary(<<"FTG1", 3, Nan/binary>>)).

refuses_an_atom_this_build_does_not_know() ->
    %% A genome from an incompatible build. The atom table is not collected, so
    %% decoding must never be able to mint one.
    Name = <<"an_atom_no_faber_build_will_ever_define_zzz">>,
    Body = <<1, (byte_size(Name)):8, Name/binary>>,
    ?assertMatch({error, {unknown_atom, Name}},
                 genotype:from_binary(<<"FTG1", Body/binary>>)).

refuses_to_overwrite_a_living_agent() ->
    AgentId = an_agent(),
    {ok, Bin} = genotype:to_binary(AgentId),
    ?assertEqual({error, {agent_exists, AgentId}}, genotype:from_binary(Bin)).

refuses_bad_magic_at_byte_four() ->
    AgentId = an_agent(),
    {ok, <<_:4/binary, Body/binary>>} = genotype:to_binary(AgentId),
    ?assertEqual({error, bad_magic}, genotype:from_binary(<<"FTG9", Body/binary>>)).

refuses_trailing_bytes() ->
    AgentId = an_agent(),
    {ok, Bin} = genotype:to_binary(AgentId),
    genotype:reset_db(),
    ?assertEqual({error, trailing_bytes}, genotype:from_binary(<<Bin/binary, 0>>)).

refuses_truncation() ->
    AgentId = an_agent(),
    {ok, Bin} = genotype:to_binary(AgentId),
    genotype:reset_db(),
    Half = binary:part(Bin, 0, byte_size(Bin) div 2),
    ?assertEqual({error, truncated}, genotype:from_binary(Half)).

limits_are_reportable_before_they_are_hit() ->
    L = genotype_codec:limits(),
    ?assert(is_integer(maps:get(max_neurons, L))),
    ?assert(is_integer(maps:get(max_bytes, L))),
    ?assert(is_integer(maps:get(max_depth, L))).
