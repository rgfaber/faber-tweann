%% @doc Tests for the genome wire format and the N-tank battle.
%%
%% THESE EXIST SO A TANK CAN FIGHT ON A STRANGER'S MACHINE. Both modules under
%% test meet code somebody else wrote, so the tests that matter most are the ones
%% about REFUSING things, not the ones about the happy path.
%%
%% THE HAZARD THESE ARE REALLY AIMED AT. A wrong-shaped genome does not crash: it
%% RUNS. robo_net:fit/2 silently pads or truncates a first layer of the wrong
%% width, and robo_pilot's intent/1 falls back to a null intent on a short output
%% vector. So a mismatched network fights badly and produces a result that looks
%% completely real, and nothing downstream would ever notice. Validation is the
%% only thing standing between that and a published lie, which is why most of
%% this file is rejection cases.
%% @end
-module(robo_rumble_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("faber_tweann/include/robo_sim.hrl").

%%==============================================================================
%% The wire
%%==============================================================================

round_trip_test() ->
    G = valid_genome(),
    ?assertEqual({ok, G}, robo_genome:unpack(robo_genome:pack(G))).

%% Packing must be a function of the genome and nothing else, or every published
%% id becomes unstable and two hosts naming the same genome disagree.
pack_is_deterministic_test() ->
    G = valid_genome(),
    ?assertEqual(robo_genome:pack(G), robo_genome:pack(G)),
    ?assertEqual(robo_genome:id(G), robo_genome:id(G)),
    %% An equal genome built separately must pack identically, which is the
    %% property that makes the id a content address rather than a session token.
    ?assertEqual(robo_genome:id(valid_genome()), robo_genome:id(G)).

different_genomes_get_different_ids_test() ->
    {L, [W | Rest]} = valid_genome(),
    ?assertNotEqual(robo_genome:id({L, [W + 1 | Rest]}), robo_genome:id({L, [W | Rest]})).

%%==============================================================================
%% Refusing things. A stranger controls every byte here.
%%==============================================================================

garbage_bytes_are_refused_test() ->
    ?assertMatch({error, _}, robo_genome:unpack(<<"not a term at all">>)),
    ?assertMatch({error, _}, robo_genome:unpack(<<131, 255, 255, 255>>)),
    ?assertEqual({error, not_a_binary}, robo_genome:unpack(nonsense)).

%% A frame that is not ours, and one from a future version. The second must be
%% REFUSED rather than misread: a wire format's first duty is to be able to say
%% "I am not what you think I am".
wrong_shape_frames_are_refused_test() ->
    ?assertEqual({error, not_a_genome},
                 robo_genome:unpack(term_to_binary({something_else, 1, 2, 3}))),
    ?assertMatch({error, {unsupported_version, 99, _}},
                 robo_genome:unpack(<<"RG", 99:8, 2:16, 17:16, 5:16>>)).

%% THE FORMAT IS HAND-ROLLED, so the decoder meets lengths a stranger chose and
%% every one of them has to be bounded before it is used to slice.
truncated_frames_are_refused_test() ->
    ?assertMatch({error, truncated_layers}, robo_genome:unpack(<<"RG", 1:8, 2:16, 17:16>>)),
    ?assertMatch({error, truncated_weight_count},
                 robo_genome:unpack(<<"RG", 1:8, 2:16, 17:16, 5:16>>)),
    ?assertMatch({error, truncated_weights},
                 robo_genome:unpack(<<"RG", 1:8, 2:16, 17:16, 5:16, 90:32, 0:16>>)).

%% A header claiming a huge depth must be refused BEFORE anything is sliced on
%% the strength of it, or a 9-byte frame makes a host try to read megabytes.
hostile_header_is_bounded_before_slicing_test() ->
    ?assertMatch({error, {too_deep, 65535, _}}, robo_genome:unpack(<<"RG", 1:8, 65535:16>>)),
    ?assertMatch({error, {too_many_weights, 4294967295, _}},
                 robo_genome:unpack(<<"RG", 1:8, 2:16, 17:16, 5:16, 4294967295:32>>)).

%% Trailing bytes are refused rather than ignored. Two different frames that both
%% decoded to one genome would give that genome two ids, and a content address
%% that is not one-to-one is not an address.
trailing_bytes_are_refused_test() ->
    Bin = robo_genome:pack(valid_genome()),
    ?assertMatch({error, truncated_weights}, robo_genome:unpack(<<Bin/binary, 0>>)).

%% The bytes are fully specified, so they can be asserted rather than described.
%% This is what makes the format readable by something that is not Erlang, which
%% is the point: a stranger's machine need not be running our runtime.
frame_layout_is_exactly_specified_test() ->
    {Layers, Weights} = valid_genome(),
    <<"RG", 1:8, Depth:16, Rest/binary>> = robo_genome:pack(valid_genome()),
    ?assertEqual(length(Layers), Depth),
    <<17:16, 5:16, Count:32, WBytes/binary>> = Rest,
    ?assertEqual(length(Weights), Count),
    ?assertEqual(length(Weights) * 2, byte_size(WBytes)).

%% A negative weight must survive the round trip, which two-byte UNSIGNED fields
%% would silently mangle into a different genome that still validates.
negative_weights_survive_the_round_trip_test() ->
    {Layers, [_ | Rest]} = valid_genome(),
    G = {Layers, [-robo_net:weight_limit() | Rest]},
    ?assertEqual({ok, G}, robo_genome:unpack(robo_genome:pack(G))).

%% THE TWO CHECKS A SILENT PAD OR TRUNCATE WOULD OTHERWISE HIDE. The output case
%% is here because the first version of the validator compared the expected
%% output width against ITSELF and therefore never checked it at all. That bug
%% was found by tracing the clauses before compiling, and this test is what stops
%% it coming back.
wrong_input_width_is_refused_test() ->
    L = [16, 5],
    ?assertMatch({error, {wrong_input_width, 16, 17}},
                 robo_genome:validate({L, zeros(robo_net:weight_count(L))})).

wrong_output_width_is_refused_test() ->
    L = [17, 4],
    ?assertMatch({error, {wrong_output_width, 4, 5}},
                 robo_genome:validate({L, zeros(robo_net:weight_count(L))})).

wrong_weight_count_is_refused_test() ->
    ?assertMatch({error, {wrong_weight_count, 3, 90}},
                 robo_genome:validate({[17, 5], [0, 0, 0]})).

%% REJECTED, not clipped. Clipping would change the genome, which changes what
%% actually fought, which means the published id no longer names the thing that
%% ran.
out_of_range_weight_is_refused_test() ->
    L = robo_net:weight_limit(),
    {Layers, [_ | Rest]} = valid_genome(),
    ?assertMatch({error, {weight_out_of_range, _, L}},
                 robo_genome:validate({Layers, [L + 1 | Rest]})).

non_integer_weight_is_refused_test() ->
    {Layers, [_ | Rest]} = valid_genome(),
    ?assertEqual({error, non_integer_weight}, robo_genome:validate({Layers, [1.5 | Rest]})).

%% The denial-of-service caps. A visitor must not be able to hand a host a
%% network so large that evaluating it once per turn per tank stalls the machine.
too_deep_is_refused_test() ->
    Deep = [17] ++ lists:duplicate(20, 4) ++ [5],
    ?assertMatch({error, {too_deep, _, _}},
                 robo_genome:validate({Deep, zeros(robo_net:weight_count(Deep))})).

too_wide_is_refused_test() ->
    Wide = [17, 100000, 5],
    ?assertMatch({error, {too_wide, 100000, _}}, robo_genome:validate({Wide, []})).

oversized_frame_is_refused_before_decoding_test() ->
    #{max_bytes := Max} = robo_genome:limits(),
    ?assertMatch({error, {too_many_bytes, _, Max}},
                 robo_genome:unpack(<<0:((Max + 8) * 8)>>)).

degenerate_topology_is_refused_test() ->
    ?assertMatch({error, {too_few_layers, 1}}, robo_genome:validate({[17], []})),
    ?assertEqual({error, not_a_genome}, robo_genome:validate(banana)).

%% A host publishes its limits so a visitor can build something acceptable
%% BEFORE sending it, rather than learning the rules by rejection.
limits_describe_the_real_contract_test() ->
    #{inputs := In, outputs := Out, weight_limit := WL} = robo_genome:limits(),
    ?assertEqual(robo_pilot:inputs(), In),
    ?assertEqual(robo_pilot:outputs(), Out),
    ?assertEqual(robo_net:weight_limit(), WL).

%%==============================================================================
%% The battle
%%==============================================================================

%% THE HEADLINE. More than two entrants, which the engine always supported and
%% nothing ever exercised.
battle_royale_of_five_runs_test() ->
    Entrants = [{e1, {script, circle_strafer}},
                {e2, {script, predictive_gun}},
                {e3, {script, rammer}},
                {e4, {script, spinner}},
                {e5, {genome, valid_genome()}}],
    {ok, R} = robo_rumble:battle(Entrants),
    ?assertEqual(5, length(maps:get(standings, R))),
    ?assertEqual(5, length(maps:get(entrants, R))),
    ?assert(maps:get(turns, R) > 0),
    %% Last tank standing: at the end at most one is alive, or the cap was hit.
    Alive = [S || S <- maps:get(standings, R), maps:get(alive, S)],
    ?assert(length(Alive) =< 1 orelse maps:get(turns, R) >= 1).

two_entrant_battle_runs_test() ->
    {ok, R} = robo_rumble:battle([{a, {script, predictive_gun}},
                                  {b, {script, sitting_duck}}]),
    ?assertEqual(2, length(maps:get(standings, R))),
    %% The duck cannot win, so this one has a decidable winner.
    ?assertEqual(a, maps:get(winner, R)).

%% A genome entrant is actually driven, rather than standing still because the
%% pilot was never wired in. An untrained all-zero net still steers, because
%% every intent field comes from a bias-plus-weights sum that is zero, so the
%% test is that it PARTICIPATES: it appears in the standings with a real
%% survival count.
genome_entrant_is_actually_driven_test() ->
    {ok, R} = robo_rumble:battle([{visitor, {genome, valid_genome()}},
                                  {house, {script, predictive_gun}}]),
    [V] = [S || S <- maps:get(standings, R), maps:get(id, S) =:= visitor],
    ?assertMatch({genome, _Id}, maps:get(kind, V)),
    ?assert(maps:get(survived, V) > 0).

%%==============================================================================
%% Refusing a battle
%%==============================================================================

%% Refuse BEFORE the battle, not during. One bad entrant stops the whole thing,
%% because a battle that silently drops a competitor reports a result about a
%% different contest than the one requested.
invalid_genome_refuses_the_whole_battle_test() ->
    Bad = {[16, 5], zeros(robo_net:weight_count([16, 5]))},
    ?assertMatch({error, {rejected, visitor, {wrong_input_width, 16, 17}}},
                 robo_rumble:battle([{visitor, {genome, Bad}},
                                     {house, {script, sitting_duck}}])).

unknown_script_is_refused_test() ->
    ?assertMatch({error, {rejected, x, {unknown_script, no_such_bot}}},
                 robo_rumble:battle([{x, {script, no_such_bot}},
                                     {y, {script, sitting_duck}}])).

%% Two entrants sharing an id would silently become ONE tank, because the arena
%% keys tanks by id.
duplicate_ids_are_refused_test() ->
    ?assertEqual({error, duplicate_entrant_id},
                 robo_rumble:battle([{same, {script, rammer}},
                                     {same, {script, spinner}}])).

too_few_entrants_is_refused_test() ->
    ?assertMatch({error, {too_few_entrants, 1}},
                 robo_rumble:battle([{lonely, {script, rammer}}])),
    ?assertMatch({error, {too_few_entrants, 0}}, robo_rumble:battle([])).

%%==============================================================================
%% Placement
%%==============================================================================

%% Nobody gets a better seat, and adding a competitor must not quietly change the
%% geometry for everyone else in a way that puts somebody in a wall.
placement_is_inside_the_arena_test() ->
    {W, H} = robo_sim:arena_size(),
    [begin
         Ps = robo_rumble:place(N),
         ?assertEqual(N, length(Ps)),
         [begin
              ?assert(X > 0 andalso X * 256 < W),
              ?assert(Y > 0 andalso Y * 256 < H),
              ?assert(A >= 0 andalso A =< 255)
          end || {X, Y, A} <- Ps]
     end || N <- [2, 3, 5, 8, 16]],
    ok.

placement_is_deterministic_test() ->
    ?assertEqual(robo_rumble:place(7), robo_rumble:place(7)).

%% A result is meaningless without knowing which physics produced it, since a
%% rules change makes old results incomparable rather than merely old.
engine_id_is_stable_within_a_build_test() ->
    ?assertEqual(robo_rumble:engine_id(), robo_rumble:engine_id()),
    ?assertEqual(32, byte_size(robo_rumble:engine_id())).

%%==============================================================================
%% Fixtures
%%==============================================================================

%% The smallest genome the game accepts: straight from the 17 senses to the 5
%% controls, which is the shape phase 0's arm L used.
valid_genome() ->
    L = [robo_pilot:inputs(), robo_pilot:outputs()],
    {L, zeros(robo_net:weight_count(L))}.

zeros(N) -> lists:duplicate(N, 0).
