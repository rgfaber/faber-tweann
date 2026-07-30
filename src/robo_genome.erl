%% @doc Robo Rumble: a genome on the wire, and the limits a host enforces on one.
%%
%% THIS EXISTS SO A TANK CAN FIGHT ON A STRANGER'S MACHINE. That is the whole
%% goal, and every decision below is answerable to it. A genome has to survive
%% being written down, sent, received by somebody who did not make it, and
%% checked before it is allowed to run.
%%
%% BUILD, NOT CLAIM. Nothing here tests a hypothesis about the world, so it gets
%% tests and a commit rather than a pre-registration and a gate.
%%
%% THE FORMAT IS ALMOST FREE, and that is a consequence of an earlier decision
%% rather than luck. A genome is a topology and a flat list of integers:
%%
%%     {Layers, Weights}   Layers  :: [pos_integer()], the layer widths in order
%%                         Weights :: [integer()], bias-then-weights per neuron,
%%                                    neurons in order, layers in order
%%
%% No floats and no maps anywhere in it. That matters because term_to_binary is
%% NOT canonical for maps: this front was already bitten once when a golden match
%% vector hashed a map and produced different bytes in two processes. Over tuples
%% and integer lists it is canonical, so packing is one call and the content hash
%% is stable.
%%
%% WHY VALIDATE RATHER THAN CLAMP. robo_net:quantize/1 clips a weight into range
%% silently, which is right for an optimiser proposing its own candidates and
%% WRONG for a stranger's genome: clipping changes the genome, which changes what
%% actually fought, which means the published id no longer identifies the thing
%% that ran. So a foreign genome is REJECTED rather than repaired, and the id a
%% host publishes is always the id of the code it executed.
%%
%% THE LIMITS ARE A DENIAL-OF-SERVICE DEFENCE, not a quality bar. PLAN_ROBO_RUMBLE
%% section 8 has listed them as owed since phase 0, with the reason: "without
%% these a visitor denial-of-services a host with a pathological topology". A host
%% runs a stranger's network up to MAX_TURNS times per tank per battle, so the
%% cost of a battle is linear in the weight count. Nothing here judges whether a
%% genome is any good; that is what the battle is for.
-module(robo_genome).

%% The wire
-export([pack/1, unpack/1, id/1]).
%% The contract a host enforces before running anything
-export([validate/1, limits/0, weight_count/1]).

-define(WIRE_VERSION, 1).

%% THE CAPS, each with its reason, because a cap with no reason gets raised by
%% the first person it inconveniences.
%%
%%   MAX_DEPTH      Phase 0's arms used 2 and 3 layers. Eight leaves room for
%%                  genuinely deeper controllers while bounding the per-turn
%%                  latency chain.
%%   MAX_WIDTH      robo_net's own overflow argument holds while a neuron's
%%                  fan-in stays under about a thousand; past that products
%%                  promote to bignums, which is a speed cliff rather than a
%%                  correctness problem. 256 sits an order of magnitude inside
%%                  that and is 21x phase 0's widest hidden layer.
%%   MAX_WEIGHTS    The real cost. A battle evaluates every live tank's network
%%                  once per turn, so this is what bounds a host's work. Phase
%%                  0's largest genome was 281 weights; 65536 is 233x that and
%%                  still cheap.
%%   MAX_BYTES      A frame bound, so a host can reject before it decodes rather
%%                  than after. Derived, not chosen: 65536 weights cannot pack
%%                  into more than this.
-define(MAX_DEPTH, 8).
-define(MAX_WIDTH, 256).
-define(MAX_WEIGHTS, 65536).
-define(MAX_BYTES, 1048576).

-type layers() :: [pos_integer()].
-type weights() :: [integer()].
-type genome() :: {layers(), weights()}.
-export_type([genome/0]).

%%==============================================================================
%% The wire
%%==============================================================================

%% Canonical bytes for a genome. VERSIONED, because the first thing a wire format
%% needs is the ability to say "I am not what you think I am". A host that meets
%% version 2 should refuse it, not misread it.
%%
%% HAND-ROLLED, AND term_to_binary IS DELIBERATELY NOT USED HERE. The first
%% version of this function packed with term_to_binary/2 and the deterministic
%% option, which is wrong for a CONTENT ADDRESS in a way that would not have shown
%% up on one machine:
%%
%%   The deterministic option fixes MAP ORDERING WITHIN A RELEASE. It does not
%%   promise the same bytes across OTP releases. Atom encoding has changed
%%   between releases (SMALL_ATOM_EXT to SMALL_ATOM_UTF8_EXT), and a list of
%%   small integers may encode as STRING_EXT or LIST_EXT depending on its
%%   contents. So two honest hosts on different OTP versions could compute
%%   DIFFERENT IDS FOR THE SAME GENOME, which destroys the only property a
%%   content address has.
%%
%% The layout below has no runtime freedom left in it. Every field is a fixed
%% width, big-endian, and the whole thing is readable by anything that can read
%% bytes, which matters because a stranger's machine need not be running Erlang.
%%
%%     "RG"          2 bytes, magic
%%     Version       1 byte
%%     Depth         2 bytes, number of layers
%%     Layers        2 bytes each, unsigned
%%     WeightCount   4 bytes
%%     Weights       2 bytes each, SIGNED
%%
%% Two-byte weights are sufficient by construction and not by luck: validate/1
%% refuses anything outside plus or minus robo_net:weight_limit(), which is 2048,
%% and layer widths are capped at MAX_WIDTH. pack/1 asserts the genome is valid
%% first, so an out-of-range value cannot be silently truncated into a different
%% genome that hashes the same.
-spec pack(genome()) -> binary().
pack({Layers, Weights} = G) ->
    {ok, G} = validate(G),
    LayerBytes = << <<L:16/big-unsigned>> || L <- Layers >>,
    WeightBytes = << <<W:16/big-signed>> || W <- Weights >>,
    <<"RG", ?WIRE_VERSION:8, (length(Layers)):16/big-unsigned, LayerBytes/binary,
      (length(Weights)):32/big-unsigned, WeightBytes/binary>>.

%% Bytes back into a genome, TOTAL over arbitrary input, because this is the
%% function that meets a stranger. Every failure is a tagged reason a host can
%% publish rather than a crash it has to survive.
%%
%% binary_to_term/2 with the safe option is what stops a hostile frame creating
%% atoms or decoding a term referring to remote resources. Without it, "receive
%% bytes from anyone" is a remote atom-table exhaustion.
-spec unpack(binary()) -> {ok, genome()} | {error, term()}.
unpack(Bin) when not is_binary(Bin) -> {error, not_a_binary};
unpack(Bin) when byte_size(Bin) > ?MAX_BYTES ->
    {error, {too_many_bytes, byte_size(Bin), ?MAX_BYTES}};
unpack(<<"RG", V:8, _/binary>>) when V =/= ?WIRE_VERSION ->
    {error, {unsupported_version, V, ?WIRE_VERSION}};
unpack(<<"RG", ?WIRE_VERSION:8, Depth:16/big-unsigned, Rest/binary>>) ->
    layers(Depth, Rest);
unpack(<<_/binary>>) -> {error, not_a_genome}.

%% Bounded BEFORE the binary is sliced, so a hostile header claiming a huge depth
%% cannot make a host allocate on the strength of a number it has not checked.
layers(Depth, _Rest) when Depth > ?MAX_DEPTH -> {error, {too_deep, Depth, ?MAX_DEPTH}};
layers(Depth, Rest) -> layers_take(Depth, Rest, Depth * 2 =< byte_size(Rest)).

layers_take(_Depth, _Rest, false) -> {error, truncated_layers};
layers_take(Depth, Rest, true) ->
    <<LB:(Depth * 2)/binary, Tail/binary>> = Rest,
    counts([L || <<L:16/big-unsigned>> <= LB], Tail).

counts(Layers, <<N:32/big-unsigned, Rest/binary>>) when N =< ?MAX_WEIGHTS ->
    weights(Layers, N, Rest, N * 2 =:= byte_size(Rest));
counts(_Layers, <<N:32/big-unsigned, _/binary>>) ->
    {error, {too_many_weights, N, ?MAX_WEIGHTS}};
counts(_Layers, _Short) -> {error, truncated_weight_count}.

%% EXACT length, not "at least". Trailing bytes are refused rather than ignored,
%% because two different frames that decode to one genome would give that genome
%% two ids, and a content address that is not one-to-one is not an address.
weights(_Layers, _N, _Rest, false) -> {error, truncated_weights};
weights(Layers, _N, Rest, true) ->
    validate({Layers, [W || <<W:16/big-signed>> <= Rest]}).

%% The content address. A host publishes this beside a result so anyone can say
%% which genome fought, and two hosts that ran the same genome agree on its name
%% without having to coordinate.
-spec id(genome() | binary()) -> binary().
id(Bin) when is_binary(Bin) -> crypto:hash(sha256, Bin);
id({_Layers, _Weights} = G) -> id(pack(G)).

%%==============================================================================
%% The contract
%%==============================================================================

%% What a host publishes so a visitor can build something acceptable BEFORE
%% sending it, rather than discovering the rules by rejection.
-spec limits() -> map().
limits() ->
    #{wire_version => ?WIRE_VERSION,
      inputs => robo_pilot:inputs(),
      outputs => robo_pilot:outputs(),
      max_depth => ?MAX_DEPTH,
      max_width => ?MAX_WIDTH,
      max_weights => ?MAX_WEIGHTS,
      max_bytes => ?MAX_BYTES,
      weight_limit => robo_net:weight_limit()}.

-spec weight_count(layers()) -> non_neg_integer().
weight_count(Layers) -> robo_net:weight_count(Layers).

%% Every check a host makes before it is willing to run a stranger's network.
%% Ordered cheapest first, and each returns a reason specific enough that the
%% sender can fix it without guessing.
%%
%% THE ARITY CHECKS ARE THE IMPORTANT ONES AND THEY ARE THE EASIEST TO SKIP.
%% robo_net:fit/2 silently pads or truncates a first layer that is the wrong
%% width, and robo_pilot's intent/1 falls back to a null intent on a short output
%% vector. So a mismatched genome does not fail: it RUNS, badly, and produces a
%% result that looks real. Nothing downstream would ever notice.
-spec validate(term()) -> {ok, genome()} | {error, term()}.
validate({Layers, Weights}) when is_list(Layers), is_list(Weights) ->
    check(lists:map(fun(F) -> F({Layers, Weights}) end, checks()), {Layers, Weights});
validate(_Other) -> {error, not_a_genome}.

checks() ->
    [fun shape_ok/1, fun depth_ok/1, fun width_ok/1, fun arity_ok/1,
     fun count_ok/1, fun range_ok/1].

check([], G) -> {ok, G};
check([ok | Rest], G) -> check(Rest, G);
check([{error, _} = E | _Rest], _G) -> E.

%% Every layer a positive integer, at least an input and an output layer.
shape_ok({Layers, Weights}) ->
    Bad = [L || L <- Layers, not is_integer(L) orelse L < 1],
    shape_verdict(length(Layers), Bad, lists:all(fun is_integer/1, Weights)).

shape_verdict(N, _Bad, _Ints) when N < 2 -> {error, {too_few_layers, N}};
shape_verdict(_N, [B | _], _Ints) -> {error, {bad_layer_width, B}};
shape_verdict(_N, [], false) -> {error, non_integer_weight};
shape_verdict(_N, [], true) -> ok.

depth_ok({Layers, _W}) -> bound(length(Layers), ?MAX_DEPTH, too_deep).

width_ok({Layers, _W}) -> bound(lists:max(Layers), ?MAX_WIDTH, too_wide).

%% The two that a silent pad or truncate would otherwise hide.
arity_ok({Layers, _W}) ->
    arity_verdict(hd(Layers), lists:last(Layers),
                  robo_pilot:inputs(), robo_pilot:outputs()).

arity_verdict(In, Out, In, WantOut) -> arity_out(Out, WantOut);
arity_verdict(In, _Out, Want, _WantOut) -> {error, {wrong_input_width, In, Want}}.

arity_out(Out, Out) -> ok;
arity_out(Out, Want) -> {error, {wrong_output_width, Out, Want}}.

count_ok({Layers, Weights}) ->
    count_verdict(length(Weights), robo_net:weight_count(Layers)).

count_verdict(N, N) -> bound(N, ?MAX_WEIGHTS, too_many_weights);
count_verdict(Got, Want) -> {error, {wrong_weight_count, Got, Want}}.

%% REJECTED, not clipped. See the module header.
range_ok({_Layers, Weights}) ->
    L = robo_net:weight_limit(),
    range_verdict([W || W <- Weights, abs(W) > L], L).

range_verdict([], _L) -> ok;
range_verdict([W | _], L) -> {error, {weight_out_of_range, W, L}}.

bound(V, Max, _Tag) when V =< Max -> ok;
bound(V, Max, Tag) -> {error, {Tag, V, Max}}.
