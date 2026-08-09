%%%-----------------------------------------------------------------------------
%%% @doc Canonical, lossless serialisation of a genotype, so an evolved topology
%%% can leave the VM it was bred in.
%%%
%%% ROADMAP item 8b. Until this existed, a genotype lived in ETS and nowhere
%%% else: it could not be persisted, could not be put on a wire, and could not be
%%% handed to another node. That was the whole of what stopped topology evolution
%%% being usable by a service, and it was a missing function rather than a
%%% missing capability.
%%%
%%% ==========================================================================
%%% WHY NOT term_to_binary/2
%%% ==========================================================================
%%%
%%% The deterministic option is not promised to be stable across OTP releases,
%%% so it is unfit for a content address: the same genotype would hash
%%% differently on two nodes running different releases, each side would
%%% conclude the other's genome was a different genome, and nothing would say
%%% so. The sibling failure is on record as hecate-dronex REGISTER I.12, where
%%% term_to_binary/1 over a map produced per-node byte differences, two
%%% identical images computed different engine fingerprints, each filtered the
%%% other out as incompatible, and no exchange was ever attempted with nothing
%%% logged anywhere.
%%%
%%% So the encoding here is hand-rolled and closed. It covers exactly the term
%%% shapes a genotype contains, which is atoms, integers, floats, binaries,
%%% proper lists and tuples, and it REFUSES everything else rather than
%%% guessing. Verified against include/records.hrl: the genotype records contain
%%% no maps, and a map is the shape that made I.12 possible.
%%%
%%% ==========================================================================
%%% VALIDATE AND REJECT, NEVER CLAMP
%%% ==========================================================================
%%%
%%% A limit that clamps changes the genome, and then the id no longer identifies
%%% the thing that ran. Every limit below refuses. The limits are a
%%% denial-of-service defence against a stranger's genome, not a quality bar.
%%%
%%% Decoding resolves atoms with binary_to_existing_atom/2 for the same reason:
%%% the atom table is not garbage collected, so decoding an untrusted genome
%%% must not be able to mint atoms. A genotype's atoms are activation function
%%% names, aggregator names, neuron types and record tags, all of which exist in
%%% any VM that loaded this application. An unknown atom means the genome came
%%% from an incompatible build, and saying so loudly is the correct answer.
%%%
%%% ==========================================================================
%%% WHAT IS AND IS NOT PRESERVED
%%% ==========================================================================
%%%
%%% Lossless. The agent record, its cortex, and every neuron, sensor and
%%% actuator the cortex names, verbatim, including bookkeeping fields that do
%%% not determine behaviour. Choosing which fields "matter" would be a silent
%%% lossy conversion, which is the exact defect this module sits beside:
%%% network_evaluator:from_genotype/1 approximates a topology and reports
%%% success. A caller that wants less can drop fields itself.
%%%
%%% Identity is preserved too, so the content address covers WHICH genotype this
%%% is and not only what shape it has. Two structurally identical genotypes bred
%%% at different times have different ids and therefore different addresses.
%%% Structural equivalence is a different question and is deliberately not
%%% answered here.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(genotype_codec).

-include("records.hrl").

-export([to_binary/1, from_binary/1, genome_id/1, limits/0]).

%% Wire magic and version. A version bump is a new magic, never a flag inside
%% the body, so an old decoder rejects a new genome at byte four rather than
%% part way through a neuron.
%% A string rather than a binary literal, so it is legal as a leading field in a
%% pattern as well as in a construction.
-define(MAGIC, "FTG1").

%% Term tags. Never reordered or reused; a new shape takes the next free tag.
-define(T_ATOM, 1).
-define(T_INT, 2).
-define(T_FLOAT, 3).
-define(T_BIN, 4).
-define(T_LIST, 5).
-define(T_TUPLE, 6).

%% Denial-of-service limits. Chosen to be far above any genotype this engine has
%% ever produced, because their job is to bound a hostile input rather than to
%% express an opinion about network size.
-define(MAX_BYTES, 16 * 1024 * 1024).
-define(MAX_NEURONS, 65536).
-define(MAX_SENSORS, 4096).
-define(MAX_ACTUATORS, 4096).
-define(MAX_DEPTH, 64).
-define(MAX_ELEMENTS, 1048576).
-define(MAX_ATOM_BYTES, 255).

-type reason() ::
    {unsupported_term, term()}
    | {non_finite_float, binary()}
    | {too_deep, pos_integer()}
    | {too_many_elements, non_neg_integer()}
    | {atom_too_long, non_neg_integer()}
    | {too_many, neurons | sensors | actuators, non_neg_integer()}
    | {too_large, non_neg_integer()}
    | {unknown_atom, binary()}
    | bad_magic
    | truncated
    | trailing_bytes
    | {missing, cortex | neuron | sensor | actuator, term()}
    | {agent_exists, term()}
    | {agent_not_found, term()}.

-export_type([reason/0]).

%%==============================================================================
%% Public API
%%==============================================================================

%% @doc The limits a genome is validated against, so a caller can check before
%% building rather than after being refused.
-spec limits() -> #{atom() => pos_integer()}.
limits() ->
    #{
        max_bytes => ?MAX_BYTES,
        max_neurons => ?MAX_NEURONS,
        max_sensors => ?MAX_SENSORS,
        max_actuators => ?MAX_ACTUATORS,
        max_depth => ?MAX_DEPTH,
        max_elements => ?MAX_ELEMENTS,
        max_atom_bytes => ?MAX_ATOM_BYTES
    }.

%% @doc Pack an agent and everything its cortex names into canonical bytes.
%%
%% The same genotype packs to the same bytes on any node and any OTP release,
%% which is what makes genome_id/1 an address rather than a hint.
-spec to_binary(term()) -> {ok, binary()} | {error, reason()}.
to_binary(AgentId) ->
    try
        Agent = required(agent, AgentId),
        Cortex = required(cortex, Agent#agent.cx_id),
        Neurons = collect(neuron, Cortex#cortex.neuron_ids, ?MAX_NEURONS, neurons),
        Sensors = collect(sensor, Cortex#cortex.sensor_ids, ?MAX_SENSORS, sensors),
        Actuators = collect(actuator, Cortex#cortex.actuator_ids, ?MAX_ACTUATORS, actuators),
        Body = enc({Agent, Cortex, Neurons, Sensors, Actuators}, 0),
        Bin = <<?MAGIC, Body/binary>>,
        ok = within(byte_size(Bin)),
        {ok, Bin}
    catch
        throw:{codec, Reason} -> {error, Reason}
    end.

%% @doc Restore a packed genotype into the local tables, verbatim.
%%
%% Refuses if an agent with that identity is already present. Importing a
%% stranger's genome as a distinct local agent is clone_Agent/1 on the restored
%% id, which is existing machinery and is not duplicated here.
-spec from_binary(binary()) -> {ok, term()} | {error, reason()}.
from_binary(Bin) when is_binary(Bin) ->
    try
        ok = within(byte_size(Bin)),
        Body = magic_stripped(Bin),
        {{Agent, Cortex, Neurons, Sensors, Actuators}, Rest} = dec(Body, 0),
        Rest =:= <<>> orelse fail(trailing_bytes),
        ok = counted(length(Neurons), ?MAX_NEURONS, neurons),
        ok = counted(length(Sensors), ?MAX_SENSORS, sensors),
        ok = counted(length(Actuators), ?MAX_ACTUATORS, actuators),
        AgentId = Agent#agent.id,
        genotype:dirty_read({agent, AgentId}) =:= undefined
            orelse fail({agent_exists, AgentId}),
        [genotype:write(R) || R <- [Agent, Cortex | Neurons ++ Sensors ++ Actuators]],
        {ok, AgentId}
    catch
        throw:{codec, Reason} -> {error, Reason};
        error:{badmatch, _} -> {error, truncated};
        error:function_clause -> {error, truncated}
    end.

%% @doc The content address of a packed genotype.
%%
%% Takes either an agent id or already-packed bytes. It identifies this
%% genotype, identity included, and not its structure alone.
-spec genome_id(binary() | term()) -> {ok, binary()} | {error, reason()}.
genome_id(Bin) when is_binary(Bin) ->
    {ok, crypto:hash(sha256, Bin)};
genome_id(AgentId) ->
    case to_binary(AgentId) of
        {ok, Bin} -> genome_id(Bin);
        {error, _} = E -> E
    end.

%%==============================================================================
%% Reading the subgraph
%%==============================================================================

required(Tag, Id) ->
    case genotype:dirty_read({Tag, Id}) of
        undefined -> fail({missing, Tag, Id});
        Rec -> Rec
    end.

%% Records are sorted by id so that two identical genotypes pack identically
%% whatever order the table happened to yield. The cortex's own id lists keep
%% their order, because that order is genome content rather than storage layout.
collect(Tag, Ids, Max, What) ->
    ok = counted(length(Ids), Max, What),
    lists:sort([required(Tag, Id) || Id <- Ids]).

counted(N, Max, _What) when N =< Max -> ok;
counted(N, _Max, What) -> fail({too_many, What, N}).

within(N) when N =< ?MAX_BYTES -> ok;
within(N) -> fail({too_large, N}).

magic_stripped(<<?MAGIC, Body/binary>>) -> Body;
magic_stripped(_) -> fail(bad_magic).

%%==============================================================================
%% Canonical encoding of the closed term subset
%%==============================================================================

enc(_T, D) when D > ?MAX_DEPTH -> fail({too_deep, D});
enc(A, _D) when is_atom(A) ->
    Bin = atom_to_binary(A, utf8),
    byte_size(Bin) =< ?MAX_ATOM_BYTES orelse fail({atom_too_long, byte_size(Bin)}),
    <<?T_ATOM, (byte_size(Bin)):8, Bin/binary>>;
enc(I, _D) when is_integer(I) ->
    {Sign, Mag} = signed_magnitude(I),
    MagBin = binary:encode_unsigned(Mag),
    <<?T_INT, Sign:8, (byte_size(MagBin)):16, MagBin/binary>>;
%% No finiteness check here, deliberately. The VM cannot hold an infinity or a
%% NaN: arithmetic raises badarith rather than overflowing, and the bit syntax
%% refuses to match those patterns. A guard here would be defensive code for a
%% state that cannot occur. The decode side is different and does check, because
%% bytes arriving from elsewhere are not bound by any of that.
enc(F, _D) when is_float(F) ->
    <<?T_FLOAT, F:64/big-float>>;
enc(B, _D) when is_binary(B) ->
    <<?T_BIN, (byte_size(B)):32, B/binary>>;
enc(L, D) when is_list(L) ->
    N = proper_length(L),
    ok = sized(N),
    <<?T_LIST, N:32, (iolist_to_binary([enc(E, D + 1) || E <- L]))/binary>>;
enc(T, D) when is_tuple(T) ->
    N = tuple_size(T),
    ok = sized(N),
    <<?T_TUPLE, N:32, (iolist_to_binary([enc(E, D + 1) || E <- tuple_to_list(T)]))/binary>>;
enc(Other, _D) ->
    fail({unsupported_term, Other}).

%% Zero is encoded with sign 0 and an empty magnitude, so it has exactly one
%% representation. Without this, 0 and -0 would be two byte strings for one
%% integer and the address would not be a function of the value.
signed_magnitude(0) -> {0, 0};
signed_magnitude(I) when I > 0 -> {0, I};
signed_magnitude(I) -> {1, -I}.

proper_length(L) -> proper_length(L, 0).

proper_length([], N) -> N;
proper_length([_ | T], N) -> proper_length(T, N + 1);
proper_length(_Improper, _N) -> fail({unsupported_term, improper_list}).

sized(N) when N =< ?MAX_ELEMENTS -> ok;
sized(N) -> fail({too_many_elements, N}).

%%==============================================================================
%% Decoding
%%==============================================================================

dec(_B, D) when D > ?MAX_DEPTH -> fail({too_deep, D});
dec(<<?T_ATOM, Len:8, Bin:Len/binary, Rest/binary>>, _D) ->
    {existing_atom(Bin), Rest};
dec(<<?T_INT, Sign:8, Len:16, Mag:Len/binary, Rest/binary>>, _D) ->
    {signed(Sign, unsigned(Mag)), Rest};
dec(<<?T_FLOAT, Bits:8/binary, Rest/binary>>, _D) ->
    {float_of(Bits), Rest};
dec(<<?T_BIN, Len:32, Bin:Len/binary, Rest/binary>>, _D) ->
    {Bin, Rest};
dec(<<?T_LIST, N:32, Rest/binary>>, D) ->
    ok = sized(N),
    dec_seq(N, Rest, D + 1, [], fun lists:reverse/1);
dec(<<?T_TUPLE, N:32, Rest/binary>>, D) ->
    ok = sized(N),
    dec_seq(N, Rest, D + 1, [], fun(Acc) -> list_to_tuple(lists:reverse(Acc)) end);
dec(_Other, _D) ->
    fail(truncated).

dec_seq(0, Rest, _D, Acc, Finish) -> {Finish(Acc), Rest};
dec_seq(N, Bin, D, Acc, Finish) ->
    {Term, Rest} = dec(Bin, D),
    dec_seq(N - 1, Rest, D, [Term | Acc], Finish).

%% An infinity or a NaN on the wire came from something that is not this codec,
%% since nothing in the VM can produce one. Refused by name so it does not
%% surface as a truncation, which would send a reader looking for the wrong bug.
float_of(Bits) ->
    case Bits of
        <<F:64/big-float>> -> F;
        _ -> fail({non_finite_float, Bits})
    end.

unsigned(<<>>) -> 0;
unsigned(Bin) -> binary:decode_unsigned(Bin).

signed(0, Mag) -> Mag;
signed(1, Mag) -> -Mag.

%% Never binary_to_atom/2. The atom table is not collected, so decoding an
%% untrusted genome must not be able to mint atoms.
existing_atom(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> fail({unknown_atom, Bin})
    end.

%%==============================================================================
%% Failure
%%==============================================================================

-spec fail(reason()) -> no_return().
fail(Reason) -> throw({codec, Reason}).
