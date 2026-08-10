%%%-----------------------------------------------------------------------------
%%% @doc The genotype layer's own random generator, so a run can be a function
%%% of its seed and a library draw can never perturb a caller's stream.
%%%
%%% ==========================================================================
%%% THE FAILURE THIS EXISTS TO PREVENT
%%% ==========================================================================
%%%
%%% Every draw in genotype construction and mutation used to be a bare
%%% rand:uniform/0, which reads and writes the ONE rand state that Erlang keeps
%%% in the calling process's dictionary. That is the state the caller is also
%%% using. So calling genome_mutator:mutate/1 silently advanced the caller's
%%% generator by an unpredictable number of steps, and a caller who had seeded
%%% deliberately no longer got the sequence they seeded for.
%%%
%%% A downstream project has this on record. hecate-dronex's island module
%%% carries its generator through every function rather than reaching for it,
%%% under a banner that reads: "Register D.5: one unrecorded draw in a library
%%% constructor was enough to make the benchmark irreproducible and to break the
%%% property that a genome specifies a controller." The unrecorded draw in a
%%% library constructor was this one.
%%%
%%% ==========================================================================
%%% WHY A SEPARATE STATE RATHER THAN THREADING
%%% ==========================================================================
%%%
%%% Threading a rand:state() through every constructor and every mutation
%%% operator is the textbook answer and it would change the arity and the return
%%% shape of genotype:construct_Agent/3, clone_Agent/1, genome_mutator:mutate/1
%%% and every operator behind them. That is a breaking change to the whole
%%% public surface, for a property most callers do not ask for.
%%%
%%% This module keeps its own state, under its own key, touched by nothing else.
%%% That alone fixes the actual defect, because the genotype layer no longer
%%% shares a generator with anybody.
%%%
%%% And a caller who DOES want pure-value semantics can have them, by threading
%%% at the call boundary rather than through the call:
%%%
%%%   ok = genotype_rand:set_state(IslandRand),
%%%   ok = genome_mutator:mutate(AgentId),
%%%   NextIslandRand = genotype_rand:state()
%%%
%%% which is three lines and needs no signature to change.
%%%
%%% ==========================================================================
%%% WHAT IS ROUTED THROUGH HERE, AND WHAT IS NOT
%%% ==========================================================================
%%%
%%% Routed, which is everything genotype:construct_Agent/3 and
%%% genome_mutator:mutate/1,2 reach: genotype, mutation_helpers, ltc_mutations,
%%% perturbation_utils and selection_utils.
%%%
%%% NOT routed, and a run using them is not yet reproducible from a seed:
%%% crossover, genome_crossover, selection_algorithm, tuning_selection,
%%% species_identifier, network_evaluator's weight initialisation, the ES
%%% optimisers and the scapes. Stated rather than implied, because "the run is
%%% reproducible" is exactly the kind of claim that costs when it is only
%%% mostly true.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(genotype_rand).

-export([seed/1, state/0, set_state/1, uniform/0, uniform/1, element_of/1]).

%% Its own key. The whole point is that this is not the state rand:uniform/0
%% reads, so nothing outside this module may touch it.
-define(KEY, '$faber_genotype_rand').

%% exsss is the current default algorithm and is explicit here so a future
%% change to the default cannot silently change what a recorded seed produces.
-define(ALG, exsss).

%%==============================================================================
%% Seeding and threading
%%==============================================================================

%% @doc Seed the genotype generator for this process.
%%
%% Two runs seeded the same way draw the same sequence, so a surprising genotype
%% can be built again.
-spec seed(integer() | {integer(), integer(), integer()}) -> ok.
seed(Seed) -> set_state(rand:seed_s(?ALG, Seed)).

%% @doc The current generator state, for a caller that wants to carry it.
%%
%% Initialises from the system on first use, so a caller that never seeds still
%% gets ordinary randomness rather than a fixed sequence.
-spec state() -> rand:state().
state() ->
    case erlang:get(?KEY) of
        undefined ->
            Fresh = rand:seed_s(?ALG),
            ok = set_state(Fresh),
            Fresh;
        Existing ->
            Existing
    end.

%% @doc Install a generator state, so a run can be resumed or replayed exactly.
-spec set_state(rand:state()) -> ok.
set_state(State) ->
    _ = erlang:put(?KEY, State),
    ok.

%%==============================================================================
%% Draws
%%==============================================================================

%% @doc A float in [0.0, 1.0).
-spec uniform() -> float().
uniform() ->
    {Value, Next} = rand:uniform_s(state()),
    ok = set_state(Next),
    Value.

%% @doc An integer in 1..N.
-spec uniform(pos_integer()) -> pos_integer().
uniform(N) ->
    {Value, Next} = rand:uniform_s(N, state()),
    ok = set_state(Next),
    Value.

%% @doc One element of a non-empty list.
-spec element_of([T, ...]) -> T.
element_of(List) -> lists:nth(uniform(length(List)), List).
