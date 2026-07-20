%% @doc Tuning selection: which neurons to perturb each memetic attempt.
%%
%% DXNN's memetic weight tuner does not perturb every neuron every attempt.
%% It perturbs a SUBSET, biased toward neurons added or mutated recently, each
%% with its own annealed spread. Old, stable neurons are left alone so their
%% already-tuned weights are not disturbed. This is what makes the hill-climber
%% efficient: it concentrates search where the topology just changed.
%%
%% faber-tweann's tuner previously perturbed all neurons every attempt with one
%% shared range (equivalent to `all' here). That was the blunt-instrument cost
%% flagged in insight 013.
%%
%% Ported from Gene Sher's DXNN2 (Handbook Ch 8). Each function returns a list
%% of {NeuronId, Spread}: the neurons to perturb and by how much.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(tuning_selection).

-include("records.hrl").

-export([select/5, dynamic/4, dynamic_random/4, active/4, current/4, all/4]).

-type id_spread() :: {term(), float()}.

%% @doc Dispatch by strategy name. Unknown strategy falls back to `dynamic',
%% the efficient default.
-spec select(atom(), [term()], non_neg_integer(), float(), float()) -> [id_spread()].
select(dynamic, Ids, Gen, PR, AP)        -> dynamic(Ids, Gen, PR, AP);
select(dynamic_random, Ids, Gen, PR, AP) -> dynamic_random(Ids, Gen, PR, AP);
select(active, Ids, Gen, PR, AP)         -> active(Ids, Gen, PR, AP);
select(current, Ids, Gen, PR, AP)        -> current(Ids, Gen, PR, AP);
select(all, Ids, Gen, PR, AP)            -> all(Ids, Gen, PR, AP);
select(_Other, Ids, Gen, PR, AP)         -> dynamic(Ids, Gen, PR, AP).

%% @doc Recently-changed neurons within a randomly chosen age limit.
%%
%% The age limit is sqrt(1/rand:uniform()), which is >=1 and usually small
%% (about 75% of the time <= 2), so most attempts perturb only the last
%% generation or two of neurons, but occasionally reach deeper. Each chosen
%% neuron's spread is annealed by its age: PR * pi * AnnealingParameter^Age.
%% If nothing qualifies, perturb the first neuron with full spread.
-spec dynamic([term()], non_neg_integer(), float(), float()) -> [id_spread()].
dynamic(Ids, Generation, PR, AP) ->
    AgeLimit = math:sqrt(1.0 / rand:uniform()),
    case recent(Ids, Generation, AgeLimit, PR, AP) of
        [] ->
            [Id | _] = Ids,
            [{Id, PR * math:pi()}];
        Chosen ->
            Chosen
    end.

%% @doc As dynamic/4, then keep a random subset of the result.
-spec dynamic_random([term()], non_neg_integer(), float(), float()) -> [id_spread()].
dynamic_random(Ids, Generation, PR, AP) ->
    random_subset(dynamic(Ids, Generation, PR, AP)).

%% @doc Neurons changed within a fixed recent window of 3 generations.
-spec active([term()], non_neg_integer(), float(), float()) -> [id_spread()].
active(Ids, Generation, PR, AP) ->
    case recent(Ids, Generation, 3.0, PR, AP) of
        [] -> [{hd(Ids), PR * math:pi()}];
        Chosen -> Chosen
    end.

%% @doc Only the neurons of the current generation.
-spec current([term()], non_neg_integer(), float(), float()) -> [id_spread()].
current(Ids, Generation, PR, AP) ->
    case recent(Ids, Generation, 0.0, PR, AP) of
        [] -> [{hd(Ids), PR * math:pi()}];
        Chosen -> Chosen
    end.

%% @doc Every neuron, each with the base spread. This is the pre-013 behaviour,
%% kept for ablation: `all' plus a const duration is the crude tuner.
-spec all([term()], non_neg_integer(), float(), float()) -> [id_spread()].
all(Ids, _Generation, PR, _AP) ->
    Spread = PR * math:pi(),
    [{Id, Spread} || Id <- Ids].

%%%============================================================================
%%% Internal
%%%============================================================================

%% @private Neurons whose generation is within AgeLimit of the current one,
%% each paired with its age-annealed spread.
recent(Ids, Generation, AgeLimit, PR, AP) ->
    lists:foldl(
        fun(Id, Acc) ->
            case generation_of(Id) of
                undefined ->
                    Acc;
                Gen when Gen >= (Generation - AgeLimit) ->
                    Age = Generation - Gen,
                    Spread = PR * math:pi() * math:pow(AP, Age),
                    [{Id, Spread} | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        Ids).

%% @private Read a neuron's or actuator's generation from the genotype.
generation_of({_, neuron} = Id) ->
    case genotype:dirty_read({neuron, Id}) of
        undefined -> undefined;
        N -> N#neuron.generation
    end;
generation_of({_, actuator} = Id) ->
    case genotype:dirty_read({actuator, Id}) of
        undefined -> undefined;
        A -> A#actuator.generation
    end;
generation_of(_) ->
    undefined.

%% @private Keep each element with probability 1/2, but never return empty.
random_subset([]) ->
    [];
random_subset(L) ->
    case [X || X <- L, rand:uniform() < 0.5] of
        [] -> [hd(L)];
        Sub -> Sub
    end.
