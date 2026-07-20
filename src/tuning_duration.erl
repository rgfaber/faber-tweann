%% @doc Tuning duration: how many memetic attempts an agent gets per evaluation.
%%
%% A larger, recently-grown network has more weights to tune and deserves more
%% hill-climbing attempts; a small stable one needs few. DXNN computes this per
%% agent rather than fixing it. faber-tweann previously hardcoded a constant
%% (15, then 60), which over-tunes small networks and under-tunes large ones
%% (insight 013).
%%
%% Ported from Gene Sher's DXNN2 (Handbook Ch 8).
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(tuning_duration).

-include("records.hrl").

-export([duration/4, const/3, wsize_proportional/3]).

%% @doc Dispatch by strategy name. Unknown strategy falls back to a constant
%% of the given parameter.
-spec duration(atom(), term(), [term()], non_neg_integer()) -> pos_integer().
duration(const, Parameter, NIds, Generation) ->
    const(Parameter, NIds, Generation);
duration(wsize_proportional, Parameter, NIds, Generation) ->
    wsize_proportional(Parameter, NIds, Generation);
duration(_Other, Parameter, NIds, Generation) ->
    const(Parameter, NIds, Generation).

%% @doc A fixed number of attempts, ignoring the network.
-spec const(pos_integer(), [term()], non_neg_integer()) -> pos_integer().
const(Parameter, _NIds, _Generation) when is_integer(Parameter), Parameter > 0 ->
    Parameter;
const(_Parameter, _NIds, _Generation) ->
    15.

%% @doc Attempts proportional to the weight count of recently-changed neurons.
%%
%% 10 + saturate(round(RecentWeightCount ^ Power), 0, 100), so between 10 and
%% 110 attempts. Power near 0.5 keeps it modest. Neurons unchanged in the last
%% three generations do not count: their weights are assumed already tuned.
-spec wsize_proportional(number(), [term()], non_neg_integer()) -> pos_integer().
wsize_proportional(Power, NIds, Generation) ->
    ActiveNIds = recent_neuron_ids(NIds, Generation, 3),
    Weights = total_weight_count(ActiveNIds),
    10 + sat(round(math:pow(max(Weights, 1), Power)), 0, 100).

%%%============================================================================
%%% Internal
%%%============================================================================

recent_neuron_ids(NIds, Generation, AgeLimit) ->
    lists:filter(
        fun(NId) ->
            case genotype:dirty_read({neuron, NId}) of
                undefined -> false;
                N -> N#neuron.generation >= (Generation - AgeLimit)
            end
        end,
        NIds).

total_weight_count(NIds) ->
    lists:foldl(
        fun(NId, Acc) ->
            case genotype:dirty_read({neuron, NId}) of
                undefined -> Acc;
                N -> Acc + input_weight_count(N#neuron.input_idps)
            end
        end,
        0,
        NIds).

%% Each input_idp is {SourceId, WeightList}; bias contributes one weight too.
input_weight_count(InputIdps) ->
    lists:foldl(
        fun({_Src, Weights}, Acc) when is_list(Weights) -> Acc + length(Weights);
           ({_Src, _W}, Acc) -> Acc + 1
        end,
        0,
        InputIdps).

sat(V, Min, _Max) when V < Min -> Min;
sat(V, _Min, Max) when V > Max -> Max;
sat(V, _Min, _Max) -> V.
