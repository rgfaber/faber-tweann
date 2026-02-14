%% @doc NEAT-style crossover for variable topology neural networks.
%%
%% This module implements crossover operations that work with networks
%% of different topologies by aligning genes via innovation numbers.
%%
%% Key concepts from NEAT (Stanley and Miikkulainen, 2002):
%% - Matching genes: Same innovation in both parents, randomly inherit
%% - Disjoint genes: Present in one parent within the other's range
%% - Excess genes: Present in one parent beyond the other's max innovation
%%
%% For speciation, compatibility distance measures structural difference
%% using weighted sum of disjoint/excess counts and weight differences.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(genome_crossover).

-include("records.hrl").

-export([
    crossover/3,
    align_genomes/2,
    compatibility_distance/3,
    extract_connection_genes/1
]).

%% Configuration for compatibility distance calculation
-record(compatibility_config, {
    c1 = 1.0 :: float(),  %% Coefficient for excess genes
    c2 = 1.0 :: float(),  %% Coefficient for disjoint genes
    c3 = 0.4 :: float()   %% Coefficient for weight differences
}).

-type connection_gene() :: #connection_gene{}.
-type genome() :: [connection_gene()].
-type fitter_parent() :: 1 | 2 | equal.
-type alignment_result() :: {
    Matching :: [{connection_gene(), connection_gene()}],
    Disjoint1 :: [connection_gene()],
    Disjoint2 :: [connection_gene()],
    Excess1 :: [connection_gene()],
    Excess2 :: [connection_gene()]
}.

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Perform NEAT-style crossover between two genomes.
%%
%% Matching genes are randomly inherited from either parent.
%% Disjoint and excess genes are inherited from the fitter parent.
%% When parents are equally fit, all genes are inherited with random selection
%% for conflicts.
%%
%% @param Genome1 First parent's connection genes
%% @param Genome2 Second parent's connection genes
%% @param FitterParent Which parent is fitter (1, 2, or equal)
%% @returns Child genome (list of connection genes)
-spec crossover(genome(), genome(), fitter_parent()) -> genome().
crossover(Genome1, Genome2, FitterParent) ->
    {Matching, Disjoint1, Disjoint2, Excess1, Excess2} = align_genomes(Genome1, Genome2),

    %% Matching genes: randomly select from either parent
    MatchingGenes = [random_select_gene(G1, G2) || {G1, G2} <- Matching],

    %% Disjoint/Excess genes: from fitter parent (or both if equal)
    ExtraGenes = select_extra_genes(FitterParent, Disjoint1, Disjoint2, Excess1, Excess2),

    %% Combine and sort by innovation number
    AllGenes = MatchingGenes ++ ExtraGenes,
    lists:sort(fun(A, B) -> A#connection_gene.innovation =< B#connection_gene.innovation end, AllGenes).

%% @doc Align two genomes by innovation number.
%%
%% Categorizes genes into:
%% - Matching: Same innovation in both parents
%% - Disjoint: Present in one parent, within the other's innovation range
%% - Excess: Present in one parent, beyond the other's max innovation
%%
%% @param Genome1 First genome (list of connection genes)
%% @param Genome2 Second genome (list of connection genes)
%% @returns {Matching, Disjoint1, Disjoint2, Excess1, Excess2}
-spec align_genomes(genome(), genome()) -> alignment_result().
align_genomes(Genome1, Genome2) ->
    %% Sort both genomes by innovation
    Sorted1 = lists:sort(fun(A, B) ->
        A#connection_gene.innovation =< B#connection_gene.innovation
    end, Genome1),
    Sorted2 = lists:sort(fun(A, B) ->
        A#connection_gene.innovation =< B#connection_gene.innovation
    end, Genome2),

    %% Find max innovations for each genome
    Max1 = max_innovation(Sorted1),
    Max2 = max_innovation(Sorted2),

    %% Build maps for O(1) lookup
    Map1 = maps:from_list([{G#connection_gene.innovation, G} || G <- Sorted1]),
    Map2 = maps:from_list([{G#connection_gene.innovation, G} || G <- Sorted2]),

    %% Categorize genes
    categorize_genes(Sorted1, Sorted2, Map1, Map2, Max1, Max2).

%% @doc Calculate compatibility distance between two genomes.
%%
%% Uses the NEAT formula:
%% delta = (c1 * E / N) + (c2 * D / N) + (c3 * W)
%%
%% Where:
%% - E = number of excess genes
%% - D = number of disjoint genes
%% - N = number of genes in larger genome (normalized, min 1)
%% - W = average weight difference in matching genes
%% - c1, c2, c3 = coefficients
%%
%% @param Genome1 First genome
%% @param Genome2 Second genome
%% @param Config Compatibility coefficients
%% @returns Compatibility distance (float)
-spec compatibility_distance(genome(), genome(), #compatibility_config{}) -> float().
compatibility_distance(Genome1, Genome2, Config) ->
    {Matching, Disjoint1, Disjoint2, Excess1, Excess2} = align_genomes(Genome1, Genome2),

    %% Count genes
    N = max(length(Genome1), length(Genome2)),
    N_normalized = max(N, 1),  % Avoid division by zero

    ExcessCount = length(Excess1) + length(Excess2),
    DisjointCount = length(Disjoint1) + length(Disjoint2),

    %% Calculate average weight difference
    AvgWeightDiff = calculate_avg_weight_diff(Matching),

    %% Apply formula
    (Config#compatibility_config.c1 * ExcessCount / N_normalized) +
    (Config#compatibility_config.c2 * DisjointCount / N_normalized) +
    (Config#compatibility_config.c3 * AvgWeightDiff).

%% @doc Extract connection genes from an agent's neural network.
%%
%% Converts the input_idps format to connection genes with innovations.
%% This enables NEAT-style crossover on existing networks.
%%
%% @param AgentId The agent to extract genes from
%% @returns List of connection genes
-spec extract_connection_genes(term()) -> genome().
extract_connection_genes(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),

    %% Extract from all neurons
    NeuronGenes = lists:flatmap(
        fun(NeuronId) ->
            Neuron = genotype:dirty_read({neuron, NeuronId}),
            extract_neuron_input_genes(Neuron)
        end,
        Cortex#cortex.neuron_ids
    ),

    %% Extract from all actuators (neuron -> actuator links)
    ActuatorGenes = lists:flatmap(
        fun(ActuatorId) ->
            Actuator = genotype:dirty_read({actuator, ActuatorId}),
            extract_actuator_input_genes(Actuator)
        end,
        Cortex#cortex.actuator_ids
    ),

    NeuronGenes ++ ActuatorGenes.

%%==============================================================================
%% Internal Functions
%%==============================================================================

max_innovation([]) -> 0;
max_innovation(Genes) ->
    lists:max([G#connection_gene.innovation || G <- Genes]).

categorize_genes(Sorted1, Sorted2, Map1, Map2, Max1, Max2) ->
    AllInnovations = lists:usort(
        [G#connection_gene.innovation || G <- Sorted1] ++
        [G#connection_gene.innovation || G <- Sorted2]
    ),

    lists:foldl(
        fun(Inn, {MatchAcc, Dis1Acc, Dis2Acc, Exc1Acc, Exc2Acc}) ->
            case {maps:get(Inn, Map1, undefined), maps:get(Inn, Map2, undefined)} of
                {G1, G2} when G1 =/= undefined, G2 =/= undefined ->
                    %% Matching gene
                    {[{G1, G2} | MatchAcc], Dis1Acc, Dis2Acc, Exc1Acc, Exc2Acc};

                {G1, undefined} when G1 =/= undefined ->
                    %% Only in genome 1
                    categorize_single_gene(Inn, G1, Max2,
                        {MatchAcc, Dis1Acc, Dis2Acc, Exc1Acc, Exc2Acc}, 1);

                {undefined, G2} when G2 =/= undefined ->
                    %% Only in genome 2
                    categorize_single_gene(Inn, G2, Max1,
                        {MatchAcc, Dis1Acc, Dis2Acc, Exc1Acc, Exc2Acc}, 2);

                _ ->
                    %% Shouldn't happen
                    {MatchAcc, Dis1Acc, Dis2Acc, Exc1Acc, Exc2Acc}
            end
        end,
        {[], [], [], [], []},
        AllInnovations
    ).

categorize_single_gene(Inn, Gene, OtherMax, {Match, Dis1, Dis2, Exc1, Exc2}, WhichGenome) ->
    case {Inn > OtherMax, WhichGenome} of
        {true, 1} ->
            %% Excess in genome 1
            {Match, Dis1, Dis2, [Gene | Exc1], Exc2};
        {true, 2} ->
            %% Excess in genome 2
            {Match, Dis1, Dis2, Exc1, [Gene | Exc2]};
        {false, 1} ->
            %% Disjoint in genome 1
            {Match, [Gene | Dis1], Dis2, Exc1, Exc2};
        {false, 2} ->
            %% Disjoint in genome 2
            {Match, Dis1, [Gene | Dis2], Exc1, Exc2}
    end.

random_select_gene(G1, G2) ->
    case rand:uniform(2) of
        1 -> G1;
        2 -> G2
    end.

select_extra_genes(1, Disjoint1, _Disjoint2, Excess1, _Excess2) ->
    Disjoint1 ++ Excess1;
select_extra_genes(2, _Disjoint1, Disjoint2, _Excess1, Excess2) ->
    Disjoint2 ++ Excess2;
select_extra_genes(equal, Disjoint1, Disjoint2, Excess1, Excess2) ->
    %% When equally fit, randomly include genes from both
    random_filter(Disjoint1) ++ random_filter(Disjoint2) ++
    random_filter(Excess1) ++ random_filter(Excess2).

random_filter(Genes) ->
    [G || G <- Genes, rand:uniform(2) =:= 1].

calculate_avg_weight_diff([]) -> 0.0;
calculate_avg_weight_diff(Matching) ->
    Diffs = [abs(G1#connection_gene.weight - G2#connection_gene.weight)
             || {G1, G2} <- Matching],
    lists:sum(Diffs) / length(Matching).

extract_neuron_input_genes(Neuron) ->
    ToId = Neuron#neuron.id,
    lists:filtermap(
        fun({FromId, Weights}) when FromId =/= bias ->
            %% Get or create innovation for this link
            Innovation = innovation:get_or_create_link_innovation(FromId, ToId),
            Weight = case Weights of
                [W | _] -> extract_weight_value(W);
                _ -> 0.0
            end,
            {true, #connection_gene{
                innovation = Innovation,
                from_id = FromId,
                to_id = ToId,
                weight = Weight,
                enabled = true
            }};
        (_) ->
            false  % Skip bias
        end,
        Neuron#neuron.input_idps
    ).

extract_actuator_input_genes(Actuator) ->
    ToId = Actuator#actuator.id,
    lists:map(
        fun(FromId) ->
            Innovation = innovation:get_or_create_link_innovation(FromId, ToId),
            #connection_gene{
                innovation = Innovation,
                from_id = FromId,
                to_id = ToId,
                weight = 1.0,  % Actuator links typically have weight 1.0
                enabled = true
            }
        end,
        Actuator#actuator.fanin_ids
    ).

extract_weight_value({W, _DW, _LR, _Params}) when is_number(W) -> W;
extract_weight_value(W) when is_number(W) -> W;
extract_weight_value(_) -> 0.0.
