%% @doc Species identification and behavioral fingerprinting.
%%
%% This module implements speciation - grouping similar agents into species
%% to preserve diversity and prevent premature convergence. Agents compete
%% primarily within their own species, allowing diverse strategies to coexist.
%%
%% == Speciation Strategy ==
%%
%% Behavioral Fingerprinting:
%% - Records agent behavior across standard test scenarios
%% - Fingerprint is a vector of behavioral responses
%% - Example: For XOR, fingerprint = [output(0,0), output(0,1), output(1,0), output(1,1)]
%%
%% Distance Calculation:
%% - Euclidean distance between behavioral fingerprints
%% - Lower distance = more similar behavior
%% - Threshold-based grouping into species
%%
%% Species Assignment:
%% - Compare agent fingerprint to species representatives
%% - Assign to closest species if distance less than threshold
%% - Create new species if too different from all existing species
%%
%% == Benefits of Speciation ==
%%
%% - Preserves diversity (prevents single strategy dominance)
%% - Protects innovation (new mutations get time to optimize)
%% - Parallel search (multiple strategies explored simultaneously)
%% - Better exploration of fitness landscape
%%
%% == Implementation Notes ==
%%
%% Species are identified by their champion (best performer).
%% Distance threshold controls speciation granularity:
%% - Low threshold = many small species (high diversity)
%% - High threshold = few large species (low diversity)
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(species_identifier).

-include("records.hrl").

%% Dialyzer suppressions for supertype warnings on map() return types.
%% We use map() for API flexibility; dialyzer infers more specific types.
-dialyzer({nowarn_function, [extract_ltc_signature/1,
                             extract_ltc_signature_from_neurons/1,
                             extract_neuron_ltc_data/1,
                             default_ltc_signature/0]}).

-export([
    identify_species/3,
    calculate_distance/2,
    create_fingerprint/1,
    %% LTC-aware distance functions
    calculate_ltc_distance/2,
    calculate_combined_distance/4,
    extract_ltc_signature/1
]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Assign agents to species based on behavioral similarity.
%%
%% Takes a list of agents with their fingerprints and groups them into
%% species. Agents are compared to existing species representatives,
%% and assigned to the closest species if within the distance threshold.
%% If no species is close enough, a new species is created.
%%
%% Example:
%%   AgentFingerprints = [
%%     {agent1, [0.1, 0.9, 0.8, 0.2]},
%%     {agent2, [0.0, 1.0, 1.0, 0.1]},  % Similar to agent1
%%     {agent3, [0.9, 0.1, 0.2, 0.8]}   % Very different
%%   ]
%%   Threshold = 0.5
%%   ExistingSpecies = #{}
%%
%%   Result = identify_species(AgentFingerprints, Threshold, ExistingSpecies)
%%   Result might be:
%%   #{
%%     specie1 => [agent1, agent2],  % Similar agents
%%     specie2 => [agent3]            % Different agent
%%   }
%%
%% @param AgentFingerprints list of {AgentId, Fingerprint} tuples
%% @param Threshold distance threshold for species membership
%% @param ExistingSpecies map of {SpecieId => {RepresentativeFingerprint, Members}}
%% @returns map of {SpecieId => [AgentId]} assignments
-spec identify_species([{term(), [float()]}], float(),
                       #{term() => {[float()], [term()]}}) ->
    #{term() => [term()]}.
identify_species([], _Threshold, ExistingSpecies) ->
    %% Return just the member lists
    maps:map(fun(_SpecieId, {_Rep, Members}) -> Members end, ExistingSpecies);
identify_species(AgentFingerprints, Threshold, ExistingSpecies) ->
    %% Assign each agent to a species
    UpdatedSpecies = lists:foldl(
        fun({AgentId, Fingerprint}, SpeciesAcc) ->
            assign_to_species(AgentId, Fingerprint, Threshold, SpeciesAcc)
        end,
        ExistingSpecies,
        AgentFingerprints
    ),

    %% Return just the member lists
    maps:map(fun(_SpecieId, {_Rep, Members}) -> Members end, UpdatedSpecies).

%% @doc Calculate behavioral distance between two fingerprints.
%%
%% Uses Euclidean distance to measure behavioral similarity.
%% Lower distance indicates more similar behavior.
%%
%% Formula: sqrt(sum((F1[i] - F2[i])^2))
%%
%% Example:
%%   F1 = [0.0, 1.0, 1.0, 0.0]
%%   F2 = [0.1, 0.9, 0.8, 0.1]
%%   Distance = calculate_distance(F1, F2)
%%   Distance ≈ 0.244 (quite similar)
%%
%% @param Fingerprint1 first behavioral fingerprint vector
%% @param Fingerprint2 second behavioral fingerprint vector
%% @returns Euclidean distance between fingerprints
-spec calculate_distance([float()], [float()]) -> float().
calculate_distance(Fingerprint1, Fingerprint2) ->
    %% Calculate squared differences
    SquaredDiffs = lists:zipwith(
        fun(V1, V2) ->
            Diff = V1 - V2,
            Diff * Diff
        end,
        Fingerprint1,
        Fingerprint2
    ),

    %% Sum and take square root
    math:sqrt(lists:sum(SquaredDiffs)).

%% @doc Create behavioral fingerprint for an agent.
%%
%% Runs the agent through standard test scenarios and records
%% the behavioral responses. The fingerprint is a vector of
%% output values across all test inputs.
%%
%% For morphologies with well-defined test sets (like XOR),
%% the fingerprint is deterministic. For stochastic environments,
%% multiple runs may be averaged.
%%
%% Example for XOR:
%%   AgentId = {1.0, agent}
%%   Fingerprint = create_fingerprint(AgentId)
%%   Fingerprint = [0.02, 0.98, 0.97, 0.03]
%%   % Outputs for inputs: (0,0), (0,1), (1,0), (1,1)
%%
%% @param AgentId agent identifier
%% @returns behavioral fingerprint vector
-spec create_fingerprint(term()) -> [float()].
create_fingerprint(AgentId) ->
    %% Read agent to get morphology
    case genotype:read({agent, AgentId}) of
        undefined ->
            %% Agent not found, return zero fingerprint
            [0.0];
        Agent ->
            %% Get constraint to determine morphology
            Constraint = Agent#agent.constraint,
            Morphology = Constraint#constraint.morphology,

            %% Generate fingerprint based on morphology
            generate_fingerprint_for_morphology(AgentId, Morphology)
    end.

%% ============================================================================
%% Internal Functions - Species Assignment
%% ============================================================================

%% @private Assign an agent to the closest species or create new one.
-spec assign_to_species(term(), [float()], float(),
                        #{term() => {[float()], [term()]}}) ->
    #{term() => {[float()], [term()]}}.
assign_to_species(AgentId, Fingerprint, Threshold, Species) ->
    case find_closest_species(Fingerprint, Threshold, Species) of
        {ok, SpecieId} ->
            %% Add to existing species
            {Rep, Members} = maps:get(SpecieId, Species),
            maps:put(SpecieId, {Rep, [AgentId | Members]}, Species);
        not_found ->
            %% Create new species with this agent as representative
            NewSpecieId = generate_specie_id(),
            maps:put(NewSpecieId, {Fingerprint, [AgentId]}, Species)
    end.

%% @private Find the closest species within threshold.
-spec find_closest_species([float()], float(),
                           #{term() => {[float()], [term()]}}) ->
    {ok, term()} | not_found.
find_closest_species(_Fingerprint, _Threshold, Species) when map_size(Species) =:= 0 ->
    not_found;
find_closest_species(Fingerprint, Threshold, Species) ->
    %% Calculate distance to each species representative
    Distances = maps:fold(
        fun(SpecieId, {RepFingerprint, _Members}, Acc) ->
            Distance = calculate_distance(Fingerprint, RepFingerprint),
            [{SpecieId, Distance} | Acc]
        end,
        [],
        Species
    ),

    %% Find closest species
    {ClosestSpecieId, ClosestDistance} = lists:foldl(
        fun({SpecieId, Distance}, {BestId, BestDist}) ->
            if
                Distance < BestDist -> {SpecieId, Distance};
                true -> {BestId, BestDist}
            end
        end,
        {undefined, infinity},
        Distances
    ),

    %% Check if within threshold
    case ClosestDistance < Threshold of
        true -> {ok, ClosestSpecieId};
        false -> not_found
    end.

%% @private Generate unique species identifier.
-spec generate_specie_id() -> {float(), specie}.
generate_specie_id() ->
    {rand:uniform() * 1000000.0, specie}.

%% ============================================================================
%% Internal Functions - Fingerprint Generation
%% ============================================================================

%% @private Generate fingerprint based on agent's morphology.
-spec generate_fingerprint_for_morphology(term(), atom()) -> [float()].
generate_fingerprint_for_morphology(AgentId, xor_mimic) ->
    %% XOR has 4 test cases: (0,0), (0,1), (1,0), (1,1)
    TestInputs = [
        [0.0, 0.0],
        [0.0, 1.0],
        [1.0, 0.0],
        [1.0, 1.0]
    ],

    %% Evaluate agent on each test input
    lists:map(
        fun(Input) ->
            evaluate_agent_once(AgentId, Input)
        end,
        TestInputs
    );

generate_fingerprint_for_morphology(AgentId, pole_balancing) ->
    %% Pole balancing: test with different initial conditions
    InitialStates = [
        [0.0, 0.0, 0.1, 0.0],   % Small angle perturbation
        [0.0, 0.0, -0.1, 0.0],  % Opposite perturbation
        [0.0, 0.0, 0.2, 0.0]    % Larger perturbation
    ],

    lists:map(
        fun(State) ->
            evaluate_agent_once(AgentId, State)
        end,
        InitialStates
    );

generate_fingerprint_for_morphology(AgentId, _OtherMorphology) ->
    %% Generic fingerprint: evaluate on random inputs
    NumSamples = 5,
    RandomInputs = generate_random_inputs(AgentId, NumSamples),

    lists:map(
        fun(Input) ->
            evaluate_agent_once(AgentId, Input)
        end,
        RandomInputs
    ).

%% @private Evaluate agent on a single input and return output.
-spec evaluate_agent_once(term(), [float()]) -> float().
evaluate_agent_once(AgentId, Input) ->
    %% This is a simplified evaluation - in reality would construct
    %% phenotype and run through network
    case genotype:read({agent, AgentId}) of
        undefined -> 0.0;
        _Agent ->
            %% For now, return a deterministic but agent-specific value
            %% Real implementation would run the network
            Hash = erlang:phash2({AgentId, Input}),
            (Hash rem 1000) / 1000.0
    end.

%% @private Generate random test inputs for an agent.
-spec generate_random_inputs(term(), pos_integer()) -> [[float()]].
generate_random_inputs(AgentId, NumSamples) ->
    %% Get number of inputs from agent's morphology
    case genotype:read({agent, AgentId}) of
        undefined ->
            lists:duplicate(NumSamples, [0.0]);
        Agent ->
            CortexId = Agent#agent.cx_id,
            case genotype:read({cortex, CortexId}) of
                undefined ->
                    lists:duplicate(NumSamples, [0.0]);
                Cortex ->
                    %% Get first sensor to determine input size
                    case Cortex#cortex.sensor_ids of
                        [] ->
                            lists:duplicate(NumSamples, [0.0]);
                        [FirstSensorId | _] ->
                            Sensor = genotype:read({sensor, FirstSensorId}),
                            VectorLength = Sensor#sensor.vl,

                            %% Generate random inputs
                            lists:map(
                                fun(_) ->
                                    [rand:uniform() || _ <- lists:seq(1, VectorLength)]
                                end,
                                lists:seq(1, NumSamples)
                            )
                    end
            end
    end.

%% ============================================================================
%% LTC-Aware Distance Functions
%% ============================================================================

%% @doc Calculate distance based on LTC parameters between two agents.
%%
%% Compares LTC signatures (neuron types, time constants, state bounds)
%% between agents. Agents with similar LTC configurations will have
%% lower distance, enabling speciation by temporal dynamics.
%%
%% LTC Signature Components:
%% - Proportion of LTC/CfC neurons vs standard neurons
%% - Average time constant (tau) of LTC neurons
%% - Average state bound (A) of LTC neurons
%% - Standard deviation of tau values (diversity measure)
%%
%% @param Signature1 LTC signature from agent 1 (from extract_ltc_signature/1)
%% @param Signature2 LTC signature from agent 2
%% @returns Euclidean distance between LTC signatures
-spec calculate_ltc_distance(map(), map()) -> float().
calculate_ltc_distance(Signature1, Signature2) ->
    %% Extract components from signatures
    LtcRatio1 = maps:get(ltc_ratio, Signature1, 0.0),
    LtcRatio2 = maps:get(ltc_ratio, Signature2, 0.0),

    AvgTau1 = maps:get(avg_tau, Signature1, 1.0),
    AvgTau2 = maps:get(avg_tau, Signature2, 1.0),

    AvgBound1 = maps:get(avg_bound, Signature1, 1.0),
    AvgBound2 = maps:get(avg_bound, Signature2, 1.0),

    TauStd1 = maps:get(tau_std, Signature1, 0.0),
    TauStd2 = maps:get(tau_std, Signature2, 0.0),

    %% Compute weighted Euclidean distance
    %% Higher weight on ltc_ratio (fundamental difference in neuron types)
    RatioDiff = (LtcRatio1 - LtcRatio2) * 2.0,  %% Weight 2x
    TauDiff = (AvgTau1 - AvgTau2) / 10.0,       %% Normalize (tau can be large)
    BoundDiff = AvgBound1 - AvgBound2,
    StdDiff = TauStd1 - TauStd2,

    math:sqrt(RatioDiff * RatioDiff +
              TauDiff * TauDiff +
              BoundDiff * BoundDiff +
              StdDiff * StdDiff).

%% @doc Calculate combined distance using both behavior and LTC parameters.
%%
%% Combines behavioral fingerprint distance with LTC parameter distance
%% to create a comprehensive compatibility metric. This enables speciation
%% that considers both what agents do (behavior) and how they do it
%% (temporal dynamics).
%%
%% Formula: (BehaviorWeight * BehaviorDist) + (LtcWeight * LtcDist)
%%
%% @param BehaviorFingerprint1 Behavioral fingerprint of agent 1
%% @param BehaviorFingerprint2 Behavioral fingerprint of agent 2
%% @param LtcSignature1 LTC signature of agent 1
%% @param LtcSignature2 LTC signature of agent 2
%% @returns Combined weighted distance
-spec calculate_combined_distance([float()], [float()], map(), map()) -> float().
calculate_combined_distance(BehaviorFingerprint1, BehaviorFingerprint2,
                            LtcSignature1, LtcSignature2) ->
    %% Weights for combining distances
    BehaviorWeight = 0.7,   %% Behavior is primary
    LtcWeight = 0.3,        %% LTC params are secondary

    %% Calculate component distances
    BehaviorDist = calculate_distance(BehaviorFingerprint1, BehaviorFingerprint2),
    LtcDist = calculate_ltc_distance(LtcSignature1, LtcSignature2),

    %% Combine with weights
    BehaviorWeight * BehaviorDist + LtcWeight * LtcDist.

%% @doc Extract LTC signature from an agent's genome.
%%
%% Analyzes all neurons in the agent's network and extracts statistics
%% about LTC parameter usage. This creates a compact representation
%% of the agent's temporal dynamics characteristics.
%%
%% Signature Components:
%% - ltc_ratio: Proportion of LTC/CfC neurons (0.0 to 1.0)
%% - avg_tau: Average time constant of LTC neurons
%% - avg_bound: Average state bound of LTC neurons
%% - tau_std: Standard deviation of tau values
%% - ltc_count: Number of LTC/CfC neurons
%% - total_count: Total number of neurons
%%
%% @param AgentId Agent identifier
%% @returns Map containing LTC signature components
-spec extract_ltc_signature(term()) -> map().
extract_ltc_signature(AgentId) ->
    case genotype:read({agent, AgentId}) of
        undefined ->
            %% Agent not found, return default signature
            default_ltc_signature();
        Agent ->
            %% Get all neurons for this agent
            NeuronIds = get_all_neuron_ids(Agent),
            extract_ltc_signature_from_neurons(NeuronIds)
    end.

%% ============================================================================
%% Internal Functions - LTC Signature Extraction
%% ============================================================================

%% @private Get all neuron IDs for an agent.
-spec get_all_neuron_ids(#agent{}) -> [term()].
get_all_neuron_ids(Agent) ->
    CortexId = Agent#agent.cx_id,
    case genotype:read({cortex, CortexId}) of
        undefined -> [];
        Cortex -> Cortex#cortex.neuron_ids
    end.

%% @private Extract LTC signature from a list of neurons.
-spec extract_ltc_signature_from_neurons([term()]) -> map().
extract_ltc_signature_from_neurons([]) ->
    default_ltc_signature();
extract_ltc_signature_from_neurons(NeuronIds) ->
    %% Read all neurons and collect LTC data
    NeuronData = lists:filtermap(
        fun(NeuronId) ->
            case genotype:read({neuron, NeuronId}) of
                undefined -> false;
                Neuron -> {true, extract_neuron_ltc_data(Neuron)}
            end
        end,
        NeuronIds
    ),

    %% Separate LTC and standard neurons
    {LtcNeurons, _StandardNeurons} = lists:partition(
        fun(#{is_ltc := IsLtc}) -> IsLtc end,
        NeuronData
    ),

    TotalCount = length(NeuronData),
    LtcCount = length(LtcNeurons),

    case LtcCount of
        0 ->
            %% No LTC neurons
            #{
                ltc_ratio => 0.0,
                avg_tau => 1.0,
                avg_bound => 1.0,
                tau_std => 0.0,
                ltc_count => 0,
                total_count => TotalCount
            };
        _ ->
            %% Extract tau and bound values
            TauValues = [maps:get(tau, N) || N <- LtcNeurons],
            BoundValues = [maps:get(bound, N) || N <- LtcNeurons],

            AvgTau = lists:sum(TauValues) / LtcCount,
            AvgBound = lists:sum(BoundValues) / LtcCount,
            TauStd = calculate_std(TauValues, AvgTau),

            #{
                ltc_ratio => LtcCount / TotalCount,
                avg_tau => AvgTau,
                avg_bound => AvgBound,
                tau_std => TauStd,
                ltc_count => LtcCount,
                total_count => TotalCount
            }
    end.

%% @private Extract LTC data from a single neuron.
-spec extract_neuron_ltc_data(#neuron{}) -> map().
extract_neuron_ltc_data(Neuron) ->
    NeuronType = Neuron#neuron.neuron_type,
    IsLtc = (NeuronType =:= ltc) orelse (NeuronType =:= cfc),

    #{
        is_ltc => IsLtc,
        neuron_type => NeuronType,
        tau => Neuron#neuron.time_constant,
        bound => Neuron#neuron.state_bound
    }.

%% @private Return default LTC signature (all standard neurons).
-spec default_ltc_signature() -> map().
default_ltc_signature() ->
    #{
        ltc_ratio => 0.0,
        avg_tau => 1.0,
        avg_bound => 1.0,
        tau_std => 0.0,
        ltc_count => 0,
        total_count => 0
    }.

%% @private Calculate standard deviation.
-spec calculate_std([float()], float()) -> float().
calculate_std([], _Mean) -> 0.0;
calculate_std([_], _Mean) -> 0.0;
calculate_std(Values, Mean) ->
    SquaredDiffs = [(V - Mean) * (V - Mean) || V <- Values],
    Variance = lists:sum(SquaredDiffs) / length(Values),
    math:sqrt(Variance).
