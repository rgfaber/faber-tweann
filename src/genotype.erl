%% @doc Genotype representation for TWEANN networks.
%%
%% This module provides the genetic encoding for neural networks using ETS
%% for in-memory storage. A genotype describes the network topology and
%% parameters that can be evolved, then converted to a running phenotype.
%%
%% Based on DXNN2 by Gene Sher ("Handbook of Neuroevolution through Erlang").
%%
%% == Genotype Structure ==
%%
%% A genotype is a collection of interconnected elements stored in Mnesia:
%%
%% - Agent - Top-level container for a neural network
%% - Cortex - Network coordinator, references sensors/neurons/actuators
%% - Sensor - Input interface with fanout connections
%% - Neuron - Processing unit with weighted inputs and outputs
%% - Actuator - Output interface with fanin connections
%%
%% == ID Format ==
%%
%% Each element has a unique ID in the format:
%% `{{LayerCoord, UniqueFloat}, Type}'
%%
%% Layer coordinates:
%% - Sensors: -1.0
%% - Hidden neurons: 0.0 to 1.0
%% - Actuators: 1.0
%% - Cortex: origin
%%
%% == Weight Format ==
%%
%% Neuron input weights use the tuple format:
%% `{Weight, DeltaWeight, LearningRate, ParameterList}'
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(genotype).

-include("records.hrl").

%% Suppress supertype warnings - specs are intentionally general for API flexibility
-dialyzer({nowarn_function, [
    dirty_read/1,
    construct_SeedNN/6,
    create_InitPattern/1,
    construct_Neuron/6,
    link_Neuron/4,
    link_FromElementToElement/3,
    clone_Agent/1,
    random_element/1,
    generate_id/1
]}).

-export([
    %% Database operations
    init_db/0,
    reset_db/0,

    %% Core operations
    read/1,
    write/1,
    dirty_read/1,
    delete/1,

    %% Agent construction
    construct_Agent/3,
    clone_Agent/1,
    delete_Agent/1,

    %% Utility
    generate_UniqueId/0,
    generate_id/1,
    random_element/1,
    update_fingerprint/1
]).

%%==============================================================================
%% Database Operations
%%==============================================================================

%% @doc Initialize ETS tables for genotype storage.
%%
%% Creates ETS tables for all genotype elements.
%% Should be called once at application startup.
%% ETS tables are faster than Mnesia for this workload since we don't
%% need distribution or disk persistence.
-spec init_db() -> ok.
init_db() ->
    %% Create ETS tables for each record type
    Tables = [agent, cortex, sensor, actuator, neuron, substrate, specie, population],

    lists:foreach(
        fun(Name) ->
            case ets:whereis(Name) of
                undefined ->
                    ets:new(Name, [set, public, named_table, {keypos, 2},
                                   {read_concurrency, true}]);
                _Tid ->
                    %% Table already exists
                    ok
            end
        end,
        Tables
    ),
    ok.

%% @doc Reset database by clearing all tables.
-spec reset_db() -> ok.
reset_db() ->
    Tables = [agent, cortex, sensor, actuator, neuron, substrate, specie, population],
    lists:foreach(
        fun(Table) ->
            case ets:whereis(Table) of
                undefined -> ok;
                _Tid -> ets:delete_all_objects(Table)
            end
        end,
        Tables
    ),
    ok.

%%==============================================================================
%% Core Operations
%%==============================================================================

%% @doc Read a record from ETS.
%%
%% Key format: {Table, RecordKey} where RecordKey is the record's id field.
-spec read(tuple()) -> tuple() | undefined.
read({Table, Key}) ->
    case ets:lookup(Table, Key) of
        [Record] -> Record;
        [] -> undefined
    end.

%% Maximum entries to keep in evo_hist (evolution history)
-define(MAX_EVO_HIST_SIZE, 50).

%% @doc Write a record to ETS.
%%
%% The record type determines which table to use (first element of record tuple).
%% For agent records, the evo_hist is capped to prevent unbounded growth.
-spec write(tuple()) -> ok.
write(Record) when element(1, Record) =:= agent ->
    %% Cap evo_hist for agent records to prevent unbounded growth
    CappedRecord = cap_evo_hist(Record),
    true = ets:insert(agent, CappedRecord),
    ok;
write(Record) ->
    Table = element(1, Record),
    true = ets:insert(Table, Record),
    ok.

%% @private Cap evo_hist to prevent unbounded memory growth
-spec cap_evo_hist(#agent{}) -> #agent{}.
cap_evo_hist(Agent) ->
    EvoHist = Agent#agent.evo_hist,
    CappedHist = case length(EvoHist) > ?MAX_EVO_HIST_SIZE of
        true -> lists:sublist(EvoHist, ?MAX_EVO_HIST_SIZE);
        false -> EvoHist
    end,
    Agent#agent{evo_hist = CappedHist}.

%% @doc Read a record directly from ETS (same as read/1 for ETS).
%%
%% This function exists for API compatibility with the old Mnesia interface.
-spec dirty_read(tuple()) -> tuple() | undefined.
dirty_read({Table, Key}) ->
    case ets:lookup(Table, Key) of
        [Record] -> Record;
        [] -> undefined
    end.

%% @doc Delete a record from ETS.
-spec delete(tuple()) -> ok.
delete({Table, Key}) ->
    true = ets:delete(Table, Key),
    ok.

%%==============================================================================
%% Agent Construction
%%==============================================================================

%% @doc Construct a new agent with neural network.
%%
%% Creates a complete agent genotype based on the species constraint.
%% The morphology in the constraint defines sensors and actuators.
%%
%% @param Specie_Id Species this agent belongs to
%% @param Agent_Id Unique identifier for the agent
%% @param SpecCon Constraint record defining evolution parameters
%% @returns Agent_Id
-spec construct_Agent(term(), term(), #constraint{}) -> term().
construct_Agent(Specie_Id, Agent_Id, SpecCon) ->
    Generation = 0,
    Encoding_Type = random_element(SpecCon#constraint.agent_encoding_types),
    SPlasticity = random_element(SpecCon#constraint.substrate_plasticities),
    SLinkform = random_element(SpecCon#constraint.substrate_linkforms),

    {Cx_Id, Pattern, Substrate_Id} = construct_Cortex(
        Agent_Id, Generation, SpecCon, Encoding_Type, SPlasticity, SLinkform
    ),

    Agent = #agent{
        id = Agent_Id,
        encoding_type = Encoding_Type,
        cx_id = Cx_Id,
        specie_id = Specie_Id,
        constraint = SpecCon,
        generation = Generation,
        pattern = Pattern,
        tuning_selection_f = random_element(SpecCon#constraint.tuning_selection_fs),
        annealing_parameter = random_element(SpecCon#constraint.annealing_parameters),
        tuning_duration_f = SpecCon#constraint.tuning_duration_f,
        perturbation_range = random_element(SpecCon#constraint.perturbation_ranges),
        mutation_operators = SpecCon#constraint.mutation_operators,
        tot_topological_mutations_f = random_element(SpecCon#constraint.tot_topological_mutations_fs),
        heredity_type = random_element(SpecCon#constraint.heredity_types),
        evo_hist = [],
        substrate_id = Substrate_Id
    },
    write(Agent),
    update_fingerprint(Agent_Id),
    Agent_Id.

%% @doc Construct cortex with sensors, neurons, and actuators.
-spec construct_Cortex(term(), integer(), #constraint{}, atom(), atom(), atom()) ->
    {term(), list(), term()}.
construct_Cortex(Agent_Id, Generation, SpecCon, Encoding_Type, _SPlasticity, _SLinkform) ->
    Cx_Id = {{origin, generate_UniqueId()}, cortex},
    Morphology = SpecCon#constraint.morphology,

    case Encoding_Type of
        neural ->
            %% Get initial sensors and actuators from morphology
            Sensors = [S#sensor{
                id = {{-1, generate_UniqueId()}, sensor},
                cx_id = Cx_Id,
                generation = Generation
            } || S <- morphology:get_InitSensors(Morphology)],

            Actuators = [A#actuator{
                id = {{1, generate_UniqueId()}, actuator},
                cx_id = Cx_Id,
                generation = Generation
            } || A <- morphology:get_InitActuators(Morphology)],

            %% Write sensors and actuators
            [write(S) || S <- Sensors],
            [write(A) || A <- Actuators],

            %% Construct initial neural network
            {N_Ids, Pattern} = construct_SeedNN(Cx_Id, Generation, SpecCon, Sensors, Actuators, []),

            S_Ids = [S#sensor.id || S <- Sensors],
            A_Ids = [A#actuator.id || A <- Actuators],

            Cortex = #cortex{
                id = Cx_Id,
                agent_id = Agent_Id,
                neuron_ids = N_Ids,
                sensor_ids = S_Ids,
                actuator_ids = A_Ids
            },
            write(Cortex),

            {Cx_Id, Pattern, undefined};

        substrate ->
            %% Substrate encoding not yet implemented
            erlang:error(substrate_not_implemented)
    end.

%% @doc Construct seed neural network (initial topology).
-spec construct_SeedNN(term(), integer(), #constraint{}, [#sensor{}], [#actuator{}], list()) ->
    {[term()], list()}.
construct_SeedNN(Cx_Id, Generation, SpecCon, Sensors, [A | Actuators], Acc) ->
    %% Create one neuron per actuator output
    N_Ids = [{{0, generate_UniqueId()}, neuron} || _ <- lists:seq(1, A#actuator.vl)],

    %% Construct each neuron
    [construct_Neuron(Cx_Id, Generation, SpecCon, N_Id, [], []) || N_Id <- N_Ids],

    %% Link neurons: sensors -> neurons -> actuator
    [link_Neuron(Generation, [S#sensor.id || S <- Sensors], N_Id, [A#actuator.id])
     || N_Id <- N_Ids],

    construct_SeedNN(Cx_Id, Generation, SpecCon, Sensors, Actuators, lists:append(N_Ids, Acc));

construct_SeedNN(_Cx_Id, _Generation, _SpecCon, _Sensors, [], Acc) ->
    {lists:reverse(Acc), create_InitPattern(Acc)}.

%% @doc Create initial layer pattern from neuron IDs.
-spec create_InitPattern([term()]) -> list().
create_InitPattern([]) -> [];
create_InitPattern([Id | Ids]) ->
    {{LI, _}, _} = Id,
    create_InitPattern(Ids, LI, [Id], []).

create_InitPattern([Id | Ids], CurIndex, CurIndexAcc, PatternAcc) ->
    {{LI, _}, _} = Id,
    case LI == CurIndex of
        true ->
            create_InitPattern(Ids, CurIndex, [Id | CurIndexAcc], PatternAcc);
        false ->
            create_InitPattern(Ids, LI, [Id], [{CurIndex, CurIndexAcc} | PatternAcc])
    end;
create_InitPattern([], CurIndex, CurIndexAcc, PatternAcc) ->
    lists:sort([{CurIndex, CurIndexAcc} | PatternAcc]).

%% @doc Construct a single neuron.
-spec construct_Neuron(term(), integer(), #constraint{}, term(), list(), list()) -> ok.
construct_Neuron(Cx_Id, Generation, SpecCon, N_Id, Input_Specs, Output_Ids) ->
    PF = generate_NeuronPF(SpecCon#constraint.neural_pfns),
    AF = generate_NeuronAF(SpecCon#constraint.neural_afs),
    AggrF = generate_NeuronAggrF(SpecCon#constraint.neural_aggr_fs),

    Input_IdPs = create_InputIdPs(Input_Specs, []),

    Neuron = #neuron{
        id = N_Id,
        cx_id = Cx_Id,
        generation = Generation,
        af = AF,
        pf = PF,
        aggr_f = AggrF,
        input_idps = Input_IdPs,
        output_ids = Output_Ids,
        ro_ids = calculate_ROIds(N_Id, Output_Ids, [])
    },
    write(Neuron).

%% @doc Link a neuron to its inputs and outputs.
-spec link_Neuron(integer(), [term()], term(), [term()]) -> ok.
link_Neuron(Generation, From_Ids, N_Id, To_Ids) ->
    [link_FromElementToElement(Generation, From_Id, N_Id) || From_Id <- From_Ids],
    [link_FromElementToElement(Generation, N_Id, To_Id) || To_Id <- To_Ids],
    ok.

%% @doc Create a link between two elements.
%%
%% Creates the appropriate connections between neural network elements:
%% - sensor to neuron: Updates sensor fanout and neuron input weights
%% - neuron to neuron: Updates output/input connections with weights
%% - neuron to actuator: Updates neuron output and actuator fanin
-spec link_FromElementToElement(integer(), term(), term()) -> ok.
link_FromElementToElement(_Generation, FromId, ToId) ->
    %% Get source element type
    {_, FromType} = FromId,
    {_, ToType} = ToId,

    case {FromType, ToType} of
        {sensor, neuron} ->
            %% Update sensor fanout
            Sensor = dirty_read({sensor, FromId}),
            write(Sensor#sensor{fanout_ids = [ToId | Sensor#sensor.fanout_ids]}),

            %% Update neuron input
            Neuron = dirty_read({neuron, ToId}),
            VL = Sensor#sensor.vl,
            Weights = create_neural_weights(VL),
            NewInputIdPs = [{FromId, Weights} | Neuron#neuron.input_idps],
            write(Neuron#neuron{input_idps = NewInputIdPs});

        {neuron, neuron} ->
            %% Update source neuron output
            FromNeuron = dirty_read({neuron, FromId}),
            write(FromNeuron#neuron{output_ids = [ToId | FromNeuron#neuron.output_ids]}),

            %% Update target neuron input
            ToNeuron = dirty_read({neuron, ToId}),
            Weights = create_neural_weights(1),
            NewInputIdPs = [{FromId, Weights} | ToNeuron#neuron.input_idps],
            write(ToNeuron#neuron{input_idps = NewInputIdPs});

        {neuron, actuator} ->
            %% Update neuron output
            Neuron = dirty_read({neuron, FromId}),
            write(Neuron#neuron{output_ids = [ToId | Neuron#neuron.output_ids]}),

            %% Update actuator fanin
            Actuator = dirty_read({actuator, ToId}),
            write(Actuator#actuator{fanin_ids = [FromId | Actuator#actuator.fanin_ids]})
    end,
    ok.

%% @doc Clone an agent and all its components.
-spec clone_Agent(term()) -> term().
clone_Agent(Agent_Id) ->
    Agent = dirty_read({agent, Agent_Id}),
    Cortex = dirty_read({cortex, Agent#agent.cx_id}),

    %% Generate new IDs
    NewAgent_Id = {generate_UniqueId(), agent},
    NewCx_Id = {{origin, generate_UniqueId()}, cortex},

    %% Clone and remap sensors
    SensorIds = Cortex#cortex.sensor_ids,
    {NewSensorIds, SensorIdMap} = clone_elements(sensor, SensorIds, NewCx_Id),

    %% Clone and remap neurons
    NeuronIds = Cortex#cortex.neuron_ids,
    {NewNeuronIds, NeuronIdMap} = clone_elements(neuron, NeuronIds, NewCx_Id),

    %% Clone and remap actuators
    ActuatorIds = Cortex#cortex.actuator_ids,
    {NewActuatorIds, ActuatorIdMap} = clone_elements(actuator, ActuatorIds, NewCx_Id),

    %% Build complete ID mapping
    IdMap = maps:merge(maps:merge(SensorIdMap, NeuronIdMap), ActuatorIdMap),

    %% Update references in cloned elements
    update_cloned_elements(sensor, NewSensorIds, IdMap),
    update_cloned_elements(neuron, NewNeuronIds, IdMap),
    update_cloned_elements(actuator, NewActuatorIds, IdMap),

    %% Write new cortex
    NewCortex = Cortex#cortex{
        id = NewCx_Id,
        agent_id = NewAgent_Id,
        sensor_ids = NewSensorIds,
        neuron_ids = NewNeuronIds,
        actuator_ids = NewActuatorIds
    },
    write(NewCortex),

    %% Write new agent
    NewAgent = Agent#agent{
        id = NewAgent_Id,
        cx_id = NewCx_Id,
        generation = Agent#agent.generation + 1,
        offspring_ids = [],
        parent_ids = [Agent_Id]
    },
    write(NewAgent),

    NewAgent_Id.

%% @doc Delete an agent and all its components.
-spec delete_Agent(term()) -> ok.
delete_Agent(Agent_Id) ->
    Agent = dirty_read({agent, Agent_Id}),
    case Agent of
        undefined -> ok;
        _ ->
            Cortex = dirty_read({cortex, Agent#agent.cx_id}),
            case Cortex of
                undefined -> ok;
                _ ->
                    %% Delete all components
                    [delete({sensor, Id}) || Id <- Cortex#cortex.sensor_ids],
                    [delete({neuron, Id}) || Id <- Cortex#cortex.neuron_ids],
                    [delete({actuator, Id}) || Id <- Cortex#cortex.actuator_ids],
                    delete({cortex, Cortex#cortex.id})
            end,
            delete({agent, Agent_Id})
    end,
    ok.

%%==============================================================================
%% Utility Functions
%%==============================================================================

%% @doc Generate a unique float identifier.
-spec generate_UniqueId() -> float().
generate_UniqueId() ->
    rand:uniform().

%% @doc Generate an ID for a specific element type.
%%
%% Creates a properly formatted ID tuple based on the element type:
%% - sensor: {{-1, UniqueFloat}, sensor}
%% - neuron: {{0, UniqueFloat}, neuron} (hidden layer)
%% - actuator: {{1, UniqueFloat}, actuator}
%% - cortex: {{origin, UniqueFloat}, cortex}
%% - agent: {UniqueFloat, agent}
%%
%% @param Type the element type atom
%% @returns properly formatted ID tuple
-spec generate_id(atom()) -> term().
generate_id(sensor) ->
    {{-1, generate_UniqueId()}, sensor};
generate_id(neuron) ->
    {{0, generate_UniqueId()}, neuron};
generate_id(actuator) ->
    {{1, generate_UniqueId()}, actuator};
generate_id(cortex) ->
    {{origin, generate_UniqueId()}, cortex};
generate_id(agent) ->
    {generate_UniqueId(), agent}.

%% @doc Select a random element from a list.
-spec random_element([T]) -> T when T :: term().
random_element(List) ->
    lists:nth(rand:uniform(length(List)), List).

%% @doc Update agent fingerprint for speciation.
%%
%% Calculates the behavioral fingerprint using species_identifier:create_fingerprint/1
%% and stores it in the agent record. The fingerprint is used for behavioral
%% distance calculations during speciation.
-spec update_fingerprint(term()) -> ok.
update_fingerprint(Agent_Id) ->
    case dirty_read({agent, Agent_Id}) of
        Agent when is_record(Agent, agent) ->
            Fingerprint = species_identifier:create_fingerprint(Agent_Id),
            write(Agent#agent{fingerprint = Fingerprint}),
            ok;
        _ ->
            ok  %% Agent not found, nothing to update
    end.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% Generate neuron activation function
generate_NeuronAF(AFs) ->
    random_element(AFs).

%% Generate neuron plasticity function
generate_NeuronPF(PFs) ->
    PFName = random_element(PFs),
    {PFName, []}.

%% Generate neuron aggregation function
generate_NeuronAggrF(AggrFs) ->
    random_element(AggrFs).

%% Create input weight list
%% Note: Currently called with empty list during initial construction.
%% Will be used with non-empty lists when genome_mutator mutations are added.
-dialyzer({no_match, create_InputIdPs/2}).
create_InputIdPs([], Acc) -> Acc;
create_InputIdPs([{Id, VL} | Rest], Acc) ->
    Weights = create_neural_weights(VL),
    create_InputIdPs(Rest, [{Id, Weights} | Acc]).

%% Create neural weights in DXNN2 format
create_neural_weights(VL) ->
    [{rand:uniform() * 2 - 1, 0.0, 0.1, []} || _ <- lists:seq(1, VL)].

%% Calculate recurrent output IDs
%% Note: Currently called with empty list during initial construction.
%% Will be used with non-empty lists when genome_mutator mutations are added.
-dialyzer({no_match, calculate_ROIds/3}).
calculate_ROIds(_N_Id, [], Acc) -> Acc;
calculate_ROIds(N_Id, [OutputId | Rest], Acc) ->
    {{N_Layer, _}, _} = N_Id,
    {{O_Layer, _}, _} = OutputId,
    case O_Layer =< N_Layer of
        true -> calculate_ROIds(N_Id, Rest, [OutputId | Acc]);
        false -> calculate_ROIds(N_Id, Rest, Acc)
    end.

%% Clone elements and create ID mapping
clone_elements(Type, Ids, NewCx_Id) ->
    lists:foldl(
        fun(OldId, {AccIds, AccMap}) ->
            Record = dirty_read({Type, OldId}),
            NewId = case Type of
                sensor -> {{-1, generate_UniqueId()}, sensor};
                neuron ->
                    {{Layer, _}, _} = OldId,
                    {{Layer, generate_UniqueId()}, neuron};
                actuator -> {{1, generate_UniqueId()}, actuator}
            end,

            NewRecord = case Type of
                sensor -> Record#sensor{id = NewId, cx_id = NewCx_Id};
                neuron -> Record#neuron{id = NewId, cx_id = NewCx_Id};
                actuator -> Record#actuator{id = NewId, cx_id = NewCx_Id}
            end,
            write(NewRecord),

            {AccIds ++ [NewId], AccMap#{OldId => NewId}}
        end,
        {[], #{}},
        Ids
    ).

%% Update cloned elements with remapped IDs
update_cloned_elements(sensor, Ids, IdMap) ->
    lists:foreach(
        fun(Id) ->
            Sensor = dirty_read({sensor, Id}),
            NewFanoutIds = [maps:get(FId, IdMap, FId) || FId <- Sensor#sensor.fanout_ids],
            write(Sensor#sensor{fanout_ids = NewFanoutIds})
        end,
        Ids
    );

update_cloned_elements(neuron, Ids, IdMap) ->
    lists:foreach(
        fun(Id) ->
            Neuron = dirty_read({neuron, Id}),

            NewInputIdPs = [{maps:get(IId, IdMap, IId), W}
                            || {IId, W} <- Neuron#neuron.input_idps],
            NewOutputIds = [maps:get(OId, IdMap, OId)
                           || OId <- Neuron#neuron.output_ids],
            NewRoIds = [maps:get(RId, IdMap, RId)
                       || RId <- Neuron#neuron.ro_ids],

            write(Neuron#neuron{
                input_idps = NewInputIdPs,
                output_ids = NewOutputIds,
                ro_ids = NewRoIds
            })
        end,
        Ids
    );

update_cloned_elements(actuator, Ids, IdMap) ->
    lists:foreach(
        fun(Id) ->
            Actuator = dirty_read({actuator, Id}),
            NewFaninIds = [maps:get(FId, IdMap, FId) || FId <- Actuator#actuator.fanin_ids],
            write(Actuator#actuator{fanin_ids = NewFaninIds})
        end,
        Ids
    ).
