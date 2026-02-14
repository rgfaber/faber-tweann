%% @doc Phenotype constructor for TWEANN networks.
%%
%% This module converts genotypes (genetic descriptions) into phenotypes
%% (running neural network processes). It spawns sensor, neuron, actuator,
%% and cortex processes based on the genotype specification.
%%
%% Based on DXNN2 by Gene Sher ("Handbook of Neuroevolution through Erlang").
%%
%% == Construction Process ==
%%
%% 1. Read agent genotype from Mnesia database
%% 2. Spawn all sensors, neurons, and actuators
%% 3. Build PID mappings from genotype IDs to process PIDs
%% 4. Spawn cortex to coordinate the network
%% 5. Return phenotype record with all PIDs
%%
%% == Phenotype Record ==
%%
%% The phenotype is a map containing:
%% - agent_id: Original agent ID
%% - cortex_pid: PID of the cortex process
%% - sensor_pids: List of sensor PIDs
%% - neuron_pids: List of neuron PIDs
%% - actuator_pids: List of actuator PIDs
%% - id_to_pid: Map from genotype IDs to PIDs
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(constructor).

-include("records.hrl").

-export([
    construct/1,
    construct/2,
    terminate/1
]).

%%==============================================================================
%% Public API
%%==============================================================================

%% @doc Construct a phenotype from an agent genotype.
%%
%% Spawns all neural network processes and returns a phenotype record.
%%
%% @param AgentId The ID of the agent genotype
%% @returns Phenotype map with all process PIDs
-spec construct(term()) -> #{
    agent_id := term(),
    cortex_pid := pid(),
    sensor_pids := [pid()],
    neuron_pids := [pid()],
    actuator_pids := [pid()],
    id_to_pid := map()
}.
construct(AgentId) ->
    construct(AgentId, #{}).

%% @doc Construct a phenotype with options.
%%
%% Options:
%% - exoself_pid: PID to receive cortex messages (default: self())
%% - scape_pid: PID of the environment/scape (default: undefined)
%%
%% @param AgentId The ID of the agent genotype
%% @param Opts Construction options
%% @returns Phenotype map with all process PIDs
-spec construct(term(), map()) -> #{
    agent_id := term(),
    cortex_pid := pid(),
    sensor_pids := [pid()],
    neuron_pids := [pid()],
    actuator_pids := [pid()],
    id_to_pid := map()
}.
construct(AgentId, Opts) ->
    ExoselfPid = maps:get(exoself_pid, Opts, self()),
    ScapePid = maps:get(scape_pid, Opts, undefined),

    %% Read agent and cortex from Mnesia (returns records)
    Agent = genotype:dirty_read({agent, AgentId}),
    CortexId = Agent#agent.cx_id,
    Cortex = genotype:dirty_read({cortex, CortexId}),

    %% Get component IDs from records
    SensorIds = Cortex#cortex.sensor_ids,
    NeuronIds = Cortex#cortex.neuron_ids,
    ActuatorIds = Cortex#cortex.actuator_ids,

    %% Spawn all components (without connections yet)
    {SensorPids, SensorIdMap} = spawn_sensors(SensorIds, ScapePid),
    {NeuronPids, NeuronIdMap} = spawn_neurons(NeuronIds),
    {ActuatorPids, ActuatorIdMap} = spawn_actuators(ActuatorIds, ScapePid),

    %% Build complete ID to PID mapping
    IdToPid = maps:merge(maps:merge(SensorIdMap, NeuronIdMap), ActuatorIdMap),

    %% Link components with actual PIDs
    link_sensors(SensorIds, IdToPid),
    link_neurons(NeuronIds, IdToPid),
    link_actuators(ActuatorIds, IdToPid),

    %% Spawn cortex
    {ok, CortexPid} = cortex:start_link(#{
        id => CortexId,
        exoself_pid => ExoselfPid,
        sensor_pids => SensorPids,
        neuron_pids => NeuronPids,
        actuator_pids => ActuatorPids
    }),

    %% Return phenotype
    #{
        agent_id => AgentId,
        cortex_pid => CortexPid,
        sensor_pids => SensorPids,
        neuron_pids => NeuronPids,
        actuator_pids => ActuatorPids,
        id_to_pid => IdToPid
    }.

%% @doc Terminate a phenotype (stop all processes).
%%
%% Sends terminate signal to the cortex, which terminates all components.
%%
%% @param Phenotype The phenotype map to terminate
-spec terminate(map()) -> ok.
terminate(Phenotype) ->
    CortexPid = maps:get(cortex_pid, Phenotype),
    CortexPid ! {exoself, stop},
    ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% Spawn sensor processes (without fanout connections)
spawn_sensors(SensorIds, ScapePid) ->
    {Pids, IdMap} = lists:foldl(
        fun(SensorId, {AccPids, AccMap}) ->
            Sensor = genotype:dirty_read({sensor, SensorId}),
            {ok, Pid} = sensor:start_link(#{
                id => SensorId,
                cortex_pid => undefined,  % Will not be used
                sensor_name => Sensor#sensor.name,
                vector_length => Sensor#sensor.vl,
                fanout_pids => [],  % Linked later
                scape_pid => ScapePid,
                parameters => case Sensor#sensor.parameters of
                    undefined -> [];
                    Params -> Params
                end
            }),
            {AccPids ++ [Pid], AccMap#{SensorId => Pid}}
        end,
        {[], #{}},
        SensorIds
    ),
    {Pids, IdMap}.

%% Spawn neuron processes (without connections)
%% Supports both standard neurons and LTC neurons based on neuron_type field
spawn_neurons(NeuronIds) ->
    {Pids, IdMap} = lists:foldl(
        fun(NeuronId, {AccPids, AccMap}) ->
            Neuron = genotype:dirty_read({neuron, NeuronId}),

            %% Get input_idps from record
            InputIdps = Neuron#neuron.input_idps,

            %% Spawn appropriate neuron type
            {ok, Pid} = spawn_neuron_by_type(Neuron, NeuronId),

            %% Store genotype input weights for later linking
            put({neuron_idps, NeuronId}, InputIdps),

            {AccPids ++ [Pid], AccMap#{NeuronId => Pid}}
        end,
        {[], #{}},
        NeuronIds
    ),
    {Pids, IdMap}.

%% @private Spawn a neuron based on its type (standard, ltc, or cfc)
spawn_neuron_by_type(Neuron, NeuronId) ->
    NeuronType = Neuron#neuron.neuron_type,

    case NeuronType of
        ltc ->
            spawn_ltc_neuron(Neuron, NeuronId, ltc);
        cfc ->
            spawn_ltc_neuron(Neuron, NeuronId, cfc);
        _ ->
            %% Standard neuron (default)
            spawn_standard_neuron(Neuron, NeuronId)
    end.

%% @private Spawn a standard neuron
spawn_standard_neuron(Neuron, NeuronId) ->
    %% Get activation function with default
    AF = case Neuron#neuron.af of
        undefined -> tanh;
        Val -> Val
    end,

    %% Get aggregation function with default
    AggrF = case Neuron#neuron.aggr_f of
        undefined -> dot_product;
        Val2 -> Val2
    end,

    neuron:start_link(#{
        id => NeuronId,
        cortex_pid => undefined,  % Will be set by cortex
        activation_function => AF,
        aggregation_function => AggrF,
        input_pids => [],  % Linked later
        output_pids => [],  % Linked later
        ro_pids => [],  % Linked later
        input_weights => #{},  % Linked later
        bias => 0.0  % Bias is part of input_idps in DXNN2
    }).

%% @private Spawn an LTC (Liquid Time-Constant) neuron
%%
%% LTC neurons have input-dependent time constants for adaptive temporal
%% processing. They can use either ODE-based evaluation (ltc) or the fast
%% closed-form approximation (cfc).
spawn_ltc_neuron(Neuron, NeuronId, Type) ->
    %% Get LTC-specific parameters from the neuron record
    TimeConstant = Neuron#neuron.time_constant,
    StateBound = Neuron#neuron.state_bound,
    BackboneWeights = Neuron#neuron.ltc_backbone_weights,
    HeadWeights = Neuron#neuron.ltc_head_weights,
    InternalState = Neuron#neuron.internal_state,

    neuron_ltc:start_link(#{
        id => NeuronId,
        cortex_pid => undefined,  % Will be set by cortex
        neuron_type => Type,
        time_constant => TimeConstant,
        state_bound => StateBound,
        ltc_backbone_weights => BackboneWeights,
        ltc_head_weights => HeadWeights,
        internal_state => InternalState,
        input_pids => [],  % Linked later
        output_pids => [],  % Linked later
        ro_pids => [],  % Linked later
        input_weights => #{},  % Linked later
        bias => 0.0  % Bias is part of input_idps in DXNN2
    }).

%% Spawn actuator processes (without fanin connections)
spawn_actuators(ActuatorIds, ScapePid) ->
    {Pids, IdMap} = lists:foldl(
        fun(ActuatorId, {AccPids, AccMap}) ->
            Actuator = genotype:dirty_read({actuator, ActuatorId}),
            {ok, Pid} = actuator:start_link(#{
                id => ActuatorId,
                cortex_pid => undefined,  % Will be set by cortex
                actuator_name => Actuator#actuator.name,
                vector_length => Actuator#actuator.vl,
                fanin_pids => [],  % Linked later
                scape_pid => ScapePid,
                parameters => case Actuator#actuator.parameters of
                    undefined -> [];
                    Params -> Params
                end
            }),
            {AccPids ++ [Pid], AccMap#{ActuatorId => Pid}}
        end,
        {[], #{}},
        ActuatorIds
    ),
    {Pids, IdMap}.

%% Link sensors to their fanout neurons
link_sensors(SensorIds, IdToPid) ->
    lists:foreach(
        fun(SensorId) ->
            Sensor = genotype:dirty_read({sensor, SensorId}),
            SensorPid = maps:get(SensorId, IdToPid),
            FanoutIds = Sensor#sensor.fanout_ids,
            FanoutPids = [maps:get(Id, IdToPid) || Id <- FanoutIds],

            %% Update sensor with actual fanout PIDs
            SensorPid ! {link, fanout_pids, FanoutPids}
        end,
        SensorIds
    ).

%% Link neurons to their inputs and outputs
link_neurons(NeuronIds, IdToPid) ->
    lists:foreach(
        fun(NeuronId) ->
            Neuron = genotype:dirty_read({neuron, NeuronId}),
            NeuronPid = maps:get(NeuronId, IdToPid),

            %% Get stored input_idps
            InputIdps = get({neuron_idps, NeuronId}),
            _ = erase({neuron_idps, NeuronId}),

            %% Convert to PID-based weights map
            InputWeights = lists:foldl(
                fun({InputId, Weights}, Acc) ->
                    InputPid = maps:get(InputId, IdToPid),
                    Acc#{InputPid => Weights}
                end,
                #{},
                InputIdps
            ),

            InputPids = [maps:get(InputId, IdToPid) || {InputId, _} <- InputIdps],

            %% Get output PIDs from record
            OutputIds = Neuron#neuron.output_ids,
            OutputPids = [maps:get(Id, IdToPid) || Id <- OutputIds],

            %% Get recurrent output PIDs from record
            RoIds = Neuron#neuron.ro_ids,
            RoPids = [maps:get(Id, IdToPid) || Id <- RoIds],

            %% Update neuron with actual PIDs
            NeuronPid ! {link, input_pids, InputPids},
            NeuronPid ! {link, output_pids, OutputPids},
            NeuronPid ! {link, ro_pids, RoPids},
            NeuronPid ! {link, input_weights, InputWeights}
        end,
        NeuronIds
    ).

%% Link actuators to their fanin neurons
link_actuators(ActuatorIds, IdToPid) ->
    lists:foreach(
        fun(ActuatorId) ->
            Actuator = genotype:dirty_read({actuator, ActuatorId}),
            ActuatorPid = maps:get(ActuatorId, IdToPid),
            FaninIds = Actuator#actuator.fanin_ids,
            FaninPids = [maps:get(Id, IdToPid) || Id <- FaninIds],

            %% Update actuator with actual fanin PIDs
            ActuatorPid ! {link, fanin_pids, FaninPids}
        end,
        ActuatorIds
    ).
