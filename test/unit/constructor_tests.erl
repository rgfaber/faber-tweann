-module(constructor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% =============================================================================
%% Setup/Teardown
%% =============================================================================

setup() ->
    %% Ensure application started (for morphology registry)
    application:ensure_all_started(faber_tweann),

    %% Register example morphologies
    test_helper:register_all_example_morphologies(),

    %% Initialize ETS tables (no Mnesia needed)
    genotype:init_db(),
    innovation:init().

teardown(_) ->
    genotype:reset_db().

%% =============================================================================
%% Test Generator
%% =============================================================================

constructor_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        fun construct_simple_network/0,
        fun phenotype_has_all_pids/0,
        fun id_to_pid_mapping/0,
        fun terminate_phenotype/0,
        fun construct_xor_network/0
     ]}.

%% =============================================================================
%% Construction Tests
%% =============================================================================

construct_simple_network() ->
    %% Create constraint with xor_mimic morphology
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Phenotype = constructor:construct(AgentId),

    ?assert(is_map(Phenotype)),
    ?assertEqual(AgentId, maps:get(agent_id, Phenotype)),
    ?assert(is_pid(maps:get(cortex_pid, Phenotype))),

    constructor:terminate(Phenotype).

%% =============================================================================
%% Phenotype Structure Tests
%% =============================================================================

phenotype_has_all_pids() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Phenotype = constructor:construct(AgentId),

    %% All PIDs should be actual processes
    CortexPid = maps:get(cortex_pid, Phenotype),
    ?assert(is_process_alive(CortexPid)),

    SensorPids = maps:get(sensor_pids, Phenotype),
    lists:foreach(fun(Pid) ->
        ?assert(is_process_alive(Pid))
    end, SensorPids),

    NeuronPids = maps:get(neuron_pids, Phenotype),
    lists:foreach(fun(Pid) ->
        ?assert(is_process_alive(Pid))
    end, NeuronPids),

    ActuatorPids = maps:get(actuator_pids, Phenotype),
    lists:foreach(fun(Pid) ->
        ?assert(is_process_alive(Pid))
    end, ActuatorPids),

    constructor:terminate(Phenotype).

id_to_pid_mapping() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Phenotype = constructor:construct(AgentId),

    IdToPid = maps:get(id_to_pid, Phenotype),

    %% Should have entries for sensor, neuron, and actuator
    %% xor_mimic has 1 sensor, 1 neuron, 1 actuator = 3
    ?assertEqual(3, maps:size(IdToPid)),

    %% All values should be PIDs
    maps:foreach(fun(_Id, Pid) ->
        ?assert(is_pid(Pid))
    end, IdToPid),

    constructor:terminate(Phenotype).

%% =============================================================================
%% Terminate Tests
%% =============================================================================

terminate_phenotype() ->
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Phenotype = constructor:construct(AgentId),

    CortexPid = maps:get(cortex_pid, Phenotype),
    ?assert(is_process_alive(CortexPid)),

    constructor:terminate(Phenotype),
    timer:sleep(100),

    %% All processes should be dead after terminate
    ?assertNot(is_process_alive(CortexPid)).

%% =============================================================================
%% XOR Network Test
%% =============================================================================

construct_xor_network() ->
    %% Construct a complete XOR network and verify structure
    Constraint = #constraint{
        morphology = xor_mimic,
        neural_afs = [tanh],
        neural_pfns = [none],
        neural_aggr_fs = [dot_product]
    },

    SpecieId = {genotype:generate_UniqueId(), specie},
    AgentId = {genotype:generate_UniqueId(), agent},

    genotype:construct_Agent(SpecieId, AgentId, Constraint),

    Phenotype = constructor:construct(AgentId),

    %% Verify we have the right number of components
    %% xor_mimic: 1 sensor (vl=2), 1 neuron (output), 1 actuator (vl=1)
    SensorPids = maps:get(sensor_pids, Phenotype),
    NeuronPids = maps:get(neuron_pids, Phenotype),
    ActuatorPids = maps:get(actuator_pids, Phenotype),

    ?assertEqual(1, length(SensorPids)),
    ?assertEqual(1, length(NeuronPids)),
    ?assertEqual(1, length(ActuatorPids)),

    constructor:terminate(Phenotype).
