%% @doc Unit tests for mutation_helpers module.
-module(mutation_helpers_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% ============================================================================
%% Module Export Tests
%% ============================================================================

mutation_helpers_exports_test() ->
    Exports = mutation_helpers:module_info(exports),
    ?assert(lists:member({select_random_neuron, 1}, Exports)),
    ?assert(lists:member({select_ltc_neuron, 1}, Exports)),
    ?assert(lists:member({link_neuron_to_target, 3}, Exports)),
    ?assert(lists:member({link_source_to_neuron, 3}, Exports)),
    ?assert(lists:member({link_sensor_to_neuron, 3}, Exports)),
    ?assert(lists:member({link_neuron_to_actuator, 3}, Exports)),
    ?assert(lists:member({update_source_output, 3}, Exports)),
    ?assert(lists:member({update_target_input, 4}, Exports)),
    ?assert(lists:member({create_random_weight, 0}, Exports)),
    ?assert(lists:member({get_link_weight, 2}, Exports)),
    ?assert(lists:member({find_splittable_link, 1}, Exports)),
    ?assert(lists:member({get_layer_coord, 1}, Exports)),
    ?assert(lists:member({perturb_ltc_weight_list, 2}, Exports)).

%% ============================================================================
%% Helper Function Tests
%% ============================================================================

setup_test() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db().

select_random_neuron_returns_neuron_id_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        NeuronId = mutation_helpers:select_random_neuron(AgentId),
        ?assert(NeuronId /= {error, no_neurons}),

        Neuron = genotype:dirty_read({neuron, NeuronId}),
        ?assert(is_record(Neuron, neuron))
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

select_ltc_neuron_returns_error_when_no_ltc_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% By default all neurons are standard
        Result = mutation_helpers:select_ltc_neuron(AgentId),
        ?assertEqual({error, no_ltc_neurons}, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

select_ltc_neuron_returns_ltc_neuron_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        %% Convert a neuron to LTC type
        StdNeuronId = mutation_helpers:select_random_neuron(AgentId),
        Neuron = genotype:dirty_read({neuron, StdNeuronId}),
        LtcNeuron = Neuron#neuron{neuron_type = ltc},
        genotype:write(LtcNeuron),

        %% Now select_ltc_neuron should find it
        Result = mutation_helpers:select_ltc_neuron(AgentId),
        ?assertEqual(StdNeuronId, Result)
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.

create_random_weight_test() ->
    Weight = mutation_helpers:create_random_weight(),
    {W, Delta, LR, Params} = Weight,
    ?assert(is_float(W)),
    ?assertEqual(+0.0, Delta),
    ?assertEqual(0.1, LR),
    ?assertEqual([], Params),
    ?assert(W >= -0.5),
    ?assert(W =< 0.5).

get_layer_coord_tuple_test() ->
    Coord = mutation_helpers:get_layer_coord({{0.5, 12345.67}, neuron}),
    ?assertEqual(0.5, Coord).

get_layer_coord_default_test() ->
    Coord = mutation_helpers:get_layer_coord(some_other_id),
    ?assertEqual(0.5, Coord).

perturb_ltc_weight_list_empty_test() ->
    Result = mutation_helpers:perturb_ltc_weight_list([], 1.0),
    ?assertEqual(2, length(Result)),
    [W1, W2] = Result,
    ?assert(W1 >= -0.5 andalso W1 =< 0.5),
    ?assert(W2 >= -0.5 andalso W2 =< 0.5).

perturb_ltc_weight_list_existing_test() ->
    Original = [0.5, 0.3, -0.2],
    Result = mutation_helpers:perturb_ltc_weight_list(Original, 0.1),
    ?assertEqual(3, length(Result)),
    %% Weights should be perturbed but list length unchanged
    ?assertNotEqual(Original, Result).

find_splittable_link_test() ->
    setup_test(),
    try
        SpecieId = test_specie,
        AgentId = genotype:generate_id(agent),
        Constraint = #constraint{morphology = xor_mimic},
        genotype:construct_Agent(SpecieId, AgentId, Constraint),

        Result = mutation_helpers:find_splittable_link(AgentId),
        case Result of
            {error, no_links} ->
                ok;
            {FromId, ToId, Weight} ->
                ?assert(FromId /= undefined),
                ?assert(ToId /= undefined),
                ?assertMatch({_, _, _, _}, Weight)
        end
    after
        genotype:reset_db(),
        application:stop(mnesia)
    end.
