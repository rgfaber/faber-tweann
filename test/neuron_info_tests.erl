-module(neuron_info_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% Test get_neuron_info with a neuron record
get_neuron_info_record_test() ->
    N = #neuron{
        neuron_type = cfc,
        time_constant = 2.5,
        state_bound = 1.0,
        internal_state = 0.3,
        af = tanh,
        pf = hebbian,
        input_idps = [{a, 1.0}, {b, 2.0}],
        output_ids = [c, d, e]
    },
    Info = neuron_info:get_neuron_info(N),
    ?assertEqual(cfc, maps:get(neuron_type, Info)),
    ?assertEqual(2.5, maps:get(time_constant, Info)),
    ?assertEqual(1.0, maps:get(state_bound, Info)),
    ?assertEqual(0.3, maps:get(internal_state, Info)),
    ?assertEqual(tanh, maps:get(activation_function, Info)),
    ?assertEqual(hebbian, maps:get(plasticity_function, Info)),
    ?assertEqual(2, maps:get(input_count, Info)),
    ?assertEqual(3, maps:get(output_count, Info)).

%% Test get_neuron_type
get_neuron_type_test() ->
    N = #neuron{neuron_type = ltc},
    ?assertEqual(ltc, neuron_info:get_neuron_type(N)),
    ?assertEqual(cfc, neuron_info:get_neuron_type(#{neuron_type => cfc})).

%% Test get_capabilities
get_capabilities_test() ->
    ?assertEqual([fast_inference, hebbian_plasticity, modulated_plasticity],
                 neuron_info:get_capabilities(standard)),
    ?assertEqual([temporal_memory, adaptive_dynamics, ode_accurate, hebbian_plasticity],
                 neuron_info:get_capabilities(ltc)),
    ?assertEqual([temporal_memory, adaptive_dynamics, fast_inference, hebbian_plasticity],
                 neuron_info:get_capabilities(cfc)).

%% Test is_temporal
is_temporal_test() ->
    ?assertEqual(false, neuron_info:is_temporal(standard)),
    ?assertEqual(true, neuron_info:is_temporal(ltc)),
    ?assertEqual(true, neuron_info:is_temporal(cfc)),
    %% Test with record
    ?assertEqual(true, neuron_info:is_temporal(#neuron{neuron_type = cfc})),
    ?assertEqual(false, neuron_info:is_temporal(#neuron{neuron_type = standard})).

%% Test describe
describe_test() ->
    ?assertMatch(<<"Standard neuron:", _/binary>>, neuron_info:describe(standard)),
    ?assertMatch(<<"LTC", _/binary>>, neuron_info:describe(ltc)),
    ?assertMatch(<<"CfC", _/binary>>, neuron_info:describe(cfc)),
    ?assertEqual(<<"Unknown neuron type.">>, neuron_info:describe(foo)).
