%%% @doc Tests for genotype to DAG conversion.
%%%
%%% The load-bearing test is the skip connection: a topology that
%%% genotype_to_network REFUSES, converting here and computing the right number.
%%% That is the whole reason this module exists.
-module(genotype_to_dag_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    application:ensure_all_started(faber_tweann),
    genotype:init_db().

teardown(_) ->
    genotype:reset_db().

genotype_to_dag_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun converts_a_plain_layered_net/0,
        fun converts_a_skip_connection_the_layered_path_refuses/0,
        fun orders_nodes_so_every_source_comes_first/0,
        fun compiles_and_evaluates_through_the_evaluator/0,
        fun refuses_a_cycle/0,
        fun refuses_a_cfc_neuron/0,
        fun refuses_an_unknown_source/0
    ]}.

%%==============================================================================
%% Fixtures
%%==============================================================================

u() -> genotype:generate_UniqueId().
w(X) -> {X, 0.0, 0.0, []}.

%% Two inputs, two hidden, one output, all linear so the arithmetic is checkable
%% by hand. Hidden neurons are deliberately written into the cortex AFTER the
%% output neuron, so a converter that trusted cortex order would emit an
%% unsorted list and read zeros.
layered() ->
    S = {{-1.0, u()}, sensor},
    H1 = {{0.0, u()}, neuron},
    H2 = {{0.0, u()}, neuron},
    O = {{0.5, u()}, neuron},
    A = {{1.0, u()}, actuator},
    Cx = {{origin, u()}, cortex},
    Ag = {{origin, u()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = in, vl = 2, fanout_ids = [H1, H2]}),
    genotype:write(#neuron{id = H1, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(1.0), w(2.0)]}, {bias, [w(0.5)]}],
                           output_ids = [O]}),
    genotype:write(#neuron{id = H2, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{S, [w(3.0), w(4.0)]}], output_ids = [O]}),
    genotype:write(#neuron{id = O, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{H1, [w(10.0)]}, {H2, [w(-1.0)]}, {bias, [w(0.25)]}],
                           output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = out, vl = 1, fanin_ids = [O]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [O, H1, H2],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    {Ag, S, H1, H2, O}.

%%==============================================================================
%% Conversion
%%==============================================================================

%% h1 = 1*1 + 2*2 + 0.5 = 5.5 ; h2 = 3*1 + 4*2 = 11.0
%% o  = 10*5.5 - 1*11.0 + 0.25 = 44.25
converts_a_plain_layered_net() ->
    {Ag, _S, _H1, _H2, _O} = layered(),
    {ok, {Nodes, InputCount, Outputs}} = genotype_to_dag:nodes(Ag),
    ?assertEqual(2, InputCount),
    ?assertEqual(5, length(Nodes)),
    ?assertEqual(1, length(Outputs)),
    {ok, Net} = genotype_to_dag:compile(Ag),
    ?assertEqual([44.25], tweann_nif:evaluate(Net, [1.0, 2.0])).

%% The output neuron reaches PAST the hidden layer straight to the sensor.
%% genotype_to_network refuses exactly this, and it is the ordinary shape of an
%% evolved topology, so it is the reason this module exists.
converts_a_skip_connection_the_layered_path_refuses() ->
    {Ag, S, _H1, _H2, O} = layered(),
    N = genotype:dirty_read({neuron, O}),
    genotype:write(N#neuron{input_idps = [{S, [w(100.0), w(0.0)]} | N#neuron.input_idps]}),
    ?assertMatch({error, {not_layerable, {source_not_in_previous_layer, _, _}}},
                 network_evaluator:from_genotype(Ag)),
    {ok, Net} = genotype_to_dag:compile(Ag),
    %% 44.25 as before, plus 100 * the first input
    ?assertEqual([144.25], tweann_nif:evaluate(Net, [1.0, 2.0])).

%% Both evaluators walk the list once in order, so an unsorted list silently
%% reads zeros. The cortex above lists the output neuron first on purpose.
orders_nodes_so_every_source_comes_first() ->
    {Ag, _S, _H1, _H2, _O} = layered(),
    {ok, {Nodes, InputCount, _}} = genotype_to_dag:nodes(Ag),
    %% index equals position, one contiguous run
    ?assertEqual(lists:seq(0, length(Nodes) - 1), [I || {I, _, _, _, _} <- Nodes]),
    %% inputs first, and nothing else marked input
    ?assertEqual(InputCount, length([I || {I, input, _, _, _} <- Nodes, I < InputCount])),
    %% every source strictly earlier than its consumer
    ?assert(lists:all(fun({I, _, _, _, Conns}) ->
                          lists:all(fun({From, _}) -> From < I end, Conns)
                      end, Nodes)).

compiles_and_evaluates_through_the_evaluator() ->
    {Ag, _S, _H1, _H2, _O} = layered(),
    {ok, Net} = genotype_to_dag:compile(Ag),
    ?assertEqual([44.25], tweann_nif:evaluate(Net, [1.0, 2.0])),
    %% and the layered path agrees, on a genotype both can take
    {ok, Layered} = network_evaluator:from_genotype(Ag),
    ?assertEqual(network_evaluator:evaluate(Layered, [1.0, 2.0]),
                 tweann_nif:evaluate(Net, [1.0, 2.0])).

%%==============================================================================
%% Refusals
%%==============================================================================

%% A cycle has no topological order. Evaluated anyway it would read 0.0 for the
%% back edge and report a number, which is the failure this refuses.
refuses_a_cycle() ->
    {Ag, _S, H1, _H2, O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{input_idps = [{O, [w(1.0)]} | N#neuron.input_idps]}),
    ?assertEqual({error, {not_convertible, cyclic}}, genotype_to_dag:nodes(Ag)).

%% compile_network/3 carries no per-node state, so a CfC neuron's memory cannot
%% be represented and converting it would silently drop the dynamics.
refuses_a_cfc_neuron() ->
    {Ag, _S, H1, _H2, _O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    genotype:write(N#neuron{neuron_type = cfc}),
    ?assertEqual({error, {not_convertible, {unsupported_neuron_type, cfc}}},
                 genotype_to_dag:nodes(Ag)).

refuses_an_unknown_source() ->
    {Ag, _S, H1, _H2, _O} = layered(),
    N = genotype:dirty_read({neuron, H1}),
    Ghost = {{0.0, u()}, neuron},
    genotype:write(N#neuron{input_idps = [{Ghost, [w(1.0)]} | N#neuron.input_idps]}),
    ?assertMatch({error, {not_convertible, {unknown_source, _, _}}},
                 genotype_to_dag:nodes(Ag)).
