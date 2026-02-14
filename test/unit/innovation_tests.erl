%% @doc Unit tests for innovation number tracking.
-module(innovation_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Test Setup/Teardown
%% ============================================================================

setup() ->
    application:ensure_all_started(faber_tweann),
    genotype:init_db(),
    innovation:init().

teardown() ->
    innovation:reset(),
    genotype:reset_db(),
    application:stop(mnesia).

%% ============================================================================
%% Module Export Tests
%% ============================================================================

innovation_exports_test() ->
    Exports = innovation:module_info(exports),
    ?assert(lists:member({init, 0}, Exports)),
    ?assert(lists:member({reset, 0}, Exports)),
    ?assert(lists:member({next_innovation, 0}, Exports)),
    ?assert(lists:member({get_or_create_link_innovation, 2}, Exports)),
    ?assert(lists:member({get_or_create_node_innovation, 2}, Exports)),
    ?assert(lists:member({get_innovation_info, 1}, Exports)),
    ?assert(lists:member({get_link_innovation, 2}, Exports)),
    ?assert(lists:member({get_node_innovation, 2}, Exports)).

%% ============================================================================
%% Next Innovation Tests
%% ============================================================================

next_innovation_increments_test() ->
    setup(),
    try
        Inn1 = innovation:next_innovation(),
        Inn2 = innovation:next_innovation(),
        Inn3 = innovation:next_innovation(),
        ?assertEqual(Inn1 + 1, Inn2),
        ?assertEqual(Inn2 + 1, Inn3)
    after
        teardown()
    end.

next_innovation_unique_test() ->
    setup(),
    try
        Innovations = [innovation:next_innovation() || _ <- lists:seq(1, 100)],
        UniqueInnovations = lists:usort(Innovations),
        ?assertEqual(length(Innovations), length(UniqueInnovations))
    after
        teardown()
    end.

%% ============================================================================
%% Link Innovation Tests
%% ============================================================================

link_innovation_creates_new_test() ->
    setup(),
    try
        FromId = {neuron, test_from},
        ToId = {neuron, test_to},

        %% First call should create innovation
        Inn1 = innovation:get_or_create_link_innovation(FromId, ToId),
        ?assert(is_integer(Inn1)),
        ?assert(Inn1 > 0)
    after
        teardown()
    end.

link_innovation_returns_same_test() ->
    setup(),
    try
        FromId = {neuron, test_from},
        ToId = {neuron, test_to},

        %% Same link should return same innovation
        Inn1 = innovation:get_or_create_link_innovation(FromId, ToId),
        Inn2 = innovation:get_or_create_link_innovation(FromId, ToId),
        ?assertEqual(Inn1, Inn2)
    after
        teardown()
    end.

link_innovation_different_links_test() ->
    setup(),
    try
        %% Different links should get different innovations
        Inn1 = innovation:get_or_create_link_innovation({n, a}, {n, b}),
        Inn2 = innovation:get_or_create_link_innovation({n, b}, {n, c}),
        Inn3 = innovation:get_or_create_link_innovation({n, a}, {n, c}),

        ?assertNotEqual(Inn1, Inn2),
        ?assertNotEqual(Inn2, Inn3),
        ?assertNotEqual(Inn1, Inn3)
    after
        teardown()
    end.

link_innovation_direction_matters_test() ->
    setup(),
    try
        %% A->B is different from B->A
        Inn1 = innovation:get_or_create_link_innovation({n, x}, {n, y}),
        Inn2 = innovation:get_or_create_link_innovation({n, y}, {n, x}),

        ?assertNotEqual(Inn1, Inn2)
    after
        teardown()
    end.

get_link_innovation_exists_test() ->
    setup(),
    try
        FromId = {neuron, src},
        ToId = {neuron, dst},

        %% Create innovation
        Expected = innovation:get_or_create_link_innovation(FromId, ToId),

        %% Should find it
        ?assertEqual(Expected, innovation:get_link_innovation(FromId, ToId))
    after
        teardown()
    end.

get_link_innovation_not_exists_test() ->
    setup(),
    try
        %% Should return undefined for non-existent link
        ?assertEqual(undefined, innovation:get_link_innovation({n, foo}, {n, bar}))
    after
        teardown()
    end.

%% ============================================================================
%% Node Innovation Tests
%% ============================================================================

node_innovation_creates_three_test() ->
    setup(),
    try
        FromId = {neuron, src},
        ToId = {neuron, dst},

        %% Splitting a link should create 3 innovations
        {NodeInn, InInn, OutInn} = innovation:get_or_create_node_innovation(FromId, ToId),

        ?assert(is_integer(NodeInn)),
        ?assert(is_integer(InInn)),
        ?assert(is_integer(OutInn)),
        ?assert(NodeInn > 0),
        ?assert(InInn > 0),
        ?assert(OutInn > 0),

        %% All three should be different
        ?assertNotEqual(NodeInn, InInn),
        ?assertNotEqual(InInn, OutInn),
        ?assertNotEqual(NodeInn, OutInn)
    after
        teardown()
    end.

node_innovation_returns_same_test() ->
    setup(),
    try
        FromId = {neuron, src},
        ToId = {neuron, dst},

        %% Same split should return same innovations
        {NodeInn1, InInn1, OutInn1} = innovation:get_or_create_node_innovation(FromId, ToId),
        {NodeInn2, InInn2, OutInn2} = innovation:get_or_create_node_innovation(FromId, ToId),

        ?assertEqual(NodeInn1, NodeInn2),
        ?assertEqual(InInn1, InInn2),
        ?assertEqual(OutInn1, OutInn2)
    after
        teardown()
    end.

node_innovation_different_splits_test() ->
    setup(),
    try
        %% Different splits should get different innovations
        {NodeInn1, _, _} = innovation:get_or_create_node_innovation({n, a}, {n, b}),
        {NodeInn2, _, _} = innovation:get_or_create_node_innovation({n, b}, {n, c}),

        ?assertNotEqual(NodeInn1, NodeInn2)
    after
        teardown()
    end.

get_node_innovation_exists_test() ->
    setup(),
    try
        FromId = {neuron, src},
        ToId = {neuron, dst},

        %% Create node innovation
        Expected = innovation:get_or_create_node_innovation(FromId, ToId),

        %% Should find it
        ?assertEqual(Expected, innovation:get_node_innovation(FromId, ToId))
    after
        teardown()
    end.

get_node_innovation_not_exists_test() ->
    setup(),
    try
        %% Should return undefined for non-existent split
        ?assertEqual(undefined, innovation:get_node_innovation({n, foo}, {n, bar}))
    after
        teardown()
    end.

%% ============================================================================
%% Innovation Info Tests
%% ============================================================================

get_innovation_info_link_test() ->
    setup(),
    try
        FromId = {neuron, src},
        ToId = {neuron, dst},

        Inn = innovation:get_or_create_link_innovation(FromId, ToId),
        Info = innovation:get_innovation_info(Inn),

        ?assertEqual({link, FromId, ToId}, Info)
    after
        teardown()
    end.

get_innovation_info_node_test() ->
    setup(),
    try
        FromId = {neuron, src},
        ToId = {neuron, dst},

        {NodeInn, InInn, OutInn} = innovation:get_or_create_node_innovation(FromId, ToId),
        Info = innovation:get_innovation_info(NodeInn),

        ?assertEqual({node, FromId, ToId, InInn, OutInn}, Info)
    after
        teardown()
    end.

get_innovation_info_not_found_test() ->
    setup(),
    try
        ?assertEqual(not_found, innovation:get_innovation_info(999999))
    after
        teardown()
    end.

%% ============================================================================
%% Reset Tests
%% ============================================================================

reset_clears_innovations_test() ->
    setup(),
    try
        %% Create some innovations
        Inn1 = innovation:get_or_create_link_innovation({n, a}, {n, b}),
        _ = innovation:get_or_create_node_innovation({n, c}, {n, d}),

        %% Reset
        innovation:reset(),
        innovation:init(),

        %% Old innovations should not exist
        ?assertEqual(undefined, innovation:get_link_innovation({n, a}, {n, b})),
        ?assertEqual(undefined, innovation:get_node_innovation({n, c}, {n, d})),

        %% Counter should restart (new innovation <= old innovation)
        InnNew = innovation:next_innovation(),
        ?assert(InnNew =< Inn1)
    after
        teardown()
    end.
