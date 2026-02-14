%% @doc Unit tests for brain system modules (brain, brain_learner, brain_sup, brain_pubsub).
%%
%% Tests the complete brain system for inference and online learning.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(brain_system_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Test Setup/Teardown
%% ============================================================================

setup() ->
    application:ensure_all_started(faber_tweann),
    ok.

teardown() ->
    ok.

%% ============================================================================
%% Module Export Tests
%% ============================================================================

brain_exports_test() ->
    Exports = brain:module_info(exports),
    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({start_link, 2}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)),
    ?assert(lists:member({evaluate, 2}, Exports)),
    ?assert(lists:member({evaluate_with_activations, 2}, Exports)),
    ?assert(lists:member({get_viz, 1}, Exports)),
    ?assert(lists:member({get_topology, 1}, Exports)),
    ?assert(lists:member({get_network, 1}, Exports)),
    ?assert(lists:member({set_network, 2}, Exports)),
    ?assert(lists:member({subscribe, 1}, Exports)),
    ?assert(lists:member({unsubscribe, 1}, Exports)).

brain_sup_exports_test() ->
    Exports = brain_sup:module_info(exports),
    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({start_link, 2}, Exports)),
    ?assert(lists:member({get_inference_pid, 1}, Exports)),
    ?assert(lists:member({get_learner_pid, 1}, Exports)).

brain_learner_exports_test() ->
    Exports = brain_learner:module_info(exports),
    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)),
    ?assert(lists:member({enable, 1}, Exports)),
    ?assert(lists:member({disable, 1}, Exports)),
    ?assert(lists:member({is_enabled, 1}, Exports)),
    ?assert(lists:member({reward, 2}, Exports)),
    ?assert(lists:member({record_experience, 3}, Exports)),
    ?assert(lists:member({learn_from_experience, 1}, Exports)),
    ?assert(lists:member({set_plasticity_rule, 2}, Exports)),
    ?assert(lists:member({get_plasticity_rule, 1}, Exports)),
    ?assert(lists:member({set_learning_rate, 2}, Exports)),
    ?assert(lists:member({get_learning_rate, 1}, Exports)).

brain_pubsub_exports_test() ->
    Exports = brain_pubsub:module_info(exports),
    ?assert(lists:member({init, 1}, Exports)),
    ?assert(lists:member({cleanup, 1}, Exports)),
    ?assert(lists:member({subscribe, 2}, Exports)),
    ?assert(lists:member({subscribe, 3}, Exports)),
    ?assert(lists:member({unsubscribe, 2}, Exports)),
    ?assert(lists:member({publish, 3}, Exports)),
    ?assert(lists:member({get_subscribers, 2}, Exports)),
    ?assert(lists:member({list_topics, 1}, Exports)).

brain_system_exports_test() ->
    Exports = brain_system:module_info(exports),
    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)),
    ?assert(lists:member({evaluate, 2}, Exports)),
    ?assert(lists:member({enable_learning, 1}, Exports)),
    ?assert(lists:member({disable_learning, 1}, Exports)),
    ?assert(lists:member({reward, 2}, Exports)),
    ?assert(lists:member({learn_step, 1}, Exports)),
    ?assert(lists:member({get_inference_pid, 1}, Exports)),
    ?assert(lists:member({get_learner_pid, 1}, Exports)),
    ?assert(lists:member({has_learner, 1}, Exports)).

%% ============================================================================
%% brain_pubsub Tests
%% ============================================================================

brain_pubsub_init_test() ->
    setup(),
    try
        BrainId = make_ref(),
        %% Init should succeed
        ?assertEqual(ok, brain_pubsub:init(BrainId)),
        %% Init again should be idempotent
        ?assertEqual(ok, brain_pubsub:init(BrainId))
    after
        teardown()
    end.

brain_pubsub_subscribe_test() ->
    setup(),
    try
        BrainId = make_ref(),
        brain_pubsub:init(BrainId),

        %% Subscribe to a topic
        ?assertEqual(ok, brain_pubsub:subscribe(BrainId, test_topic)),

        %% Check we're subscribed
        Subscribers = brain_pubsub:get_subscribers(BrainId, test_topic),
        ?assert(lists:member(self(), Subscribers)),

        %% Unsubscribe
        ?assertEqual(ok, brain_pubsub:unsubscribe(BrainId, test_topic)),
        SubsAfter = brain_pubsub:get_subscribers(BrainId, test_topic),
        ?assertNot(lists:member(self(), SubsAfter))
    after
        teardown()
    end.

brain_pubsub_publish_receive_test() ->
    setup(),
    try
        BrainId = make_ref(),
        brain_pubsub:init(BrainId),

        %% Subscribe to topic
        brain_pubsub:subscribe(BrainId, test_event),

        %% Publish an event
        EventData = #{value => 42},
        brain_pubsub:publish(BrainId, test_event, EventData),

        %% Receive the event
        receive
            {brain_event, test_event, Data} ->
                ?assertEqual(EventData, Data)
        after 1000 ->
            ?assert(false)  %% Timeout - event not received
        end,

        brain_pubsub:unsubscribe(BrainId, test_event)
    after
        teardown()
    end.

brain_pubsub_subscribe_multiple_topics_test() ->
    setup(),
    try
        BrainId = make_ref(),
        brain_pubsub:init(BrainId),

        %% Subscribe to multiple topics at once
        Topics = [topic1, topic2, topic3],
        ?assertEqual(ok, brain_pubsub:subscribe(BrainId, Topics)),

        %% Verify subscriptions
        ?assert(lists:member(self(), brain_pubsub:get_subscribers(BrainId, topic1))),
        ?assert(lists:member(self(), brain_pubsub:get_subscribers(BrainId, topic2))),
        ?assert(lists:member(self(), brain_pubsub:get_subscribers(BrainId, topic3))),

        brain_pubsub:unsubscribe(BrainId, Topics)
    after
        teardown()
    end.

brain_pubsub_list_topics_test() ->
    setup(),
    try
        BrainId = make_ref(),
        brain_pubsub:init(BrainId),

        brain_pubsub:subscribe(BrainId, topic_a),
        brain_pubsub:subscribe(BrainId, topic_b),

        Topics = brain_pubsub:list_topics(BrainId),
        ?assert(lists:member(topic_a, Topics)),
        ?assert(lists:member(topic_b, Topics)),

        brain_pubsub:unsubscribe(BrainId, [topic_a, topic_b])
    after
        teardown()
    end.

brain_pubsub_cleanup_test() ->
    setup(),
    try
        BrainId = make_ref(),
        brain_pubsub:init(BrainId),

        brain_pubsub:subscribe(BrainId, cleanup_test),
        ?assert(lists:member(self(), brain_pubsub:get_subscribers(BrainId, cleanup_test))),

        brain_pubsub:cleanup(BrainId),
        %% After cleanup, subscription should be gone
        ?assertNot(lists:member(self(), brain_pubsub:get_subscribers(BrainId, cleanup_test)))
    after
        teardown()
    end.

%% ============================================================================
%% brain Tests
%% ============================================================================

brain_start_stop_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => false},

        {ok, Pid} = brain:start_link(Opts),
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid)),

        brain:stop(Pid),
        timer:sleep(10),
        ?assertNot(is_process_alive(Pid))
    after
        teardown()
    end.

brain_evaluate_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => false},

        {ok, Pid} = brain:start_link(Opts),

        %% Evaluate with 2 inputs, expect 1 output
        Inputs = [0.5, 0.5],
        Outputs = brain:evaluate(Pid, Inputs),

        ?assert(is_list(Outputs)),
        ?assertEqual(1, length(Outputs)),
        [Output] = Outputs,
        ?assert(is_float(Output)),
        %% Tanh activation bounds output to [-1, 1]
        ?assert(Output >= -1.0),
        ?assert(Output =< 1.0),

        brain:stop(Pid)
    after
        teardown()
    end.

brain_evaluate_with_activations_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => true},

        {ok, Pid} = brain:start_link(Opts),

        Inputs = [0.5, 0.5],
        {Outputs, Activations} = brain:evaluate_with_activations(Pid, Inputs),

        ?assert(is_list(Outputs)),
        ?assertEqual(1, length(Outputs)),

        %% Activations should include all layers: input (2), hidden (4), output (1)
        ?assert(is_list(Activations)),
        ?assertEqual(3, length(Activations)),
        [InputActs, HiddenActs, OutputActs] = Activations,
        ?assertEqual(2, length(InputActs)),
        ?assertEqual(4, length(HiddenActs)),
        ?assertEqual(1, length(OutputActs)),

        brain:stop(Pid)
    after
        teardown()
    end.

brain_get_set_network_test() ->
    setup(),
    try
        Network1 = network_evaluator:create_feedforward(2, [4], 1),
        Network2 = network_evaluator:create_feedforward(2, [8], 1),
        Opts = #{network => Network1, viz_enabled => false},

        {ok, Pid} = brain:start_link(Opts),

        Retrieved1 = brain:get_network(Pid),
        ?assertEqual(Network1, Retrieved1),

        brain:set_network(Pid, Network2),
        Retrieved2 = brain:get_network(Pid),
        ?assertEqual(Network2, Retrieved2),

        brain:stop(Pid)
    after
        teardown()
    end.

brain_get_topology_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(3, [5, 4], 2),
        Opts = #{network => Network, viz_enabled => false},

        {ok, Pid} = brain:start_link(Opts),

        Topology = brain:get_topology(Pid),
        ?assert(is_map(Topology)),
        ?assert(maps:is_key(layer_sizes, Topology)),
        ?assertEqual([3, 5, 4, 2], maps:get(layer_sizes, Topology)),

        brain:stop(Pid)
    after
        teardown()
    end.

brain_subscribe_unsubscribe_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => true},

        {ok, Pid} = brain:start_link(Opts),

        %% Subscribe
        brain:subscribe(Pid),

        %% Evaluate to trigger viz notification
        brain:evaluate(Pid, [0.5, 0.5]),

        %% Should receive viz message
        receive
            {brain_viz, Pid, VizData} ->
                ?assert(is_map(VizData))
        after 1000 ->
            ?assert(false)
        end,

        %% Unsubscribe
        brain:unsubscribe(Pid),

        %% Evaluate again
        brain:evaluate(Pid, [0.5, 0.5]),

        %% Should not receive another viz message
        receive
            {brain_viz, Pid, _} ->
                ?assert(false)  %% Should not get here
        after 100 ->
            ok  %% Good - no message received
        end,

        brain:stop(Pid)
    after
        teardown()
    end.

brain_viz_data_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => true},

        {ok, Pid} = brain:start_link(Opts),

        %% Before evaluation, viz should be undefined
        ?assertEqual(undefined, brain:get_viz(Pid)),

        %% Evaluate
        brain:evaluate(Pid, [0.5, 0.5]),

        %% Now viz should have data
        VizData = brain:get_viz(Pid),
        ?assert(is_map(VizData)),
        ?assert(maps:is_key(nodes, VizData)),
        ?assert(maps:is_key(connections, VizData)),
        ?assert(maps:is_key(layer_sizes, VizData)),
        ?assert(maps:is_key(inputs, VizData)),
        ?assert(maps:is_key(outputs, VizData)),

        brain:stop(Pid)
    after
        teardown()
    end.

%% ============================================================================
%% brain_sup Tests
%% ============================================================================

brain_sup_inference_only_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{
            network => Network,
            learning_enabled => false,
            viz_enabled => false
        },

        {ok, SupPid} = brain_sup:start_link(Opts),
        unlink(SupPid),

        %% Should have inference process
        InfPid = brain_sup:get_inference_pid(SupPid),
        ?assert(is_pid(InfPid)),
        ?assert(is_process_alive(InfPid)),

        %% Should not have learner process
        ?assertEqual(undefined, brain_sup:get_learner_pid(SupPid)),

        exit(SupPid, kill),
        timer:sleep(10)
    after
        teardown()
    end.

brain_sup_with_learner_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{
            network => Network,
            learning_enabled => true,
            plasticity_rule => hebbian,
            viz_enabled => false
        },

        {ok, SupPid} = brain_sup:start_link(Opts),
        unlink(SupPid),

        %% Should have both inference and learner
        InfPid = brain_sup:get_inference_pid(SupPid),
        LearnerPid = brain_sup:get_learner_pid(SupPid),

        ?assert(is_pid(InfPid)),
        ?assert(is_pid(LearnerPid)),
        ?assert(is_process_alive(InfPid)),
        ?assert(is_process_alive(LearnerPid)),

        exit(SupPid, kill),
        timer:sleep(10)
    after
        teardown()
    end.

%% ============================================================================
%% brain_system Tests
%% ============================================================================

%% @private Stop a brain system safely from a separate process
%% to avoid propagating exit signals to the test process.
stop_system_safely(System) ->
    SupPid = element(2, System),  %% Extract sup_pid from brain_system record
    unlink(SupPid),
    exit(SupPid, kill),
    timer:sleep(10).

brain_system_start_stop_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => false},

        {ok, System} = brain_system:start_link(Opts),

        %% Check we have an inference process
        InfPid = brain_system:get_inference_pid(System),
        ?assert(is_pid(InfPid)),

        %% Without learning_enabled, no learner
        ?assertEqual(undefined, brain_system:get_learner_pid(System)),
        ?assertNot(brain_system:has_learner(System)),

        %% Stop in separate process to avoid propagating exit signal
        stop_system_safely(System)
    after
        teardown()
    end.

brain_system_with_learning_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{
            network => Network,
            learning_enabled => true,
            viz_enabled => false
        },

        {ok, System} = brain_system:start_link(Opts),

        ?assert(brain_system:has_learner(System)),
        LearnerPid = brain_system:get_learner_pid(System),
        ?assert(is_pid(LearnerPid)),

        stop_system_safely(System)
    after
        teardown()
    end.

brain_system_evaluate_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => false},

        {ok, System} = brain_system:start_link(Opts),

        Outputs = brain_system:evaluate(System, [0.5, 0.5]),
        ?assert(is_list(Outputs)),
        ?assertEqual(1, length(Outputs)),

        stop_system_safely(System)
    after
        teardown()
    end.

brain_system_learning_control_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{
            network => Network,
            learning_enabled => true,
            viz_enabled => false
        },

        {ok, System} = brain_system:start_link(Opts),

        %% Learning should be enabled by default
        ?assert(brain_system:is_learning_enabled(System)),

        %% Disable learning
        ?assertEqual(ok, brain_system:disable_learning(System)),
        ?assertNot(brain_system:is_learning_enabled(System)),

        %% Enable learning
        ?assertEqual(ok, brain_system:enable_learning(System)),
        ?assert(brain_system:is_learning_enabled(System)),

        stop_system_safely(System)
    after
        teardown()
    end.

brain_system_no_learner_errors_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, learning_enabled => false},

        {ok, System} = brain_system:start_link(Opts),

        %% Learning operations should return errors
        ?assertEqual({error, no_learner}, brain_system:enable_learning(System)),
        ?assertEqual({error, no_learner}, brain_system:disable_learning(System)),
        ?assertEqual({error, no_learner}, brain_system:reward(System, 1.0)),
        ?assertEqual({error, no_learner}, brain_system:learn_step(System)),

        stop_system_safely(System)
    after
        teardown()
    end.

brain_system_plasticity_config_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{
            network => Network,
            learning_enabled => true,
            plasticity_rule => hebbian,
            learning_rate => 0.05,
            viz_enabled => false
        },

        {ok, System} = brain_system:start_link(Opts),

        ?assertEqual(hebbian, brain_system:get_plasticity_rule(System)),
        ?assertEqual(0.05, brain_system:get_learning_rate(System)),

        %% Change configuration
        ?assertEqual(ok, brain_system:set_plasticity_rule(System, modulated)),
        ?assertEqual(ok, brain_system:set_learning_rate(System, 0.1)),

        ?assertEqual(modulated, brain_system:get_plasticity_rule(System)),
        ?assertEqual(0.1, brain_system:get_learning_rate(System)),

        stop_system_safely(System)
    after
        teardown()
    end.

brain_system_reward_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{
            network => Network,
            learning_enabled => true,
            viz_enabled => false
        },

        {ok, System} = brain_system:start_link(Opts),

        %% Reward should succeed
        ?assertEqual(ok, brain_system:reward(System, 1.0)),
        ?assertEqual(ok, brain_system:reward(System, -0.5)),

        stop_system_safely(System)
    after
        teardown()
    end.

brain_system_network_access_test() ->
    setup(),
    try
        Network = network_evaluator:create_feedforward(2, [4], 1),
        Opts = #{network => Network, viz_enabled => false},

        {ok, System} = brain_system:start_link(Opts),

        Retrieved = brain_system:get_network(System),
        ?assertEqual(Network, Retrieved),

        Topology = brain_system:get_topology(System),
        ?assert(is_map(Topology)),
        ?assertEqual([2, 4, 1], maps:get(layer_sizes, Topology)),

        stop_system_safely(System)
    after
        teardown()
    end.

