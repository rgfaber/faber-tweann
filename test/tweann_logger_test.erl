%% @doc Tests for tweann_logger module.
-module(tweann_logger_test).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Logging API Tests
%% ============================================================================

debug_logging_test() ->
    %% Verify debug logging doesn't crash
    Result = tweann_logger:debug("Test debug message: ~p", [test_value]),
    ?assertEqual(ok, Result).

info_logging_test() ->
    %% Verify info logging doesn't crash
    Result = tweann_logger:info("Test info message: ~p", [test_value]),
    ?assertEqual(ok, Result).

warning_logging_test() ->
    %% Verify warning logging doesn't crash
    Result = tweann_logger:warning("Test warning message: ~p", [test_value]),
    ?assertEqual(ok, Result).

error_logging_test() ->
    %% Verify error logging doesn't crash
    Result = tweann_logger:error("Test error message: ~p", [test_value]),
    ?assertEqual(ok, Result).

%% ============================================================================
%% Format String Tests
%% ============================================================================

multiple_args_test() ->
    %% Verify multiple format arguments work
    Result = tweann_logger:info("Agent ~p mutation ~p result ~p",
                                [agent1, add_neuron, ok]),
    ?assertEqual(ok, Result).

no_args_test() ->
    %% Verify logging with no arguments works
    Result = tweann_logger:info("Simple message with no args", []),
    ?assertEqual(ok, Result).

complex_term_test() ->
    %% Verify complex terms can be logged
    ComplexTerm = #{agent => {1.0, agent}, fitness => [100.0, 200.0]},
    Result = tweann_logger:debug("Complex term: ~p", [ComplexTerm]),
    ?assertEqual(ok, Result).
