%% @doc Unit tests for application modules.
%%
%% Tests faber_tweann_app, faber_tweann_sup, and tweann_logger modules.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(app_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% faber_tweann_app Export Tests
%% ============================================================================

faber_tweann_app_exports_test() ->
    Exports = faber_tweann_app:module_info(exports),
    ?assert(lists:member({start, 2}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)).

faber_tweann_app_behaviour_test() ->
    Behaviours = proplists:get_value(behaviour, faber_tweann_app:module_info(attributes), []),
    ?assert(lists:member(application, Behaviours)).

%% ============================================================================
%% faber_tweann_sup Export Tests
%% ============================================================================

faber_tweann_sup_exports_test() ->
    Exports = faber_tweann_sup:module_info(exports),
    ?assert(lists:member({start_link, 0}, Exports)),
    ?assert(lists:member({init, 1}, Exports)).

faber_tweann_sup_behaviour_test() ->
    Behaviours = proplists:get_value(behaviour, faber_tweann_sup:module_info(attributes), []),
    ?assert(lists:member(supervisor, Behaviours)).

%% ============================================================================
%% tweann_logger Export Tests
%% ============================================================================

tweann_logger_exports_test() ->
    Exports = tweann_logger:module_info(exports),
    ?assert(lists:member({debug, 2}, Exports)),
    ?assert(lists:member({info, 2}, Exports)),
    ?assert(lists:member({warning, 2}, Exports)),
    ?assert(lists:member({error, 2}, Exports)).

%% ============================================================================
%% tweann_logger Functional Tests
%% ============================================================================

tweann_logger_debug_returns_ok_test() ->
    ?assertEqual(ok, tweann_logger:debug("Test debug message ~p", [123])).

tweann_logger_info_returns_ok_test() ->
    ?assertEqual(ok, tweann_logger:info("Test info message ~p", [456])).

tweann_logger_warning_returns_ok_test() ->
    ?assertEqual(ok, tweann_logger:warning("Test warning message ~p", [789])).

tweann_logger_error_returns_ok_test() ->
    ?assertEqual(ok, tweann_logger:error("Test error message ~p", [error_code])).

tweann_logger_empty_args_test() ->
    ?assertEqual(ok, tweann_logger:info("Simple message with no args", [])).

tweann_logger_complex_args_test() ->
    ComplexTerm = #{key => value, nested => [1, 2, 3]},
    ?assertEqual(ok, tweann_logger:debug("Complex term: ~p", [ComplexTerm])).

tweann_logger_multiple_args_test() ->
    ?assertEqual(ok, tweann_logger:info("Agent ~p gen ~p fitness ~p", [agent_1, 5, 0.95])).

%% ============================================================================
%% Application Start/Stop Tests
%% ============================================================================

application_can_start_test() ->
    %% Ensure application is started
    application:ensure_all_started(faber_tweann),
    Apps = application:which_applications(),
    ?assert(lists:keymember(faber_tweann, 1, Apps)).

application_info_test() ->
    %% Ensure application is loaded
    application:load(faber_tweann),
    %% Verify application metadata is accessible
    {ok, Description} = application:get_key(faber_tweann, description),
    ?assert(is_list(Description)).

application_vsn_test() ->
    %% Ensure application is loaded
    application:load(faber_tweann),
    %% Verify version is accessible
    {ok, Vsn} = application:get_key(faber_tweann, vsn),
    ?assert(is_list(Vsn) orelse is_binary(Vsn)).

%% ============================================================================
%% Supervisor Tests
%% ============================================================================

supervisor_children_test() ->
    %% Ensure application is started
    application:ensure_all_started(faber_tweann),
    %% Get the children specification
    Children = supervisor:which_children(faber_tweann_sup),
    ?assert(is_list(Children)).

supervisor_has_morphology_registry_test() ->
    %% Ensure application is started
    application:ensure_all_started(faber_tweann),
    %% Verify morphology_registry is a child
    Children = supervisor:which_children(faber_tweann_sup),
    ChildIds = [Id || {Id, _, _, _} <- Children],
    ?assert(lists:member(morphology_registry, ChildIds)).

supervisor_morphology_registry_running_test() ->
    %% Ensure application is started
    application:ensure_all_started(faber_tweann),
    %% Verify morphology_registry process is running
    Children = supervisor:which_children(faber_tweann_sup),
    case lists:keyfind(morphology_registry, 1, Children) of
        {morphology_registry, Pid, worker, _} ->
            ?assert(is_pid(Pid)),
            ?assert(is_process_alive(Pid));
        _ ->
            ?assert(false)
    end.

%% ============================================================================
%% Supervisor Restart Strategy Tests
%% ============================================================================

supervisor_restart_strategy_test() ->
    %% Get supervisor flags via init callback
    case faber_tweann_sup:init([]) of
        {ok, {SupFlags, _ChildSpecs}} ->
            ?assertMatch(#{strategy := one_for_one}, SupFlags);
        _ ->
            ?assert(false)
    end.

supervisor_intensity_test() ->
    case faber_tweann_sup:init([]) of
        {ok, {SupFlags, _ChildSpecs}} ->
            Intensity = maps:get(intensity, SupFlags, 0),
            ?assert(Intensity > 0);
        _ ->
            ?assert(false)
    end.

supervisor_period_test() ->
    case faber_tweann_sup:init([]) of
        {ok, {SupFlags, _ChildSpecs}} ->
            Period = maps:get(period, SupFlags, 0),
            ?assert(Period > 0);
        _ ->
            ?assert(false)
    end.

