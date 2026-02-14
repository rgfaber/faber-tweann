%% @doc Tests for morphology_registry module.
-module(morphology_registry_test).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%%==============================================================================
%% Test Fixtures
%%==============================================================================

%% Start/stop registry for each test
registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_register_valid_morphology/1,
      fun test_register_invalid_module/1,
      fun test_register_missing_callbacks/1,
      fun test_get_registered_morphology/1,
      fun test_get_unregistered_morphology/1,
      fun test_unregister_morphology/1,
      fun test_list_all_empty/1,
      fun test_list_all_multiple/1,
      fun test_is_registered/1
     ]}.

setup() ->
    % Registry might already be started by application
    % Just ensure it's running
    case whereis(morphology_registry) of
        undefined ->
            {ok, Pid} = morphology_registry:start_link(),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

cleanup(_Pid) ->
    % Don't stop registry - it's managed by application supervisor
    % Just clean up all registrations to ensure tests start fresh
    AllMorphologies = morphology_registry:list_all(),
    lists:foreach(fun(Name) ->
        morphology_registry:unregister(Name)
    end, AllMorphologies),
    ok.

%%==============================================================================
%% Tests
%%==============================================================================

test_register_valid_morphology(_Pid) ->
    % Create a test morphology module
    ok = compile_test_morphology(),

    % Register should succeed
    Result = morphology_registry:register(test_morph, test_morphology_module),
    ?_assertEqual(ok, Result).

test_register_invalid_module(_Pid) ->
    % Registering non-existent module should fail
    Result = morphology_registry:register(invalid, non_existent_module),
    ?_assertMatch({error, {module_not_loaded, _}}, Result).

test_register_missing_callbacks(_Pid) ->
    % Create module without required callbacks
    ok = compile_bad_morphology(),

    % Should fail validation
    Result = morphology_registry:register(bad_morph, bad_morphology_module),
    ?_assertMatch({error, {missing_callback, _, _}}, Result).

test_get_registered_morphology(_Pid) ->
    ok = compile_test_morphology(),
    ok = morphology_registry:register(test_morph, test_morphology_module),

    % Get should return the module
    Result = morphology_registry:get(test_morph),
    ?_assertEqual({ok, test_morphology_module}, Result).

test_get_unregistered_morphology(_Pid) ->
    % Getting unregistered morphology should return error
    Result = morphology_registry:get(nonexistent),
    ?_assertEqual({error, not_found}, Result).

test_unregister_morphology(_Pid) ->
    ok = compile_test_morphology(),
    ok = morphology_registry:register(test_morph, test_morphology_module),

    % Unregister
    ok = morphology_registry:unregister(test_morph),

    % Should no longer be found
    Result = morphology_registry:get(test_morph),
    ?_assertEqual({error, not_found}, Result).

test_list_all_empty(_Pid) ->
    % Initially empty
    Result = morphology_registry:list_all(),
    ?_assertEqual([], Result).

test_list_all_multiple(_Pid) ->
    ok = compile_test_morphology(),
    ok = morphology_registry:register(morph1, test_morphology_module),
    ok = morphology_registry:register(morph2, test_morphology_module),

    % Should list both
    Result = lists:sort(morphology_registry:list_all()),
    ?_assertEqual([morph1, morph2], Result).

test_is_registered(_Pid) ->
    ok = compile_test_morphology(),
    ok = morphology_registry:register(test_morph, test_morphology_module),

    % Test both cases
    [
     ?_assertEqual(true, morphology_registry:is_registered(test_morph)),
     ?_assertEqual(false, morphology_registry:is_registered(nonexistent))
    ].

%%==============================================================================
%% Test Helpers
%%==============================================================================

%% Dynamically create a valid test morphology module
compile_test_morphology() ->
    Forms = test_morphology_forms(),
    {ok, Module, Binary} = compile:forms(Forms),
    code:load_binary(Module, "test_morphology_module.erl", Binary),
    ok.

test_morphology_forms() ->
    [
     {attribute, 1, module, test_morphology_module},
     {attribute, 2, export, [{get_sensors, 1}, {get_actuators, 1}]},

     % get_sensors/1
     {function, 3, get_sensors, 1,
      [{clause, 3, [{atom, 3, test_morph}], [],
        [{cons, 3, {tuple, 3, [{atom, 3, sensor}]}, {nil, 3}}]}]},

     % get_actuators/1
     {function, 4, get_actuators, 1,
      [{clause, 4, [{atom, 4, test_morph}], [],
        [{cons, 4, {tuple, 4, [{atom, 4, actuator}]}, {nil, 4}}]}]},

     {eof, 5}
    ].

%% Dynamically create an invalid morphology module (missing get_actuators/1)
compile_bad_morphology() ->
    Forms = [
             {attribute, 1, module, bad_morphology_module},
             {attribute, 2, export, [{get_sensors, 1}]},

             % Only has get_sensors/1, missing get_actuators/1
             {function, 3, get_sensors, 1,
              [{clause, 3, [{atom, 3, bad_morph}], [],
                [{cons, 3, {tuple, 3, [{atom, 3, sensor}]}, {nil, 3}}]}]},

             {eof, 4}
            ],
    {ok, Module, Binary} = compile:forms(Forms),
    code:load_binary(Module, "bad_morphology_module.erl", Binary),
    ok.
