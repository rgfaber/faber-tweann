%% @doc Registry for morphology modules.
%%
%% Provides runtime registration of morphology implementations. Applications
%% can register custom morphologies without modifying the faber-tweann library.
%%
%% Usage:
%%
%% Start registry (done automatically by application):
%% morphology_registry:start_link().
%%
%% Register a morphology:
%% ok = morphology_registry:register(my_problem, my_morphology_module).
%%
%% Get registered module:
%% {ok, my_morphology_module} = morphology_registry:get(my_problem).
%%
%% List all registered:
%% [my_problem, xor_mimic] = morphology_registry:list_all().
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_registry).

-behaviour(gen_server).

-compile({no_auto_import, [get/1]}).

%% API
-export([
    start_link/0,
    register/2,
    unregister/1,
    get/1,
    list_all/0,
    is_registered/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, morphology_registry_table).

-record(state, {}).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Start the morphology registry server.
%%
%% Creates ETS table for storing morphology registrations.
%%
%% @returns {ok, Pid} | {error, Reason}
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a morphology module.
%%
%% Verifies that the module implements morphology_behaviour before registering.
%%
%% Example:
%%
%% ok = morphology_registry:register(xor_mimic, morphology_xor).
%%
%% @param MorphologyName Atom identifying the morphology (e.g., xor_mimic)
%% @param Module Module implementing morphology_behaviour
%% @returns ok | {error, Reason}
-spec register(atom(), module()) -> ok | {error, term()}.
register(MorphologyName, Module) when is_atom(MorphologyName), is_atom(Module) ->
    gen_server:call(?SERVER, {register, MorphologyName, Module}).

%% @doc Unregister a morphology.
%%
%% @param MorphologyName The morphology to remove
%% @returns ok
-spec unregister(atom()) -> ok.
unregister(MorphologyName) when is_atom(MorphologyName) ->
    gen_server:call(?SERVER, {unregister, MorphologyName}).

%% @doc Get the module for a registered morphology.
%%
%% @param MorphologyName The morphology to look up
%% @returns {ok, Module} | {error, not_found}
-spec get(atom()) -> {ok, module()} | {error, not_found}.
get(MorphologyName) when is_atom(MorphologyName) ->
    case ets:lookup(?TABLE, MorphologyName) of
        [{MorphologyName, Module}] -> {ok, Module};
        [] -> {error, not_found}
    end.

%% @doc List all registered morphologies.
%%
%% @returns List of morphology names
-spec list_all() -> [atom()].
list_all() ->
    [Name || {Name, _Module} <- ets:tab2list(?TABLE)].

%% @doc Check if a morphology is registered.
%%
%% @param MorphologyName The morphology to check
%% @returns boolean()
-spec is_registered(atom()) -> boolean().
is_registered(MorphologyName) when is_atom(MorphologyName) ->
    case get(MorphologyName) of
        {ok, _} -> true;
        {error, not_found} -> false
    end.

%%==============================================================================
%% gen_server Callbacks
%%==============================================================================

%% @private
init([]) ->
    % Create ETS table for morphology storage
    _Tab = ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]),
    {ok, #state{}}.

%% @private
handle_call({register, MorphologyName, Module}, _From, State) ->
    % Verify module is loaded
    case code:ensure_loaded(Module) of
        {module, Module} ->
            % Verify module exports required callbacks
            case verify_morphology_module(Module) of
                ok ->
                    ets:insert(?TABLE, {MorphologyName, Module}),
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, {module_not_loaded, Reason}}, State}
    end;

handle_call({unregister, MorphologyName}, _From, State) ->
    ets:delete(?TABLE, MorphologyName),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Verify that a module implements morphology_behaviour.
%%
%% Checks that the module exports get_sensors/1 and get_actuators/1.
%%
%% @param Module The module to verify
%% @returns ok | {error, Reason}
-spec verify_morphology_module(module()) ->
    ok | {error, {missing_callback, get_sensors | get_actuators, 1}}.
verify_morphology_module(Module) ->
    Exports = Module:module_info(exports),
    HasGetSensors = lists:member({get_sensors, 1}, Exports),
    HasGetActuators = lists:member({get_actuators, 1}, Exports),

    if
        HasGetSensors andalso HasGetActuators ->
            ok;
        not HasGetSensors ->
            {error, {missing_callback, get_sensors, 1}};
        not HasGetActuators ->
            {error, {missing_callback, get_actuators, 1}}
    end.
