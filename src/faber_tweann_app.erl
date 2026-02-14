%% @doc Application behaviour for faber_tweann.
%%
%% Starts the morphology registry on application startup.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(faber_tweann_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%==============================================================================
%% application Callbacks
%%==============================================================================

%% @doc Start the application.
%%
%% Starts the morphology registry supervisor.
-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
            StartArgs :: term()) ->
    {ok, pid()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    faber_tweann_sup:start_link().

%% @doc Stop the application.
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
