%% @doc Logging infrastructure for faber-tweann.
%%
%% Provides structured logging with different severity levels using OTP logger.
%% This module wraps the OTP logger to provide a consistent interface across
%% the TWEANN library.
%%
%% == Usage ==
%%
%% Debug level - for detailed diagnostic information:
%%   tweann_logger:debug("Mutation started for agent ~p", [AgentId])
%%
%% Info level - for significant but normal events:
%%   tweann_logger:info("Population evaluation complete, generation ~p", [Gen])
%%
%% Warning level - for unexpected but recoverable conditions:
%%   tweann_logger:warning("Agent ~p failed evaluation: ~p", [AgentId, Reason])
%%
%% Error level - for errors that require attention:
%%   tweann_logger:error("Database operation failed: ~p", [Reason])
%%
%% == Configuration ==
%%
%% Log level can be configured in sys.config:
%%   {kernel, [
%%     {logger_level, info}  %% debug | info | warning | error
%%   ]}
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(tweann_logger).

-export([
    debug/2,
    info/2,
    warning/2,
    error/2
]).

%% ============================================================================
%% API
%% ============================================================================

%% @doc Log debug message.
%%
%% Use for detailed diagnostic information useful during development.
%%
%% Example:
%%   tweann_logger:debug("Adding neuron to agent ~p", [AgentId])
%%
%% @param Format format string (printf-style)
%% @param Args list of arguments for format string
-spec debug(Format, Args) -> ok when
    Format :: string(),
    Args :: [term()].
debug(Format, Args) ->
    log(debug, Format, Args).

%% @doc Log info message.
%%
%% Use for significant normal events (milestones, completions).
%%
%% Example:
%%   tweann_logger:info("Generation ~p complete, best fitness: ~p", [Gen, Fitness])
%%
%% @param Format format string (printf-style)
%% @param Args list of arguments for format string
-spec info(Format, Args) -> ok when
    Format :: string(),
    Args :: [term()].
info(Format, Args) ->
    log(info, Format, Args).

%% @doc Log warning message.
%%
%% Use for unexpected but recoverable conditions.
%%
%% Example:
%%   tweann_logger:warning("Mutation failed for agent ~p: ~p", [AgentId, Reason])
%%
%% @param Format format string (printf-style)
%% @param Args list of arguments for format string
-spec warning(Format, Args) -> ok when
    Format :: string(),
    Args :: [term()].
warning(Format, Args) ->
    log(warning, Format, Args).

%% @doc Log error message.
%%
%% Use for errors requiring attention.
%%
%% Example:
%%   tweann_logger:error("Database write failed: ~p", [Reason])
%%
%% @param Format format string (printf-style)
%% @param Args list of arguments for format string
-spec error(Format, Args) -> ok when
    Format :: string(),
    Args :: [term()].
error(Format, Args) ->
    log(error, Format, Args).

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% @private Log message at specified level using OTP logger.
-spec log(Level, Format, Args) -> ok when
    Level :: debug | info | warning | error,
    Format :: string(),
    Args :: [term()].
log(Level, Format, Args) ->
    logger:log(Level, Format, Args),
    ok.
