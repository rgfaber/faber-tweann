%% @doc Behaviour for implementing morphologies (sensor/actuator specifications).
%%
%% A morphology defines the I/O interface between a neural network and its environment.
%% Applications implement this behaviour to create custom morphologies without modifying
%% the faber-tweann library.
%%
%% Example Implementation:
%%
%% -module(my_morphology).
%% -behaviour(morphology_behaviour).
%%
%% -export([get_sensors/1, get_actuators/1]).
%% -include("records.hrl").
%%
%% get_sensors(my_problem) ->
%%     [#sensor{name = my_sensor, vl = 3, ...}];
%% get_sensors(_) ->
%%     error(invalid_morphology).
%%
%% get_actuators(my_problem) ->
%%     [#actuator{name = my_actuator, vl = 2, ...}];
%% get_actuators(_) ->
%%     error(invalid_morphology).
%%
%% Then register at runtime:
%%
%% morphology_registry:register(my_problem, my_morphology).
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_behaviour).

-include("records.hrl").

%% Behaviour callbacks
-callback get_sensors(MorphologyName :: atom()) -> [#sensor{}].
-callback get_actuators(MorphologyName :: atom()) -> [#actuator{}].
