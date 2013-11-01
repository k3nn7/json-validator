-module(jvalidator_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	application:start(jvalidator).

start(_StartType, _StartArgs) ->
	application:start(sasl),
	jvalidator_sup:start_link().

stop(_State) ->
	ok.
