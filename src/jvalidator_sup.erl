-module(jvalidator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ClientAccess =
		{jvalidator_client_access,
		 {jvalidator_client_access, start_link, []},
		 permanent,
		 brutal_kill,
		 worker,
		 [jvalidator_client_access]
		},

    {ok, {{one_for_one, 5, 10}, 
    	[
    	 ClientAccess
     	]}}.

