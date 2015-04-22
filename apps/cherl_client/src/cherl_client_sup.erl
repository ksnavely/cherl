-module(cherl_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link([CherlServer, Username, Password]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [CherlServer, Username, Password]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([CherlServer, Username, Password]) ->
    Children = [{
        cherl_client,              % Id
        {cherl_client, start, [CherlServer, Username, Password]}, % {Module, Function, Arguments}
        temporary,                 % RestartStrategy
        brutal_kill,               % ShutdownStrategy
        worker,                    % worker or supervisor
        [cherl_client]             % ModuleList which implements the process
    }],
    % {ok, {{RestartStrategy, AllowedRestarts, MaxSeconds}, ChildSpecificationList}}
    {ok, { {one_for_one, 5, 10}, Children} }.

