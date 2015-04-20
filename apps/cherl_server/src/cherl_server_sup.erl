-module(cherl_server_sup).

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
    Children = [{
        cherl_server,              % Id
        {cherl_server, start, []}, % {Module, Function, Arguments}
        temporary,                 % RestartStrategy
        brutal_kill,               % ShutdownStrategy
        worker,                    % worker or supervisor
        [cherl_server]             % ModuleList which implements the process
    }],
    % {ok, {{RestartStrategy, AllowedRestarts, MaxSeconds}, ChildSpecificationList}}
    {ok, {{one_for_one, 5, 10}, Children}}.

