-module(cherl_server).

-export([server/0, test/1]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).

-behavior(gen_server).

%% Server API functions are below.

%% server
%%
%% - Initiates server_users table if it doesn't exist
%% - Creates a cherl_server OTP application process
server() ->
  %% Start the server OTP application
  {ok, Pid} = gen_server:start(?MODULE, [], []),
  register(cherl_server, Pid).

test(Message) ->
  gen_server:cast(?MODULE, {cherl_server, Message}).

%% Server OTP callbacks below

handle_call({create_client, Username, Password}, _From, LoopData) ->
  % Register the username/password with our server_users ETS table
  case ets:lookup(server_users, Username) of
   [] -> io:format("Did not find ~s, inserting.~n", [Username]),
         ets:insert(server_users, {Username, Password});
    _ -> io:format("User ~s already exists!~n", [Username])
  end,
  {reply, ok, LoopData}.

handle_cast({chat, Username, Password, Message}, LoopData) ->
  io:format("~s: ~s!~n",[Username, Message]),
  {noreply, LoopData}.

init(_Args) ->
  %% Create users table, if it doesn't exist
  %% Will let us later use multiple server processes
  case ets:info(server_users) of
    undefined -> ets:new(server_users, [named_table]);
    _ -> server_users
  end,

  {ok, null}.

terminate(_Reason, _LoopData) ->
  ok.
