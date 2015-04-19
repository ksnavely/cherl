-module(cherl_server).

-export([server/0]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).

-behavior(gen_server).

%% Server API functions are below.

%% server
%%
%% - Creates a cherl_server OTP application process
server() ->
  {ok, Pid} = gen_server:start(?MODULE, [], []),
  register(cherl_server, Pid).

%% Non-api functions

chat(Username, Message) ->
  % We've recieved an authenticated chat
  io:format("~s: ~s~n", [Username, Message]),
  ets:foldl(
    fun(Entry, Acc) ->
      {IterUsername, {_, IterNode}} = Entry,
      case Username of
        IterUsername -> Acc; % Don't echo the user's message to themselves
        _ -> spawn(IterNode, gen_server, cast, [client, {server_message, Username, Message}]),
             Acc % Just recycle this
      end
    end,
    0,
    server_users
  ).

%% Server OTP callbacks below

handle_call({create_client, Username, Password, Node}, _From, LoopData) ->
  % Register the username/password with our server_users ETS table
  case ets:lookup(server_users, Username) of
   [] -> io:format("[INFO] Did not find ~s, inserting.~n", [Username]),
         ets:insert(server_users, {Username, {Password, Node}});
    _ -> io:format("[WARNING] User ~s already exists!~n", [Username])
  end,
  {reply, ok, LoopData}.

handle_cast({chat, Username, Password, Message}, LoopData) ->
  case ets:lookup(server_users, Username) of
    [{Username, {Password, _}}] -> chat(Username, Message);
    [] -> io:format("[WARNING] No such user: ~s~n", [Username]);
    _ -> io:format("[WARNING] Unauthorized access attempt for: ~s~n", [Username])
  end,
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
