%%%%
% cherl_server
%
% This module implements an OTP chat server application. It has a very basic
% (toy) authentication layer backed by a users ETS table.
%
% The server process can do two things:
%   - Handle a new client with a create_client tagged call.
%   - Recieve and redistribute a chat message from a user with a chat tagged
%     cast.
%
% A server process can be started by calling server/0 -- be sure to set the
% node name too.
%%%%

-module(cherl_server).

-export([start/0]).
-export([handle_call/3, handle_cast/2, init/1]).

-behavior(gen_server).

%% Server API functions are below.

start() ->
  % - Creates a cherl_server OTP application process
  {ok, Pid} = gen_server:start(?MODULE, [], []),
  register(cherl_server, Pid).

%% Non-api functions

chat(Username, Message) ->
  % We've recieved an authenticated chat, broadcast the message
  % to all the users in the table.
  io:format("[CHAT] ~s: ~s~n", [Username, Message]),
  ets:foldl(
    fun(Entry, Acc) ->
      {IterUsername, {_, IterNode}} = Entry,
      case Username of
        IterUsername -> Acc; % Don't echo the user's message to themselves
        _ -> spawn(IterNode, gen_server, cast, [client, {server_message, Username, Message}]),
             Acc % Just recycle this accumulator, we don't do anything with it.
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
   {Username, {Password, Node}} -> io:format("[INFO] User ~s has reconnected.~n", [Username]);
    _ -> io:format("[WARNING] User ~s already exists, and authentication failed!~n", [Username])
  end,
  {reply, ok, LoopData}.

handle_cast({chat, Username, Password, Message}, LoopData) ->
  % We've recieved a chat. If the user authenticates successfully, handle the
  % chat. Log if we don't recognize the user or the user fails auth.
  case ets:lookup(server_users, Username) of
    [{Username, {Password, _}}] -> chat(Username, Message);
    [] -> io:format("[WARNING] No such user: ~s~n", [Username]);
    _ -> io:format("[WARNING] Unauthorized access attempt for: ~s~n", [Username])
  end,
  {noreply, LoopData}.

init(_Args) ->
  % Create users table, if it doesn't exist
  % Will let us later use multiple server processes
  case ets:info(server_users) of
    undefined -> ets:new(server_users, [named_table]);
    _ -> server_users
  end,
  {ok, null}.
