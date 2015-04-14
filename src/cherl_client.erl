-module(cherl_client).

-export([create/2, create/3, chat/2, chat/3, shell_start/3, start/2]).
-export([handle_cast/2, init/1, terminate/2]).

-behavior(gen_server).

%% Client API functions are below.

shell_start(ServerNode, Username, Password) ->
  ok.

create(Username, Password) ->
  %% create
  %% Register a username/password with the cherl server. Start a client OTP
  %% application which can do chats.
  gen_server:call(cherl_server, {create_client, Username, Password}),
  start(Username, Password).

create(Node, Username, Password) ->
  %% create
  %% Register a username/password with the cherl server. Start a client OTP
  %% application which can do chats. This one does it to a remote cherl_server.
  spawn(Node, gen_server, call, [cherl_server, {create_client, Username, Password}]),
  start(Username, Password).

start(Username, Password) ->
  %% Create an OTP application and register the pid based on the username
  {ok, Pid} = gen_server:start(?MODULE, {Username, Password}, []),
  register(list_to_atom(Username), Pid).

chat(Client, Message) ->
  gen_server:cast(Client, {chat, Message}).

chat(Node, Client, Message) ->
  gen_server:cast(Client, {chat, Node, Message}).

%% Client OTP callbacks below

handle_cast({chat, Message}, {Username, Password}) ->
  %% Local shell chat
  gen_server:cast(cherl_server, {chat, Username, Password, Message}),
  {noreply, {Username, Password}};
handle_cast({chat, Node, Message}, {Username, Password}) ->
  %% Fancy remote chat
  spawn(Node, gen_server, cast, [cherl_server, {chat, Username, Password, Message}]),
  {noreply, {Username, Password}}.

init({Username, Password}) ->
  {ok, {Username, Password}}.

terminate(_Reason, _LoopData) ->
  ok.
