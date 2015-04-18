-module(cherl_client).

-export([create/3, chat/3, loop/2, start/1]).
-export([handle_cast/2, init/1, terminate/2]).

-behavior(gen_server).

%% Client API functions are below.

create(Node, Username, Password) ->
  %% create
  %% Register a username/password with the cherl server. Start a client OTP
  %% application which can do chats. This one does it to a remote cherl_server.
  spawn(Node, gen_server, call, [cherl_server, {create_client, Username, Password}]),
  {ok, Pid} = gen_server:start(?MODULE, {Username, Password}, []),
  register(list_to_atom(Username), Pid).

start([Node, UsernameAtom]) ->
  Username = atom_to_list(UsernameAtom),
  %% Prompt for remote server, username, password
  io:format("~nWelcome ~s! You will be connected to the cherl server at ~s.~nAll newlines and tabs will be stripped from passwords and chats.~n", [Username, Node]),
  Prompt = io_lib:format("~nPlease enter the password for ~s:~s => ", [Node, Username]),
  Pwd = re:replace(io:get_line(Prompt), "[\\t\\n]+", "", [global,{return,list}]),
  create(Node, Username, Pwd),
  % Now we need a loop to prompt for chats, later recieve chat messages
  io:format("Chat session started.~n", []),
  loop(Node, list_to_atom(Username)).

loop(Node, Client) ->
  Msg = re:replace(io:get_line("=> "), "[\\t\\n]+", "", [global,{return,list}]),
  chat(Node, Client, Msg),
  loop(Node, Client).

chat(Node, Client, Message) ->
  gen_server:cast(Client, {chat, Node, Message}).

%% Client OTP callbacks below

handle_cast({chat, Node, Message}, {Username, Password}) ->
  %% Fancy remote chat
  spawn(Node, gen_server, cast, [cherl_server, {chat, Username, Password, Message}]),
  {noreply, {Username, Password}}.

init({Username, Password}) ->
  {ok, {Username, Password}}.

terminate(_Reason, _LoopData) ->
  ok.
