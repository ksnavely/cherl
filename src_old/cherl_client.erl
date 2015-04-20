%%%%
% cherl_client
%
% The cherl_client module implements a chat client suitable for interacting
% with a cherl_server process.
%
% When start/1 is called, it kicks off the creation of a user with the cherl
% server. The user may already exist so long as the password matches what is
% stored on the server.
%
% start/1 will drop one into an io:get_line/1 prompt. Messages broadcasted by
% the server will appear as USERNAME: MESSAGE output in the terminal. At any
% one can enter a message which will be sent to the server. Messages by the
% local user/client will appear without the username prefix.
%%%%

-module(cherl_client).

-export([start/1]).
-export([handle_cast/2, init/1]).

-behavior(gen_server).

%% Client API functions are below.

start([Node, UsernameAtom]) ->
  Username = atom_to_list(UsernameAtom),
  % Display a welcome message, and prompt for a password
  io:format("~nWelcome ~s! You will be connected to the cherl server at ~s.~nAll newlines and tabs will be stripped from passwords and chats.~n", [Username, Node]),
  Prompt = io_lib:format("~nPlease enter the password for ~s:~s => ", [Node, Username]),
  Pwd = re:replace(io:get_line(Prompt), "[\\t\\n]+", "", [global,{return,list}]),
  create(Node, Username, Pwd),
  io:format("Chat session started.~n", []),
  % Enter the chat prompt loop
  loop(Node).

%% Non-api functions

create(Node, Username, Password) ->
  % create
  % Register a username/password with the cherl server. Start a client OTP
  % application which can do chats. This one does it to a remote cherl_server.
  spawn(Node, gen_server, call, [cherl_server, {create_client, Username, Password, node()}]),
  {ok, Pid} = gen_server:start(?MODULE, {Username, Password}, []),
  register(client, Pid).

loop(Node) ->
  % The chat prompt loop. Loop data is the cherl server node.
  Msg = re:replace(io:get_line("=> "), "[\\t\\n]+", "", [global,{return,list}]),
  gen_server:cast(client, {chat, Node, Msg}),
  loop(Node).

%% Client OTP callbacks below

handle_cast({chat, Node, Message}, {Username, Password}) ->
  % Send a chat message to the cherl server.
  spawn(Node, gen_server, cast, [cherl_server, {chat, Username, Password, Message}]),
  {noreply, {Username, Password}};
handle_cast({server_message, Sender, Message}, {Username, Password}) ->
  % The client has received a chat from the server
  io:format("~s: ~s~n", [Sender, Message]),
  {noreply, {Username, Password}}.

init({Username, Password}) ->
  % The username and password are used as loop data for authentication with
  % the cherl_server process.
  {ok, {Username, Password}}.
