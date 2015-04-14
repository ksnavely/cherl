-module(cherl_client).

-export([create/2, test/2]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).

-behavior(gen_server).

%% Client API functions are below.

%% create
%% Register a username/password with the cherl server. Kick off a client
%% OTP application which can do chats.
create(Username, Password) ->
  gen_server:call(cherl_server, {create_client, Username, Password}),
  {ok, Pid} = gen_server:start(?MODULE, {Username, Password}, []),
  register(Username, Pid).

test(Username, Message) ->
  gen_server:cast(Username, {chat, Message}).

%% Client OTP callbacks below

handle_cast({chat, Message}, {Username, Password}) ->
  gen_server:cast(cherl_server, {chat, Username, Password, Message}),
  {noreply, {Username, Password}}.

handle_call({chat, Message}, _From, {Username, Password}) ->
  gen_server:call(cherl_server, {chat, Username, Password, Message}),
  {reply, ok, {Username, Password}}.

init({Username, Password}) ->
  {ok, {Username, Password}}.

terminate(_Reason, _LoopData) ->
  ok.
