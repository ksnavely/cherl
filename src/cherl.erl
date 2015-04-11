-module(cherl).

-export([chat/2, start_chat/1, start_server/0]).
-export([init/0, init_client/1]).

%% Server functions are below.

init() ->
  loop().

start_server() ->
  register(cherl_server, spawn(cherl, init, [])).

loop() ->
  receive
    {chat, ChatClient, Msg} ->
      io:format("~s: ~s!~n",[ChatClient, Msg])
  end.

%% Client functions are below.

start_chat(ChatClient) ->
  register(ChatClient, spawn(cherl, init_client, [ChatClient])).

init_client(ChatClient) ->
  loop_client(ChatClient).

loop_client(ChatClient) ->
  receive
    {chat, Msg} ->
      cherl_server ! {chat, ChatClient, Msg}
  end.

chat(ChatClient, Msg) ->
  ChatClient ! {chat, Msg}.
