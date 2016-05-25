%% TODO based on http://20bits.com/article/erlang-a-generalized-tcp-server
%% Heavy WIP and tired coding

% cherl_ssl_server
%
% The idea here is that a new gen_server will create a new SSL socket during
% init, and once the socket is set up, jump into a loop with the socket as the
% state. The loop should recieve messages from the socket and process them as
% cherl server does now.

% Some part of the socket acceptance process is likely blocking. In order to
% prevent blocking of loop execution, spawn a new
-module(cherl_ssl_server).

-behavior(gen_server).

-export([start/3, init/1, accept/1, handle_cast/2, accept_loop/1, fake_cherl_server/1, test/0]).

-record(
    server_state,
    {
        host,
        port,
        loop,
        ssl_sock
    }
).

start(Module, Port, Loop) ->
    ssl:start(),
    State = #server_state{port = Port, loop = Loop},
    gen_server:start_link({local, Module}, ?MODULE, State, []).

init(State = #server_state{port = Port, loop = Loop}) ->
    % Open up a socket
    {ok, ListenSocket} = ssl:listen(9999, [{certfile, "/home/ksnavely/programming/localhost-certs/localhost.crt"}, {keyfile, "/home/ksnavely/programming/localhost-certs/localhost.key"}, {reuseaddr, true}]),
    State2 = State#server_state{ssl_sock = ListenSocket},
    % Kick off the initial connection
    {ok, accept(State2)}.

accept(State = #server_state{ssl_sock = ListenSocket, loop = Loop}) ->
    % Process the rest of the socket acceptance in another process
    proc_lib:spawn(?MODULE, accept_loop, [{self(), ListenSocket, Loop}]),
    State.

handle_cast({accepted, _Pid}, State) ->
    {noreply, accept(State)}.

accept_loop({Pid, ListenSocket, {M, F} = Loop}) ->
    % Do the blocking part of socket acceptance
    {ok, ListenSocket2} = ssl:transport_accept(ListenSocket),
    ok = ssl:ssl_accept(ListenSocket2),

    % We've accepted, cast to trigger this accept_loop again
    gen_server:cast(Pid, {accepted, self()}),

    % Work with the connection
    M:F(ListenSocket2).

fake_cherl_server(Socket) ->
    ssl:send(Socket, "test message"),
    % Recv or something
    timer:sleep(1000),
    fake_cherl_server(Socket).

test() ->
    start(?MODULE, 9999, {?MODULE, fake_cherl_server}).
