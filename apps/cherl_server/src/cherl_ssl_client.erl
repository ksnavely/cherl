-module(cherl_ssl_client).

-export([start/0, go/0]).

start() ->
    ssl:start(),
    {ok, Socket} = ssl:connect("localhost", 9999, [], infinity),
    go().

go() ->
    c:flush(),
    go().
