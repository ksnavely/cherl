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

-export([cli/0, go/1, login/0, start/3, zombies/1]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).

-behavior(gen_server).

%% Zombie army API

zombies([CherlServer, TotalClientsAtom, SleepTimeAtom]) ->
    % spawn TotalClients worth of client processes, log them in
    % they'll chat uniformly over a period of SleepTime milliseconds
    {TotalClients, []} = string:to_integer(atom_to_list(TotalClientsAtom)),
    {SleepTime, []} = string:to_integer(atom_to_list(SleepTimeAtom)),

    io:format(
        "Building zombie army.~n  Host: ~s~n  Total Clients: ~s~n  Sleep Ceiling: ~s~n~n",
        [CherlServer, integer_to_list(TotalClients), integer_to_list(SleepTime)]
    ),
    Zombies = [make_zombie(CherlServer, N) || N <-lists:seq(1, TotalClients)],
    zombie_chat_loop(Zombies, SleepTime, 1).

make_zombie(CherlServer, N) ->
    ZPassword = "zombiepass",
    Id = io_lib:format("zombie-~B", [N]),
    {ok, Pid} = gen_server:start(?MODULE, {CherlServer, Id, ZPassword}, []),
    gen_server:call(Pid, {create_client}),
    {zombie, Id, Pid}.

zombie_chat_loop(Zombies, SleepTime, Acc) ->
    % Fire off callbacks for randomly timed zombie chats
    %   Zombies: list of {zombie, ZId, ZPid} tuples
    %   SleepTime: Sleep ceiling in ms
    %   Acc: recurse counter
    io:format("The zombie horde stirs... [~B]~n", [Acc]),
    lists:foldl(
        fun(Zombie, Acc) -> 
            {zombie, ZId, ZPid} = Zombie,
            gen_server:cast(ZPid, {zombie_chat, SleepTime}),
            Acc
        end,
        0,
        Zombies
    ),
    % Now wait the full sleep time, so ~all callbacks complete
    % randomly/uniformly. Then recurse and bump the call counter.
    timer:sleep(SleepTime),
    zombie_chat_loop(Zombies, SleepTime, Acc + 1).

%% CLI Client API functions are below.

go([CherlServer, UsernameAtom]) ->
  % Display a welcome message, and prompt for a password
  Username = atom_to_list(UsernameAtom),
  io:format("~nWelcome ~s! You will be connected to the cherl server at ~s.~nAll newlines and tabs will be stripped from passwords and chats.~n", [Username, CherlServer]),
  Prompt = io_lib:format("~nPlease enter the password for ~s:~s => ", [CherlServer, Username]),
  Pwd = re:replace(io:get_line(Prompt), "[\\t\\n]+", "", [global,{return,list}]),
  
  % Start the client OTP application
  cherl_client_sup:start_link([CherlServer, Username, Pwd]),

  % Attempt login/user creation
  login(),
  io:format("Login complete.~n", []),

  % Recursive CLI chat prompt
  cli().

% Supervisor entry point

start(CherlServer, Username, Password) ->
  % Start the client OTP application, and register an handy atom to the pid
  {ok, Pid} = gen_server:start(?MODULE, {CherlServer, Username, Password}, []),
  register(cherl_client, Pid),
  {ok, Pid}.

%% Non-api functions

login() ->
  % Pass user credentials to the server. TODO authn failure modes
  gen_server:call(cherl_client, {create_client}).

cli() ->
  % The chat prompt loop. Loop data is the cherl server node.
  Msg = re:replace(io:get_line("=> "), "[\\t\\n]+", "", [global,{return,list}]),
  gen_server:cast(cherl_client, {chat, Msg}),
  cli().

%% Client OTP callbacks below

handle_call({create_client}, _From, {CherlServer, Username, Password}) ->
  spawn(CherlServer, gen_server, call, [cherl_server, {create_client, Username, Password, node()}]),
  {reply, ok, {CherlServer, Username, Password}}.

handle_cast({chat, Message}, {CherlServer, Username, Password}) ->
  % Send a chat message to the cherl server.
  spawn(CherlServer, gen_server, cast, [cherl_server, {chat, Username, Password, Message}]),
  {noreply, {CherlServer, Username, Password}};
handle_cast({server_message, Sender, Message}, {CherlServer, Username, Password}) ->
  % The client has received a chat from the server
  io:format("~s: ~s~n", [Sender, Message]),
  {noreply, {CherlServer, Username, Password}};
handle_cast({zombie_chat, SleepTime}, {CherlServer, Username, Password}) ->
  % TODO randome sleep time
  timer:sleep(random:uniform(SleepTime)),
  % Send a zombie chat message to the server
  Message = "zombie chat",
  spawn(CherlServer, gen_server, cast, [cherl_server, {chat, Username, Password, Message}]),
  {noreply, {CherlServer, Username, Password}}.
   

init({CherlServer, Username, Password}) ->
  % The username and password are used as loop data for authentication with
  % the cherl_server process.
  {ok, {CherlServer, Username, Password}}.

terminate(_Reason, _LoopData) ->
  ok.
