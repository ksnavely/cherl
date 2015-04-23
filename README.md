# cherl
An erlang based chat server and client. 

## Currently
The chat and client are OTP applications. You can create a single chat server
and start client processes to interact with the chat server. I've only been
working on this locally and remote erlang nodes may require additional work.

The code is packaged as multiple OTP applications in the apps/ directory.
  - cherl_server: supervised single master chat server
  - cherl_client: supervised chat client, CLI and zombie-load entry points in module

## Usage examples

### Compiling the code

If you update the code compile the erlang by running `rebar compile` in the parent directory. If you are in the same directory as the .erl files you can use `erlc` or `c(...)` in the repl.

### Starting the server

Kick off a server with: `./server SHORT_SERVER_NODE_NAME`. You'll see some log
messages when users connect, are created, fail authentication, chat, etc... The
server also logs all chat messages for debugging purposes.

### Starting a client

Start a client with: `./client.sh CLIENT_NAME FULL_SERVER_NODE_NAME USERNAME`.
You will be prompted for a password to correspond with `USERNAME`.

### Example session

The following is an example session between a server and two clients. Various
erlang compilation warnings are excluded.

```
[20:19:30 cherl] (master)./server.sh chatserver
[INFO] cherl_server has been started with pid: <0.35.0>.
[INFO] Did not find foo, inserting.
[INFO] Did not find baz, inserting.
[CHAT] baz: Hey anybody there?
[CHAT] foo: Yes, hello baz how are you?

───────────────────────────────────────────────────────────────────────────────
[20:20:20 cherl] (master)./client.sh fooclient 'chatserver@WKSTN0011' foo

Welcome foo! You will be connected to the cherl server at chatserver@WKSTN0011.
All newlines and tabs will be stripped from passwords and chats.

Please enter the password for chatserver@WKSTN0011:foo => bar
Chat session started.
=> baz: Hey anybody there?
=> Yes, hello baz how are you?

───────────────────────────────────────────────────────────────────────────────
[20:23:26 cherl] (master)./client.sh barclient 'chatserver@WKSTN0011' baz

Welcome baz! You will be connected to the cherl server at chatserver@WKSTN0011.
All newlines and tabs will be stripped from passwords and chats.

Please enter the password for chatserver@WKSTN0011:baz => womp
Chat session started.
=> Hey anybody there?
=> foo: Yes, hello baz how are you?
```

## Command your own zombie army

I've added hooks to cherl_client.erl so you can spawn your very own chatbot
army. This still needs load balancing optimization but works.

### Starting a zombie swarm

Start a set of cherl clients  with: 
`./zombies.sh SWARM_SHORT_NAME FULL_SERVER_NODE_NAME TOTAL_CLIENTS SLEEP_TIME`.
Each time the horde ticks by `SLEEP_TIME` milliseconds, the stdout will update. Check out the chat server
logs for some action from the horde.

### Example zombie session
```
[00:09:42 cherl] (master)./server.sh chatserver | tee chats5.log                                                                                                                                                                   
[INFO] cherl_server has been started with pid: <0.35.0>.
[INFO] Did not find zombie-1, inserting.
[INFO] Did not find zombie-2, inserting.
[CHAT] zombie-2: zombie chat
[CHAT] zombie-1: zombie chat
[CHAT] zombie-2: zombie chat
[CHAT] zombie-1: zombie chat
[CHAT] zombie-2: zombie chat
[CHAT] zombie-1: zombie chat
...

───────────────────────────────────────────────────────────────────────────────

[00:09:44 cherl] (master)./zombies.sh zserv 'chatserver@WKSTN0011' 2 2000
Building zombie army.
  Host: chatserver@WKSTN0011
  Total Clients: 2
 Sleep Ceiling: 2000

The zombie horde stirs... [1]
The zombie horde stirs... [2]
The zombie horde stirs... [3]
The zombie horde stirs... [4]
The zombie horde stirs... [5]
...
```

## TODO
  - Use eunit for basic unit testing
  - Distributed server: Be able to spawn and load balance multiple chat server
    instances. Bonus, do it across computers.
  - Better supervision and fault tolerance: Be able to recover from just about
    everything dying and restarting.
  - Slack integration for servers
