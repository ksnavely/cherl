# cherl
An erlang based chat server and client. 

## Currently
The chat and client are OTP applications. You can create a single chat server
and start client processes to interact with the chat server. I've only been
working on this locally and remote erlang nodes may require additional work.

## Usage examples

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

## TODO
  - Distributed server: Be able to spawn and load balance multiple chat server
    instances. Bonus, do it across computers.
  - Supervision and fault tolerance: Be able to recover from just about
    everything dying and restarting.
  - Slack integration for servers
