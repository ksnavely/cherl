# cherl
An erlang based chat server and client. 

## Currently
The chat and client are OTP applications. You can create a single chat server
and pass messages about in the shell.

## TODO
  - Unique login: Server stores credentials in memory, users can create a user
    (conflict if already existing username), then do password based
    authentication when sending chat messages.
  - Distributed server: Be able to spawn and load balance multiple chat server
    instances. Bonus, do it across computers.
  - Supervision and fault tolerance: Be able to recover from just about
    everything dying and restarting.
  - CLI: Provide a CLI for the client that's not the erlang shell.
  - Packaging: The server should be seperate from the client ;)
  - Slack integration for servers
