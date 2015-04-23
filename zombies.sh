#!/bin/bash
# args: zombie_node_name server_name n_zombies t_sleep
#  $1 - zombie_node_name: A short node name for the zombie host e.g. 'bar'
#  $2 - server_name: The full cherl_server node name e.g. 'foo@WKSTN0011'
#  $3 - n_zombies: The total number of chat clients to create.
#  $4 - t_sleep: The ceiling of a random sleep time in millisencons
erl -sname $1 -pa apps/cherl_client/ebin -s cherl_client zombies $2 $3 $4 -noshell
