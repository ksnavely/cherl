#!/bin/bash
cd src
erlc cherl_client.erl
erl -sname $1 -s cherl_client start $2 $3 -noshell
