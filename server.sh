#!/bin/bash
cd src
erlc cherl_server.erl
erl -sname $1 -s cherl_server start -noshell
