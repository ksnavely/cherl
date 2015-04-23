#!/bin/bash
erl -sname $1 -pa apps/cherl_server/ebin -s cherl_server start -noshell
