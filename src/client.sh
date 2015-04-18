#!/bin/bash
erl -sname $1 -s cherl_client start $2 $3 -noshell
