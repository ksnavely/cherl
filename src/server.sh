#!/bin/bash
erl -sname $1 -s cherl_server server -noshell
