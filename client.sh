#!/bin/bash
erl -sname $1 -pa apps/cherl_client/ebin -s cherl_client go $2 $3 -noshell
