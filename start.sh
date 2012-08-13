#!/bin/sh
erl -sname hrm -pa ./ebin -pa ./deps/*/ebin -boot start_sasl -s hrm -eval "sync:go()."