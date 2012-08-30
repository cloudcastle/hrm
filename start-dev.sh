#!/bin/sh
rebar compile && erl -sname hrm -pa ./ebin -pa ./deps/*/ebin -boot start_sasl -s hrm_app -sasl errlog_type error -eval "sync:go()."