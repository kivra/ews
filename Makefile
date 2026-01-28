# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserve.
.PHONY: clean distclean upgrade compile test dialyzer eunit xref

default: compile

clean:
	rebar3 clean --all
	rm -rf _build/*/rel
	rm -f _build/*/*/*/ebin/*
	find . -name "erlcinfo" -exec rm {} \;

distclean: clean
	rm -rf .eunit
	rm -rf _build
	rm -f rebar.lock

upgrade:
	rebar3 upgrade

compile:
	rebar3 compile

test: xref eunit ct dialyzer

dialyzer:
	rebar3 dialyzer

eunit:
	rebar3 eunit

ct:
	rebar3 ct

xref:
	rebar3 xref

repl:
	rebar3 as test shell
