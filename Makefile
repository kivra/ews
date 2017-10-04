# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserve.
REBAR=bin/rebar3

.PHONY: clean distclean upgrade compile test dialyzer eunit xref

default: compile

clean:
	@${REBAR} clean --all
	rm -rf _build/*/rel
	rm -f _build/*/*/*/ebin/*
	find . -name "erlcinfo" -exec rm {} \;

distclean: clean
	rm -rf .eunit
	rm -rf _build
	rm -f rebar.lock

upgrade:
	@${REBAR} upgrade

compile:
	@${REBAR} compile

test: xref eunit ct dialyzer

dialyzer:
	@${REBAR} dialyzer

eunit:
	@${REBAR} eunit

ct:
	@${REBAR} ct

xref:
	@${REBAR} xref
