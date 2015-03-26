# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserve.
ERL ?= erl
REBAR=bin/rebar
APP := ews

.PHONY: deps

all: deps
	@${REBAR} -j compile

test: all
	@${REBAR} -j eunit skip_deps=true

xref: all
	@${REBAR} -j xref skip_deps=true

deps:
	@${REBAR} -j get-deps

clean:
	@${REBAR} -j clean
	rm -fr logs

dist-clean: clean
	@rm -rf .eunit
	@${REBAR} -j delete-deps
