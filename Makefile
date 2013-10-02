REBAR="bin/rebar"
JAVAFOLDERS="apps/ebase/java_src"
PWD=$(shell pwd)
DIALYZER_PLT="$(PWD)/dev.plt"
DIALYZER_OUT="$(PWD)/dialyzer_result.txt"

.PHONY: all compile check test doc clean get-deps update-deps tools

all: get-deps compile

compile:
	@$(REBAR) -j compile

java:
	@$(MAKE) --directory=$(JAVAFOLDERS)

xref:
	@$(REBAR) -jk skip_deps=true xref

$(DIALYZER_PLT):
	mv apps/custmgmt custmgmt
	- dialyzer --output_plt $(DIALYZER_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key -r apps
	mv custmgmt apps/

dialyzer: get-deps compile tools $(DIALYZER_PLT)
	bin/run_dialyzer $(DIALYZER_PLT) $(DIALYZER_OUT)

test: all
	@rm -rf .eunit apps/*/.eunit
	@bin/run_global_tests
	@bin/check_specs_for_reltool.rb
	@$(REBAR) -jk eunit skip_deps=true

external-test: all
	@rm -rf .eunit apps/*/.eunit
	@$(REBAR) -jk eunit skip_deps=true -DEUNIT_EXTERNAL

doc:
	@$(REBAR) -j doc skip_deps=true

clean:
	@$(REBAR) -j clean

clean-java:
	@$(MAKE) --directory=$(JAVAFOLDERS) clean

dist-clean: clean clean-java
	@$(REBAR) -j delete-deps
	@rm -rfv deps apps/ec_scribe/gen_* apps/ec_scribe/test/*_test_dir
	@rm -rf apps/ec_gaw/priv/services/*.bin
	@rm -f ./bin/jenkins
	@rm -fr apps/adhoc_downloader

get-deps:
	@$(REBAR) -j get-deps

update-deps:
	@$(REBAR) -j update-deps
	@$(REBAR) -j get-deps

tools:
	@./bin/bootstrap
