-module(ews_alias_SUITE).
-include_lib("common_test/include/ct.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_testcase/2, end_per_testcase/2]).

%% Tests
-export([create_alias_simple/1,
         create_alias_capitals/1,
         create_unique_alias/1,
         create_unique_alias_duplicates/1,
         two_models_sharing_a_type/1,
         two_models_disagreeing_on_a_name/1,
         removing_a_model_forgets_its_aliases/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{create_tests, [shuffle],
      [create_alias_simple,
       create_alias_capitals,
       create_unique_alias,
       create_unique_alias_duplicates,
       two_models_sharing_a_type,
       two_models_disagreeing_on_a_name,
       removing_a_model_forgets_its_aliases
      ]}].

all() ->
    [{group, create_tests}].

init_per_testcase(_TestCase, Config) ->
    {ok, _Pid} = ews_alias:start_link(),
    Config.

end_per_testcase(_TestCase, Config) ->
    ews_alias:stop(),
    Config.

create_alias_simple(_Config) ->
    test = ews_alias:create({"NameSpace", "test"}).

create_alias_capitals(_Config) ->
    this_is_a_test = ews_alias:create({"NameSpace", "ThisIsATest"}).

create_unique_alias(_Config) ->
    Qname = {"namespace", "name"},
    Alias = ews_alias:create_unique(Qname, test),
    Qname = ews_alias:get_qname(Alias, test).

create_unique_alias_duplicates(_Config) ->
    name = ews_alias:create_unique({"namespace", "name"}, test),
    name_1 = ews_alias:create_unique({"namespace2", "name"}, test).

%% Two models can be built in one node, and they can share a type -- an
%% xmldsig schema imported by two APIs, say. Each keeps its own alias for it:
%% the alias belongs to the pair, not to the qname, and the second model to ask
%% must not take the name off the first.
two_models_sharing_a_type(_Config) ->
    Shared = {"http://www.w3.org/2000/09/xmldsig#", "KeyInfoType"},
    key_info_type = ews_alias:create_unique(Shared, first),
    key_info_type = ews_alias:create_unique(Shared, second),
    key_info_type = ews_alias:get_alias(Shared, first),
    key_info_type = ews_alias:get_alias(Shared, second),
    Shared = ews_alias:get_qname(key_info_type, first),
    Shared = ews_alias:get_qname(key_info_type, second),
    %% And a name collision within one model is still resolved per model, so
    %% the second model's first type keeps the plain name.
    name = ews_alias:create_unique({"ns", "name"}, first),
    name_1 = ews_alias:create_unique({"ns2", "name"}, first),
    name = ews_alias:create_unique({"ns2", "name"}, second),
    ok.

%% The other half of per-model aliasing: the shared type has to take the
%% suffixed name in a model where something already holds the plain one, and
%% keep the plain one in the model where nothing does.
two_models_disagreeing_on_a_name(_Config) ->
    Shared = {"http://www.w3.org/2000/09/xmldsig#", "KeyInfoType"},
    Other = {"urn:elsewhere", "KeyInfoType"},
    key_info_type = ews_alias:create_unique(Shared, first),
    key_info_type = ews_alias:create_unique(Other, second),
    key_info_type_1 = ews_alias:create_unique(Shared, second),
    key_info_type = ews_alias:get_alias(Shared, first),
    key_info_type_1 = ews_alias:get_alias(Shared, second),
    %% The map a model reports is its own.
    [{Shared, key_info_type}] = ews_alias:get_alias_map(first),
    [_, _] = ews_alias:get_alias_map(second),
    ok.

%% A model's aliases go when the model does, since nothing else removes them.
removing_a_model_forgets_its_aliases(_Config) ->
    Qname = {"ns", "name"},
    name = ews_alias:create_unique(Qname, first),
    name = ews_alias:create_unique(Qname, second),
    ok = ews_alias:remove_model(first),
    false = ews_alias:get_alias(Qname, first),
    [] = ews_alias:get_alias_map(first),
    %% And only that model's.
    name = ews_alias:get_alias(Qname, second),
    ok.
