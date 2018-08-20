-module(ews_alias_SUITE).
-include_lib("common_test/include/ct.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_testcase/2, end_per_testcase/2]).

%% Tests
-export([create_alias_simple/1,
         create_alias_capitals/1,
         create_unique_alias/1,
         create_unique_alias_duplicates/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{create_tests, [shuffle],
      [create_alias_simple,
       create_alias_capitals,
       create_unique_alias,
       create_unique_alias_duplicates
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
