-module(ews_emit_SUITE).
-include_lib("common_test/include/ct.hrl").

%%TODO header should be inside of include, but we need to rename the git repo
%%to ews first
-include("../src/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_testcase/2, end_per_testcase/2]).

%% Tests
-export([empty_table/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{emit_test, [shuffle],
      [empty_table
      ]}].

all() ->
    [{group, emit_test}].

init_per_testcase(empty_table, Config) ->
    Table = ets:new(empty_table, []),
    Model = #model{type_map=Table},
    Filename = file_in_test_priv_dir,
    ews_emit:model_to_file(Model, Filename, test),
    [{filename, Filename} | Config].

end_per_testcase(_TestCase, Config) ->
    Config.

empty_table(Config) ->
    Filename = proplists:get_value(filename, Config),
    {ok, <<>>} = file:read_file(Filename).
