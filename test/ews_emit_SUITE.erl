-module(ews_emit_SUITE).
-include_lib("common_test/include/ct.hrl").

%%TODO header should be inside of include, but we need to rename the git repo
%%to ews first
-include("../src/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Tests
-export([empty_table/1,
         simple_graph/1,
         simple_circular_graph/1,
         circular_graph/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{emit_test, [shuffle],
      [empty_table,
       simple_graph,
       simple_circular_graph,
       circular_graph
      ]}].

all() ->
    [{group, emit_test}].

init_per_suite(Config) ->
    ews:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Testcase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

empty_table(_Config) ->
    Table = ets:new(empty_table, []),
    Model = #model{type_map=Table},
    Filename = file_in_test_priv_dir,
    ews_emit:model_to_file(Model, Filename, test, #{}),
    {ok, <<>>} = file:read_file(Filename),
    file:delete(foo).

simple_graph(_Config) ->
    Table = ets:new(type_table, []),
    ets:insert(Table, {{"ns", "t1"},
                       #type{qname={"ns", "t1"},
                            alias = t1,
                            elems =
                                [#elem{qname = {"ns", "e1"},
                                       type = int_type(),
                                       meta = meta(undefined, 0, 1)},
                                 #elem{qname = {"ns", "e2"},
                                       type = string_type(),
                                       meta = meta("true", 0, 1)}]}}),
    Model = #model{type_map=Table},
    Filename = file_in_test_priv_dir,
    ews_emit:model_to_file(Model, Filename, test, #{}),
    {ok, Bin} = file:read_file(Filename),
    <<"-record(t1, {e1 :: integer() | undefined,\n"
      "             e2 :: string() | binary() | nil | undefined}).\n\n">> =
        Bin,
    ok.

simple_circular_graph(_Config) ->
    Table = ets:new(type_table, []),
    ews_alias:create_unique({"ns", "t1"}, test),
    ets:insert(Table, {{"ns", "t1"},
                       #type{qname={"ns", "t1"},
                            alias = t1,
                            elems =
                                [#elem{qname = {"ns", "e1"},
                                       type = {"ns", "t1"},
                                       meta = meta(undefined, 0, 1)}]}}),
    Model = #model{type_map=Table},
    Filename = file_in_test_priv_dir,
    ews_emit:model_to_file(Model, Filename, test, #{}),
    {ok, Bin} = file:read_file(Filename),
    <<"-record(t1, {e1 :: #t1{} | undefined}).\n\n">> =
        Bin,
    ok.

circular_graph(_Config) ->
    Table = ets:new(type_table, []),
    ews_alias:create_unique({"ns", "t1"}, test),
    ews_alias:create_unique({"ns", "t2"}, test),
    ets:insert(Table, {{"ns", "t1"},
                       #type{qname={"ns", "t1"},
                            alias = t1,
                            elems =
                                [#elem{qname = {"ns", "t1e1"},
                                       type = {"ns", "t2"},
                                       meta = meta(undefined, 0, 1)}]}}),
    ets:insert(Table, {{"ns", "t2"},
                       #type{qname={"ns", "t2"},
                            alias = t2,
                            elems =
                                [#elem{qname = {"ns", "t2e1"},
                                       type = {"ns", "t1"},
                                       meta = meta(undefined, 0, 1)}]}}),
    Model = #model{type_map=Table},
    Filename = file_in_test_priv_dir,
    ews_emit:model_to_file(Model, Filename, test, #{}),
    {ok, Bin} = file:read_file(Filename),
    B1 = <<"-type '#t1'() :: tuple().  %% Needed due to circular type definition\n\n"
           "-record(t2, {t2e1 :: '#t1'() | undefined}).\n\n"
           "-record(t1, {t1e1 :: #t2{} | undefined}).\n\n">>,
    B2 = <<"-type '#t2'() :: tuple().  %% Needed due to circular type definition\n\n"
           "-record(t1, {t1e1 :: '#t2'() | undefined}).\n\n"
           "-record(t2, {t2e1 :: #t1{} | undefined}).\n\n">>,
    true = B1 == Bin orelse B2 == Bin orelse Bin,
    ok.

int_type() ->
    #base{xsd_type = {"http://www.w3.org/2001/XMLSchema","int"},
          erl_type = integer,
          list = false,
          union = false}.

string_type() ->
    #base{xsd_type = {"http://www.w3.org/2001/XMLSchema","string"},
          erl_type = string,
          list = false,
          union = false}.

meta(Nillable, Min, Max) ->
    #meta{nillable = Nillable, min = Min, max = Max}.
