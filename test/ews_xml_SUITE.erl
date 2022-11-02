-module(ews_xml_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT functions
-export([suite/0, groups/0, all/0]).

%% Tests
-export([tag_with_multiple_namespaces/1,
         namespace_owerwriting/1,
         forbidden_characters/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{xml_test, [shuffle],
      [tag_with_multiple_namespaces,
       namespace_owerwriting,
       forbidden_characters
      ]}].

all() ->
    [{group, xml_test}].

tag_with_multiple_namespaces(_Config) ->
    XMLString = "<a:test xmlns:a=\"ns-a\" xmlns:b=\"ns-b\" ><b:test2/></a:test>",
    Terms = ews_xml:decode(XMLString),
    Terms = [{{"ns-a", "test"}, [],
             [{{"ns-b","test2"}, [], []}]}].

namespace_owerwriting(_Config) ->
    XMLString = "<test xmlns=\"a\" ><test2 xmlns=\"b\" /></test>",
    Terms = ews_xml:decode(XMLString),
    Terms = [{{"a","test"},[],[{{"b","test2"},[],[]}]}].

forbidden_characters(_Config) ->
    Input = [{{"http://minameddelanden.gov.se/schema/Recipient/v3",
               "register"},
              [],
              [{{"http://minameddelanden.gov.se/schema/Recipient/v3",
                 "accountRequest"},
                [],
                [{{"http://minameddelanden.gov.se/schema/Recipient",
                   "AgreementText"},
                  [],
                  [{txt,<<"yo&\r\n<>öö/utf-8">>}]}]}]}],
    Output = ews_xml:decode(iolist_to_binary(ews_xml:encode(Input))),
    ?assertMatch(Input, Output).
