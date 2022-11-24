-module(ews_xml_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("ews/src/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0, init_per_suite/1, end_per_suite/1]).

%% Tests
-export([tag_with_multiple_namespaces/1,
         namespace_owerwriting/1,
         forbidden_characters/1,
         schema_with_string_enum/1,
         reference_in_parts/1,
         choice_in_sequence/1,
         import_any_order/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{xml_test, [shuffle],
      [tag_with_multiple_namespaces,
       namespace_owerwriting,
       forbidden_characters
      ]},
      {xsd_test, [shuffle],
       [import_any_order,
        reference_in_parts,
        choice_in_sequence,
        schema_with_string_enum
       ]}].

all() ->
    [{group, xml_test},
     {group, xsd_test}].

init_per_suite(Config) ->
    application:ensure_all_started(ews),
    Config.
end_per_suite(Config) ->
    Config.

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

import_any_order(_Config) ->
    Dir = filename:join(code:priv_dir(ews), "../test"),
    File = filename:join(Dir, "importer.xsd"),
    {ok, XsdBin} = file:read_file(File),
    {Schema, _} = xmerl_scan:string(binary_to_list(XsdBin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),

    {Model, default_testtest} =
        ews_xsd:parse_schema(Schema, {undefined, default_testtest, Dir}),

    ?assertMatch(#model{}, Model),
    #model{type_map = TypeMap} = Model,

    Signatures = ews_model:get({"http://example.com/importee","Signatures"}, TypeMap),
    ?assertMatch(#type{alias = signatures}, Signatures),
    Signed = ews_model:get_elem({"http://example.com/importer","Signed"}, TypeMap),
    ?assertMatch(#elem{}, Signed),
    Sealed = ews_model:get({"http://example.com/importer","Sealed"}, TypeMap),
    ?assertMatch(#type{alias = sealed}, Sealed),
    ok.

reference_in_parts(_Config) ->
    Dir = filename:join(code:priv_dir(ews), "../test"),
    File = filename:join(Dir, "importer.xsd"),
    {ok, XsdBin} = file:read_file(File),
    {Schema, _} = xmerl_scan:string(binary_to_list(XsdBin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),

    {Model, choice} =
        ews_xsd:parse_schema(Schema, {undefined, choice, Dir}),

    ?assertMatch(#model{}, Model),
    #model{type_map = TypeMap} = Model,

    Signatures = ews_model:get({"http://example.com/importee","Signatures"}, TypeMap),
    %% Make sure references are handled and put in type in model
    ?assertMatch(#type{ alias = signatures
                      , elems = [#elem{qname = {"http://www.w3.org/2000/09/xmldsig#",
                                                "Signature"},
                                       type = {"http://www.w3.org/2000/09/xmldsig#",
                                               "SignatureType"},
                                       meta = #meta{nillable = undefined,
                                                    default = undefined,
                                                    fixed = undefined,
                                                    max = 1,min = 1}}]
                      }, Signatures),
    ok.

choice_in_sequence(_Config) ->
    Dir = filename:join(code:priv_dir(ews), "../test"),
    File = filename:join(Dir, "importer.xsd"),
    {ok, XsdBin} = file:read_file(File),
    {Schema, _} = xmerl_scan:string(binary_to_list(XsdBin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),

    {Model, default_testtest} =
        ews_xsd:parse_schema(Schema, {undefined, default_testtest, Dir}),

    ?assertMatch(#model{}, Model),
    #model{type_map = TypeMap} = Model,

    X509DT = ews_model:get({"http://www.w3.org/2000/09/xmldsig#","X509DataType"},
                           TypeMap),
    %% Make sure references are handled and put in type in model
    ?assertMatch(#type{ alias = x509_data_type
                      , elems = [_|_]
                      }, X509DT),
    ok.

schema_with_string_enum(_Config) ->
    Dir = filename:join(code:priv_dir(ews), "../test"),
    File = filename:join(Dir, "with_enum.xsd"),
    {ok, XsdBin} = file:read_file(File),
    {Schema, _} = xmerl_scan:string(binary_to_list(XsdBin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),

    {Model, with_enum} = ews_xsd:parse_schema(Schema, {undefined, with_enum, Dir}),

    ?assertMatch(#model{}, Model),
    #model{type_map = TypeMap} = Model,

    FridayStatus = ews_model:get_elem({"http://example.com/with_enum","FridayStatus"}, TypeMap),
    ?assertMatch(#elem{}, FridayStatus),
    ok.
