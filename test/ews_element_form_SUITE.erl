%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2026 Kivra
%%%
%%% Distribution subject to the terms of the LGPL-3.0-or-later, see
%%% the COPYING.LESSER file in the root of the distribution
%%%
%%% Tests for XSD `elementFormDefault` / per-element `form` handling, using
%%% the two-schema fixture in element_form.wsdl.
%%%
%%% XSD rules under test:
%%%   * A *global* element declaration (a direct child of <schema>) is always
%%%     qualified by the schema's targetNamespace.
%%%   * A *local* element declaration is qualified only if the effective form
%%%     is "qualified" -- that is the enclosing schema's elementFormDefault
%%%     (which itself defaults to "unqualified"), overridable per declaration
%%%     with form="qualified" / form="unqualified".
%%%
%%% ews models an unqualified tag as a plain string qname and a qualified one
%%% as a {Namespace, Name} tuple (see the qname() type in ews_xml), so most
%%% assertions below are made on the decoded qnames of a serialized envelope
%%% rather than on generated namespace prefixes. The serialize_xml_* cases
%%% additionally pin the whole envelope, bytes and prefixes included.
%%%
%%% Half of these cases cover the form rules themselves; the rest are guards,
%%% against qualifying too little (serialize_form_qualified_override,
%%% serialize_qualified_schema) or against breaking the decode direction
%%% (decode_unqualified_local, decode_qualified_local, roundtrip_request).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ews_element_form_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../src/ews.hrl").
-include_lib("ews/include/ews.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

-export([ model_unqualified_locals_are_bare/1
        , model_qualified_locals_keep_ns/1
        , serialize_unqualified_locals/1
        , serialize_form_qualified_override/1
        , serialize_qualified_schema/1
        , serialize_form_unqualified_override/1
        , serialize_xml_unqualified_schema/1
        , serialize_xml_qualified_schema/1
        , decode_unqualified_local/1
        , decode_qualified_local/1
        , roundtrip_request/1
        ]).

-define(MODEL, element_form).
-define(SVC, "FormService").
-define(UNQ_NS, "http://example.com/form/unqualified").
-define(Q_NS, "http://example.com/form/qualified").

suite() -> [{timetrap, {seconds, 30}}].

all() ->
    [ model_unqualified_locals_are_bare
    , model_qualified_locals_keep_ns
    , serialize_unqualified_locals
    , serialize_form_qualified_override
    , serialize_qualified_schema
    , serialize_form_unqualified_override
    , serialize_xml_unqualified_schema
    , serialize_xml_qualified_schema
    , decode_unqualified_local
    , decode_qualified_local
    , roundtrip_request
    ].

init_per_suite(Config) ->
    ews:start(),
    application:load(ews),
    Dir = filename:join(code:priv_dir(ews), "../test"),
    {ok, Bin} = file:read_file(filename:join(Dir, "element_form.wsdl")),
    {ok, [{?SVC, 2}]} = ews_svc:add_wsdl_bin(?MODEL, Bin),
    Config.

end_per_suite(_Config) ->
    ews_svc:remove_model(?MODEL),
    ok.

%%% Model ----------------------------------------------------------------

%% testOp is declared in a schema with no elementFormDefault, i.e.
%% "unqualified": the wrapper element itself is global and stays qualified,
%% but arg0/arg1 are locals and must carry no namespace.
model_unqualified_locals_are_bare(_Config) ->
    #type{elems = Elems} = ews_svc:get_type(?MODEL, {?UNQ_NS, "testOp"}),
    ?assertEqual(["arg0", "arg1", {?UNQ_NS, "arg2"}], qnames(Elems)),
    %% Same story for the response wrapper's single local element.
    #type{elems = RespElems} =
        ews_svc:get_type(?MODEL, {?UNQ_NS, "testOpResponse"}),
    ?assertEqual(["return", "item"], qnames(RespElems)),
    ok.

%% The imported schema sets elementFormDefault="qualified", so its locals
%% keep the namespace -- except field1, which overrides with
%% form="unqualified".
model_qualified_locals_keep_ns(_Config) ->
    #type{elems = PersonElems} = ews_svc:get_type(?MODEL, {?Q_NS, "Person"}),
    ?assertEqual([{?Q_NS, "Id"}, {?Q_NS, "Name"}], qnames(PersonElems)),
    #type{elems = OpElems} = ews_svc:get_type(?MODEL, {?Q_NS, "qualifiedOp"}),
    ?assertEqual([{?Q_NS, "field0"}, "field1"], qnames(OpElems)),
    ok.

%%% Serialization --------------------------------------------------------

%% The mixed case: the wrapper is qualified, its locals are not, and the
%% children of arg1 -- declared in the qualified schema -- stay qualified.
serialize_unqualified_locals(_Config) ->
    {Qname, _, Children} = serialize_test_op(),
    ?assertEqual({?UNQ_NS, "testOp"}, Qname),
    ?assertEqual(["arg0", "arg1", {?UNQ_NS, "arg2"}], qnames(Children)),
    {"arg1", _, PersonChildren} = lists:keyfind("arg1", 1, Children),
    ?assertEqual([{?Q_NS, "Id"}, {?Q_NS, "Name"}], qnames(PersonChildren)),
    ok.

%% arg2 is declared form="qualified" in an unqualified schema.
serialize_form_qualified_override(_Config) ->
    {_, _, Children} = serialize_test_op(),
    ?assertMatch({_, _, [{txt, <<"Ownership">>}]},
                 lists:keyfind({?UNQ_NS, "arg2"}, 1, Children)),
    ok.

%% Regression guard: a schema that already says elementFormDefault=
%% "qualified" must keep serializing fully qualified.
serialize_qualified_schema(_Config) ->
    {Qname, _, Children} = serialize_qualified_op(),
    ?assertEqual({?Q_NS, "qualifiedOp"}, Qname),
    ?assertMatch({_, _, [{txt, <<"a">>}]},
                 lists:keyfind({?Q_NS, "field0"}, 1, Children)),
    ok.

%% field1 is declared form="unqualified" in a qualified schema.
serialize_form_unqualified_override(_Config) ->
    {_, _, Children} = serialize_qualified_op(),
    ?assertMatch({_, _, [{txt, <<"b">>}]}, lists:keyfind("field1", 1, Children)),
    ok.

%% The two cases above assert on decoded qnames, which is what the form rules
%% are about. These two pin the literal bytes instead, so the diff on a
%% regression shows the envelope that would go on the wire. The generated
%% prefixes (p1, p2, ...) are ews's own allocation rather than part of the
%% contract -- what the schema decides is which tags carry a namespace at all
%% -- so a change in how they are handed out is expected to fail here and be
%% re-read, not silently accepted.
serialize_xml_unqualified_schema(_Config) ->
    ?assertEqual(<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                   "<p3:Envelope xmlns:p1=\"" ?Q_NS "\""
                   " xmlns:p2=\"" ?UNQ_NS "\""
                   " xmlns:p3=\"" ?SOAPNS "\">"
                   "<p3:Body>"
                   "<p2:testOp>"
                   "<arg0>165560000000</arg0>"
                   "<arg1>"
                   "<p1:Id>191212121212</p1:Id>"
                   "<p1:Name>Tolvan Tolvansson</p1:Name>"
                   "</arg1>"
                   "<p2:arg2>Ownership</p2:arg2>"
                   "</p2:testOp>"
                   "</p3:Body></p3:Envelope>">>,
                 iolist_to_binary(
                   ews_svc:serialize(?MODEL, ?SVC, "TestOp", [],
                                     [test_op_record()]))),
    ok.

serialize_xml_qualified_schema(_Config) ->
    ?assertEqual(<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                   "<p2:Envelope xmlns:p1=\"" ?Q_NS "\""
                   " xmlns:p2=\"" ?SOAPNS "\">"
                   "<p2:Body>"
                   "<p1:qualifiedOp>"
                   "<p1:field0>a</p1:field0>"
                   "<field1>b</field1>"
                   "</p1:qualifiedOp>"
                   "</p2:Body></p2:Envelope>">>,
                 iolist_to_binary(
                   ews_svc:serialize(?MODEL, ?SVC, "QualifiedOp", [],
                                     [{qualified_op, <<"a">>, <<"b">>}]))),
    ok.

%%% Decoding -------------------------------------------------------------

%% The straightforward direction: the model holds bare qnames for these
%% locals and the server sends them bare.
decode_unqualified_local(_Config) ->
    Response =
        envelope(<<"<ns:testOpResponse xmlns:ns=\"" ?UNQ_NS "\">"
                   "<return>hello</return>"
                   "<item>a</item><item>b</item>"
                   "</ns:testOpResponse>">>),
    ?assertEqual({ok, [{test_op_response, <<"hello">>, [<<"a">>, <<"b">>]}]},
                 decode_out(Response)),
    ok.

%% The mirror image: a server that qualifies these locals even though its own
%% schema says not to is still decoded, the same way ews has always accepted a
%% bare tag for an element the model has qualified. `item' is maxOccurs=
%% unbounded, so a single qualified occurrence must still come back as a list
%% -- the cardinality is a property of the model, not of how the tag was
%% written.
decode_qualified_local(_Config) ->
    Response =
        envelope(<<"<ns:testOpResponse xmlns:ns=\"" ?UNQ_NS "\">"
                   "<ns:return>hello</ns:return>"
                   "<ns:item>a</ns:item>"
                   "</ns:testOpResponse>">>),
    ?assertEqual({ok, [{test_op_response, <<"hello">>, [<<"a">>]}]},
                 decode_out(Response)),
    ok.

%% Symmetry guard: whatever the serializer writes, the decoder reads back.
%% A change to one that forgets the other breaks here.
roundtrip_request(_Config) ->
    Request = ews_svc:serialize(?MODEL, ?SVC, "TestOp", [], [test_op_record()]),
    ?assertEqual({ok, {?SVC, "TestOp", [], [test_op_record()]}},
                 ews_svc:decode_in(?MODEL, Request)),
    ok.

%%% Helpers --------------------------------------------------------------

test_op_record() ->
    {test_op, <<"165560000000">>,
     {person, <<"191212121212">>, <<"Tolvan Tolvansson">>},
     <<"Ownership">>}.

serialize_test_op() ->
    body_element(ews_svc:serialize(?MODEL, ?SVC, "TestOp", [],
                                   [test_op_record()])).

serialize_qualified_op() ->
    body_element(ews_svc:serialize(?MODEL, ?SVC, "QualifiedOp", [],
                                   [{qualified_op, <<"a">>, <<"b">>}])).

%% The single element inside <Body>, as a decoded xml term. ews always emits
%% explicit prefixes and never a default xmlns, so a tag that comes back as a
%% plain string was written to the wire unqualified.
body_element(Envelope) ->
    {ok, {_Headers, [Element]}} =
        ews_soap:parse_envelope(ews_xml:decode(Envelope)),
    Element.

decode_out(Envelope) ->
    {ok, {Headers, Body}} = ews_soap:parse_envelope(ews_xml:decode(Envelope)),
    ews_svc:decode(?MODEL, ?SVC, "TestOp", Headers, Body,
                   #{include_headers => false}).

envelope(Body) ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<senv:Envelope xmlns:senv=\"" ?SOAPNS "\"><senv:Body>",
      Body/binary,
      "</senv:Body></senv:Envelope>">>.

qnames(Elems) ->
    [ qname_of(E) || E <- Elems ].

qname_of(#elem{qname = Qname}) -> Qname;
qname_of({Qname, _Attrs, _Children}) -> Qname.
