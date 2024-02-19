-module(ews_soap_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("ews/src/ews.hrl").
-include_lib("ews/include/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

%% Tests
-export([hackney_error/1,
         no_header_response/1,
         full_response/1,
         fault_response/1,
         not_an_envelope/1,
         efficient_prefixes/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

all() ->
    [{group, soap_test}].

groups() ->
    [{soap_test, [shuffle],
      [hackney_error,
       no_header_response,
       full_response,
       fault_response,
       not_an_envelope,
       efficient_prefixes
      ]}].

init_per_group(soap_test, Config) ->
    application:load(ews),
    Config.

end_per_group(soap_test, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    meck:new(hackney),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(hackney).

hackney_error(_Config) ->
    meck:expect(hackney, request, 5, {error, test_error}),

    Endpoint = endpoint,
    OpName = "moose",
    SoapAction = soap_action,
    Header = {"Hdr", [], []},
    Body = {"Bdy", [], []},
    Opts = #{},
    {error, test_error} =
        ews_soap:call(Endpoint, OpName, SoapAction, Header, Body, Opts).

no_header_response(_Config) ->
    Return = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
               "<s:Body>"
                 "<Res />"
               "</s:Body>"
             "</s:Envelope>",
    meck:expect(hackney, request, 5, {ok, 200, [], noref}),
    meck:expect(hackney, body, 1, {ok, Return}),

    Endpoint = endpoint,
    OpName = "moose",
    SoapAction = soap_action,
    Header = [],
    Body = [{"Res", [], []}],
    Opts = #{},
    {ok, {Header, Body}} =
        ews_soap:call(Endpoint, OpName, SoapAction, Header, Body, Opts).

full_response(_Config) ->
    Return = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
               "<s:Header>"
                 "<HdrRes />"
               "</s:Header>"
               "<s:Body>"
                 "<Res />"
               "</s:Body>"
             "</s:Envelope>",
    meck:expect(hackney, request, 5, {ok, 200, [], noref}),
    meck:expect(hackney, body, 1, {ok, Return}),

    Endpoint = endpoint,
    OpName = "moose",
    SoapAction = soap_action,
    Header = [{"HdrRes", [], []}],
    Body = [{"Res", [], []}],
    Opts = #{},
    {ok, {Header, Body}} =
        ews_soap:call(Endpoint, OpName, SoapAction, Header, Body, Opts).

fault_response(_Config) ->
    Return = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
               "<s:Header>"
                 "<HdrRes />"
               "</s:Header>"
               "<s:Body>"
               "  <s:Fault />"
               "</s:Body>"
             "</s:Envelope>",
    meck:expect(hackney, request, 5, {ok, 201, [], noref}),
    meck:expect(hackney, body, 1, {ok, Return}),

    Endpoint = endpoint,
    OpName = "moose",
    SoapAction = soap_action,
    Header = [{"HdrRes", [], []}],
    Body = #fault{},
    Opts = #{},
    {fault, {Header, Body}} = ews_soap:call(Endpoint, OpName, SoapAction, Header,
                                             [{"", [], []}], Opts).

not_an_envelope(_Config) ->
    %% TODO We should not get a {fault | ok, _} tuple back if the response is
    %% not a correct SOAP envelope
    Return = "<Res />",
    meck:expect(hackney, request, 5, {ok, 201, [], noref}),
    meck:expect(hackney, body, 1, {ok, Return}),

    Endpoint = endpoint,
    OpName = "moose",
    SoapAction = soap_action,
    Header = [{"HdrRes", [], []}],
    Body = [{"Res", [], []}],
    Opts = #{},
    ?assertMatch({error, {not_envelope, _}},
                 ews_soap:call(Endpoint, OpName, SoapAction, Header, Body,
                               Opts)).

efficient_prefixes(_Config) ->
    Header = [],
    Body = [{{"http://minameddelanden.gov.se/schema/Recipient/v2",
              "storeAccountPreferences"},
             [],
             [{{"http://minameddelanden.gov.se/schema/Recipient/v2",
                "preferences"},
               [],
               [{{"http://minameddelanden.gov.se/schema/Recipient",
                  "AcceptedSenders"},
                 [],
                 [{{"http://minameddelanden.gov.se/schema/Sender","Id"},
                   [],
                   [{txt,<<"168">>}]}]},
                {{"http://minameddelanden.gov.se/schema/Recipient",
                  "AcceptedSenders"},
                 [],
                 [{{"http://minameddelanden.gov.se/schema/Sender","Id"},
                   [],
                   [{txt,<<"169">>}]}]}]},
              {{"http://minameddelanden.gov.se/schema/Recipient/v2",
                "AgreementText"},
               [],
               [{txt,<<"yo&\r\n<>öö/utf-8">>}]},
              {{"http://minameddelanden.gov.se/schema/Recipient/v2",
                "SignatureData"},
               [],
               [{{"http://minameddelanden.gov.se/schema/Common",
                  "Signature"},
                 [],
                 [{txt,<<"boo">>}]}]}]}],
    AllNss = find_nss(Body, []),
    InNss = lists:usort([?SOAPNS | AllNss]),
    XML = unicode:characters_to_list(ews_soap:make_soap(Header, Body), utf8),
    Tokens = string:tokens(XML, " <>"),
    XMLNss0 = [ begin [_, Ns1] = string:tokens(Ns0, "\""), Ns1 end ||
                  "xmlns:"++Ns0 <- Tokens ],
    ?assertEqual(length(InNss), length(XMLNss0)),
    XMLNss1 = lists:usort(XMLNss0),
    ?assertEqual(InNss, XMLNss1).

find_nss([{{Ns, _},Attrs,Children} | T], Acc) ->
    find_nss(T, find_nss(Attrs, [])++find_nss(Children, [])++[Ns | Acc]);
find_nss([{txt, _} | T], Acc) ->
    find_nss(T, Acc);
find_nss([], Acc) ->
    Acc.
