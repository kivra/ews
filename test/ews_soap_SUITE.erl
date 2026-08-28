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
         body_returned_directly/1,
         fault_body_returned_directly/1,
         error_body_returned_directly/1,
         asks_hackney_for_the_body/1,
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
       body_returned_directly,
       fault_body_returned_directly,
       error_body_returned_directly,
       asks_hackney_for_the_body,
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

%%% The three cases above answer the way hackney 1.x does: a client reference,
%%% and the body fetched with hackney:body/1. These three answer the way 4.x
%%% does -- the body itself, in place of the reference -- and none of them may
%%% call hackney:body/1, which is what the expectation below enforces.
%%%
%%% The third is the one that used to crash. The non-200 clause fetched the body
%%% unconditionally, so hackney:body(<<>>) raised function_clause and the caller
%%% got an exception where it expected {error, _}.

body_returned_directly(_Config) ->
    Return = <<"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
                 "<s:Header>"
                   "<HdrRes />"
                 "</s:Header>"
                 "<s:Body>"
                   "<Res />"
                 "</s:Body>"
               "</s:Envelope>">>,
    meck:expect(hackney, request, 5, {ok, 200, [], Return}),
    reject_body_calls(),

    Header = [{"HdrRes", [], []}],
    Body = [{"Res", [], []}],
    ?assertEqual({ok, {Header, Body}},
                 ews_soap:call(endpoint, "moose", soap_action, Header, Body,
                               #{})).

fault_body_returned_directly(_Config) ->
    Return = <<"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
                 "<s:Header>"
                   "<HdrRes />"
                 "</s:Header>"
                 "<s:Body>"
                   "<s:Fault />"
                 "</s:Body>"
               "</s:Envelope>">>,
    meck:expect(hackney, request, 5, {ok, 500, [], Return}),
    reject_body_calls(),

    Header = [{"HdrRes", [], []}],
    ?assertEqual({fault, {Header, #fault{}}},
                 ews_soap:call(endpoint, "moose", soap_action, Header,
                               [{"", [], []}], #{})).

%% An error whose body is not a SOAP envelope at all -- a proxy's 404, or the
%% empty body a mock returns for an unmatched request. The caller gets an error
%% it can act on rather than an exception.
error_body_returned_directly(_Config) ->
    meck:expect(hackney, request, 5, {ok, 404, [], <<>>}),
    reject_body_calls(),

    ?assertMatch({error, {not_envelope, _}},
                 ews_soap:call(endpoint, "moose", soap_action, [],
                               [{"Res", [], []}], #{})).

%% The option is what makes hackney 1.x answer the way 4.x does, so removing it
%% would quietly reintroduce two response shapes -- and the cases above would
%% still pass, since they mock the answer rather than hackney's own behaviour.
asks_hackney_for_the_body(_Config) ->
    Return = <<"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
                 "<s:Body><Res /></s:Body>"
               "</s:Envelope>">>,
    meck:expect(hackney, request, 5, {ok, 200, [], Return}),
    reject_body_calls(),

    {ok, _} = ews_soap:call(endpoint, "moose", soap_action, [],
                            [{"Res", [], []}], #{}),

    [{_Pid, {hackney, request, [_, _, _, _, Options]}, _}] =
        meck:history(hackney),
    ?assert(proplists:get_bool(with_body, Options)),

    %% And it is not added twice when a caller passes it.
    meck:reset(hackney),
    meck:expect(hackney, request, 5, {ok, 200, [], Return}),
    {ok, _} = ews_soap:call(endpoint, "moose", soap_action, [],
                            [{"Res", [], []}],
                            #{http_options => [with_body]}),
    [{_, {hackney, request, [_, _, _, _, Options2]}, _}] =
        meck:history(hackney),
    ?assertEqual(1, length([O || O <- Options2, O =:= with_body])).

%% hackney:body/1 does not exist for a body that is already a body. Fail loudly
%% rather than let a mock quietly cover for calling it.
reject_body_calls() ->
    meck:expect(hackney, body,
                fun(Arg) -> ct:fail({hackney_body_called_with, Arg}) end).

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
