-module(ews_soap_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ews/src/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

%% Tests
-export([lhttpc_error/1,
         no_header_response/1,
         full_response/1,
         fault_response/1,
         not_an_envelope/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

all() ->
    [{group, soap_test}].

groups() ->
    [{soap_test, [shuffle],
      [lhttpc_error,
       no_header_response,
       full_response,
       fault_response,
       not_an_envelope
      ]}].

init_per_group(soap_test, Config) ->
    application:load(ews),
    Config.

end_per_group(soap_test, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    meck:new(lhttpc),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(lhttpc).

lhttpc_error(_Config) ->
    meck:expect(lhttpc, request, 5, {error, test_error}),

    Endpoint = endpoint,
    SoapAction = soap_action,
    Header = {"Hdr", [], []},
    Body = {"Bdy", [], []},
    Opts = #{},
    {error, test_error} =
        ews_soap:call(Endpoint, SoapAction, Header, Body, Opts).

no_header_response(_Config) ->
    Return = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
               "<s:Body>"
                 "<Res />"
               "</s:Body>"
             "</s:Envelope>",
    meck:expect(lhttpc, request, 5, {ok, {{200, ok}, [], Return}}),

    Endpoint = endpoint,
    SoapAction = soap_action,
    Header = [],
    Body = [{"Res", [], []}],
    Opts = #{},
    {ok, {Header, Body}} =
        ews_soap:call(Endpoint, SoapAction, Header, Body, Opts).

full_response(_Config) ->
    Return = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
               "<s:Header>"
                 "<HdrRes />"
               "</s:Header>"
               "<s:Body>"
                 "<Res />"
               "</s:Body>"
             "</s:Envelope>",
    meck:expect(lhttpc, request, 5, {ok, {{200, ok}, [], Return}}),

    Endpoint = endpoint,
    SoapAction = soap_action,
    Header = [{"HdrRes", [], []}],
    Body = [{"Res", [], []}],
    Opts = #{},
    {ok, {Header, Body}} =
        ews_soap:call(Endpoint, SoapAction, Header, Body, Opts).

fault_response(_Config) ->
    Return = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
               "<s:Header>"
                 "<HdrRes />"
               "</s:Header>"
               "<s:Body>"
               "  <s:Fault />"
               "</s:Body>"
             "</s:Envelope>",
    meck:expect(lhttpc, request, 5, {ok, {{201, ok}, [], Return}}),

    Endpoint = endpoint,
    SoapAction = soap_action,
    Header = [{"HdrRes", [], []}],
    Body = #fault{},
    Opts = #{},
    {fault, {Header, Body}} = ews_soap:call(Endpoint, SoapAction, Header,
                                             [{"", [], []}], Opts).

not_an_envelope(_Config) ->
    %% TODO We should not get a {fault | ok, _} tuple back if the response is
    %% not a correct SOAP envelope
    Return = "<Res />",
    meck:expect(lhttpc, request, 5, {ok, {{201, ok}, [], Return}}),

    Endpoint = endpoint,
    SoapAction = soap_action,
    Header = [{"HdrRes", [], []}],
    Body = [{"Res", [], []}],
    Opts = #{},
    {error, not_envelope} =
        ews_soap:call(Endpoint, SoapAction, Header, Body, Opts).
