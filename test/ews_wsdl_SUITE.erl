-module(ews_wsdl_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%TODO header should be inside of include, but we need to rename the git repo
%%to ews first
-include("../src/ews.hrl").
-include_lib("ews/include/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_group/2, end_per_group/2
        ]).

%% Tests
-export([ google_v201306_ensure_record/1
        , google_v201306_correct_service/1
        , colliding_types/1
        , serialize_deserialize/1
        , dont_emit_simplecontent/1
        , many_schemas_n_refs/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [ {google_v201306_campaignService, [shuffle, parallel],
       [google_v201306_ensure_record,
        google_v201306_correct_service
       ]}
    , {mm_service,
       [ dont_emit_simplecontent
       ]}
    , {mm_notification,
       [ colliding_types
       , serialize_deserialize
       ]}
    , {teleadr,
       [ many_schemas_n_refs
       ]}
    ].

all() ->
    [ {group, google_v201306_campaignService}
    , {group, mm_service}
    , {group, mm_notification}
    , {group, teleadr}
    ].

init_per_group(google_v201306_campaignService, Config) ->
    ews:start(),

    %% Read file
    application:load(ews),
    Dir = filename:join(code:priv_dir(ews), "../test/data"),
    File = filename:join(Dir, "google_v201306_CampaignService.wsdl"),
    {ok, Bin} = file:read_file(File),

    %% Mock request
    meck:new(hackney),
    meck:expect(hackney, request, 5, {ok, 200, ignore, Bin}),

    %% Get Wsdl, the actual URL is not important as we mock the hackney call
    %% with an already downloaded version
    Url = "https://adwords.google.com/api/adwords/cm/"
          "v201306/CampaignService?wsdl",
    Wsdl = ews_wsdl:fetch_and_parse(Url, test),

    meck:unload(hackney),
    ews:stop(),
    [{google_v201306_campaignService, Wsdl} | Config];
init_per_group(mm_notification, Config) ->
    application:ensure_all_started(ews),
    Config;
init_per_group(mm_service, Config) ->
    application:ensure_all_started(ews),
    Config;
init_per_group(teleadr, Config) ->
    application:ensure_all_started(ews),
    Config.

end_per_group(google_v201306_campaignService, _Config) ->
    ok;
end_per_group(mm_notification, _Config) ->
    ews:remove_model(mm_notification);
end_per_group(mm_service, _Config) ->
    ews:remove_model(ek_mm_test);
end_per_group(teleadr, _Config) ->
    ews:remove_model(tiny).

google_v201306_ensure_record(Config) ->
    Wsdl = proplists:get_value(google_v201306_campaignService, Config),
    true = is_record(Wsdl, wsdl).

google_v201306_correct_service(Config) ->
    Wsdl = proplists:get_value(google_v201306_campaignService, Config),
    [ResService] = Wsdl#wsdl.services,
    "CampaignService" = ResService#service.name.

dont_emit_simplecontent(_Config) ->
    Dir = filename:join(code:priv_dir(ews), "../test"),
    File = filename:join(Dir, "mm_service.wsdl"),
    %% Mock request
    meck:new(hackney),
    meck:expect(hackney, request, 5, {ok, 200, ignore, <<>>}),
    ews:add_wsdl_to_model(ek_mm_test, File),
    #model{type_map=Tbl, simple_types=Ts} =
        ews_svc:get_model(ek_mm_test),
    Ret = ews_emit:sort_types(Tbl, Ts),
    %% Nothing should be unresolved
    ?assertMatch({[], [_|_]}, Ret),
    %% This simpleContent should not be in Tbl
    ?assertEqual(false,
                 ews_model:get({"http://www.w3.org/2000/09/xmldsig#",
                                "SignatureValueType"}
                              , Tbl)),
    ok = ews_test:test_everything(ek_mm_test),
    ok.

colliding_types(_Config) ->
    %% Mock request
    meck:new(hackney),
    meck:expect(hackney, request, 5, {ok, 200, ignore, <<>>}),

    {ok, _} = ews:add_wsdl_to_model(mm_notification,
                                    test_wsdl_file("mm_notification.wsdl")),

    #model{type_map=Tbl} = ews_svc:get_model(mm_notification),

    %% EmailMessage and SmsMessage should both have been parsed
    EmailMessage = ews_model:get({"http://example.com/importee",
                                  "EmailMessage"}
                                , Tbl),
    SmsMessage = ews_model:get({"http://example.com/importee",
                                "SmsMessage"}
                              , Tbl),
    ?assertMatch(#type{elems = [#elem{qname={_, "header"}}|_]}, EmailMessage),
    ?assertMatch(#type{elems = [#elem{qname={_, "header"}}|_]}, SmsMessage),
    [#elem{type = EmailHeaderType}|_] = EmailMessage#type.elems,
    [#elem{type = SmsHeaderType}|_] = SmsMessage#type.elems,
    ?assertEqual({ "http://example.com/importee"
                 , "EmailMessage@header"
                 }, EmailHeaderType),
    ?assertEqual({ "http://example.com/importee"
                 , "SmsMessage@header"
                 }, SmsHeaderType),
    %% the header elements inside SmsMessage and EmailMessage
    %% should both resolve
    ?assertMatch(#type{alias=header_1, elems=[_, _]},
                 ews_model:get(EmailHeaderType, Tbl)),
    ?assertMatch(#type{alias=header, elems=[_]},
                 ews_model:get(SmsHeaderType, Tbl)),
    ok.

serialize_deserialize(_Config) ->
    %% Mock request
    meck:new(hackney),
    meck:expect(hackney, request, 5, {ok, 200, ignore, <<>>}),

    {ok, _} = ews:add_wsdl_to_model(mm_notification,
                                    test_wsdl_file("mm_notification.wsdl")),

    %% "client" serializes request
    EmailMessage =
        [{email_message,
          {header_1,
           <<"moose@sausage.com">>, <<"Hej">>},
          <<"apa">>}],
    EmailSOAP = ews:serialize_service_op( mm_notification
                                        , "notification"
                                        , "pokeball"
                                        , []
                                        , EmailMessage
                                        ),

    %% "server" deserializes request
    {ok, {Svc, OpName, [], OpIn}} = ews:decode_in(mm_notification, EmailSOAP),
    ?assertMatch("notification", Svc),
    ?assertMatch("pokeball", OpName),
    ?assertMatch(EmailMessage, OpIn),

    %% "server" serializes response
    SmsMessage =
        [{sms_message,
          {header,
           <<"0015551212">>},
          <<"bepa">>}],
    SmsSOAP = iolist_to_binary(
                ews:encode_service_op_result( mm_notification
                                            , "notification"
                                            , "pokeball"
                                            , []
                                            , SmsMessage
                                            )),

    %% "client" deserializes response
    XmlTerm = ews_xml:decode(SmsSOAP),
    {ok, {[], Resp}} = ews_soap:parse_envelope(XmlTerm),
    {ok, [OpOut]} = ews:decode_service_op_result("notification", "pokeball",
                                                 [Resp]),

    ?assertMatch(SmsMessage, OpOut),
    ok.

many_schemas_n_refs(_Config) ->
    {ok, _} = ews:add_wsdl_to_model(tiny,
                                    test_wsdl_file("tiny.wsdl")),
    TmpFile = tempfile(),
    ok = ews:emit_complete_model_types(tiny, TmpFile),
    Find = [{find,
             {find_love_class,
              #{'UserId' => <<"TEST">>,
                'Password' => <<"TEST">>,
                'AccountID' => <<>>,
                'TargetType' => 1},
              {query_params_class,
               #{'Gender' => <<"Foden">>,
                 'Internet' => 1},
               undefined,
               <<"197001011234">>},
              {query_columns_class,
               #{'Vkiid' => 1,
                 'Vkid' => 1}}}}],
    ct:pal("Find: ~p", [Find]),
    Header = [{api_soap_header, <<"apa">>, undefined}],
    FindSOAP = iolist_to_binary(
                 ews:serialize_service_op( tiny
                                         , "NNAPIWebService"
                                         , "Find"
                                         , Header
                                         , Find
                                         )),
    {ok, {Svc, OpName, OpHdr, OpIn}} = ews:decode_in(tiny, FindSOAP),
    ?assertMatch("NNAPIWebService", Svc),
    ?assertMatch("Find", OpName),
    ?assertMatch(Header, OpHdr),
    ?assertMatch(Find, OpIn),
    Response = [{find_response,
                 {api_result,
                  #{error_text => <<>>,
                    count_private => 1,
                    count_company => 0,
                    rowcount_private => 1,
                    rowcount_company => 0,
                    rowcount => 1        ,
                    response_time => 17       ,
                    error_code => 0},
                  [{record_list,
                    #{start_pos => 0,
                      num_records => 1,
                      target_type => 1},
                    []}]}}],
    ResSOAP = iolist_to_binary(
                ews:encode_service_op_result( tiny
                                            , "NNAPIWebService"
                                            , "Find"
                                            , Header
                                            , Response
                                            )),
    XmlTerm = ews_xml:decode(ResSOAP),
    {ok, {HResp, Resp}} = ews_soap:parse_envelope(XmlTerm),
    {ok, HOut, OpOut} = ews:decode_service_op_result( "NNAPIWebService"
                                                         , "Find"
                                                         , HResp
                                                         , Resp
                                                         ),
    ?assertMatch(Header, HOut),
    ?assertMatch(Response, OpOut),

    ok = ews_test:test_everything(tiny),
    file:delete(TmpFile),
    ok.

tempfile() ->
    filename:join("/tmp",
                  io_lib:format("~p.hrl",
                                [erlang:phash2(make_ref())])).

test_wsdl_file(Basename) ->
    Dir = filename:join(code:priv_dir(ews), "../test"),
    File = filename:join(Dir, Basename),
    File.
