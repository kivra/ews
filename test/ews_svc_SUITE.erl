-module(ews_svc_SUITE).
-include_lib("common_test/include/ct.hrl").

%%TODO header should be inside of include, but we need to rename the git repo
%%to ews first
-include("../src/ews.hrl").
-include_lib("ews/include/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_suite/1, end_per_suite/1
        ]).

%% Tests
-export([empty_lists_services/1,
         empty_list_types/1,
         empty_get_model/1,
         empty_list_full_clashes/1,
         empty_list_simple_clashes/1,
         empty_emit_model/1,
         add_model/1,
         one_model_list_services/1,
         one_model_list_service_ops/1,
         one_model_get_op_info/1,
         one_model_mutate_op_info/1,
         one_model_query_op_info/1,
         one_model_get_op/1,
         one_model_mutate_op/1,
         one_model_query_op/1,
         one_model_list_types/1,
         one_model_emit_model/1,
         one_model_get_type/1,
         one_model_call/1,
         one_model_pre_hook/1,
         one_model_post_hook/1,
         one_model_remove_hook/1,
         add_two_models/1,
         two_models_explicit_model/1,
         two_models_unambiguous/1,
         two_models_ambiguous/1,
         remove_model/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{no_models, [parallel, shuffle],
      [empty_lists_services,
       empty_list_types,
       empty_get_model,
       empty_list_full_clashes,
       empty_list_simple_clashes,
       empty_emit_model
      ]},
     {one_model, [parallel, shuffle],
      [one_model_list_services,
       one_model_list_service_ops,
       one_model_get_op_info,
       one_model_mutate_op_info,
       one_model_query_op_info,
       one_model_get_op,
       one_model_mutate_op,
       one_model_query_op,
       one_model_list_types,
       one_model_emit_model,
       one_model_get_type
      ]},
     {one_model_hooks, [sequence],
      [one_model_pre_hook,
       one_model_post_hook,
       one_model_remove_hook
      ]},
     {two_models, [parallel, shuffle],
      [two_models_explicit_model,
       two_models_unambiguous,
       two_models_ambiguous
      ]}
    ].

all() ->
    [{group, no_models},
     add_model,
     {group, one_model},
     one_model_call,
     {group, one_model_hooks},
     add_two_models,
     {group, two_models},
     remove_model
    ].

init_per_suite(Config) ->
    ews:start(),
    Config.

end_per_suite(_Config) ->
    ok.

% No model added tests ----------------------------------------------
empty_lists_services(_Config) ->
    {ok, []} = ews_svc:list_services(default).

empty_list_types(_Config) ->
    [] = ews_svc:list_types(default).

empty_get_model(_Config) ->
    undefined = ews_svc:get_model(default).

empty_list_full_clashes(_Config) ->
    [] = ews_svc:list_full_clashes(default).

empty_list_simple_clashes(_Config) ->
    [] = ews_svc:list_simple_clashes(default).

empty_emit_model(_Config) ->
    {error, no_model} = ews_svc:emit_model(default, "test_file").

% One model added testing -------------------------------------------
add_model(_Config) ->
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
    {ok,[{"CampaignService",3}]} = ews_svc:add_wsdl_url(default, Url),

    true = meck:validate(hackney),
    meck:unload(hackney).

one_model_list_services(_Config) ->
    {ok, ["CampaignService"]} = ews_svc:list_services(default).

one_model_list_service_ops(_Config) ->
    Ops = ["get","mutate","query"],
    {ok, Ops} = ews_svc:list_service_ops(default, "CampaignService").

one_model_get_op_info(_Config) ->
    Service = "CampaignService",
    Ns = "https://adwords.google.com/api/adwords/cm/v201306",
    EndPoint = lists:concat([Ns, "/", Service]),

    {ok, GetInfo} = ews_svc:get_op_info(default, Service, "get"),
    "get" = proplists:get_value(name, GetInfo),
    [{"get", {Ns, "get"}}] = proplists:get_value(in, GetInfo),
    [{"RequestHeader", {Ns, "SoapHeader"}}] = proplists:get_value(in_hdr,
                                                                  GetInfo),
    [{"getResponse", {Ns, "getResponse"}}] = proplists:get_value(out, GetInfo),
    [{"ResponseHeader", {Ns, "SoapResponseHeader"}}] = proplists:get_value(
                                                         out_hdr, GetInfo),
    [{"ApiExceptionFault", {Ns, "ApiException"}}] = proplists:get_value(
                                                      fault, GetInfo),
    EndPoint = proplists:get_value(endpoint, GetInfo),
    [] = proplists:get_value(action, GetInfo),
    GetDocBin = proplists:get_value(doc, GetInfo),
    true = byte_size(GetDocBin) > 0.

one_model_mutate_op_info(_Config) ->
    Service = "CampaignService",
    Ns = "https://adwords.google.com/api/adwords/cm/v201306",
    EndPoint = lists:concat([Ns, "/", Service]),

    {ok, MutateInfo} = ews_svc:get_op_info(default, Service, "mutate"),
    "mutate" = proplists:get_value(name, MutateInfo),
    [{"mutate", {Ns, "mutate"}}] = proplists:get_value(in, MutateInfo),
    [{"RequestHeader", {Ns, "SoapHeader"}}] = proplists:get_value(in_hdr,
                                                                  MutateInfo),
    [{"mutateResponse", {Ns, "mutateResponse"}}] = proplists:get_value(
                                                     out, MutateInfo),
    [{"ResponseHeader", {Ns, "SoapResponseHeader"}}] = proplists:get_value(
                                                         out_hdr, MutateInfo),
    [{"ApiExceptionFault", {Ns, "ApiException"}}] = proplists:get_value(
                                                      fault, MutateInfo),
    EndPoint = proplists:get_value(endpoint, MutateInfo),
    [] = proplists:get_value(action, MutateInfo),
    MutateDocBin = proplists:get_value(doc, MutateInfo),
    true = byte_size(MutateDocBin) > 0.

one_model_query_op_info(_Config) ->
    Service = "CampaignService",
    Ns = "https://adwords.google.com/api/adwords/cm/v201306",
    EndPoint = lists:concat([Ns, "/", Service]),

    {ok, QueryInfo} = ews_svc:get_op_info(default, Service, "query"),
    "query" = proplists:get_value(name, QueryInfo),
    [{"query", {Ns, "query"}}] = proplists:get_value(in, QueryInfo),
    [{"RequestHeader", {Ns, "SoapHeader"}}] = proplists:get_value(in_hdr,
                                                                  QueryInfo),
    [{"queryResponse", {Ns, "queryResponse"}}] = proplists:get_value(
                                                   out, QueryInfo),
    [{"ResponseHeader", {Ns, "SoapResponseHeader"}}] = proplists:get_value(
                                                         out_hdr, QueryInfo),
    [{"ApiExceptionFault", {Ns, "ApiException"}}] = proplists:get_value(
                                                      fault, QueryInfo),
    EndPoint = proplists:get_value(endpoint, QueryInfo),
    [] = proplists:get_value(action, QueryInfo),
    QueryDocBin = proplists:get_value(doc, QueryInfo),
    true = byte_size(QueryDocBin) > 0.

one_model_get_op(_Config) ->
    Service = "CampaignService",
    {ok, Op} = ews_svc:get_op(default, Service, "get"),
    "get" = Op#op.name.

one_model_mutate_op(_Config) ->
    Service = "CampaignService",
    {ok, Op} = ews_svc:get_op(default, Service, "mutate"),
    "mutate" = Op#op.name.

one_model_query_op(_Config) ->
    Service = "CampaignService",
    {ok, Op} = ews_svc:get_op(default, Service, "query"),
    "query" = Op#op.name.

one_model_list_types(_Config) ->
    95 = length(ews_svc:list_types(default)).

one_model_emit_model(_Config) ->
    meck:new(ews_emit),
    meck:expect(ews_emit, model_to_file,
                fun(_Model, File, _) -> {model_emitted, File} end),

    {model_emitted, "test_file"} =
        ews_svc:emit_model(default, "test_file"),
    true = meck:validate(ews_emit),
    ok = meck:unload(ews_emit).

one_model_get_type(_Config) ->
    Ns = "https://adwords.google.com/api/adwords/cm/v201306",
    TypeKey = {Ns, "mutateResponse"},

    Type = ews_svc:get_type(default, TypeKey),

    TypeKey = Type#type.qname,
    mutate_response = Type#type.alias,
    undefined = Type#type.extends,
    [Element] = Type#type.elems,

    {Ns, "rval"} = Element#elem.qname,
    {Ns, "CampaignReturnValue"} = Element#elem.type,
    #meta{max = 1, min = 0} = Element#elem.meta.

one_model_call(_Config) ->
    Service = "CampaignService",
    Op = "get",
    HeaderParts = [<<"user">>, <<"pass">>],
    BodyParts = [<<"test_body">>],
    Opts = #{include_headers => false},

    meck:new(ews_soap),
    meck:expect(ews_soap, call, 6, {ok, {header, body}}),
    meck:expect(ews_soap, call, 7, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, 3, [decoded]),

    {ok, decoded} =
        ews_svc:call(default, Service, Op, HeaderParts, BodyParts, Opts),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}] =
        meck:history(ews_soap),
    EndPoint = "https://adwords.google.com/api/adwords/"
               "cm/v201306/CampaignService",
    Op = "get",
    [EndPoint, Op, [], encoded, encoded, Opts, []] = CallArgs,

    [{Pid, {ews_serialize, encode, HeaderArgs}, encoded},
     {Pid, {ews_serialize, encode, BodyArgs}, encoded},
     {Pid, {ews_serialize, decode, DecodeArgs}, [decoded]}
    ] = meck:history(ews_serialize),
    [HeaderParts, _InHdrs, Model] = HeaderArgs,
    [BodyParts, _Ins, Model] = BodyArgs,
    %% body comes from the ews_soap:call response
    [body, _Outs, Model] = DecodeArgs,

    true = meck:validate(ews_serialize),
    true = meck:validate(ews_soap),
    ok = meck:unload(ews_serialize),
    ok = meck:unload(ews_soap).

one_model_pre_hook(_Config) ->
    Service = "CampaignService",
    Op = "get",
    HeaderParts = [<<"user">>, <<"pass">>],
    BodyParts = [<<"test_body">>],
    Opts = #{x => 1, include_headers => false},

    meck:new(ews_soap),
    meck:expect(ews_soap, call, 6, {ok, {header, body}}),
    meck:expect(ews_soap, call, 7, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, 3, [decoded]),

    R1 = ews:add_pre_hook(fun ([_, _, _, _, _, O = #{x := 1}]) ->
                                  [a1, b1, c1, d1, e1, O#{x => 2}]
                          end),
    R2 = ews:add_pre_hook(fun ([a1, b1, c1, d1, e1, O = #{x := 2}]) ->
                                  [a2, b2, c2, d2, e2, O#{x => 3}]
                          end),
    {ok, decoded} =
        ews_svc:call(default, Service, Op, HeaderParts, BodyParts, Opts),
    ok = ews:remove_pre_hook(R1),
    ok = ews:remove_pre_hook(R2),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}] =
        meck:history(ews_soap),
    [a2, b2, c2, d2, e2, #{x := 3}, []] = CallArgs,

    [{Pid, {ews_serialize, encode, HeaderArgs}, encoded},
     {Pid, {ews_serialize, encode, BodyArgs}, encoded},
     {Pid, {ews_serialize, decode, DecodeArgs}, [decoded]}] =
        meck:history(ews_serialize),
    [HeaderParts, _InHdrs, Model] = HeaderArgs,
    [BodyParts, _Ins, Model] = BodyArgs,
    %% body comes from the ews_soap:call response
    [body, _Outs, Model] = DecodeArgs,

    true = meck:validate(ews_serialize),
    true = meck:validate(ews_soap),
    ok = meck:unload(ews_serialize),
    ok = meck:unload(ews_soap).

one_model_post_hook(_Config) ->
    Service = "CampaignService",
    Op = "get",
    HeaderParts = [<<"user">>, <<"pass">>],
    BodyParts = [<<"test_body">>],
    Opts = #{x => 1, include_headers => false},

    meck:new(ews_soap),
    meck:expect(ews_soap, call, 6, {ok, {header, body}}),
    meck:expect(ews_soap, call, 7, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, fun (B,_,_) -> [B] end),

    R = ews:add_post_hook(
          fun ([header, body, O]) ->
                  [header, {hooked_response, O#{y => 1}}, O]
          end),
    {ok, {hooked_response, #{x := 1, y := 1}}} =
        ews_svc:call(default, Service, Op, HeaderParts, BodyParts, Opts),
    ews:remove_post_hook(R),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}] =
        meck:history(ews_soap),
    EndPoint = "https://adwords.google.com/api/adwords/"
               "cm/v201306/CampaignService",
    [EndPoint, [], encoded, encoded, Opts] = CallArgs,

    [{Pid, {ews_serialize, encode, HeaderArgs}, encoded},
     {Pid, {ews_serialize, encode, BodyArgs}, encoded},
     {Pid, {ews_serialize, decode, DecodeArgs},
      [{hooked_response, #{x := 1, y := 1}}]}] =
        meck:history(ews_serialize),
    [HeaderParts, _InHdrs, Model] = HeaderArgs,
    [BodyParts, _Ins, Model] = BodyArgs,
    %% body comes from the ews_soap:call response
    [{hooked_response, #{x := 1, y := 1}}, _Outs, Model] = DecodeArgs,

    true = meck:validate(ews_serialize),
    true = meck:validate(ews_soap),
    ok = meck:unload(ews_serialize),
    ok = meck:unload(ews_soap).

one_model_remove_hook(_Config) ->
    Service = "CampaignService",
    Op = "get",
    HeaderParts = [<<"user">>, <<"pass">>],
    BodyParts = [<<"test_body">>],
    Opts = #{include_headers => false},

    meck:new(ews_soap),
    meck:expect(ews_soap, call, 6, {ok, {header, body}}),
    meck:expect(ews_soap, call, 7, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, fun (B,_,_) -> [B] end),

    R1 = ews:add_pre_hook(fun ([_, _, _, _, O]) -> [a, b, c, d, O] end),
    R2 = ews:add_pre_hook(fun ([_, _, _, _, O]) -> [a1, b1, c1, d1, O] end),
    R3 = ews:add_post_hook(fun ([H, _, O]) -> [H, {hooked_response, O}, O] end),
    ok = ews:remove_pre_hook(R2),
    {ok, {hooked_response, Opts}} =
        ews_svc:call(default, Service, Op, HeaderParts, BodyParts, Opts),
    ok = ews:remove_post_hook(R1),
    ok = ews:remove_post_hook(R3),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}] =
        meck:history(ews_soap),
    [a, b, c, d, Opts] = CallArgs,

    [{Pid, {ews_serialize, encode, HeaderArgs}, encoded},
     {Pid, {ews_serialize, encode, BodyArgs}, encoded},
     {Pid, {ews_serialize, decode, DecodeArgs}, [{hooked_response, Opts}]}] =
        meck:history(ews_serialize),
    [HeaderParts, _InHdrs, Model] = HeaderArgs,
    [BodyParts, _Ins, Model] = BodyArgs,
    %% body comes from the ews_soap:call response
    [{hooked_response, Opts}, _Outs, Model] = DecodeArgs,

    true = meck:validate(ews_serialize),
    true = meck:validate(ews_soap),
    ok = meck:unload(ews_serialize),
    ok = meck:unload(ews_soap).

add_two_models(_COnfig) ->
    %% Read file
    application:load(ews),
    Dir = filename:join(code:priv_dir(ews), "../test/data"),
    File1 = filename:join(Dir, "yahoo_v6.5_CampaignService.wsdl"),
    {ok, Bin1} = file:read_file(File1),
    File2 = filename:join(Dir, "yahoo_v6.5_LocationService.wsdl"),
    {ok, Bin2} = file:read_file(File2),

    %% Mock request
    meck:new(hackney),

    %% Get Wsdl, the actual URL is not important as we mock the hackney call
    %% with an already downloaded version
    meck:expect(hackney, request, 5, {ok, 200, ignore, Bin1}),
    Url1 = "https://ss.yahooapis.jp/services/V6.5/CampaignService?wsdl",
    {ok,[{"CampaignService",2}]} = ews_svc:add_wsdl_url(yahoo_campaign, Url1),
    {ok,[{"LocationService",1}]} = ews_svc:add_wsdl_bin(yahoo_location, Bin2),
    {ok,Services} = ews_svc:list_services(),
    Expected =  [{default, "CampaignService"},
                 {yahoo_campaign, "CampaignService"},
                 {yahoo_location, "LocationService"}],
    [] = Services -- Expected,
    {ok, ["CampaignService"]} = ews_svc:list_services(yahoo_campaign),
    {ok, ["LocationService"]} = ews_svc:list_services(yahoo_location),
    {ok, ["CampaignService"]} = ews_svc:list_services(default),

    true = meck:validate(hackney),
    meck:unload(hackney).

two_models_explicit_model(_Config) ->
    Service = "CampaignService",
    Ns1 = "https://adwords.google.com/api/adwords/cm/v201306",
    Ns2 = "http://ss.yahooapis.jp/V6",
    EndPoint1 = lists:concat([Ns1, "/", Service]),
    EndPoint2 = "https://USE_ADDRESS_RETURNED_BY_LOCATION_SERVICE/services/V6.5/CampaignService",

    {ok, GetInfo1} = ews_svc:get_op_info(default, Service, "get"),
    "get" = proplists:get_value(name, GetInfo1),
    [{"get", {Ns1, "get"}}] = proplists:get_value(in, GetInfo1),
    [{"RequestHeader", {Ns1, "SoapHeader"}}] = proplists:get_value(in_hdr,
                                                                   GetInfo1),
    [{"getResponse", {Ns1, "getResponse"}}] =
        proplists:get_value(out, GetInfo1),
    [{"ResponseHeader", {Ns1, "SoapResponseHeader"}}] = proplists:get_value(
                                                          out_hdr, GetInfo1),
    [{"ApiExceptionFault", {Ns1, "ApiException"}}] = proplists:get_value(
                                                       fault, GetInfo1),
    EndPoint1 = proplists:get_value(endpoint, GetInfo1),
    [] = proplists:get_value(action, GetInfo1),
    GetDocBin1 = proplists:get_value(doc, GetInfo1),
    true = byte_size(GetDocBin1) > 0,
    {ok, GetInfo2} = ews_svc:get_op_info(yahoo_campaign, Service, "get"),
    "get" = proplists:get_value(name, GetInfo2),
    [{"get", {Ns2, "get"}}] = proplists:get_value(in, GetInfo2),
    [{"RequestHeader", {Ns2, "SoapHeader"}}] = proplists:get_value(in_hdr,
                                                                   GetInfo2),
    [{"getResponse", {Ns2, "getResponse"}}] =
        proplists:get_value(out, GetInfo2),
    [{"ResponseHeader", {Ns2, "SoapResponseHeader"}}] = proplists:get_value(
                                                          out_hdr, GetInfo2),
    [{"ApiExceptionFault",
      {base,{"http://www.w3.org/2001/XMLSchema","string"},
       string,undefined,false,false}}] = proplists:get_value(
                                           fault, GetInfo2),
    EndPoint2 = proplists:get_value(endpoint, GetInfo2),
    [] = proplists:get_value(action, GetInfo2),
    undefined = proplists:get_value(doc, GetInfo2).

two_models_unambiguous(_Config) ->
    Service = "LocationService",
    Ns = "http://ss.yahooapis.jp/V6",
    EndPoint = "https://ss.yahooapis.jp/services/V6.5/LocationService",

    {ok, GetInfo} = ews_svc:get_op_info(Service, "get"),
    "get" = proplists:get_value(name, GetInfo),
    [{"get", {Ns, "get"}}] = proplists:get_value(in, GetInfo),
    [{"RequestHeader", {Ns, "SoapHeader"}}] = proplists:get_value(in_hdr,
                                                                   GetInfo),
    [{"getResponse", {Ns, "getResponse"}}] =
        proplists:get_value(out, GetInfo),
    [{"ResponseHeader", {Ns, "SoapResponseHeader"}}] = proplists:get_value(
                                                          out_hdr, GetInfo),
    [{"ApiExceptionFault",
      {base,{"http://www.w3.org/2001/XMLSchema","string"},
       string,undefined,false,false}}] = proplists:get_value(
                                           fault, GetInfo),
    EndPoint = proplists:get_value(endpoint, GetInfo),
    [] = proplists:get_value(action, GetInfo),
    undefined = proplists:get_value(doc, GetInfo).

two_models_ambiguous(_Config) ->
    Service = "CampaignService",
    {error, ambiguous_service} = ews_svc:get_op_info(Service, "get").

remove_model(_Config) ->
    {ok, Services} = ews_svc:list_services(),
    Expected =  [{default, "CampaignService"},
                 {yahoo_campaign, "CampaignService"},
                 {yahoo_location, "LocationService"}],
    [] = Services -- Expected,
    {ok, ["CampaignService"]} = ews_svc:list_services(yahoo_campaign),
    {ok, ["LocationService"]} = ews_svc:list_services(yahoo_location),
    {ok, ["CampaignService"]} = ews_svc:list_services(default),
    ews_svc:remove_model(yahoo_campaign),
    {ok, NewServices} = ews_svc:list_services(),
    NewExpected =  [{default, "CampaignService"},
                    {yahoo_location, "LocationService"}],
    [] = NewServices -- NewExpected,
    {error, no_service} =
        ews_svc:get_op_info(yahoo_campaign, "CampaignService", "get"),
    {ok, _} = ews_svc:get_op_info("CampaignService", "get"),
    ok.
