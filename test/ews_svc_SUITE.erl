-module(ews_svc_SUITE).
-include_lib("common_test/include/ct.hrl").

%%TODO header should be inside of include, but we need to rename the git repo
%%to ews first
-include("../src/ews.hrl").

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
         one_model_remove_hook/1
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
      ]}
    ].

all() ->
    [{group, no_models},
     add_model,
     {group, one_model},
     one_model_call,
     one_model_pre_hook,
     one_model_post_hook,
     one_model_remove_hook
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
    meck:new(lhttpc),
    meck:expect(lhttpc, request, 6, {ok, {{200, ignore}, ignore, Bin}}),

    %% Get Wsdl, the actual URL is not important as we mock the lhttpc call
    %% with an already downloaded version
    Url = "https://adwords.google.com/api/adwords/cm/"
          "v201306/CampaignService?wsdl",
    {ok,[{"CampaignService",3}]} = ews_svc:add_wsdl_url(default, Url),

    true = meck:validate(lhttpc),
    meck:unload(lhttpc).


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
    meck:expect(ews_emit, model_to_file, 2,
                fun(_Model, File) -> {model_emitted, File} end),

    {model_emitted, "test_file"} = ews_svc:emit_model(default, "test_file"),
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

    meck:new(ews_soap),
    meck:expect(ews_soap, call,4, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, 3, [decoded]),

    {ok, decoded} = ews_svc:call(default, Service, Op, HeaderParts, BodyParts),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}
    ] = meck:history(ews_soap),
    EndPoint = "https://adwords.google.com/api/adwords/"
               "cm/v201306/CampaignService",
    [EndPoint, [], encoded, encoded] = CallArgs,

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

    meck:new(ews_soap),
    meck:expect(ews_soap, call,4, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, 3, [decoded]),

    R1 = ews:add_pre_hook(fun ([_, _, _, _]) -> [a1, b1, c1, d1] end),
    R2 = ews:add_pre_hook(fun ([a1, b1, c1, d1]) -> [a2, b2, c2, d2] end),
    {ok, decoded} = ews_svc:call(default, Service, Op, HeaderParts, BodyParts),
    ok = ews:remove_pre_hook(R1),
    ok = ews:remove_pre_hook(R2),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}
    ] = meck:history(ews_soap),
    [a2, b2, c2, d2] = CallArgs,

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

one_model_post_hook(_Config) ->
    Service = "CampaignService",
    Op = "get",
    HeaderParts = [<<"user">>, <<"pass">>],
    BodyParts = [<<"test_body">>],

    meck:new(ews_soap),
    meck:expect(ews_soap, call,4, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, 3, [decoded]),

    R = ews:add_post_hook(fun (_) -> hooked_response end),
    {ok, hooked_response} =
        ews_svc:call(default, Service, Op, HeaderParts, BodyParts),
    ews:remove_post_hook(R),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}
    ] = meck:history(ews_soap),
    EndPoint = "https://adwords.google.com/api/adwords/"
               "cm/v201306/CampaignService",
    [EndPoint, [], encoded, encoded] = CallArgs,

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

one_model_remove_hook(_Config) ->
    Service = "CampaignService",
    Op = "get",
    HeaderParts = [<<"user">>, <<"pass">>],
    BodyParts = [<<"test_body">>],

    meck:new(ews_soap),
    meck:expect(ews_soap, call,4, {ok, {header, body}}),

    meck:new(ews_serialize),
    meck:expect(ews_serialize, encode, 3, encoded),
    meck:expect(ews_serialize, decode, 3, [decoded]),

    R1 = ews:add_pre_hook(fun ([_, _, _, _]) -> [a, b, c, d] end),
    R2 = ews:add_pre_hook(fun ([_, _, _, _]) -> [a1, b1, c1, d1] end),
    R3 = ews:add_post_hook(fun (_) -> hooked_response end),
    ok = ews:remove_pre_hook(R2),
    {ok, hooked_response} =
        ews_svc:call(default, Service, Op, HeaderParts, BodyParts),
    ok = ews:remove_post_hook(R1),
    ok = ews:remove_post_hook(R3),

    [{Pid, {ews_soap, call, CallArgs}, {ok, {header, body}}}
    ] = meck:history(ews_soap),
    [a, b, c, d] = CallArgs,

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
