-module(ews_wsdl_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%TODO header should be inside of include, but we need to rename the git repo
%%to ews first
-include("../src/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_group/2, end_per_group/2
        ]).

%% Tests
-export([ google_v201306_ensure_record/1
        , google_v201306_correct_service/1
        , restriction/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [ {google_v201306_campaignService, [shuffle, parallel],
       [google_v201306_ensure_record,
        google_v201306_correct_service
       ]}
    , {mm_service,
       [restriction
       ]}
    ].

all() ->
    [ {group, google_v201306_campaignService}
    , {group, mm_service}
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
init_per_group(mm_service, Config) ->
    application:ensure_all_started(ews),
    Config.

end_per_group(google_v201306_campaignService, _Config) ->
    ok;
end_per_group(mm_service, _Config) ->
    ews:remove_model(ek_mm_test).

google_v201306_ensure_record(Config) ->
    Wsdl = proplists:get_value(google_v201306_campaignService, Config),
    true = is_record(Wsdl, wsdl).

google_v201306_correct_service(Config) ->
    Wsdl = proplists:get_value(google_v201306_campaignService, Config),
    [ResService] = Wsdl#wsdl.services,
    "CampaignService" = ResService#service.name.

restriction(_Config) ->
    Dir = filename:join(code:priv_dir(ews), "../test"),
    File = filename:join(Dir, "mm_service.wsdl"),
    %% Mock request
    meck:new(hackney),
    meck:expect(hackney, request, 5, {ok, 200, ignore, <<>>}),
    ews:add_wsdl_to_model(ek_mm_test, File),
    #model{type_map=Tbl, simple_types=Ts} =
        ews_svc:get_model(ek_mm_test),
    Ret = ews_emit:sort_types(Tbl, Ts),
    ?assertEqual(xxx, Ret).
