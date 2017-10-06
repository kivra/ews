-module(ews_wsdl_SUITE).
-include_lib("common_test/include/ct.hrl").

%%TODO header should be inside of include, but we need to rename the git repo
%%to ews first
-include("../src/ews.hrl").

%% CT functions
-export([suite/0, groups/0, all/0,
         init_per_group/2, end_per_group/2
        ]).

%% Tests
-export([google_v201306_ensure_record/1,
         google_v201306_correct_service/1
        ]).

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [{google_v201306_campaignService, [shuffle, parallel],
      [google_v201306_ensure_record,
       google_v201306_correct_service
      ]}].

all() ->
    [{group, google_v201306_campaignService}].

init_per_group(google_v201306_campaignService, Config) ->
    ews:start(),

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
    Wsdl = ews_wsdl:fetch_and_parse(Url),

    meck:unload(lhttpc),
    ews:stop(),
    [{google_v201306_campaignService, Wsdl} | Config].

end_per_group(google_v201306_campaignService, _Config) ->
    ok.

google_v201306_ensure_record(Config) ->
    Wsdl = proplists:get_value(google_v201306_campaignService, Config),
    true = is_record(Wsdl, wsdl).

google_v201306_correct_service(Config) ->
    Wsdl = proplists:get_value(google_v201306_campaignService, Config),
    [ResService] = Wsdl#wsdl.services,
    "CampaignService" = ResService#service.name.
