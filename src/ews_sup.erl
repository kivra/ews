%%  ews_sup
%%
%% >-----------------------------------------------------------------------< %%

-module(ews_sup).

-export([start_link/0]).

-export([init/1]).

-behaviour(supervisor).

%% >-----------------------------------------------------------------------< %%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% >-----------------------------------------------------------------------< %%

init(no_arg) ->
    Svc = child(ews_svc, ews_svc, worker, []),
    Alias = child(ews_alias, ews_alias, worker, []),
    Strategy = {one_for_one, 1, 60},
    {ok, {Strategy, [Svc, Alias]}}.

%% >-----------------------------------------------------------------------< %%

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, permanent, 3000, Type, [Mod]}.
