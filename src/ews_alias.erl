%%  ews_alias
%%
%% >-----------------------------------------------------------------------< %%

-module(ews_alias).

-export([start_link/0]).

-export([create/1, create_unique/1, get_alias/1, get_qname/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-behaviour(gen_server).

-record(state, {alias_map}).

-compile(export_all).

%% >-----------------------------------------------------------------------< %%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% >-----------------------------------------------------------------------< %%

create({_, N}) ->
    to_underscore(N).

create_unique({Ns, N}) ->
    gen_server:call(?MODULE, {create, {Ns, N}}).

get_alias({Ns, N}) ->
    gen_server:call(?MODULE, {get_alias, {Ns, N}}).

get_qname(Alias) ->
    gen_server:call(?MODULE, {get_qname, Alias}).

get_alias_map() ->
    gen_server:call(?MODULE, get_alias_map).

%% >-----------------------------------------------------------------------< %%

init([]) ->
    {ok, #state{alias_map=ets:new(ews_alias_map, [])}}.

handle_call({create, {Ns, N}}, _, #state{alias_map=Map} = State) ->
    Alias = create_alias({Ns, N}, Map),
    {reply, Alias, State};
handle_call({get_alias, {Ns, N}}, _, #state{alias_map=Map} = State) ->
    Alias = get_alias({Ns, N}, Map),
    {reply, Alias, State};
handle_call({get_qname, Alias}, _, #state{alias_map=Map} = State) ->
    Qname = get_qname(Alias, Map),
    {reply, Qname, State};
handle_call(get_alias_map, _, #state{alias_map=Map} = State) ->
    Matches = ets:match(Map, {{'$0', '$1'}, '$2'}),
    {reply, [ {{Ns, N}, A} || [Ns, N, A] <- Matches ], State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% >-----------------------------------------------------------------------< %%

create_alias({Ns, N}, Map) ->
    Alias = to_underscore(N),
    case get_qname(Alias, Map) of
        false ->
            ets:insert(Map, {{Ns, N}, Alias}),
            Alias;
        {Ns, N} ->
            Alias;
        {_, N} ->
            case get_alias({Ns, N}, Map) of
                false ->
                    Aliases = [ A || [A] <- ets:match(Map, {{'_', N}, '$1'}) ],
                    NewAlias = create_new_alias(Aliases),
                    ets:insert(Map, {{Ns, N}, NewAlias}),
                    NewAlias;
                AnAlias ->
                    AnAlias
            end
    end.

create_new_alias([Alias]) ->
    list_to_atom(atom_to_list(Alias)++"_1");
create_new_alias(Aliases) ->
    {Base, Num} = find_postfix(lists:last(lists:sort(Aliases))),
    list_to_atom(Base ++ "_" ++ integer_to_list(Num+1)).

find_postfix(Alias) when is_atom(Alias) ->
    find_postfix(atom_to_list(Alias));
find_postfix(Alias) ->
    [PostFix|BaseTokens] = lists:reverse(string:tokens(Alias, "_")),
    NumTest = fun(C) when C >= $0, C =< $9 -> true; (_) -> false end,
    case lists:all(NumTest, PostFix) of
        true ->
            {string:join(lists:reverse(BaseTokens), "_"),
             list_to_integer(PostFix)};
        false ->
            {string:join(lists:reverse([PostFix|BaseTokens]), "_"), 1}
    end.

get_alias({Ns, N}, Map) ->
    case ets:match(Map, {{Ns,N}, '$0'}) of
        [] ->
            false;
        [[Alias]] ->
            Alias
    end.

get_qname(Alias, Map) ->
    case ets:match(Map, {'$0', Alias}) of
        [] ->
            false;
        [[Qname]] ->
            Qname
    end.

to_underscore(Word) ->
    list_to_atom(string:join(to_underscore(Word, [], []), "_")).

to_underscore([C,D|Word], Stack, Res) when C >= $A, C =< $Z, D >= $a, D =< $z ->
    to_underscore([D|Word], [C], [lists:reverse(Stack)|Res]);
to_underscore([C,D|Word], Stack, Res) when C >= $a, C =< $z, D >= $A, D =< $Z ->
    to_underscore([D|Word], [], [lists:reverse([C|Stack])|Res]);
to_underscore([C|Word], Stack, Res) ->
    to_underscore(Word, [C|Stack], Res);
to_underscore([], Stack, Res) ->
    [ string:to_lower(P) || P <- lists:reverse([lists:reverse(Stack)|Res]),
                            length(P) > 0 ].
