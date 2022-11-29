%%  ews_alias
%%
%% >-----------------------------------------------------------------------< %%

-module(ews_alias).

-export([start_link/0, stop/0]).

-export([create/1, create_unique/2, get_alias/2, get_qname/2,
         get_alias_map/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-behaviour(gen_server).

-record(state, {alias_map}).

%% >-----------------------------------------------------------------------< %%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% >-----------------------------------------------------------------------< %%

create({_, N}) ->
    to_underscore(N).

create_unique({Ns, N}, Model) when is_atom(Model) ->
    gen_server:call(?MODULE, {create, {Ns, N}, Model}).

get_alias({Ns, N}, Model) when is_atom(Model) ->
    gen_server:call(?MODULE, {get_alias, {Ns, N}, Model}).

get_qname(Alias, Model) when is_atom(Model) ->
    gen_server:call(?MODULE, {get_qname, Alias, Model}).

get_alias_map(Model) when is_atom(Model) ->
    gen_server:call(?MODULE, {get_alias_map, Model}).

stop() ->
    gen_server:cast(?MODULE, stop).

%% >-----------------------------------------------------------------------< %%

init([]) ->
    {ok, #state{alias_map=ets:new(ews_alias_map, [])}}.

handle_call({create, {Ns, N}, Model}, _, #state{alias_map=Map} = State) ->
    Alias = create_alias({Ns, N}, Model, Map),
    {reply, Alias, State};
handle_call({get_alias, {Ns, N}, Model}, _, #state{alias_map=Map} = State) ->
    Alias = get_alias({Ns, N}, Model, Map),
    {reply, Alias, State};
handle_call({get_qname, Alias, Model}, _, #state{alias_map=Map} = State) ->
    Qname = get_qname(Alias, Model, Map),
    {reply, Qname, State};
handle_call({get_alias_map, Model}, _, #state{alias_map=AliasMap} = State) ->
    %% We need to process the alias map a bit to return it in the correct
    %% format
    Map = process_alias_map(AliasMap, Model),
    {reply, Map, State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% >-----------------------------------------------------------------------< %%

create_alias({Ns, N}, Model, Map) when is_atom(Model) ->
    N2 = remove_parent(N),
    Alias = to_underscore(N2),
    case get_qname(Alias, Model, Map) of
        false ->
            ets:insert(Map, {{Ns, N}, Alias, Alias, Model}),
            Alias;
        {Ns, N} ->
            Alias;
        {_, _} ->
            case get_alias({Ns, N}, Model, Map) of
                false ->
                    Aliases =
                        [ A || [A] <- ets:match(Map, {'_', '$1', Alias, Model}) ],
                    NewAlias = create_new_alias(Aliases),
                    ets:insert(Map, {{Ns, N}, NewAlias, Alias, Model}),
                    NewAlias;
                AnAlias ->
                    AnAlias
            end
    end.

remove_parent(Name) ->
    case string:tokens(Name, "@") of
        [_, N] -> N;
        [N] -> N
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

get_alias({Ns, N}, Model, Map) when is_atom(Model) ->
    case ets:match(Map, {{Ns,N}, '$0', '_', Model}) of
        [] ->
            false;
        [[Alias]] ->
            Alias
    end.

get_qname(Alias, Model, Map) when is_atom(Model) ->
    case ets:match(Map, {'$0', Alias, '_', Model}) of
        [] ->
            false;
        [[Qname]] ->
            Qname
    end.

process_alias_map(AliasMap, Model) ->
    Matches = ets:match(AliasMap, {{'$0', '$1'}, '$2', '_', Model}),
    [ {{Ns, N}, A} || [Ns, N, A] <- Matches ].

to_underscore(Word) ->
    list_to_atom(string:join(to_underscore(Word, [], []), "_")).

to_underscore([C,D|Word], Stack, Res) when C >= $A, C =< $Z, D >= $a, D =< $z ->
    to_underscore([D|Word], [C], [lists:reverse(Stack)|Res]);
to_underscore([C,D|Word], Stack, Res) when D >= $A, D =< $Z ->
    case (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) of
        true ->
            to_underscore([D|Word], [], [lists:reverse([C|Stack])|Res]);
        false ->
            to_underscore([D|Word], [C|Stack], Res)
    end;
to_underscore([C,D|Word], Stack, Res) ->
    case is_alnum(C) of
        true ->
            to_underscore([D|Word], [C|Stack], Res);
        false ->
            to_underscore(Word, [D,C|Stack], Res)
    end;
to_underscore([C|Word], Stack, Res) ->
    to_underscore(Word, [C|Stack], Res);
to_underscore([], Stack, Res) ->
    [ string:to_lower(P) || P <- lists:reverse([lists:reverse(Stack)|Res]),
                            length(P) > 0 ].

is_alnum(C) when C >= $A, C =< $Z -> true;
is_alnum(C) when C >= $a, C =< $z -> true;
is_alnum(C) when C >= $0, C =< $9 -> true;
is_alnum(_) -> false.
