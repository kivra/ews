-module(ews_model).

-export([new/0, put/2, get/2, get_parts/2,
         get_from_base/2, get_from_alias/2, get_super/2,
         keys/1, values/1]).

-include("ews.hrl").

%% ---
%% TODO: Also put elements in and give them properties that are easy to ask
%% ---

new() ->
    ets:new(types, []).

put(#type{qname=Key} = T, Table) ->
    ets:insert_new(Table, {Key, T#type{alias=ews_alias:create_unique(Key)}});
put(#elem{qname=Key} = E, Table) ->
    ets:insert_new(Table, {Key, E}).

get({_,_} = Key, Table) ->
    case ets:lookup(Table, Key) of
        [{Key, Value}] ->
            Value;
        [] ->
            false
    end;
get(Key, Table) when is_atom(Key) ->
    get_from_alias(Key, Table).

get_parts(Key, Table) ->
    case ews_type:get(Key, Table) of
        [#type{elems=Parts, extends=undefined}|_] ->
            Parts;
        [#type{elems=Parts, extends=ExtendKey}|_] ->
            ExtendParts = get_parts(ExtendKey, Table),
            ExtendParts ++ Parts;
        false ->
            []
    end.

get_super({_, _} = Key, Table) ->
    case ews_type:get(Key, Table) of
        false ->
            Key;
        #type{extends=undefined} ->
            Key;
        #type{extends=Super} ->
            Super
    end;
get_super(Key, Table) when is_atom(Key) ->
    case ews_type:get_from_alias(Key, Table) of
        false ->
            Key;
        #type{extends=undefined} ->
            Key;
        #type{extends=Super} ->
            Super
    end.

get_from_alias(Alias, Tbl) ->
    case ets:match(Tbl, {'_', {type, '$0', Alias, '_', '_', '_'}}) of
        [] ->
            false;
        [[Key]] ->
            ews_type:get(Key, Tbl)
    end.

get_from_base(BaseKey, Table) ->
    case ets:match(Table, {{'$0', BaseKey}, '$1'}) of
        [] ->
            false;
        Vals ->
            [ {{Ns, BaseKey}, Type} || [Ns, Type] <- Vals ]
    end.

%% ----------------------------------------------------------------------------

keys(Tbl) ->
    case ets:match(Tbl, {'$0', '_'}) of
        [] ->
            [];
        Vals ->
            [ V || [V] <- Vals ]
    end.

values(Tbl) ->
    case ets:match(Tbl, {'_', '$0'}) of
        [] ->
            [];
        Vals ->
            [ V || [V] <- Vals ]
    end.

%% ----------------------------------------------------------------------------
