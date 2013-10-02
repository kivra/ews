-module(ews_type).

-export([new/0, put/2, append/2, get/2, get_parts/2, get_from_base/2,
         to_list/1, keys/1, values/1]).

-include("ews.hrl").

%% ---

new() ->
    ets:new(types, []).

put(#type{qname=Key} = T, Table) ->
    ets:insert_new(Table, {Key, T#type{alias=ews_alias:create_unique(Key)}}).

append(#type{qname=Key} = T, Table) ->
    T2 = T#type{alias=ews_alias:create(Key)},
    case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, {Key, T2});
        [{Key, Value}] when is_tuple(Value) ->
            ets:insert(Table, {Key, [T2, Value]});
        [{Key, Values}] when is_list(Values) ->
            ets:insert(Table, {Key, [T2|Values]})
    end.

get(Key, Table) ->
    case ets:lookup(Table, Key) of
        [{Key, Value}] ->
            Value;
        [] ->
            false
    end.

get_parts(Key, Table) ->
    case ets:lookup(Table, Key) of
        [{Key, #type{qname=Key, elems=Parts, extends=undefined}}] ->
            Parts;
        [{Key, #type{qname=Key, elems=Parts, extends={ExtNs, ExtName}}}] ->
            ExtendKey = {ExtNs, ExtName},
            ExtendParts = get_parts(ExtendKey, Table),
            ExtendParts ++ Parts;
        [] ->
            []
    end.

get_from_base(BaseKey, Table) ->
    case ets:match(Table, {{'$0', BaseKey}, '$1'}) of
        [] ->
            false;
        Vals ->
            [ {{Ns, BaseKey}, Type} || [Ns, Type] <- Vals ]
    end.

to_list(Tbl) ->
    case ets:match(Tbl, '$0') of
        [] ->
            [];
        Vals ->
            [ V || [V] <- Vals ]
    end.

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
