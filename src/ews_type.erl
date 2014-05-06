-module(ews_type).

-export([new/0, put/2, append/2, get/2, get_parts/2,
         get_from_base/2, get_from_alias/2,
         get_super/2, get_inherited/2, inherits/3, equals/3,
         to_list/1, keys/1, values/1]).

-include("ews.hrl").

%% ---
%% TODO: Module could probably be a bit more lean
%% TODO: Remove all funs from export that isn't used outside this module
%% ---

new() ->
    ets:new(types, []).

put(#type{qname=Key} = T, Table) ->
    ets:insert_new(Table, {Key, T#type{alias=ews_alias:create_unique(Key)}}).

append(#type{qname=Key} = T, Table) ->
    T2 = T#type{alias=ews_alias:create_unique(Key)},
    case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, {Key, T2});
        [{Key, Value}] when is_tuple(Value) ->
            ets:insert(Table, {Key, [T2, Value]});
        [{Key, Values}] when is_list(Values) ->
            ets:insert(Table, {Key, [T2|Values]})
    end.

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
        #type{elems=Parts, extends=undefined} ->
            Parts;
        #type{elems=Parts, extends=ExtendKey} ->
            ExtendParts = get_parts(ExtendKey, Table),
            ExtendParts ++ Parts;
        false ->
            []
    end.

get_inherited(Key, Table) ->
    case ets:match(Table, {'_', {type, '$1', '$2', '_', Key, '_'}}) of
        [] ->
            false;
        Res ->
            [{N,A} || [N, A] <- Res ]
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

inherits(Key, SuperKey, Tbl) ->
    case get_super(Key, Tbl) of
        false ->
            false;
        SuperKey ->
            true
    end.

equals(Key1, Key2, Tbl) ->
    case {ews_type:get(Key1, Tbl), ews_type:get(Key2, Tbl)} of
        {#type{qname=Qn1, alias=A1}, #type{qname=Qn2, alias=A2}} ->
            Qn1 == Qn2 orelse A1 == A2;
        _ ->
            false
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
