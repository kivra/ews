-module(ews_model).

-export([new/0, put/3, put_elem/3, replace/2, get/2, get_elem/2, get_elem/3,
         get_parts/2,
         get_from_base/2, get_from_alias/2, get_super/2, get_subs/2,
         keys/1, values/1, elem_keys/1, elem_values/1,
         is_root/2, append_model/3]).

-include("ews.hrl").
-include_lib("ews/include/ews.hrl").

%% ---
%% TODO: Also put elements in and give them properties that are easy to inspect
%% ---

new() ->
    ets:new(types, []).

put(#type{qname=Key} = T, Model, Table) when is_atom(Model) ->
    Type = T#type{alias=ews_alias:create_unique(Key, Model)},
    ets:insert_new(Table, {Key, Type});
put(#elem{qname=Key} = E, _Model, Table) ->
    ets:insert_new(Table, {{Key, root}, E}).

put_elem(#elem{qname=Key} = E, Parent, Table) ->
    ets:insert_new(Table, {{Key, Parent}, E}).

replace(#type{qname=Key} = Type, Table) ->
    ets:insert(Table, {Key, Type});
replace(#elem{qname=Key} = E, Table) ->
    ets:insert(Table, {{Key, root}, E}).

get({_,_} = Key, Table) ->
    case ets:lookup(Table, Key) of
        [{Key, Value}] ->
            Value;
        [] ->
            false
    end;
get(Key, Table) when is_atom(Key) ->
    get_from_alias(Key, Table).

get_elem(#elem{qname=Key}, Table) ->
    get_elem(Key, Table);
get_elem({_,_} = Key, Table) ->
    case ets:match(Table, {{Key, root}, '$1'}) of
        [] ->
            false;
        [[Elem]] ->
            Elem
    end.

get_elem(#elem{qname=Key}, Parent, Table) ->
    get_elem(Key, Parent, Table);
get_elem({_,_} = Key, Parent, Table) ->
    case ets:match(Table, {{Key, Parent}, '$1'}) of
        [] ->
            false;
        [[Elem]] ->
            Elem
    end.

get_parts(Key, _Table) when is_list(Key) ->
    ct:pal("warning: can't get parts for type ~p", [Key]),
    [];
get_parts(Key, Table) ->
    case ews_model:get(Key, Table) of
        #type{elems=Parts, extends=undefined} ->
            Parts;
        #type{elems=Parts, extends=ExtendKey} ->
            ExtendParts = get_parts(ExtendKey, Table),
            ExtendParts ++ Parts;
        false ->
            []
    end.

get_super({_, _} = Key, Table) ->
    case ews_model:get(Key, Table) of
        false ->
            Key;
        #type{extends=undefined} ->
            Key;
        #type{extends=Super} ->
            Super
    end;
get_super(Key, Table) when is_atom(Key) ->
    case get_from_alias(Key, Table) of
        false ->
            Key;
        #type{extends=undefined} ->
            Key;
        #type{extends=Super} ->
            Super
    end.

get_subs(Key, Table) ->
    Children = ets:match_object(Table, {'_', #type{extends = Key, _ = '_'}}),
    LowerDescendants = [get_subs(K, Table) || {K, _} <- Children],
    Children ++ lists:append(LowerDescendants).

get_from_alias(Alias, Tbl) ->
    case ets:match(Tbl, {'_', {type, '$0', Alias, '_', '_', '_'}}) of
        [] ->
            false;
        [[Key]] ->
            ews_model:get(Key, Tbl)
    end.

get_from_base(BaseKey, Table) ->
    case ets:match(Table, {{'$0', BaseKey}, '$1'}) of
        [] ->
            false;
        Vals ->
            [ {{Ns, BaseKey}, Type} || [Ns, Type] <- Vals ]
    end.

is_root(#elem{qname=Key}, Table) ->
    is_root(Key, Table);
is_root(Key, Table) ->
    length(ets:match(Table, {{Key, root}, '_'})) > 0.

append_model(undefined, Model, _) -> Model;
append_model(Model, undefined, _) -> Model;
append_model(CM = #model{type_map=CurrentMap, elems=E1, clashes=CurrentClashes},
             #model{type_map=NewMap, elems=E2}, ModelName) ->
    NewElems = lists:ukeysort(#elem.qname, E1++E2),
    NewClashes = merge_types(CurrentMap, NewMap, CurrentClashes, ModelName),
    CM#model{elems=NewElems, clashes=NewClashes}.

%% ----------------------------------------------------------------------------

keys(Tbl) ->
    case ets:match(Tbl, {'$0', '$1'}) of
        [] ->
            [];
        Vals ->
            [ V || [V = {_, _}, #type{}] <- Vals ]
    end.

values(Tbl) ->
    case ets:match(Tbl, {'_', '$0'}) of
        [] ->
            [];
        Vals ->
            [ V || [#type{} = V] <- Vals ]
    end.

elem_keys(Tbl) ->
    case ets:match(Tbl, {'$0', '$1'}) of
        [] ->
            [];
        Vals ->
            [ V || [V, #elem{}] <- Vals ]
    end.

elem_values(Tbl) ->
    case ets:match(Tbl, {'_', '$0'}) of
        [] ->
            [];
        Vals ->
            [ V || [#elem{} = V] <- Vals ]
    end.

%% ----------------------------------------------------------------------------

%% FIXME: Seems broken. Somewhere we lose types.
merge_types(CurrentMap, NewMap, ClashDict, ModelName) ->
    TF = fun(Key, Clashes) ->
             NewType = ews_model:get(Key, NewMap),
             case ews_model:put(NewType, ModelName, CurrentMap) of
                 true ->
                     Clashes;
                 false ->
                     OldType = ews_model:get(Key, CurrentMap),
                     case merge_types(OldType, NewType) of
                         OldType ->
                             Clashes;
                         MergedType = #type{} ->
                             replace(MergedType, CurrentMap),
                             Clashes;
                         _ ->
                             NewClashes = dict:append(Key, NewType, Clashes),
                             dict:append(Key, OldType, NewClashes)
                     end
             end
         end,
    EF = fun({Key, Parent}, Clashes) ->
             NewElem = get_elem(Key, Parent, NewMap),
             case ews_model:put(NewElem, ModelName, CurrentMap) of
                true ->
                    Clashes;
                false ->
                     %% borken
                    OldElem = get_elem(Key, Parent, CurrentMap),
                    case merge_elem(OldElem, NewElem) of
                        OldElem ->
                            Clashes;
                        MergedElem = #elem{} ->
                            replace(MergedElem, CurrentMap),
                             Clashes;
                        _ ->
                            NewClashes = dict:append({root, Key},
                                                     NewElem, Clashes),
                            dict:append({root, Key}, OldElem, NewClashes)
                    end
             end
         end,
    TypeKeys = keys(NewMap),
    ElemKeys = elem_keys(NewMap),
    NewClashDict = lists:foldl(TF, ClashDict, TypeKeys),
    lists:foldl(EF, NewClashDict, ElemKeys).

merge_types(T1 = #type{elems = T1Elems, extends = E, abstract = A},
           #type{elems = T2Elems, extends = E, abstract = A}) ->
    case merge_elem_lists(T1Elems, T2Elems) of
        E = {error, _} ->
            E;
        MergedElems ->
            T1#type{elems = MergedElems}
    end;
merge_types(T1, T2) ->
    {error, {incompatible_types, T1, T2}}.

merge_elem_lists(Elems1, Elems2) ->
    MF = fun (_, E = {error, _}) ->
                 E;
             (E1 = #elem{qname = Qn1, meta = #meta{min = Min}}, {Es, E2s}) ->
                 case lists:splitwith(fun (#elem{qname = Qn2}) ->
                                              Qn2 /= Qn1
                                      end, E2s) of
                     {H, [E2 | T]} ->
                         case merge_elem(E1, E2) of
                             E = {error, _} ->
                                 E;
                             NewE ->
                                 {[NewE | Es], H ++ T}
                         end;
                     {_, []} when Min == 0 ->
                         {[E1 | Es], E2s};
                     Error ->
                         error({unmatched_element, Error, E1, Elems2}),
                         {error, {unmatched_element, E1, Elems2}}
                 end
         end,
    case lists:foldl(MF, {[], Elems2}, Elems1) of
        {Res, []} ->
            lists:reverse(Res);
        E = {error, _} ->
            E;
        {Res, Unmatched} ->
            case lists:all(fun (#elem{meta = #meta{min = Min}}) ->
                                   Min == 0
                           end, Unmatched) of
                true ->
                    lists:reverse(Res) ++ Unmatched;
                false ->
                    {error, {unmatched_elements, Unmatched}}
            end
    end.

merge_elem(E1 = #elem{qname = Qn, type = T1, meta = M1},
           #elem{qname = Qn, type = T2, meta = M2}) ->
    E1#elem{type = combine_types(T1, T2), meta = combine_meta(M1, M2)};
merge_elem(E1, E2) ->
    {error, {incompatible_elements, E1, E2}}.

combine_types(T, T) ->
    T;
combine_types(T1, T2) when is_list(T1), is_list(T2) ->
    T1 ++ (T2 -- T1);
combine_types(T1, T2) when is_list(T1) ->
    combine_types(T1, [T2]);
combine_types(T1, T2) when is_list(T2) ->
    combine_types([T1], T2);
combine_types(T1, T2) ->
    [T1, T2].

combine_meta(
  M1 = #meta{nillable = N, default = D, fixed = F, max = Max1, min = Min1},
  #meta{nillable = N, default = D, fixed = F, max = Max2, min = Min2}) ->
    M1#meta{min = min2(Min1, Min2), max = max2(Max1, Max2)}.

min2(undefined, M) -> M;
min2(M, undefined) -> M;
min2(M1, M2) -> min(M1, M2).

max2(undefined, M) -> M;
max2(M, undefined) -> M;
max2(M1, M2) -> max(M1, M2).
