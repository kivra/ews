-module(ews_serialize).

-export([encode/3, decode/3, validate/3]).

-compile(export_all).

-include("ews.hrl").

-define(SCHEMA_INSTANCE_NS, "http://www.w3.org/2001/XMLSchema-instance").

%% ---------------------------------------------------------------------------

encode(Terms, MsgElems, #model{elems=RootElems, type_map=Tbl}) ->
    case validate_root(MsgElems, RootElems) of
        {error, Error} ->
            error({error, Error});
        BaseElems ->
            Zipped = lists:zip(Terms, BaseElems),
            [ validate(Term, Elem, Tbl) || {Term, Elem} <- Zipped ]
    end.

validate_root([{_,_}=Qname | Es], RootEs) ->
    case lists:keyfind(Qname, #elem.qname, RootEs) of
        false ->
            {error, {"message input faulty", Qname}};
        #elem{qname=Qname} = E ->
            [ E | validate_root(Es, RootEs) ]
    end;
validate_root([#elem{qname=Qname} | Es], RootEs) ->
    case lists:keyfind(Qname, #elem.qname, RootEs) of
        false ->
            {error, {"message input faulty", Qname}};
        #elem{qname=Qname} = E ->
            [ E | validate_root(Es, RootEs) ]
    end;
validate_root([], _) ->
    [].

%% TODO: handle list of Terms -> check if #meta{max=M}, M > 1
%% TODO: check meta on undefined to see if zero elems is ok
validate(Terms, Types, Tbl) when is_list(Terms), is_list(Types) ->
    [ validate(Term, Type, Tbl) || {Term, Type} <- lists:zip(Terms, Types) ];
validate(undefined, _, _) ->
    undefined;
validate([_|_]=Terms, #elem{qname=Qname, meta=M}=E, Tbl) ->
    case M of
        #meta{max=Max} when Max > 1 ->
            [ validate(T, E, Tbl) || T <- Terms ];
        _ ->
            error({"expected single value: ", element(2, Qname), Terms})
    end;
validate(Term, #elem{qname=Qname, type={_,_}=TypeKey}, Tbl) ->
    Type = ews_type:get(TypeKey, Tbl), %% TODO: Handle false here
    {Qname, [], validate(Term, Type, Tbl)};
validate(Term, #elem{qname=Qname, type=Type}, Tbl) ->
    {Qname, [], validate(Term, Type, Tbl)};
validate(Term, #type{qname={_Ns, N}=Key, alias=A}, Tbl) when is_tuple(Term) ->
    [Name|Values] = tuple_to_list(Term),
    Elems = ews_type:get_parts(Key, Tbl),
    case N == atom_to_list(Name) orelse A == Name of
        false ->
            error({"expected #"++A++"{}", Term});
        true ->
            case length(Elems) == length(Values) of
                false ->
                    error("Wrong number of args in record "++N);
                true ->
                    Parts = lists:zip(Values, Elems),
                    lists:flatten([ validate(V, E, Tbl) ||
                                    {V, E} <- Parts, V /= undefined ])
            end
    end;
validate(Term, #type{qname={_, N}}, _) ->
    error({"expected #"++N++"{}", Term});
validate(Term, #base{erl_type=Type}, _) ->
    case Type of
        string when is_binary(Term) ->
            [{txt, Term}];
        integer when is_integer(Term) ->
            [{txt, integer_to_list(Term)}];
        float when is_float(Term) ->
            [{txt, float_to_list(Term)}];
        boolean when is_boolean(Term) ->
            [{txt, atom_to_list(Term)}];
        _ ->
            error({"expected "++atom_to_list(Type), Term})
    end;
validate(Term, #enum{type=#base{erl_type=_Base}, values=Values}, _) ->
    case lists:keyfind(Term, 1, Values) of
        false ->
            Accepted = string:join([ atom_to_list(A) || {A,_} <- Values]," | "),
            error({"expected "++Accepted, Term});
        {Term, Value} ->
            [{txt, Value}]
    end.

%% ---------------------------------------------------------------------------

decode(Terms, Elems, #model{elems=_Elems, type_map=Tbl}) ->
   [ validate_xml(T, E, Tbl) || {T, E} <- lists:zip(Terms, Elems) ].

validate_xml(undefined, #elem{meta=#meta{min=0}}, _) ->
    undefined;
validate_xml({Qname, As, Cs}, #elem{qname=Qname,type={_,_}=TypeKey}, Tbl) ->
    case has_inherited_type(As, Tbl) of
        #type{} = Type ->
            validate_xml({Qname, As, Cs}, Type, Tbl);
        false ->
            Type = ews_type:get(TypeKey, Tbl),
            validate_xml({Qname, As, Cs}, Type, Tbl)
    end;
validate_xml([{Qname, As, _}|_]=Es, #elem{qname=Qname,type={_,_}=TypeKey}, Tbl) ->
    case has_inherited_type(As, Tbl) of %% FIXME: Can't assume all element is same type in list
        #type{} = Type ->
            validate_xml(Es, Type, Tbl);
        false ->
            Type = ews_type:get(TypeKey, Tbl),
            validate_xml(Es, Type, Tbl)
    end;
validate_xml({Qname, As, Cs}, #elem{qname=Qname,type=Type}, Tbl) ->
    validate_xml({Qname, As, Cs}, Type, Tbl);
validate_xml([{Qname, _, _}|_]=Es, #elem{qname=Qname,type=Type}, Tbl) ->
    validate_xml(Es, Type, Tbl);
validate_xml(Es, Type, Tbl) when is_list(Es) ->
    [ validate_xml(E, Type, Tbl) || E <- Es ];
validate_xml({_, As, Cs}, #type{qname=Key, alias=Alias}, Tbl) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            Elems = ews_type:get_parts(Key, Tbl),
            Pairs = match_children_elems(Cs, Elems, [], []),
            list_to_tuple([Alias | [ validate_xml(T, E, Tbl) || {T, E} <- Pairs ]])
    end;
validate_xml({_Qname, _, []}, #base{erl_type=string}, _) ->
    undefined;
validate_xml({_Qname, _, [{txt, Txt}]}, #base{erl_type=Type}, _) ->
    case catch to_base(Txt, Type) of
        {'EXIT', _Reason} ->
            error({"failed to convert base", {Txt, Type}});
        Term ->
            Term
    end;
validate_xml({_Qname, _, [{txt, Txt}]}, #enum{values=Vs}, _) ->
    Str = binary_to_list(Txt),
    case lists:keyfind(Str, 2, Vs) of
        {Value, Str} ->
            Value;
        false ->
            error({"failed to convert enum", {Txt, Vs}})
    end.

%% TODO: Fix Max > 1 terms are bunched into a list
%% TODO: Just start by matching pairs together, maybe not even check meta now,
%%       but after all terms that conform to the same Qname have been bunched
match_children_elems([{Qname,_,_}=C1, {Qname,_,_}=C2|Cs], [#elem{qname=Qname}=E|Es], Acc, Res) ->
    match_children_elems([C2|Cs], [E|Es], [C1|Acc], Res);

match_children_elems([{Qname,_,_}=C1|Cs], [#elem{qname=Qname}=E|Es], [], Res) ->
    match_children_elems(Cs, Es, [], [{C1,E}|Res]);

match_children_elems([{Qname,_,_}=C1|Cs], [#elem{qname=Qname}=E|Es], [{Qname,_,_}|Acc], Res) ->
    match_children_elems(Cs, Es, [], [{lists:reverse([C1|Acc]),E}|Res]);

match_children_elems([{Qname,_,_}=C1|Cs], [#elem{qname=Qname}=E|Es], Acc, Res) ->
    match_children_elems([C1|Cs], Es, [], [{lists:reverse(Acc),E}|Res]);

match_children_elems([{_,_,_}=C|Cs], [#elem{meta=#meta{min=0}}=E|Es], Acc, Res) ->
    match_children_elems([C|Cs], Es, Acc, [{undefined,E}|Res]);

match_children_elems([{Qname,_,_}|_], [#elem{qname={_,N}}|_], _, _) ->
    error({"expected "++N, Qname});

match_children_elems([], [], [], Res) ->
    lists:reverse(Res);
match_children_elems([], [#elem{meta=#meta{min=0}}=E|Es], [], Res) ->
    match_children_elems([], Es, [], [{undefined,E}|Res]);
match_children_elems([], [#elem{meta=#meta{min=0}}=E|Es], Acc, Res) ->
    match_children_elems([], Es, [], [{undefined,E},lists:reverse(Acc)|Res]);
match_children_elems([], [], Acc, Res) ->
    [lists:reverse(Acc) | lists:reverse(Res)].

to_base(Txt, string) when is_binary(Txt) -> Txt;
to_base(Txt, string) when is_list(Txt) -> list_to_binary(Txt);
to_base(Txt, integer) -> list_to_integer(binary_to_list(Txt));
to_base(Txt, float) -> try_cast_float(binary_to_list(Txt));
to_base(<<"true">>, boolean) -> true;
to_base(<<"false">>, boolean) -> false;
to_base(<<"TRUE">>, boolean) -> true;
to_base(<<"FALSE">>, boolean) -> false;
to_base(<<"True">>, boolean) -> true;
to_base(<<"False">>, boolean) -> false;
to_base(<<"1">>, boolean) -> true;
to_base(<<"0">>, boolean) -> false.

try_cast_float(Str) ->
    case catch list_to_float(Str) of
        {'EXIT', _} ->
            case catch list_to_integer(Str) of
                {'EXIT', _} ->
                    Str;
                Int ->
                    float(Int)
            end;
        Float ->
            Float
    end.

is_nil(Attributes) ->
    case lists:keyfind({?SCHEMA_INSTANCE_NS, "nil"}, 1, Attributes) of
        {_, "true"} ->
            true;
        {_, "false"} ->
            false;
        false ->
            false
    end.

has_inherited_type(Attributes, Tbl) ->
    case lists:keyfind({?SCHEMA_INSTANCE_NS, "type"}, 1, Attributes) of
        {_, TypeBase} ->
            [{_, Type}] = ews_type:get_from_base(TypeBase, Tbl),
            Type;
        false ->
            false
    end.
