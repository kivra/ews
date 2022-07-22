-module(ews_serialize).

-export([encode/3, decode/3, record_to_map/2]).

-include("ews.hrl").

-define(SCHEMA_INSTANCE_NS, "http://www.w3.org/2001/XMLSchema-instance").

%% API ------------------------------------------------------------------------

%% @doc Encodes and validates a list of erlang terms that describes
%%      a soap message. The soap message has root elements that each term must
%%      correspond with.
%%          Terms       - The terms that correspond to the MessageElements
%%          MsgElems    - The Elements that make up the message we want to
%%                        validate against
%%          Model       - The model that describe the types that the elements
%%                        in the message has.

-spec encode([any()], [any()], #model{}) -> iolist().
encode(Terms, MsgElems, #model{type_map=Tbl}) ->
    case lists:all(fun(E) -> ews_model:is_root(E, Tbl) end, MsgElems) of
        false ->
            error({error, {all_not_root, MsgElems}});
        true ->
            BaseElems = [ ews_model:get_elem(E, Tbl) || E <- MsgElems ],
            Zipped = lists:zip(Terms, BaseElems),
            [ encode_term(Term, Elem, Tbl) || {Term, Elem} <- Zipped ]
    end.

%% @doc Decodes and validates an xml string that represents a soap message.
%%      Returns a structured term that represents the payload
%%          Terms       - String representing xml roots
%%          Elems       - The Elements that make up the message we want to
%%                        validate against
%%          Model       - The model that describe the types that the elements
%%                        in the message has.
-spec decode([any()], [any()], #model{}) -> [any()].
decode(Terms, Elems, #model{elems=_Elems, type_map=Tbl}) ->
    [validate_xml(T, E, Tbl) || {T, E} <- lists:zip(Terms, Elems)].

%% @doc Converts a term represented by a tuple to a map where the keys
%%      are the same as the record field names. Any value that is undefined
%%      is not included in the returned map.
%%          Term        - Record representing a valid term
%%          Model       - The model that describes the type that the term
%%                        has.
-spec record_to_map(tuple(), #model{}) -> map().
record_to_map(Term, _M) when is_record(Term, fault) ->
    %% Should probably be handled by having fault in a model...
    FieldNames = record_info(fields, fault),
    Fields = tl(tuple_to_list(Term)),
    maps:from_list([{K, V} || {K, V} <- lists:zip(FieldNames, Fields),
                              V /= undefined]);
record_to_map(Term, M = #model{type_map = Tbl}) ->
    [Alias | Values] = tuple_to_list(Term),
    Parts = ews_model:get_parts(Alias, Tbl),
    FieldNames = field_names(Parts),
    MapValues = lists:map(fun (V) -> field_to_map(V, M) end, Values),
    maps:from_list([{K, V} ||  {K, V} <- lists:zip(FieldNames, MapValues),
                               V /= undefined]).
%% Internal -------------------------------------------------------------------

%% TODO: handle list of Terms -> check if #meta{max=M}, M > 1
%% TODO: check meta on undefined to see if zero elems is ok
encode_term(Terms, Types, Tbl) when is_list(Terms), is_list(Types) ->
    [ encode_term(Term, Type, Tbl) ||
      {Term, Type} <- lists:zip(Terms, Types) ];
encode_term(undefined, _, _) ->
    undefined;
encode_term(nil, #elem{qname=Qname, meta=#meta{nillable=true}}, _) ->
    {Qname, [{{?SCHEMA_INSTANCE_NS, "nil"}, "true"}], []};
encode_term(nil, #elem{qname=Qname, meta=#meta{nillable=false}}, _) ->
    error({"non-nillable type nilled", Qname});
encode_term([_|_]=Terms, #elem{qname=Qname, meta=M, type=Type}=E, Tbl) ->
    case M of
        #meta{max=Max} when Max > 1 ->
            [ encode_term(T, E, Tbl) || T <- Terms ];
        _ ->
            case Type of
                #base{list=true} ->
                    {Qname, [], encode_term(Terms, Type, Tbl)};
                #base{erl_type=string} ->
                    {Qname, [], [{txt, list_to_binary(Terms)}]};
                #enum{list=true} ->
                    {Qname, [], encode_term(Terms, Type, Tbl)};
                _ ->
                    error({"expected single value: ", element(2, Qname), Terms})
            end
    end;
encode_term(Term, #elem{type=Types}=E, Tbl) when is_list(Types) ->
    TestType = fun (Type, undefined) ->
                       try
                           {ok, encode_term(Term, E#elem{type=Type}, Tbl)}
                       catch
                           _:_ ->
                               undefined
                       end;
                   (_Type, Result) ->
                       Result
               end,
    case lists:foldl(TestType, undefined, Types) of
        {ok, Result} ->
            Result;
        undefined ->
            Ts = [ews_model:get(Qn, Tbl) || Qn <- Types],
            Aliases = [[$# | atom_to_list(T#type.alias) ++ "{}"] || T <- Ts],
            Records = string:join(Aliases, ", "),
            error({"expected one of " ++ Records, Term})
    end;
encode_term(Term, #elem{qname=Qname, type={_,_}=TypeKey}, Tbl) ->
    [Name|_] = tuple_to_list(Term),
    #type{qname=InheritedTypeKey} = InheritedType = ews_model:get(Name, Tbl),
    SuperKey = ews_model:get_super(Name, Tbl),
    case TypeKey of
        InheritedTypeKey ->
            {Qname, [], encode_term(Term, InheritedType, Tbl)};
        SuperKey ->
            TypeDecl = {{?SCHEMA_INSTANCE_NS, "type"}, InheritedTypeKey},
            Super = ews_model:get(SuperKey, Tbl),
            {Qname, [TypeDecl], encode_term(Term, Super, Tbl)}
    end;
encode_term(Term, #elem{qname=Qname, type=Type}, Tbl) ->
    {Qname, [], encode_term(Term, Type, Tbl)};
encode_term(Term, #type{qname=Key, alias=A}, Tbl) when is_tuple(Term) ->
    [Name|Values] = tuple_to_list(Term), %% TODO: Move this one clause up
    Super = ews_model:get_super(Name, Tbl),
    case ews_model:get(Name, Tbl) of
        #type{qname=InheritedKey} when Super == Key ->
            Elems = ews_model:get_parts(InheritedKey, Tbl),
            Parts = lists:zip(Values, Elems),
            lists:flatten([ encode_term(V, E, Tbl) ||
                            {V, E} <- Parts, V /= undefined ]);
        #type{qname=Key} ->
            Elems = ews_model:get_parts(Key, Tbl),
            Parts = lists:zip(Values, Elems),
            lists:flatten([ encode_term(V, E, Tbl) ||
                            {V, E} <- Parts, V /= undefined ]);
        #type{qname=_Qname} ->
            #type{alias=KeyAlias} = ews_model:get(Key, Tbl),
            error({"expected #"++atom_to_list(KeyAlias)++"{}", Term});
        false ->
            error({"expected #"++atom_to_list(A)++"{}", Term})
    end;
encode_term(Term, #type{qname={_, N}}, _) ->
    error({"expected #"++N++"{}", Term});
encode_term(Term, #base{erl_type=Type, list=IsList}, _) ->
    case is_list(Term) of
        false ->
            [{txt, encode_single_base(Term, Type)}];
        true when IsList ->
            ListParts = [ encode_single_base(T, Type) || T <- Term ],
            [{txt, string:join(ListParts, " ")}];
        true ->
            error({"expected non-list "++atom_to_list(Type), Term})
    end;
encode_term(Term, #enum{values=Values, list=IsList}, _) ->
    case is_list(Term) of
        true when IsList ->
            ListParts = [ encode_single_enum(T, Values) || T <- Term ],
            [{txt, string:join(ListParts, " ")}];
        true ->
            Accept = string:join([ atom_to_list(A) || {A,_} <- Values ], " | "),
            error({"expected non-list "++Accept, Term});
        false ->
            [{txt, encode_single_enum(Term, Values)}]
    end.

encode_single_base(Term, BaseType) ->
    case BaseType of
        string when is_binary(Term); is_list(Term) ->
            Term;
        integer when is_integer(Term) ->
            integer_to_list(Term);
        float when is_float(Term) ->
            float_to_list(Term);
        boolean when is_boolean(Term) ->
            atom_to_list(Term);
        _ ->
            error({"expected "++atom_to_list(BaseType), Term})
    end.

encode_single_enum(Term, Values) ->
    case lists:keyfind(Term, 1, Values) of
        false ->
            Accept = string:join([ atom_to_list(A) || {A,_} <- Values ], " | "),
            error({bad_term, Term, "expected one of: " ++ Accept});
        {Term, Value} ->
            Value
    end.

%% ---------------------------------------------------------------------------

validate_xml(undefined, #elem{meta=#meta{min=0, max=Max}}, _)
  when Max > 1 ->
    [];
validate_xml(undefined, #elem{meta=#meta{min=0}}, _) ->
    undefined;
validate_xml({Qname, _, _}=E, #elem{qname=Qname,type=Types}=ME, Tbl)
  when is_list(Types) ->
    TestType = fun (Type, undefined) ->
                       try
                           {ok, validate_xml(E, ME#elem{type=Type}, Tbl)}
                       catch
                           _:_ ->
                               undefined
                       end;
                   (_Type, Result) ->
                       Result
               end,
    {ok, Result} = lists:foldl(TestType, undefined, Types),
    Result;
validate_xml({Qname, _, _}=E, #elem{qname=Qname,meta=#meta{max=Max}}=ME, Tbl)
  when Max > 1 ->
    validate_xml([E], ME, Tbl);
validate_xml({Qname, As, Cs}, #elem{qname=Qname,type={_,_}=TypeKey}, Tbl) ->
    case has_inherited_type(As, Tbl, TypeKey) of
        #type{} = Type ->
            validate_xml({Qname, As, Cs}, Type, Tbl);
        false ->
            Type = ews_model:get(TypeKey, Tbl),
            validate_xml({Qname, As, Cs}, Type, Tbl)
    end;
validate_xml([{Qname, _, _}|_]=Es, #elem{qname=Qname,
                                         type={_,_},
                                         meta=#meta{max=Max}=Meta}=ME,
             Tbl) when Max > 1 ->
    NewME = ME#elem{meta=Meta#meta{max=1}},
    [validate_xml(E, NewME, Tbl) || E <- Es];
validate_xml({Qname, As, Cs}, #elem{qname=Qname,type=Type}, Tbl) ->
    validate_xml({Qname, As, Cs}, Type, Tbl);
validate_xml([{Qname, _, _}|_]=Es, #elem{qname=Qname,type=Type}, Tbl) ->
    validate_xml(Es, Type, Tbl);
validate_xml(Es, Type, Tbl) when is_list(Es) ->
    [ validate_xml(E, Type, Tbl) || E <- Es ];
validate_xml({_, As, []}, #type{}, _Tbl) ->
    %% This is broken, an empty type that shouldn't be.
    case is_nil(As) of
        true ->
            nil;
        false ->
            undefined
    end;
validate_xml({_, As, Cs}, #type{qname=Key, alias=Alias}, Tbl) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            Elems = case has_inherited_type(As, Tbl, Key) of
                        false ->
                            ews_model:get_parts(Key, Tbl);
                        #type{qname=InheritedKey} ->
                            ews_model:get_parts(InheritedKey, Tbl)
                    end,
            Pairs = match_children_elems(Cs, Elems, [], []),
            ValidatedXml =[ validate_xml(T, E, Tbl) || {T, E} <- Pairs ],
            list_to_tuple([Alias | ValidatedXml])
    end;
validate_xml({_Qname, As, []}, #base{}, _) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            undefined
    end;
validate_xml({_Qname, _, [{txt, Txt}]}, #base{erl_type=Type}, _) ->
    case catch to_base(Txt, Type) of
        {'EXIT', _Reason} ->
            error({"failed to convert base", {Txt, Type}});
        Term ->
            Term
    end;
validate_xml({_Qname, As, []}, #enum{}, _) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            undefined
    end;
validate_xml({_Qname, _, [{txt, Txt}]}, #enum{values=Vs}, _) ->
    Str = binary_to_list(Txt),
    case lists:keyfind(Str, 2, Vs) of
        false ->
            error({"failed to convert enum", {Txt, Vs}});
        {V, _} ->
            V
    end.

%% TODO: Fix Max > 1 terms are bunched into a list
%% TODO: Just start by matching pairs together, maybe not even check meta now,
%%       but after all terms that conform to the same Qname have been bunched
match_children_elems([{Qname,_,_}=C1, {Qname,_,_}=C2|Cs],
                     [#elem{qname=Qname}=E|Es], Acc, Res) ->
    match_children_elems([C2|Cs], [E|Es], [C1|Acc], Res);
match_children_elems([{Qname,_,_}=C1|Cs],
                     [#elem{qname=Qname}=E|Es], [], Res) ->
    match_children_elems(Cs, Es, [], [{C1,E}|Res]);
match_children_elems([{Qname,_,_}=C1|Cs],
                     [#elem{qname=Qname}=E|Es], [{Qname,_,_}|_]=Acc, Res) ->
    match_children_elems(Cs, Es, [], [{lists:reverse([C1|Acc]),E}|Res]);
match_children_elems([{Qname,_,_}=C1|Cs],
                     [#elem{qname=Qname}=E|Es], Acc, Res) ->
    match_children_elems([C1|Cs], Es, [], [{lists:reverse(Acc),E}|Res]);
match_children_elems([{_,_,_}=C|Cs],
                     [#elem{meta=#meta{min=0}}=E|Es], Acc, Res) ->
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

has_inherited_type(Attributes, Tbl, TypeKey) ->
    case lists:keyfind({?SCHEMA_INSTANCE_NS, "type"}, 1, Attributes) of
        {_, TypeBase} ->
            case lists:member($:, TypeBase) of
                true ->
                    [_, Base] = string:tokens(TypeBase, ":"),
                    get_from_base(Base, Tbl, TypeKey);
                false ->
                    get_from_base(TypeBase, Tbl, TypeKey)
            end;
        false ->
            false
    end.

get_from_base(Base, Tbl, TypeKey) ->
    case ews_model:get_from_base(Base, Tbl) of
        false ->
            false;
        Candidates ->
            case [T || {_, T} <- Candidates, T#type.extends == TypeKey] of
                [Type] ->
                    Type;
                [] ->
                    false
            end
    end.

field_to_map(V, M) when is_tuple(V) ->
    record_to_map(V, M);
field_to_map(V, M) when is_list(V) ->
    [field_to_map(E, M) || E <- V];
field_to_map(V, _M) ->
    V.

field_names(Parts) ->
    [ews_alias:create(QN) || #elem{qname = QN} <- Parts].
