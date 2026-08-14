%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2013-2017 Campanja
%%% Copyright (c) 2017-2020 [24]7.ai
%%% Copyright (c) 2022-2025 Kivra
%%%
%%% Distribution subject to the terms of the LGPL-3.0-or-later, see
%%% the COPYING.LESSER file in the root of the distribution
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ews_serialize).

-export([ encode/3
        , encode_non_root/3
        , compile_non_root/2
        , compile_elem_plan/2
        , encode_compiled/2
        , decode_compiled/2
        , decode/3
        , record_to_map/2
        ]).

-include("ews.hrl").
-include("ews_plan.hrl").
-include_lib("ews/include/ews.hrl").

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

encode_non_root(Term, MsgElem, #model{type_map=Tbl}) ->
    case ews_model:is_root(MsgElem, Tbl) of
        false ->
            %% If the record isn't a root element but instead a type,
            %% we have lost the parent element at this stage.
            %% So instead we create an element from the type, similarly
            %% to what libraries in other languages do.
            {Ns, TypeName} = MsgElem,
            %% If this is an unnamed type inside an element remove the
            %% internal ews notation in front of the @ sign.
            Elem = case string:lexemes(TypeName, "@") of
                       [_ParentElem, TypePart] ->
                           {Ns, TypePart};
                       [TypePart] ->
                           {Ns, TypePart}
                   end,
            Type = ews_model:get(MsgElem, Tbl),
            %% This creates an element with the type's name.
            [{Elem, [], encode_term(Term, Type, Tbl)}];
        true ->
            BaseElem = ews_model:get_elem(MsgElem, Tbl),
            [ encode_term(Term, BaseElem, Tbl) ]
    end.

%%%===================================================================
%%% Precompiled encoder (upb-198)
%%%
%%% encode_non_root/3 -> encode_term/3 re-run the same ews_model ETS
%%% lookups (get / get_super / get_parts / get_elem / get_subs / is_root)
%%% for *every* record. For a homogeneous list those results never change,
%%% and profiling batch encoding in a client application showed ~2/3 of
%%% the time spent in those lookups (dominated by the unkeyed ets:match
%%% in get_from_alias).
%%%
%%% compile_non_root/2 walks the model ONCE and resolves every lookup into
%%% an explicit tree of plan records (see ews_plan.hrl) -- NOT closures, so
%%% the plan is human-inspectable and can be reused to drive decoding too.
%%% encode_compiled/2 traverses that tree with a record and touches no ETS;
%%% it only runs the pure leaf/attr encoders and make_xml/3, so the produced
%%% term (and thus the XML) is byte-identical to encode_non_root/3.
%%%
%%% Anything the compiler cannot statically resolve -- type unions/choices,
%%% polymorphic subtypes (xsi:type), inline/simple element types, or an
%%% unexpected record tag at runtime -- becomes a #pfallback{} that defers to
%%% the runtime encode_term/3, so correctness is preserved; only the fast,
%%% common path is specialised.
%%%===================================================================

%% @doc Build a reusable plan for records whose root corresponds to the
%%      element/type named by MsgElem (a qname). Returns a #pdoc{} tree;
%%      apply it with encode_compiled/2.
-spec compile_non_root({string(), string()}, #model{}) -> #pdoc{}.
compile_non_root({_, _} = MsgElem, #model{type_map = Tbl}) ->
    case ews_model:is_root(MsgElem, Tbl) of
        false ->
            {Ns, TypeName} = MsgElem,
            Elem = case string:lexemes(TypeName, "@") of
                       [_ParentElem, TypePart] -> {Ns, TypePart};
                       [TypePart]              -> {Ns, TypePart}
                   end,
            TypeNode = compile_type(ews_model:get(MsgElem, Tbl), Tbl),
            #pdoc{mode = typed, node = TypeNode, elem_qname = Elem, tbl = Tbl};
        true ->
            Node = compile_elem(ews_model:get_elem(MsgElem, Tbl), Tbl),
            #pdoc{mode = root, node = Node, tbl = Tbl}
    end.

-doc """
Build a reusable decode plan for a single `#elem{}`, e.g. the
repeated child element of a container that is being streamed with
`ews_stream`. The element is compiled with cardinality one, so
applying the plan with `decode_compiled/2` to one xml term returns
one record. Elements the compiler cannot specialise fall back to the
runtime decoder, exactly as in `compile_non_root/2`.
""".
-spec compile_elem_plan(Elem :: #elem{}, Model :: #model{}) -> #pdoc{}.
compile_elem_plan(#elem{meta = Meta0} = Elem, #model{type_map = Tbl}) ->
    Meta = case Meta0 of
               #meta{} -> Meta0#meta{min = 1, max = 1};
               undefined -> #meta{min = 1, max = 1}
           end,
    Elem1 = Elem#elem{meta = Meta},
    #pdoc{mode = root, node = compile_elem(Elem1, Tbl), tbl = Tbl}.

%% A complex type -> #ptype{}.
compile_type(#type{qname = Key, alias = Alias, attrs = PossAttrs}, Tbl) ->
    Fields = [ compile_part(P, Tbl) || P <- ews_model:get_parts(Key, Tbl) ],
    #ptype{qname = Key, tag = Alias, fields = Fields, attrs = PossAttrs}.

%% A part of a complex type: an own element (#elem) or simpleContent (#sc).
compile_part(#elem{} = Elem, Tbl) ->
    compile_elem(Elem, Tbl);
compile_part(#sc{type = Type}, _Tbl) ->
    #psc{leaf = #pleaf{model = Type}}.

%% An element whose type is a qname referring to a monomorphic complex type
%% is specialised into a #pelem{}; everything else becomes a #pfallback{}.
compile_elem(#elem{qname = Qname, type = {_, _} = TypeKey, meta = Meta} = Elem,
             Tbl) ->
    case ews_model:get(TypeKey, Tbl) of
        #type{} = DeclaredType ->
            case ews_model:get_subs(TypeKey, Tbl) of
                [] ->
                    #pelem{qname = Qname,
                           card  = card(Meta),
                           type  = compile_type(DeclaredType, Tbl),
                           orig  = Elem};
                _Subtypes ->
                    %% polymorphism / xsi:type -> keep runtime dispatch
                    #pfallback{elem = Elem}
            end;
        _NotAComplexType ->
            #pfallback{elem = Elem}
    end;
compile_elem(#elem{} = Elem, _Tbl) ->
    #pfallback{elem = Elem}.

%% Cardinality: max > 1 means the field value is a list whose members each
%% encode to their own element.
card(#meta{max = Max}) when Max =:= infinite; (is_integer(Max) andalso Max > 1) ->
    many;
card(_Meta) ->
    single.

%%%-------------------------------------------------------------------
%%% Applying a compiled plan (encode direction)
%%%-------------------------------------------------------------------

%% @doc Encode one record with a #pdoc{} plan from compile_non_root/2.
%%      Returns a body that can be handed straight to ews_soap:make_xml/1.
-spec encode_compiled(#pdoc{}, tuple()) -> [term()].
encode_compiled(#pdoc{mode = root, node = Node, tbl = Tbl}, Term) ->
    [ enc_elem(Node, Term, Tbl) ];
encode_compiled(#pdoc{mode = typed, node = Node, elem_qname = Q, tbl = Tbl},
                Term) ->
    [ {Q, [], enc_type(Node, Term, Tbl)} ].

%% An element node against a field value.
enc_elem(#pelem{qname = Q, card = many, type = T, orig = O}, Values, Tbl)
  when is_list(Values) ->
    [ enc_elem_single(Q, T, V, O, Tbl) || V <- Values ];
enc_elem(#pelem{qname = Q, type = T, orig = O}, Value, Tbl) ->
    enc_elem_single(Q, T, Value, O, Tbl);
enc_elem(#pfallback{elem = Elem}, Value, Tbl) ->
    encode_term(Value, Elem, Tbl).

enc_elem_single(_Q, _T, Term, Orig, Tbl) when not is_tuple(Term) ->
    %% nil / [] / list-of-values etc. -> let the runtime handle it
    encode_term(Term, Orig, Tbl);
enc_elem_single(Q, #ptype{tag = Tag} = T, Term, _Orig, Tbl)
  when element(1, Term) =:= Tag ->
    case enc_type(T, Term, Tbl) of
        {Attrs, Children} -> {Q, Attrs, Children};   %% make_xml/3 attr merge
        Children          -> {Q, [], Children}
    end;
enc_elem_single(_Q, _T, Term, Orig, Tbl) ->
    %% record tag did not match the specialised type -> runtime fallback
    encode_term(Term, Orig, Tbl).

%% A complex type node against a record.
enc_type(#ptype{fields = Fields, attrs = []}, Term, Tbl) ->
    [_Name | Values] = tuple_to_list(Term),
    enc_fields(Fields, Values, Tbl);
enc_type(#ptype{qname = Key, fields = Fields, attrs = [_ | _] = PossAttrs},
         Term, Tbl) ->
    [_Name, Attrs | Values] = tuple_to_list(Term),
    {encode_attributes(Attrs, PossAttrs, Key),
     enc_fields(Fields, Values, Tbl)}.

enc_fields(Fields, Values, Tbl) ->
    lists:flatten([ enc_field(F, V, Tbl)
                    || {F, V} <- lists:zip(Fields, Values), V =/= undefined ]).

enc_field(#pelem{} = E, V, Tbl)      -> enc_elem(E, V, Tbl);
enc_field(#psc{leaf = Leaf}, V, Tbl) -> enc_leaf(Leaf, V, Tbl);
enc_field(#pfallback{elem = E}, V, Tbl) -> encode_term(V, E, Tbl).

%% Leaves reuse the runtime scalar/enum encoder (pure -- ignores Tbl) via the
%% embedded model record, guaranteeing identical output.
enc_leaf(#pleaf{model = Model}, V, Tbl) ->
    encode_term(V, Model, Tbl).

%%%-------------------------------------------------------------------
%%% Applying a compiled plan (decode direction)
%%%
%%% The SAME #pdoc{} tree drives decoding. Structural recursion is guided by
%%% the plan (so no ews_model:get / get_parts per element); the pure runtime
%%% helpers (match_children_elems / validate_attrs / validate_xml on leaves)
%%% are reused so the produced record is identical to decode/3. Nil elements,
%%% xsi:type polymorphism, and any unexpected shape defer to validate_xml via
%%% the embedded original #elem/type, preserving correctness.
%%%-------------------------------------------------------------------

%% @doc Decode a parsed xml term ([{Qname, Attrs, Children}]) with a #pdoc{}
%%      plan from compile_non_root/2. Returns the erlang record.
-spec decode_compiled(#pdoc{}, [tuple()]) -> term().
decode_compiled(#pdoc{mode = root, node = Node, tbl = Tbl}, [{_, _, _} = Xml]) ->
    dec_node(Node, Xml, Tbl);
decode_compiled(#pdoc{mode = typed, node = #ptype{} = PT, tbl = Tbl},
                [{_, _, _} = Xml]) ->
    dec_type(PT, Xml, Tbl).

dec_node(#pelem{} = P, Xml, Tbl)      -> dec_elem_single(P, Xml, Tbl);
dec_node(#pfallback{elem = E}, Xml, Tbl) -> validate_xml(Xml, E, Tbl).

%% Decode one xml element into a record. Polymorphic (xsi:type) elements defer
%% to the runtime, which resolves the concrete subtype from the model. Callers
%% (dec_node/3, dec_field/3) handle #pfallback{} themselves, so this only ever
%% sees a #pelem{} (whose type is always a #ptype{}, per compile_elem/2).
dec_elem_single(#pelem{type = #ptype{} = PT, orig = Orig}, {_, As, _} = Xml,
                Tbl) ->
    case has_xsi_type(As) of
        true  -> validate_xml(Xml, Orig, Tbl);
        false -> dec_type(PT, Xml, Tbl)
    end.

%% simpleContent with attributes -> {Tag, AttrsMap, Value}.
dec_type(#ptype{tag = Alias,
                fields = [#psc{leaf = #pleaf{model = BaseOrEnum}}],
                attrs = [_ | _] = PossAttrs},
         {_, As, _} = In, Tbl) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            Sc = validate_xml(In, BaseOrEnum, Tbl),
            list_to_tuple([Alias, validate_attrs(As, PossAttrs, #{}), Sc])
    end;
%% A present-but-empty element has several shape-dependent results
%% (undefined / {Alias} / {Alias, undefined} / attrs-only record); defer
%% to the runtime decoder for exact parity with decode/3.
dec_type(#ptype{qname = Key}, {_, _, []} = In, Tbl) ->
    validate_xml(In, ews_model:get(Key, Tbl), Tbl);
%% Complex type with child elements.
dec_type(#ptype{tag = Alias, qname = Key, fields = Fields, attrs = PossAttrs},
         {ElemQ, As, Cs} = In, Tbl) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            try
                {ok, Elems} = fields_elems(Fields),
                Pairs  = match_children_elems(Cs, Elems, [], []),
                Values = dec_pairs(Fields, Pairs, Tbl),
                case PossAttrs of
                    [] ->
                        list_to_tuple([Alias | Values]);
                    _ ->
                        list_to_tuple(
                          [Alias, validate_attrs(As, PossAttrs, #{}) | Values])
                end
            catch
                _:_ ->
                    %% unexpected shape (empty/nil/grouping edge case) ->
                    %% re-derive from the model via a synthetic element
                    validate_xml(In, #elem{qname = ElemQ, type = Key}, Tbl)
            end
    end.

%% The original #elem{} for each field, in element order; error if a field is
%% not element-shaped (so the caller falls back to the runtime decoder).
fields_elems(Fields) ->
    fields_elems(Fields, []).

fields_elems([#pelem{orig = E} | T], Acc)    -> fields_elems(T, [E | Acc]);
fields_elems([#pfallback{elem = E} | T], Acc) -> fields_elems(T, [E | Acc]);
fields_elems([_ | _], _Acc)                  -> error;
fields_elems([], Acc)                        -> {ok, lists:reverse(Acc)}.

%% Fields and match pairs are both in element order and 1:1; a mismatch throws
%% and the caller falls back to validate_xml.
dec_pairs([F | Fs], [{T, _E} | Ps], Tbl) ->
    [ dec_field(F, T, Tbl) | dec_pairs(Fs, Ps, Tbl) ];
dec_pairs([], [], _Tbl) ->
    [];
dec_pairs(_, _, _) ->
    throw(pair_mismatch).

dec_field(#pelem{card = many} = P, T, Tbl) ->
    [ dec_elem_single(P, X, Tbl) || X <- norm_many(T) ];
dec_field(#pelem{card = single}, undefined, _Tbl) ->
    undefined;
dec_field(#pelem{card = single} = P, T, Tbl) ->
    dec_elem_single(P, T, Tbl);
dec_field(#pfallback{elem = E}, T, Tbl) ->
    validate_xml(T, E, Tbl).

%% match_children_elems bunches repeated children into a list, but leaves a
%% single occurrence as one element; undefined means the optional child was
%% absent.
norm_many(undefined)           -> [];
norm_many(L) when is_list(L)   -> L;
norm_many({_, _, _} = One)     -> [One].

has_xsi_type(As) ->
    lists:keymember({?SCHEMA_INSTANCE_NS, "type"}, 1, As).

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
    case ews_model:get(Alias, Tbl) of
        #type{attrs = [_|_]} ->
            [AttrsVal | ElemValues] = Values,
            MapValues = lists:map(fun (V) -> field_to_map(V, M) end,
                                  ElemValues),
            AttrsList =
                case AttrsVal of
                    undefined -> [];
                    _ -> [{'__attrs', AttrsVal}]
                end,
            maps:from_list(
              AttrsList ++
              [{K, V} || {K, V} <- lists:zip(FieldNames, MapValues),
                          V /= undefined]);
        #type{attrs = []} ->
            MapValues = lists:map(fun (V) -> field_to_map(V, M) end, Values),
            maps:from_list(
              [{K, V} || {K, V} <- lists:zip(FieldNames, MapValues),
                          V /= undefined])
    end.
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
encode_term(Term, #elem{qname=Qname, type={_,_}=TypeKey}, Tbl) when is_tuple(Term) ->
    [Name|_] = tuple_to_list(Term),
    #type{qname=InheritedTypeKey}= InheritedType = ews_model:get(Name, Tbl),
    SuperKey = ews_model:get_super(Name, Tbl),
    case TypeKey of
        InheritedTypeKey ->
            make_xml(Qname, [], encode_term(Term, InheritedType, Tbl));
        SuperKey ->
            TypeDecl = {{?SCHEMA_INSTANCE_NS, "type"}, InheritedTypeKey},
            Super = ews_model:get(SuperKey, Tbl),
            make_xml(Qname, [TypeDecl], encode_term(Term, Super, Tbl))
    end;
encode_term([], #elem{qname=_Qname, type={_,_}, meta=#meta{min=0}}, _Tbl) ->
    [];
encode_term(Term, #elem{qname=Qname, type=Type}, Tbl) ->
    {Qname, [], encode_term(Term, Type, Tbl)};
encode_term(Term, #type{qname=Key, alias=A, attrs=[]}, Tbl) when is_tuple(Term) ->
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
encode_term(Term, #type{qname=Key, alias=A, attrs=[_|_]=PossAttrs}, Tbl)
  when is_tuple(Term) ->
    %% logger:notice("PossAttrs: ~p~n", [PossAttrs]),
    [Name, Attrs|Values] = tuple_to_list(Term), %% TODO: Move this one clause up
    %% logger:notice("Attrs: ~p~n", [Attrs]),
    EncAttrs = encode_attributes(Attrs, PossAttrs, Key),
    Super = ews_model:get_super(Name, Tbl),
    case ews_model:get(Name, Tbl) of
        #type{qname=InheritedKey} when Super == Key ->
            Elems = ews_model:get_parts(InheritedKey, Tbl),
            Parts = lists:zip(Values, Elems),
            {EncAttrs,
             lists:flatten([ encode_term(V, E, Tbl) ||
                               {V, E} <- Parts, V /= undefined ])};
        #type{qname=Key} ->
            Elems = ews_model:get_parts(Key, Tbl),
            Parts = lists:zip(Values, Elems),
            {EncAttrs,
             lists:flatten([ encode_term(V, E, Tbl) ||
                               {V, E} <- Parts, V /= undefined ])};
        #type{qname=_Qname} ->
            #type{alias=KeyAlias} = ews_model:get(Key, Tbl),
            error({"expected #"++atom_to_list(KeyAlias)++"{}", Term});
        false ->
            error({"expected #"++atom_to_list(A)++"{}", Term})
    end;
encode_term(Term, #type{qname={_, N}}, _) ->
    error({"expected #"++N++"{}", Term});
encode_term(Term, #sc{type=Type}, Tbl) ->
    encode_term(Term, Type, Tbl);
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
encode_term(Term, #enum{values=Values, list=IsList, type=Type}, _) ->
    case is_list(Term) of
        true when IsList ->
            ListParts = [ encode_single_enum(T, Values, Type) || T <- Term ],
            [{txt, string:join(ListParts, " ")}];
        true ->
            Accept = string:join([ atom_to_list(A) || {A,_} <- Values ], " | "),
            error({"expected non-list "++Accept, Term});
        false ->
            [{txt, encode_single_enum(Term, Values, Type)}]
    end.

%% If the type had attributes we have to add them to the attributes
%% of this element.
make_xml(Name, Typedef, {Attrs, Elems}) ->
  {Name, Typedef++Attrs, Elems};
make_xml(Name, Typedef, Elems) ->
  {Name, Typedef, Elems}.

encode_attributes(Attrs, PossAttrs, Name) when is_map(Attrs) ->
    do_encode_attributes(maps:to_list(Attrs), PossAttrs, Name, []);
encode_attributes(undefined, _, _) ->
    [].

do_encode_attributes([Attr | Tail], PossAttrs, Name, Acc) ->
    %% logger:notice("encoding: PossAttrs: ~tp~nAttr: ~tp~nName: ~tp~n",
    %%               [PossAttrs, Attr, Name]),
    EncAttr = encode_attr(PossAttrs, Attr, Name),
    do_encode_attributes(Tail, PossAttrs, Name, [EncAttr | Acc]);
do_encode_attributes([], _, _, Acc) ->
    lists:sort(Acc).

encode_attr(Attrs, {Id, Value}, Name) when is_atom(Id) ->
    encode_attr(Attrs, {atom_to_list(Id), Value}, Name);
encode_attr([#attribute{name = {_, Id}, base = BaseType} | _Tail],
            {Id, Value}, _Name) ->
    [{txt, Enc}] = encode_term(Value, BaseType, noarg),
    {Id, Enc};
encode_attr([#attribute{name = Id, base = BaseType} | _Tail],
            {Id, Value}, _Name) ->
    [{txt, Enc}] = encode_term(Value, BaseType, noarg),
    {Id, Enc};
encode_attr([#attribute{name = _} | Tail], {Id, Value}, Name) ->
    encode_attr(Tail, {Id, Value}, Name);
encode_attr([], {Id, _Value}, Name) ->
    error({unexpected_attribute_for_type, Id, Name}).

encode_single_base(Term, BaseType) ->
    case BaseType of
        string when is_binary(Term) ->
            Term;
        string when is_list(Term) ->
            unicode:characters_to_binary(Term, utf8);
        integer when is_integer(Term) ->
            integer_to_list(Term);
        float when is_float(Term) ->
            float_to_list(Term);
        boolean when is_boolean(Term) ->
            atom_to_list(Term);
        _ ->
            error({"expected "++atom_to_list(BaseType), Term})
    end.

encode_single_enum(Term, Values, #base{erl_type=ErlType}) ->
    case lists:keyfind(Term, 1, Values) of
        false ->
            Accept = string:join([ atom_to_list(A) || {A,_} <- Values ], " | "),
            error({bad_term, Term, "expected one of: " ++ Accept});
        {Term, Value} ->
            encode_single_base(Value, ErlType)
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
validate_xml({Name, As, Cs}, #elem{qname={_,Name},type={_,_}=TypeKey}, Tbl) ->
    case has_inherited_type(As, Tbl, TypeKey) of
        #type{} = Type ->
            validate_xml({Name, As, Cs}, Type, Tbl);
        false ->
            Type = ews_model:get(TypeKey, Tbl),
            validate_xml({Name, As, Cs}, Type, Tbl)
    end;
validate_xml({{_, Name}, As, Cs}, #elem{qname=Name, type={_,_}=TypeKey}, Tbl)
  when is_list(Name) ->
    case has_inherited_type(As, Tbl, TypeKey) of
        #type{} = Type ->
            validate_xml({Name, As, Cs}, Type, Tbl);
        false ->
            Type = ews_model:get(TypeKey, Tbl),
            validate_xml({Name, As, Cs}, Type, Tbl)
    end;
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
validate_xml({Name, As, Cs}, #elem{qname={_,Name},type=Type}, Tbl) ->
    validate_xml({Name, As, Cs}, Type, Tbl);
validate_xml({{_, Name}, As, Cs}, #elem{qname=Name, type=Type}, Tbl)
  when is_list(Name) ->
    validate_xml({Name, As, Cs}, Type, Tbl);
validate_xml({Qname, As, Cs}, #elem{qname=Qname,type=Type}, Tbl) ->
    validate_xml({Qname, As, Cs}, Type, Tbl);
validate_xml([{Qname, _, _}|_]=Es, #elem{qname=Qname,type={_,_}=TypeKey}, Tbl) ->
    Type = ews_model:get(TypeKey, Tbl),
    validate_xml(Es, Type, Tbl);
validate_xml([{Qname, _, _}|_]=Es, #elem{qname=Qname,type=Type}, Tbl) ->
    validate_xml(Es, Type, Tbl);
validate_xml(Es, Type, Tbl) when is_list(Es) ->
    [ validate_xml(E, Type, Tbl) || E <- Es ];
%% type validation, single elems below TOMAYBEDO: separate element and type validation
validate_xml({_, [_|_] = As, []}, #type{qname=Key, alias=Alias,
                                        attrs=[_|_]=PossAttrs}, Tbl) ->
    %% Empty element with possible attributes
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
            Pairs = match_children_elems([], Elems, [], []),
            ValidatedXml =[ validate_xml(T, E, Tbl) || {T, E} <- Pairs ],
            Attrs = validate_attrs(As, PossAttrs, #{}),
            list_to_tuple([Alias, Attrs | ValidatedXml])
    end;
validate_xml({_, [], []}, #type{alias=Alias, elems=[], attrs=[_|_]}, _Tbl) ->
    %% An element that should be empty, but with attributes
    %% Should become an empty record like this:
    %% -record(sausage, {'__attrs' :: #{a => binary()}}).
    %% return `{sausage, undefined}`
    {Alias, undefined};
validate_xml({_, [], []}, #type{alias=Alias, elems=[], attrs=[]}, _Tbl) ->
    %% An element that should be empty.
    %% Should become an empty record like this:
    %% -record(sausage, {}).
    %% return `{sausage}`
    {Alias};
validate_xml({_, As, []}, #type{}, _Tbl) ->
    %% This is broken, an empty type that shouldn't be.
    case is_nil(As) of
        true ->
            nil;
        false ->
            undefined
    end;
validate_xml({_, As, Cs}, #type{qname=Key, alias=Alias, attrs=[]}, Tbl) ->
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
validate_xml({_, As, Cs} = In,
             #type{qname=Key, alias=Alias, attrs=PossAttrs}, Tbl) ->
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
            case Elems of
                %% Special case of a simpleContent with attributes
                [#sc{type=BaseOrEnum}] ->
                    %% logger:notice("decode PossAttrs: ~tp~nAs: ~tp~n",
                    %%               [PossAttrs, As]),
                    Sc = validate_xml(In, BaseOrEnum, Tbl),
                    Attrs = validate_attrs(As, PossAttrs, #{}),
                    list_to_tuple([Alias, Attrs, Sc]);
                _ ->
                    Pairs = match_children_elems(Cs, Elems, [], []),
                    ValidatedXml =[ validate_xml(T, E, Tbl) || {T, E} <- Pairs ],
                    Attrs = validate_attrs(As, PossAttrs, #{}),
                    list_to_tuple([Alias, Attrs | ValidatedXml])
            end
    end;
validate_xml({_Qname, As, []}, #base{}, _) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            undefined
    end;
validate_xml({_Qname, _, [{txt, Txt}]}, #base{erl_type=Type}, _) ->
    try
        to_base(Txt, Type)
    catch
        error:_ ->
            error({"failed to convert base", {Txt, Type}})
    end;
validate_xml({_Qname, As, []}, #enum{}, _) ->
    case is_nil(As) of
        true ->
            nil;
        false ->
            undefined
    end;
validate_xml({_Qname, _, [{txt, Txt}]}, #enum{values=Vs}, _) ->
    Str = unicode:characters_to_list(Txt, utf8),
    case lists:keyfind(Str, 2, Vs) of
        false ->
            error({"failed to convert enum", {Txt, Vs}});
        {V, _} ->
            V
    end.

%% TODO: Fix Max > 1 terms are bunched into a list
%% TODO: Just start by matching pairs together, maybe not even check meta now,
%%       but after all terms that conform to the same Qname have been bunched
%% Each shape comes in three: the wire tag carries no namespace while the
%% model qname does, the two match exactly, or the wire tag is qualified while
%% the model qname is bare. Only the middle one is strictly right; the other
%% two let a server that disagrees with its own schema about element form
%% still be decoded.
match_children_elems([{Name,_,_}=C1, {Name,_,_}=C2|Cs],
                     [#elem{qname={_,Name}}=E|Es], Acc, Res) ->
    match_children_elems([C2|Cs], [E|Es], [C1|Acc], Res);
match_children_elems([{Qname,_,_}=C1, {Qname,_,_}=C2|Cs],
                     [#elem{qname=Qname}=E|Es], Acc, Res) ->
    match_children_elems([C2|Cs], [E|Es], [C1|Acc], Res);
match_children_elems([{{_,Name},_,_}=C1, {{_,Name},_,_}=C2|Cs],
                     [#elem{qname=Name}=E|Es], Acc, Res) when is_list(Name) ->
    match_children_elems([C2|Cs], [E|Es], [C1|Acc], Res);
match_children_elems([{Name,_,_}=C1|Cs],
                     [#elem{qname={_,Name}}=E|Es], [], Res) ->
    match_children_elems(Cs, Es, [], [{C1,E}|Res]);
match_children_elems([{Qname,_,_}=C1|Cs],
                     [#elem{qname=Qname}=E|Es], [], Res) ->
    match_children_elems(Cs, Es, [], [{C1,E}|Res]);
match_children_elems([{{_,Name},_,_}=C1|Cs],
                     [#elem{qname=Name}=E|Es], [], Res) when is_list(Name) ->
    match_children_elems(Cs, Es, [], [{C1,E}|Res]);
match_children_elems([{Name,_,_}=C1|Cs],
                     [#elem{qname={_,Name}}=E|Es], [{Name,_,_}|_]=Acc, Res) ->
    match_children_elems(Cs, Es, [], [{lists:reverse([C1|Acc]),E}|Res]);
match_children_elems([{Qname,_,_}=C1|Cs],
                     [#elem{qname=Qname}=E|Es], [{Qname,_,_}|_]=Acc, Res) ->
    match_children_elems(Cs, Es, [], [{lists:reverse([C1|Acc]),E}|Res]);
match_children_elems([{{_,Name},_,_}=C1|Cs],
                     [#elem{qname=Name}=E|Es], [{{_,Name},_,_}|_]=Acc, Res)
  when is_list(Name) ->
    match_children_elems(Cs, Es, [], [{lists:reverse([C1|Acc]),E}|Res]);
match_children_elems([{Name,_,_}=C1|Cs],
                     [#elem{qname={_,Name}}=E|Es], Acc, Res) ->
    match_children_elems([C1|Cs], Es, [], [{lists:reverse(Acc),E}|Res]);
match_children_elems([{Qname,_,_}=C1|Cs],
                     [#elem{qname=Qname}=E|Es], Acc, Res) ->
    match_children_elems([C1|Cs], Es, [], [{lists:reverse(Acc),E}|Res]);
match_children_elems([{{_,Name},_,_}=C1|Cs],
                     [#elem{qname=Name}=E|Es], Acc, Res) when is_list(Name) ->
    match_children_elems([C1|Cs], Es, [], [{lists:reverse(Acc),E}|Res]);
match_children_elems([{_,_,_}=C|Cs],
                     [#elem{meta=#meta{min=0}}=E|Es], Acc, Res) ->
    match_children_elems([C|Cs], Es, Acc, [{undefined,E}|Res]);
match_children_elems([{Qname,_,_}|_], [#elem{qname={_,N}}|_], _, _) ->
    error({"expected "++N, Qname});
match_children_elems([{Qname,_,_}|_], [#elem{qname=N}|_], _, _)
  when is_list(N) ->
    error({"expected "++N, Qname});
match_children_elems([], [], [], Res) ->
    lists:reverse(Res);
match_children_elems([], [#elem{meta=#meta{min=0}}=E|Es], [], Res) ->
    match_children_elems([], Es, [], [{undefined,E}|Res]);
match_children_elems([], [#elem{meta=#meta{min=0}}=E|Es], Acc, Res) ->
    match_children_elems([], Es, [], [{undefined,E},lists:reverse(Acc)|Res]);
match_children_elems([], [#elem{qname=Name,meta=#meta{min=_N}}|_], _, _) ->
    error({missing_non_optional_element, Name});
match_children_elems([], [], Acc, Res) ->
    [lists:reverse(Acc) | lists:reverse(Res)].

validate_attrs([{?SCHEMA_INSTANCE_NS, _} | As], PossAttrs, Acc) ->
    validate_attrs(As, PossAttrs, Acc);
validate_attrs([{Name, Value} | As], PossAttrs, Acc) ->
    Attr =
        case {[ P || #attribute{name = {_, N}} = P <- PossAttrs, N == Name ],
              [ P || #attribute{name = N} = P <- PossAttrs, N == Name ]} of
            {[], []} -> undefined;
            {[#attribute{} = A], _} -> A;
            {_, [#attribute{} = A]} -> A
        end,
    case Attr of
        undefined ->
            logger:notice("Unexpected attribute: ~p~n", [Name]),
            validate_attrs(As, PossAttrs, Acc);
        #attribute{base = BaseOrEnum} ->
            %% By compiling with debug_info the typespec in the records should
            %% load all possible atoms.
            case BaseOrEnum of
                #base{erl_type=ErlType} ->
                    NameAtom = list_to_existing_atom(Name),
                    ValueBase = to_base(list_to_binary(Value), ErlType),
                    validate_attrs(As, PossAttrs, Acc#{NameAtom => ValueBase});
                #enum{values=Vs} ->
                    case lists:keyfind(Value, 2, Vs) of
                        false ->
                            error({"failed to convert enum", {Value, Vs}});
                        {V, _} ->
                            NameAtom = list_to_existing_atom(Name),
                            validate_attrs(As, PossAttrs,
                                           Acc#{NameAtom => V})
                    end
            end
    end;
validate_attrs([], _, Acc) ->
    Acc.

to_base(Txt, string) when is_binary(Txt) -> Txt;
to_base(Txt, string) when is_list(Txt) ->
    unicode:characters_to_binary(Txt, unicode, utf8);
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
    try
        list_to_float(Str)
    catch
        error:badarg ->
            try
                Int = list_to_integer(Str),
                float(Int)
            catch
                error:badarg ->
                    Str
            end
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
    lists:filtermap(fun(#elem{qname = QN}) -> {true, ews_alias:create(QN)};
                       (#sc{qname = QN})   -> {true, ews_alias:create(QN)};
                       (_)                 -> false
                    end, Parts).
