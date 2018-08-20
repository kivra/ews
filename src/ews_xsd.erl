%%% ---------------------------------------------------------------------------
%%% WSDL Types parsing
%%% Doesn't support: group, attributeGroup, include and notation types
%%%                  unique, key or keyref
%%% FIXME: Merge elements and types.
%%% ---------------------------------------------------------------------------

-module(ews_xsd).

-export([parse_schema/2]).

-export([print_schema_stats/1]).

-include("ews.hrl").

-define(HTTP_OPTS, [{connect_options, [{versions, [tlsv1, sslv3]}]}]).

%% ----------------------------------------------------------------------------
%% Api

parse_schema(Schema, {Acc, Model}) when is_atom(Model) ->
    Schemas = get_all_schemas(Schema),
%%  [ print_schema_stats(S) || {_, _, S} <- Schemas ],
    PrSchemas = [ #schema{namespace=Ns,
                          url=Url,
                          types=parse_types(S)} || {Ns, Url, S} <- Schemas ],
    NewTypes = process(propagate_namespaces(PrSchemas), Model),
    {ews_model:append_model(Acc, NewTypes, Model), Model}.

%% ----------------------------------------------------------------------------
%% Import schema functions

get_all_schemas(TopSchema) ->
    Namespace = wh:get_attribute(TopSchema, targetNamespace),
    Input = {Namespace, undefined, TopSchema},
    AllSchemas = lists:flatten(get_all_schemas(Input, [])),
    lists:ukeysort(1, AllSchemas).

get_all_schemas({Ns, Base, Schema}, Acc) ->
    case find_imports(Schema) of
        [] ->
            [{Ns, Base, Schema} | Acc];
        Imports ->
            ImpSchemas = [ {ImpNs, Url, import_schema(Url)} ||
                             {ImpNs, Url} <- Imports,
                             Url /= undefined,
                             not lists:keymember(ImpNs, 1, Acc) ],
            [{Ns, Base, Schema} |
             [ get_all_schemas(S, Acc++ImpSchemas) || S <- ImpSchemas ]]
    end.

find_imports(Schema) ->
    Imports = wh:get_children(Schema, "import"),
    [ {wh:get_attribute(I, namespace),
       wh:get_attribute(I, schemaLocation)} || I <- Imports ].

import_schema(SchemaUrl) ->
    {ok, Bin} = request_cached(SchemaUrl),
    {Schema, _} = xmerl_scan:string(binary_to_list(Bin),
                                    [{space, normalize},
                                     {namespace_conformant, true},
                                     {validation, schema}]),
    Schema.

request_cached(SchemaUrl) ->
    CacheDir = application:get_env(ews, cache_base_dir, "priv"),
    File = filename:join([CacheDir, "xsds", escape_slash(SchemaUrl)]),
    ok = filelib:ensure_dir(File),
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, Bin};
        {error, Error} ->
            case lhttpc:request(SchemaUrl, get, [], [], 10000, ?HTTP_OPTS) of
                {ok, {{200, _}, _, Bin}} ->
                    ok = file:write_file(File, Bin),
                    {ok, Bin};
                {ok, {{_, _}, _, Bin}} ->
                    {error, Bin};
                {error, Error} ->
                    {error, Error}
            end
    end.

escape_slash([]) -> [];
escape_slash([$/ | Rest]) -> [$- | escape_slash(Rest)];
escape_slash([C | Rest]) -> [C | escape_slash(Rest)].

%% ----------------------------------------------------------------------------
%% Parse schema functions

%% TODO: Handle includes
parse_types(Schema) ->
    Elements = wh:get_all_child_elements(Schema),
    Types = [ parse_type(E) || E = #xmlElement{} <- Elements ],
    [ T || T <- Types, T /= import, T /= include ].

%% FIXME: Must handle import/any/anyAttribute/group/attributeGroup/notation/
%%                    appinfo/documentation/field/key/keyref/selector/unique
parse_type(undefined) ->
    undefined;
parse_type(#xmlElement{} = Type) ->
    case wh:get_simple_name(Type) of
        "element" ->
            parse_element(Type);
        "simpleType" ->
            parse_simple_type(Type);
        "complexType" ->
            parse_complex_type(Type);
        "attribute" ->
            parse_attribute(Type);
        "annotation" ->
            parse_annotation(Type);
        "restriction" ->
            parse_restriction(Type);
        "complexContent" ->
            parse_complex_content(Type);
        "sequence" ->
            parse_sequence(Type);
        "all" ->
            parse_all(Type);
        "choice" ->
            parse_choice(Type);
        "extension" ->
            parse_extension(Type);
        "list" ->
            parse_list(Type);
        "import" ->
            import;
        "any" ->
            '#any';
        "anyAttribute" ->
            anyAttribute;
        "group" ->
            group;
        "attributeGroup" ->
            attributeGroup;
        "notation" ->
            notation;
        Other ->
            io:format("ERROR: unrecognized xsd-element: ~p~n",
                      [wh:get_name(Other)]),
            {error, {unknown_type, Other}}
    end.

%% FIXME: Must handle 'ref' attributes
parse_element(Element) ->
    Name = wh:get_attribute(Element, name),
    Type = wh:get_attribute(Element, type),
    Default = wh:get_attribute(Element, default),
    Fixed = wh:get_attribute(Element, fixed),
    Nillable = wh:get_attribute(Element, nillable),
    MinOccurs = to_integer(wh:get_attribute(Element, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Element, maxOccurs)),
    Children = [ parse_type(C) || C <- wh:get_all_child_elements(Element) ],
    #element{name=Name, type=Type, default=Default,
             fixed=Fixed, nillable=Nillable,
             min_occurs=MinOccurs, max_occurs=MaxOccurs,
             parts=Children}.

parse_complex_type(ComplexType) ->
    Name = wh:get_attribute(ComplexType, name),
    Abstract = wh:get_attribute(ComplexType, abstract),
    Restriction = parse_type(wh:find_element(ComplexType, "restriction")),
    Extension = parse_type(wh:find_element(ComplexType, "extension")),
    Types = [ parse_type(C) || C <- wh:get_all_child_elements(ComplexType) ],
    {Order, Parts} = select_ordering(Types),
    {Extends, ExtOrder, ExtendParts} = extract_extension(Extension),
    #complex_type{name=Name, order=choose_order(Order, ExtOrder),
                  extends=Extends, abstract=Abstract,
                  restrictions=Restriction,
                  parts=Parts++ExtendParts}.

extract_extension(undefined) -> {undefined, undefined, []};
extract_extension(#extension{base=Base, parts=ExtParts}) ->
    {Order, Parts} = select_ordering(ExtParts),
    {Base, Order, Parts}.


choose_order(undefined, O2) -> O2;
choose_order(O1, undefined) -> O1;
choose_order(O1, _) -> O1.

parse_simple_type(Simple) ->
    Name = wh:get_attribute(Simple, name),
    {Order, NewSimple} = list_or_union(Simple),
    Restriction = wh:find_element(NewSimple, "restriction"),
    CompiledRestriction = parse_restriction(Restriction),
    #simple_type{name=Name, order=Order,
                 restrictions=CompiledRestriction}.

list_or_union(Simple) ->
    case wh:find_element(Simple, "list") of
        undefined ->
            case wh:find_element(Simple, "union") of
                undefined ->
                    {undefined, Simple};
                Union ->
                    {union, wh:get_child(Union, "simpleType")}
            end;
        List ->
            {list, wh:get_child(List, "simpleType")}
    end.


parse_attribute(Attribute) ->
    Name = wh:get_attribute(Attribute, name),
    Type = wh:get_attribute(Attribute, type),
    Use = wh:get_attribute(Attribute, use),
    Default = wh:get_attribute(Attribute, default),
    Fixed = wh:get_attribute(Attribute, fixed),
    #attribute{name=Name, type=Type, use=Use, default=Default, fixed=Fixed}.

parse_restriction(undefined) -> undefined;
parse_restriction(Restriction) ->
    RestrictionBaseType = wh:get_attribute(Restriction, base),
    Restrictions = wh:get_all_child_elements(Restriction),
    Values = [ parse_restriction_property(R) || R <- Restrictions ],
    case is_enumeration(Values) of
        true ->
            #enumeration{base_type=RestrictionBaseType, values=Values};
        false ->
            #restriction{base_type=RestrictionBaseType, values=Values}
    end.

is_enumeration([]) ->
    false;
is_enumeration(Values) ->
    lists:all(fun({enumeration, _}) -> true; (_) -> false end, Values).

parse_complex_content(ComplexContent) ->
    case wh:get_all_child_elements(ComplexContent) of
        [Child] ->
            parse_type(Child);
        Children ->
            [ parse_type(C) || C <- Children ]
    end.

parse_sequence(Sequence) ->
    MinOccurs = to_integer(wh:get_attribute(Sequence, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Sequence, maxOccurs)),
    Parts = [ parse_type(C) || C <- wh:get_all_child_elements(Sequence) ],
    #sequence{min_occurs=MinOccurs, max_occurs=MaxOccurs, parts=Parts}.

parse_choice(Choice) ->
    MinOccurs = to_integer(wh:get_attribute(Choice, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Choice, maxOccurs)),
    Parts = [ parse_type(C) || C <- wh:get_all_child_elements(Choice) ],
    #choice{min_occurs=MinOccurs, max_occurs=MaxOccurs, parts=Parts}.

parse_all(All) ->
    MinOccurs = to_integer(wh:get_attribute(All, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(All, maxOccurs)),
    Parts = [ parse_type(C) || C <- wh:get_all_child_elements(All) ],
    #all{min_occurs=MinOccurs, max_occurs=MaxOccurs, parts=Parts}.

parse_extension(Extension) ->
    BaseType = wh:get_attribute(Extension, base),
    Children = [ parse_type(C) || C <- wh:get_all_child_elements(Extension) ],
    #extension{base=BaseType, parts=Children}.

parse_list(List) ->
    Type = wh:get_attribute(List, itemType),
    Parts = [ parse_type(P) || P <- wh:get_all_child_elements(List) ],
    {list, Type, Parts}.

parse_annotation(Annotation) ->
    {doc, wh:get_docs(Annotation)}.

%% ----------------------------------------------------------------------------
%% Utility functions

select_ordering(Types) ->
    Sequence = lists:keyfind(sequence, 1, Types),
    Choice = lists:keyfind(choice, 1, Types),
    All = lists:keyfind(all, 1, Types),
    case {Sequence, Choice, All} of
        {false, false, false} ->
            {undefined, []};
        {false, false, #all{min_occurs=Min, max_occurs=Max, parts=Parts}} ->
            {{all, Min, Max}, Parts};
        {false, #choice{min_occurs=Min, max_occurs=Max, parts=Parts}, _} ->
            {{choice, Min, Max}, Parts};
        {#sequence{min_occurs=Min, max_occurs=Max, parts=Parts}, _, _} ->
            {{sequence, Min, Max}, Parts}
    end.

print_schema_stats(Schema) ->
    Elements = wh:get_children(Schema, "element"),
    SimpleTypes = wh:get_children(Schema, "simpleType"),
    ComplexTypes = wh:get_children(Schema, "complexType"),
    Groups = wh:get_children(Schema, "group"),
    Attributes = wh:get_children(Schema, "attribute"),
    AttributeGroups = wh:get_children(Schema, "attributeGroup"),
    Notations = wh:get_children(Schema, "notation"),
    Annotations = wh:get_children(Schema, "annotation"),
    io:format("e: ~p, s: ~p, c: ~p, g: ~p, a: ~p, ag: ~p, n: ~p, an: ~p~n",
              [length(Elements),
               length(SimpleTypes),
               length(ComplexTypes),
               length(Groups),
               length(Attributes),
               length(AttributeGroups),
               length(Notations),
               length(Annotations)]).

to_integer(undefined) -> 1;
to_integer("unbounded") -> infinite;
to_integer(List) when is_list(List) ->
    list_to_integer(List).

parse_restriction_property(Restriction) ->
    Value = wh:get_attribute(Restriction, value),
    case wh:get_simple_name(Restriction) of
        "minExclusive" ->
            {min_exclusive, Value};
        "minInclusive" ->
            {min_inclusive, Value};
        "maxExclusive" ->
            {max_exclusive, Value};
        "maxInclusive" ->
            {max_inclusive, Value};
        "totalDigits" ->
            {total_digits, Value};
        "fractionDigits" ->
            {fraction_digits, Value};
        "length" ->
            {length, to_integer(Value)};
        "minLength" ->
            {min_length, to_integer(Value)};
        "maxLength" ->
            {max_length, to_integer(Value)};
        "enumeration" ->
            {enumeration, Value};
        "whiteSpace" ->
            {whitespace, Value};
        "pattern" ->
            {pattern, Value}
    end.

%% ----------------------------------------------------------------------------
%% Type aggregation functions

propagate_namespaces(Schemas) ->
    lists:foldl(fun(S, Res) -> insert_nss(S, Res) end, [], Schemas).

insert_nss(#schema{namespace=Ns, types=Ts}, Res) ->
    insert_nss(Ts, Ns, Res).

insert_nss([E = #element{name=N, parts=[]} | Ts], Ns, Res) ->
    insert_nss(Ts, Ns, [E#element{name=qname(N, Ns)}|Res]);
insert_nss([E = #element{name=N, parts=Ps} | Ts], Ns, Res) ->
    NewParts = case Ps of
                   [#simple_type{}] ->
                       error({not_implemented, unnamed_simple_type},
                             [E, Ns, Res]);
                   [#complex_type{parts=CtPs} = Ct] ->
                       [Ct#complex_type{parts=insert_nss(CtPs, Ns, [])}];
                   [Doc, #complex_type{parts=CtPs} = Ct] ->
                       [Doc, Ct#complex_type{parts=insert_nss(CtPs, Ns, [])}];
                   [{doc, Doc}] ->
                       [{doc, Doc}]
               end,
    insert_nss(Ts, Ns, [E#element{name=qname(N, Ns), parts=NewParts}|Res]);
insert_nss([A = #attribute{name=N} | Ts], Ns, Res) ->
    insert_nss(Ts, Ns, [A#attribute{name=qname(N, Ns)}|Res]);
insert_nss([St = #simple_type{name=N} | Ts], Ns, Res) ->
    insert_nss(Ts, Ns, [St#simple_type{name=qname(N, Ns)}|Res]);
insert_nss([Ct = #complex_type{name=N, parts=Ps} | Ts], Ns, Res) ->
    NewPs = insert_nss(Ps, Ns, []),
    NewRes = [Ct#complex_type{name=qname(N, Ns), parts=NewPs}|Res],
    insert_nss(Ts, Ns, NewRes);
insert_nss([_E | Ts], Ns, Res) ->
    insert_nss(Ts, Ns, Res);
insert_nss([], _, Res) ->
    lists:reverse(Res).

qname({Ns, N}, _) -> {to_string(Ns), to_string(N)};
qname(N, Ns) -> {to_string(Ns), to_string(N)}.

to_string(Val) when is_atom(Val) -> atom_to_list(Val);
to_string(Val) -> Val.

%% ----------------------------------------------------------------------------

process(Types, Model) ->
    Ts = process_all_simple(Types),
    TypeMap = ews_model:new(),
    {AllTypes, Elems} = process(Types, Ts, [], []),
    [ ews_model:put(T, Model, TypeMap) || T <- AllTypes ],
    [ ews_model:put(E, Model, TypeMap) || E <- Elems ],
    #model{type_map=TypeMap, elems=[]}.

process([#element{name=Qname, type=undefined, parts=Ps} = E | Rest], Ts,
        TypeAcc, ElemAcc) ->
    Meta = parse_meta(E),
    Elem = #elem{qname=Qname, type=Qname, meta=Meta},
    #complex_type{extends=Ext, parts=Ps2,
                  abstract=Abstract} = lists:keyfind(complex_type, 1, Ps),
    {AccWithSubTypes, SubElems} = process(Ps2, Ts, TypeAcc, []),
    Type = #type{qname=Qname, extends=Ext,
                 abstract=Abstract, elems=SubElems},
    process(Rest, Ts, [Type | AccWithSubTypes], [Elem | ElemAcc]);
process([#element{parts=[{doc, _}]} = E | Rest], Ts, TypeAcc, ElemAcc) ->
    process([E#element{parts=[]} | Rest], Ts, TypeAcc, ElemAcc);
process([#element{name=Qname, type=T, parts=[]} = E | Rest], Ts,
        TypeAcc, ElemAcc) ->
    Meta = parse_meta(E),
    Qtype = qname(T, no_ns),
    case to_base(T) of
        false ->
            case lists:keyfind(Qtype, 1, Ts) of
                false ->
                    Elem = #elem{qname=Qname, type=Qtype, meta=Meta},
                    process(Rest, Ts, TypeAcc, [Elem | ElemAcc]);
                {Qtype, BaseOrEnum} ->
                    Elem = #elem{qname=Qname, type=BaseOrEnum, meta=Meta},
                    process(Rest, Ts, TypeAcc, [Elem | ElemAcc])
            end;
        #base{} = Base ->
            Elem = #elem{qname=Qname, type=Base, meta=Meta} ,
            process(Rest, Ts, TypeAcc, [Elem | ElemAcc])
    end;
process([#simple_type{} | Rest], Ts, TypeAcc, ElemAcc) ->
    process(Rest, Ts, TypeAcc, ElemAcc);
process([#complex_type{name=Qname, extends=Ext, parts=Ps} | Rest], Ts,
        TypeAcc, ElemAcc) ->
    {AccWithSubTypes, SubElems} = process(Ps, Ts, TypeAcc, []),
    Type = #type{qname=Qname, extends=Ext, elems=SubElems},
    process(Rest, Ts, [Type | AccWithSubTypes], ElemAcc);
process([_T | Rest], Ts, TypeAcc, ElemAcc) ->
    %% io:format("error: unexpected ~p~n", [T]),
    process(Rest, Ts, TypeAcc, ElemAcc);
process([], _, TypeAcc, ElemAcc) ->
    {TypeAcc, lists:reverse(ElemAcc)}.

process_all_simple([#simple_type{name=Qname} = S | Rest]) ->
    [{Qname, process_simple(S)} | process_all_simple(Rest) ];
process_all_simple([_ | Rest]) ->
    process_all_simple(Rest);
process_all_simple([]) -> [].

process_simple(#simple_type{restrictions=Rs, order=Order}) ->
    IsList = case Order of list -> true; _ -> false end,
    IsUnion = case Order of union -> true; _ -> false end,
    case Rs of
        #enumeration{base_type=Base, values=Values} ->
            Vs = [ {ews_alias:create({ok, Str}), Str} ||
                   {enumeration, Str} <- Values ],
            #enum{type=to_base(Base), values=Vs, list=IsList, union=IsUnion};
        #restriction{base_type=Base, values=Rvals} ->
            BaseRec = to_base(Base),
            BaseRec#base{restrictions=Rvals, list=IsList, union=IsUnion}
    end.

parse_meta(#element{default=D, fixed=F, nillable=N,
                    min_occurs=Min, max_occurs=Max}) ->
    #meta{nillable=N, default=D, fixed=F, min=Min, max=Max}.

%% TODO: Handle more refined basic spec types (i.e. non_neg_integer() etc)
to_base({"http://www.w3.org/2001/XMLSchema", "boolean"} = Qn) ->
    #base{xsd_type=Qn, erl_type=boolean};
to_base({"http://www.w3.org/2001/XMLSchema", N} = Qn) ->
    IntTypes = [integer, int, long, short, byte,
                unsignedInt, unsignedLong, unsignedShort,
                negativeInteger, positiveInteger, nonNegativeInteger],
    FloatTypes = [decimal, double, float],
    case lists:member(list_to_atom(N), IntTypes) of
        true ->
            #base{xsd_type=Qn, erl_type=integer};
        false ->
            case lists:member(list_to_atom(N), FloatTypes) of
                true ->
                    #base{xsd_type=Qn, erl_type=float};
                false ->
                    #base{xsd_type=Qn, erl_type=string}
            end
    end;
to_base(_) -> false.

%% ----------------------------------------------------------------------------
