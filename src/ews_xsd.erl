%%% ---------------------------------------------------------------------------
%%% WSDL Types parsing
%%% Doesn't support: group, attributeGroup, include and notation types
%%%                  unique, key or keyref
%%% FIXME: Merge elements and types.
%%% ---------------------------------------------------------------------------

-module(ews_xsd).

-export([parse_schema/2]).

-export([ print_all_schema_stats/1
        , print_schema_stats/1
        ]).

-include("ews.hrl").

-define(HTTP_OPTS, [ {connect_options,
                      [ {connect_timeout, timer:seconds(400)}
                      , {recv_timeout, timer:seconds(400)}
                      ]}
                   , with_body
                   ]).

-ifdef(DEBUG).
-define(print_stats(Schemas), print_all_schema_stats(Schemas)).
-else.
-define(print_stats(_Schemas), ok).
-endif.

%% ----------------------------------------------------------------------------
%% Api

parse_schema(Schema, {Acc, Model}) when is_atom(Model) ->
    Schemas = get_all_schemas(Schema),
    ?print_stats(Schemas),
    PrSchemas = [ #schema{namespace=Ns,
                          url=Url,
                          types=parse_types(S)} || {Ns, Url, S} <- Schemas ],
    NewTypes = process(propagate_namespaces(PrSchemas), Model),
    {ews_model:append_model(Acc, NewTypes, Model), Model};
parse_schema(Schema, {Acc, Model, BaseDir}) when is_atom(Model) ->
    Schemas = get_all_schemas(Schema, BaseDir),
    ?print_stats(Schemas),
    PrSchemas = [ #schema{namespace=Ns,
                          url=Url,
                          types=parse_types(S)} || {Ns, Url, S, _} <- Schemas ],
    NewTypes = process(propagate_namespaces(PrSchemas), Model),
    {ews_model:append_model(Acc, NewTypes, Model), Model}.

%% ----------------------------------------------------------------------------
%% Import schema functions

get_all_schemas(TopSchema) ->
    Namespace = wh:get_attribute(TopSchema, targetNamespace),
    Input = {Namespace, undefined, TopSchema},
    AllSchemas = lists:flatten(do_get_all_schemas(Input, [])),
    lists:ukeysort(1, AllSchemas).

get_all_schemas(TopSchema, BaseDir) ->
    Namespace = wh:get_attribute(TopSchema, targetNamespace),
    Input = {Namespace, undefined, TopSchema, BaseDir},
    AllSchemas = lists:flatten(do_get_all_schemas_local(Input, [])),
    lists:ukeysort(1, AllSchemas).

do_get_all_schemas({Ns, Base, Schema}, Acc) ->
    case find_imports(Schema) of
        [] ->
            [{Ns, Base, Schema} | Acc];
        Imports ->
            ImpSchemas = [ {ImpNs, Url, import_schema(Url)} ||
                             {ImpNs, Url} <- Imports,
                             Url /= undefined,
                             not lists:keymember(ImpNs, 1, Acc) ],
            [{Ns, Base, Schema} |
             [ do_get_all_schemas(S, Acc++ImpSchemas) || S <- ImpSchemas ]]
    end.

do_get_all_schemas_local({Ns, Base, Schema, BaseDir}, Acc) ->
    case find_imports(Schema) of
        [] ->
            [{Ns, Base, Schema, BaseDir} | Acc];
        Imports ->
            ?log("Imports: ~p~n", [Imports]),
            ImpSchemas = [ {ImpNs, Url, import_schema(Url, BaseDir),
                            basedir(Url, BaseDir)} ||
                             {ImpNs, Url} <- Imports,
                             Url /= undefined,
                             not lists:keymember(ImpNs, 1, Acc) ],
            [{Ns, Base, Schema, BaseDir} |
             [ do_get_all_schemas_local(S, Acc++ImpSchemas) || S <- ImpSchemas ]]
    end.

basedir(Url, BaseDir) ->
    case {uri_string:parse(Url), filename:dirname(Url)} of
        {#{scheme := _}, _} ->
            BaseDir;
        {#{}, "."} ->
            BaseDir;
        {#{}, DirName} ->
            filename:join(BaseDir, DirName)
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

import_schema(SchemaUrl, BaseDir) ->
    {ok, Bin} = request_cached(SchemaUrl, BaseDir),
    {Schema, _} = xmerl_scan:string(binary_to_list(Bin),
                                    [{space, normalize},
                                     {namespace_conformant, true},
                                     {validation, schema}]),
    Schema.

request_cached(SchemaUrl) ->
    CacheDir = application:get_env(ews, cache_base_dir, code:priv_dir(ews)),
    File = filename:join([CacheDir, "xsds", escape_slash(SchemaUrl)]),
    ok = filelib:ensure_dir(File),
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, Bin};
        {error, Error} ->
            case hackney:request(get, SchemaUrl, [], [], ?HTTP_OPTS) of
                {ok, 200, _, Bin} ->
                    ok = file:write_file(File, Bin),
                    {ok, Bin};
                {ok, _, _, Bin} ->
                    {error, Bin};
                {error, Error} ->
                    {error, Error}
            end
    end.

request_cached(SchemaUrl, BaseDir) ->
    CacheDir = application:get_env(ews, cache_base_dir, code:priv_dir(ews)),
    File = filename:join([CacheDir, "xsds", escape_slash(SchemaUrl)]),
    ok = filelib:ensure_dir(File),
    URI = uri_string:parse(SchemaUrl),
    case {file:read_file(File), URI} of
        {{ok, Bin}, _} ->
            {ok, Bin};
        {{error, Error}, #{scheme := _Scheme}} ->
            case hackney:request(get, SchemaUrl, [], [], ?HTTP_OPTS) of
                {ok, 200, _, Bin} ->
                    ok = file:write_file(File, Bin),
                    {ok, Bin};
                {ok, _, _, Bin} ->
                    {error, Bin};
                {error, Error} ->
                    {error, Error}
            end;
        %% Not a URI, fetch locally
        {{error, Error}, #{}} ->
            XSDFilename = filename:join(BaseDir, SchemaUrl),
            case file:read_file(XSDFilename) of
                {ok, Bin} ->
                    {ok, Bin};
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
                      [Other]),
            {error, {unknown_type, Other}}
    end.

%% FIXME: Must handle 'ref' attributes
parse_element(Element) ->
    Ref = wh:get_attribute(Element, ref),
    maybe_ref(Ref, Element).

maybe_ref(undefined, Element) ->
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
             parts=Children};
maybe_ref(Qname, _Element) ->
    #element{name=Qname, type=#reference{name=Qname}, parts=[]}.

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
    lists:any(fun({enumeration, _}) -> true; (_) -> false end, Values).

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

print_all_schema_stats(Schemas) ->
    [ print_schema_stats(S) || {_, _, S} <- Schemas ].

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
                   [#simple_type{} = St] ->
                       [St#simple_type{name=qname(N, Ns)}];
                   [#complex_type{parts=CtPs} = Ct] ->
                       [Ct#complex_type{parts=insert_nss(CtPs, Ns, [])}];
                   [Doc, #complex_type{parts=CtPs} = Ct] ->
                       [Doc, Ct#complex_type{parts=insert_nss(CtPs, Ns, [])}];
                   [Doc, #simple_type{} = St] ->
                       [Doc, St#simple_type{name=qname(N, Ns)}];
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
    %% pass 1
    {AllTypes, Elems} = case process(Types, [], Ts, [], [], TypeMap, Model) of
        {A, E, []} -> {A, E};
        {A, E, Retry} ->
            %% pass 2
            case process(Retry, [], Ts, [], [], TypeMap, Model) of
                {A2, E2, []} -> {A2 ++ A, E2 ++ E};
                {_, _, R2} -> error({cannot_resolve, R2})
            end
    end,
    [ ews_model:put(T, Model, TypeMap) || T <- AllTypes ],
    [ ews_model:put(Ex, Model, TypeMap) || Ex <- Elems ],
    #model{type_map=TypeMap, elems=[], simple_types=Ts}.

process([#element{name=Qname, type=undefined, parts=Ps} = E | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model) ->
    Meta = parse_meta(E),
    ?log("Elem ~p Ps: ~p~n", [Qname, Ps]),
    case lists:keyfind(complex_type, 1, Ps) of
        #complex_type{extends=Ext, parts=Ps2,
                      abstract=Abstract} ->
            Elem = #elem{qname=Qname, type=Qname, meta=Meta},
            ews_model:put(Elem, Model, TypeMap),
            {AccWithSubTypes, SubElems, Retry2} = process(Ps2, Retry, Ts, TypeAcc, [],
                                                  TypeMap, Model),
            Type = #type{qname=Qname, extends=Ext,
                         abstract=Abstract, elems=SubElems},
            ews_model:put(Type, Model, TypeMap),
            process(Rest, Retry2, Ts, [Type | AccWithSubTypes], [Elem | ElemAcc],
                    TypeMap, Model);
        false ->
            #simple_type{} = Type = lists:keyfind(simple_type, 1, Ps),
            #base{} = Base = process_simple(Type),
            Elem = #elem{qname=Qname, type=Base, meta=Meta} ,
            ews_model:put(Elem, Model, TypeMap),
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model)
    end;
process([#element{parts=[{doc, _}]} = E | Rest], Retry, Ts, TypeAcc, ElemAcc,
        TypeMap, Model) ->
    process([E#element{parts=[]} | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model);
process([#element{name=_Name, type=#reference{name=Qname}, parts=[]} = E | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model) ->
    %% this is a reference, replace with definition and try again
    case ews_model:get_elem(Qname, TypeMap) of
        false ->
            process(Rest, [E | Retry], Ts, TypeAcc, ElemAcc, TypeMap, Model);
        #elem{type = #base{}} = E1 ->
            process(Rest, Retry, Ts, TypeAcc, [E1 | ElemAcc], TypeMap, Model);
        #elem{type = _} = E1 ->
            process(Rest, Retry, Ts, TypeAcc, [E1 | ElemAcc], TypeMap, Model)
    end;
process([#element{name=Qname, type=T, parts=[]} = E | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model) ->
    Meta = parse_meta(E),
    Qtype = qname(T, no_ns),
    case to_base(Qtype) of
        false ->
            case lists:keyfind(Qtype, 1, Ts) of
                false ->
                    Elem = #elem{qname=Qname, type=Qtype, meta=Meta},
                    ews_model:put(Elem, Model, TypeMap),
                    process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model);
                {Qtype, BaseOrEnum} ->
                    Elem = #elem{qname=Qname, type=BaseOrEnum, meta=Meta},
                    ews_model:put(Elem, Model, TypeMap),
                    process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model)
            end;
        #base{} = Base ->
            Elem = #elem{qname=Qname, type=Base, meta=Meta} ,
            ews_model:put(Elem, Model, TypeMap),
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model)
    end;
process([#simple_type{} | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model) ->
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model);
process([#complex_type{name=Qname, extends=Ext, parts=Ps} | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model) ->
    {AccWithSubTypes, SubElems, Retry2} = process(Ps, Retry, Ts, TypeAcc, [], TypeMap, Model),
    Type = #type{qname=Qname, extends=Ext, elems=SubElems},
    ews_model:put(Type, Model, TypeMap),
    process(Rest, Retry2, Ts, [Type | AccWithSubTypes], ElemAcc, TypeMap, Model);
process([T | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model) ->
    io:format("error: unexpected ~p~n", [T]),
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model);
process([], Retry, _, TypeAcc, ElemAcc, _TypeMap, _Model) ->
    {TypeAcc, lists:reverse(ElemAcc), Retry}.

process_all_simple([#simple_type{name=Qname} = S | Rest]) ->
    [{Qname, process_simple(S)} | process_all_simple(Rest) ];
process_all_simple([_ | Rest]) ->
    process_all_simple(Rest);
process_all_simple([]) -> [].

process_simple(#simple_type{restrictions=Rs, order=Order}) ->
    IsList = case Order of list -> true; _ -> false end,
    IsUnion = case Order of union -> true; _ -> false end,
    case Rs of
        #enumeration{base_type=_Base, values=Values} = Enum ->
            Vs = [ {ews_alias:create({ok, Str}), Str} ||
                   {enumeration, Str} <- Values ],
            #enum{type=to_base(Enum), values=Vs, list=IsList, union=IsUnion};
        #restriction{base_type=_Base, values=Rvals} = Restriction ->
            BaseRec = to_base(Restriction),
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
to_base(#restriction{base_type = "boolean" = Qn}) ->
    #base{xsd_type=Qn, erl_type=boolean};
to_base(#restriction{base_type = Qn}) ->
    N = no_ns(Qn),
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
to_base(#enumeration{base_type = "boolean" = Qn}) ->
    #base{xsd_type=Qn, erl_type=boolean};
to_base(#enumeration{base_type = Qn}) ->
    N = no_ns(Qn),
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
to_base({"no_ns", "boolean"} = Qn) ->
    #base{xsd_type=Qn, erl_type=boolean};
to_base({"no_ns", N} = Qn) ->
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

no_ns({_NS, N}) -> N;
no_ns(N) -> N.

%% ----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

insert_nss_doc_simpletype_test() ->
    SimpleType =
        #simple_type{ name = undefined
                    , order = undefined
                    , restrictions =
                          #restriction{base_type = "string"
                                      , values = [{max_length, 2000}]}},
    DocAnnotation = {doc, <<"fake docs, please ignore">>},
    TestElem = #element{name="fake", parts=[DocAnnotation, SimpleType]},
    ?assertMatch([#element{}], insert_nss([TestElem], "https://example.com", [])).

-endif.
