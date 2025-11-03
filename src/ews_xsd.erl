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
%%% ---------------------------------------------------------------------------
%%% WSDL Types parsing
%%% Doesn't support: group, attributeGroup, include and notation types
%%%                  unique, key or keyref
%%% FIXME: Merge elements and types.
%%% ---------------------------------------------------------------------------

-module(ews_xsd).

-export([ parse_schema/2
        , parse_schema/3
        ]).

-export([ print_all_schema_stats/1
        , print_schema_stats/1
        ]).

-export([split_schemas/1]).

-export([ process/2
        , propagate_namespaces/1
        , import_schema/2
        , parse_types/1
        , to_base/1
        ]).

-include("ews.hrl").
-include_lib("ews/include/ews.hrl").

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

-spec parse_schema(list(binary()), atom()) -> #model{}.
parse_schema(Schemas0, Model) when is_atom(Model) ->
    Schemas = get_all_schemas(Schemas0),
    %%logger:notice("~p~n", [Schemas]),
    PrSchemas = [ S#schema{url=Url,
                           types=parse_types(Types)} ||
                    {_, Url, #schema{types=Types} = S} <- Schemas ],
    NewTypes = process(propagate_namespaces(PrSchemas), Model),
    NewTypes.
-spec parse_schema(list(binary()), atom(), file:name_all()) -> #model{}.
parse_schema(Schemas0, Model, BaseDir) when is_atom(Model) ->
    Schemas = get_all_schemas(Schemas0, BaseDir),
    %%logger:notice("~p~n", [Schemas]),
    PrSchemas = [ S#schema{url=Url,
                           types=parse_types(Types)} ||
                    {_, Url, #schema{types=Types} = S, _} <- Schemas ],
    NewTypes = process(propagate_namespaces(PrSchemas), Model),
    NewTypes.

%% ----------------------------------------------------------------------------
%% Import schema functions

get_all_schemas([TopSchema | T]) ->
    Namespace = wh:get_attribute(TopSchema, targetNamespace),
    Input = {Namespace, undefined, #schema{namespace=Namespace,
                                           types=TopSchema}},
    AllSchemas = lists:flatten(do_get_all_schemas(Input, [])),
    lists:ukeysort(1, AllSchemas ++ get_all_schemas(T));
get_all_schemas([]) ->
    [];
get_all_schemas(Schema) ->
    get_all_schemas([Schema]).

get_all_schemas([TopSchema | T], BaseDir) ->
    Namespace = wh:get_attribute(TopSchema, targetNamespace),
    Input = {Namespace, undefined, #schema{namespace=Namespace,
                                           types=TopSchema}, BaseDir},
    AllSchemas = lists:flatten(do_get_all_schemas_local(Input, [])),
    lists:ukeysort(1, AllSchemas ++ get_all_schemas(T, BaseDir));
get_all_schemas([], _)->
    [];
get_all_schemas(Schema, BaseDir) ->
    get_all_schemas([Schema], BaseDir).

do_get_all_schemas({Ns, Base, Schema}, Acc) ->
    case find_imports(Schema) of
        [] ->
            [{Ns, Base, Schema} | Acc];
        Imports ->
            ImpSchemas = [ {ImpNs, Url, import_schema(Url, ImpNs)} ||
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
            logger:debug("Imports: ~p~n", [Imports]),
            ImpSchemas = [ {ImpNs, Url, import_schema(Url, ImpNs, BaseDir),
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

find_imports(#schema{types=Schema}) ->
    Imports = wh:get_children(Schema, "import"),
    [ {wh:get_attribute(I, namespace),
       wh:get_attribute(I, schemaLocation)} || I <- Imports ].

import_schema(SchemaUrl, ImpNs) ->
    {ok, Bin} = request_cached(SchemaUrl),
    {Schemas, _} = xmerl_scan:string(binary_to_list(Bin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),
    find_schema(split_schemas(Schemas), ImpNs).

import_schema(SchemaUrl, ImpNs, BaseDir) ->
    {ok, Bin} = request_cached(SchemaUrl, BaseDir),
    {Schemas, _} = xmerl_scan:string(binary_to_list(Bin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),
    find_schema(split_schemas(Schemas), ImpNs).

split_schemas(#xmlElement{} = Schemas) ->
    do_split_schemas([Schemas], []).

do_split_schemas([#xmlElement{
                     expanded_name =
                         {'http://www.w3.org/2001/XMLSchema',schema}}
                  = Schema | Tail
                 ], Acc) ->
    Ns = wh:get_attribute(Schema, targetNamespace),
    do_split_schemas(Tail, [#schema{ namespace = Ns
                                   , types = Schema
                                   } | Acc]);
do_split_schemas([#xmlElement{content = Content} | Tail], Acc0) ->
    Acc = do_split_schemas(Content, Acc0),
    do_split_schemas(Tail, Acc);
do_split_schemas([#xmlText{} | Tail], Acc) ->
    do_split_schemas(Tail, Acc);
do_split_schemas([], Acc) ->
    Acc.

find_schema([#schema{namespace = ImpNs} = Schema | _ ], ImpNs) ->
    Schema;
find_schema([_ | T], ImpNs) ->
    find_schema(T, ImpNs);
find_schema([], ImpNs) ->
    error({cant_find_import_schema, ImpNs}).

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
        %% This is handled by parse_complex_type/1
        %% "simpleContent" ->
        %%     parse_simple_content(Type);
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
            ?log("ERROR: unrecognized xsd-element: ~p~n",
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
maybe_ref(Qname, Element) ->
    Default = wh:get_attribute(Element, default),
    Fixed = wh:get_attribute(Element, fixed),
    Nillable = wh:get_attribute(Element, nillable),
    MinOccurs = to_integer(wh:get_attribute(Element, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Element, maxOccurs)),
    #element{name=Qname, type=#reference{name=Qname},
             default=Default, fixed=Fixed, nillable=Nillable,
             min_occurs=MinOccurs, max_occurs=MaxOccurs,
             parts=[]}.

parse_complex_type(ComplexType) ->
    Name = wh:get_attribute(ComplexType, name),
    %%logger:notice("ComplexType: ~p~n", [Name]),
    Abstract = wh:get_attribute(ComplexType, abstract),
    Children = wh:get_all_child_elements(ComplexType),
    Restriction = parse_type(wh:find_element(ComplexType, "restriction")),
    Extension = parse_type(wh:find_element(ComplexType, "extension")),
    case Children of
        [#xmlElement{name = simpleContent} = SimpleContent] ->
            RestrictionSC = parse_type(wh:find_element(SimpleContent,
                                                       "restriction")),
            ExtensionSC = parse_type(wh:find_element(SimpleContent, "extension")),
            %% TODO: handle extenstions without this ugly hack
            %% This is converted to a simple_type since we don't want to emit
            %% a record for a simpleContent
            RestrictionFinal = extract_base(RestrictionSC, ExtensionSC),
            #simple_type{name=Name,
                         restrictions=RestrictionFinal
                         };
        _ ->
            ChildTypes = [ parse_type(C) || C <- Children ],
            Parts = flatten_children(ChildTypes),
            {Extends, ExtendParts} = extract_extension(Extension),
            #complex_type{name=Name,
                          extends=Extends, abstract=Abstract,
                          restrictions=Restriction,
                          parts=Parts++ExtendParts}
    end.

extract_base(undefined, #extension{base=Base}) ->
    #restriction{base_type=Base};
extract_base(Restriction, _) ->
    Restriction.

extract_extension(undefined) -> {undefined, []};
extract_extension(#extension{base=Base, parts=ExtParts}) ->
    Parts = flatten_children(ExtParts),
    {Base, Parts}.

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

flatten_children(Types) ->

    %% We lose some accuracy here, the nesting disappears but
    %% we don't use it for now anyway.

    %% common case:
    %%   just a flat sequence, with only elements
    %% special case, sequence of choices:
    %%   get all choice elements as if they were the sequence, all minoccurs:=0
    %% special case, choice of sequences:
    %%   recurse through sequences, find parts, merge, remove dupes
    %%
    %% after this do some uniqueness.
    %%
    Children = lists:flatten([ flatten_children(T, false) || T <- Types ]),
    UniqueChildren = lists:foldl(
        fun (Child, Acc) ->
            case lists:member(Child, Acc) of
                false -> Acc ++ [Child];
                true -> Acc
            end
        end,
        [],
        Children
    ),
    UniqueChildren.

flatten_children(Types, PropUndefined) when is_list(Types) ->
    [ flatten_children(T, PropUndefined) || T <- Types ];
flatten_children(#sequence{min_occurs=0, parts=Parts}, _PropUndefined) ->
    [ flatten_children(T, true) || T <- Parts ];
flatten_children(#sequence{min_occurs=_, parts=Parts}, PropUndefined) ->
    [ flatten_children(T, PropUndefined) || T <- Parts ];
flatten_children(#choice{min_occurs=_, parts=Parts}, _PropUndefined) ->
    [ flatten_children(T, true) || T <- Parts ];
flatten_children(#all{min_occurs=_, parts=Parts}, PropUndefined) ->
    [ flatten_children(T, PropUndefined) || T <- Parts ];
flatten_children(#element{} = E, true) ->
    E#element{min_occurs=0};
flatten_children(Any, _PropUndefined) ->
    Any.

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
    logger:debug("e: ~p, s: ~p, c: ~p, g: ~p, a: ~p, ag: ~p, n: ~p, an: ~p~n",
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
    {_AllTypes, _Elems} =
        case process(Types, [], Ts, [], [], TypeMap, Model, root, []) of
            {A, E, [], []} -> {A, E};
            {A, E, Retry, []} ->
                %% pass 2
                case process(Retry, [], Ts, [], [], TypeMap, Model, root, []) of
                    {A2, E2, [], []} -> {A2 ++ A, E2 ++ E};
                    {_, _, R2, []} ->
                        %%logger:error("Can't resolve all refs~n~p~n",
                        %%             [lists:usort(ets:tab2list(TypeMap))]),
                        error({cannot_resolve, R2})
                end
    end,
    #model{type_map=TypeMap, elems=[], simple_types=Ts}.

process([#element{name=Qname, type=undefined, parts=Ps} = E | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc) ->
    Meta = parse_meta(E),
    case lists:keyfind(complex_type, 1, Ps) of
        #complex_type{extends=Ext, parts=Ps2,
                      abstract=Abstract} ->
            TypeName = type_name(Qname, Parent),
            Elem = #elem{qname=Qname, type=TypeName, meta=Meta},
            ews_model:put_elem(Elem, Parent, TypeMap),
            {AccWithSubTypes, SubElems, Retry2, _} =
                process(Ps2, Retry, Ts, TypeAcc, [],
                        TypeMap, Model, Parent, AttrAcc),
            %% Check if any child is a reference.
            case [ Ref || #element{type=#reference{}} = Ref <- SubElems] of
                [] ->
                    Type = #type{qname=TypeName, extends=Ext,
                                 abstract=Abstract, elems=SubElems},
                    ews_model:put(Type, Model, TypeMap),
                    process(Rest, Retry2, Ts, [Type | AccWithSubTypes],
                            [Elem | ElemAcc],
                            TypeMap, Model, Parent, AttrAcc);
                [_ | _] ->
                    %% At least one child is a reference, put this
                    %% element in the Retry acc and it shoud be
                    %% resolved for the second pass.
                    process(Rest, [E | Retry2], Ts, AccWithSubTypes,
                            [Elem | ElemAcc],
                            TypeMap, Model, Parent, AttrAcc)
            end;
        false ->
            #simple_type{} = Type = lists:keyfind(simple_type, 1, Ps),
            Base = process_simple(Type),
            Elem = #elem{qname=Qname, type=Base, meta=Meta},
            ews_model:put_elem(Elem, Parent, TypeMap),
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#element{parts=[{doc, _}]} = E | Rest], Retry, Ts, TypeAcc, ElemAcc,
        TypeMap, Model, Parent, AttrAcc) ->
    process([E#element{parts=[]} | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap,
            Model, Parent, AttrAcc);
process([#element{name=_Name, type=#reference{name=Qname}, parts=[]} = E | Rest],
        Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc) ->
    Meta = parse_meta(E),
    %% this is a reference, replace with definition and try again
    case ews_model:get_elem(Qname, TypeMap) of
        false ->
            process(Rest, [E | Retry], Ts, TypeAcc, [E | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc);
        #elem{type = #base{}} = E1 ->
            %% Override with meta from element with reference
            Elem = E1#elem{meta = Meta},
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc);
        #elem{type = _} = E1 ->
            %% Override with meta from element with reference
            Elem = E1#elem{meta = Meta},
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#element{name=Qname, type=T, parts=[]} = E | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc) ->
    Meta = parse_meta(E),
    Qtype = qname(T, no_ns),
    case to_base(Qtype) of
        false ->
            case lists:keyfind(Qtype, 1, Ts) of
                false ->
                    Elem = #elem{qname=Qname, type=Qtype, meta=Meta},
                    ews_model:put_elem(Elem, Parent, TypeMap),
                    process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap,
                            Model, Parent, AttrAcc);
                {Qtype, BaseOrEnum} ->
                    Elem = #elem{qname=Qname, type=BaseOrEnum, meta=Meta},
                    ews_model:put_elem(Elem, Parent, TypeMap),
                    process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap,
                            Model, Parent, AttrAcc)
            end;
        #base{} = Base ->
            Elem = #elem{qname=Qname, type=Base, meta=Meta} ,
            ews_model:put_elem(Elem, Parent, TypeMap),
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#simple_type{} | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model,
        Parent, AttrAcc) ->
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc);
process([#complex_type{name=Qname, extends=Ext, parts=Ps} = CT | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, _AttrAcc) ->
    %% We don't want to pass in Retry in processing of parts
    case process(Ps, [], Ts, TypeAcc, [], TypeMap, Model, Qname, []) of
        {AccWithSubTypes, SubElems, [], AttrAcc} ->
            Type = #type{qname=Qname, extends=Ext, elems=SubElems,
                         attrs=AttrAcc},
            ews_model:put(Type, Model, TypeMap),
            process(Rest, Retry, Ts, [Type | AccWithSubTypes], ElemAcc,
                    TypeMap, Model, Parent, []);
        {_, _, [_|_], AttrAcc} ->
            process(Rest, [CT | Retry], Ts, TypeAcc, ElemAcc, TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#attribute{} = A | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model,
        Parent, AttrAcc)->
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent,
            [ A | AttrAcc]);
process([T | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc)
  when not is_record(T, attribute) ->
    ?log("warning: unhandled ~p~n", [T]),
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc);
process([], Retry, _, TypeAcc, ElemAcc, _TypeMap, _Model, _Parent, AttrAcc) ->
    {TypeAcc, lists:reverse(ElemAcc), Retry, lists:reverse(AttrAcc)}.

type_name({Ns, N}, {_, Parent}) ->
    {Ns, Parent++"@"++N};
type_name({Ns, N}, root) ->
    {Ns, N}.

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
