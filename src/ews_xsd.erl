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
%%% Doesn't support: group, attributeGroup and notation types
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
        , import_schema/2
        , parse_types/1
        , to_base/1
        ]).

-include("ews.hrl").
-include_lib("ews/include/ews.hrl").

%% The parse-time context of one schema document: the targetNamespace that all
%% its declarations belong to, and the elementFormDefault that decides whether
%% its *local* element declarations are namespace-qualified on the wire.
-record(sctx, {ns, form_default = unqualified}).

-define(HTTP_OPTS, [ %% {connect_options,
                     %%  [ {connect_timeout, timer:seconds(400)}
                     %%  , {recv_timeout, timer:seconds(400)}
                     %%  ]}
                    with_body
                   ]).

-ifdef(DEBUG).
-define(print_stats(Schemas), print_all_schema_stats(Schemas)).
-else.
-define(print_stats(_Schemas), ok).
-endif.

%% ----------------------------------------------------------------------------
%% Api

-spec parse_schema(list(any()), atom()) -> #model{}.
parse_schema(Schemas0, Model) when is_atom(Model) ->
    Schemas = get_all_schemas(Schemas0),
    %%logger:notice("~p~n", [Schemas]),
    PrSchemas = [ S#schema{url=Url,
                           types=parse_types(Types)} ||
                    {_, Url, #schema{types=Types} = S} <- Schemas ],
    NewTypes = process(all_types(PrSchemas), Model),
    NewTypes.
-spec parse_schema(list(any()), atom(), file:name_all()) -> #model{}.
parse_schema(Schemas0, Model, BaseDir) when is_atom(Model) ->
    Schemas = get_all_schemas(Schemas0, BaseDir),
    %%logger:notice("~p~n", [Schemas]),
    PrSchemas = [ S#schema{url=Url,
                           types=parse_types(Types)} ||
                    {_, Url, #schema{types=Types} = S, _} <- Schemas ],
    NewTypes = process(all_types(PrSchemas), Model),
    NewTypes.

%% Every schema's declarations, in one list. The qnames were settled while
%% parsing, so from here on nothing needs to know which schema a type came
%% from.
%%
%% The order is not arbitrary: process/2 hands the types to ews_alias in this
%% order, and the first type to claim a record name keeps it. This reproduces
%% the order the namespace-propagation pass used to leave behind -- it flipped
%% the accumulator once per schema -- so that record names stay put.
all_types(Schemas) ->
    lists:foldl(fun(#schema{types=Ts}, Acc) -> lists:reverse(Acc) ++ Ts end,
                [], Schemas).

%% ----------------------------------------------------------------------------
%% Import schema functions

get_all_schemas([TopSchema | T]) ->
    Namespace = wh:get_attribute(TopSchema, targetNamespace),
    ExpandedSchema = find_includes(TopSchema, Namespace,
                                   form_default(TopSchema)),
    %%logger:notice("ExpandedSchema: ~tp~n", [ExpandedSchema]),
    Input = {Namespace, undefined, #schema{namespace=Namespace,
                                           types=ExpandedSchema}},
    AllSchemas = lists:flatten(do_get_all_schemas(Input, [])),
    lists:ukeysort(1, AllSchemas ++ get_all_schemas(T));
get_all_schemas([]) ->
    [];
get_all_schemas(Schema) ->
    get_all_schemas([Schema]).

get_all_schemas([TopSchema | T], BaseDir) ->
    Namespace = wh:get_attribute(TopSchema, targetNamespace),
    ExpandedSchema = find_includes(TopSchema, Namespace,
                                   form_default(TopSchema)),
    %%logger:notice("ExpandedSchema: ~tp~n", [ExpandedSchema]),
    Input = {Namespace, undefined, #schema{namespace=Namespace,
                                           types=ExpandedSchema}, BaseDir},
    AllSchemas = lists:flatten(do_get_all_schemas_local(Input, [])),
    lists:ukeysort(1, AllSchemas ++ get_all_schemas(T, BaseDir));
get_all_schemas([], _)->
    [];
get_all_schemas(Schema, BaseDir) ->
    get_all_schemas([Schema], BaseDir).

%% Keep the schema element itself, not just its content: parse_types/1 reads
%% targetNamespace and elementFormDefault off it.
find_includes(#xmlElement{content=Content} = Schema, Ns, Form) ->
    Schema#xmlElement{content=find_includes(Content, Ns, Form)};
find_includes([#xmlText{} = Txt | T], Ns, Form) ->
    [Txt | find_includes(T, Ns, Form)];
find_includes([#xmlElement{
                  expanded_name =
                      {'http://www.w3.org/2001/XMLSchema',
                       include}} = IncElem | T], Ns, Form) ->
    Url = wh:get_attribute(IncElem, schemaLocation),
    %%logger:notice("Url: ~tp~n", [Url]),
    #schema{types = Include} = import_schema(Url, Ns),
    #xmlElement{content=Content} = Include,
    warn_on_form_clash(Url, Include, Form),
    Content ++ find_includes(T, Ns, Form);
find_includes([#xmlElement{content=Content} = Elem | T], Ns, Form) ->
    [Elem#xmlElement{content=find_includes(Content, Ns, Form)} |
     find_includes(T, Ns, Form)];
find_includes([#xmlComment{} = Comment | T], Ns, Form) ->
    [Comment | find_includes(T, Ns, Form)];
find_includes([], _, _) ->
    [].

form_default(Schema) ->
    form(wh:get_attribute(Schema, elementFormDefault), unqualified).

%% Splicing the included content in loses the document boundary, so its
%% declarations are parsed under the including schema's elementFormDefault.
%% When the two disagree, that silently gives the included schema's local
%% elements the wrong form on the wire -- say so rather than let it pass.
warn_on_form_clash(Url, Included, Form) ->
    case form_default(Included) of
        Form ->
            ok;
        Other ->
            logger:warning("included schema ~ts declares elementFormDefault "
                           "~p but is parsed as ~p: its local elements will "
                           "get the wrong form~n", [Url, Other, Form])
    end.

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
    %%logger:notice("Schema: ~tp~n", [Schema]),
    Imports = wh:get_children(Schema, "import"),
    %%logger:notice("Imports: ~tp~n", [Imports]),
    [ {wh:get_attribute(I, namespace),
       wh:get_attribute(I, schemaLocation)} || I <- Imports ].

import_schema(SchemaUrl, ImpNs) ->
    %%logger:notice("Import: ~tp~n", [SchemaUrl]),
    {ok, Bin} = request_cached(SchemaUrl),
    %% Yes, binary_to_list. Let xmerl figure out the encoding.
    {Schemas, _} = xmerl_scan:string(binary_to_list(Bin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),
    find_schema(split_schemas(Schemas), ImpNs).

import_schema(SchemaUrl, ImpNs, BaseDir) ->
    %%logger:notice("Import: ~tp  (~tp)~n", [SchemaUrl, BaseDir]),
    {ok, Bin} = request_cached(SchemaUrl, BaseDir),
    %% Yes, binary_to_list. Let xmerl figure out the encoding.
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
    CacheApp = application:get_env(ews, cache_base_app, ews),
    CacheDir = application:get_env(ews, cache_base_dir,
                                   code:priv_dir(CacheApp)),
    File = filename:join([CacheDir, "xsds", escape_slash(SchemaUrl)]),
    ok = filelib:ensure_dir(File),
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, Bin};
        {error, Error} ->
            case hackney:request(get, iolist_to_binary(SchemaUrl), [], [],
                                 ?HTTP_OPTS) of
                {ok, 200, _, Bin} ->
                    ok = file:write_file(File, Bin),
                    {ok, Bin};
                {ok, _, _, Bin} ->
                    {error, Bin};
                {error, Error} ->
                    logger:error("Problem fetching XSD: ~tp~n", [SchemaUrl]),
                    {error, Error}
            end
    end.

request_cached(SchemaUrl, BaseDir) ->
    CacheApp = application:get_env(ews, cache_base_app, ews),
    CacheDir = application:get_env(ews, cache_base_dir,
                                   code:priv_dir(CacheApp)),
    File = filename:join([CacheDir, "xsds", escape_slash(SchemaUrl)]),
    ok = filelib:ensure_dir(File),
    URI = uri_string:parse(SchemaUrl),
    case {file:read_file(File), URI} of
        {{ok, Bin}, _} ->
            {ok, Bin};
        {{error, Error}, #{scheme := _Scheme}} ->
            case hackney:request(get, iolist_to_binary(SchemaUrl), [], [],
                                 ?HTTP_OPTS) of
                {ok, 200, _, Bin} ->
                    ok = file:write_file(File, Bin),
                    {ok, Bin};
                {ok, _, _, Bin} ->
                    {error, Bin};
                {error, Error} ->
                    logger:error("Problem fetching XSD: ~tp~n", [SchemaUrl]),
                    {error, Error}
            end;
        %% Not a URI, fetch locally
        {{error, Error}, #{}} ->
            XSDFilename = filename:join(BaseDir, SchemaUrl),
            case file:read_file(XSDFilename) of
                {ok, Bin} ->
                    {ok, Bin};
                {error, Error} ->
                    logger:error("Problem fetching XSD: ~tp~n", [XSDFilename]),
                    {error, Error}
            end
    end.

escape_slash([]) -> [];
escape_slash([$/ | Rest]) -> [$- | escape_slash(Rest)];
escape_slash([C | Rest]) -> [C | escape_slash(Rest)].

%% ----------------------------------------------------------------------------
%% Parse schema functions

%% TODO: Handle includes
%%
%% NOTE: an <include>d schema has been spliced into this one by find_includes/3
%% before we get here, so its declarations are parsed under the *including*
%% schema's elementFormDefault. That is wrong if the two documents disagree on
%% it -- rare, since an include shares the targetNamespace, and warned about by
%% warn_on_form_clash/3 -- and would need find_includes to keep the boundary
%% to fix.
parse_types(Schema) ->
    Ctx = #sctx{ns = wh:get_attribute(Schema, targetNamespace),
                form_default = form(wh:get_attribute(Schema,
                                                     elementFormDefault),
                                    unqualified)},
    Elements = wh:get_all_child_elements(Schema),
    %% Direct children of <schema> are global declarations; everything the
    %% recursion below reaches is local.
    Types = [ parse_type(E, Ctx, global) || E = #xmlElement{} <- Elements ],
    %% A top-level <annotation> documents the schema document itself rather
    %% than any declaration in it, and there is nowhere to put that once every
    %% schema's types are emitted into one file, so it goes no further.
    [ T || T <- Types, T /= import, T /= include, not is_doc(T) ].

is_doc({doc, _}) -> true;
is_doc(_) -> false.

%% Both elementFormDefault and a per-element form attribute default to
%% "unqualified" per the XSD spec.
form(undefined, Default) -> Default;
form("qualified", _) -> qualified;
form("unqualified", _) -> unqualified;
form(Other, Default) ->
    logger:warning("ignoring unknown element form: ~tp~n", [Other]),
    Default.

%% FIXME: Must handle import/any/anyAttribute/group/attributeGroup/notation/
%%                    appinfo/documentation/field/key/keyref/selector/unique
parse_type(Type, Ctx) ->
    parse_type(Type, Ctx, local).

parse_type(undefined, _Ctx, _Scope) ->
    undefined;
parse_type(#xmlElement{} = Type, Ctx, Scope) ->
    case wh:get_simple_name(Type) of
        "element" ->
            parse_element(Type, Ctx, Scope);
        "simpleType" ->
            parse_simple_type(Type, Ctx);
        "complexType" ->
            parse_complex_type(Type, Ctx);
        "attribute" ->
            parse_attribute(Type, Ctx);
        "annotation" ->
            parse_annotation(Type);
        "restriction" ->
            parse_restriction(Type);
        "complexContent" ->
            parse_complex_content(Type, Ctx);
        %% This is handled by parse_complex_type/2
        %% "simpleContent" ->
        %%     parse_simple_content(Type, Ctx);
        "sequence" ->
            parse_sequence(Type, Ctx);
        "all" ->
            parse_all(Type, Ctx);
        "choice" ->
            parse_choice(Type, Ctx);
        "extension" ->
            parse_extension(Type, Ctx);
        "list" ->
            parse_list(Type, Ctx);
        "import" ->
            import;
        "any" ->
            '#any';
        "anyAttribute" ->
            anyAttribute;
        "group" ->
            parse_group(Type, Ctx);
        "attributeGroup" ->
            attributeGroup;
        "notation" ->
            notation;
        Other ->
            logger:warning("ERROR: unrecognized xsd-element: ~p~n",
                 [Other]),
            {error, {unknown_type, Other}}
    end.

parse_element(Element, Ctx, Scope) ->
    Ref = wh:get_attribute(Element, ref),
    maybe_ref(Ref, Element, Ctx, Scope).

maybe_ref(undefined, Element, #sctx{ns=Ns} = Ctx, Scope) ->
    Type = wh:get_attribute(Element, type),
    Default = wh:get_attribute(Element, default),
    Fixed = wh:get_attribute(Element, fixed),
    Nillable = wh:get_attribute(Element, nillable),
    MinOccurs = to_integer(wh:get_attribute(Element, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Element, maxOccurs)),
    Children = [ parse_type(C, Ctx) ||
                   C <- wh:get_all_child_elements(Element) ],
    {Doc, Parts} = split_doc(Children),
    #element{name=element_qname(Element, Ctx, Scope), ns=Ns, type=Type,
             doc=Doc, default=Default, fixed=Fixed, nillable=Nillable,
             min_occurs=MinOccurs, max_occurs=MaxOccurs,
             parts=Parts};
maybe_ref(Ref, Element, #sctx{ns=Ns} = Ctx, _Scope) ->
    Default = wh:get_attribute(Element, default),
    Fixed = wh:get_attribute(Element, fixed),
    Nillable = wh:get_attribute(Element, nillable),
    MinOccurs = to_integer(wh:get_attribute(Element, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Element, maxOccurs)),
    %% A ref always resolves to a global declaration, which is qualified by
    %% the namespace it was declared in -- form does not apply.
    Qname = qname(Ref, Ns),
    {Doc, _} = split_doc([ parse_type(C, Ctx) ||
                             C <- wh:get_all_child_elements(Element) ]),
    #element{name=Qname, ns=Ns, type=#reference{name=Qname}, doc=Doc,
             default=Default, fixed=Fixed, nillable=Nillable,
             min_occurs=MinOccurs, max_occurs=MaxOccurs,
             parts=[]}.

%% A global element declaration -- a direct child of <schema> -- is always
%% qualified by the target namespace. A local one is qualified only when its
%% effective form says so: its own form attribute if it has one, otherwise the
%% schema's elementFormDefault. An unqualified element gets a bare name, which
%% is how ews_xml spells a tag with no namespace.
element_qname(Element, #sctx{ns=Ns}, global) ->
    qname(wh:get_attribute(Element, name), Ns);
element_qname(Element, #sctx{ns=Ns, form_default=Default}, local) ->
    Name = wh:get_attribute(Element, name),
    case form(wh:get_attribute(Element, form), Default) of
        qualified -> qname(Name, Ns);
        unqualified -> to_string(Name)
    end.

%% The qname of a named type, group or element declaration. Anonymous
%% declarations -- an inline complexType, say -- have no name of their own;
%% process/9 names them after the element they sit in.
decl_qname(undefined, _Ctx) -> undefined;
decl_qname(Name, #sctx{ns=Ns}) -> qname(Name, Ns).

parse_complex_type(ComplexType, Ctx) ->
    Name = decl_qname(wh:get_attribute(ComplexType, name), Ctx),
    %% logger:notice("ComplexType: ~p~n", [Name]),
    Abstract = wh:get_attribute(ComplexType, abstract),
    OwnDoc = own_doc(ComplexType),
    Children = wh:get_all_child_elements(ComplexType),
    %%logger:notice("Children: ~tp~n", [Children]),
    Restriction = parse_type(wh:find_element(ComplexType, "restriction"), Ctx),
    Extension = parse_type(wh:find_element(ComplexType, "extension"), Ctx),
    %% The simpleContent is looked for among the children rather than being
    %% required to be the only one: an <annotation> is allowed beside it, and
    %% requiring it alone meant a documented type took the branch below, where
    %% simpleContent is not understood, and lost its value field.
    case [ C || C = #xmlElement{expanded_name =
                                    {'http://www.w3.org/2001/XMLSchema',
                                     simpleContent}} <- Children ] of
        [SimpleContent] ->
            RestrictionSC = parse_type(wh:find_element(SimpleContent,
                                                       "restriction"), Ctx),
            ExtensionSC = parse_type(wh:find_element(SimpleContent, "extension"),
                                     Ctx),
            %% TODO: handle extenstions without this ugly hack
            %% This is converted to a simple_type since we don't want to emit
            %% a record for a simpleContent.
            %% Unless of course the simpleContent has attributes, then we
            %% need to emit a special record like this:
            %% -record(foo, {'__attrs' :: #{bar => string() | binary()} | undefined
            %%               value :: integer() | undefined}).
            RestrictionFinal = extract_base(RestrictionSC, ExtensionSC),
            %% logger:notice("SimpleContent: ~tp~n", [SimpleContent]),
            %% logger:notice("ExtensionSC: ~tp~n", [ExtensionSC]),
            #extension{parts=ExtensionParts} = ExtensionSC,
            case [ EP || #attribute{} = EP <- ExtensionParts ] of
                [] ->
                    #simple_type{name=Name,
                                 restrictions=RestrictionFinal,
                                 doc=OwnDoc
                                };
                [#attribute{} | _] = Attributes ->
                    %% logger:notice("SimpleContentAttrs: ~tp~n", [Attributes]),
                    #simple_content{name=Name,
                                    restrictions=RestrictionFinal,
                                    doc=OwnDoc,
                                    attrs=Attributes
                                   }
            end;
        [] ->
            ChildTypes = [ parse_type(C, Ctx) || C <- Children ],
            %% Flattened first: a <complexContent> with more than one child
            %% comes back as a list, and an <annotation> under it or under the
            %% <extension> would otherwise stay buried in the parts.
            {NestedDoc, TypeParts} = split_doc(lists:flatten(ChildTypes)),
            Parts = flatten_children(TypeParts),
            {Extends, ExtendParts} = extract_extension(Extension),
            #complex_type{name=Name,
                          extends=Extends, abstract=Abstract,
                          restrictions=Restriction,
                          doc=doc_or(OwnDoc, NestedDoc),
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

parse_simple_type(Simple, Ctx) ->
    Name = decl_qname(wh:get_attribute(Simple, name), Ctx),
    Doc = own_doc(Simple),
    {Order, NewSimple} = list_or_union(Simple),
    case Order of
        union ->
            #simple_type{name=Name, order=Order, doc=Doc,
                         unionmembers = NewSimple};
        Other when Other == list orelse Other == undefined ->
            Restriction = wh:find_element(NewSimple, "restriction"),
            CompiledRestriction = parse_restriction(Restriction),
            #simple_type{name=Name, order=Order, doc=Doc,
                         restrictions=CompiledRestriction}
    end.

%% The declaration's own <annotation>, as opposed to one on anything nested
%% inside it -- wh:get_docs/1 searches descendants, so it is given the
%% annotation element rather than the declaration.
own_doc(Element) ->
    case wh:get_child(Element, "annotation") of
        undefined ->
            undefined;
        Annotation ->
            case wh:get_docs(Annotation) of
                <<>> -> undefined;
                Doc -> Doc
            end
    end.

list_or_union(Simple) ->
    case wh:find_element(Simple, "list") of
        undefined ->
            case wh:find_element(Simple, "union") of
                undefined ->
                    {undefined, Simple};
                Union ->
                    %% logger:notice("Union: ~tp~n",  [Union]),
                    [#xmlAttribute{name = memberTypes,
                                   namespace = Ns,
                                   value = MemberString}] =
                        Union#xmlElement.attributes,
                    QMembers = parse_union_members(
                                 string:tokens(MemberString, " "), Ns, Union),
                    {union, QMembers}
            end;
        List ->
            {list, wh:get_child(List, "simpleType")}
    end.

parse_union_members([Member | T], #xmlNamespace{nodes = Nodes} = Ns, Union) ->
    [Prefix, Name] = string:tokens(Member, ":"),
    case proplists:get_value(Prefix, Nodes) of
        undefined ->
            logger:error("Can't find ns prefix ~tp in union: ~tp~n",
                         [Prefix, Union]),
            error({bad_ns_prefix, Prefix});
        NsAtom ->
            [{atom_to_list(NsAtom), Name} |
             parse_union_members(T, Ns, Union) ]
    end;
parse_union_members([], _, _) ->
    [].

parse_group(Group, Ctx) ->
    Ref = wh:get_attribute(Group, ref),
    maybe_group_ref(Ref, Group, Ctx).

maybe_group_ref(undefined, Group, Ctx) ->
    Name = decl_qname(wh:get_attribute(Group, name), Ctx),
    Children = wh:get_all_child_elements(Group),
    ChildTypes = [ parse_type(C, Ctx) || C <- Children ],
    {Doc, GroupParts} = split_doc(ChildTypes),
    Parts = flatten_children(GroupParts),
    #group{name=Name, parts=Parts, doc=Doc};
maybe_group_ref(Reference0, Group, #sctx{ns=Ns}) ->
    Reference = qname(Reference0, Ns),
    MinOccurs = to_integer(wh:get_attribute(Group, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Group, maxOccurs)),
    #group_ref{ref=Reference,
               min_occurs=MinOccurs, max_occurs=MaxOccurs}.

parse_attribute(Attribute, #sctx{ns=Ns}) ->
    Name = wh:get_attribute(Attribute, name),
    Type = type_qname(wh:get_attribute(Attribute, type), Ns),
    Use = wh:get_attribute(Attribute, use),
    Default = wh:get_attribute(Attribute, default),
    Fixed = wh:get_attribute(Attribute, fixed),
    #attribute{name=Name, type=Type, use=Use, default=Default, fixed=Fixed}.

parse_restriction(undefined) -> undefined;
parse_restriction(#xmlElement{name = Name, expanded_name = Qname} = Restriction)
  when Name == restriction orelse Qname == {'http://www.w3.org/2001/XMLSchema',
                                            restriction} ->
    %% logger:notice("Restriction: ~tp~n", [Restriction]),
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
    lists:any(fun({enumeration, _, _}) -> true; (_) -> false end, Values).

parse_complex_content(ComplexContent, Ctx) ->
    case wh:get_all_child_elements(ComplexContent) of
        [Child] ->
            parse_type(Child, Ctx);
        Children ->
            [ parse_type(C, Ctx) || C <- Children ]
    end.

parse_sequence(Sequence, Ctx) ->
    MinOccurs = to_integer(wh:get_attribute(Sequence, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Sequence, maxOccurs)),
    Parts = [ parse_type(C, Ctx) ||
                C <- wh:get_all_child_elements(Sequence) ],
    #sequence{min_occurs=MinOccurs, max_occurs=MaxOccurs, parts=Parts}.

parse_choice(Choice, Ctx) ->
    MinOccurs = to_integer(wh:get_attribute(Choice, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(Choice, maxOccurs)),
    Parts = [ parse_type(C, Ctx) || C <- wh:get_all_child_elements(Choice) ],
    #choice{min_occurs=MinOccurs, max_occurs=MaxOccurs, parts=Parts}.

parse_all(All, Ctx) ->
    MinOccurs = to_integer(wh:get_attribute(All, minOccurs)),
    MaxOccurs = to_integer(wh:get_attribute(All, maxOccurs)),
    Parts = [ parse_type(C, Ctx) || C <- wh:get_all_child_elements(All) ],
    #all{min_occurs=MinOccurs, max_occurs=MaxOccurs, parts=Parts}.

parse_extension(Extension, Ctx) ->
    BaseType = wh:get_attribute(Extension, base),
    Children = [ parse_type(C, Ctx) ||
                   C <- wh:get_all_child_elements(Extension) ],
    #extension{base=BaseType, parts=Children}.

parse_list(List, Ctx) ->
    Type = wh:get_attribute(List, itemType),
    Parts = [ parse_type(P, Ctx) || P <- wh:get_all_child_elements(List) ],
    {list, Type, Parts}.

parse_annotation(Annotation) ->
    {doc, wh:get_docs(Annotation)}.

%% An <annotation> is not a part of the type it sits in, it is documentation
%% for it, so it travels in its own field. Leaving it among the parts is how it
%% used to end up in process/9's "unhandled" catch-all.
split_doc(Children) ->
    case lists:keytake(doc, 1, Children) of
        {value, {doc, <<>>}, Rest} -> {undefined, Rest};
        {value, {doc, Doc}, Rest} -> {Doc, Rest};
        false -> {undefined, Children}
    end.

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
    lists:foldl(fun add_unique/2, [], Children).

flatten_children(Types, PropUndefined) when is_list(Types) ->
    [ flatten_children(T, PropUndefined) || T <- Types ];
flatten_children(#sequence{min_occurs=0, parts=Parts}, _PropUndefined) ->
    flatten_container(Parts, true);
flatten_children(#sequence{min_occurs=_, parts=Parts}, PropUndefined) ->
    flatten_container(Parts, PropUndefined);
flatten_children(#choice{min_occurs=_, parts=Parts}, _PropUndefined) ->
    flatten_container(Parts, true);
flatten_children(#all{min_occurs=_, parts=Parts}, PropUndefined) ->
    flatten_container(Parts, PropUndefined);
flatten_children(#element{} = E, true) ->
    E#element{min_occurs=0};
flatten_children(Any, _PropUndefined) ->
    Any.

%% Two branches of a choice can declare the same element, and only one field
%% can be emitted for it. Documentation is not part of what makes an element
%% the same element, so it must not split a duplicate in two -- that put the
%% same field in a record twice, and the generated file would not compile. The
%% copy that is kept inherits the text if it had none of its own.
add_unique(Child, Acc) ->
    case lists:splitwith(fun (Seen) -> not same_part(Seen, Child) end, Acc) of
        {_, []} ->
            Acc ++ [Child];
        {Before, [Seen | After]} ->
            Before ++ [fill_doc(Seen, Child) | After]
    end.

same_part(A, B) ->
    without_doc(A) =:= without_doc(B).

without_doc(#element{} = E) -> E#element{doc = undefined};
without_doc(Part) -> Part.

fill_doc(#element{doc = undefined} = Seen, #element{doc = Doc}) ->
    Seen#element{doc = Doc};
fill_doc(Seen, _) ->
    Seen.

%% A sequence, choice or all can carry an annotation of its own, and
%% dissolving the container leaves nowhere to put it -- so, as for a group, it
%% goes to the elements the container contributes.
flatten_container(Parts0, PropUndefined) ->
    {Doc, Parts} = split_doc(Parts0),
    Flat = lists:flatten([ flatten_children(P, PropUndefined) || P <- Parts ]),
    propagate_doc(Flat, Doc).

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

to_integer(undefined) -> undefined;
to_integer("unbounded") -> infinite;
to_integer(List) when is_list(List) ->
    list_to_integer(List).

parse_restriction_property(Restriction) ->
    %%logger:notice("Restriction: ~tp~n", [Restriction]),
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
            %% A value can say what it means, and that is worth keeping.
            {enumeration, Value, own_doc(Restriction)};
        "whiteSpace" ->
            {whitespace, Value};
        "pattern" ->
            {pattern, Value}
    end.

%% ----------------------------------------------------------------------------
%% Type aggregation functions

%% A name that already carries a namespace -- a prefixed attribute value that
%% wh:get_attribute has resolved -- keeps it. A bare one belongs to the schema
%% being parsed.
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
                        error({cannot_resolve, R2, Ts})
                end
    end,
    #model{type_map=TypeMap, elems=[], simple_types=Ts}.

process([#element{name=Qname, ns=Ns, type=undefined, parts=Ps,
                  doc=Doc} = E | Rest],
        Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc) ->
    Meta = parse_meta(E),
    case lists:keyfind(complex_type, 1, Ps) of
        #complex_type{extends=Ext, parts=Ps2,
                      abstract=Abstract, doc=CtDoc} ->
            TypeName = type_name(Qname, Ns, Parent),
            Elem = #elem{qname=Qname, type=TypeName, meta=Meta, doc=Doc},
            ews_model:put_elem(Elem, Parent, TypeMap),
            {AccWithSubTypes, SubElems, Retry2, AttrAcc1} =
                process(Ps2, Retry, Ts, TypeAcc, [],
                        TypeMap, Model, Parent, []),
            %% Check if any child is a reference.
            case [ Ref || #element{type=#reference{}} = Ref <- SubElems] of
                [] ->
                    Type = #type{qname=TypeName, extends=Ext,
                                 abstract=Abstract, elems=SubElems,
                                 doc=CtDoc, attrs=AttrAcc1},
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
            Base = process_simple(Type, Ts),
            Elem = #elem{qname=Qname, type=Base, meta=Meta,
                         doc=doc_or(Doc, simple_doc(Base))},
            ews_model:put_elem(Elem, Parent, TypeMap),
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#element{ns=Ns, type=#reference{name=RName}, parts=[],
                  doc=Doc} = E | Rest],
        Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc) ->
    Meta = parse_meta(E),
    %%logger:notice("Element with ref: ~tp~n", [E]),
    Qname = type_qname(RName, Ns),
    %% this is a reference, replace with definition and try again
    case ews_model:get_elem(Qname, TypeMap) of
        false ->
            process(Rest, [E | Retry], Ts, TypeAcc, [E | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc);
        #elem{type = #base{}} = E1 ->
            %% Override with meta from element with reference
            Elem = E1#elem{meta = Meta, doc = doc_or(Doc, E1#elem.doc)},
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc);
        #elem{type = _} = E1 ->
            %% Override with meta from element with reference
            Elem = E1#elem{meta = Meta, doc = doc_or(Doc, E1#elem.doc)},
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#element{name=Qname, ns=Ns, type=T, parts=[], doc=Doc} = E | Rest],
        Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc) ->
    Meta = parse_meta(E),
    Qtype = type_qname(T, Ns),
    case to_base(Qtype) of
        false ->
            case lists:keyfind(Qtype, 1, Ts) of
                false ->
                    Elem = #elem{qname=Qname, type=Qtype, meta=Meta, doc=Doc},
                    ews_model:put_elem(Elem, Parent, TypeMap),
                    process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap,
                            Model, Parent, AttrAcc);
                {Qtype, BaseOrEnum} ->
                    %% A field typed by a documented simple type says what the
                    %% type says, unless the element says something itself.
                    Elem = #elem{qname=Qname, type=BaseOrEnum, meta=Meta,
                                 doc=doc_or(Doc, simple_doc(BaseOrEnum))},
                    ews_model:put_elem(Elem, Parent, TypeMap),
                    process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap,
                            Model, Parent, AttrAcc)
            end;
        #base{} = Base ->
            Elem = #elem{qname=Qname, type=Base, meta=Meta, doc=Doc} ,
            ews_model:put_elem(Elem, Parent, TypeMap),
            process(Rest, Retry, Ts, TypeAcc, [Elem | ElemAcc], TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#simple_type{} | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model,
        Parent, AttrAcc) ->
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc);
process([#complex_type{name=Qname, extends=Ext, parts=Ps, doc=Doc} = CT | Rest],
        Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, _AttrAcc) ->
    %% We don't want to pass in Retry in processing of parts
    case process(Ps, [], Ts, TypeAcc, [], TypeMap, Model, Qname, []) of
        {AccWithSubTypes, SubElems, [], AttrAcc} ->
            Type = #type{qname=Qname, extends=Ext, elems=SubElems, doc=Doc,
                         attrs=AttrAcc},
            ews_model:put(Type, Model, TypeMap),
            process(Rest, Retry, Ts, [Type | AccWithSubTypes], ElemAcc,
                    TypeMap, Model, Parent, []);
        {_, _, [_|_], AttrAcc} ->
            process(Rest, [CT | Retry], Ts, TypeAcc, ElemAcc, TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#simple_content{name=Qname, restrictions=Restrictions, attrs=Ps,
                         doc=Doc} = CT
        | Rest], Retry, Ts,
        TypeAcc, ElemAcc, TypeMap, Model, Parent, _AttrAcc) ->
    #restriction{base_type = Bs} = Restrictions,
    %% We don't want to pass in Retry in processing of parts
    case process(Ps, [], Ts, TypeAcc, [], TypeMap, Model, Qname, []) of
        {AccWithSubTypes, [], [], AttrAcc} ->
            MaybeBaseOrEnum =
                case {to_base(type_qname(Bs, ns_of(Qname))),
                      lists:keyfind(Bs, 1, Ts)} of
                    {false, false} -> undefined;
                    {false, {_Qtype, BOrE}} -> BOrE;
                    {BOrE, _} -> BOrE
                end,
            case MaybeBaseOrEnum of
                undefined ->
                    %% logger:error("Can't find simpletype: ~tp~nTs: ~tp~n",
                    %%              [CT, Ts]),
                    process(Rest, [CT | Retry], Ts, TypeAcc, ElemAcc, TypeMap,
                            Model, Parent, AttrAcc);
                BaseOrEnum ->
                    SC = #sc{qname={ok,"value"}, type=BaseOrEnum,
                             %% hard code meta, the encasing element has the
                             %% correct meta.
                             meta=#meta{min=1, max=1}},
                    %% logger:notice("AttrAcc ~tp~n", [AttrAcc]),
                    Type = #type{qname=Qname,
                                 elems=[SC],
                                 doc=Doc,
                                 attrs=AttrAcc},
                    ews_model:put(Type, Model, TypeMap),
                    process(Rest, Retry, Ts, [Type | AccWithSubTypes], ElemAcc,
                            TypeMap, Model, Parent, [])
            end;
        {_, _, [_|_], AttrAcc} ->
            process(Rest, [CT | Retry], Ts, TypeAcc, ElemAcc, TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([#attribute{type=Type, base=undefined} = A | Rest], Retry, Ts, TypeAcc,
        ElemAcc, TypeMap, Model, Parent, AttrAcc)->
    case to_base(Type) of
        false ->
            case lists:keyfind(Type, 1, Ts) of
                false ->
                    %% logger:error("Can't find simpletype: ~tp~n",
                    %%              [Type]),
                    process(Rest, [A | Retry], Ts, TypeAcc, ElemAcc, TypeMap,
                            Model, Parent, AttrAcc);
                {_Qtype, BaseOrEnum} ->
                    NewA = A#attribute{base=BaseOrEnum},
                    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap,
                            Model, Parent, [NewA | AttrAcc])
            end;
        #base{} = Base ->
            NewA = A#attribute{base=Base},
            process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap,
                    Model, Parent, [NewA | AttrAcc])
    end;
process([#group{} = Group | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model,
        Parent, AttrAcc) ->
    ews_model:put_group(Group, Model, TypeMap),
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc);
process([#group_ref{ref=Ref, min_occurs=Min, max_occurs=Max} = Grr | Rest],
        Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc) ->
    %% logger:notice("GroupRef: ~tp~n", [Ref]),
    case ews_model:get_group(Ref, Model, TypeMap) of
        false ->
            process(Rest, [Grr | Retry], Ts, TypeAcc, ElemAcc, TypeMap, Model,
                    Parent, AttrAcc);
        #group{parts=Ps, doc=Doc} ->
            %% Turn a group into a sequence
            NewPs = propagate_doc(propagate_meta(Ps, Min, Max), Doc),
            process(NewPs ++ Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model,
                    Parent, AttrAcc)
    end;
process([T | Rest], Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc)
  when not is_record(T, attribute) ->
    logger:warning("warning: unhandled ~tp~n", [T]),
    process(Rest, Retry, Ts, TypeAcc, ElemAcc, TypeMap, Model, Parent, AttrAcc);
process([], Retry, _, TypeAcc, ElemAcc, _TypeMap, _Model, _Parent, AttrAcc) ->
    {TypeAcc, lists:reverse(ElemAcc), Retry, lists:reverse(AttrAcc)}.

propagate_meta([#element{min_occurs=Min, max_occurs=Max} = E | T],
               RefMin, RefMax) ->
    [E#element{min_occurs=maybe_override(RefMin, Min),
               max_occurs=maybe_override(RefMax, Max)} |
     propagate_meta(T, RefMin, RefMax)];
propagate_meta([#group_ref{min_occurs=Min, max_occurs=Max} = Grr | T],
               RefMin, RefMax) ->
    [Grr#group_ref{min_occurs=maybe_override(RefMin, Min),
                   max_occurs=maybe_override(RefMax, Max)} |
     propagate_meta(T, RefMin, RefMax)];
propagate_meta([Part | T], RefMin, RefMax) ->
    %% Anything else a group can hold is left alone rather than crashing the
    %% whole model: process/9 below reports what it cannot use.
    [Part | propagate_meta(T, RefMin, RefMax)];
propagate_meta([], _, _) ->
    [].

%% A group is dissolved into the type that references it, so there is no
%% record for it to document. Its text goes to the elements it contributes,
%% except where an element documents itself, which is more specific.
propagate_doc(Parts, undefined) ->
    Parts;
propagate_doc(Parts, Doc) ->
    [ case P of
          #element{doc = undefined} -> P#element{doc = Doc};
          _ -> P
      end || P <- Parts ].

maybe_override(undefined, undefined) -> 1;
maybe_override(undefined, N) -> N;
maybe_override(N, _) -> N.

%% The name for the anonymous type of an element declaration. It is derived
%% from the element's own name, which may be unqualified -- but a type always
%% lives in the namespace of the schema that declared it, so that is what we
%% qualify it with here.
type_name(Qname, Ns, {_, Parent}) ->
    {Ns, Parent++"@"++local_name(Qname)};
type_name(Qname, Ns, root) ->
    {Ns, local_name(Qname)}.

local_name({_Ns, N}) -> N;
local_name(N) -> N.

ns_of({Ns, _N}) -> Ns;
ns_of(_) -> undefined.

%% A type reference: already namespaced when it was written with a prefix,
%% otherwise it names a builtin or a type in the referring schema.
type_qname({_,_} = Qname, _) ->
    Qname;
type_qname(undefined, _) ->
    undefined;
type_qname(Name, Ns) ->
    case is_builtin(Name) of
        true -> {"no_ns", Name};
        false -> {Ns, Name}
    end.

is_builtin("anyURI") -> true;
is_builtin("anyAtomicType") -> true;
is_builtin("anySimpleType") -> true;
is_builtin("base64Binary") -> true;
is_builtin("boolean") -> true;
is_builtin("byte") -> true;
is_builtin("date") -> true;
is_builtin("dateTime") -> true;
is_builtin("dateTimeStamp") -> true;
is_builtin("dayTimeDuration") -> true;
is_builtin("decimal") -> true;
is_builtin("double") -> true;
is_builtin("duration") -> true;
is_builtin("ENTITIES") -> true;
is_builtin("ENTITY") -> true;
is_builtin("float") -> true;
is_builtin("gDay") -> true;
is_builtin("gMonth") -> true;
is_builtin("gMonthDay") -> true;
is_builtin("gYear") -> true;
is_builtin("gYearMonth") -> true;
is_builtin("hexBinary") -> true;
is_builtin("ID") -> true;
is_builtin("IDREF") -> true;
is_builtin("IDREFS") -> true;
is_builtin("int") -> true;
is_builtin("integer") -> true;
is_builtin("language") -> true;
is_builtin("long") -> true;
is_builtin("Name") -> true;
is_builtin("NCName") -> true;
is_builtin("negativeInteger") -> true;
is_builtin("NMTOKEN") -> true;
is_builtin("NMTOKENS") -> true;
is_builtin("nonNegativeInteger") -> true;
is_builtin("nonPositiveInteger") -> true;
is_builtin("normalizedString") -> true;
is_builtin("NOTATION") -> true;
is_builtin("positiveInteger") -> true;
is_builtin("precisionDecimal") -> true;
is_builtin("QName") -> true;
is_builtin("short") -> true;
is_builtin("string") -> true;
is_builtin("time") -> true;
is_builtin("token") -> true;
is_builtin("unsignedByte") -> true;
is_builtin("unsignedInt") -> true;
is_builtin("unsignedLong") -> true;
is_builtin("unsignedShort") -> true;
is_builtin("yearMonthDuration") -> true;
is_builtin(OtherType) when is_list(OtherType) -> false.

process_all_simple([#simple_type{name=Qname} = S | Rest]) ->
    [{Qname, do_process_simple(S)} | process_all_simple(Rest) ];
process_all_simple([_ | Rest]) ->
    process_all_simple(Rest);
process_all_simple([]) -> [].

%% This process can't handle unions correctly
do_process_simple(#simple_type{restrictions=Rs, order=Order, doc=Doc}) ->
    %% logger:notice("St: ~tp~n", [St]),
    IsList = case Order of list -> true; _ -> false end,
    IsUnion = case Order of union -> true; _ -> false end,
    case Rs of
        #enumeration{base_type=_Base, values=Values} = Enum ->
            #enum{type=to_base(Enum), values=enum_values(Values),
                  value_docs=enum_value_docs(Values),
                  list=IsList, union=IsUnion, doc=Doc};
        #restriction{base_type=_Base, values=Rvals} = Restriction ->
            BaseRec = to_base(Restriction),
            BaseRec#base{restrictions=Rvals, list=IsList, union=IsUnion,
                         doc=Doc}
    end.

%% This process can handle unions by finding them in the all processed
%% simple types handles above by process_all_simple
process_simple(#simple_type{restrictions=Rs, order=Order,
                            unionmembers=Members, doc=Doc}, Ts) ->
    %% logger:notice("St: ~tp~n", [St]),
    IsList = case Order of list -> true; _ -> false end,
    IsUnion = case Order of union -> true; _ -> false end,
    case {Rs, IsUnion} of
        {#enumeration{base_type=_Base, values=Values} = Enum, false} ->
            #enum{type=to_base(Enum), values=enum_values(Values),
                  value_docs=enum_value_docs(Values),
                  list=IsList, union=IsUnion, doc=Doc};
        {#restriction{base_type=_Base, values=Rvals} = Restriction, false} ->
            BaseRec = to_base(Restriction),
            BaseRec#base{restrictions=Rvals, list=IsList, union=IsUnion,
                         doc=Doc};
        {undefined, true} ->
            %% FIXME: get all the base types and restrictions for this union
            First = hd(Members),
            case proplists:get_value(First, Ts) of
                undefined ->
                    error({non_existent_simple_type, First, Ts});
                #base{} = BaseRec->
                    BaseRec#base{list=IsList, union=IsUnion,
                                 doc=doc_or(Doc, BaseRec#base.doc)}
            end
    end.

parse_meta(#element{default=D, fixed=F, nillable=N,
                    min_occurs=Min, max_occurs=Max}) ->
    #meta{nillable=N, default=D, fixed=F, min=def_one(Min), max=def_one(Max)}.

def_one(undefined) -> 1;
def_one(Any) -> Any.

%% A declaration that references another one documents itself if it says
%% anything; otherwise it inherits what the referenced declaration says.
doc_or(undefined, Doc) -> Doc;
doc_or(Doc, _) -> Doc.

enum_values(Values) ->
    [ {ews_alias:create({ok, Str}), Str} || {enumeration, Str, _} <- Values ].

enum_value_docs(Values) ->
    [ {Str, Doc} || {enumeration, Str, Doc} <- Values, Doc /= undefined ].

simple_doc(#base{doc=Doc}) -> Doc;
simple_doc(#enum{doc=Doc}) -> Doc;
simple_doc(_) -> undefined.

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

%% A schema with one global element wrapping four local ones: a plain local,
%% one forced qualified, one forced unqualified, and one with an annotation and
%% an inline simpleType (a shape that used to trip up namespace propagation).
form_schema(ElementFormDefault) ->
    "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
        " targetNamespace=\"urn:t\"" ++ ElementFormDefault ++ ">"
        "<xsd:element name=\"top\"><xsd:complexType><xsd:sequence>"
        "<xsd:element name=\"plain\" type=\"xsd:string\"/>"
        "<xsd:element name=\"q\" form=\"qualified\" type=\"xsd:string\"/>"
        "<xsd:element name=\"u\" form=\"unqualified\" type=\"xsd:string\"/>"
        "<xsd:element name=\"doc\">"
        "<xsd:annotation><xsd:documentation>d</xsd:documentation>"
        "</xsd:annotation>"
        "<xsd:simpleType><xsd:restriction base=\"xsd:string\"/></xsd:simpleType>"
        "</xsd:element>"
        "</xsd:sequence></xsd:complexType></xsd:element></xsd:schema>".

%% A schema whose annotations sit in the three places that matter: on the
%% schema itself, on a named complexType, and on a local element.
doc_schema() ->
    "<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
        " targetNamespace=\"urn:t\">"
        "<xsd:annotation><xsd:documentation>Version 2.0</xsd:documentation>"
        "</xsd:annotation>"
        "<xsd:complexType name=\"T\">"
        "<xsd:annotation><xsd:documentation>What T is for.</xsd:documentation>"
        "</xsd:annotation>"
        "<xsd:sequence>"
        "<xsd:element name=\"a\" type=\"xsd:string\">"
        "<xsd:annotation><xsd:documentation>What a is for.</xsd:documentation>"
        "</xsd:annotation>"
        "</xsd:element>"
        "<xsd:element name=\"b\" type=\"xsd:string\"/>"
        "</xsd:sequence></xsd:complexType></xsd:schema>".

%% The schema's own annotation documents the document, not a declaration, so
%% it is dropped rather than left to look like a type. The other two are kept
%% on the declaration they belong to, and out of its parts.
annotation_test() ->
    {Schema, []} = xmerl_scan:string(doc_schema(),
                                     [{space, normalize},
                                      {namespace_conformant, true}]),
    [#complex_type{doc = TypeDoc, parts = Parts}] = parse_types(Schema),
    ?assertEqual(<<"What T is for.">>, TypeDoc),
    %% Bare names: the fixture declares no elementFormDefault.
    ?assertEqual([{"a", <<"What a is for.">>}, {"b", undefined}],
                 [ {N, D} || #element{name = N, doc = D} <- Parts ]),
    ?assertEqual([[], []], [ P || #element{parts = P} <- Parts ]).

parse_form_schema(ElementFormDefault) ->
    {Schema, []} = xmerl_scan:string(form_schema(ElementFormDefault),
                                     [{space, normalize},
                                      {namespace_conformant, true}]),
    [#element{name = TopName, parts = [#complex_type{parts = Locals}]}] =
        parse_types(Schema),
    {TopName, [ N || #element{name = N} <- Locals ]}.

%% No elementFormDefault means "unqualified", so only the element that asks
%% for it is qualified. The global wrapper is qualified either way.
element_form_default_unqualified_test() ->
    ?assertEqual({{"urn:t", "top"}, ["plain", {"urn:t", "q"}, "u", "doc"]},
                 parse_form_schema("")).

element_form_default_qualified_test() ->
    ?assertEqual({{"urn:t", "top"},
                  [{"urn:t", "plain"}, {"urn:t", "q"}, "u", {"urn:t", "doc"}]},
                 parse_form_schema(" elementFormDefault=\"qualified\"")).

-endif.
