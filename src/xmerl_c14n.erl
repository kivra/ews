%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2022-2023 Kivra
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
%% esaml - SAML for erlang
%%
%% Copyright (c) 2013, Alex Wilson and the University of Queensland
%% All rights reserved.
%%
%% Distributed subject to the terms of the 2-clause BSD license, see
%% the 2BSD file in the root of the distribution.

%% @doc XML canonocialisation for xmerl
%%
%% Functions for performing XML canonicalisation (C14n), as specified
%% at http://www.w3.org/TR/xml-c14n .
%%
%% These routines work on xmerl data structures (see the xmerl user guide
%% for details).
-module(xmerl_c14n).

-export([c14n/3, c14n/2, c14n/1, xml_safe_string/2, xml_safe_string/1, canon_name/1]).
-export([remove_xmlns_prefixes/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("public_key/include/public_key.hrl").

%% @doc Returns the canonical namespace-URI-prefix-resolved version of an XML name.
%% @private
-spec canon_name(Prefix :: string(), Name :: string() | atom(), Nsp :: #xmlNamespace{}) -> string().
canon_name(Ns, Name, Nsp) ->
    NsPartRaw = case Ns of
        empty -> Nsp#xmlNamespace.default;
        [] ->
            if Nsp == [] -> 'urn:oasis:names:tc:SAML:2.0:assertion';
               true -> Nsp#xmlNamespace.default
            end;
        _ ->
            case proplists:get_value(Ns, Nsp#xmlNamespace.nodes) of
                undefined ->
                    error({ns_not_found, Ns, Nsp});
                Uri -> atom_to_list(Uri)
            end
    end,
    NsPart = if is_atom(NsPartRaw) -> atom_to_list(NsPartRaw); true -> NsPartRaw end,
    NamePart = if is_atom(Name) -> atom_to_list(Name); true -> Name end,
    lists:flatten([NsPart | NamePart]).

%% @doc Returns the canonical URI name of an XML element or attribute.
%% @private
-spec canon_name(#xmlElement{} | #xmlAttribute{}) -> string().
canon_name(#xmlAttribute{name = Name, nsinfo = Exp, namespace = Nsp}) ->
    case Exp of
        {Ns, Nme} -> canon_name(Ns, Nme, Nsp);
        _ -> canon_name([], Name, Nsp)
    end;
canon_name(#xmlElement{name = Name, nsinfo = Exp, namespace = Nsp}) ->
    case Exp of
        {Ns, Nme} -> canon_name(Ns, Nme, Nsp);
        _ -> canon_name([], Name, Nsp)
    end.

%% @doc Compares two XML attributes for c14n purposes
-spec attr_lte(A :: #xmlAttribute{}, B :: #xmlAttribute{}) -> true | false.
attr_lte(AttrA, AttrB) ->
    logger:debug("AttrA: ~p AttrB: ~p~n", [AttrA, AttrB]),
    A = canon_name(AttrA), B = canon_name(AttrB),
    PrefixedA = case AttrA#xmlAttribute.nsinfo of {_, _} -> true; _ -> false end,
    PrefixedB = case AttrB#xmlAttribute.nsinfo of {_, _} -> true; _ -> false end,
    if (PrefixedA) andalso (not PrefixedB) ->
        false;
    (not PrefixedA) andalso (PrefixedB) ->
        true;
    true ->
        A =< B
    end.

%% @doc Cleans out all namespace definitions from an attribute list and returns it sorted.
%% @private
-spec clean_sort_attrs(Attrs :: [#xmlAttribute{}]) -> [#xmlAttribute{}].
clean_sort_attrs(Attrs) ->
    lists:sort(fun(A,B) ->
        attr_lte(A, B)
    end, lists:filter(fun(Attr) ->
        case Attr#xmlAttribute.nsinfo of
            {"xmlns", _} -> false;
            _ -> case Attr#xmlAttribute.name of
                'xmlns' -> false;
                _ -> true
            end
        end
    end, Attrs)).

%% @doc Returns the list of namespace prefixes "needed" by an element in canonical form
%% @private
-spec needed_ns(Elem :: #xmlElement{}, InclNs :: [string()]) -> [string()].
needed_ns(#xmlElement{nsinfo = NsInfo, attributes = Attrs}, InclNs) ->
    NeededNs1 = case NsInfo of
        {Nas, _} -> [Nas];
        _ -> []
    end,
    %% show through namespaces that apply at the bottom level? this part of the spec is retarded
    %%KidElems = [K || K <- Kids, element(1, K) =:= xmlElement],
    NeededNs2 = NeededNs1, %case KidElems of
        %%[] -> [K || {K,V} <- E#xmlElement.namespace#xmlNamespace.nodes];
        %%_ -> NeededNs1
    %%end,
    lists:foldl(fun(Attr, Needed) ->
        case Attr#xmlAttribute.nsinfo of
            {"xmlns", Prefix} ->
                case lists:member(Prefix, InclNs) and not lists:member(Prefix, Needed) of
                    true -> [Prefix | Needed];
                    _ -> Needed
                end;
            {Ns, _Name} ->
                case lists:member(Ns, Needed) of
                    true -> Needed;
                    _ -> [Ns | Needed]
                end;
            _ -> Needed
        end
    end, NeededNs2, Attrs).

%% @doc Make xml ok to eat, in a non-quoted situation.
%% @private
-spec xml_safe_string(term()) -> string().
xml_safe_string(Term) -> xml_safe_string(Term, false).

%% @doc Make xml ok to eat
%% @private
-spec xml_safe_string(String :: term(), Quotes :: boolean()) -> string().
xml_safe_string(Atom, Quotes) when is_atom(Atom) -> xml_safe_string(atom_to_list(Atom), Quotes);
xml_safe_string(Bin, Quotes) when is_binary(Bin) -> xml_safe_string(binary_to_list(Bin), Quotes);
xml_safe_string([], _) -> [];
xml_safe_string(Str, Quotes) when is_list(Str) ->
    [Next | Rest] = Str,
    if
        (not Quotes andalso ([Next] =:= "\n")) -> [Next | xml_safe_string(Rest, Quotes)];
        (Next < 32) ->
            lists:flatten(["&#x" ++ integer_to_list(Next, 16) ++ ";" | xml_safe_string(Rest, Quotes)]);
        (Quotes andalso ([Next] =:= "\"")) -> lists:flatten(["&quot;" | xml_safe_string(Rest, Quotes)]);
        ([Next] =:= "&") -> lists:flatten(["&amp;" | xml_safe_string(Rest, Quotes)]);
        ([Next] =:= "<") -> lists:flatten(["&lt;" | xml_safe_string(Rest, Quotes)]);
        (not Quotes andalso ([Next] =:= ">")) -> lists:flatten(["&gt;" | xml_safe_string(Rest, Quotes)]);
        true -> [Next | xml_safe_string(Rest, Quotes)]
    end;
xml_safe_string(Term, Quotes) ->
    xml_safe_string(io_lib:format("~p", [Term]), Quotes).

%% @doc Worker function for canonicalisation (c14n). It accumulates the canonical string data
%%      for a given XML "thing" (element/attribute/whatever)
%% @private
-type xml_thing() :: #xmlDocument{} | #xmlElement{} | #xmlAttribute{} | #xmlPI{} | #xmlText{} | #xmlComment{}.
-spec c14n(XmlThing :: xml_thing(), KnownNs :: [{string(), string()}], ActiveNS :: [string()], Comments :: boolean(), InclNs :: [string()], Acc :: [string() | number()]) -> [string() | number()].

c14n(#xmlText{value = Text}, _KnownNS, _ActiveNS, _Comments, _InclNs, Acc) ->
    [xml_safe_string(Text) | Acc];
c14n(#xmlComment{value = Text}, _KnownNS, _ActiveNS, true, _InclNs, Acc) ->
    ["-->", xml_safe_string(Text), "<!--" | Acc];
c14n(#xmlPI{name = Name, value = Value}, _KnownNS, _ActiveNS, _Comments, _InclNs, Acc) ->
    NameString = if is_atom(Name) -> atom_to_list(Name); true -> string:strip(Name) end,
    case string:strip(Value) of
        [] -> ["?>", NameString, "<?" | Acc];
        _ -> ["?>", Value, " ", NameString, "<?" | Acc]
    end;
c14n(#xmlDocument{content = Kids}, KnownNS, ActiveNS, Comments, InclNs, Acc) ->
    case lists:foldl(fun(Kid, AccIn) ->
                             case c14n(Kid, KnownNS, ActiveNS, Comments, InclNs, AccIn) of
                                 AccIn -> AccIn;
                                 Other -> ["\n" | Other]
                             end
                     end, Acc, Kids) of
        ["\n" | Rest] -> Rest;
        Other -> Other
    end;
c14n(#xmlAttribute{nsinfo = NsInfo, name = Name, value = Value}, _KnownNs, ActiveNS, _Comments, _InclNs, Acc) ->
    case NsInfo of
        {Ns, NName} ->
            case lists:member(Ns, ActiveNS) of
                true -> ["\"",xml_safe_string(Value, true),"=\"",NName,":",Ns," " | Acc];
                _ -> error("attribute namespace is not active")
            end;
        _ ->
            ["\"",xml_safe_string(Value, true),"=\"",atom_to_list(Name)," " | Acc]
    end;
c14n(Elem = #xmlElement{}, KnownNSIn, ActiveNSIn, Comments, InclNs, Acc) ->
    Namespace = Elem#xmlElement.namespace,
    Default = case Elem#xmlElement.nsinfo of
                  [] -> Namespace#xmlNamespace.default;
                  _ -> [] % omit a default namespace if it is not visibly utilized.
              end,
    {ActiveNS, ParentDefault} = case ActiveNSIn of
                                    [{default, P} | Rest] -> {Rest, P};
                                    Other -> {Other, ''}
                                end,
    %% add any new namespaces this element has that we haven't seen before
    KnownNS = lists:foldl(fun({Ns, Uri}, Nss) ->
                                  case proplists:is_defined(Ns, Nss) of
                                      true -> Nss;
                                      _ -> [{Ns, atom_to_list(Uri)} | Nss]
                                  end
                          end, KnownNSIn, Namespace#xmlNamespace.nodes),

    %% now figure out the minimum set of namespaces we need at this level
    NeededNs = needed_ns(Elem, InclNs),
    %% and all of the attributes that aren't xmlns
    Attrs = clean_sort_attrs(Elem#xmlElement.attributes),

    %% we need to append any xmlns: that our parent didn't have (ie, aren't in ActiveNS) but
    %% that we need
    %% This also removes duplicates from InclNs that are in NeededNs
    NewNS = (InclNs -- NeededNs) ++ NeededNs -- ActiveNS,
    NewActiveNS = ActiveNS ++ NewNS,

    %% the opening tag
    Acc1 = case Elem#xmlElement.nsinfo of
               {ENs, EName} ->
                   [EName, ":", ENs, "<" | Acc];
               _ ->
                   [atom_to_list(Elem#xmlElement.name), "<" | Acc]
           end,
    %% xmlns definitions
    {Acc2, FinalActiveNS} =
        if
            not (Default =:= []) andalso not (Default =:= ParentDefault) ->
                {["\"", xml_safe_string(Default, true), " xmlns=\"" | Acc1],
                 [{default, Default} | NewActiveNS]};
            not (Default =:= []) ->
                {Acc1, [{default, Default} | NewActiveNS]};
            true ->
                {Acc1, NewActiveNS}
        end,
    Acc3 = lists:foldl(fun(Ns, AccIn) ->
                               ["\"",xml_safe_string(
                                       proplists:get_value(Ns, KnownNS, ""),
                                       true),"=\"",Ns,":"," xmlns" | AccIn]
                       end, Acc2, lists:sort(NewNS)),
    %% any other attributes
    Acc4 = lists:foldl(fun(Attr, AccIn) ->
                               c14n(Attr, KnownNS, FinalActiveNS, Comments, [], AccIn)
                       end, Acc3, Attrs),
    %% close the opening tag
    Acc5 = [">" | Acc4],

    %% now accumulate all our children
    Acc6 = lists:foldl(fun(Kid, AccIn) ->
                               c14n(Kid, KnownNS, FinalActiveNS, Comments, [], AccIn)
                       end, Acc5, Elem#xmlElement.content),

    %% and finally add the close tag
    case Elem#xmlElement.nsinfo of
        {Ns, Name} ->
            [">", Name, ":", Ns, "</" | Acc6];
        _ ->
            [">",atom_to_list(Elem#xmlElement.name),"</" | Acc6]
    end;
%% I do not give a shit
c14n(_, _KnownNS, _ActiveNS, _Comments, _InclNs, Acc) ->
    Acc.

%% @doc Puts an XML document or element into canonical form, as a string.
-spec c14n(XmlThing :: xml_thing()) -> string().
c14n(Elem) ->
    c14n(Elem, true).

%% @doc Puts an XML document or element into canonical form, as a string.
%%
%% If the Comments argument is true, preserves comments in the output.
-spec c14n(XmlThing :: xml_thing(), Comments :: boolean()) -> string().
c14n(Elem, Comments) ->
    c14n(Elem, Comments, []).

%% @doc Puts an XML document or element into exclusive canonical form, as a string.
%%
%% From what we could understand, the original authors code implemented
%% inclusive canonical form, since the unit tests are examples from that
%% standard document.
%%
%% We have now tried to change it to implement exclusive c14n according to
%% this standard.
%%
%%     http://www.w3.org/TR/xml-exc-c14n/
%%
%% Inclusive Namespaces is now treated as if they come from an envelope
%% and they are rendered in the first tag.
%%
%% If the Comments argument is true, preserves comments in the output. Any
%% namespace prefixes listed in InclusiveNs will be left as they are and not
%% modified during canonicalization.
-spec c14n(XmlThing :: xml_thing(), Comments :: boolean(), InclusiveNs :: [string()]) -> string().
c14n(Elem, Comments, InclusiveNs) ->
    {NakedNss, _} = naked_nss(Elem, #{}, 0),
    logger:debug("NakedNss: ~p~n", [lists:keysort(2, maps:to_list(NakedNss))]),
    SortedNss = [ Prefix || {Prefix, _} <-
                               lists:keysort(2, maps:to_list(NakedNss)) ],
    FirstTagNss = lists:usort(InclusiveNs ++ SortedNss),
    lists:flatten(lists:reverse(c14n(Elem, [], [], Comments, FirstTagNss, []))).

naked_nss(#xmlDocument{content = Kids}, NSMap, Seq) ->
    naked_nss(Kids, NSMap, Seq);
naked_nss(#xmlElement{content = Kids, nsinfo = {Prefix, _},
                      attributes = Attrs, namespace = Ns}, NSMap1, Seq1) ->
    case [ A || #xmlAttribute{nsinfo = {"xmlns", P}} = A <- Attrs, P == Prefix ] of
        [#xmlAttribute{}] ->
            %% ns defined here, so not needed
            naked_nss(Kids, NSMap1, Seq1);
        [] ->
            case Ns#xmlNamespace.default of
                [] ->
                    %% No defaultns, so we need this
                    {NSMap2, Seq2} = insert_ns(Prefix, NSMap1, Seq1),
                    naked_nss(Kids, NSMap2, Seq2);
                _ ->
                    %% Element has defaultns, so not needed
            naked_nss(Kids, NSMap1, Seq1)
            end
    end;
naked_nss(#xmlElement{content = Kids}, NSMap, Seq) ->
    naked_nss(Kids, NSMap, Seq);
naked_nss(#xmlText{}, NSMap, Seq) ->
    {NSMap, Seq};
naked_nss(#xmlComment{}, NSMap, Seq) ->
    {NSMap, Seq};
naked_nss(#xmlPI{}, NSMap, Seq) ->
    {NSMap, Seq};
naked_nss([H | T], NSMap1, Seq1) ->
    {NSMap2, Seq2} = naked_nss(H, NSMap1, Seq1),
    naked_nss(T, NSMap2, Seq2);
naked_nss([], NSMap, Seq) ->
    {NSMap, Seq}.

insert_ns(NS, NSMap, Seq) ->
    case maps:is_key(NS, NSMap) of
        true ->
            {NSMap, Seq};
        false ->
            {NSMap#{NS => Seq}, Seq+1}
    end.

remove_xmlns_prefixes(Xml) ->
    do_remove_xmlns_prefixes(Xml, '').

do_remove_xmlns_prefixes(#xmlElement{ content = Kids
                                    , nsinfo = []
                                    , attributes = Attrs
                                    } = Element, DefaultNs) ->
    NewKids = do_remove_xmlns_prefixes(Kids, DefaultNs),
    NewAttrs = strip_xmlns_attrs(Attrs),
    Element#xmlElement{content = NewKids, attributes = NewAttrs};
do_remove_xmlns_prefixes(#xmlElement{ content = Kids
                                    , nsinfo = {_Prefix, Name}
                                    , attributes = Attrs
                                    } = Element, DefaultNs) ->
    NewDefaultNs = get_defaultns(Element),
    NewKids = do_remove_xmlns_prefixes(Kids, NewDefaultNs),
    NewName = list_to_atom(Name),
    NewAttrs = add_defaultns(strip_xmlns_attrs(Attrs), DefaultNs, NewDefaultNs),
    Element#xmlElement{content = NewKids
                      , attributes = NewAttrs
                      , name = NewName
                      , expanded_name = NewName
                      , nsinfo = []
                      };
do_remove_xmlns_prefixes(#xmlText{} = Text, _) ->
    Text;
do_remove_xmlns_prefixes(#xmlComment{} = Comment, _) ->
    Comment;
do_remove_xmlns_prefixes([H | T], DefaultNs) ->
    NewKid = do_remove_xmlns_prefixes(H, DefaultNs),
    [NewKid | do_remove_xmlns_prefixes(T, DefaultNs)];
do_remove_xmlns_prefixes([], _) ->
    [].

strip_xmlns_attrs(Attrs) ->
    lists:filter(fun strip_xmlns/1, Attrs).

strip_xmlns(#xmlAttribute{nsinfo = {"xmlns", _}}) -> false;
strip_xmlns(#xmlAttribute{}) -> true.

add_defaultns(Attrs, '', '') ->
    Attrs;
add_defaultns(Attrs, DefaultNs, DefaultNs) ->
    Attrs;
add_defaultns(Attrs, _DefaultNs, NewDefaultNs) ->
    [#xmlAttribute{ name = xmlns
                  , pos = 1
                  , value = atom_to_list(NewDefaultNs)
                  , normalized = false
                  } | Attrs].

get_defaultns(#xmlElement{ nsinfo = {Prefix, _Name}
                         , namespace = Nss
                         }) ->
    {Prefix, URI} = lists:keyfind(Prefix, 1, Nss#xmlNamespace.nodes),
    URI.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec build_nsinfo(#xmlNamespace{}, #xmlElement{}) -> #xmlElement{}.
build_nsinfo(Ns, Attr = #xmlAttribute{name = Name}) ->
    case string:tokens(atom_to_list(Name), ":") of
        [NsPrefix, Rest] -> Attr#xmlAttribute{namespace = Ns, nsinfo = {NsPrefix, Rest}};
        _ -> Attr#xmlAttribute{namespace = Ns}
    end;
build_nsinfo(Ns, Elem = #xmlElement{name = Name, content = Kids, attributes = Attrs}) ->
    Elem2 = case string:tokens(atom_to_list(Name), ":") of
        [NsPrefix, Rest] -> Elem#xmlElement{namespace = Ns, nsinfo = {NsPrefix, Rest}};
        _ -> Elem#xmlElement{namespace = Ns}
    end,
    Elem2#xmlElement{attributes = [build_nsinfo(Ns, Attr) || Attr <- Attrs],
                    content = [build_nsinfo(Ns, Kid) || Kid <- Kids]};
build_nsinfo(_Ns, Other) -> Other.

canon_name_test() ->
    "urn:foo:Blah" = canon_name("foo", "Blah", #xmlNamespace{nodes = [{"foo", 'urn:foo:'}]}),
    "urn:foo:Blah" = canon_name("foo", 'Blah', #xmlNamespace{nodes = [{"foo", 'urn:foo:'}]}),
    {'EXIT', {{ns_not_found, _, _}, _}} = (catch canon_name("foo", "Blah", #xmlNamespace{})),
    "urn:bar:Blah" = canon_name("bar", "Blah", #xmlNamespace{nodes = [{"bar", 'urn:bar:'}]}).
canon_name_attr_test() ->
    "urn:foo:Blah" = canon_name(#xmlAttribute{name = 'Blah', nsinfo = {"foo", "Blah"}, namespace = #xmlNamespace{nodes = [{"foo", 'urn:foo:'}]}}).
canon_name_elem_test() ->
    "urn:foo:Blah" = canon_name(#xmlElement{name = 'Blah', nsinfo = {"foo", "Blah"}, namespace = #xmlNamespace{nodes = [{"foo", 'urn:foo:'}]}}).

canon_name_default_ns_test() ->
    {Doc, []} = xmerl_scan:string("<foo:a xmlns:foo=\"urn:foo:\"><b xmlns=\"urn:bar:\"></b></foo:a>", [{namespace_conformant, true}]),
    A = #xmlElement{content = [B]} = Doc,
    "urn:foo:a" = canon_name(A),
    "urn:bar:b" = canon_name(B).

needed_ns_test() ->
    Ns = #xmlNamespace{nodes = [{"foo", 'urn:foo:'}, {"bar", 'urn:bar:'}]},

    E1 = build_nsinfo(Ns, #xmlElement{name = 'foo:Blah', attributes = [#xmlAttribute{name = 'bar:name', value="foo"}]}),
    ["bar", "foo"] = lists:sort(needed_ns(E1, [])),

    E2 = build_nsinfo(Ns, #xmlElement{name = 'Blah', attributes = [#xmlAttribute{name = 'bar:name', value = "foo"}]}),
    ["bar"] = needed_ns(E2, []),

    E3 = build_nsinfo(Ns, #xmlElement{name = 'Blah', attributes = [#xmlAttribute{name = 'name', value = "foo"}], content = [#xmlElement{name = 'foo:InnerBlah'}]}),
    [] = needed_ns(E3, []),

    E4 = build_nsinfo(Ns, #xmlElement{name = 'Blah'}),
    [] = needed_ns(E4, []),
    [] = needed_ns(E4, ["foo"]),

    {E5, []} = xmerl_scan:string("<foo:a xmlns:foo=\"urn:foo:\" xmlns:bar=\"urn:bar:\"><foo:b bar:nothing=\"something\">foo</foo:b></foo:a>", [{namespace_conformant, true}]),
    ["foo"] = needed_ns(E5, []),
    ["bar", "foo"] = needed_ns(E5, ["bar"]).

xml_safe_string_test() ->
    "foo" = xml_safe_string('foo'),
    "foo \ngeorge" = xml_safe_string(<<"foo \ngeorge">>),
    "foo &lt;&#x5;&gt; = &amp; help" = xml_safe_string(lists:flatten(["foo <", 5, "> = & help"])),
    "&#xE;" = xml_safe_string(<<14>>),
    "\"foo\"" = xml_safe_string("\"foo\""),
    "test&#xD;\n" = xml_safe_string("test\r\n").

xml_safe_string_utf8_test() ->
    String = unicode:characters_to_list(<<"バカの名前"/utf8>>),
    String = xml_safe_string(String).

c14n_3_1_test() ->
    {Doc, _} = xmerl_scan:string("<?xml version=\"1.0\"?>\n\n<?xml-stylesheet   href=\"doc.xsl\"\n   type=\"text/xsl\"   ?>\n\n<doc>Hello, world!<!-- Comment 1 --></doc>\n\n<?pi-without-data     ?>\n\n<!-- Comment 2 -->\n\n<!-- Comment 3 -->", [{namespace_conformant, true}, {document, true}]),
    WithoutComments = "<?xml-stylesheet href=\"doc.xsl\"\n   type=\"text/xsl\"   ?>\n<doc>Hello, world!</doc>\n<?pi-without-data?>",
    WithoutComments = c14n(Doc, false),

    WithComments = "<?xml-stylesheet href=\"doc.xsl\"\n   type=\"text/xsl\"   ?>\n<doc>Hello, world!<!-- Comment 1 --></doc>\n<?pi-without-data?>\n<!-- Comment 2 -->\n<!-- Comment 3 -->",
    WithComments = c14n(Doc, true).

c14n_3_2_test() ->
    {Doc, _} = xmerl_scan:string("<doc>\n   <clean>   </clean>\n   <dirty>   A   B   </dirty>\n   <mixed>\n      A\n      <clean>   </clean>\n      B\n      <dirty>   A   B   </dirty>\n      C\n   </mixed>\n</doc>", [{namespace_conformant, true}, {document, true}]),

    Target = "<doc>\n   <clean>   </clean>\n   <dirty>   A   B   </dirty>\n   <mixed>\n      A\n      <clean>   </clean>\n      B\n      <dirty>   A   B   </dirty>\n      C\n   </mixed>\n</doc>",
    Target = c14n(Doc, true).

c14n_3_3_test() ->
    {Doc, _} = xmerl_scan:string("<!DOCTYPE doc [<!ATTLIST e9 attr CDATA \"default\">]>\n<doc>\n   <e1   />\n   <e2   ></e2>\n   <e3   name = \"elem3\"   id=\"elem3\"   />\n   <e4   name=\"elem4\"   id=\"elem4\"   ></e4>\n   <e5 a:attr=\"out\" b:attr=\"sorted\" attr2=\"all\" attr=\"I'm\"\n      xmlns:b=\"http://www.ietf.org\"\n      xmlns:a=\"http://www.w3.org\"\n      xmlns=\"http://example.org\"/>\n   <e6 xmlns=\"\" xmlns:a=\"http://www.w3.org\">\n      <e7 xmlns=\"http://www.ietf.org\">\n         <e8 xmlns=\"\" xmlns:a=\"http://www.w3.org\">\n            <e9 xmlns=\"\" xmlns:a=\"http://www.ietf.org\"/>\n         </e8>\n      </e7>\n   </e6>\n</doc>", [{namespace_conformant, true}, {document, true}]),

    Target = "<doc>\n   <e1></e1>\n   <e2></e2>\n   <e3 id=\"elem3\" name=\"elem3\"></e3>\n   <e4 id=\"elem4\" name=\"elem4\"></e4>\n   <e5 xmlns=\"http://example.org\" xmlns:a=\"http://www.w3.org\" xmlns:b=\"http://www.ietf.org\" attr=\"I'm\" attr2=\"all\" b:attr=\"sorted\" a:attr=\"out\"></e5>\n   <e6>\n      <e7 xmlns=\"http://www.ietf.org\">\n         <e8 xmlns=\"\">\n            <e9></e9>\n         </e8>\n      </e7>\n   </e6>\n</doc>",
    Target = c14n(Doc, true).

default_ns_test() ->
    {Doc, _} = xmerl_scan:string("<foo:a xmlns:foo=\"urn:foo:\"><b xmlns=\"urn:bar:\"><c xmlns=\"urn:bar:\" /></b><c xmlns=\"urn:bar:\"><d /></c><foo:e><f xmlns=\"urn:foo:\"><foo:x>blah</foo:x></f></foo:e></foo:a>", [{namespace_conformant, true}]),

    Target = "<foo:a xmlns:foo=\"urn:foo:\"><b xmlns=\"urn:bar:\"><c></c></b><c xmlns=\"urn:bar:\"><d></d></c><foo:e><f xmlns=\"urn:foo:\"><foo:x>blah</foo:x></f></foo:e></foo:a>",
    Target = c14n(Doc, true),

    {Doc2, _} = xmerl_scan:string("<?xml version=\"1.0\" encoding=\"UTF-8\"?><saml2p:Response xmlns:saml2p=\"urn:oasis:names:tc:SAML:2.0:protocol\" ID=\"_83dbf3f1-53c2-4f49-b294-7c19cbf2b77b\" Version=\"2.0\" IssueInstant=\"2013-10-30T11:15:47.517Z\" Destination=\"https://10.10.18.25/saml/consume\"><Assertion xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\" Version=\"2.0\" ID=\"_debe5f4e-4343-4f95-b997-89db5a483202\" IssueInstant=\"2013-10-30T11:15:47.517Z\"><Issuer>foo</Issuer><Subject><NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\"/><SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"><SubjectConfirmationData NotOnOrAfter=\"2013-10-30T12:15:47.517Z\" Recipient=\"https://10.10.18.25/saml/consume\"/></SubjectConfirmation></Subject></Assertion></saml2p:Response>", [{namespace_conformant, true}]),

    Target2 = "<saml2p:Response xmlns:saml2p=\"urn:oasis:names:tc:SAML:2.0:protocol\" Destination=\"https://10.10.18.25/saml/consume\" ID=\"_83dbf3f1-53c2-4f49-b294-7c19cbf2b77b\" IssueInstant=\"2013-10-30T11:15:47.517Z\" Version=\"2.0\"><Assertion xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\" ID=\"_debe5f4e-4343-4f95-b997-89db5a483202\" IssueInstant=\"2013-10-30T11:15:47.517Z\" Version=\"2.0\"><Issuer>foo</Issuer><Subject><NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified\"></NameID><SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\"><SubjectConfirmationData NotOnOrAfter=\"2013-10-30T12:15:47.517Z\" Recipient=\"https://10.10.18.25/saml/consume\"></SubjectConfirmationData></SubjectConfirmation></Subject></Assertion></saml2p:Response>",
    Target2 = c14n(Doc2, true).

omit_default_ns_test() ->
    {Doc, _} = xmerl_scan:string("<foo:a xmlns:foo=\"urn:foo\"><bar:b xmlns=\"urn:bar\" xmlns:bar=\"urn:bar\"><bar:c /></bar:b></foo:a>", [{namespace_conformant, true}]),

    Target = "<foo:a xmlns:foo=\"urn:foo\"><bar:b xmlns:bar=\"urn:bar\"><bar:c></bar:c></bar:b></foo:a>",
    Target = c14n(Doc, true).

c14n_inclns_test() ->
    {Doc, []} = xmerl_scan:string("<foo:a xmlns:foo=\"urn:foo:\" xmlns:bar=\"urn:bar:\"><foo:b bar:nothing=\"something\">foo</foo:b></foo:a>", [{namespace_conformant, true}]),

    Target1 = "<foo:a xmlns:foo=\"urn:foo:\"><foo:b xmlns:bar=\"urn:bar:\" bar:nothing=\"something\">foo</foo:b></foo:a>",
    Target1 = c14n(Doc, false),

    Target2 = "<foo:a xmlns:bar=\"urn:bar:\" xmlns:foo=\"urn:foo:\"><foo:b bar:nothing=\"something\">foo</foo:b></foo:a>",
    Target2 = c14n(Doc, false, ["bar"]).
-endif.
