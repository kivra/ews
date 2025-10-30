%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2013-2017 Campanja
%%% Copyright (c) 2017-2020 [24]7.ai
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
-module(ews_xml).

-export([encode/1,
         encode/2,
         decode/1,
         compare/2,
         get_all_nss/1
        ]).

-define(XML_NS, "http://www.w3.org/XML/1998/namespace").

-type name() :: atom() | string().
-type qname() :: name() | {name(), name()}.
-type xml_data() :: {qname(), [{name(), string()}], list()}.

%% ----------------------------------------------------------------------------
%% Api

-spec encode(xml_data() | list(xml_data())) -> iolist().
encode(Data) when is_tuple(Data) andalso size(Data) == 3 ->
    emit_tag(Data, []);
encode(Data) when is_list(Data) ->
    [ emit_tag(D, []) || D <- Data ].


encode(Data, Nss) when is_tuple(Data) andalso size(Data) == 3 ->
    emit_tag(Data, Nss);
encode(Data, Nss) when is_list(Data) ->
    [ emit_tag(D, Nss) || D <- Data ].

-spec decode(string()|binary()) -> xml_data() | [xml_data()].
decode(XmlString) when is_binary(XmlString) ->
    decode(binary_to_list(XmlString));
decode(XmlString) when is_list(XmlString) ->
    Tokens = scan_tag(XmlString, [], [], []),
    parse_xml(Tokens, [], []).

get_all_nss(Data) when is_tuple(Data) andalso size(Data) == 3 ->
    do_get_all_nss([Data], #{?XML_NS => false});
get_all_nss(Data) when is_list(Data) ->
    do_get_all_nss(Data, #{?XML_NS => false}).

do_get_all_nss([Head | Tail], NssMap) ->
    NewNssMap = ns_tag(Head, NssMap),
    do_get_all_nss(Tail, NewNssMap);
do_get_all_nss([], NssMap0) ->
    NssMap1 = maps:filter(fun(_, V) -> V end, NssMap0),
    NssList = lists:reverse(maps:keys(NssMap1)),
    {Attrs, NewNss} = lists:foldr(fun(Ns, {Acc, Nss0}) ->
                                          {_, XmlDecl, Nss1} =
                                              get_ns_prefix(Ns, Nss0),
                                          {[XmlDecl | Acc], Nss1}
                                  end,
                                  {[], []}, NssList),
    {lists:flatten(lists:reverse(Attrs)), NewNss}.

%% ----------------------------------------------------------------------------
%% encode

emit_tag({txt, Content}, _) ->
    export_text(Content);
emit_tag({{Ns, txt}, Content}, Nss) ->
    %% Ugly hack for namespaced txts
    {Prefix, _XmlNsDecl, _NewNss} = get_ns_prefix(Ns, Nss),
    to_string({Prefix, export_text(Content)});
emit_tag({{Ns, Name}, Attributes, Children}, Nss) ->
    {Prefix, XmlNsDecl, NewNss} = get_ns_prefix(Ns, Nss),
    QName = {Prefix, Name},
    [emit_start_tag(QName),
     %% don't use ukeysort here, that would hide if there is a bug
     %% in the xsds with duplicate namespaces.
     emit_attributes(lists:keysort(2, XmlNsDecl)++Attributes, NewNss),
     emit_children(QName, Children, NewNss)];
emit_tag({Name, Attributes, Children}, Nss) ->
    [emit_start_tag(Name),
     emit_attributes(Attributes, Nss),
     emit_children(Name, Children, Nss)].

emit_attributes([], _) ->
    "";
emit_attributes(Attributes, NssStore) ->
    emit_attributes(Attributes, NssStore, []).

emit_attributes([{{NsN, N}, {NsV, V}} | Attrs], Nss, Acc) ->
    {PrefixN, XmlNsDeclN, NewNss} = get_ns_prefix(NsN, Nss),
    {PrefixV, XmlNsDeclV, NewerNss} = get_ns_prefix(NsV, NewNss),
    NewAttrs = lists:usort(XmlNsDeclN ++ XmlNsDeclV) ++
               [{to_string({PrefixN, N}), to_string({PrefixV, V})}] ++ Attrs,
    emit_attributes(NewAttrs, NewerNss, Acc);
emit_attributes([{{Ns, N}, Value} | Attrs], Nss, Acc) ->
    {Prefix, XmlNsDecl, NewNss} = get_ns_prefix(Ns, Nss),
    NewAttrs = XmlNsDecl++[{to_string({Prefix, N}), Value}]++Attrs,
    emit_attributes(NewAttrs, NewNss, Acc);
emit_attributes([{Key, Value} | Attrs], Nss, Acc) ->
    emit_attributes(Attrs, Nss, [[to_string(Key), "=\"", Value, "\""] | Acc]);
emit_attributes([], _, Acc) ->
    [" ", string:join(lists:reverse(Acc), " ")].

emit_children([$?|_], _, []) ->
    "?>";
emit_children(_, _, []) ->
    "/>";
emit_children(Name, Children, Nss) ->
    [">", [ emit_tag(C, Nss) || C <- Children ], emit_end_tag(Name)].

emit_start_tag({Ns, Name}) ->
    ["<", to_string(Ns), ":", to_string(Name)];
emit_start_tag(Name) ->
    ["<", to_string(Name)].

emit_end_tag({Ns, Name}) ->
    ["</", to_string(Ns), ":", to_string(Name), ">"];
emit_end_tag(Name) ->
    ["</", to_string(Name), ">"].

to_string({Ns, Name}) -> [Ns, ":", Name];
to_string(Name) when is_atom(Name) -> atom_to_list(Name);
to_string(Name) when is_binary(Name) -> binary_to_list(Name);
to_string(Name) when is_list(Name) -> Name.

%% Escape special characters `\r` `<' and `&', flattening the text.
%% Also escapes `>', just for symmetry.

export_text(T) ->
    export_text(T, []).

export_text([$\r | T], Cont) ->
    "&#13;" ++ export_text(T, Cont);
export_text([$< | T], Cont) ->
    "&lt;" ++ export_text(T, Cont);
export_text([$> | T], Cont) ->
    "&gt;" ++ export_text(T, Cont);
export_text([$& | T], Cont) ->
    "&amp;" ++ export_text(T, Cont);
export_text([C | T], Cont) when is_integer(C) ->
    [C | export_text(T, Cont)];
export_text([T | T1], Cont) ->
    export_text(T, [T1 | Cont]);
export_text([], [T | Cont]) ->
    export_text(T, Cont);
export_text([], []) ->
    [];
export_text(Bin, Cont) ->
    export_text(binary_to_list(Bin), Cont).

%% ----------------------------------------------------------------------------
%% decode

scan_tag([$<, $/ | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [$/ ,$<], [], [{txt, Txt} | Acc]);
scan_tag([$< | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [$<], [], [{txt, Txt} | Acc]);
scan_tag([$> | Rest], Tag, [], Acc) ->
    Element = parse_tag(lists:reverse([$> | Tag])),
    scan_tag(Rest, [], [], [Element| Acc]);
scan_tag([$&, $#, $1, $3, $; | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [], [$\r | Txt], Acc);
scan_tag([$&, $l, $t, $; | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [], [$< | Txt], Acc);
scan_tag([$&, $g, $t, $; | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [], [$> | Txt], Acc);
scan_tag([$&, $a, $m, $p, $; | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [], [$& | Txt], Acc);
scan_tag([C | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [], [C | Txt], Acc);
scan_tag([C | Rest], Stack, [], Acc) ->
    scan_tag(Rest, [C | Stack], [], Acc);
scan_tag([], _, _, Acc) -> strip_empty(Acc, []).

strip_empty([{txt, Txt} | Rest], Acc) ->
    case string:tokens(Txt, " \n\t") == "" of
        true ->
            strip_empty(Rest, Acc);
        false ->
            strip_empty(Rest, [{txt, to_txt(Txt)}|Acc])
    end;
strip_empty([E | Rest], Acc) ->
    strip_empty(Rest, [E | Acc]);
strip_empty([], Acc) ->
    Acc.

to_txt(TxtLst) ->
    list_to_binary(lists:reverse(TxtLst)).

parse_tag(Element) ->
    [Tag | Attrs] = split_on_space(Element),
    Fun = fun(A, Acc) ->
              case lists:member($=, A) of
                  true ->
                       [split_attribute(A)|Acc];
                  false ->
                       Acc
              end
          end,
    NewAttrs = lists:foldl(Fun, [], Attrs),
    {Tag, lists:reverse(NewAttrs), []}.

split_on_space(S) ->
    outside_quote(S, [], []).

outside_quote([$< | T], Acc, Attrs) ->
    outside_quote(T, Acc, Attrs);
outside_quote([$/,$> | T], Acc, Attrs) ->
    outside_quote(T, Acc, Attrs);
outside_quote([$> | T], Acc, Attrs) ->
    outside_quote(T, Acc, Attrs);
outside_quote([C | T], [_|_] = Acc, Attrs) when
      C == $ ; C == $\t; C == $\r; C == $\n ->
    outside_quote(T, [], [lists:reverse(Acc) | Attrs]);
outside_quote([C | T], [], Attrs) when
      C == $ ; C == $\t; C == $\r; C == $\n ->
    outside_quote(T, [], Attrs);
outside_quote([$" = C | T], Acc, Attrs) ->
    inside_quote(T, [C | Acc], Attrs);
outside_quote([C | T], Acc, Attrs) ->
    outside_quote(T, [C | Acc], Attrs);
outside_quote([], Acc, Attrs) ->
    lists:reverse([lists:reverse(Acc) | Attrs]).

inside_quote([$" = C | T], Acc, Attrs) ->
    outside_quote(T, [C | Acc], Attrs);
inside_quote([C | T], Acc, Attrs) ->
    inside_quote(T, [C | Acc], Attrs).

split_attribute(Attr) ->
    [Key | Vals] = string:tokens(Attr, "="),
    Val = string:join(Vals, "="),
    FinalVal =
        case string:tokens(Val, "\"") of
            [Val1|_] ->
                Val1;
            [] ->
                ""
        end,
    {split_qname(Key), clean_attr(FinalVal)}.

split_qname(QName) ->
    case string:tokens(QName, ":") of
        [Prefix, Name] ->
            {Prefix, clean_name(Name)};
        [Name] ->
            clean_name(Name)
    end.

clean_name(Name) ->
    N1 = string:strip(Name, right, $/),
    N2 = string:strip(N1, right, $\n),
    string:strip(N2).

clean_attr(Att) -> clean_attr(Att, []).
clean_attr([$\" | Rest], Acc) ->
    clean_attr(Rest, Acc);
clean_attr([C | Rest], Acc) ->
    clean_attr(Rest, [C | Acc]);
clean_attr([], Acc) ->
    lists:reverse(Acc).

%% parses the xml tree from tokens

parse_xml([{[$/ | Tag], _, _} | Rest], Stack, Nss) ->
    Key = case split_qname(Tag) of {_, N} -> N; N -> N end,
    {Element, NewStack} = find_start(Stack, Key),
    NewNss = lists:keydelete(Tag, 1, Nss),
    parse_xml(Rest, [Element | NewStack], NewNss);
parse_xml([{txt, _} = Txt | Rest], Stack, Nss) ->
    parse_xml(Rest, [Txt | Stack], Nss);
parse_xml([{Ignore, _, _} | Rest], Stack, Nss) when Ignore == "!--";
                                                    Ignore == "?xml";
                                                    Ignore == "?Xml";
                                                    Ignore == "?XML" ->
    parse_xml(Rest, Stack, Nss);
parse_xml([{Tag, Attrs ,_} | Rest], Stack, Nss) ->
    Key = case split_qname(Tag) of {_, N} -> N; N -> N end,
    {NewNss, NewAttrs} = push_xmlns(Attrs, Key, Nss),
    parse_xml(Rest, [{parse_qname(Tag, NewNss), NewAttrs, []} | Stack], NewNss);
parse_xml([], Stack, _) ->
    lists:reverse(Stack).

find_start(Stack, Tag) -> find_start(Stack, Tag, []).
find_start([{{_, Tag} = Qname, Attrs, []}|Rest], Tag, Acc) ->
    {{Qname, Attrs, Acc}, Rest};
find_start([{Tag, Attrs, []}|Rest], Tag, Acc) ->
    {{Tag, Attrs, Acc}, Rest};
find_start([Elem | Rest], Tag, Acc) ->
    find_start(Rest, Tag, [Elem | Acc]);
find_start([], _, Acc) ->
    {Acc, []}.

push_xmlns(Attrs, Key, Nss) ->
    {NewNss, NewAttrs} = push_xmlns(Attrs, Key, [], []),
    FinalNss = NewNss ++ Nss,
    {FinalNss, find_ns(NewAttrs, [ N || {_, N} <- FinalNss ])}.

push_xmlns([{"xmlns", Ns}|Rest], Key, Nss, Attrs) ->
    push_xmlns(Rest, Key, [{Key, {"", Ns}}|Nss], Attrs);
push_xmlns([{{"xmlns", Prefix}, Ns}|Rest], Key, Nss, Attrs) ->
    push_xmlns(Rest, Key, [{Key, {Prefix, Ns}}|Nss], Attrs);
push_xmlns([A|Rest], Key, Nss, Attrs) ->
    push_xmlns(Rest, Key, Nss, [A|Attrs]);
push_xmlns([], _, Nss, Attrs) ->
    {lists:usort(lists:reverse(Nss)), lists:reverse(Attrs)}.

find_ns([{{P, N}, V} = A | Rest], Nss) ->
    case lists:keyfind(P, 1, Nss) of
        false ->
            [A | find_ns(Rest, Nss)];
        {_, Ns} ->
            [{{Ns, N}, V} | find_ns(Rest,Nss)]
    end;
find_ns([A | Rest], Nss) ->
    [A|find_ns(Rest, Nss)];
find_ns([], _) -> [].

%% ----------------------------------------------------------------------------
%% Get All Nss
ns_tag({txt, _Content}, NssMap) ->
    NssMap;
ns_tag({{Ns, txt}, _Content}, NssMap) ->
    %% Ugly hack for namespaced txts
    ns_add_prefix(Ns, NssMap);
ns_tag({{Ns, _Name}, Attributes, Children}, NssMap0) ->
    NssMap1 = ns_add_prefix(Ns, NssMap0),
    NssMap2 = ns_attributes(Attributes, NssMap1),
    ns_children(Children, NssMap2);
ns_tag({_Name, Attributes, Children}, NssMap0) ->
    NssMap1 = ns_attributes(Attributes, NssMap0),
    ns_children(Children, NssMap1).

ns_attributes([{{NsN, _N}, {NsV, _V}} | Attrs], NssMap0) ->
    NssMap1 = ns_add_prefix(NsN, NssMap0),
    NssMap2 = ns_add_prefix(NsV, NssMap1),
    ns_attributes(Attrs, NssMap2);
ns_attributes([{{Ns, _N}, _Value} | Attrs], NssMap0) ->
    NssMap1 = ns_add_prefix(Ns, NssMap0),
    ns_attributes(Attrs, NssMap1);
ns_attributes([{_Key, _Value} | Attrs], NssMap) ->
    ns_attributes(Attrs, NssMap);
ns_attributes([], NssMap) ->
    NssMap.

ns_children([C | Children], NssMap0) ->
    NssMap1 = ns_tag(C, NssMap0),
    ns_children(Children, NssMap1);
ns_children([], NssMap) ->
    NssMap.

ns_add_prefix(Ns, NssMap) ->
    case maps:is_key(Ns, NssMap) of
        true -> NssMap;
        false -> NssMap#{Ns => true}
    end.

%% ----------------------------------------------------------------------------
%% Xml utils

%% @doc Used to debug differences between large strings
compare(S1, S2) -> compare(S1, S2, 0).
compare([C1 | Rest1], [C1 | Rest2], Acc) ->
    compare(Rest1, Rest2, Acc+1);
compare([_ | _] = S1, [_ | _] = S2, Acc) ->
    {Acc, {lists:sublist(S1, 100), lists:sublist(S2, 100)}};
compare([], [], _) ->
    equal;
compare([], S1, Acc) ->
    {Acc, {rh, lists:sublist(S1, 100)}};
compare(S1, [], Acc) ->
    {Acc, {lh, lists:sublist(S1, 100)}}.

%% ----------------------------------------------------------------------------
%% Namespace utils

get_ns_prefix(?XML_NS, Store) ->
    {"xml", ?XML_NS, Store};
get_ns_prefix(Ns, Store) ->
    case lists:keyfind(Ns, 2, Store) of
        {Prefix, Ns, _} ->
            {Prefix, [], Store};
        false ->
            add_prefix(Ns, Store)
    end.

add_prefix(Ns, []) ->
    add_prefix(Ns, 0, []);
add_prefix(Ns, Store) ->
    Max = lists:max([ N || {_, _, N} <- Store ]),
    add_prefix(Ns, Max, Store).

add_prefix(Ns, Max, Store) ->
    NewPrefix = ["p", integer_to_list(Max+1)],
    XmlNsDecl = [{["xmlns:", NewPrefix], Ns}],
    {NewPrefix,
     XmlNsDecl,
     lists:keystore(NewPrefix, 1, Store, {NewPrefix, Ns, Max+1})}.

parse_qname(Qname, RawNss) ->
    Nss = [ N || {_, N} <- RawNss ],
    case split_qname(Qname) of
        {Prefix, Name} ->
            case lists:keyfind(Prefix, 1, Nss) of
                {Prefix, Ns} ->
                    {Ns, Name};
                false ->
                    {Prefix, Name}
            end;
        Name ->
            case lists:keyfind("", 1, Nss) of
                {"", Ns} ->
                    {Ns, Name};
                false ->
                    Name
            end
    end.

%% ----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

parse_attribute_with_space_test() ->
    TestTag =
        binary_to_list(
          <<"<res text=\"Tillåtet värde för a.\"\n co=\"0\"\tec=\"54\" />"/utf8>>),
    ?assertMatch({"res", [ {"text",
                            "TillÃ¥tet vÃ¤rde fÃ¶r a."}
                         , {"co", "0"}
                         , {"ec", "54"}
                         ], []}, parse_tag(TestTag)).

-endif.
