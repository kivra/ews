-module(ews_xml).

-export([encode/1, decode/1, compare/2]).

-define(XML_NS, "http://www.w3.org/XML/1998/namespace").

-type name() :: atom() | string().
-type qname() :: name() | {name(), name()}.
-type xml_data() :: {qname(), [{name(), string()}], list()}.

%% ----------------------------------------------------------------------------
%% Api

-spec encode(xml_data()) -> iolist().
encode(Data) when is_tuple(Data) andalso size(Data) == 3 ->
    emit_tag(Data, []);
encode(Data) when is_list(Data) ->
    [ emit_tag(D, []) || D <- Data ].

-spec decode(string()|binary()) -> xml_data() | [xml_data()].
decode(XmlString) when is_binary(XmlString) ->
    decode(binary_to_list(XmlString));
decode(XmlString) when is_list(XmlString) ->
    Tokens = scan_tag(XmlString, [], [], []),
    parse_xml(Tokens, [], []).

%% ----------------------------------------------------------------------------
%% encode

emit_tag({txt, Content}, _) ->
    Content;
emit_tag({{Ns, Name}, Attributes, Children}, Nss) ->
    {Prefix, XmlNsDecl, NewNss} = get_ns_prefix(Ns, Nss),
    QName = {Prefix, Name},
    [emit_start_tag(QName),
     emit_attributes(lists:ukeysort(2, XmlNsDecl++Attributes), NewNss),
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

%% ----------------------------------------------------------------------------
%% decode

scan_tag([$<, $/ | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [$/ ,$<], [], [{txt, Txt} | Acc]);
scan_tag([$< | Rest], [], Txt, Acc) ->
    scan_tag(Rest, [$<], [], [{txt, Txt} | Acc]);
scan_tag([$> | Rest], Tag, [], Acc) ->
    Element = parse_tag(lists:reverse([$> | Tag])),
    scan_tag(Rest, [], [], [Element| Acc]);
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
    [Tag | Attrs] = string:tokens(Element, "<> "),
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

split_attribute(Attr) ->
    [Key | Vals] = string:tokens(Attr, "="),
    Val = string:join(Vals, "="),
    [FinalVal|_] = string:tokens(Val, "\""),
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
parse_xml([{"!--", _, _} | Rest], Stack, Nss) ->
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
