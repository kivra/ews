-module(ews_xml).

-export([from_term/1, to_term/1, compare/2]).

-type name() :: atom() | string().
-type qname() :: name() | {name(), name()}.
-type xml_data() :: {qname(), [{name(), string()}], list()}.

%% TODO: Handle naked xmlns-declarations
%% ----------------------------------------------------------------------------
%% Api

-spec from_term(xml_data()) -> iolist().
from_term(Data) when is_tuple(Data) andalso size(Data) == 3 ->
    emit_tag(Data, []);
from_term(Data) when is_list(Data) ->
    [ emit_tag(D, []) || D <- Data ].

-spec to_term(string()|binary()) -> xml_data() | [xml_data()].
to_term(XmlString) when is_binary(XmlString) ->
    to_term(binary_to_list(XmlString));
to_term(XmlString) when is_list(XmlString) ->
    Tokens = scan_tag(XmlString, [], [], [], []),
    parse_xml(Tokens, []).

%% ----------------------------------------------------------------------------
%% term to xml

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
%% xml to term

to_txt(TxtLst) ->
    list_to_binary(lists:reverse(TxtLst)).

scan_tag([$<, $/ | Rest], [], Txt, Nss, Acc) ->
    scan_tag(Rest, [$/ ,$<], [], Nss, [{txt, Txt} | Acc]);
scan_tag([$< | Rest], [], Txt, Nss, Acc) ->
    scan_tag(Rest, [$<], [], Nss, [{txt, Txt} | Acc]);
scan_tag([$> | Rest], Tag, [], Nss, Acc) ->
    {Element, NewNss} = parse_tag(lists:reverse([$> | Tag]), Nss),
    scan_tag(Rest, [], [], NewNss, [Element| Acc]);
scan_tag([C | Rest], [], Txt, Nss, Acc) ->
    scan_tag(Rest, [], [C | Txt], Nss, Acc);
scan_tag([C | Rest], Stack, [], Nss, Acc) ->
    scan_tag(Rest, [C | Stack], [], Nss, Acc);
scan_tag([], _, _, _, Acc) -> strip_empty(Acc, []).

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

parse_tag(Element, EntryNss) ->
    [Tag | Attrs] = string:tokens(Element, "<> "),
    Fun = fun(A, {Acc, Nss}) ->
              case lists:member($=, A) of
                  true ->
                      case split_attribute(A) of
                          {"xmlns", Tns} ->
                              NewNss = lists:keystore("", 1,
                                                      Nss, {"", Tns}),
                              {[{{"xmlns", ""}, Tns} | Acc], NewNss};
                          {{"xmlns", Prefix} = Key, Ns} ->
                              NewNss = lists:keystore(Prefix, 1,
                                                      Nss, {Prefix, Ns}),
                              {[{Key, Ns} | Acc], NewNss};
                          {{Prefix, Key}, Val}  ->
                              {Prefix, Ns} = lists:keyfind(Prefix, 1, Nss),
                              {[{{Ns, Key}, Val} | Acc], Nss}
                      end;
                  false ->
                      {Acc, Nss}
              end
          end,
    {NewAttrs, NewNss} = lists:foldl(Fun, {[], EntryNss}, Attrs),
    Qname = parse_qname(Tag, NewNss),
    {{Qname, lists:reverse(NewAttrs), []}, NewNss}.

parse_qname(Qname, Nss) ->
    case split_qname(Qname) of
        {"/"++Prefix, Name} ->
            {Prefix, Ns} = lists:keyfind(Prefix, 1, Nss),
            {"/"++Ns, Name};
        {Prefix, Name} ->
            case lists:keyfind(Prefix, 1, Nss) of
                {Prefix, Ns} ->
                    {Ns, Name};
                false ->
                    io:format("didnt find ~p in ~p~n", [Prefix, Nss]),
                    {Prefix, Name}
            end;
        "/"++Name ->
            case lists:keyfind("", 1, Nss) of
                {"", Ns} ->
                    {"/"++Ns, Name};
                false ->
                    "/"++Name
            end;
        Name ->
            case lists:keyfind("", 1, Nss) of
                {"", Ns} ->
                    {Ns, Name};
                false ->
                    Name
            end
    end.

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

parse_xml([{{[$/ | Ns], Name}, _, _} | Rest], Stack) ->
    {Element, NewStack} = find_start(Stack, {Ns, Name}),
    parse_xml(Rest, [Element | NewStack]);
parse_xml([{[$/ | Tag], _, _} | Rest], Stack) ->
    {Element, NewStack} = find_start(Stack, Tag),
    parse_xml(Rest, [Element | NewStack]);
parse_xml([{txt, _} = Txt | Rest], Stack) ->
    parse_xml(Rest, [Txt | Stack]);
parse_xml([{"!--", _, _} | Rest], Stack) ->
    parse_xml(Rest, Stack);
parse_xml([Tag | Rest], Stack) ->
    parse_xml(Rest, [Tag | Stack]);
parse_xml([], Stack) ->
    lists:reverse(Stack).

find_start(Stack, Tag) -> find_start(Stack, Tag, []).
find_start([{Tag, Attrs, []}|Rest], Tag, Acc) ->
    {{Tag, Attrs, Acc}, Rest};
find_start([Elem | Rest], Tag, Acc) ->
    find_start(Rest, Tag, [Elem | Acc]);
find_start([], _, Acc) ->
    {Acc, []}.

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
