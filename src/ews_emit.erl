-module(ews_emit).

-export([model_to_file/2, output_type/2]).

-include("ews.hrl").

model_to_file(#model{type_map=Tbl}, Filename) ->
    AllTypes = [ output_type(T, Tbl) || T <- sort_types(Tbl) ],
    io:format("emitting ~p records~n", [length(AllTypes)]),
    {ok, Fd} = file:open(Filename, [write]),
    [ ok = file:write(Fd, [T, $\n]) || T <- AllTypes ],
    file:close(Fd).

output_type(#type{qname=Qname, alias=Alias}, Tbl) ->
    Line1 = ["-record(", tick_word(Alias), ", {"],
    Indent = iolist_size(Line1),
    PartRows = [ output_part(P, Indent) ||
                 P <- ews_model:get_parts(Qname, Tbl) ],
    JoinStr = ",\n"++lists:duplicate(Indent, $ ),
    [Line1, string:join(PartRows, JoinStr), "}).\n"].

output_part(#elem{qname=Qname, type=#base{erl_type=Et}, meta=M}, _) ->
    A = ews_alias:create(Qname),
    #meta{max=Max} = M,
    case {Et, Max} of
        {string, 1} ->
            [tick_word(A), " :: string()"];
        {string, Max} when Max > 1 ->
            [tick_word(A), " :: [string()]"];
        {integer, 1} ->
            [tick_word(A), " :: integer()"];
        {integer, Max} when Max > 1 ->
            [tick_word(A), " :: [integer()]"];
        {float, 1} ->
            [tick_word(A), " :: float()"];
        {float, Max} when Max > 1 ->
            [tick_word(A), " :: [float()]"];
        {boolean, 1} ->
            [tick_word(A), " :: boolean()"];
        {boolean, Max} when Max > 1 ->
            [tick_word(A), " :: [boolean()]"]
    end;
output_part(#elem{qname=Qname, type=#enum{values=Values}=E, meta=M}, Indent) ->
    %% TODO: Save an enum type for -type() emit:ing
    #meta{max=Max} = M,
    #enum{list=IsList} = E,
    A = ews_alias:create(Qname),
    PartLine = [tick_word(A), " :: "],
    SpecIndent = Indent + iolist_size(PartLine),
    EnumSpec = emit_enum([ V || {V, _} <- Values ], SpecIndent),
    case Max of
        1 when not IsList ->
            [PartLine, EnumSpec];
        Max when Max > 1; IsList ->
            [PartLine, ["[", EnumSpec, "]"]]
    end;
output_part(#elem{qname=Qname, type={_,_}=Tn, meta=#meta{max=Max}}, _) ->
    A = ews_alias:create(Qname),
    Atn = ews_alias:get_alias(Tn),
    case Max of
        1 ->
            [tick_word(A), " :: #", tick_word(Atn), "{}"];
        Max when Max > 1 ->
            [tick_word(A), " :: [#", tick_word(Atn), "{}]"]
    end.

tick_word(Word) when is_atom(Word) ->
    tick_word(atom_to_list(Word));
tick_word([C|_] = Word) when C >= $A andalso C =< $Z ->
    [$', Word, $'];
tick_word(Word) ->
    Word.

%% ---------------------------------------------------------------------------

sort_types(Tbl) ->
    Graph = create_graph(Tbl),
    OrderedQns = sort_types(Graph, [], []),
    [ ews_model:get(Qn, Tbl) || Qn <- OrderedQns ].

sort_types([{Qn, []}|Types], Overflow, Res) ->
    sort_types(Types, Overflow, [Qn|Res]);
sort_types([{Qn, Deps}|Types], Overflow, Res) ->
    case lists:all(fun(D) -> lists:member(D, Res) end, Deps) of
        true ->
            sort_types(Types, Overflow, [Qn|Res]);
        false ->
            sort_types(Types, [{Qn, Deps}|Overflow], Res)
    end;
sort_types([], [], Res) ->
    lists:reverse(Res);
sort_types([], Overflow, Res) ->
    sort_types(Overflow, [], Res).

create_graph(Tbl) ->
    Types = ews_model:values(Tbl),
    F = fun(#type{qname=Qn}, D) ->
            Es = ews_model:get_parts(Qn, Tbl),
            ElemFun = fun(#elem{type={_,_}=K}, A) -> [K|A]; (_, A) -> A end,
            Deps = lists:foldl(ElemFun, [], Es),
            dict:store(Qn, Deps, D)
        end,
    dict:to_list(lists:foldl(F, dict:new(), Types)).

emit_enum(Values, Indent) ->
    TickedValues = [ tick_word(V) || V <- Values ],
    JoinStr = [$\n,  lists:duplicate(Indent-2, $ ), $|, $ ],
    string:join(TickedValues, lists:flatten(JoinStr)).
