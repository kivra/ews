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
    PartRows = [ output_part(P, Indent, Tbl) ||
                 P <- ews_model:get_parts(Qname, Tbl) ],
    JoinStr = ",\n"++lists:duplicate(Indent, $ ),
    [Line1, string:join(PartRows, JoinStr), "}).\n"].

output_part(#elem{qname=Qname, type=T, meta=M}, Indent, Tbl) ->
    A = ews_alias:create(Qname),
    #meta{min=Min} = M,
    Base = [tick_word(A), " :: "],
    SpecIndent = Indent + iolist_size(Base),
    Ts = output_types(T, M, SpecIndent, Tbl),
    check_min(check_nillable([Base, Ts], M), Min).

output_types(T, M, SpecIndent, Tbl) when not is_list(T) ->
    output_single_type(T, M, SpecIndent, Tbl);
output_types(Types, M, SpecIndent, Tbl) ->
    lists:join(" | ",
               [output_single_type(T, M, SpecIndent, Tbl) || T <- Types]).

output_single_type(#base{erl_type=Et}, #meta{max = Max}, _SpecIndent, _Tbl) ->
    add_list(output_erl_type(Et), Max > 1);
output_single_type(E = #enum{values=Values}, #meta{max = Max},
                   SpecIndent, _Tbl) ->
    #enum{list=IsList} = E,
    EnumSpec = emit_enum([ V || {V, _} <- Values ], SpecIndent),
    add_list(EnumSpec, IsList orelse Max > 1);
output_single_type(Tn = {_,_}, #meta{max = Max}, _SpecIndent, Tbl) ->
    Atn = ews_alias:get_alias(Tn),
    SubTypes = ews_model:get_subs(Tn, Tbl),
    Atns = [Atn | [ews_alias:get_alias(T) || {T, _} <- SubTypes]],
    string:join([add_list(record_spec(T), Max > 1) || T <- Atns], " | ").

output_erl_type(string) ->
    "string() | binary()";
output_erl_type(integer) ->
    "integer()";
output_erl_type(float) ->
    "float()";
output_erl_type(boolean) ->
    "boolean()".

add_list(Str, true) ->
    ["[", Str, "]"];
add_list(Str, false) ->
    Str.

record_spec(T) ->
    ["#", tick_word(T), "{}"].

tick_word(Word) when is_atom(Word) ->
    tick_word(atom_to_list(Word));
tick_word(Word = [C | _]) ->
    case C >= $a andalso C =< $z andalso lists:all(fun atom_char/1, Word) of
        true ->
            Word;
        false ->
            [$', Word, $']
    end.

atom_char(C) when C >= $a, C =< $z ->
    true;
atom_char(C) when C >= $A, C =< $Z ->
    true;
atom_char(C) when C >= $0, C =< $9 ->
    true;
atom_char(C) when C == $_; C == $@ ->
    true;
atom_char(_C) ->
    false.

check_nillable(Base, #meta{nillable = "true"}) ->
    [Base, " | nil"];
check_nillable(Base, _) ->
    Base.

check_min(Base, 0) ->
    [Base, " | undefined"];
check_min(Base, _) ->
    Base.

%% ---------------------------------------------------------------------------

sort_types(Tbl) ->
    Graph = create_graph(Tbl),
    OrderedQns = sort_types(Graph, [], []),
    [ ews_model:get(Qn, Tbl) || Qn <- OrderedQns ].

sort_types([{Qn, []}|Types], Overflow, Res) ->
    sort_types(Types, Overflow, [Qn|Res]);
sort_types([{Qn, Deps}|Types], Overflow, Res) ->
    case lists:all(fun(D) -> lists:member(D, Res) orelse D == Qn end, Deps) of
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
                ElemFun = fun (#elem{type={_,_}=K}, A) ->
                                  [K|A];
                              (#elem{type=[_|_]=Ts}, A) ->
                                  Keys = [K || {_,_}=K <- Ts],
                                  Keys ++ A;
                              (_, A) ->
                                  A
                          end,
                Deps = lists:foldl(ElemFun, [], Es),
                DepsSubs = lists:append(
                             [ews_model:get_subs(K, Tbl) || K <- Deps]),
                dict:store(Qn, Deps ++ [K || {K, _} <- DepsSubs], D)
        end,
    dict:to_list(lists:foldl(F, dict:new(), Types)).

emit_enum(Values, Indent) ->
    TickedValues = [ tick_word(V) || V <- Values ],
    JoinStr = [$\n,  lists:duplicate(Indent-2, $ ), $|, $ ],
    string:join(TickedValues, lists:flatten(JoinStr)).
