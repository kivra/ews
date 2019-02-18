-module(ews_emit).

-export([model_to_file/4]).

-include("ews.hrl").

model_to_file(#model{type_map=Tbl}, Filename, ModelRef, Opts) ->
    NewOpts = maps:merge(#{enum_value_string => false}, Opts),
    {Unresolved, Resolved} = sort_types(Tbl),
    io:format("emitting ~p records~n", [length(Unresolved) + length(Resolved)]),
    UnresolvedTypeDefs = [output_typedef(T) || T <- Unresolved],
    ResolvedTypes = [output_type(T, Tbl, ModelRef, Unresolved, NewOpts) ||
                        T <- Resolved ++ Unresolved],
    {ok, Fd} = file:open(Filename, [write]),
    [ok = file:write(Fd, [T, $\n]) || T <- UnresolvedTypeDefs],
    [ok = file:write(Fd, [T, $\n]) || T <- ResolvedTypes],
    file:close(Fd).

output_typedef(#type{alias=Alias}) ->
    ["-type '#", atom_to_list(Alias), "'() :: tuple().  "
     "%% Needed due to circular type definition\n"].

output_type(#type{qname=Qname, alias=Alias}, Tbl, ModelRef, Unresolved, Opts) ->
    Line1 = ["-record(", tick_word(Alias), ", {"],
    Indent = iolist_size(Line1),
    PartRows = [output_part(P, Indent, Tbl, ModelRef, Unresolved, Opts) ||
                   P <- ews_model:get_parts(Qname, Tbl)],
    JoinStr = ",\n"++lists:duplicate(Indent, $ ),
    [Line1, string:join(PartRows, JoinStr), "}).\n"].

output_part(#elem{qname=Qname, type=T, meta=M}, Indent, Tbl,
            ModelRef, Unresolved, Opts) ->
    A = ews_alias:create(Qname),
    #meta{min=Min} = M,
    Base = [tick_word(A), " :: "],
    SpecIndent = Indent + iolist_size(Base),
    Ts = output_types(T, M, SpecIndent, Tbl, ModelRef, Unresolved, Opts),
    check_min(check_nillable([Base, Ts], M), Min).

output_types(T, M, SpecIndent, Tbl, ModelRef, Unresolved, Opts)
  when not is_list(T) ->
    output_single_type(T, M, SpecIndent, Tbl, ModelRef, Unresolved, Opts);
output_types(Types, M, SpecIndent, Tbl, MR, Unresolved, Opts) ->
    lists:join(" | ",
               [output_single_type(T, M, SpecIndent, Tbl, MR, Unresolved, Opts)
                || T <- Types]).

output_single_type(#base{erl_type=Et}, #meta{max = Max}, _SpecIndent, _Tbl,
                   _ModelRef, _Unresolved, Opts) ->
    add_list(output_erl_type(Et, Opts), Max > 1);
output_single_type(E = #enum{values=Values}, #meta{max = Max},
                   SpecIndent, _Tbl, _ModelRef, _Unresolved, Opts) ->
    #enum{list=IsList} = E,
    EnumSpec = emit_enum([ V || {V, _} <- Values ], SpecIndent, Opts),
    add_list(EnumSpec, IsList orelse Max > 1);
output_single_type(Tn = {_,_}, #meta{max = Max}, _SpecIndent, Tbl,
                   ModelRef, Unresolved, Opts) ->
    Atn = ews_alias:get_alias(Tn, ModelRef),
    SubTypes = ews_model:get_subs(Tn, Tbl),
    Atns = [Atn | [ews_alias:get_alias(T, ModelRef) || {T, _} <- SubTypes]],
    string:join(
      [add_list(record_spec(T, Unresolved, Opts), Max > 1) || T <- Atns],
      " | ").

output_erl_type(string, _) ->
    "string() | binary()";
output_erl_type(integer, _) ->
    "integer()";
output_erl_type(float, _) ->
    "float()";
output_erl_type(boolean, _) ->
    "boolean()".

add_list(Str, true) ->
    ["[", Str, "]"];
add_list(Str, false) ->
    Str.

record_spec(T, Unresolved, _Opts) ->
    case lists:keymember(T, #type.alias, Unresolved) of
        false ->
            ["#", tick_word(T), "{}"];
        true ->
            ["'#", atom_to_list(T), "'()"]
    end.

tick_word(Word) when is_atom(Word) ->
    tick_word(atom_to_list(Word));
tick_word(Word) ->
    case erl_scan:string(Word) of
        {ok, [{atom, _, _}], _} ->
            Word;
        _ ->
            [$', Word, $']
    end.

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
    {Unresolved, Resolved} = sort_types(Graph, [], [], [], false),
    {[ews_model:get(Qn, Tbl) || Qn <- Unresolved],
     [ews_model:get(Qn, Tbl) || Qn <- Resolved]}.

sort_types([{Qn, []}|Types], Overflow, Res, Unresolvable,_Progressed) ->
    sort_types(Types, Overflow, [Qn|Res], Unresolvable, true);
sort_types([{Qn, Deps}|Types], Overflow, Res, Unresolvable, Progressed) ->
    case [D || D <- Deps,
               D /= Qn,
               not lists:member(D, Res),
               not lists:member(D, Unresolvable)] of
        [] ->
            sort_types(Types, Overflow, [Qn|Res], Unresolvable, true);
        UnresolvedDeps ->
            sort_types(Types, [{Qn, UnresolvedDeps}|Overflow], Res,
                       Unresolvable, Progressed)
    end;
sort_types([], [], Res, Unresolvable, _Progressed) ->
    {Unresolvable, lists:reverse(Res)};
sort_types([], Overflow, Res, Unresolvable, false) ->
    NewUnresolvable = choose_unresolvable(Overflow),
    NewOverflow = lists:keydelete(NewUnresolvable, 1, Overflow),
    sort_types(NewOverflow, [], Res, [NewUnresolvable | Unresolvable], false);
sort_types([], Overflow, Res, Unresolvable, true) ->
    sort_types(Overflow, [], Res, Unresolvable, false).

choose_unresolvable([{Qn, [D | _]} | _] = Graph) ->
    Chosen = find_loop(D, [Qn], Graph),
    Chosen.

find_loop(Qn, Passed = [Previous | _], Graph) ->
    case lists:member(Qn, Passed) of
        true ->
            Previous;
        false ->
            {_, [D | _]} = lists:keyfind(Qn, 1, Graph),
            find_loop(D, [Qn | Passed], Graph)
    end.

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

emit_enum(Values, Indent, #{enum_value_string := AllowString}) ->
    TickedValues = [tick_word(V) || V <- Values],
    JoinStr = [$\n,  lists:duplicate(Indent-2, $ ), $|, $ ],
    string:join(add_string(TickedValues, AllowString), lists:flatten(JoinStr)).

add_string(Values, true) ->
    ["string()" | Values];
add_string(Values, false) ->
    Values.
