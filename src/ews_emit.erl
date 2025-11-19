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
-module(ews_emit).

-export([model_to_file/3]).
-export([sort_types/2]).

-include("ews.hrl").
-include_lib("ews/include/ews.hrl").

model_to_file(#model{type_map=Tbl, simple_types=Ts}, Filename, ModelRef) ->
    {Unresolved, Resolved} = sort_types(Tbl, Ts),
    logger:notice("emitting ~p records unresolved: ~p resolved: ~p~n",
                  [ length(Unresolved) + length(Resolved)
                  , length(Unresolved)
                  , length(Resolved)
                  ]),
    UnresolvedTypeDefs = [output_typedef(T) || T <- Unresolved],
    ResolvedTypes = [output_type(T, Tbl, ModelRef, Unresolved) ||
                        T <- Resolved ++ Unresolved],
    {ok, Fd} = file:open(Filename, [write]),
    [ok = file:write(Fd, [T, $\n]) || T <- UnresolvedTypeDefs],
    [ok = file:write(Fd, [T, $\n]) || T <- ResolvedTypes],
    file:close(Fd).

output_typedef(#type{alias=Alias}) ->
    ["-type '#", utf8_atom_to_list(Alias), "'() :: tuple().  "
     "%% Needed due to circular type definition\n"].

output_type(#type{qname=Qname, alias=Alias, attrs=[]}, Tbl, ModelRef, Unresolved) ->
    Line1 = ["-record(", tick_word(Alias), ", {"],
    Indent = iolist_size(Line1),
    PartRows = [output_part(P, Indent, Tbl, ModelRef, Unresolved) ||
                   P <- ews_model:get_parts(Qname, Tbl)],
    JoinStr = ",\n"++lists:duplicate(Indent, $ ),
    [Line1, string:join(PartRows, JoinStr), "}).\n"];
output_type(#type{qname=Qname, alias=Alias, attrs=Attrs}, Tbl, ModelRef,
            Unresolved) ->
    Line0 = "%% @doc Possible keys for '__attrs'\n",
    AttrDocs = [ ["%% ", tick_word(A), " :: ", no_ns(T), "\n"] ||
                   #attribute{name={_,A},type=T} <- Attrs ],
    Line1 = ["-record(", tick_word(Alias), ", {"],
    Indent = iolist_size(Line1),
    Attr = ["'__attrs' :: #{"],
    AttrIndent = Indent + iolist_size(Attr),
    AttrEnd = output_attr_undefined(Attrs),
    AttrRows = [lists:flatten([tick_word(A), output_map_default(U),
                               erl_type(T)]) ||
                   #attribute{name={_,A},use=U,type=T} <- Attrs],
    JoinAttrs = ",\n"++lists:duplicate(AttrIndent, $ ),
    AttrStr = lists:flatten([Attr, string:join(AttrRows, JoinAttrs), AttrEnd]),
    PartRows = [output_part(P, Indent, Tbl, ModelRef, Unresolved) ||
                   P <- ews_model:get_parts(Qname, Tbl)],
    JoinStr = ",\n"++lists:duplicate(Indent, $ ),
    [Line0, AttrDocs, Line1, string:join([AttrStr | PartRows],
                                         JoinStr), "}).\n"].

output_part(#elem{qname=Qname, type=T, meta=M}, Indent, Tbl,
            ModelRef, Unresolved) ->
    A = ews_alias:create(Qname),
    #meta{min=Min} = M,
    Base = [tick_word(A), " :: "],
    SpecIndent = Indent + iolist_size(Base),
    Ts = output_types(T, M, SpecIndent, Tbl, ModelRef, Unresolved),
    check_min(check_nillable([Base, Ts], M), Min).

output_types(T, M, SpecIndent, Tbl, ModelRef, Unresolved)
  when not is_list(T) ->
    output_single_type(T, M, SpecIndent, Tbl, ModelRef, Unresolved);
output_types(Types, M, SpecIndent, Tbl, MR, Unresolved) ->
    lists:join(" | ",
               [output_single_type(T, M, SpecIndent, Tbl, MR, Unresolved)
                || T <- Types]).

output_single_type(#base{erl_type=Et}, #meta{max = Max}, _SpecIndent, _Tbl,
                   _ModelRef, _Unresolved) ->
    add_list(output_erl_type(Et), Max > 1);
output_single_type(E = #enum{values=Values}, #meta{max = Max},
                   SpecIndent, _Tbl, _ModelRef, _Unresolved) ->
    #enum{list=IsList} = E,
    EnumSpec = emit_enum([ V || {V, _} <- Values ], SpecIndent),
    add_list(EnumSpec, IsList orelse Max > 1);
output_single_type(Tn = {_,_}, #meta{max = Max}, _SpecIndent, Tbl,
                   ModelRef, Unresolved) ->
    Atn = ews_alias:get_alias(Tn, ModelRef),
    SubTypes = ews_model:get_subs(Tn, Tbl),
    Atns = [Atn | [ews_alias:get_alias(T, ModelRef) || {T, _} <- SubTypes]],
    string:join(
      [add_list(record_spec(T, Unresolved), Max > 1) || T <- Atns],
      " | ").

output_erl_type(string) ->
    "string() | binary()";
output_erl_type(integer) ->
    "integer()";
output_erl_type(float) ->
    "float()";
output_erl_type(boolean) ->
    "boolean()".

output_map_default(undefined) ->
    " => ";
output_map_default("optional") ->
    " => ";
output_map_default("required") ->
    " := ".

output_attr_undefined(Attrs) ->
    %% if any attribute is required, the map needs to exist
    case lists:any(fun any_required/1, Attrs) of
        true ->
            ["}"];
        false ->
            ["} | undefined"]
    end.

any_required(#attribute{use="required"}) -> true;
any_required(#attribute{}) -> false.

add_list(Str, true) ->
    ["[", Str, "]"];
add_list(Str, false) ->
    Str.

record_spec(T, Unresolved) ->
    case lists:keymember(T, #type.alias, Unresolved) of
        false ->
            ["#", tick_word(T), "{}"];
        true ->
            ["'#", utf8_atom_to_list(T), "'()"]
    end.

tick_word(Word) when is_atom(Word) ->
    tick_word(utf8_atom_to_list(Word));
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

sort_types(Tbl, Ts) ->
    Graph = create_graph(Tbl),
    {Unresolved, Resolved} = sort_types(Graph, [], [], [], false, Ts),
    {[ews_model:get(Qn, Tbl) || Qn <- Unresolved],
     [ews_model:get(Qn, Tbl) || Qn <- Resolved]}.

sort_types([{Qn, []}|Types], Overflow, Res, Unresolvable,_Progressed, Ts) ->
    sort_types(Types, Overflow, [Qn|Res], Unresolvable, true, Ts);
sort_types([{Qn, Deps}|Types], Overflow, Res, Unresolvable, Progressed, Ts) ->
    case [D || D <- Deps,
               D /= Qn,
               not lists:member(D, Res),
               not lists:member(D, Unresolvable)] of
        [] ->
            sort_types(Types, Overflow, [Qn|Res], Unresolvable, true, Ts);
        UnresolvedDeps ->
            sort_types(Types, [{Qn, UnresolvedDeps}|Overflow], Res,
                       Unresolvable, Progressed, Ts)
    end;
sort_types([], [], Res, Unresolvable, _Progressed, _Ts) ->
    {Unresolvable, lists:reverse(Res)};
sort_types([], Overflow, Res, Unresolvable, false, Ts) ->
    NewUnresolvable = choose_unresolvable(Overflow, Ts),
    NewOverflow = lists:keydelete(NewUnresolvable, 1, Overflow),
    sort_types(NewOverflow, [], Res, [NewUnresolvable | Unresolvable], false, Ts);
sort_types([], Overflow, Res, Unresolvable, true, Ts) ->
    sort_types(Overflow, [], Res, Unresolvable, false, Ts).

choose_unresolvable([{Qn, [D | _]} | _] = Graph, Ts) ->
    Chosen = find_loop(D, [Qn], Graph, Ts),
    Chosen.

find_loop(Qn, Passed = [Previous | _], Graph, Ts) ->
    case lists:member(Qn, Passed) of
        true ->
            Previous;
        false ->
            try
                {_, [D | _]} = lists:keyfind(Qn, 1, Graph),
                find_loop(D, [Qn | Passed], Graph, Ts)
            catch
                _:_ ->
                    case lists:keyfind(Qn, 1, Ts) of
                        {_, Foo} ->
                            error({found, Qn, Foo}, [Qn, Passed, Ts]);
                        false ->
                            error({cant_find, Qn, Ts}, [Qn, Passed, Ts])
                    end
            end
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
    lists:sort(dict:to_list(lists:foldl(F, dict:new(), Types))).

emit_enum(Values, Indent) ->
    TickedValues = [tick_word(V) || V <- Values],
    JoinStr = [$\n,  lists:duplicate(Indent-2, $ ), $|, $ ],
    string:join(TickedValues, lists:flatten(JoinStr)).

no_ns({_NS, N}) -> N;
no_ns(N) -> N.

erl_type({_,_} = T) ->
    #base{erl_type = ET} = ews_xsd:to_base(T),
    output_erl_type(ET);
erl_type(T) ->
    #base{erl_type = ET} = ews_xsd:to_base({"no_ns", T}),
    output_erl_type(ET).

%% This outputs an atom as an utf8 string.
utf8_atom_to_list(Atom) ->
    binary_to_list(atom_to_binary(Atom)).
