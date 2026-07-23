%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2026 Kivra
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
-module(ews_stream).
-moduledoc """
Streaming decode of a repeated child element inside a large XML
document, without ever holding the whole document (or the whole
decoded result) in memory.

The target element is identified by a container record (or its
alias) plus the record field index of the repeated child, e.g.
`#items_type{}` and `#items_type.item` for a schema where an
`Items` container element holds an unbounded sequence of `Item`
elements.

Every call returns `{ok, Msg}` or `{error, Reason}`, where Msg tells
the caller what to do next without inspecting the records:

- `{cont, Count, SkipLeft, Records, State}` - the buffered input is
  exhausted; feed more data.
- `{max_reached, Count, SkipLeft, Records, State}` - Max records were
  decoded; call again with an empty chunk to drain buffered data
  before feeding more input.
- `{trailers, Count, SkipLeft, Records, Trailers, State}` - the
  stream has ended. Trailers is the decoded document *around* the
  streamed elements: the whole document record with the streamed
  field set to the empty list (or `undefined` if the root cannot be
  decoded).

Count is the number of elements consumed in this call, INCLUDING
skipped ones, so summing Count over all calls gives the absolute
position in the stream - exactly the value to pass as Skip when
restarting. The decoded records of the call are Records (so
`length(Records)` is the decoded count; the difference is what was
skipped). Skipped elements never appear in Records and do not count
towards Max - a call that only skips returns
`{cont, Count, SkipLeft, [], State}`, never max_reached.

SkipLeft is how many of the Skip elements remain to be skipped after
this call, so a restart that spans several calls and chunks can be
followed as it fast-forwards: suppose a run with Max 1000 died after
10000 processed elements. Restart from the top of the file with
Skip = 10000 and feed chunks as usual; the messages count SkipLeft
down from 10000 to 0 (with Records = [] on the way, Count showing
how many elements each call skipped), and decoding then resumes at
element 10001.

The XML is parsed incrementally by `ews_xml:decode/2`, which returns
completed target elements as xml terms as soon as their closing tags
have been seen; each term is then decoded with a compiled plan.
Namespace prefixes declared on ancestor elements (typically the
document root) resolve naturally since the parser sees the whole
document from the start.
""".

-export([decode/7, seen/1]).

-include("ews.hrl").
-include("ews_plan.hrl").
-include_lib("ews/include/ews.hrl").

-opaque state() :: #{xml := ews_xml:stream(),
                     pending := [ews_xml:xml_data()],
                     plan := #pdoc{},
                     model := #model{},
                     container := {atom(), {string(), string()},
                                   pos_integer()},
                     trailers := undefined | {done, tuple() | undefined},
                     seen := non_neg_integer()}.
-type ews_stream_msg() ::
        {cont, Count :: non_neg_integer(), SkipLeft :: non_neg_integer(),
         Records :: [tuple()], state()} |
        {max_reached, Count :: non_neg_integer(),
         SkipLeft :: non_neg_integer(), Records :: [tuple()], state()} |
        {trailers, Count :: non_neg_integer(),
         SkipLeft :: non_neg_integer(), Records :: [tuple()],
         Trailers :: tuple() | undefined, state()}.
-export_type([state/0, ews_stream_msg/0]).

-doc """
Decode up to Max target elements from Chunk (+ previously
buffered data). Rest is `<<>>` or `undefined` on the first call;
on subsequent calls pass the state returned inside the previous
message. The first Skip target elements of the stream are skipped
without being decoded, which allows restarting an interrupted stream
from the top of the file.

Returns `{ok, Msg}` (see the ews_stream_msg() type and the module doc) or
`{error, Reason}` - e.g. `{error, {not_a_list, Qname}}` if the
selected field is not a repeated element (maxOccurs 1 in the schema,
a non-list field in the record).
""".
-spec decode(ModelRef :: atom(),
             ContainingRecord :: tuple() | atom(),
             RecordIdx :: pos_integer(),
             Chunk :: binary(),
             Rest :: binary() | undefined | state(),
             Max :: pos_integer(),
             Skip :: non_neg_integer()) ->
          {ok, ews_stream_msg()} | {error, term()}.
decode(ModelRef, ContainingRecord, RecordIdx, Chunk, Rest, Max, Skip)
  when Rest =:= undefined; is_binary(Rest) ->
    try init(ModelRef, ContainingRecord, RecordIdx) of
        State ->
            Chunk1 = case Rest of
                         undefined -> Chunk;
                         <<>> -> Chunk;
                         _ -> <<Rest/binary, Chunk/binary>>
                     end,
            decode(ModelRef, ContainingRecord, RecordIdx, Chunk1, State,
                   Max, Skip)
    catch
        error:Reason ->
            {error, Reason}
    end;
decode(_ModelRef, _ContainingRecord, _RecordIdx, Chunk,
       #{xml := _} = State, Max, Skip)
  when is_binary(Chunk), is_integer(Max), Max > 0,
       is_integer(Skip), Skip >= 0 ->
    try
        run(State, Chunk, Max, Skip)
    catch
        error:Reason ->
            {error, Reason}
    end.

-doc """
Number of target elements consumed (skipped + decoded) since the
start of the stream. Use as the Skip argument when restarting.
""".
-spec seen(State :: state()) -> non_neg_integer().
seen(#{seen := Seen}) ->
    Seen.

%% ----------------------------------------------------------------------------

init(ModelRef, ContainingRecord, RecordIdx) ->
    #model{type_map = Tbl} = Model = ews_svc:get_model(ModelRef),
    Alias = case ContainingRecord of
                A when is_atom(A) -> A;
                T when is_tuple(T) -> element(1, T)
            end,
    case ews_model:get(Alias, Tbl) of
        false ->
            error({not_in_model, Alias});
        #type{qname = ContainerKey, attrs = Attrs} ->
            Parts = ews_model:get_parts(Alias, Tbl),
            %% Records for types with attributes have '__attrs' as their
            %% first field, so the parts list is offset one extra step.
            Offset = case Attrs of [] -> 1; [_|_] -> 2 end,
            #elem{qname = Qname, meta = Meta} = Elem =
                lists:nth(RecordIdx - Offset, Parts),
            %% Streaming only makes sense for a repeated element (a list
            %% field in the record, maxOccurs > 1 in the schema).
            case Meta of
                #meta{max = Max} when Max =:= infinite;
                                      is_integer(Max) andalso Max > 1 ->
                    ok;
                _ ->
                    error({not_a_list, Qname})
            end,
            %% Resolve all model lookups once; every emitted term is then
            %% decoded with the compiled plan instead of the interpretive
            %% decoder.
            Plan = ews_serialize:compile_elem_plan(Elem, Model),
            #{xml => ews_xml:stream_new(Qname), pending => [],
              plan => Plan, model => Model,
              container => {Alias, ContainerKey, RecordIdx},
              trailers => undefined, seen => 0}
    end.

run(#{trailers := {done, Trailers}, seen := Seen} = State,
    _Chunk, _Max, Skip) ->
    {ok, {trailers, 0, skip_left(Skip, Seen), [], Trailers, State}};
run(#{xml := Xml0, pending := Pending0, seen := Seen0,
      plan := Plan} = State, Chunk, Max, Skip) ->
    {Terms, Xml} = ews_xml:decode(Chunk, Xml0),
    {Records, Decoded, Pending, Seen} =
        take(Pending0 ++ Terms, Seen0, Skip, Max, Plan, [], 0),
    NewState = State#{xml := Xml, pending := Pending, seen := Seen},
    %% Count includes skipped elements: the sum of Counts over all
    %% calls is the absolute position in the stream.
    Count = Seen - Seen0,
    SkipLeft = skip_left(Skip, Seen),
    case Pending =:= [] andalso ews_xml:stream_done(Xml) of
        true ->
            Trailers = build_trailers(NewState),
            FinalState = NewState#{trailers := {done, Trailers}},
            {ok, {trailers, Count, SkipLeft, Records, Trailers, FinalState}};
        false when Decoded =:= Max ->
            {ok, {max_reached, Count, SkipLeft, Records, NewState}};
        false ->
            {ok, {cont, Count, SkipLeft, Records, NewState}}
    end.

skip_left(Skip, Seen) when Skip > Seen -> Skip - Seen;
skip_left(_Skip, _Seen) -> 0.

%% Skip terms while Seen < Skip, then decode up to Max terms into
%% records. Anything beyond Max stays pending for the next call.
take([_Term | Terms], Seen, Skip, Max, Plan, Acc, N) when Seen < Skip ->
    take(Terms, Seen + 1, Skip, Max, Plan, Acc, N);
take([Term | Terms], Seen, Skip, Max, Plan, Acc, N) when N < Max ->
    Record = ews_serialize:decode_compiled(Plan, [Term]),
    take(Terms, Seen + 1, Skip, Max, Plan, [Record | Acc], N + 1);
take(Terms, Seen, _Skip, _Max, _Plan, Acc, N) ->
    {lists:reverse(Acc), N, Terms, Seen}.

%% ----------------------------------------------------------------------------
%% Trailers: the document around the streamed elements

%% Decode the completed root element (the emitted target elements are
%% not part of it) and set the streamed field of the container record
%% to []. A container that lost all its children decodes to undefined,
%% in which case a fresh container record is constructed in its place.
%% Best effort: returns undefined if the root cannot be decoded.
build_trailers(#{xml := Xml, model := #model{type_map = Tbl} = Model,
                 container := Container}) ->
    case ews_xml:stream_root(Xml) of
        undefined ->
            undefined;
        {ok, {RootQ, _, _} = RootTerm} ->
            case ews_model:get_elem(RootQ, Tbl) of
                false ->
                    undefined;
                #elem{} = RootElem ->
                    [Doc] = ews_serialize:decode([RootTerm], [RootElem],
                                                 Model),
                    fix_container(Doc, Container, Tbl)
            end
    end.

%% Walk the decoded document and set the streamed field of the container
%% record to [].
fix_container(Term, {CAlias, _, Idx} = C, Tbl)
  when is_tuple(Term), is_atom(element(1, Term)) ->
    case element(1, Term) of
        CAlias ->
            setelement(Idx, Term, []);
        Alias ->
            case ews_model:get(Alias, Tbl) of
                #type{attrs = Attrs} ->
                    Parts = ews_model:get_parts(Alias, Tbl),
                    Offset = case Attrs of [] -> 1; [_|_] -> 2 end,
                    fix_fields(Term, Parts, Offset + 1, C, Tbl);
                false ->
                    Term
            end
    end;
fix_container(List, C, Tbl) when is_list(List) ->
    [fix_container(E, C, Tbl) || E <- List];
fix_container(Term, _C, _Tbl) ->
    Term.

fix_fields(Term, [Part | Parts], FieldIx, {_, CKey, _} = C, Tbl) ->
    Value0 = element(FieldIx, Term),
    Value = case {Part, Value0} of
                {#elem{type = CKey}, undefined} ->
                    %% The container lost all its children to streaming
                    %% and decoded to undefined: rebuild it empty.
                    empty_container(C, Tbl);
                _ ->
                    fix_container(Value0, C, Tbl)
            end,
    fix_fields(setelement(FieldIx, Term, Value), Parts, FieldIx + 1, C, Tbl);
fix_fields(Term, [], _FieldIx, _C, _Tbl) ->
    Term.

empty_container({CAlias, _, Idx}, Tbl) ->
    #type{attrs = Attrs} = ews_model:get(CAlias, Tbl),
    Parts = ews_model:get_parts(CAlias, Tbl),
    Arity = 1 + (case Attrs of [] -> 0; [_|_] -> 1 end) + length(Parts),
    Empty = setelement(1, erlang:make_tuple(Arity, undefined), CAlias),
    setelement(Idx, Empty, []).
