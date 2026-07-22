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

The XML is parsed incrementally by `ews_xml:decode/2`, which returns
completed target elements as xml terms as soon as their closing tags
have been seen; each term is then validated against the model into
its record. Namespace prefixes declared on ancestor elements
(typically the document root) resolve naturally since the parser sees
the whole document from the start.
""".

-export([decode/7, seen/1]).

-include("ews.hrl").
-include("ews_plan.hrl").
-include_lib("ews/include/ews.hrl").

-opaque state() :: #{xml := ews_xml:stream(),
                     pending := [ews_xml:xml_data()],
                     plan := #pdoc{},
                     seen := non_neg_integer()}.
-export_type([state/0]).

-doc """
Decode up to Max target elements from Chunk (+ previously
buffered data). Rest is `<<>>` or `undefined` on the first call;
on subsequent calls pass the state returned by the previous call.
The first Skip target elements of the stream are skipped without
being decoded, which allows restarting an interrupted stream from
the top of the file. Returns
`{ok, Records, State}` - feed more data (or an empty chunk if
exactly Max records were returned, to drain buffered data),
`{done, Records, State}` - no more target elements in the stream.
""".
-spec decode(ModelRef :: atom(),
             Container :: tuple() | atom(),
             RecordIdx :: pos_integer(),
             Chunk :: binary(),
             Rest :: binary() | undefined | state(),
             Max :: pos_integer(),
             Skip :: non_neg_integer()) ->
          {ok, Records :: [tuple()], state()} |
          {done, Records :: [tuple()], state()}.
decode(ModelRef, Container, RecordIdx, Chunk, Rest, Max, Skip)
  when Rest =:= undefined; is_binary(Rest) ->
    State = init(ModelRef, Container, RecordIdx),
    Chunk1 = case Rest of
                 undefined -> Chunk;
                 <<>> -> Chunk;
                 _ -> <<Rest/binary, Chunk/binary>>
             end,
    decode(ModelRef, Container, RecordIdx, Chunk1, State, Max, Skip);
decode(_ModelRef, _Container, _RecordIdx, Chunk, #{xml := _} = State, Max, Skip)
  when is_binary(Chunk), is_integer(Max), Max > 0,
       is_integer(Skip), Skip >= 0 ->
    run(State, Chunk, Max, Skip).

-doc """
Number of target elements consumed (skipped + decoded) since the
start of the stream. Use as the Skip argument when restarting.
""".
-spec seen(State :: state()) -> non_neg_integer().
seen(#{seen := Seen}) ->
    Seen.

%% ----------------------------------------------------------------------------

init(ModelRef, Container, RecordIdx) ->
    #model{type_map = Tbl} = Model = ews_svc:get_model(ModelRef),
    Alias = case Container of
                A when is_atom(A) -> A;
                T when is_tuple(T) -> element(1, T)
            end,
    #type{attrs = Attrs} = ews_model:get(Alias, Tbl),
    Parts = ews_model:get_parts(Alias, Tbl),
    %% Records for types with attributes have '__attrs' as their first
    %% field, so the parts list is offset one extra step.
    Offset = case Attrs of [] -> 1; [_|_] -> 2 end,
    #elem{qname = Qname} = Elem = lists:nth(RecordIdx - Offset, Parts),
    %% Resolve all model lookups once; every emitted term is then decoded
    %% with the compiled plan instead of the interpretive decoder.
    Plan = ews_serialize:compile_elem_plan(Elem, Model),
    #{xml => ews_xml:stream_new(Qname), pending => [],
      plan => Plan, seen => 0}.

run(#{xml := Xml0, pending := Pending0, seen := Seen0,
      plan := Plan} = State, Chunk, Max, Skip) ->
    {Terms, Xml} = ews_xml:decode(Chunk, Xml0),
    {Records, Pending, Seen} =
        take(Pending0 ++ Terms, Seen0, Skip, Max, Plan, [], 0),
    NewState = State#{xml := Xml, pending := Pending, seen := Seen},
    case Pending =:= [] andalso ews_xml:stream_done(Xml) of
        true -> {done, Records, NewState};
        false -> {ok, Records, NewState}
    end.

%% Skip terms while Seen < Skip, then decode up to Max terms into
%% records. Anything beyond Max stays pending for the next call.
take([_Term | Terms], Seen, Skip, Max, Plan, Acc, N) when Seen < Skip ->
    take(Terms, Seen + 1, Skip, Max, Plan, Acc, N);
take([Term | Terms], Seen, Skip, Max, Plan, Acc, N) when N < Max ->
    Record = ews_serialize:decode_compiled(Plan, [Term]),
    take(Terms, Seen + 1, Skip, Max, Plan, [Record | Acc], N + 1);
take(Terms, Seen, _Skip, _Max, _Plan, Acc, _N) ->
    {lists:reverse(Acc), Terms, Seen}.
