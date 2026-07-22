%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2026 Kivra
%%%
%%% Distribution subject to the terms of the LGPL-3.0-or-later, see
%%% the COPYING.LESSER file in the root of the distribution
%%%
%%% Tests for streaming decode (ews:stream_decode/7, ews_stream,
%%% ews_xml:decode/2) using the generic batch/items schema in stream.wsdl:
%%% a Batch root element holds an Items container with an unbounded
%%% sequence of Item elements, spread over two namespaces whose prefixes
%%% are declared on the document root.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ews_stream_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

-export([ whole_doc_parity/1
        , chunked_parity/1
        , max_and_drain/1
        , skip_restart/1
        , container_as_record/1
        , empty_container/1
        , not_a_list_field/1
        , xml_stream_api/1
        ]).

-define(MODEL, stream_batch).
-define(ITEM_FIELD_IX, 2). %% #items_type.item

-define(BATCH_NS, "http://example.com/schema/batch").
-define(ITEM_NS, "http://example.com/schema/item").

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    [ whole_doc_parity
    , chunked_parity
    , max_and_drain
    , skip_restart
    , container_as_record
    , empty_container
    , not_a_list_field
    , xml_stream_api
    ].

init_per_suite(Config) ->
    ews:start(),
    application:load(ews),
    Dir = filename:join(code:priv_dir(ews), "../test"),
    {ok, Bin} = file:read_file(filename:join(Dir, "stream.wsdl")),
    {ok, [{"BatchService", 1}]} = ews_svc:add_wsdl_bin(?MODEL, Bin),
    Config.

end_per_suite(_Config) ->
    ews_svc:remove_model(?MODEL),
    ok.

%% Streaming the whole document in one chunk gives exactly the item
%% records that a whole-document ews:decode/2 gives.
whole_doc_parity(_Config) ->
    N = 100,
    Xml = doc(N),
    {done, Streamed, St} = stream_all(Xml, byte_size(Xml), 1000, 0),
    ?assertEqual(N, length(Streamed)),
    ?assertEqual(N, ews_stream:seen(St)),
    ?assertEqual(whole_decode_items(Xml), Streamed),
    ?assertEqual(expected_items(N), Streamed),
    ok.

%% Tiny chunks stress every buffer boundary: tags, text and entities
%% split at arbitrary byte positions must not change the result.
chunked_parity(_Config) ->
    N = 25,
    Xml = doc(N),
    Expected = expected_items(N),
    [begin
         {done, Streamed, _} = stream_all(Xml, ChunkSize, 1000, 0),
         ?assertEqual(Expected, Streamed)
     end || ChunkSize <- [1, 7, 256]],
    ok.

%% At most Max records are returned per call; buffered data is drained
%% with empty chunks and the batches concatenate to the full result.
max_and_drain(_Config) ->
    N = 20,
    Max = 3,
    Xml = doc(N),
    {ok, First, St0} =
        ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                          Xml, <<>>, Max, 0),
    ?assertEqual(Max, length(First)),
    {Batches, StN} = drain(St0, Max, []),
    All = First ++ lists:append(Batches),
    ?assertEqual(expected_items(N), All),
    ?assertEqual(N, ews_stream:seen(StN)),
    [?assert(length(B) =< Max) || B <- Batches],
    ok.

%% Skip fast-forwards a restarted stream: skipping all but the last two
%% items returns exactly those two.
skip_restart(_Config) ->
    N = 30,
    Xml = doc(N),
    {done, Tail, St} = stream_all(Xml, 512, 1000, N - 2),
    ?assertEqual(lists:nthtail(N - 2, expected_items(N)), Tail),
    ?assertEqual(N, ews_stream:seen(St)),
    ok.

%% The container can be given as a record tuple instead of its alias.
container_as_record(_Config) ->
    N = 3,
    Xml = doc(N),
    {done, Streamed, _} =
        ews:stream_decode(?MODEL, {items_type, undefined}, ?ITEM_FIELD_IX,
                          Xml, <<>>, 1000, 0),
    ?assertEqual(expected_items(N), Streamed),
    ok.

%% Documents with an empty, self-closed or absent container produce no
%% records and finish with done.
empty_container(_Config) ->
    [begin
         Xml = doc_with_items(Items),
         {done, [], St} = stream_all(Xml, 32, 1000, 0),
         ?assertEqual(0, ews_stream:seen(St))
     end || Items <- ["<i:Items></i:Items>", "<i:Items/>", ""]],
    ok.

%% Selecting a field that is not a repeated element (maxOccurs 1, a
%% non-list record field) is an error: streaming a single-occurrence
%% element makes no sense.
not_a_list_field(_Config) ->
    Xml = doc(1),
    %% #batch.batch_info: a single-occurrence complex element.
    ?assertError({not_a_list, {?BATCH_NS, "BatchInfo"}},
                 ews:stream_decode(?MODEL, batch, 2, Xml, <<>>, 10, 0)),
    %% #batch.items: the container itself also occurs only once.
    ?assertError({not_a_list, {?ITEM_NS, "Items"}},
                 ews:stream_decode(?MODEL, batch, 3, Xml, <<>>, 10, 0)),
    %% #batch_info_type.created: a single-occurrence string element.
    ?assertError({not_a_list, {?BATCH_NS, "Created"}},
                 ews:stream_decode(?MODEL, batch_info_type, 2,
                                   Xml, <<>>, 10, 0)),
    ok.

%% The incremental xml parser: decode as much as possible per chunk,
%% keep partial input, and report done when the root element closes.
xml_stream_api(_Config) ->
    Target = {?ITEM_NS, "Item"},
    S0 = ews_xml:stream_new(Target),
    Doc = binary_to_list(doc(2)),
    %% Split in the middle of a tag/text/entity: byte-by-byte feeding.
    {Terms, S1} =
        lists:foldl(fun(Char, {Acc, S}) ->
                            {Ts, S2} = ews_xml:decode([Char], S),
                            {Acc ++ Ts, S2}
                    end, {[], S0}, Doc),
    ?assertEqual(2, length(Terms)),
    [?assertMatch({Target, [], [_ | _]}, T) || T <- Terms],
    ?assert(ews_xml:stream_done(S1)),
    %% Same terms as one-shot decoding.
    {Terms2, S3} = ews_xml:decode(doc(2), ews_xml:stream_new(Target)),
    ?assertEqual(Terms, Terms2),
    ?assert(ews_xml:stream_done(S3)),
    %% Not done while the root element is still open.
    {[], S4} = ews_xml:decode("<Batch xmlns=\"" ?BATCH_NS "\">",
                              ews_xml:stream_new(Target)),
    ?assertNot(ews_xml:stream_done(S4)),
    ok.

%%% Helpers --------------------------------------------------------------

%% Feed Xml through ews:stream_decode/7 in ChunkSize pieces and collect
%% all records.
stream_all(Xml, ChunkSize, Max, Skip) ->
    stream_chunks(chunk(Xml, ChunkSize), <<>>, Max, Skip, []).

stream_chunks([Chunk | Chunks], Rest, Max, Skip, Acc) ->
    case ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                           Chunk, Rest, Max, Skip) of
        {done, Records, St} ->
            {done, lists:append(lists:reverse([Records | Acc])), St};
        {ok, Records, St} when length(Records) =:= Max ->
            %% Full batch: drain buffered data before feeding more input.
            stream_chunks([<<>> | Chunks], St, Max, Skip, [Records | Acc]);
        {ok, Records, St} when Chunks =/= [] ->
            stream_chunks(Chunks, St, Max, Skip, [Records | Acc]);
        {ok, Records, St} ->
            {ok, lists:append(lists:reverse([Records | Acc])), St}
    end.

drain(St, Max, Acc) ->
    case ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                           <<>>, St, Max, 0) of
        {done, Records, StN} -> {lists:reverse([Records | Acc]), StN};
        {ok, Records, St1} -> drain(St1, Max, [Records | Acc])
    end.

chunk(Bin, Size) when byte_size(Bin) =< Size ->
    [Bin];
chunk(Bin, Size) ->
    <<C:Size/binary, Rest/binary>> = Bin,
    [C | chunk(Rest, Size)].

whole_decode_items(Xml) ->
    {batch, _Info, {items_type, Items}} = ews:decode(?MODEL, Xml),
    Items.

%% A Batch document with N items. Namespace prefixes are declared on the
%% root element only, as is common for documents of this kind.
doc(N) ->
    doc_with_items(["<i:Items>\n",
                    [item(I) || I <- lists:seq(1, N)],
                    "</i:Items>\n"]).

doc_with_items(Items) ->
    iolist_to_binary(
      ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
       "<Batch xmlns=\"" ?BATCH_NS "\" xmlns:i=\"" ?ITEM_NS "\">\n"
       "<BatchInfo><Created>2026-01-01</Created>"
       "<Source>unit &amp; integration</Source></BatchInfo>\n",
       Items,
       "</Batch>\n"]).

item(N) ->
    ["<i:Item><i:Id>", integer_to_list(N), "</i:Id>",
     case N rem 7 of
         0 -> "";
         _ -> ["<i:Name>Item ", integer_to_list(N), " &lt;&amp;&gt;</i:Name>"]
     end,
     case N rem 2 of
         0 -> "<i:Status>INACTIVE</i:Status>";
         _ -> "<i:Status>ACTIVE</i:Status>"
     end,
     case N rem 5 of
         0 -> "<i:Note/>"; %% present but empty -> decodes to undefined
         1 -> "";
         _ -> ["<i:Note><i:Text>note ", integer_to_list(N),
               "</i:Text></i:Note>"]
     end,
     "</i:Item>\n"].

expected_items(N) ->
    [expected_item(I) || I <- lists:seq(1, N)].

expected_item(N) ->
    Name = case N rem 7 of
               0 -> undefined;
               _ -> unicode:characters_to_binary(
                      ["Item ", integer_to_list(N), " <&>"])
           end,
    Status = case N rem 2 of
                 0 -> inactive;
                 _ -> active
             end,
    Note = case N rem 5 of
               0 -> undefined;
               1 -> undefined;
               _ -> {note_type,
                     unicode:characters_to_binary(
                       ["note ", integer_to_list(N)])}
           end,
    {item_type, integer_to_binary(N), Name, Status, Note}.
