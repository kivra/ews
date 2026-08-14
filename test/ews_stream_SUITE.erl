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
        , skip_progress/1
        , container_as_record/1
        , empty_container/1
        , not_a_list_field/1
        , broken_xml/1
        , xml_stream_api/1
        , xml_stream_target_form/1
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
    , skip_progress
    , container_as_record
    , empty_container
    , not_a_list_field
    , broken_xml
    , xml_stream_api
    , xml_stream_target_form
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
    {done, Streamed, Trailers, St} = stream_all(Xml, byte_size(Xml), 1000, 0),
    ?assertEqual(N, length(Streamed)),
    ?assertEqual(N, ews_stream:seen(St)),
    ?assertEqual(whole_decode_items(Xml), Streamed),
    ?assertEqual(expected_items(N), Streamed),
    %% The trailers are the document around the streamed elements, with
    %% the streamed field set to [].
    ?assertEqual(expected_trailers(), Trailers),
    ok.

%% Tiny chunks stress every buffer boundary: tags, text and entities
%% split at arbitrary byte positions must not change the result.
chunked_parity(_Config) ->
    N = 25,
    Xml = doc(N),
    Expected = expected_items(N),
    [begin
         {done, Streamed, Trailers, _} = stream_all(Xml, ChunkSize, 1000, 0),
         ?assertEqual(Expected, Streamed),
         ?assertEqual(expected_trailers(), Trailers)
     end || ChunkSize <- [1, 7, 256]],
    ok.

%% The max_reached message tells the caller a full batch was returned
%% (no length checking); buffered data is drained with empty chunks and
%% the batches concatenate to the full result, ending with trailers.
max_and_drain(_Config) ->
    N = 20,
    Max = 3,
    Xml = doc(N),
    {ok, {max_reached, Max, 0, First, St0}} =
        ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                          Xml, <<>>, Max, 0),
    ?assertEqual(Max, length(First)),
    {Batches, Trailers, StN} = drain(St0, Max, []),
    All = First ++ lists:append(Batches),
    ?assertEqual(expected_items(N), All),
    ?assertEqual(N, ews_stream:seen(StN)),
    ?assertEqual(expected_trailers(), Trailers),
    [?assert(length(B) =< Max) || B <- Batches],
    %% Calling again after trailers just repeats the trailers.
    {ok, {trailers, 0, 0, [], Trailers, _}} =
        ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                          <<>>, StN, Max, 0),
    ok.

%% Skip fast-forwards a restarted stream: skipping all but the last two
%% items returns exactly those two.
skip_restart(_Config) ->
    N = 30,
    Xml = doc(N),
    {done, Tail, _Trailers, St} = stream_all(Xml, 512, 1000, N - 2),
    ?assertEqual(lists:nthtail(N - 2, expected_items(N)), Tail),
    ?assertEqual(N, ews_stream:seen(St)),
    ok.

%% A restart that spans several calls and chunks can be followed via
%% SkipLeft: it counts down to 0 while records are withheld, and calls
%% that only skip return cont with Count = 0 - never max_reached, and
%% skipped elements are not included in Count or Records.
skip_progress(_Config) ->
    N = 30,
    Skip = 25,
    Xml = doc(N),
    Msgs = collect_msgs(chunk(Xml, 256), <<>>, 7, Skip, []),
    %% Calls that still have skipping left decoded nothing, but their
    %% Count reports how many elements they skipped.
    [begin
         ?assertEqual([], element(4, M)),
         ?assertNotEqual(max_reached, element(1, M))
     end || M <- Msgs, element(3, M) > 0],
    %% Count includes skipped elements: the counts sum to the absolute
    %% stream position - the Skip value for a restart.
    ?assertEqual(N, lists:sum([element(2, M) || M <- Msgs])),
    %% SkipLeft counts down monotonically from Skip to 0.
    SkipLefts = [element(3, M) || M <- Msgs],
    ?assertEqual(SkipLefts, lists:reverse(lists:sort(SkipLefts))),
    ?assert(hd(SkipLefts) > 0),
    ?assertEqual(0, lists:last(SkipLefts)),
    %% Only the last N - Skip records come out, and the stream ends
    %% with trailers as usual.
    Records = lists:append([element(4, M) || M <- Msgs]),
    ?assertEqual(lists:nthtail(Skip, expected_items(N)), Records),
    {trailers, _, 0, _, Trailers, St} = lists:last(Msgs),
    ?assertEqual(expected_trailers(), Trailers),
    ?assertEqual(N, ews_stream:seen(St)),
    ok.

%% The container can be given as a record tuple instead of its alias.
container_as_record(_Config) ->
    N = 3,
    Xml = doc(N),
    {ok, {trailers, N, 0, Streamed, _Trailers, _}} =
        ews:stream_decode(?MODEL, {items_type, undefined}, ?ITEM_FIELD_IX,
                          Xml, <<>>, 1000, 0),
    ?assertEqual(expected_items(N), Streamed),
    ok.

%% Documents with an empty, self-closed or absent container produce no
%% records; the trailers still carry the container with the streamed
%% field set to [].
empty_container(_Config) ->
    [begin
         Xml = doc_with_items(Items),
         {done, [], Trailers, St} = stream_all(Xml, 32, 1000, 0),
         ?assertEqual(0, ews_stream:seen(St)),
         ?assertEqual(expected_trailers(), Trailers)
     end || Items <- ["<i:Items></i:Items>", "<i:Items/>", ""]],
    ok.

%% Selecting a field that is not a repeated element (maxOccurs 1, a
%% non-list record field) is an error: streaming a single-occurrence
%% element makes no sense.
not_a_list_field(_Config) ->
    Xml = doc(1),
    %% #batch.batch_info: a single-occurrence complex element.
    ?assertEqual({error, {not_a_list, {?BATCH_NS, "BatchInfo"}}},
                 ews:stream_decode(?MODEL, batch, 2, Xml, <<>>, 10, 0)),
    %% #batch.items: the container itself also occurs only once.
    ?assertEqual({error, {not_a_list, {?ITEM_NS, "Items"}}},
                 ews:stream_decode(?MODEL, batch, 3, Xml, <<>>, 10, 0)),
    %% #batch_info_type.created: a single-occurrence string element.
    ?assertEqual({error, {not_a_list, {?BATCH_NS, "Created"}}},
                 ews:stream_decode(?MODEL, batch_info_type, 2,
                                   Xml, <<>>, 10, 0)),
    %% An alias that is not in the model at all.
    ?assertEqual({error, {not_in_model, no_such_type}},
                 ews:stream_decode(?MODEL, no_such_type, 2,
                                   Xml, <<>>, 10, 0)),
    ok.

%% Broken or unexpected input always comes back as {error, Reason} or
%% as a cont that never turns into trailers (the caller detects
%% truncation at end of input) - never as an exception or silently
%% wrong records.
broken_xml(_Config) ->
    %% Mismatched close tag: broken nesting is an error.
    Bad1 = doc_with_items(
             "<i:Items><i:Item><i:Id>1</i:Uh></i:Item></i:Items>"),
    ?assertMatch({error, {unmatched_close_tag, _}},
                 ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                                   Bad1, <<>>, 10, 0)),
    %% A child element that is not in the schema.
    Bad2 = doc_with_items(
             "<i:Items><i:Item><i:Id>1</i:Id>"
             "<i:Bogus>x</i:Bogus></i:Item></i:Items>"),
    ?assertMatch({error, _},
                 ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                                   Bad2, <<>>, 10, 0)),
    %% An enumeration value that is not in the schema.
    Bad3 = doc_with_items(
             "<i:Items><i:Item><i:Id>1</i:Id>"
             "<i:Status>BOGUS</i:Status></i:Item></i:Items>"),
    ?assertMatch({error, _},
                 ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                                   Bad3, <<>>, 10, 0)),
    %% Input that is not XML at all: nothing to decode, more input
    %% might still bring the document - cont with zero records.
    {ok, {cont, 0, 0, [], _}} =
        ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                          <<"hello world">>, <<>>, 10, 0),
    %% A truncated document never produces trailers; the caller sees
    %% cont at end of input.
    Whole = doc(5),
    Truncated = binary:part(Whole, 0, byte_size(Whole) - 40),
    {cont, Some, _} = stream_all(Truncated, 128, 10, 0),
    ?assert(length(Some) < 5),
    %% A well-formed document of a completely different shape: no
    %% target elements, and trailers that cannot be decoded against
    %% the model come back as undefined.
    Other = <<"<Other xmlns=\"http://example.com/other\">"
              "<Foo>1</Foo></Other>">>,
    {ok, {trailers, 0, 0, [], undefined, _}} =
        ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                          Other, <<>>, 10, 0),
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
    %% The completed root is available, without the emitted elements.
    {ok, {RootQ, _, RootChildren}} = ews_xml:stream_root(S1),
    ?assertEqual({?BATCH_NS, "Batch"}, RootQ),
    ?assertMatch([{{?BATCH_NS, "BatchInfo"}, _, _},
                  {{?ITEM_NS, "Items"}, _, []}], RootChildren),
    %% Same terms as one-shot decoding.
    {Terms2, S3} = ews_xml:decode(doc(2), ews_xml:stream_new(Target)),
    ?assertEqual(Terms, Terms2),
    ?assert(ews_xml:stream_done(S3)),
    %% Not done while the root element is still open.
    {[], S4} = ews_xml:decode("<Batch xmlns=\"" ?BATCH_NS "\">",
                              ews_xml:stream_new(Target)),
    ?assertNot(ews_xml:stream_done(S4)),
    ?assertEqual(undefined, ews_xml:stream_root(S4)),
    ok.

%% The target qname comes from the model, so it is bare for an element its
%% schema declares unqualified. A document that qualifies such an element
%% anyway must still stream, rather than silently yielding nothing -- the same
%% tolerance the non-streaming decode has.
xml_stream_target_form(_Config) ->
    Qualified = <<"<Batch xmlns:i=\"" ?ITEM_NS "\">"
                  "<i:Item>1</i:Item><i:Item>2</i:Item></Batch>">>,
    Bare = <<"<Batch><Item>1</Item><Item>2</Item></Batch>">>,
    %% Bare target, qualified document.
    {BareTargetTerms, _} = ews_xml:decode(Qualified, ews_xml:stream_new("Item")),
    ?assertEqual(2, length(BareTargetTerms)),
    %% Qualified target, bare document.
    {QTargetTerms, _} =
        ews_xml:decode(Bare, ews_xml:stream_new({?ITEM_NS, "Item"})),
    ?assertEqual(2, length(QTargetTerms)),
    %% A genuinely different element is still not a target.
    {[], _} = ews_xml:decode(Bare, ews_xml:stream_new("Other")),
    ok.

%%% Helpers --------------------------------------------------------------

%% Feed Xml through ews:stream_decode/7 in ChunkSize pieces and collect
%% all records. Every Count is asserted against the returned batch.
stream_all(Xml, ChunkSize, Max, Skip) ->
    stream_chunks(chunk(Xml, ChunkSize), <<>>, Max, Skip, []).

stream_chunks([Chunk | Chunks], Rest, Max, Skip, Acc) ->
    case ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                           Chunk, Rest, Max, Skip) of
        {ok, {trailers, Count, _SkipLeft, Records, Trailers, St}} ->
            true = Count >= length(Records),
            {done, lists:append(lists:reverse([Records | Acc])), Trailers,
             St};
        {ok, {max_reached, Max, _SkipLeft, Records, St}} ->
            %% Full batch: drain buffered data before feeding more input.
            Max = length(Records),
            stream_chunks([<<>> | Chunks], St, Max, Skip, [Records | Acc]);
        {ok, {cont, Count, _SkipLeft, Records, St}} when Chunks =/= [] ->
            true = Count >= length(Records),
            stream_chunks(Chunks, St, Max, Skip, [Records | Acc]);
        {ok, {cont, _Count, _SkipLeft, Records, St}} ->
            {cont, lists:append(lists:reverse([Records | Acc])), St}
    end.

%% Like stream_chunks/5 but returns every message, in order.
collect_msgs([Chunk | Chunks], Rest, Max, Skip, Acc) ->
    {ok, Msg} = ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                                  Chunk, Rest, Max, Skip),
    case Msg of
        {trailers, _, _, _, _, _St} ->
            lists:reverse([Msg | Acc]);
        {max_reached, _, _, _, St} ->
            collect_msgs([<<>> | Chunks], St, Max, Skip, [Msg | Acc]);
        {cont, _, _, _, St} when Chunks =/= [] ->
            collect_msgs(Chunks, St, Max, Skip, [Msg | Acc]);
        {cont, _, _, _, _St} ->
            lists:reverse([Msg | Acc])
    end.

drain(St, Max, Acc) ->
    case ews:stream_decode(?MODEL, items_type, ?ITEM_FIELD_IX,
                           <<>>, St, Max, 0) of
        {ok, {trailers, Count, _SkipLeft, Records, Trailers, StN}} ->
            true = Count >= length(Records),
            {lists:reverse([Records | Acc]), Trailers, StN};
        {ok, {max_reached, Max, _SkipLeft, Records, St1}} ->
            drain(St1, Max, [Records | Acc]);
        {ok, {cont, _Count, _SkipLeft, Records, St1}} ->
            drain(St1, Max, [Records | Acc])
    end.

chunk(Bin, Size) when byte_size(Bin) =< Size ->
    [Bin];
chunk(Bin, Size) ->
    <<C:Size/binary, Rest/binary>> = Bin,
    [C | chunk(Rest, Size)].

whole_decode_items(Xml) ->
    {batch, _Info, {items_type, Items}} = ews:decode(?MODEL, Xml),
    Items.

%% The document around the streamed elements, streamed field set to [].
expected_trailers() ->
    {batch,
     {batch_info_type, <<"2026-01-01">>, <<"unit & integration">>},
     {items_type, []}}.

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
