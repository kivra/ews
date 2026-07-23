# ews
Erlang Library for interacting with SOAP Web Services.

## Introduction

ews is a library for interacting with SOAP web services. It includes functionality to

* generate a model from the WSDLs that describe the web services
* generate a model from plain XSD for file encoding and decoding
* emit an Erlang .hrl file defining records corresponding to the types in the model
* call web service operations with automatic encoding of operands and decoding of the response
* supply hooks that are applied immediately before or after the actual SOAP calls

## Changes between 5.1.1 and 5.2.0

* New: streaming decode of large XML documents with `ews:stream_decode/7`.
  A repeated child element (e.g. an unbounded sequence of `Item` elements
  inside an `Items` container) is decoded record by record from a document
  fed in chunks, without ever holding the whole document or the whole
  decoded result in memory. Returns `{ok, ews_stream:ews_stream_msg()}` or
  `{error, term()}`, where the message (`cont` | `max_reached` |
  `trailers`) carries the decoded-record count and, at end of stream,
  the decoded document around the streamed elements. See "Streaming
  decode of large documents" under Interface below.
* New: `ews_xml:decode/2` parses xml incrementally: it consumes as much as
  possible of each chunk, keeps partial input and parser state between
  calls, and emits completed target elements as xml terms.
* New: `ews_serialize:compile_elem_plan/2` builds a compiled decode plan
  for a single element; streaming decode uses it so each record is decoded
  without any model (ETS) lookups.
* Fix: `ews:decode_compiled/2` now decodes present-but-empty elements
  (e.g. `<Note/>`) exactly like `ews:decode/2` does.

## Changes between 5.0.0 and 5.1.0

* New: precompiled encode/decode plans for fast batch processing.
  `ews:compile_record_encoder/2` walks the model once and resolves every
  model (ETS) lookup into a reusable plan tree — plain records (see
  `src/ews_plan.hrl`), not closures, so a plan is human-inspectable.
  `ews:encode_compiled/2` and `ews:decode_compiled/2` apply the plan; the
  same plan drives both directions. Profiling batch encoding showed ~2/3
  of the time spent in repeated model lookups, which the plan eliminates.
  Constructs the compiler cannot specialise statically (type unions,
  polymorphic subtypes/xsi:type, inline simple types) fall back to the
  runtime encoder/decoder per node, so results are identical to
  `ews:encode/2` / `ews:decode/2`. See "Precompiled encode/decode plans"
  under Interface below.

## Changes between 4.4.0 and 5.0.0

* `soap_timeout` has been replaced by `connect_timeout` and `recv_timeout`.
* By default the hackney pool is now named after the model.

## Changes between 4.3.0 and 4.4.0
* Unions now work, but only if they have the same base since it usess the first base for all of the union.

## Changes between 4.2.0 and 4.3.0

* Add the possibility to add plain XSDs to the model to generate record for elements and types that aren't part of a WSDL.
* Encoding and decoding of any records in the model.

Known issues: Encoding and decoding of overloaded unnamed types like `Foo@bar_1` will probably not work.

## Changes between 4.1.0 and 4.2.0

* Change the order of emited types to `binary() | string()`. This will change almost every line in the generated hrls generated from previous versions.

## Changes between 4.0.0 and 4.1.0

* Support for `include` in schemas.
* Handle non-ASCII in types
* Fix for a bug that didn't decode emtpy records like `-record(foo, {}).`
* Handle `group`.
* Handle `simpleContent` with attributes by creating a record for it.

## Changes between 3.1.0 and 4.0.0

Breaking changes

* `ews:call_service_op` will now return `{ok, [term()]}` instead of `{ok, term()}`, cause there can actually  be more than one message returned.
* `ews:decode_in` will not return `{ok, {Svc, Op, list(Headers), list(Body)}}`

Other changes

* Non-optional attributes in `__attrs` will now have `:=` in their typespec
* A more consistent ordering of records. First in dep order, then namespace and lastly name. This will reorder everything once now, but less in the future.

## Changes between 3.0.1 and 3.1.0

* No more support for just verifying fingerprints. It has to be certificates.
* Fix of bug allowing expired certs to be verified.

## Changes between 2.0.0 and 3.0.0

Two breaking changes have been introduced in 3.0.0.

* Prehooks now take 6 arguments and should return 6 arguments.
* XSDs that define attributes now result in a records with an __attrs map.

### New Preehook arguments

Versions before 3.0.0 documented that the second argument to prehooks
was the Operation, but in fact it was the SOAPAction. From version 3.0.0
both these are now arguments so hooks now need 6 arguments and need to
return 6 arguments.

There is also a new type of pre_post hook that lets you modify the actual
rendered XML instead of the internal ews representation.

### New attributes support

Version 3.0.0 introduces an extra field called `__attrs` first of record
where the XSD defines attributes. `__attrs` is a map and keys should be
atoms.

### New simpleContent with attributes support

Version 4.1.0 introduces new logic for simpleContent with attributes.
Normally we don't emit records for simpleContent, but if the simpleContent
has attributes we have to. Since the simpleContent isn't an element we
don't have a name for the field in the record so it will get the special
name `value`. Example:

    -record(foo, {'__attrs' :: #{bar => string() | binary()} | undefined
                  value :: integer() | undefined}).

### New XSD support

In order to support encoding and decoding of XML-files that aren't in a message
in a WSDL, support for adding random XSDs has been added.

    ews:add_xsd_to_model(xsd_test, "test/xmldsig-core-schema.xsd").

    Encoded = ews:encode(xsd_test, #rsa_key_value_type{modulus = <<"15">>,
                                                       exponent = <<"3">>}).
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><p1:RSAKeyValueType x"
          "mlns:p1=\"http://www.w3.org/2000/09/xmldsig#\"><p1:Modulus>15</"
          "p1:Modulus><p1:Exponent>3</p1:Exponent></p1:RSAKeyValueType>">>

    ews:decode(xsd_test, Encoded).
        #rsa_key_value_type{modulus = <<"15">>,
                            exponent = <<"3">>}

Known issues: Encoding and decoding of overloaded unnamed types like `Foo@bar_1` will probably not work.

## Interface

ews is an Erlang application that can be started and stopped in the normal way.

All functions that take a model as a parameter have corresponding ones where you omit it in which case the model used will be `default`.

### Models

`ews:add_wsdl_to_model(Model :: atom(), URL :: list())`

Downloads the WSDL file specified by the `URL` and adds the contents to the specified model.

`ews:add_wsdl_to_model(Model :: atom(), WSDL :: binary())`

The WSDL file contents provided is added to the specified model.

`ews:remove_model(Model :: atom())`

Removes the specified model.

### Generating Erlang records for a model

`ews:emit_complete_model_types(Model :: atom(), FileName :: list())`

Emits the entire model as an Erlang .hrl file.

### Services

`ews:list_services(Model :: atom()) -> {ok, Service :: [list()]}`

Returns a list of the services defined in the specified model.

`ews:list_model_services() -> {ok, [{Model :: atom(), Service :: list()}]}`

Returns a list of all services ews knows about along with the model they belong to.

### Service operations

`ews:call_service_op(Model :: atom(), Service :: list(), Op :: list(), Header :: list(), Body :: term(), Options :: map()) -> {ok, Response :: term()} | {error, term()}`

Calls the specified service operation providing the given header and body. Valid options are
* `timeout => integer()` Call timeout in milliseconds
* `http_headers => list()` Extra HTTP headers added to the lhttpc call
* `include_http_response_headers => boolean()` When true, includes the HTTP response headers in the returned value

`ews:get_service_ops(Model :: atom(), Service :: list()) -> {ok, [OpName :: list()]} | {error, no_service}`

Returns a list of the available operations for the specified service in the given model.

`ews:get_service_op_info(Model :: atom(), Service :: list()) -> {ok, proplists:proplist()} | {error, no_service} | {error, no_op}`

Returns a list with detailed information about the given operation.

### Pre- and post-call hooks

It is possible to add pre-call hook functions that are called between the encoding of operation parameters and the HTTP call. You can also add post-call hook functions that are called between the response reception and the decoding of the response. This can be useful for metrics, logging, trouble shooting or to be able to handle strange stuff that WSDL authors come up with :)

A pre-call hook function is a fun of one argument which is a list `[Endpoint :: list(), Operation :: list, SoapAction :: list(), Headers :: list(), Body :: term(), Options :: map()]` which should return a list of the same type where the elements might be updated. This includes the `Options` map, which can be used to pass information to other pre-call and post-call hooks.

A pre-post-call hook function is a fun of one argument which is a list `[Endpoint :: list(), Operation :: list, XMLBody :: binary(), Options :: prop_list()]` which should return a list of the same type where the elements might be updated.

A post-call hook is a fun which takes as arument a list `[Headers :: list(), Body :: term(), Options :: map()]` and which should return a list of the same type.

If there are multiple pre- or post-call hooks they will be called in the order they were added, each one being passed the output of the previous one.

`ews:add_pre_hook(Model :: atom(), Hook :: fun()) -> HookRef :: ref()`

Adds a pre-call hook to the specified model.

`ews:add_pre_post_hook(Model :: atom(), Hook :: fun()) -> HookRef :: ref()`

Adds a pre-post-call hook to the specified model.

`ews:add_post_hook(Model :: atom(), Hook :: fun()) -> HookRef :: ref()`

Adds a post-call hook to the specified model.

`ews:remove_pre_hook(Model :: atom(), HookRef :: ref()) -> ok`

Removes a pre-call hook. The HookRef specified should be what the `add_pre_hook` call returned when the hook was added.

`ews:remove_pre_post_hook(Model :: atom(), HookRef :: ref()) -> ok`

Removes a pre-post-call hook. The HookRef specified should be what the `add_pre_hook` call returned when the hook was added.

`ews:remove_post_hook(Model :: atom(), HookRef :: ref()) -> ok`

Removes a post-call hook. The HookRef specified should be what the `add_post_hook` call returned when the hook was added.

### Record -> map transformation

`ews:record_to_map(Model :: atom(), Record :: record()) -> map()`

Converts a record representation of an entity in the model into a map representation.

### Encode and decode

There exist functions to encode and decode service operation parameters without actually doing the call. This was done to allow for a model where batch operations were specified as taking lists of encoded operations and returning lists of the individual results.

`ews:encode_service_op(Model :: atom(), Service :: list(), Op :: list(), Header :: list(), Body :: term(), Options :: map()) -> {ok, Xml :: term()} | {error, term()}`

Returns and encoding of the specified service operation providing the given header and body.

`ews:decode_service_op_result(Model :: atom(), Service :: list(), Op :: list(), Body :: term(), Options :: map()) -> {ok, term()} | {error, term()}`

Returns the Erlang representation of the provided result of calling the specified operation.

### Precompiled encode/decode plans

`ews:encode/2` and `ews:decode/2` repeat the same model (ETS) lookups for
every record. When processing a large homogeneous batch — encoding or
decoding many records of the same type — those lookups dominate. A plan
resolves them once, up front:

`ews:compile_record_encoder(Model :: atom(), Alias :: atom()) -> Plan`

Builds a reusable plan for records tagged with `Alias` (the record name,
as emitted in the generated .hrl). The plan is a tree of plain records
(see `src/ews_plan.hrl`), so it can be inspected in the shell. Raises
`error({not_in_model, Alias})` for an unknown alias.

`ews:encode_compiled(Plan, Record :: record()) -> Xml :: binary()`

Encodes one record with the plan. Same output as `ews:encode/2`.

`ews:decode_compiled(Plan, Xml :: binary()) -> record()`

Decodes an xml binary with the same plan. Same result as `ews:decode/2`.

Example, with a model containing `-record(item_type, {id, name})`:

    Plan = ews:compile_record_encoder(my_model, item_type),

    %% Encode a large batch, one xml document per record:
    Xmls = [ews:encode_compiled(Plan, Item) || Item <- Items],

    %% The same plan decodes too:
    #item_type{} = ews:decode_compiled(Plan, hd(Xmls)).

Constructs that cannot be specialised statically (type unions,
polymorphic subtypes/xsi:type, inline simple element types) fall back to
the runtime encoder/decoder for that node, so compiled results are
always identical to the uncompiled ones.

### Streaming decode of large documents

When a document is too large to decode in one go — typically a batch file
where one container element holds an unbounded number of child elements —
it can be decoded in a streaming fashion, feeding the raw XML in chunks
and receiving the repeated child as records, a bounded batch at a time.
Neither the whole document nor the whole decoded result is ever held in
memory.

Suppose a schema where the root element `Batch` contains an `Items`
container with an unbounded sequence of `Item` elements, giving these
generated records:

    -record(item_type, {id, name, status, note}).
    -record(items_type, {item :: [#item_type{}] | undefined}).

`ews:stream_decode(Model :: atom(), ContainingRecord :: tuple() | atom(), RecordIdx :: integer(), Chunk :: binary(), Rest, Max :: integer(), Skip :: integer()) -> {ok, ews_stream:ews_stream_msg()} | {error, term()}`

Decodes up to `Max` child records from `Chunk` plus any data buffered in
`Rest`. The repeated child is identified by the container record (or its
alias) and the record field index of the child, e.g. `#items_type{}` and
`#items_type.item`. Always returns a 2-tuple; the message inside `ok`
tells the caller what to do next (in the style of hackney's `h2_msg()`),
and always carries the number of records decoded in the call:

    -type ews_stream_msg() ::
            {cont,        Count, SkipLeft, Records, State}
          | {max_reached, Count, SkipLeft, Records, State}
          | {trailers,    Count, SkipLeft, Records, Trailers, State}.

* `cont` — the buffered input is exhausted; feed more data.
* `max_reached` — `Max` records were decoded; call again with an empty
  chunk to drain already-buffered data before reading more input. No
  need to compare `Count` against `Max` yourself.
* `trailers` — the stream has ended. `Trailers` is the decoded document
  *around* the streamed elements: the whole document record with the
  streamed field set to `[]`. Further calls repeat the trailers with
  `Count = 0`.
* `Count` is the number of elements consumed in the call, INCLUDING
  skipped ones — summing `Count` over all calls gives the absolute
  position in the stream, exactly the value to pass as `Skip` on a
  restart. The decoded records are `Records` (`length(Records)` is the
  decoded count). Skipped elements never appear in `Records` and do
  not count towards `Max` — a call that only skips returns
  `{cont, Count, SkipLeft, [], State}`, never `max_reached`.
* `SkipLeft` is how many of the `Skip` elements remain to be skipped
  after the call.
* `Rest` is `<<>>` (or `undefined`) on the first call; on subsequent
  calls pass the `State` from the previous message. It is an opaque
  state that carries buffered input, parser state and progress.
* `Skip` skips the first `Skip` child elements of the stream without
  decoding them, which allows restarting an interrupted stream from the
  top of the file. Suppose a run with `Max` 1000 died after 10000
  processed elements (`ews_stream:seen(State)` at the last checkpoint,
  or the number of records the caller had handled). Restart with
  `Rest = <<>>` and `Skip = 10000` and feed chunks as usual: the
  messages count `SkipLeft` down from 10000 to 0 over as many calls and
  chunks as it takes (with `Records = []` on the way, `Count` showing
  how many elements each call skipped),
  and decoding then resumes at element 10001.
* Errors come back as `{error, Reason}`, e.g.
  `{error, {not_a_list, Qname}}` when the selected field is not a
  repeated element, `{error, {not_in_model, Alias}}` for an unknown
  container, `{error, {unmatched_close_tag, Name}}` for broken tag
  nesting, or a decode error when an element does not match the schema.
  A truncated document is not detectable by the parser (more input
  might still arrive): the caller sees `cont` at end of input, as in
  the example below.

Namespace prefixes declared on ancestor elements (typically the document
root) are resolved as usual. The child element must not nest inside
itself, and a self-closing (`<Item/>`) child element is not emitted.
Decoding uses a compiled plan, so no model lookups are made per record.

Example, decoding a large file read in chunks:

    process(File) ->
        {ok, Fd} = file:open(File, [read, raw, binary, read_ahead]),
        try stream(Fd, <<>>, <<>>)
        after file:close(Fd)
        end.

    stream(Fd, Chunk, Rest) ->
        case ews:stream_decode(my_model, #items_type{}, #items_type.item,
                               Chunk, Rest, 500, 0) of
            {ok, {trailers, _Count, _SkipLeft, Items, Batch, _Rest1}} ->
                handle_items(Items),
                %% Batch is the whole document with item = []
                {ok, Batch};
            {ok, {max_reached, _Count, _SkipLeft, Items, Rest1}} ->
                handle_items(Items),
                stream(Fd, <<>>, Rest1);
            {ok, {cont, _Count, _SkipLeft, Items, Rest1}} ->
                handle_items(Items),
                case file:read(Fd, 65536) of
                    {ok, Chunk1} -> stream(Fd, Chunk1, Rest1);
                    eof -> {error, truncated_document}
                end;
            {error, Reason} ->
                {error, Reason}
        end.

The underlying incremental xml parser can also be used on its own:
`ews_xml:stream_new(TargetQname)` creates a parser state, and
`ews_xml:decode(Chunk, State) -> {XmlTerms, State1}` consumes as much as
possible of each chunk and returns the target elements completed so far
as xml terms. `ews_xml:stream_done(State)` reports whether the document's
root element has been closed.

### Environment

The ews application uses the following application environment variables:

`connect_timeout`

Connect timeout for SOAP calls in milliseconds (default: 8000).

`recv_timeout`

Receive timeout for SOAP calls in milliseconds (default: 5000).

`cache_base_dir`

Base directory under which ews stores cached xsds (default: `code:priv_dir(ews)`).

### Testing your generated code

The function `ews_test:test_everything(Model :: atom())` will serialize and
deserialize all possible ins, outs and faults of all ops the the selected model.
It will generate defaults for every entry in every record that can be reached.
A possible ct test would look like this:

    serialize_deserialize(_Config) ->
        {ok, _} = ews:add_wsdl_to_model(moose,
                                        "moose.wsdl")),
        ok = ews_test:test_everything(moose),
        ok.
