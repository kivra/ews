# ews
Erlang Library for interacting with SOAP Web Services.

## Introduction

ews is a library for interacting with SOAP web services. It includes functionality to

* generate a model from the WSDLs that describe the web services
* generate a model from plain XSD for file encoding and decoding
* emit an Erlang .hrl file defining records corresponding to the types in the model
* call web service operations with automatic encoding of operands and decoding of the response
* supply hooks that are applied immediately before or after the actual SOAP calls

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

### Environment

The ews application uses the following application environment variables:

`soap_timeout`

Timeout for SOAP calls in milliseconds (default: 6000).

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
