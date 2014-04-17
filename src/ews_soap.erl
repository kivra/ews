-module(ews_soap).

-export([call/4, call/5]).

-define(SOAPNS, "http://schemas.xmlsoap.org/soap/envelope/").
-define(HTTP_TIMEOUT, 10000).

%% ----------------------------------------------------------------------------

call(Endpoint, SoapAction, Header, Body) ->
    call(Endpoint, SoapAction, Header, Body, ?HTTP_TIMEOUT).

%% TODO: Handle http-headers like gzip etc.
call(Endpoint, SoapAction, Header, Body, Timeout) ->
    Hdrs = [{"SOAPAction", SoapAction},
            {"Content-Type", "text/xml"}],
    Envelope = make_envelope(Header, Body),
    BodyIoList = ews_xml:from_term(Envelope),
    Log = init_log(),
    log_request(Log, BodyIoList),
    Response = case lhttpc:request(Endpoint, post, Hdrs, BodyIoList, Timeout) of
                   {ok, {{200,_}, _, RespEnv}} ->
                       parse_response(ok, Log, RespEnv);
                   {ok, {_Code, _, FaultEnv}} ->
                       parse_response(fault, Log, FaultEnv);
                   {error, Error} ->
                       {error, Error}
               end,
    cleanup_log(Log),
    Response.

%% ----------------------------------------------------------------------------

parse_response(Type, Log, Response) ->
    log_response(Log, Response),
    XmlTerm = ews_xml:to_term(Response),
    case parse_envelope(XmlTerm) of
        {error, Error} ->
            {error, Error};
        ParsedResponse ->
            {Type, ParsedResponse}
    end.

make_envelope(undefined, Body) ->
    {{?SOAPNS, "Envelope"}, [], [make_body(Body)]};
make_envelope(Header, Body) ->
    {{?SOAPNS, "Envelope"}, [], [make_header(Header), make_body(Body)]}.

make_body(Content) when is_list(Content) ->
    {{?SOAPNS, "Body"}, [], Content};
make_body(Content) ->
    {{?SOAPNS, "Body"}, [], [Content]}.

make_header(Content) when is_list(Content) ->
    {{?SOAPNS, "Header"}, [], Content};
make_header(Content) ->
    {{?SOAPNS, "Header"}, [], [Content]}.

parse_envelope([{{?SOAPNS, "Envelope"}, _, [Headers, Body]}]) ->
    {{?SOAPNS, "Header"}, _, HeaderFields} = Headers,
    {{?SOAPNS, "Body"}, _, BodyFields} = Body,
    {HeaderFields, BodyFields};
parse_envelope([{{?SOAPNS, "Envelope"}, _, [Body]}]) ->
    {{?SOAPNS, "Body"}, _, BodyFields} = Body,
    {undefined, BodyFields};
parse_envelope(_) ->
    {error, not_envelope}.

init_log() ->
    {ok, Dir} = application:get_env(ews, soap_log_dir),
    filelib:ensure_dir(Dir),
    Filename = get_filename(),
    {ok, Fd} = file:open(filename:join([Dir, Filename]), [write]),
    Fd.

get_filename() ->
    %%TODO filename should include request id and timestamp
    "rand.soap".

log_request(Fd, Body) ->
    file:write(Fd, ["request:\n", Body, 10, 10]).

log_response(Fd, Body) ->
    file:write(Fd, ["\nresponse:\n", Body]).

cleanup_log(Fd) ->
    file:close(Fd).
