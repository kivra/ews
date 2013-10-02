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
    Log = log_request(BodyIoList),
    case lhttpc:request(Endpoint, post, Hdrs, BodyIoList, Timeout) of
        {ok, {{200,_}, _, RespEnv}} ->
            log_response(Log, RespEnv),
            XmlTerm = ews_xml:to_term(RespEnv),
            {ok, parse_envelope(XmlTerm)};
            %% {ok, {empty, RespEnv}};
        {ok, {_Code, _, FaultEnv}} ->
            log_response(Log, FaultEnv),
            XmlTerm = ews_xml:to_term(FaultEnv),
            {fault, parse_envelope(XmlTerm)};
        {error, Error} ->
            {error, Error}
    end.

%% ----------------------------------------------------------------------------

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
parse_envelope(_) ->
    {error, not_envelope}.

log_request(Body) ->
    Dir = "/tmp/soap/",
    filelib:ensure_dir(Dir),
    {ok, Fd} = file:open(filename:join([Dir, "rand.soap"]), [write]),
    file:write(Fd, ["request:\n", Body, 10, 10]),
    Fd.

log_response(Fd, Body) ->
    file:write(Fd, ["\nresponse:\n", Body]),
    file:close(Fd).
