-module(ews_soap).

-export([call/4, call/5]).

-define(SOAPNS, "http://schemas.xmlsoap.org/soap/envelope/").
-define(HTTP_DEF_TIMEOUT, 60000).

-include("ews.hrl").

%% ----------------------------------------------------------------------------

call(Endpoint, SoapAction, Header, Body) ->
    {ok, Timeout} = application:get_env(ews, soap_timeout, ?HTTP_DEF_TIMEOUT),
    call(Endpoint, SoapAction, Header, Body, Timeout).

%% TODO: Handle http-headers like gzip etc.
call(Endpoint, SoapAction, Header, Body, Timeout) ->
    Hdrs = [{"SOAPAction", SoapAction},
            {"Content-Type", "text/xml"}],
    Envelope = make_envelope(Header, Body),
    BodyIoList = ews_xml:encode(Envelope),
    Log = init_log(),
    log_request(Log, BodyIoList),
    Response = case lhttpc:request(Endpoint, post, Hdrs, BodyIoList, Timeout) of
                   {ok, {{200,_}, _, RespEnv}} ->
                       log_response(Log, RespEnv),
                       XmlTerm = ews_xml:decode(RespEnv),
                       parse_envelope(XmlTerm);
                   {ok, {_Code, _, FaultEnv}} ->
                       log_response(Log, FaultEnv),
                       XmlTerm = ews_xml:decode(FaultEnv),
                       parse_envelope(XmlTerm);
                   {error, Error} ->
                       {error, Error}
               end,
    cleanup_log(Log),
    Response.

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

parse_envelope([{{?SOAPNS, "Envelope"}, _, [Header, Body]}]) ->
    {{?SOAPNS, "Header"}, _, HeaderFields} = Header,
    {{?SOAPNS, "Body"}, _, BodyFields} = Body,
    case BodyFields of
        [{{?SOAPNS, "Fault"}, _, Faults}] ->
            {fault, {HeaderFields, parse_fault(Faults)}};
        _ ->
            {ok, {HeaderFields, BodyFields}}
    end;
parse_envelope([{{?SOAPNS, "Envelope"}, _, [Body]}]) ->
    {{?SOAPNS, "Body"}, _, BodyFields} = Body,
    case BodyFields of
        [{{?SOAPNS, "Fault"}, _, Faults}] ->
            {fault, parse_fault(Faults)};
        _ ->
            {ok, {[], BodyFields}}
    end;
parse_envelope(_) ->
    {error, not_envelope}.

parse_fault(Fields) ->
    #fault{code=parse_fault_field("faultcode", Fields),
           string=parse_fault_field("faultstring", Fields),
           actor=parse_fault_field("faultactor", Fields),
           detail=parse_fault_field("detail", Fields)}.

parse_fault_field(Name, Fields) ->
    case lists:keyfind(Name, 1, Fields) of
        {_, _, [{txt, Txt}]} ->
            Txt;
        {_, _, Content} ->
            Content;
        false ->
            undefined
    end.

init_log() ->
    {ok, Dir} = application:get_env(ews, soap_log_dir),
    Filename = get_filename(),
    FilePath = filename:join([Dir, Filename]),
    filelib:ensure_dir(FilePath),
    {ok, Fd} = file:open(FilePath, [write]),
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
