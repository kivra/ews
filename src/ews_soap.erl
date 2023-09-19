-module(ews_soap).

-export([ call/6
        , call/7
        , make_fault/3
        , make_soap/2
        , make_envelope/2
        , parse_envelope/1
        ]).

-include("ews.hrl").
-include_lib("ews/include/ews.hrl").

%% ----------------------------------------------------------------------------

call(Endpoint, OpName, SoapAction, Header, Body, Opts) ->
    call(Endpoint, OpName, SoapAction, Header, Body, Opts, []).

call(Endpoint, OpName, SoapAction, Header, Body, Opts, PrePostHooks) ->
    {ok, DefaultTimeout} = application:get_env(ews, soap_timeout),
    %% FIXME: get the timeout into hackney
    _Timeout = maps:get(timeout, Opts, DefaultTimeout),
    IncludeHttpHdr = maps:get(include_http_response_headers, Opts, false),
    ExtraHeaders = maps:get(http_headers, Opts, []),
    HttpOpts = maps:get(http_options, Opts, []),
    Hdrs = [{<<"SOAPAction">>, a2b(SoapAction)},
            {<<"Content-Type">>, <<"text/xml; charset=utf-8">>}] ++ ExtraHeaders,
    BodyIoList = make_soap(Header, Body),
    HookArgs = [Endpoint, OpName, BodyIoList, HttpOpts],
    [NewEndpoint, _NewOpName, NewSoap, NewHttpOpts] =
        ews_svc:run_Hooks(PrePostHooks, HookArgs),
    case hackney:request(post, NewEndpoint, Hdrs, NewSoap, NewHttpOpts) of
        {ok, 200, HttpHdr, RespRef} ->
            {ok, RespEnv} = hackney:body(RespRef),
            XmlTerm = ews_xml:decode(RespEnv),
            Resp = parse_envelope(XmlTerm),
            fix_header(Resp, HttpHdr, IncludeHttpHdr);
        {ok, _Code, HttpHdr, FaultRef} ->
            {ok, FaultEnv} = hackney:body(FaultRef),
            XmlTerm = ews_xml:decode(FaultEnv),
            Resp = parse_envelope(XmlTerm),
            fix_header(Resp, HttpHdr, IncludeHttpHdr);
        {error, Error} ->
            {error, Error}
    end.

%% ----------------------------------------------------------------------------

a2b(B) when is_binary(B) -> B;
a2b(L) when is_list(L) -> iolist_to_binary(L);
a2b(A) when is_atom(A) -> atom_to_binary(A).

make_fault(FaultCode, FaultString, Detail) ->
    Envelope = {{?SOAPNS, "Envelope"}, [],
                [{{?SOAPNS, "Body"}, [],
                 [{{?SOAPNS, "Fault"}, [],
                   [ {"faultcode", [], [{{?SOAPNS, txt}, FaultCode}]}
                   , {"faultstring", [], [{txt, FaultString}]}
                   , {"detail", [], Detail}
                   ]}]}]},
    BodyIoList = [?XML_HDR, ews_xml:encode(Envelope)],
    iolist_to_binary(BodyIoList).

make_soap(Header, Body) ->
    {EnvTag, _, Rest} = Envelope0 = make_envelope(Header, Body),
    {Attrs, Nss} = ews_xml:get_all_nss(Envelope0),
    Envelope1 = {EnvTag, Attrs, Rest},
    [?XML_HDR, ews_xml:encode(Envelope1, Nss)].

make_envelope(undefined, Body) ->
    {{?SOAPNS, "Envelope"}, [], [make_body(Body)]};
make_envelope([], Body) ->
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
            {fault, {[], parse_fault(Faults)}};
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
fix_header(Response, _HttpHdr, false) ->
    Response;
fix_header({error, E}, _HttpHdr, _) ->
    {error, E};
fix_header({Code, {SoapHdr, SoapResp}}, HttpHdr, true) ->
    {Code, {[{http_response_headers, HttpHdr} | SoapHdr], SoapResp}}.
