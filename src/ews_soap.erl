%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2013-2017 Campanja
%%% Copyright (c) 2017-2020 [24]7.ai
%%% Copyright (c) 2022-2023 Kivra
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
-module(ews_soap).

-export([ call/6
        , call/7
        , call/8
        , make_fault/3
        , make_soap/2
        , make_xml/1
        , make_envelope/2
        , parse_envelope/1
        ]).

-include("ews.hrl").
-include_lib("ews/include/ews.hrl").

%% Dialyzer analyses against whichever hackney is in *this* build -- 4.x, whose
%% spec says request/5 answers with a binary body -- and so declares the hackney
%% 1.x path dead twice over: body/1's reference clause, and call/8's branch for
%% the {error, _} that hackney:body/1 can answer with. Both are reachable under
%% 1.x, which consumers do pin (see the comment on body/1). A -spec on body/1
%% does not help, since dialyzer prefers its own success typing for a local call,
%% so silence the two functions. This file carried the same suppression, for the
%% same reason, before the paths were factored out.
-dialyzer({nowarn_function, [body/1, call/8]}).

%% ----------------------------------------------------------------------------

call(Endpoint, OpName, SoapAction, Header, Body, Opts) ->
    call(Endpoint, OpName, SoapAction, Header, Body, Opts, []).

call(Endpoint, OpName, SoapAction, Header, Body, Opts, PrePostHooks) ->
    call(Endpoint, OpName, SoapAction, Header, Body, Opts, PrePostHooks, ews).

call(Endpoint, OpName, SoapAction, Header, Body, Opts, PrePostHooks,
     ModelRef) ->
    IncludeHttpHdr = maps:get(include_http_response_headers, Opts, false),
    ExtraHeaders = maps:get(http_headers, Opts, []),
    HttpOpts0 = maps:get(http_options, Opts, []),
    HttpOpts1 = add_pool(HttpOpts0, ModelRef),
    HttpOpts2 = add_timeouts(HttpOpts1),
    HttpOpts = add_with_body(HttpOpts2),
    Hdrs = [{<<"SOAPAction">>, a2b(SoapAction)},
            {<<"Content-Type">>, <<"text/xml; charset=utf-8">>}] ++ ExtraHeaders,
    BodyIoList = make_soap(Header, Body),
    HookArgs = [Endpoint, OpName, BodyIoList, HttpOpts],
    [NewEndpoint, _NewOpName, NewSoap, NewHttpOpts] =
        ews_svc:run_hooks(PrePostHooks, HookArgs),
    case hackney:request(post, NewEndpoint, Hdrs, NewSoap, NewHttpOpts) of
        {ok, _Code, HttpHdr, BodyOrRef} ->
            case body(BodyOrRef) of
                {ok, Env} ->
                    Resp = parse_envelope(ews_xml:decode(Env)),
                    fix_header(Resp, HttpHdr, IncludeHttpHdr);
                {error, _} = Error ->
                    Error
            end;
        {error, Error} ->
            {error, Error}
    end.

%% With `with_body' asked for above, both hackney generations answer with the body
%% and this is the only clause that runs. It stays because the option can be
%% taken away again: http_options come from the caller, and a pre_post hook is
%% free to rebuild the list (ek_spar's does). Losing the option under hackney 1.x
%% would otherwise hand a client reference to the XML decoder, which is a worse
%% error than the one this fixes -- so accept either shape and be done with it.
%%
%% Both generations are in use among the applications depending on ews: ek_mm
%% pins hackney 1.25, kivra_core 1.17, sparer 4.7. A level-0 pin in any of them
%% decides which hackney ews runs against, whatever this application's own
%% constraint says.
%%
%% The status code plays no part: a fault arrives as a SOAP envelope like any
%% other answer, parse_envelope/1 recognises it, and a body that is not an
%% envelope at all comes back as {error, {not_envelope, _}}. Deciding by status
%% instead is what left the non-200 path calling hackney:body/1 on a binary.
%% The spec is the contract across both hackney generations, whatever dialyzer
%% infers from the one in this build: under 1.x the reference clause runs and
%% hackney:body/1 may answer {error, _}.
-spec body(binary() | string() | term()) -> {ok, binary() | string()} | {error, term()}.
body(Body) when is_binary(Body); is_list(Body) ->
    {ok, Body};
body(Ref) ->
    hackney:body(Ref).

%% Make both hackney generations answer the same way. 1.x hands back a client
%% reference unless asked for the body up front; 4.x always hands back the body
%% and treats this option as a deprecated no-op (hackney.erl: "The `with_body'
%% option is deprecated and ignored"). Asking for it means one response shape
%% whichever version a consumer has pinned.
add_with_body(Options) ->
    case proplists:is_defined(with_body, Options) of
        true  -> Options;
        false -> Options ++ [with_body]
    end.

add_pool(Options, ModelRef) ->
    Options ++ [{pool, ModelRef},
                {max_connections, 100}].

add_timeouts(Options0) ->
    Options1 =
        case application:get_env(ews, connect_timeout) of
            undefined ->
                Options0;
            {ok, ConnTimeout} when is_integer(ConnTimeout) ->
                Options0 ++ [{connect_timeout, ConnTimeout}]
        end,
    Options =
        case application:get_env(ews, recv_timeout) of
            undefined ->
                Options1;
            {ok, RecvTimeout} when is_integer(RecvTimeout) ->
                Options1 ++ [{recv_timeout, RecvTimeout}]
        end,
    Options.

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

make_xml([{Tag, _, Rest}] = Body0) ->
    {Attrs, Nss} = ews_xml:get_all_nss(Body0),
    %% logger:notice("Attrs:  ~tp~nNss: ~tp~n", [Attrs, Nss]),
    Body1 = {Tag, Attrs, Rest},
    BodyIoList = [?XML_HDR, ews_xml:encode(Body1, Nss)],
    iolist_to_binary(BodyIoList).

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
parse_envelope(NotEnvelope) ->
    {error, {not_envelope, NotEnvelope}}.

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
