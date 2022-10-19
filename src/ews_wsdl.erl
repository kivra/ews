%%% ---------------------------------------------------------------------------
%%% WSDL Parse flow
%%%
%%% Service->Port->Binding->portType->Message->Types
%%%
%%% Broken:
%%% ---------------------------------------------------------------------------

-module(ews_wsdl).

-export([ parse/2
        , parse/3
        , parse_local/2
        , fetch/1
        , fetch_and_parse/2
        ]).

-include("ews.hrl").

-define(HTTP_OPTS, [ {connect_options,
                      [ {connect_timeout, timer:seconds(400)}
                      , {recv_timeout, timer:seconds(400)}
                      ]}
                   , with_body
                   ]).

%% ----------------------------------------------------------------------------

fetch(WsdlUrl) ->
    case hackney:request(get, WsdlUrl, [], [], ?HTTP_OPTS) of
        {ok, 200, _, Bin} ->
            {ok, Bin};
        {ok, _, _, Error} ->
            {error, Error};
        {error, Error} ->
            {error, Error}
    end.

parse(WsdlBin, Model) when is_atom(Model) ->
    {WsdlDoc, _} = xmerl_scan:string(binary_to_list(WsdlBin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),
    TargetNs = wh:get_attribute(WsdlDoc, targetNamespace),
    Messages = parse_messages(WsdlDoc, TargetNs),
    Bindings = parse_bindings(WsdlDoc, TargetNs),
    PortTypes = parse_port_types(WsdlDoc, TargetNs),
    Services = parse_services(WsdlDoc),
    Types = parse_types(WsdlDoc, Model),
    #wsdl{target_ns=TargetNs,
          services=Services,
          bindings=Bindings,
          port_types=PortTypes,
          messages=Messages,
          types=Types}.

parse(WsdlBin, Model, BaseUrl) when is_atom(Model) ->
    {WsdlDoc, _} = xmerl_scan:string(binary_to_list(WsdlBin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),
    TargetNs = wh:get_attribute(WsdlDoc, targetNamespace),
    Messages = parse_messages(WsdlDoc, TargetNs),
    Bindings = parse_bindings(WsdlDoc, TargetNs),
    PortTypes = parse_port_types(WsdlDoc, TargetNs),
    Services = parse_services(WsdlDoc),
    Types = parse_types(WsdlDoc, Model, BaseUrl),
    #wsdl{target_ns=TargetNs,
          services=Services,
          bindings=Bindings,
          port_types=PortTypes,
          messages=Messages,
          types=Types}.

%% For now this is how to test.
%%
%% add this branch to kivra_core
%% download the .jar from mm and unpack it to mm_api
%% ews_wsdl:parse_local("mm_api/META-INF/wsdl/public/Recipient.wsdl", mm_rpc).
%%
parse_local(WsdlPath, Model) when is_atom(Model) ->
    {ok, WsdlBin} = file:read_file(WsdlPath),
    WsdlBasePath = filename:dirname(WsdlPath),
    {WsdlDoc, _} = xmerl_scan:string(binary_to_list(WsdlBin),
                                     [{space, normalize},
                                      {namespace_conformant, true},
                                      {validation, schema}]),
    TargetNs = wh:get_attribute(WsdlDoc, targetNamespace),
    Messages = parse_messages(WsdlDoc, TargetNs),
    Bindings = parse_bindings(WsdlDoc, TargetNs),
    PortTypes = parse_port_types(WsdlDoc, TargetNs),
    Services = parse_services(WsdlDoc),
    Types = parse_types(WsdlDoc, Model, WsdlBasePath),
    #wsdl{target_ns=TargetNs,
          services=Services,
          bindings=Bindings,
          port_types=PortTypes,
          messages=Messages,
          types=Types}.

fetch_and_parse(WsdlUrl, Model) when is_atom(Model) ->
    BaseUrl = filename:dirname(WsdlUrl),
    {ok, WsdlBin} = fetch(WsdlUrl),
    parse(WsdlBin, Model, BaseUrl).

%% ---------------------------------------------------------------------------

parse_services(Doc) ->
    [ parse_service(S) || S <- wh:get_children(Doc, "service") ].

parse_service(Service) ->
    Name = wh:get_attribute(Service, name),
    Ports = [ parse_port(P) || P <- wh:get_children(Service, "port") ],
    #service{name=Name, ports=Ports}.

parse_port(Elem) ->
    Name = wh:get_attribute(Elem, name),
    Binding = wh:get_attribute(Elem, binding),
    Address = wh:get_attribute(wh:get_child(Elem, "address"), location),
    #port{name=Name, endpoint=Address, binding=Binding}.

parse_bindings(Doc, TargetNs) ->
    [ parse_binding(B, TargetNs) || B <- wh:get_children(Doc, "binding") ].

parse_binding(Binding, TargetNs) ->
   Name = wh:get_attribute(Binding, name),
   PortType = wh:get_attribute(Binding, type),
   Style = wh:get_attribute(wh:get_child(Binding, "binding"), style),
   Transport = wh:get_attribute(wh:get_child(Binding, "binding"), transport),
   Operations = wh:get_children(Binding, "operation"),
   #binding{name={TargetNs, Name},
            port_type=PortType,
            style=Style,
            transport=Transport,
            ops=[ parse_binding_op(O) || O <- Operations ]}.

parse_binding_op(Op) ->
    Name = wh:get_attribute(Op, name),
    Input = wh:get_child(Op, "input"),
    Output = wh:get_child(Op, "output"),
    Faults = wh:get_children(Op, "fault"),
    SoapAction = wh:get_attribute(wh:get_child(Op, "operation"), soapAction),
    #binding_op{name=Name,
                action=SoapAction,
                input=parse_in_out_put(Input),
                output=parse_in_out_put(Output),
                faults=[ parse_fault(F) || F <- Faults ]}.

parse_in_out_put(undefined) -> undefined;
parse_in_out_put(InOutPut) ->
    Name = wh:get_attribute(InOutPut, name),
    Headers = wh:get_children(InOutPut, "header"),
    Body = wh:get_child(InOutPut, "body"),
    BodyUse = wh:get_attribute(Body, use),
    BodyMessage = wh:get_attribute(Body, message),
    #binding_op_msg{name=Name,
                    headers=[ parse_header(H) || H <- Headers ],
                    body=#op_part{message=BodyMessage,
                                  use=BodyUse}}.

parse_header(Header) ->
    HeaderUse = wh:get_attribute(Header, use),
    HeaderMessage = wh:get_attribute(Header, message),
    HeaderPart = wh:get_attribute(Header, part),
    #op_part{part=HeaderPart,
             message=HeaderMessage,
             use=HeaderUse}.

parse_fault(Fault) ->
    SoapFault = wh:get_child(Fault, "fault"),
    FaultUse = wh:get_attribute(SoapFault, use),
    FaultName = wh:get_attribute(SoapFault, name),
    #binding_op_fault{name=FaultName, use=FaultUse}.

parse_messages(Doc, TargetNs) ->
    [ parse_message(M, TargetNs) || M <- wh:get_children(Doc, "message") ].

parse_message(Message, TargetNs) ->
    Name = wh:get_attribute(Message, name),
    Parts = [ parse_message_part(P) || P <- wh:get_children(Message, "part") ],
    #message{name={TargetNs, Name}, parts=Parts}.

parse_message_part(Part) ->
    Name = wh:get_attribute(Part, name),
    Element = wh:get_attribute(Part, element),
    Type = wh:get_attribute(Part, type),
    #part{name=Name, element=Element, type=Type}.

parse_port_types(Doc, TargetNs) ->
    [ parse_port_type(T, TargetNs) || T <- wh:get_children(Doc, "portType") ].

parse_port_type(Type, TargetNs) ->
    Name = wh:get_attribute(Type, name),
    Docs = wh:get_docs(Type),
    Ops = wh:get_children(Type, "operation"),
    #port_type{name={TargetNs, Name},
               doc=Docs,
               ops=parse_port_type_ops(Ops)}.

parse_port_type_ops(Ops) ->
    [ parse_port_type_op(O) || O <- Ops ].

parse_port_type_op(Op) ->
    Docs = wh:get_docs(Op),
    Mep = determine_message_exchange_pattern(Op),
    InputName = wh:get_attribute(wh:get_child(Op, "input"), name),
    InputMsg = wh:get_attribute(wh:get_child(Op, "input"), message),
    OutputName = wh:get_attribute(wh:get_child(Op, "output"), name),
    OutputMsg = wh:get_attribute(wh:get_child(Op, "output"), message),
    Faults = wh:get_children(Op, "fault"),
    #port_type_op{name=wh:get_attribute(Op, name),
                  doc=Docs,
                  mep=Mep,
                  input={InputName, InputMsg}, %% Called tParam
                  output={OutputName, OutputMsg},
                  faults=[ {wh:get_attribute(F, name),
                            wh:get_attribute(F, message)} || F <- Faults]}.

determine_message_exchange_pattern(#xmlElement{content=Children}) ->
    ChildrenNames = [ wh:get_simple_name(E) ||
                      E = #xmlElement{} <- Children,
                      wh:get_simple_name(E) == input orelse
                      wh:get_simple_name(E) == output orelse
                      wh:get_simple_name(E) == fault ],
    case ChildrenNames of
        [input, output | _] -> request_response;
        [output, input] -> solicit_response;
        [output | _] -> notification;
        [input | _] -> one_way;
        OpChildren -> {unknown, OpChildren}
    end.

parse_types(WsdlDoc, Model) ->
    Types = wh:get_child(WsdlDoc, "types"),
    Schemas = wh:get_children(Types, "schema"),
    {Res, _} =
        lists:foldl(fun ews_xsd:parse_schema/2, {undefined, Model}, Schemas),
    Res.

parse_types(WsdlDoc, Model, BaseDir) ->
    Types = wh:get_child(WsdlDoc, "types"),
    Schemas = wh:get_children(Types, "schema"),
    {Res, _} =
        lists:foldl(fun ews_xsd:parse_schema/2, {undefined, Model, BaseDir},
                    Schemas),
    Res.

%% >-----------------------------------------------------------------------< %%
