%%  ews_svc
%%
%% >-----------------------------------------------------------------------< %%

-module(ews_svc).

-export([start_link/0]).

-export([add_wsdl/1,
         list_services/0, list_service_ops/1,
         get_op_info/2, list_types/0, get_type/1,
         list_simple_clashes/0, list_full_clashes/0,
         emit_model/1,
         call/4]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-behaviour(gen_server).

-record(state, {services=[], model}).

-include("ews.hrl").

-compile(export_all).

%% >-----------------------------------------------------------------------< %%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_wsdl(WsdlUrl) ->
    gen_server:call(?MODULE, {add_wsdl, WsdlUrl}, timer:minutes(1)).

list_services() ->
    gen_server:call(?MODULE, list_services).

list_service_ops(Service) ->
    gen_server:call(?MODULE, {list_service_ops, Service}).

get_op_info(Service, Op) ->
    gen_server:call(?MODULE, {get_op_info, Service, Op}).

get_op(Service, Op) ->
    gen_server:call(?MODULE, {get_op, Service, Op}).

get_op_message_details(Service, Op) ->
    gen_server:call(?MODULE, {get_op_message_details, Service, Op}).

list_types() ->
    gen_server:call(?MODULE, list_types).

get_type(TypeKey) ->
    gen_server:call(?MODULE, {get_type, TypeKey}).

get_model() ->
    gen_server:call(?MODULE, get_model).

list_full_clashes() ->
    gen_server:call(?MODULE, list_clashes).

list_simple_clashes() ->
    F = fun({Ns, N}, D) -> dict:append(N, Ns, D) end,
    TypeList = dict:to_list(lists:foldl(F, dict:new(), ews_svc:list_types())),
    [ Qname || Qname = {_, Nss} <- lists:usort(TypeList), length(Nss) > 1 ].

emit_model(File) ->
    gen_server:call(?MODULE, {emit_model, File}).

call(ServiceName, OpName, HeaderParts, BodyParts) ->
    Model = gen_server:call(?MODULE, get_model),
    call_service_op(ServiceName, OpName, HeaderParts, BodyParts, Model).

%% >-----------------------------------------------------------------------< %%

init([]) ->
    {ok, #state{}}.

handle_call({add_wsdl, WsdlUrl}, _, State) ->
    #state{services=OldSvcs, model=OldModel} = State,
    Wsdl = #wsdl{types=Model} = ews_wsdl:fetch_and_parse(WsdlUrl),
    Svcs = compile_wsdl(Wsdl),
    NewSvcs = lists:ukeysort(1, Svcs++OldSvcs),
    NewModel = merge_models(OldModel, Model),
    Count = [ {N, length(Ops)} || {N, Ops} <- Svcs ],
    {reply, {ok, Count}, State#state{services=NewSvcs, model=NewModel}};
handle_call(list_services, _, #state{services=Svcs} = State) ->
    {reply, {ok, [ N || {N, _} <- Svcs ]}, State};
handle_call({list_service_ops, Svc}, _, #state{services=Svcs} = State) ->
    case lists:keyfind(Svc, 1, Svcs) of
        false ->
            {reply, {error, no_service}, State};
        {Svc, Ops} ->
            Names = [ N || #op{name=N} <- Ops ],
            {reply, {ok, Names}, State}
    end;
handle_call({get_op_info, SvcName, OpName}, _, State) ->
    #state{services=Svcs, model=Model} = State,
    {reply, find_op(SvcName, OpName, Svcs, Model), State};
handle_call({get_op, SvcName, OpName}, _, #state{services=Svcs} = State) ->
    case lists:keyfind(SvcName, 1, Svcs) of
        false ->
            {reply, {error, no_service}, State};
        {SvcName, Ops} ->
            case lists:keyfind(OpName, #op.name, Ops) of
                false ->
                    {reply, {error, no_op}, State};
                Op ->
                    {reply, {ok, Op}, State}
            end
    end;
handle_call({get_op_message_details, SvcName, OpName}, _, State) ->
    #state{services=Svcs, model=Model} = State,
    case lists:keyfind(SvcName, 1, Svcs) of
        false ->
            {reply, {error, no_service}, State};
        {SvcName, Ops} ->
            case lists:keyfind(OpName, #op.name, Ops) of
                false ->
                    {reply, {error, no_op}, State};
                Op ->
                    {reply, {ok, message_info(Op, Model)}, State}
            end
    end;
handle_call(list_clashes, _, #state{model=#model{clashes=Clashes}} = State) ->
    {reply, dict:to_list(Clashes), State};
handle_call(list_types, _, #state{model=#model{type_map=Map}} = State) ->
    {reply, ews_type:keys(Map), State};
handle_call({get_type, Key}, _, #state{model=#model{type_map=Map}} = State) ->
    {reply, ews_type:get(Key, Map), State};
handle_call({emit_model, File}, _, #state{model=Model} = State) ->
    {reply, ews_emit:model_to_file(Model, File), State};
handle_call(get_model, _, #state{model=Model} = State) ->
    {reply, Model, State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_Msg, #state{} = State) ->
    {noreply, State}.

handle_info(_Msg, #state{} = State) ->
    {noreply, State}.

terminate(_Reason, #state{} = _State) ->
    ok.

code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% >-----------------------------------------------------------------------< %%

compile_wsdl(Wsdl) ->
    #wsdl{services=Services,
          messages=Messages,
          port_types=PortTypes,
          bindings=Bindings} = Wsdl,
    [ compile_ops(S, Messages, PortTypes, Bindings) || S <- Services ].

compile_ops(#service{name=Name, ports=[Port]}, Messages, PortTypes, Bindings) ->
    #port{endpoint=Endpoint, binding=Binding} = Port,
    case lists:keyfind(Binding, #binding.name, Bindings) of
        #binding{port_type=PortType,
                 style=Style,
                 ops=BindingOps,
                 transport="http://schemas.xmlsoap.org/soap/http"} ->
            case lists:keyfind(PortType, #port_type.name, PortTypes) of
                #port_type{ops=PortOps} ->
                    {Name, compile_ops(Endpoint, Style, Messages,
                                       BindingOps, PortOps)};
                false ->
                    {error, binding_lacks_port_type}
            end;
        false ->
            {error, service_lack_soap_binding}
    end.

compile_ops(EndPoint, Style, Messages, BindingOps, PortOps) ->
   [ compile_op(O, EndPoint, Style, Messages, BindingOps) || O <- PortOps ].

compile_op(PortOp, EndPoint, Style, Messages, BindingOps) ->
    #port_type_op{name=Name,
                  doc=Doc,
                  input={_InputName, InputMessageRef},
                  output={_OutputName, OutputMessageRef},
                  faults=Faults} = PortOp,
    BindingOp = lists:keyfind(Name, #binding_op.name, BindingOps),
    {InputHeaderMsg, OutputHeaderMsg} = determine_headers(BindingOp, Messages), 
    InputMsg = lists:keyfind(InputMessageRef, #message.name, Messages),
    OutputMsg = lists:keyfind(OutputMessageRef, #message.name, Messages),
    SoapAction = BindingOp#binding_op.action,
    #op{name=Name, doc=Doc,
        input={InputHeaderMsg, InputMsg},
        output={OutputHeaderMsg, OutputMsg},
        faults=[ lists:keyfind(F, #message.name, Messages) || {_,F} <- Faults ],
        style=Style, endpoint=EndPoint, action=SoapAction}.

determine_headers(#binding_op{input=Input, output=Output}, Messages) ->
    #binding_op_msg{headers=[#op_part{message=InputHdrMsg}|_]} = Input,
    #binding_op_msg{headers=[#op_part{message=OutputHdrMsg}|_]} = Output,
    {lists:keyfind(InputHdrMsg, #message.name, Messages),
     lists:keyfind(OutputHdrMsg, #message.name, Messages)}.

%% >-----------------------------------------------------------------------< %%

merge_models(undefined, Model) -> Model;
merge_models(Model, undefined) -> Model;
merge_models(#model{type_map=CurrentMap, elems=E1, clashes=CurrentClashes},
             #model{type_map=NewMap, elems=E2}) ->
    NewElems = lists:ukeysort(#elem.qname, E1++E2),
    NewClashes = merge_maps(CurrentMap, NewMap, CurrentClashes),
    #model{type_map=CurrentMap, elems=NewElems, clashes=NewClashes}.

merge_maps(CurrentMap, NewMap, ClashDict) ->
    F = fun(Key, Clashes) ->
            NewType = ews_type:get(Key, NewMap),
            case ets:insert_new(CurrentMap, {Key, NewType}) of
                true ->
                    Clashes;
                false ->
                    OldType = ews_type:get(Key, CurrentMap),
                    case NewType of
                        OldType ->
                            Clashes;
                        _ ->
                            NewClashes = dict:append(Key, NewType, Clashes),
                            dict:append(Key, OldType, NewClashes)
                    end
            end
        end,
    NewKeys = ews_type:keys(NewMap),
    lists:foldl(F, ClashDict, NewKeys).

%% >-----------------------------------------------------------------------< %%

find_op(SvcName, OpName, Svcs, Model) ->
    case lists:keyfind(SvcName, 1, Svcs) of
        false ->
            {error, no_service};
        {SvcName, Ops} ->
            case lists:keyfind(OpName, #op.name, Ops) of
                false ->
                    {error, no_op};
                Op ->
                    {ok, op_info(Op, Model)}
            end
    end.

op_info(Op, Model) ->
    #op{name=OpName,
        doc=Doc,
        input=InputMsg,
        output=OutputMsg,
        faults=FaultMsgs,
        endpoint=Endpoint,
        action=Action} = Op,
    {#message{parts=InHdrParts}, #message{parts=InParts}} = InputMsg,
    {#message{parts=OutHdrParts}, #message{parts=OutParts}} = OutputMsg,
    InHdrs = [ type_info(E, Model) || #part{element=E} <- InHdrParts ],
    OutHdrs = [ type_info(E, Model) || #part{element=E} <- OutHdrParts ],
    Ins = [ type_info(E, Model) || #part{element=E} <- InParts ],
    Outs = [ type_info(E, Model) || #part{element=E} <- OutParts ],
    Faults = [ type_info(E, Model) || #message{parts=Parts} <- FaultMsgs,
                                      #part{element=E} <- Parts ],
    [{name, OpName}, {doc, Doc},
     {in, Ins}, {in_hdr, InHdrs},
     {out, Outs}, {out_hdr, OutHdrs}, {fault, Faults},
     {endpoint, Endpoint}, {action, Action}].

type_info(ElemName, #model{elems=Elems}) ->
    case lists:keyfind(ElemName, #elem.qname, Elems) of
        false ->
            {error, not_root_elem};
        #elem{qname={_, N}, type={_,_}=TypeName} ->
            {N, TypeName};
        #elem{qname={_, N}, type=#base{}=Base} ->
            {N, Base};
        #elem{qname={_, N}, type=#enum{}=Enum} ->
            {N, Enum}
    end.

message_info(Op, Model) ->
    #op{name=OpName,
        doc=Doc,
        input=InputMsg,
        output=OutputMsg,
        faults=FaultMsgs,
        endpoint=Endpoint,
        action=Action} = Op,
    {#message{parts=InHdrParts}, #message{parts=InParts}} = InputMsg,
    {#message{parts=OutHdrParts}, #message{parts=OutParts}} = OutputMsg,
    InHdrs = [ E || #part{element=E} <- InHdrParts ],
    OutHdrs = [ E || #part{element=E} <- OutHdrParts ],
    Ins = [ E || #part{element=E} <- InParts ],
    Outs = [ E || #part{element=E} <- OutParts ],
    Faults = [ E || #message{parts=Parts} <- FaultMsgs,
                                      #part{element=E} <- Parts ],
    [{name, OpName}, {doc, Doc},
     {in, [ find_elem(I, Model) || I <- Ins ]},
     {in_hdr, [ find_elem(I, Model) || I <- InHdrs ]},
     {out, [ find_elem(O, Model) || O <- Outs ]},
     {out_hdr, [ find_elem(O, Model) || O <- OutHdrs ]},
     {faults,  [ find_elem(F, Model) || F <- Faults ]},
     {endpoint, Endpoint}, {action, Action}].

find_elem(Qname, #model{elems=Elems}) ->
    case lists:keyfind(Qname, #elem.qname, Elems) of
        false ->
            {error, not_root_elem};
        #elem{} = E ->
            E
    end.

%% >-----------------------------------------------------------------------< %%

%% TODO: Verify that # headers and body parts are same as message parts,
%%       could/should be done in the verify step
%% TODO: Serialize headers
%% TODO: Simplify ews_type module. Maybe use a named ets.
call_service_op(ServiceName, OpName, HeaderParts, BodyParts, Model) ->
    case get_op_message_details(ServiceName, OpName) of
        {error, Error} ->
            {error, Error};
        {ok, Info} ->
            InHdrs = proplists:get_value(in_hdr, Info),
            EncodedHeader = ews_serialize:encode(HeaderParts, InHdrs, Model),

            Ins = proplists:get_value(in, Info),
            Endpoint = proplists:get_value(endpoint, Info),
            Action = proplists:get_value(action, Info),
            EncodedBody = ews_serialize:encode(BodyParts, Ins, Model),

            case ews_soap:call(Endpoint, Action, EncodedHeader, EncodedBody) of
                {error, Error} ->
                    {error, Error};
                {ok, {_ResponseHeader, ResponseBody}} ->
                    Outs = proplists:get_value(out, Info),
                    ews_serialize:decode(ResponseBody, Outs, Model);
                {fault, {_FaultHeader, FaultBody}} ->
                    Faults = proplists:get_value(faults, Info),
                    ews_serialize:decode(FaultBody, hd(Faults), Model)
            end
    end.

get_type_list(TypeInfo) ->
    [ I || {_, I} <- TypeInfo ].
