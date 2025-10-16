-module(ews_test).

-include_lib("stdlib/include/assert.hrl").

-include_lib("ews.hrl").
-include_lib("ews/include/ews.hrl").

-export([ test_everything/1
        ]).

test_everything(Model) ->
    #model{type_map=Tbl} = TheModel =
        ews_svc:get_model(Model),
    {ok, Services} = ews:list_services(Model),
    test_services(Services, Model, Tbl, TheModel).

test_services([Service | T], Model, Tbl, TheModel) ->
    {ok, Ops} = ews:get_service_ops(Model, Service),
    test_ops(Ops, Service, Model, Tbl, TheModel),
    test_services(T, Model, Tbl, TheModel);
test_services([], _, _, _) ->
    ok.

test_ops([Op | T], Service, Model, Tbl, TheModel) ->
    {ok, Info} = ews:get_service_op_info(Model, Service, Op),
    #{in_hdr := InHdr, in := In, out_hdr := OutHdr, out := Out,
      fault := Fault} = maps:from_list(Info),
    {ok, FaultInfo} = ews_svc:get_op_message_details(Model, Service, Op),
    test_in(InHdr, In, Model, Service, Op, Tbl),
    test_out(OutHdr, Out, Model, Service, Op, Tbl),
    test_fault(Fault, FaultInfo, Model, Service, Op, Tbl, TheModel),
    test_ops(T, Service, Model, Tbl, TheModel);
test_ops([], _, _, _, _) ->
    ok.

test_in(InHdr, In, Model, Service, Op, Tbl) ->
    InHdrVals = def_vals(InHdr, Tbl),
    InVals = def_vals(In, Tbl),
    logger:notice("InHdrVals: ~tp~n", [InHdrVals]),
    logger:notice("InVals: ~tp~n", [InVals]),
    InSOAP = iolist_to_binary(ews:serialize_service_op(Model, Service, Op,
                                                       InHdrVals, InVals)),
    logger:notice("InSOAP: ~tp~n", [InSOAP]),
    {ok, {Svc, OpName, InHdrs, InRes}} = ews:decode_in(Model, InSOAP),
    ?assertMatch(Service, Svc),
    ?assertMatch(Op, OpName),
    ?assertMatch(InHdrVals, InHdrs),
    cond_assert(InVals, InRes),
    ok.

test_out(OutHdr, Out, Model, Service, Op, Tbl) ->
    OutHdrVals = def_vals(OutHdr, Tbl),
    OutVals = def_vals(Out, Tbl),
    logger:notice("HdrVals: ~tp~n", [OutHdrVals]),
    logger:notice("OutVals: ~tp~n", [OutVals]),
    OutSOAP = iolist_to_binary(
                ews:encode_service_op_result(Model, Service, Op,
                                             OutHdrVals, OutVals)),
    XmlTerm = ews_xml:decode(OutSOAP),
    {ok, {HdrResp, Resp}} = ews_soap:parse_envelope(XmlTerm),
    {ok, HdrOut, OpOut} = ews:decode_service_op_result(Model, Service, Op,
                                                       HdrResp, Resp),
    ?assertMatch(OutHdrVals, HdrOut),
    cond_assert(OutVals, OpOut),
    ok.

test_fault(undefined, _, _, _, _, _, _) -> ok;
test_fault([], _, _, _, _, _, _) -> ok;
test_fault(Fault, Info, Model, Service, Op, Tbl, TheModel) ->
    logger:notice("Fault: ~tp~n", [Fault]),
    FaultVals = def_vals(Fault, Tbl),
    logger:notice("FaultVals: ~tp~n", [FaultVals]),
    FaultSOAP = ews:encode_service_op_faults(Model, Service, Op,
                                             <<"9000">>, <<"boo">>, FaultVals),
    XmlTerm = ews_xml:decode(FaultSOAP),
    {fault, FaultRes} = ews_soap:parse_envelope(XmlTerm),
    #fault{ code = Code
          , string = FaultString
          , detail = Detail
          } = ews_svc:parse_fault(FaultRes, Info, TheModel),
    ?assertMatch(<<"p1:9000">>, Code),
    ?assertMatch(<<"boo">>, FaultString),
    ?assertMatch(FaultVals, Detail),
    ok.

def_vals([{_,_} = Type], Tbl) ->
    [def_type(Type, Tbl)];
def_vals([], _) ->
    [].

def_type({_, Qname}, Tbl) ->
   #type{ alias = Alias
        , elems = Elems
        , attrs = Attrs
        } =  ews_model:get(Qname, Tbl),
    case Attrs of
        [] ->
            list_to_tuple([Alias | def_elems(Elems, Tbl)]);
        [_|_] ->
            list_to_tuple([Alias, def_attrs(Attrs, #{}) |
                           def_elems(Elems, Tbl)])
    end.

def_elems([#elem{type = #base{erl_type = ET},
                 meta = #meta{max = Max}} | T], Tbl) when Max > 1 ->
    [[def_erl(ET)] | def_elems(T, Tbl)];
def_elems([#elem{type = #base{erl_type = ET}} | T], Tbl) ->
    [def_erl(ET) | def_elems(T, Tbl)];
def_elems([#elem{type = #enum{type = _Base, values = [{Key,_Value}|_],
                              list = false, union = false},
                 meta = #meta{max = Max}} | T], Tbl) when Max > 1 ->
    [[Key] | def_elems(T, Tbl)];
def_elems([#elem{type = #enum{type = _Base, values = [{Key,_Value}|_],
                              list = false, union = false}} | T], Tbl) ->
    [Key | def_elems(T, Tbl)];
def_elems([#elem{type = {_,_} = Qname,
                 meta = #meta{max = Max}} | T], Tbl) when Max > 1 ->
    [[def_type({no_name, Qname}, Tbl)] | def_elems(T, Tbl)];
def_elems([#elem{type = {_,_} = Qname} | T], Tbl) ->
    [def_type({no_name, Qname}, Tbl) | def_elems(T, Tbl)];
def_elems([], _) ->
    [].

def_attrs([#attribute{ name = {_, Name}, type = Type}| T], Acc) ->
    Key = list_to_atom(Name),
    Value = def_erl(erl_type(Type)),
    def_attrs(T, Acc#{Key => Value});
def_attrs([], Acc) ->
    Acc.

def_erl(string) -> <<"ö"/utf8>>;
def_erl(integer) -> 17;
def_erl(float) -> 1.0;
def_erl(boolean) -> true.

erl_type({_,_} = T) ->
    #base{erl_type = ET} = ews_xsd:to_base(T),
    ET;
erl_type(T) ->
    #base{erl_type = ET} = ews_xsd:to_base({"no_ns", T}),
    ET.

%% TODO: ews should decode empty record, but it seems it doesn't
cond_assert([{_}], _) ->
    ok;
cond_assert(In, Res) ->
    ?assertMatch(In, Res).
