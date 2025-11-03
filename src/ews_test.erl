%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2013-2017 Campanja
%%% Copyright (c) 2017-2020 [24]7.ai
%%% Copyright (c) 2022-2025 Kivra
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
%%% @doc This module serializes and deserializes all messages and faults
%%% for all endpoints of a model. This is here in src/ and not in test/
%%% so it easily can be called from ct tests in libraries that use ews
%%% as a dep.
%%% This is how to use it in a ct test
%%%
%%% serialize_deserialize(_Config) ->
%%%    {ok, _} = ews:add_wsdl_to_model(moose,
%%%                                    "moose.wsdl")),
%%%    ok = ews_test:test_everything(moose),
%%%    ok.
%%% @end
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
    test_in(InHdr, In, Model, Service, Op, Tbl),
    test_out(OutHdr, Out, Model, Service, Op, Tbl),
    test_fault(Fault, Model, Service, Op, Tbl, TheModel),
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

test_fault(undefined, _, _, _, _, _) -> ok;
test_fault([], _, _, _, _, _) -> ok;
test_fault(Fault, Model, Service, Op, Tbl, TheModel) ->
    {ok, Info} = ews_svc:get_op_message_details(Model, Service, Op),
    logger:notice("Fault: ~tp~n", [Fault]),
    FaultVals = def_vals(Fault, Tbl),
    logger:notice("FaultVals: ~tp~n", [FaultVals]),
    FaultCode = <<"9000">>,
    FaultString = <<"boo">>,
    FaultSOAP = ews:encode_service_op_faults(Model, Service, Op,
                                             FaultCode, FaultString, FaultVals),
    XmlTerm = ews_xml:decode(FaultSOAP),
    {fault, FaultRes} = ews_soap:parse_envelope(XmlTerm),
    #fault{ code = Code
          , string = String
          , detail = Detail
          } = ews_svc:parse_fault(FaultRes, Info, TheModel),
    ?assertMatch(<<"p1:", FaultCode/binary>>, Code),
    ?assertMatch(FaultString, String),
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
