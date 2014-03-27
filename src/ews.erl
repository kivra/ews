-module(ews).

-export([start/0, stop/0]).

-export([add_wsdl_to_model/1,
         emit_complete_model_types/1, emit_service_types/2, emit_service_ops/1,
         list_services/0, get_service_ops/1, get_service_op_info/2,
         call_service_op/4]).

-include("ews.hrl").

start() ->
    Deps = [crypto, public_key, ssl, inets, lhttpc],
    [ application:start(A) || A <- Deps ],
    application:start(ews).

stop() ->
    application:stop(ews),
    application:stop(lhttpc).

add_wsdl_to_model(WsdlUrl) ->
    ews_svc:add_wsdl(WsdlUrl).

emit_complete_model_types(Filename) ->
    ews_svc:emit_model(Filename).

emit_service_types(_, _) -> ok.

emit_service_ops(_) -> ok.

list_services() ->
    ews_svc:list_services().

get_service_ops(Service) ->
    ews_svc:list_service_ops(Service).

get_service_op_info(Service, Op) ->
    ews_svc:get_op_info(Service, Op).

call_service_op(Service, Op, Header, Body) ->
    ews_svc:call(Service, Op, Header, Body).
