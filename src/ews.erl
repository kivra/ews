-module(ews).

-export([start/0, stop/0]).

-export([add_wsdl_to_model/1, add_wsdl_to_model/2,
         emit_complete_model_types/1, emit_complete_model_types/2,
         emit_service_types/2, emit_service_types/3,
         emit_service_ops/1, emit_service_ops/2,
         list_services/0, list_services/1,
         get_service_ops/1, get_service_ops/2,
         get_service_op_info/2, get_service_op_info/3,
         call_service_op/4, call_service_op/5]).

-include("ews.hrl").

start() ->
    application:ensure_all_started(ews).

stop() ->
    application:stop(ews).

add_wsdl_to_model(WsdlUrl) ->
    add_wsdl_to_model(default, WsdlUrl).
add_wsdl_to_model(Model, WsdlUrl) ->
    ews_svc:add_wsdl_url(Model, WsdlUrl).

emit_complete_model_types(Filename) ->
    emit_complete_model_types(default, Filename).
emit_complete_model_types(Model, Filename) ->
    ews_svc:emit_model(Model, Filename).

emit_service_types(_, _) ->
    {error, not_implemented}.
emit_service_types(_, _, _) ->
    {error, not_implemented}.

emit_service_ops(_) ->
    {error, not_implemented}.
emit_service_ops(_, _) ->
    {error, not_implemented}.

list_services() ->
    list_services(default).
list_services(Model) ->
    ews_svc:list_services(Model).

get_service_ops(Service) ->
    get_service_ops(default, Service).
get_service_ops(Model, Service) ->
    ews_svc:list_service_ops(Model, Service).

get_service_op_info(Service, Op) ->
    get_service_op_info(default, Service, Op).
get_service_op_info(Model, Service, Op) ->
    ews_svc:get_op_info(Model, Service, Op).

call_service_op(Service, Op, Header, Body) ->
    call_service_op(default, Service, Op, Header, Body).
call_service_op(Model, Service, Op, Header, Body) ->
    ews_svc:call(Model, Service, Op, Header, Body).
