-module(ews).

-export([start/0, stop/0]).

-export([add_wsdl_to_model/1, add_wsdl_to_model/2,
         emit_complete_model_types/1, emit_complete_model_types/2,
         emit_service_types/2, emit_service_types/3,
         emit_service_ops/1, emit_service_ops/2,
         list_services/0, list_services/1, list_model_services/0,
         get_service_ops/1, get_service_ops/2,
         get_service_op_info/2, get_service_op_info/3,
         call_service_op/4, call_service_op/5, call_service_op/6,
         add_pre_hook/1, add_pre_hook/2,
         add_post_hook/1, add_post_hook/2,
         remove_pre_hook/1, remove_pre_hook/2,
         remove_post_hook/1, remove_post_hook/2,
         remove_model/1]).

-include("ews.hrl").

start() ->
    application:ensure_all_started(ews).

stop() ->
    application:stop(ews).

add_wsdl_to_model(WsdlUrl) ->
    add_wsdl_to_model(default, WsdlUrl).
add_wsdl_to_model(Model, WsdlUrl) when is_atom(Model) ->
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
    case ews_svc:list_services() of
        {ok, ModelSvcs} ->
            {ok, [Name || {_, Name} <- ModelSvcs]};
        Error ->
            Error
    end.
list_services(Model) ->
    ews_svc:list_services(Model).

list_model_services() ->
    ews_svc:list_services().

get_service_ops(Service) ->
    ews_svc:list_service_ops(Service).
get_service_ops(Model, Service) ->
    ews_svc:list_service_ops(Model, Service).

get_service_op_info(Service, Op) ->
    ews_svc:get_op_info(Service, Op).
get_service_op_info(Model, Service, Op) ->
    ews_svc:get_op_info(Model, Service, Op).

call_service_op(Service, Op, Header, Body) ->
    ews_svc:call(Service, Op, Header, Body).
call_service_op(Service, Op, Header, Body, Opaque)  when is_list(Service) ->
    ews_svc:call(Service, Op, Header, Body, Opaque);
call_service_op(Model, Service, Op, Header, Body) when is_atom(Model) ->
    ews_svc:call(Model, Service, Op, Header, Body).
call_service_op(Model, Service, Op, Header, Body, Opaque) when is_atom(Model) ->
    ews_svc:call(Model, Service, Op, Header, Body, Opaque).

add_pre_hook(Hook) ->
    ews_svc:add_pre_hook(default, Hook).
add_pre_hook(Model, Hook) ->
    ews_svc:add_pre_hook(Model, Hook).

add_post_hook(Hook) ->
    ews_svc:add_post_hook(default, Hook).
add_post_hook(Model, Hook) ->
    ews_svc:add_post_hook(Model, Hook).

remove_pre_hook(HookRef) ->
    ews_svc:remove_pre_hook(default, HookRef).
remove_pre_hook(Model, HookRef) ->
    ews_svc:remove_pre_hook(Model, HookRef).

remove_post_hook(HookRef) ->
    ews_svc:remove_post_hook(default, HookRef).
remove_post_hook(Model, HookRef) ->
    ews_svc:remove_post_hook(Model, HookRef).

remove_model(Model) ->
    ews_svc:remove_model(Model).
