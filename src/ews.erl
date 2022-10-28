-module(ews).

-export([start/0, stop/0]).

-export([add_wsdl_to_model/1, add_wsdl_to_model/2,
         emit_complete_model_types/1, emit_complete_model_types/2,
         list_services/0, list_services/1, list_model_services/0,
         get_service_ops/1, get_service_ops/2,
         get_service_op_info/2, get_service_op_info/3,
         call_service_op/4, call_service_op/5, call_service_op/6,
         encode_service_op/4, encode_service_op/5, encode_service_op/6,
         serialize_service_op/5,
         encode_service_op_faults/6,
         encode_service_op_result/5,
         decode_service_op_result/3, decode_service_op_result/4,
         decode_service_op_result/5,
         decode_in/1, decode_in/2,
         record_to_map/1, record_to_map/2,
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

add_wsdl_to_model(Wsdl) ->
    add_wsdl_to_model(default, Wsdl).

add_wsdl_to_model(Model, Wsdl) when is_atom(Model), is_binary(Wsdl) ->
    ews_svc:add_wsdl_bin(Model, Wsdl);
add_wsdl_to_model(Model, WsdlUrl) when is_atom(Model) ->
    case uri_string:parse(WsdlUrl) of
        #{scheme := _} ->
            ews_svc:add_wsdl_url(Model, WsdlUrl);
        #{} ->
            ews_svc:add_wsdl_local(Model, WsdlUrl)
    end.

emit_complete_model_types(Filename) ->
    emit_complete_model_types(default, Filename).
emit_complete_model_types(Model, Filename) ->
    ews_svc:emit_model(Model, Filename).

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

%% Call a service operation.
%% If the service only exists in one model, the model arg is not
%% necessary.
%% Opts is a map and is used to set http options as well as pass information
%% to any hooks that are defined. By default an empty map will be used.
%% Options that affect ews are:
%%  http_headers :: proplists:proplist()          (default [])
%%       Extra headers that are added to the http call
%%  timeout :: integer()                          (default 1 minute)
%%       Timeout for SOAP call in msecs
%%  include_http_response_headers :: boolean()    (default false)
%%       If true, HTTP response headers are included in the returned headers
call_service_op(Service, Op, Header, Body) ->
    ews_svc:call(Service, Op, Header, Body, default_call_opts()).

call_service_op(Service, Op, Header, Body, Opts)  when is_list(Service) ->
    ews_svc:call(Service, Op, Header, Body,
                 maps:merge(default_call_opts(), Opts));
call_service_op(Model, Service, Op, Header, Body) when is_atom(Model) ->
    ews_svc:call(Model, Service, Op, Header, Body, default_call_opts()).
call_service_op(Model, Service, Op, Header, Body, Opts) when is_atom(Model) ->
    ews_svc:call(Model, Service, Op, Header, Body,
                 maps:merge(default_call_opts(), Opts)).

encode_service_op(Service, Op, Header, Body) ->
    ews_svc:encode(Service, Op, Header, Body, #{}).

encode_service_op(Service, Op, Header, Body, Opts)  when is_list(Service) ->
    ews_svc:encode(Service, Op, Header, Body, Opts);
encode_service_op(Model, Service, Op, Header, Body) when is_atom(Model) ->
    ews_svc:encode(Model, Service, Op, Header, Body, #{}).
encode_service_op(Model, Service, Op, Header, Body, Opts) when is_atom(Model) ->
    ews_svc:encode(Model, Service, Op, Header, Body, Opts).

serialize_service_op(Model, Service, Op, Header, Body) when is_atom(Model) ->
    ews_svc:serialize(Model, Service, Op, Header, Body).

encode_service_op_result(Model, Service, Op, Header, Body) when is_atom(Model) ->
    ews_svc:encode_out(Model, Service, Op, Header, Body, #{}).

encode_service_op_faults(Model, Service, Op, FaultCode, FaultString, Body)
  when is_atom(Model) ->
    ews_svc:encode_faults(Model, Service, Op, FaultCode, FaultString, Body).

decode_service_op_result(Service, Op, Body) ->
    ews_svc:decode(Service, Op, Body, #{}).

decode_service_op_result(Service, Op, Body, Opts)
  when is_list(Service) ->
    ews_svc:decode(Service, Op, Body, Opts);
decode_service_op_result(Model, Service, Op, Body)
  when is_atom(Model) ->
    ews_svc:decode(Model, Service, Op, Body, #{}).
decode_service_op_result(Model, Service, Op, Body, Opts)
  when is_atom(Model) ->
    ews_svc:decode(Model, Service, Op, Body, Opts).

decode_in(Soap) ->
    decode_in(default, Soap).

decode_in(Model, Soap) ->
    ews_svc:decode_in(Model, Soap).

%% Convert a record representation of a term to a map.
record_to_map(R) ->
    record_to_map(default, R).
record_to_map(Model, R) ->
    ews_serialize:record_to_map(R, ews_svc:get_model(Model)).

%% Add a pre-call hook which is called just before making the actual
%% SOAP call. A pre hook is a function of one argument, which will be
%% a list [EndPoint, Operation, EncodedHeader, EncodedBody, Options] where:
%%  Endpoint:      The service endpoint that will be used for the call
%%  Operation:     Service operation
%%  EncodedHeader: Header after XML encoding
%%  EncodedBody:   Body after XML encoding
%%  Options:       Map supplied in the call_service_op call
%% It should return a list of the same kind, with potentially updated
%% values that are to be used in the call.
%% Hooks are called in the order they were added, each hook being passed
%% the output of the previous hook.
add_pre_hook(Hook) ->
    ews_svc:add_pre_hook(default, Hook).
add_pre_hook(Model, Hook) ->
    ews_svc:add_pre_hook(Model, Hook).

%% Add a post-call hook which is called after making the actual
%% SOAP call. A post hook is a function of one argument, which will be
%% a list [EncodedHeader, EncodedBody, Options] where:
%%  EncodedHeader: The header of the returned value
%%  EncodedBody:   The body of the returned value
%%  Options:       Map supplied in the call_service_op call, possibly changed
%%                 by pre-call hooks
%% It should return a list of the same kind, with potentially updated
%% values. The call_service_op will only decode and return the tranformed
%% EncodedBody, the header and opaque args are provided for the hooks only.
%% Hooks are called in the order they were added, each hook being passed
%% the output of the previous hook.
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

default_call_opts() ->
    #{include_http_response_headers => false,
      include_headers => false}.
