%%% ---------------------------------------------------------------------------
%%% wsdl/soap records
%%% ---------------------------------------------------------------------------

-include_lib("xmerl/include/xmerl.hrl").

%% WSDL parsing
-record(wsdl, {target_ns, services, bindings, port_types, messages, types}).
-record(message, {name, parts}).
-record(part, {name, element, type}).
-record(port_type, {name, doc, ops}).
-record(port_type_op, {name, doc, input, output, faults, mep}).
-record(binding, {name, port_type, style, transport, ops}).
-record(binding_op, {name, action, input, output, faults}).
-record(binding_op_msg, {name, headers, body}).
-record(binding_op_fault, {name, use}).
-record(op_part, {name, message, part, use}).
-record(service, {name, ports}).
-record(port, {name, endpoint, binding}).

%% XSD parsing
-record(schema, {namespace, url, types}).
-record(element, {name, type, default, fixed, nillable=false,
                  min_occurs=1, max_occurs=1, parts}).
-record(simple_type, {name, order, restrictions}).
-record(attribute, {name, type, use, default, fixed}).
-record(complex_type, {name, extends, abstract, restrictions, parts}).
-record(reference, {name}).

-record(restriction, {base_type, values}).
-record(extension, {base, parts}).
-record(sequence, {min_occurs=1, max_occurs=1, parts}).
-record(choice, {min_occurs=1, max_occurs=1, parts}).
-record(all, {min_occurs=1, max_occurs=1, parts}).
-record(enumeration, {base_type, values}).

%% Simplified XSD
-record(elem, {qname, type, meta}).
-record(type, {qname, alias, elems, extends, abstract}).
-record(base, {xsd_type, erl_type, restrictions, list=false, union=false}).
-record(enum, {type, values, list=false, union=false}).
-record(meta, {nillable=false, default, fixed, max, min}).

%% Macro definitions
-ifdef(DEBUG).
-define(log(Expression), Expression).
-define(log(Format, Arguments), io:format(Format, Arguments)).
-else.
-define(log(Expression), ok).
-define(log(Format, Arguments), ok).
-endif.

-define(XML_HDR, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>).
-define(SOAPNS, "http://schemas.xmlsoap.org/soap/envelope/").
