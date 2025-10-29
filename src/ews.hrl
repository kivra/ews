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
-record(binding, {name, port_type, style, transport, ops, soap_version}).
-record(binding_op, {name, action, input, output, faults}).
-record(binding_op_msg, {name, headers, body}).
-record(binding_op_fault, {name, use}).
-record(op_part, {name, message, part, use}).
-record(service, {name, ports}).
-record(port, {name, endpoint, binding, soap_version}).

%% XSD parsing
-record(schema, {namespace, url, types}).
-record(element, {name, type, default, fixed, nillable=false,
                  min_occurs=1, max_occurs=1, parts, attrs=[]}).
-record(simple_type, {name, order, restrictions}).
-record(attribute, {name, type, use, default, fixed}).
-record(complex_type, {name, extends, abstract, restrictions, parts, attrs=[]}).
-record(reference, {name}).

-record(restriction, {base_type, values}).
-record(extension, {base, parts}).
-record(sequence, {min_occurs=1, max_occurs=1, parts}).
-record(choice, {min_occurs=1, max_occurs=1, parts}).
-record(all, {min_occurs=1, max_occurs=1, parts}).
-record(enumeration, {base_type, values}).

%% Simplified XSD
-record(elem, {qname, type, meta, attrs=[]}).
-record(type, {qname, alias, elems, extends, abstract, attrs=[]}).
-record(base, {xsd_type, erl_type, restrictions, list=false, union=false}).
-record(enum, {type, values, list=false, union=false}).
-record(meta, {nillable=false, default, fixed, max, min}).

%% Macro definitions
-ifdef(DEBUG).
-define(log(Expression), Expression).
-define(log(Format, Arguments), io:format(Format, Arguments)).
-else.
-define(log(Expression), ok).
-define(log(Format, Arguments), begin
                                    _ = Format,
                                    _ = Arguments
                                end).
-endif.

-define(XML_HDR, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>).
-define(SOAPNS, "http://schemas.xmlsoap.org/soap/envelope/").
