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
%% `name' is the qname the element has on the wire: {Namespace, Name} when it
%% is namespace-qualified, a bare Name when it is not (see element_qname/3 in
%% ews_xsd). `ns' is the targetNamespace of the schema that declared it, which
%% is where an unqualified element's *type* still lives.
-record(element, {name, ns, type, doc, default, fixed, nillable=false,
                  min_occurs=1, max_occurs=1, parts, attrs=[]}).
-record(simple_type, {name, order, restrictions, unionmembers, doc}).
-record(simple_content, {name, order, restrictions, doc, attrs=[]}).
-record(attribute, {name, base, type, use, default, fixed}).
-record(complex_type, {name, extends, abstract, restrictions, parts, doc,
                       attrs=[]}).
-record(group, {name, parts, doc}).
-record(group_ref, {ref, min_occurs, max_occurs}).
-record(reference, {name}).

-record(restriction, {base_type, values}).
-record(extension, {base, parts}).
-record(sequence, {min_occurs=1, max_occurs=1, parts}).
-record(choice, {min_occurs=1, max_occurs=1, parts}).
-record(all, {min_occurs=1, max_occurs=1, parts}).
-record(enumeration, {base_type, values}).

%% Simplified XSD
%% `doc' on both is the text of the declaration's own <annotation>, carried
%% through so ews_emit can comment the generated records with it.
-record(elem, {qname, type, meta, doc, attrs=[]}).
-record(type, {qname, alias, elems, extends, abstract, doc, attrs=[]}).
%% `doc' is what the simple type's own <annotation> said, so that a field
%% typed by it can be commented even when the element says nothing itself.
-record(base, {xsd_type, erl_type, restrictions, list=false, union=false, doc}).
-record(enum, {type, values, list=false, union=false, doc}).
-record(meta, {nillable=false, default, fixed, max, min}).
-record(sc, {qname, type, meta, attrs=[]}).

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
