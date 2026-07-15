%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiled encode/decode plan (upb-198)
%%%
%%% A `#pdoc{}` is a resolved, *human-inspectable* skeleton produced ONCE from
%%% the model (ews_serialize:compile_non_root/2). Every ews_model/ETS lookup is
%%% done at compile time and baked into these records, so traversing the tree
%%% per record (encode) or per xml element (decode) touches no ETS.
%%%
%%% The tree is direction-neutral: it describes the mapping between an erlang
%%% record shape and the xml structure, so the same #pdoc{} can drive both
%%% encoding and decoding. Leaves and fallback nodes embed the original model
%%% records (#base{}/#enum{}/#elem{}) which are themselves the neutral
%%% representation shared by the runtime encoder and decoder.
%%%
%%% Print one with, e.g.:  rr("src/ews.hrl"), rr("src/ews_plan.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Top of a compiled plan.
%%   mode      = root  -> the record is wrapped by its root element (#pelem node)
%%             | typed -> no root element in the model; wrap in `elem_qname`
%%   node      = #pelem{} (root mode) | #ptype{} (typed mode)
%%   elem_qname= synthesised wrapper qname (typed mode only)
%%   tbl       = model type_map, threaded to fallback nodes only (no ETS on the
%%               fast path)
-record(pdoc, {mode, node, elem_qname, tbl}).

%% An xml element.
%%   qname = element qname
%%   card  = single | many          (from #meta.max; `many` field value is a list)
%%   type  = #ptype{} | #pleaf{}    (the element's content)
%%   orig  = the original #elem{}, used only on the runtime-fallback path
-record(pelem, {qname, card = single, type, orig}).

%% A complex type: an erlang record.
%%   qname  = type qname
%%   tag    = record tag / alias (element 1 of the term)
%%   fields = ordered [#pelem{} | #psc{} | #pfallback{}], aligned with the
%%            record's fields (after the tag, and after '__attrs' when attrs/=[])
%%   attrs  = list of #attribute{} (possible attributes); [] when none
-record(ptype, {qname, tag, fields = [], attrs = []}).

%% simpleContent: text content mapped to a record field (no wrapping element).
%%   leaf = #pleaf{}
-record(psc, {leaf}).

%% A scalar / enum leaf.
%%   model = the original #base{} | #enum{} model record (shared by enc + dec)
-record(pleaf, {model}).

%% A node the compiler could not statically specialise (type unions/choices,
%% polymorphic subtypes/xsi:type, inline simple element types). The runtime
%% encoder/decoder handles it unchanged using the embedded model element.
%%   elem = the original #elem{}
-record(pfallback, {elem}).
