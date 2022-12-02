%% Service related records
-record(op, {name, doc, input, output, faults, style, endpoint, action}).
-record(model, {type_map, elems, clashes=dict:new(),
                pre_hooks=[], post_hooks=[], simple_types=[]}).
-record(fault, {code, string, actor, detail}).
