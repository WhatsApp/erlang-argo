%%% % @format
-ifndef(ARGO_INDEX_MAP_HRL).
-define(ARGO_INDEX_MAP_HRL, 1).

-record(argo_index_map, {
    indices :: #{argo_index_map:key() => argo_index_map:index()},
    entries :: array:array({argo_index_map:key(), argo_index_map:value()})
}).

-endif.
