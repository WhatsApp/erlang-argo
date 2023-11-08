%%% % @format
-ifndef(ARGO_INDEX_SET_HRL).
-define(ARGO_INDEX_SET_HRL, 1).

-record(argo_index_set, {
    map :: argo_index_map:t(argo_index_set:element(), [])
}).

-endif.
