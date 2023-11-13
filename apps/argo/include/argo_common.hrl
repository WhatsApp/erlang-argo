%%% % @format
-ifndef(ARGO_COMMON_HRL).
-define(ARGO_COMMON_HRL, 1).

-define(is_i64(X), (is_integer((X)) andalso (X) >= -16#8000000000000000 andalso (X) =< 16#7FFFFFFFFFFFFFFF)).
-define(is_u32(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FFFFFFFF)).
-define(is_u64(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FFFFFFFFFFFFFFFF)).
-define(is_usize(X), ?is_u64(X)).

-endif.
