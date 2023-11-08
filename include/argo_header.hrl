%%% % @format
-ifndef(ARGO_HEADER_HRL).
-define(ARGO_HEADER_HRL, 1).

-record(argo_header, {
    inline_everything = false :: boolean(),
    self_describing = false :: boolean(),
    out_of_band_field_errors = true :: boolean(),
    self_describing_errors = true :: boolean(),
    null_terminated_strings = false :: boolean(),
    no_deduplication = false :: boolean(),
    has_user_flags = false :: boolean(),
    user_flags = undefined :: undefined | bitstring()
}).

-endif.
