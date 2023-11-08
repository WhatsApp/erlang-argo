%%% % @format
-ifndef(ARGO_VALUE_HRL).
-define(ARGO_VALUE_HRL, 1).

-record(argo_array_value, {
    items :: [argo_value:t()]
}).

-record(argo_block_value, {
    'of' :: argo_scalar_wire_type:t(),
    key :: argo_types:name(),
    dedupe :: boolean(),
    value :: argo_scalar_value:t()
}).

-record(argo_desc_value, {
    inner :: argo_desc_value:inner()
}).

-record(argo_error_value, {
    message :: argo_desc_value:desc_string(),
    location :: none | {some, [argo_location_value:t()]},
    path :: none | {some, argo_path_value:t()},
    extensions :: none | {some, argo_desc_value:desc_object()}
}).

-record(argo_field_value, {
    name :: argo_types:name(),
    inner :: argo_field_value:inner()
}).

-record(argo_json_value_decoder, {
    current_path :: argo_path_value:t(),
    field_errors :: argo_index_map:t(argo_path_value:t(), argo_error_value:t()),
    response_errors :: [argo_error_value:t()]
}).

-record(argo_json_value_encoder, {
    current_path :: argo_path_value:t(),
    field_errors :: argo_index_map:t(argo_path_value:t(), argo_error_value:t()),
    response_errors :: [argo_error_value:t()]
}).

-record(argo_location_value, {
    line :: argo_types:varint(),
    column :: argo_types:varint()
}).

-record(argo_nullable_value, {
    inner :: argo_nullable_value:inner()
}).

-record(argo_path_value, {
    segments :: array:array(argo_path_value:segment())
}).

-record(argo_record_value, {
    fields :: argo_index_map:t(argo_types:name(), argo_field_value:t())
}).

-record(argo_scalar_value, {
    inner :: argo_scalar_value:inner()
}).

-record(argo_value, {
    inner :: argo_value:inner()
}).

-record(argo_value_decoder, {
    message :: argo_message_decoder:t()
}).

-record(argo_value_encoder, {
    message :: argo_message_encoder:t()
}).

-endif.
