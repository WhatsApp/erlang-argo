%%% % @format
-ifndef(ARGO_MESSAGE_HRL).
-define(ARGO_MESSAGE_HRL, 1).

-record(argo_message_decoder, {
    header :: argo_header:t(),
    blocks :: argo_block_decoders:t(),
    core :: argo_core_reader:t()
}).

-record(argo_message_encoder, {
    header :: argo_header:t(),
    blocks :: argo_block_encoders:t(),
    core :: argo_core_writer:t()
}).

-endif.
