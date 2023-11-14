%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%% @oncall whatsapp_clr
-ifndef(ARGO_BLOCK_HRL).
-define(ARGO_BLOCK_HRL, 1).

-record(argo_block_decoder, {
    block_wire_type :: argo_block_wire_type:t(),
    kind :: argo_block_decoder:kind()
}).

-record(argo_block_decoders, {
    header :: argo_header:t(),
    core_popped = false :: boolean(),
    blocks = queue:new() :: queue:queue(binary()),
    inner :: argo_index_map:t(argo_types:name(), argo_block_decoder:t())
}).

-record(argo_block_encoder, {
    block_wire_type :: argo_block_wire_type:t(),
    kind :: argo_block_encoder:kind()
}).

-record(argo_block_encoders, {
    header :: argo_header:t(),
    inner :: argo_index_map:t(argo_types:name(), argo_block_encoder:t())
}).

-record(argo_block_reader, {
    block :: binary()
}).

-record(argo_block_writer, {
    block :: binary()
}).

-endif.
