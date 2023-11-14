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
