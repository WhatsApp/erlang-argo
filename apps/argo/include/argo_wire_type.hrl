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
-ifndef(ARGO_WIRE_TYPE_HRL).
-define(ARGO_WIRE_TYPE_HRL, 1).

-record(argo_array_wire_type, {
    'of' :: argo_wire_type:t()
}).

-record(argo_block_wire_type, {
    'of' :: argo_scalar_wire_type:t(),
    key :: argo_types:name(),
    dedupe :: boolean()
}).

-record(argo_desc_wire_type, {}).

-record(argo_error_wire_type, {}).

-record(argo_field_wire_type, {
    name :: argo_types:name(),
    type :: argo_wire_type:t(),
    omittable :: boolean()
}).

-record(argo_fixed_wire_type, {
    length :: argo_types:usize()
}).

-record(argo_json_wire_type_decoder, {}).

-record(argo_json_wire_type_encoder, {
    strict :: boolean()
}).

-record(argo_nullable_wire_type, {
    'of' :: argo_wire_type:t()
}).

-record(argo_path_wire_type, {}).

-record(argo_record_wire_type, {
    fields :: argo_index_map:t(argo_types:name(), argo_field_wire_type:t())
}).

-record(argo_scalar_wire_type, {
    inner :: argo_scalar_wire_type:inner()
}).

-record(argo_wire_path, {
    segments :: array:array(argo_wire_path:segment())
}).

-record(argo_wire_type, {
    inner :: argo_wire_type:inner()
}).

-record(argo_wire_type_decoder, {
    message :: argo_message_decoder:t()
}).

-record(argo_wire_type_encoder, {
    message :: argo_message_encoder:t()
}).

-record(argo_wire_type_store, {
    types :: argo_index_map:t(argo_types:name(), argo_wire_type:t())
}).

-endif.
