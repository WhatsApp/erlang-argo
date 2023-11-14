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
-module(argo_label).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_label.hrl").

%% API
-export([
    self_describing_blocks_boolean/0,
    self_describing_blocks_bytes/0,
    self_describing_blocks_float64/0,
    self_describing_blocks_string/0,
    self_describing_blocks_varint/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec self_describing_blocks_boolean() -> argo_block_wire_type:t().
self_describing_blocks_boolean() ->
    Of = argo_scalar_wire_type:boolean(),
    argo_block_wire_type:new(
        Of, ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_BOOLEAN, argo_scalar_wire_type:deduplicate_by_default(Of)
    ).

-spec self_describing_blocks_bytes() -> argo_block_wire_type:t().
self_describing_blocks_bytes() ->
    Of = argo_scalar_wire_type:bytes(),
    argo_block_wire_type:new(
        Of, ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_BYTES, argo_scalar_wire_type:deduplicate_by_default(Of)
    ).

-spec self_describing_blocks_float64() -> argo_block_wire_type:t().
self_describing_blocks_float64() ->
    Of = argo_scalar_wire_type:float64(),
    argo_block_wire_type:new(
        Of, ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_FLOAT64, argo_scalar_wire_type:deduplicate_by_default(Of)
    ).

-spec self_describing_blocks_string() -> argo_block_wire_type:t().
self_describing_blocks_string() ->
    Of = argo_scalar_wire_type:string(),
    argo_block_wire_type:new(
        Of, ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_STRING, argo_scalar_wire_type:deduplicate_by_default(Of)
    ).

-spec self_describing_blocks_varint() -> argo_block_wire_type:t().
self_describing_blocks_varint() ->
    Of = argo_scalar_wire_type:varint(),
    argo_block_wire_type:new(
        Of, ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_VARINT, argo_scalar_wire_type:deduplicate_by_default(Of)
    ).
