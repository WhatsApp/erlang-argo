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
-module(argo_block_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/2,
    key/1,
    to_block_wire_type/1
]).

%% Types
-type t() :: #argo_block_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(BlockWireType, ScalarValue) -> BlockValue when
    BlockWireType :: argo_block_wire_type:t(), ScalarValue :: argo_scalar_value:t(), BlockValue :: t().
new(BlockWireType = #argo_block_wire_type{}, Value = #argo_scalar_value{}) ->
    #argo_block_value{wire_type = BlockWireType, value = Value}.

-compile({inline, [key/1]}).
-spec key(BlockValue) -> Key when BlockValue :: t(), Key :: argo_types:name().
key(#argo_block_value{wire_type = #argo_block_wire_type{key = Key}}) ->
    Key.

-spec to_block_wire_type(BlockValue) -> BlockWireType when BlockValue :: t(), BlockWireType :: argo_block_wire_type:t().
to_block_wire_type(#argo_block_value{wire_type = BlockWireType = #argo_block_wire_type{}}) ->
    BlockWireType.
