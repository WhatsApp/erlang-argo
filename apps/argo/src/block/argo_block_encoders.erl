%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_block_encoders).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_block.hrl").
-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_core.hrl").
-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    to_writer/1,
    encode_boolean/3,
    encode_bytes/3,
    encode_fixed/3,
    encode_float64/3,
    encode_scalar_with_key/5,
    encode_string/3,
    encode_varint/3
]).

%% Types
-type maybe_block_wire_type() ::
    argo_block_value:t() | argo_block_wire_type:t() | fun(() -> argo_block_value:t() | argo_block_wire_type:t()).
-type t() :: #argo_block_encoders{}.

-export_type([
    maybe_block_wire_type/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header) -> BlockEncoders when Header :: argo_header:t(), BlockEncoders :: t().
new(Header = #argo_header{}) ->
    #argo_block_encoders{header = Header, inner = argo_index_map:new()}.

-spec to_writer(BlockEncoders) -> Writer when BlockEncoders :: t(), Writer :: binary().
to_writer(#argo_block_encoders{inner = Inner}) ->
    argo_index_map:foldl(
        fun(_Index, _Key, BlockEncoder, Acc) ->
            <<Acc/bytes, (argo_block_encoder:to_writer(BlockEncoder))/bytes>>
        end,
        <<>>,
        Inner
    ).

-spec encode_boolean(BlockEncoders, CoreWriter, Value) -> {BlockEncoders, CoreWriter} when
    BlockEncoders :: t(), CoreWriter :: argo_core_writer:t(), Value :: boolean().
encode_boolean(BlockEncoders1 = #argo_block_encoders{inner = Inner0}, CoreWriter1 = #argo_core_writer{}, Value) when
    is_boolean(Value)
->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_BOOLEAN,
    BlockEncoder1 = get_or_else(BlockEncoders1, Key, fun argo_label:self_describing_blocks_boolean/0),
    {BlockEncoder2, CoreWriter2} = argo_block_encoder:encode_boolean(BlockEncoder1, CoreWriter1, Value),
    Inner1 = argo_index_map:put(Key, BlockEncoder2, Inner0),
    BlockEncoders2 = BlockEncoders1#argo_block_encoders{inner = Inner1},
    {BlockEncoders2, CoreWriter2}.

-spec encode_bytes(BlockEncoders, CoreWriter, Value) -> {BlockEncoders, CoreWriter} when
    BlockEncoders :: t(), CoreWriter :: argo_core_writer:t(), Value :: binary().
encode_bytes(BlockEncoders1 = #argo_block_encoders{inner = Inner0}, CoreWriter1 = #argo_core_writer{}, Value) when
    is_binary(Value)
->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_BYTES,
    BlockEncoder1 = get_or_else(BlockEncoders1, Key, fun argo_label:self_describing_blocks_bytes/0),
    {BlockEncoder2, CoreWriter2} = argo_block_encoder:encode_bytes(BlockEncoder1, CoreWriter1, Value),
    Inner1 = argo_index_map:put(Key, BlockEncoder2, Inner0),
    BlockEncoders2 = BlockEncoders1#argo_block_encoders{inner = Inner1},
    {BlockEncoders2, CoreWriter2}.

-spec encode_fixed(BlockEncoders, CoreWriter, Value) -> {BlockEncoders, CoreWriter} when
    BlockEncoders :: t(), CoreWriter :: argo_core_writer:t(), Value :: binary().
encode_fixed(BlockEncoders1 = #argo_block_encoders{}, CoreWriter1 = #argo_core_writer{}, Value) when is_binary(Value) ->
    CoreWriter2 = argo_core_writer:write_bytes(CoreWriter1, Value),
    {BlockEncoders1, CoreWriter2}.

-spec encode_float64(BlockEncoders, CoreWriter, Value) -> {BlockEncoders, CoreWriter} when
    BlockEncoders :: t(), CoreWriter :: argo_core_writer:t(), Value :: float().
encode_float64(BlockEncoders1 = #argo_block_encoders{inner = Inner0}, CoreWriter1 = #argo_core_writer{}, Value) when
    is_float(Value)
->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_FLOAT64,
    BlockEncoder1 = get_or_else(BlockEncoders1, Key, fun argo_label:self_describing_blocks_float64/0),
    {BlockEncoder2, CoreWriter2} = argo_block_encoder:encode_float64(BlockEncoder1, CoreWriter1, Value),
    Inner1 = argo_index_map:put(Key, BlockEncoder2, Inner0),
    BlockEncoders2 = BlockEncoders1#argo_block_encoders{inner = Inner1},
    {BlockEncoders2, CoreWriter2}.

-spec encode_scalar_with_key(BlockEncoders, CoreWriter, Key, MaybeBlockWireType, ScalarValue) ->
    {BlockEncoders, CoreWriter}
when
    BlockEncoders :: t(),
    CoreWriter :: argo_core_writer:t(),
    Key :: argo_types:name(),
    MaybeBlockWireType :: maybe_block_wire_type(),
    ScalarValue :: argo_scalar_value:t().
encode_scalar_with_key(
    BlockEncoders1 = #argo_block_encoders{inner = Inner0},
    CoreWriter1 = #argo_core_writer{},
    Key,
    MaybeBlockWireType,
    ScalarValue = #argo_scalar_value{}
) when is_binary(Key) ->
    BlockEncoder1 = get_or_else(BlockEncoders1, Key, MaybeBlockWireType),
    {BlockEncoder2, CoreWriter2} = argo_block_encoder:encode_scalar(BlockEncoder1, CoreWriter1, ScalarValue),
    Inner1 = argo_index_map:put(Key, BlockEncoder2, Inner0),
    BlockEncoders2 = BlockEncoders1#argo_block_encoders{inner = Inner1},
    {BlockEncoders2, CoreWriter2}.

-spec encode_string(BlockEncoders, CoreWriter, Value) -> {BlockEncoders, CoreWriter} when
    BlockEncoders :: t(), CoreWriter :: argo_core_writer:t(), Value :: unicode:unicode_binary().
encode_string(BlockEncoders1 = #argo_block_encoders{inner = Inner0}, CoreWriter1 = #argo_core_writer{}, Value) when
    is_binary(Value)
->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_STRING,
    BlockEncoder1 = get_or_else(BlockEncoders1, Key, fun argo_label:self_describing_blocks_string/0),
    {BlockEncoder2, CoreWriter2} = argo_block_encoder:encode_string(BlockEncoder1, CoreWriter1, Value),
    Inner1 = argo_index_map:put(Key, BlockEncoder2, Inner0),
    BlockEncoders2 = BlockEncoders1#argo_block_encoders{inner = Inner1},
    {BlockEncoders2, CoreWriter2}.

-spec encode_varint(BlockEncoders, CoreWriter, Value) -> {BlockEncoders, CoreWriter} when
    BlockEncoders :: t(), CoreWriter :: argo_core_writer:t(), Value :: argo_types:i64().
encode_varint(BlockEncoders1 = #argo_block_encoders{inner = Inner0}, CoreWriter1 = #argo_core_writer{}, Value) when
    ?is_i64(Value)
->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_VARINT,
    BlockEncoder1 = get_or_else(BlockEncoders1, Key, fun argo_label:self_describing_blocks_varint/0),
    {BlockEncoder2, CoreWriter2} = argo_block_encoder:encode_varint(BlockEncoder1, CoreWriter1, Value),
    Inner1 = argo_index_map:put(Key, BlockEncoder2, Inner0),
    BlockEncoders2 = BlockEncoders1#argo_block_encoders{inner = Inner1},
    {BlockEncoders2, CoreWriter2}.

%% @private
-spec get_or_else(BlockEncoders, Key, MaybeBlockWireType) -> BlockEncoder when
    BlockEncoders :: t(),
    Key :: argo_types:name(),
    MaybeBlockWireType :: maybe_block_wire_type(),
    BlockEncoder :: argo_block_encoder:t().
get_or_else(#argo_block_encoders{header = Header, inner = Inner0}, Key, MaybeBlockWireType) ->
    case argo_index_map:find(Key, Inner0) of
        {ok, BlockEncoder1 = #argo_block_encoder{}} ->
            BlockEncoder1;
        error ->
            BlockWireType = resolve_block_wire_type(MaybeBlockWireType),
            BlockEncoder1 = argo_block_encoder:new(Header, BlockWireType),
            BlockEncoder1
    end.

%% @private
-spec resolve_block_wire_type(MaybeBlockWireType) -> BlockWireType when
    MaybeBlockWireType :: maybe_block_wire_type(), BlockWireType :: argo_block_wire_type:t().
resolve_block_wire_type(#argo_block_value{wire_type = BlockWireType = #argo_block_wire_type{}}) ->
    BlockWireType;
resolve_block_wire_type(BlockWireType = #argo_block_wire_type{}) ->
    BlockWireType;
resolve_block_wire_type(Function) when is_function(Function, 0) ->
    case Function() of
        BlockValue = #argo_block_value{} ->
            resolve_block_wire_type(BlockValue);
        BlockWireType = #argo_block_wire_type{} ->
            BlockWireType
    end.
