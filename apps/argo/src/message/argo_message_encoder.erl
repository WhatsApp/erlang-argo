%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_message_encoder).
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
-include_lib("argo/include/argo_message.hrl").
-include_lib("argo/include/argo_value.hrl").

%% API
-export([
    new/1,
    to_writer/1,
    encode_block_boolean/2,
    encode_block_bytes/2,
    encode_block_fixed/2,
    encode_block_float64/2,
    encode_block_string/2,
    encode_block_type/2,
    encode_block_varint/2,
    write_core_bytes/2,
    write_core_float64/2,
    write_core_label/2,
    write_core_labeled_type/2,
    write_core_length/2,
    write_core_nullable_type/3,
    write_core_omittable_type/3,
    write_core_string/2,
    write_core_varint/2
]).

%% Types
-type t() :: #argo_message_encoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header) -> MessageEncoder when Header :: argo_header:t(), MessageEncoder :: t().
new(Header = #argo_header{}) ->
    #argo_message_encoder{header = Header, blocks = argo_block_encoders:new(Header), core = argo_core_writer:new(<<>>)}.

-spec to_writer(MessageEncoder) -> Writer when MessageEncoder :: t(), Writer :: binary().
to_writer(#argo_message_encoder{header = Header, blocks = Blocks, core = Core}) ->
    <<
        (argo_header:to_writer(Header))/bytes,
        (argo_block_encoders:to_writer(Blocks))/bytes,
        (argo_core_writer:to_writer(Core, argo_header:inline_everything(Header)))/bytes
    >>.

-spec encode_block_boolean(MessageEncoder, Value) -> MessageEncoder when MessageEncoder :: t(), Value :: boolean().
encode_block_boolean(MessageEncoder1 = #argo_message_encoder{blocks = Blocks1, core = Core1}, Value) when
    is_boolean(Value)
->
    {Blocks2, Core2} = argo_block_encoders:encode_boolean(Blocks1, Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{blocks = Blocks2, core = Core2},
    MessageEncoder2.

-spec encode_block_bytes(MessageEncoder, Value) -> MessageEncoder when MessageEncoder :: t(), Value :: binary().
encode_block_bytes(MessageEncoder1 = #argo_message_encoder{blocks = Blocks1, core = Core1}, Value) when
    is_binary(Value)
->
    {Blocks2, Core2} = argo_block_encoders:encode_bytes(Blocks1, Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{blocks = Blocks2, core = Core2},
    MessageEncoder2.

-spec encode_block_fixed(MessageEncoder, Value) -> MessageEncoder when MessageEncoder :: t(), Value :: binary().
encode_block_fixed(MessageEncoder1 = #argo_message_encoder{blocks = Blocks1, core = Core1}, Value) when
    is_binary(Value)
->
    {Blocks2, Core2} = argo_block_encoders:encode_fixed(Blocks1, Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{blocks = Blocks2, core = Core2},
    MessageEncoder2.

-spec encode_block_float64(MessageEncoder, Value) -> MessageEncoder when MessageEncoder :: t(), Value :: float().
encode_block_float64(MessageEncoder1 = #argo_message_encoder{blocks = Blocks1, core = Core1}, Value) when
    is_float(Value)
->
    {Blocks2, Core2} = argo_block_encoders:encode_float64(Blocks1, Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{blocks = Blocks2, core = Core2},
    MessageEncoder2.

-spec encode_block_string(MessageEncoder, Value) -> MessageEncoder when
    MessageEncoder :: t(), Value :: unicode:unicode_binary().
encode_block_string(MessageEncoder1 = #argo_message_encoder{blocks = Blocks1, core = Core1}, Value) when
    is_binary(Value)
->
    {Blocks2, Core2} = argo_block_encoders:encode_string(Blocks1, Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{blocks = Blocks2, core = Core2},
    MessageEncoder2.

-spec encode_block_type(MessageEncoder, BlockValue) -> MessageEncoder when
    MessageEncoder :: t(), BlockValue :: argo_block_value:t().
encode_block_type(
    MessageEncoder1 = #argo_message_encoder{blocks = Blocks1, core = Core1}, BlockValue = #argo_block_value{}
) ->
    Key = argo_block_value:key(BlockValue),
    {Blocks2, Core2} = argo_block_encoders:encode_scalar_with_key(
        Blocks1, Core1, Key, BlockValue, BlockValue#argo_block_value.value
    ),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{blocks = Blocks2, core = Core2},
    MessageEncoder2.

-spec encode_block_varint(MessageEncoder, Value) -> MessageEncoder when
    MessageEncoder :: t(), Value :: argo_types:i64().
encode_block_varint(MessageEncoder1 = #argo_message_encoder{blocks = Blocks1, core = Core1}, Value) when
    ?is_i64(Value)
->
    {Blocks2, Core2} = argo_block_encoders:encode_varint(Blocks1, Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{blocks = Blocks2, core = Core2},
    MessageEncoder2.

-spec write_core_bytes(MessageEncoder, Value) -> MessageEncoder when MessageEncoder :: t(), Value :: binary().
write_core_bytes(MessageEncoder1 = #argo_message_encoder{core = Core1}, Value) when is_binary(Value) ->
    Core2 = argo_core_writer:write_bytes(Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_float64(MessageEncoder, Value) -> MessageEncoder when MessageEncoder :: t(), Value :: float().
write_core_float64(MessageEncoder1 = #argo_message_encoder{core = Core1}, Value) when is_float(Value) ->
    Core2 = argo_core_writer:write_float64(Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_label(MessageEncoder, Label) -> MessageEncoder when MessageEncoder :: t(), Label :: argo_types:i64().
write_core_label(MessageEncoder1 = #argo_message_encoder{core = Core1}, Label) when ?is_i64(Label) ->
    Core2 = argo_core_writer:write_label(Core1, Label),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_labeled_type(MessageEncoder, LabeledType) -> MessageEncoder when
    MessageEncoder :: t(), LabeledType :: argo_core:labeled_type().
write_core_labeled_type(MessageEncoder1 = #argo_message_encoder{core = Core1}, LabeledType) ->
    Core2 = argo_core_writer:write_labeled_type(Core1, LabeledType),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_length(MessageEncoder, Length) -> MessageEncoder when
    MessageEncoder :: t(), Length :: argo_types:length().
write_core_length(MessageEncoder1 = #argo_message_encoder{core = Core1}, Length) when ?is_usize(Length) ->
    Core2 = argo_core_writer:write_length(Core1, Length),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_nullable_type(MessageEncoder, NullableType, IsLabeled) -> MessageEncoder when
    MessageEncoder :: t(), NullableType :: argo_core:nullable_type(), IsLabeled :: boolean().
write_core_nullable_type(MessageEncoder1 = #argo_message_encoder{core = Core1}, NullableType, IsLabeled) when
    is_boolean(IsLabeled)
->
    Core2 = argo_core_writer:write_nullable_type(Core1, NullableType, IsLabeled),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_omittable_type(MessageEncoder, OmittableType, IsLabeled) -> MessageEncoder when
    MessageEncoder :: t(), OmittableType :: argo_core:omittable_type(), IsLabeled :: boolean().
write_core_omittable_type(MessageEncoder1 = #argo_message_encoder{core = Core1}, OmittableType, IsLabeled) when
    is_boolean(IsLabeled)
->
    Core2 = argo_core_writer:write_omittable_type(Core1, OmittableType, IsLabeled),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_string(MessageEncoder, Value) -> MessageEncoder when
    MessageEncoder :: t(), Value :: unicode:unicode_binary().
write_core_string(MessageEncoder1 = #argo_message_encoder{header = Header, core = Core1}, Value) when
    is_binary(Value)
->
    Core2 = argo_core_writer:write_string(Core1, Value, argo_header:null_terminated_strings(Header)),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.

-spec write_core_varint(MessageEncoder, Value) -> MessageEncoder when MessageEncoder :: t(), Value :: argo_types:i64().
write_core_varint(MessageEncoder1 = #argo_message_encoder{core = Core1}, Value) when ?is_i64(Value) ->
    Core2 = argo_core_writer:write_varint(Core1, Value),
    MessageEncoder2 = MessageEncoder1#argo_message_encoder{core = Core2},
    MessageEncoder2.
