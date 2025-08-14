%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_wire_type_decoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_message.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    from_reader/1,
    decode_wire_type/1,
    decode_wire_type_store/1
]).

%% Types
-type t() :: #argo_wire_type_decoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(MessageDecoder) -> WireTypeDecoder when MessageDecoder :: argo_message_decoder:t(), WireTypeDecoder :: t().
new(MessageDecoder = #argo_message_decoder{}) ->
    #argo_wire_type_decoder{message = MessageDecoder}.

-spec from_reader(Reader) -> {Reader, WireTypeDecoder} when Reader :: binary(), WireTypeDecoder :: t().
from_reader(Reader1) ->
    {Reader2, MessageDecoder} = argo_message_decoder:from_reader(Reader1),
    WireTypeDecoder = new(MessageDecoder),
    {Reader2, WireTypeDecoder}.

-spec decode_wire_type(WireTypeDecoder) -> {WireTypeDecoder, WireType} when
    WireTypeDecoder :: t(), WireType :: argo_wire_type:t().
decode_wire_type(WireTypeDecoder1 = #argo_wire_type_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    WireTypeDecoder2 = WireTypeDecoder1#argo_wire_type_decoder{message = MessageDecoder2},
    case Label of
        ?ARGO_LABEL_WIRE_TYPE_MARKER_STRING ->
            {WireTypeDecoder2, argo_wire_type:scalar(argo_scalar_wire_type:string())};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_BOOLEAN ->
            {WireTypeDecoder2, argo_wire_type:scalar(argo_scalar_wire_type:boolean())};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_VARINT ->
            {WireTypeDecoder2, argo_wire_type:scalar(argo_scalar_wire_type:varint())};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_FLOAT64 ->
            {WireTypeDecoder2, argo_wire_type:scalar(argo_scalar_wire_type:float64())};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_BYTES ->
            {WireTypeDecoder2, argo_wire_type:scalar(argo_scalar_wire_type:bytes())};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_FIXED ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            WireTypeDecoder3 = WireTypeDecoder2#argo_wire_type_decoder{message = MessageDecoder3},
            {WireTypeDecoder3, argo_wire_type:scalar(argo_scalar_wire_type:fixed(Length))};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_BLOCK ->
            {WireTypeDecoder3 = #argo_wire_type_decoder{message = MessageDecoder3}, Of} = decode_scalar_wire_type(
                WireTypeDecoder2
            ),
            {MessageDecoder4, Key} = argo_message_decoder:decode_block_string(MessageDecoder3),
            {MessageDecoder5, Dedupe} = argo_message_decoder:decode_block_boolean(MessageDecoder4),
            WireTypeDecoder4 = WireTypeDecoder3#argo_wire_type_decoder{message = MessageDecoder5},
            {WireTypeDecoder4, argo_wire_type:block(argo_block_wire_type:new(Of, Key, Dedupe))};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_NULLABLE ->
            {WireTypeDecoder3, Of} = decode_wire_type(WireTypeDecoder2),
            {WireTypeDecoder3, argo_wire_type:nullable(argo_nullable_wire_type:new(Of))};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_ARRAY ->
            {WireTypeDecoder3, Of} = decode_wire_type(WireTypeDecoder2),
            {WireTypeDecoder3, argo_wire_type:array(argo_array_wire_type:new(Of))};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_RECORD ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            WireTypeDecoder3 = WireTypeDecoder2#argo_wire_type_decoder{message = MessageDecoder3},
            {WireTypeDecoder4, RecordWireType} = decode_record_wire_type(
                WireTypeDecoder3, Length, argo_record_wire_type:new()
            ),
            {WireTypeDecoder4, argo_wire_type:record(RecordWireType)};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_DESC ->
            {WireTypeDecoder2, argo_wire_type:desc()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_ERROR ->
            {WireTypeDecoder2, argo_wire_type:error()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_EXTENSIONS ->
            {WireTypeDecoder2, argo_wire_type:extensions()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_PATH ->
            {WireTypeDecoder2, argo_wire_type:path()}
    end.

%% @private
-spec decode_record_wire_type(WireTypeDecoder, Length, RecordWireType) -> {WireTypeDecoder, RecordWireType} when
    WireTypeDecoder :: t(), Length :: argo_types:length(), RecordWireType :: argo_record_wire_type:t().
decode_record_wire_type(WireTypeDecoder1 = #argo_wire_type_decoder{}, 0, RecordWireType) ->
    {WireTypeDecoder1, RecordWireType};
decode_record_wire_type(WireTypeDecoder1 = #argo_wire_type_decoder{}, Length, RecordWireType1) when
    is_integer(Length) andalso Length > 0
->
    {WireTypeDecoder2, FieldWireType} = decode_field_wire_type(WireTypeDecoder1),
    RecordWireType2 = argo_record_wire_type:insert(RecordWireType1, FieldWireType),
    decode_record_wire_type(WireTypeDecoder2, Length - 1, RecordWireType2).

%% @private
-spec decode_field_wire_type(WireTypeDecoder) -> {WireTypeDecoder, FieldWireType} when
    WireTypeDecoder :: t(), FieldWireType :: argo_field_wire_type:t().
decode_field_wire_type(WireTypeDecoder1 = #argo_wire_type_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, Name} = argo_message_decoder:decode_block_string(MessageDecoder1),
    WireTypeDecoder2 = WireTypeDecoder1#argo_wire_type_decoder{message = MessageDecoder2},
    {WireTypeDecoder3 = #argo_wire_type_decoder{message = MessageDecoder3}, Of} = decode_wire_type(WireTypeDecoder2),
    {MessageDecoder4, Omittable} = argo_message_decoder:decode_block_boolean(MessageDecoder3),
    WireTypeDecoder4 = WireTypeDecoder3#argo_wire_type_decoder{message = MessageDecoder4},
    {WireTypeDecoder4, argo_field_wire_type:new(Name, Of, Omittable)}.

%% @private
-spec decode_scalar_wire_type(WireTypeDecoder) -> {WireTypeDecoder, ScalarWireType} when
    WireTypeDecoder :: t(), ScalarWireType :: argo_scalar_wire_type:t().
decode_scalar_wire_type(WireTypeDecoder1 = #argo_wire_type_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    WireTypeDecoder2 = WireTypeDecoder1#argo_wire_type_decoder{message = MessageDecoder2},
    case Label of
        ?ARGO_LABEL_WIRE_TYPE_MARKER_STRING ->
            {WireTypeDecoder2, argo_scalar_wire_type:string()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_BOOLEAN ->
            {WireTypeDecoder2, argo_scalar_wire_type:boolean()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_VARINT ->
            {WireTypeDecoder2, argo_scalar_wire_type:varint()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_FLOAT64 ->
            {WireTypeDecoder2, argo_scalar_wire_type:float64()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_BYTES ->
            {WireTypeDecoder2, argo_scalar_wire_type:bytes()};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_FIXED ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            WireTypeDecoder3 = WireTypeDecoder2#argo_wire_type_decoder{message = MessageDecoder3},
            {WireTypeDecoder3, argo_scalar_wire_type:fixed(Length)};
        ?ARGO_LABEL_WIRE_TYPE_MARKER_DESC ->
            {WireTypeDecoder2, argo_scalar_wire_type:desc()}
    end.

-spec decode_wire_type_store(WireTypeDecoder) -> {WireTypeDecoder, WireTypeStore} when
    WireTypeDecoder :: t(), WireTypeStore :: argo_wire_type_store:t().
decode_wire_type_store(WireTypeDecoder1 = #argo_wire_type_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    WireTypeDecoder2 = WireTypeDecoder1#argo_wire_type_decoder{message = MessageDecoder2},
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            WireTypeDecoder3 = WireTypeDecoder2#argo_wire_type_decoder{message = MessageDecoder3},
            {WireTypeDecoder4, WireTypeStore} = decode_wire_type_store_types(
                WireTypeDecoder3, Length, argo_wire_type_store:new()
            ),
            {WireTypeDecoder4, WireTypeStore}
    end.

%% @private
-spec decode_wire_type_store_types(WireTypeDecoder, Length, WireTypeStore) -> {WireTypeDecoder, WireTypeStore} when
    WireTypeDecoder :: t(), Length :: argo_types:length(), WireTypeStore :: argo_wire_type_store:t().
decode_wire_type_store_types(WireTypeDecoder1 = #argo_wire_type_decoder{}, 0, WireTypeStore) ->
    {WireTypeDecoder1, WireTypeStore};
decode_wire_type_store_types(
    WireTypeDecoder1 = #argo_wire_type_decoder{message = MessageDecoder1}, Length, WireTypeStore1
) when is_integer(Length) andalso Length > 0 ->
    {MessageDecoder2, TypeName} = argo_message_decoder:decode_block_string(MessageDecoder1),
    WireTypeDecoder2 = WireTypeDecoder1#argo_wire_type_decoder{message = MessageDecoder2},
    {WireTypeDecoder3, WireType} = decode_wire_type(WireTypeDecoder2),
    WireTypeStoreEntry = argo_wire_type_store_entry:new(TypeName, WireType),
    WireTypeStore2 = argo_wire_type_store:insert(WireTypeStore1, WireTypeStoreEntry),
    decode_wire_type_store_types(WireTypeDecoder3, Length - 1, WireTypeStore2).
