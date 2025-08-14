%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_wire_type_encoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    to_writer/1,
    encode_wire_type/2,
    encode_wire_type_store/2
]).

%% Types
-type t() :: #argo_wire_type_encoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header) -> WireTypeEncoder when Header :: argo_header:t(), WireTypeEncoder :: t().
new(Header = #argo_header{}) ->
    #argo_wire_type_encoder{message = argo_message_encoder:new(Header)}.

-spec to_writer(WireTypeEncoder) -> Writer when WireTypeEncoder :: t(), Writer :: binary().
to_writer(#argo_wire_type_encoder{message = MessageEncoder}) ->
    argo_message_encoder:to_writer(MessageEncoder).

-spec encode_wire_type(WireTypeEncoder, WireType) -> WireTypeEncoder when
    WireTypeEncoder :: t(), WireType :: argo_wire_type:t().
encode_wire_type(WireTypeEncoder1 = #argo_wire_type_encoder{}, WireType = #argo_wire_type{}) ->
    case WireType#argo_wire_type.inner of
        ScalarWireType = #argo_scalar_wire_type{} -> encode_scalar_wire_type(WireTypeEncoder1, ScalarWireType);
        BlockWireType = #argo_block_wire_type{} -> encode_block_wire_type(WireTypeEncoder1, BlockWireType);
        NullableWireType = #argo_nullable_wire_type{} -> encode_nullable_wire_type(WireTypeEncoder1, NullableWireType);
        ArrayWireType = #argo_array_wire_type{} -> encode_array_wire_type(WireTypeEncoder1, ArrayWireType);
        RecordWireType = #argo_record_wire_type{} -> encode_record_wire_type(WireTypeEncoder1, RecordWireType);
        #argo_desc_wire_type{} -> encode_desc_wire_type(WireTypeEncoder1);
        #argo_error_wire_type{} -> encode_error_wire_type(WireTypeEncoder1);
        #argo_extensions_wire_type{} -> encode_extensions_wire_type(WireTypeEncoder1);
        #argo_path_wire_type{} -> encode_path_wire_type(WireTypeEncoder1)
    end.

-spec encode_wire_type_store(WireTypeEncoder, WireTypeStore) -> WireTypeEncoder when
    WireTypeEncoder :: t(), WireTypeStore :: argo_wire_type_store:t().
encode_wire_type_store(
    WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}, WireTypeStore = #argo_wire_type_store{}
) ->
    Types = WireTypeStore#argo_wire_type_store.types,
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT),
    MessageEncoder3 = argo_message_encoder:write_core_length(MessageEncoder2, argo_index_map:size(Types)),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder3},
    WireTypeEncoder3 = argo_index_map:foldl(
        fun(
            _Index,
            TypeName,
            #argo_wire_type_store_entry{name = TypeName, type = WireType},
            WireTypeEncoderAcc1 = #argo_wire_type_encoder{message = MessageEncoderAcc1}
        ) ->
            MessageEncoderAcc2 = argo_message_encoder:encode_block_string(MessageEncoderAcc1, TypeName),
            WireTypeEncoderAcc2 = WireTypeEncoderAcc1#argo_wire_type_encoder{message = MessageEncoderAcc2},
            WireTypeEncoderAcc3 = encode_wire_type(WireTypeEncoderAcc2, WireType),
            WireTypeEncoderAcc3
        end,
        WireTypeEncoder2,
        Types
    ),
    WireTypeEncoder3.

%% @private
-spec encode_scalar_wire_type(WireTypeEncoder, ScalarWireType) -> WireTypeEncoder when
    WireTypeEncoder :: t(), ScalarWireType :: argo_scalar_wire_type:t().
encode_scalar_wire_type(
    WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}, ScalarWireType = #argo_scalar_wire_type{}
) ->
    MessageEncoder2 =
        case ScalarWireType#argo_scalar_wire_type.inner of
            string ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_STRING);
            boolean ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_BOOLEAN);
            varint ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_VARINT);
            float64 ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_FLOAT64);
            bytes ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_BYTES);
            #argo_fixed_wire_type{length = Length} ->
                ME1 = MessageEncoder1,
                ME2 = argo_message_encoder:write_core_label(ME1, ?ARGO_LABEL_WIRE_TYPE_MARKER_FIXED),
                ME3 = argo_message_encoder:write_core_length(ME2, Length),
                ME3;
            desc ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_DESC)
        end,
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder2.

%% @private
-spec encode_block_wire_type(WireTypeEncoder, BlockWireType) -> WireTypeEncoder when
    WireTypeEncoder :: t(), BlockWireType :: argo_block_wire_type:t().
encode_block_wire_type(
    WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}, BlockWireType = #argo_block_wire_type{}
) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_BLOCK),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder3 =
        #argo_wire_type_encoder{message = MessageEncoder3} = encode_scalar_wire_type(
            WireTypeEncoder2, BlockWireType#argo_block_wire_type.'of'
        ),
    MessageEncoder4 = argo_message_encoder:encode_block_string(MessageEncoder3, BlockWireType#argo_block_wire_type.key),
    MessageEncoder5 = argo_message_encoder:encode_block_boolean(
        MessageEncoder4, BlockWireType#argo_block_wire_type.dedupe
    ),
    WireTypeEncoder4 = WireTypeEncoder3#argo_wire_type_encoder{message = MessageEncoder5},
    WireTypeEncoder4.

%% @private
-spec encode_nullable_wire_type(WireTypeEncoder, NullableWireType) -> WireTypeEncoder when
    WireTypeEncoder :: t(), NullableWireType :: argo_nullable_wire_type:t().
encode_nullable_wire_type(
    WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}, NullableWireType = #argo_nullable_wire_type{}
) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_NULLABLE),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder3 = encode_wire_type(WireTypeEncoder2, NullableWireType#argo_nullable_wire_type.'of'),
    WireTypeEncoder3.

%% @private
-spec encode_array_wire_type(WireTypeEncoder, ArrayWireType) -> WireTypeEncoder when
    WireTypeEncoder :: t(), ArrayWireType :: argo_array_wire_type:t().
encode_array_wire_type(
    WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}, ArrayWireType = #argo_array_wire_type{}
) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_ARRAY),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder3 = encode_wire_type(WireTypeEncoder2, ArrayWireType#argo_array_wire_type.'of'),
    WireTypeEncoder3.

%% @private
-spec encode_record_wire_type(WireTypeEncoder, RecordWireType) -> WireTypeEncoder when
    WireTypeEncoder :: t(), RecordWireType :: argo_record_wire_type:t().
encode_record_wire_type(
    WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}, RecordWireType = #argo_record_wire_type{}
) ->
    Fields = RecordWireType#argo_record_wire_type.fields,
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_RECORD),
    MessageEncoder3 = argo_message_encoder:write_core_length(MessageEncoder2, argo_index_map:size(Fields)),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder3},
    WireTypeEncoder3 = argo_index_map:foldl(
        fun(_Index, _Name, FieldWireType, WireTypeEncoderAcc) ->
            encode_field_wire_type(WireTypeEncoderAcc, FieldWireType)
        end,
        WireTypeEncoder2,
        Fields
    ),
    WireTypeEncoder3.

%% @private
-spec encode_field_wire_type(WireTypeEncoder, FieldWireType) -> WireTypeEncoder when
    WireTypeEncoder :: t(), FieldWireType :: argo_field_wire_type:t().
encode_field_wire_type(
    WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}, FieldWireType = #argo_field_wire_type{}
) ->
    MessageEncoder2 = argo_message_encoder:encode_block_string(
        MessageEncoder1, FieldWireType#argo_field_wire_type.name
    ),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder3 =
        #argo_wire_type_encoder{message = MessageEncoder3} = encode_wire_type(
            WireTypeEncoder2, FieldWireType#argo_field_wire_type.'of'
        ),
    MessageEncoder4 = argo_message_encoder:encode_block_boolean(
        MessageEncoder3, argo_field_wire_type:is_omittable(FieldWireType)
    ),
    WireTypeEncoder4 = WireTypeEncoder3#argo_wire_type_encoder{message = MessageEncoder4},
    WireTypeEncoder4.

%% @private
-spec encode_desc_wire_type(WireTypeEncoder) -> WireTypeEncoder when WireTypeEncoder :: t().
encode_desc_wire_type(WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_DESC),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder2.

%% @private
-spec encode_error_wire_type(WireTypeEncoder) -> WireTypeEncoder when WireTypeEncoder :: t().
encode_error_wire_type(WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_ERROR),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder2.

%% @private
-spec encode_extensions_wire_type(WireTypeEncoder) -> WireTypeEncoder when WireTypeEncoder :: t().
encode_extensions_wire_type(WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_EXTENSIONS),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder2.

%% @private
-spec encode_path_wire_type(WireTypeEncoder) -> WireTypeEncoder when WireTypeEncoder :: t().
encode_path_wire_type(WireTypeEncoder1 = #argo_wire_type_encoder{message = MessageEncoder1}) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_WIRE_TYPE_MARKER_PATH),
    WireTypeEncoder2 = WireTypeEncoder1#argo_wire_type_encoder{message = MessageEncoder2},
    WireTypeEncoder2.
