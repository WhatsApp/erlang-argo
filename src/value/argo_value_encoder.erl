%%% % @format
-module(argo_value_encoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_header.hrl").
-include("argo_label.hrl").
-include("argo_message.hrl").
-include("argo_value.hrl").

%% API
-export([
    new/1,
    to_writer/1,
    encode_value/2
]).

%% Types
-type t() :: #argo_value_encoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header) -> ValueEncoder when Header :: argo_header:t(), ValueEncoder :: t().
new(Header = #argo_header{}) ->
    #argo_value_encoder{message = argo_message_encoder:new(Header)}.

-spec to_writer(ValueEncoder) -> Writer when ValueEncoder :: t(), Writer :: binary().
to_writer(#argo_value_encoder{message = MessageEncoder}) ->
    argo_message_encoder:to_writer(MessageEncoder).

-spec encode_value(ValueEncoder, Value) -> ValueEncoder when ValueEncoder :: t(), Value :: argo_value:t().
encode_value(ValueEncoder1 = #argo_value_encoder{}, Value = #argo_value{}) ->
    case Value#argo_value.inner of
        ScalarValue = #argo_scalar_value{} -> encode_scalar_value(ValueEncoder1, ScalarValue);
        BlockValue = #argo_block_value{} -> encode_block_value(ValueEncoder1, BlockValue);
        NullableValue = #argo_nullable_value{} -> encode_nullable_value(ValueEncoder1, NullableValue);
        ArrayValue = #argo_array_value{} -> encode_array_value(ValueEncoder1, ArrayValue);
        RecordValue = #argo_record_value{} -> encode_record_value(ValueEncoder1, RecordValue);
        DescValue = #argo_desc_value{} -> encode_desc_value(ValueEncoder1, DescValue);
        ErrorValue = #argo_error_value{} -> encode_error_value(ValueEncoder1, ErrorValue);
        PathValue = #argo_path_value{} -> encode_path_value(ValueEncoder1, PathValue)
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec encode_scalar_value(ValueEncoder, ScalarValue) -> ValueEncoder when
    ValueEncoder :: t(), ScalarValue :: argo_scalar_value:t().
encode_scalar_value(ValueEncoder1 = #argo_value_encoder{}, ScalarValue = #argo_scalar_value{}) ->
    ValueEncoder2 =
        #argo_value_encoder{message = MessageEncoder1} = maybe_encode_self_describing_label_for_scalar(
            ValueEncoder1, ScalarValue
        ),
    MessageEncoder2 =
        case ScalarValue#argo_scalar_value.inner of
            {string, Value} -> argo_message_encoder:encode_block_string(MessageEncoder1, Value);
            {boolean, Value} -> argo_message_encoder:encode_block_boolean(MessageEncoder1, Value);
            {varint, Value} -> argo_message_encoder:encode_block_varint(MessageEncoder1, Value);
            {float64, Value} -> argo_message_encoder:encode_block_float64(MessageEncoder1, Value);
            {bytes, Value} -> argo_message_encoder:encode_block_bytes(MessageEncoder1, Value);
            {fixed, Value} -> argo_message_encoder:encode_block_fixed(MessageEncoder1, Value)
        end,
    ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
    ValueEncoder3.

%% @private
-spec encode_block_value(ValueEncoder, BlockValue) -> ValueEncoder when
    ValueEncoder :: t(), BlockValue :: argo_block_value:t().
encode_block_value(ValueEncoder1 = #argo_value_encoder{}, BlockValue = #argo_block_value{}) ->
    ValueEncoder2 =
        #argo_value_encoder{message = MessageEncoder1} = maybe_encode_self_describing_label_for_scalar(
            ValueEncoder1, BlockValue#argo_block_value.value
        ),
    MessageEncoder2 = argo_message_encoder:encode_block_type(MessageEncoder1, BlockValue),
    ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
    ValueEncoder3.

%% @private
-spec encode_nullable_value(ValueEncoder, NullableValue) -> ValueEncoder when
    ValueEncoder :: t(), NullableValue :: argo_nullable_value:t().
encode_nullable_value(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, NullableValue = #argo_nullable_value{}
) ->
    case NullableValue#argo_nullable_value.inner of
        null ->
            MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_MARKER_NULL),
            ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder2;
        {non_null, Value = #argo_value{}} ->
            MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_MARKER_NON_NULL),
            ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder3 = encode_value(ValueEncoder2, Value),
            ValueEncoder3;
        {field_errors, _} ->
            MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_MARKER_ERROR),
            ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder2
    end.

%% @private
-spec encode_array_value(ValueEncoder, ArrayValue) -> ValueEncoder when
    ValueEncoder :: t(), ArrayValue :: argo_array_value:t().
encode_array_value(ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, ArrayValue = #argo_array_value{}) ->
    MessageEncoder2 =
        case argo_header:self_describing(MessageEncoder1#argo_message_encoder.header) of
            false ->
                MessageEncoder1;
            true ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST)
        end,
    MessageEncoder3 = argo_message_encoder:write_core_length(
        MessageEncoder2, length(ArrayValue#argo_array_value.items)
    ),
    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder3},
    ValueEncoder3 = lists:foldl(
        fun(Value = #argo_value{}, ValueEncoderAcc) ->
            encode_value(ValueEncoderAcc, Value)
        end,
        ValueEncoder2,
        ArrayValue#argo_array_value.items
    ),
    ValueEncoder3.

%% @private
-spec encode_record_value(ValueEncoder, RecordValue) -> ValueEncoder when
    ValueEncoder :: t(), RecordValue :: argo_record_value:t().
encode_record_value(ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, RecordValue = #argo_record_value{}) ->
    case argo_header:self_describing(MessageEncoder1#argo_message_encoder.header) of
        false ->
            argo_index_map:foldl(
                fun(_Index, _FieldName, FieldValue, ValueEncoderAcc) ->
                    encode_field_value(ValueEncoderAcc, FieldValue)
                end,
                ValueEncoder1,
                RecordValue#argo_record_value.fields
            );
        true ->
            MessageEncoder2 = argo_message_encoder:write_core_label(
                MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT
            ),
            FieldCount = argo_record_value:present_fields_count(RecordValue),
            MessageEncoder3 = argo_message_encoder:write_core_length(MessageEncoder2, FieldCount),
            ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder3},
            ValueEncoder3 = argo_index_map:foldl(
                fun(_Index, _FieldName, FieldValue, ValueEncoderAcc) ->
                    encode_field_value(ValueEncoderAcc, FieldValue)
                end,
                ValueEncoder2,
                RecordValue#argo_record_value.fields
            ),
            ValueEncoder3
    end.

%% @private
-spec encode_field_value(ValueEncoder, FieldValue) -> ValueEncoder when
    ValueEncoder :: t(), FieldValue :: argo_field_value:t().
encode_field_value(ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, FieldValue = #argo_field_value{}) ->
    case argo_header:self_describing(MessageEncoder1#argo_message_encoder.header) of
        false ->
            case FieldValue#argo_field_value.inner of
                {optional, none} ->
                    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_MARKER_ABSENT),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    ValueEncoder2;
                {optional, {some, Value}} ->
                    MessageEncoder2 =
                        case
                            argo_header:inline_everything(MessageEncoder1#argo_message_encoder.header) orelse
                                argo_value:is_labeled(Value)
                        of
                            false ->
                                MessageEncoder1;
                            true ->
                                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_MARKER_NON_NULL)
                        end,
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    encode_value(ValueEncoder2, Value);
                {required, Value} ->
                    encode_value(ValueEncoder1, Value)
            end;
        true ->
            case FieldValue#argo_field_value.inner of
                {optional, none} ->
                    ValueEncoder1;
                {optional, {some, Value}} ->
                    MessageEncoder2 = argo_message_encoder:encode_block_string(
                        MessageEncoder1, FieldValue#argo_field_value.name
                    ),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    encode_value(ValueEncoder2, Value);
                {required, Value} ->
                    MessageEncoder2 = argo_message_encoder:encode_block_string(
                        MessageEncoder1, FieldValue#argo_field_value.name
                    ),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    encode_value(ValueEncoder2, Value)
            end
    end.

%% @private
-spec encode_desc_value(ValueEncoder, DescValue) -> ValueEncoder when
    ValueEncoder :: t(), DescValue :: argo_desc_value:t().
encode_desc_value(ValueEncoder1 = #argo_value_encoder{}, DescValue = #argo_desc_value{}) ->
    ValueEncoder2 =
        #argo_value_encoder{message = MessageEncoder1} = encode_self_describing_label_for_desc(
            ValueEncoder1, DescValue
        ),
    case DescValue#argo_desc_value.inner of
        null ->
            ValueEncoder2;
        {boolean, _} ->
            ValueEncoder2;
        {object, Object} ->
            MessageEncoder2 = argo_message_encoder:write_core_length(MessageEncoder1, argo_index_map:size(Object)),
            ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder4 = argo_index_map:foldl(
                fun(_Index, Key, Value, ValueEncoderAcc1 = #argo_value_encoder{message = MessageEncoderAcc1}) ->
                    MessageEncoderAcc2 = argo_message_encoder:encode_block_string(MessageEncoderAcc1, Key),
                    ValueEncoderAcc2 = ValueEncoderAcc1#argo_value_encoder{message = MessageEncoderAcc2},
                    encode_desc_value(ValueEncoderAcc2, Value)
                end,
                ValueEncoder3,
                Object
            ),
            ValueEncoder4;
        {list, List} ->
            MessageEncoder2 = argo_message_encoder:write_core_length(MessageEncoder1, length(List)),
            ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder4 = argo_index_map:foldl(
                fun(Value, ValueEncoderAcc1) ->
                    encode_desc_value(ValueEncoderAcc1, Value)
                end,
                ValueEncoder3,
                List
            ),
            ValueEncoder4;
        {string, Value} ->
            MessageEncoder2 = argo_message_encoder:encode_block_string(MessageEncoder1, Value),
            ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder3;
        {bytes, Value} ->
            MessageEncoder2 = argo_message_encoder:encode_block_bytes(MessageEncoder1, Value),
            ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder3;
        {int, Value} ->
            MessageEncoder2 = argo_message_encoder:encode_block_varint(MessageEncoder1, Value),
            ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder3;
        {float, Value} ->
            MessageEncoder2 = argo_message_encoder:encode_block_float64(MessageEncoder1, Value),
            ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
            ValueEncoder3
    end.

%% @private
-spec encode_error_value(ValueEncoder, ErrorValue) -> ValueEncoder when
    ValueEncoder :: t(), ErrorValue :: argo_error_value:t().
encode_error_value(_ValueEncoder1 = #argo_value_encoder{message = _MessageEncoder1}, _ErrorValue = #argo_error_value{}) ->
    erlang:error(unsupported).

%% @private
-spec encode_path_value(ValueEncoder, PathValue) -> ValueEncoder when
    ValueEncoder :: t(), PathValue :: argo_path_value:t().
encode_path_value(_ValueEncoder1 = #argo_value_encoder{message = _MessageEncoder1}, _PathValue = #argo_path_value{}) ->
    erlang:error(unsupported).

%% @private
-spec encode_self_describing_label_for_desc(ValueEncoder, DescValue) -> ValueEncoder when
    ValueEncoder :: t(), DescValue :: argo_desc_value:t().
encode_self_describing_label_for_desc(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, DescValue = #argo_desc_value{}
) ->
    MessageEncoder2 =
        case DescValue#argo_desc_value.inner of
            null ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_NULL);
            {boolean, false} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FALSE);
            {boolean, true} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_TRUE);
            {object, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT);
            {list, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST);
            {string, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING);
            {bytes, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES);
            {int, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT);
            {float, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT)
        end,
    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
    ValueEncoder2.

%% @private
-spec maybe_encode_self_describing_label_for_scalar(ValueEncoder, ScalarValue) -> ValueEncoder when
    ValueEncoder :: t(), ScalarValue :: argo_scalar_value:t().
maybe_encode_self_describing_label_for_scalar(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, ScalarValue = #argo_scalar_value{}
) ->
    case argo_header:self_describing(MessageEncoder1#argo_message_encoder.header) of
        false ->
            ValueEncoder1;
        true ->
            encode_self_describing_label_for_scalar(ValueEncoder1, ScalarValue)
    end.

%% @private
-spec encode_self_describing_label_for_scalar(ValueEncoder, ScalarValue) -> ValueEncoder when
    ValueEncoder :: t(), ScalarValue :: argo_scalar_value:t().
encode_self_describing_label_for_scalar(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, ScalarValue = #argo_scalar_value{}
) ->
    MessageEncoder2 =
        case ScalarValue#argo_scalar_value.inner of
            {string, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING);
            {boolean, _} ->
                MessageEncoder1;
            {varint, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT);
            {float64, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT);
            {bytes, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES);
            {fixed, _} ->
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES)
        end,
    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
    ValueEncoder2.
