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
-module(argo_value_encoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_message.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    to_writer/1,
    encode_value/2,
    encode_value/3
]).
%% Errors API
-export([
    format_error/2
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
    #argo_value_encoder{message = argo_message_encoder:new(Header), wire_type = undefined}.

-spec to_writer(ValueEncoder) -> Writer when ValueEncoder :: t(), Writer :: binary().
to_writer(#argo_value_encoder{message = MessageEncoder}) ->
    argo_message_encoder:to_writer(MessageEncoder).

-spec encode_value(ValueEncoder, Value) -> ValueEncoder when ValueEncoder :: t(), Value :: argo_value:t().
encode_value(ValueEncoder1 = #argo_value_encoder{wire_type = undefined}, Value = #argo_value{}) ->
    case Value#argo_value.inner of
        RecordValue = #argo_record_value{} ->
            case argo_record_value:find(RecordValue, <<"data">>) of
                {ok,
                    _FieldValue = #argo_field_value{
                        wire_type = #argo_field_wire_type{'of' = DataWireType = #argo_wire_type{}}
                    }} ->
                    encode_value(ValueEncoder1, Value, {some, DataWireType});
                error ->
                    RootWireType = argo_value:to_wire_type(Value),
                    encode_value(ValueEncoder1, Value, {some, RootWireType})
            end;
        _ ->
            encode_value(ValueEncoder1, Value, none)
    end;
encode_value(ValueEncoder1 = #argo_value_encoder{}, Value = #argo_value{}) ->
    case Value#argo_value.inner of
        ScalarValue = #argo_scalar_value{} -> encode_scalar_value(ValueEncoder1, ScalarValue);
        BlockValue = #argo_block_value{} -> encode_block_value(ValueEncoder1, BlockValue);
        NullableValue = #argo_nullable_value{} -> encode_nullable_value(ValueEncoder1, NullableValue);
        ArrayValue = #argo_array_value{} -> encode_array_value(ValueEncoder1, ArrayValue);
        RecordValue = #argo_record_value{} -> encode_record_value(ValueEncoder1, RecordValue);
        DescValue = #argo_desc_value{} -> encode_desc_value(ValueEncoder1, DescValue);
        ErrorValue = #argo_error_value{} -> encode_error_value(ValueEncoder1, ErrorValue);
        ExtensionsValue = #argo_extensions_value{} -> encode_extensions_value(ValueEncoder1, ExtensionsValue);
        PathValue = #argo_path_value{} -> encode_path_value(ValueEncoder1, PathValue)
    end.

-spec encode_value(ValueEncoder, Value, OptionWireType) -> ValueEncoder when
    ValueEncoder :: t(), Value :: argo_value:t(), OptionWireType :: argo_types:option(argo_wire_type:t()).
encode_value(ValueEncoder1 = #argo_value_encoder{wire_type = undefined}, Value = #argo_value{}, OptionWireType) when
    ?is_option_record(OptionWireType, argo_wire_type)
->
    ValueEncoder2 = ValueEncoder1#argo_value_encoder{wire_type = OptionWireType},
    ValueEncoder3 = encode_value(ValueEncoder2, Value),
    ValueEncoder4 = ValueEncoder3#argo_value_encoder{wire_type = undefined},
    ValueEncoder4.

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
            {fixed, Value} -> argo_message_encoder:encode_block_fixed(MessageEncoder1, Value);
            {desc, _Value} -> MessageEncoder1
        end,
    ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
    ValueEncoder4 =
        case ScalarValue#argo_scalar_value.inner of
            {desc, DescValue} ->
                encode_desc_value(ValueEncoder3, DescValue);
            _ ->
                ValueEncoder3
        end,
    ValueEncoder4.

%% @private
-spec encode_block_value(ValueEncoder, BlockValue) -> ValueEncoder when
    ValueEncoder :: t(), BlockValue :: argo_block_value:t().
encode_block_value(ValueEncoder1 = #argo_value_encoder{}, BlockValue = #argo_block_value{}) ->
    ValueEncoder2 =
        #argo_value_encoder{message = MessageEncoder1} = maybe_encode_self_describing_label_for_scalar(
            ValueEncoder1, BlockValue#argo_block_value.value
        ),
    MessageEncoder2 =
        case argo_scalar_value:is_desc(BlockValue#argo_block_value.value) of
            false ->
                argo_message_encoder:encode_block_type(MessageEncoder1, BlockValue);
            true ->
                MessageEncoder1
        end,
    ValueEncoder3 = ValueEncoder2#argo_value_encoder{message = MessageEncoder2},
    ValueEncoder4 =
        case BlockValue#argo_block_value.value#argo_scalar_value.inner of
            {desc, DescValue} ->
                encode_desc_value(ValueEncoder3, DescValue);
            _ ->
                ValueEncoder3
        end,
    ValueEncoder4.

%% @private
-spec encode_nullable_value(ValueEncoder, NullableValue) -> ValueEncoder when
    ValueEncoder :: t(), NullableValue :: argo_nullable_value:t().
encode_nullable_value(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, NullableValue = #argo_nullable_value{}
) ->
    case argo_header:self_describing(MessageEncoder1#argo_message_encoder.header) of
        false ->
            IsLabeled = argo_nullable_value:is_labeled(NullableValue),
            case NullableValue#argo_nullable_value.inner of
                null ->
                    MessageEncoder2 = argo_message_encoder:write_core_nullable_type(MessageEncoder1, null, IsLabeled),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    ValueEncoder2;
                {non_null, Value = #argo_value{}} ->
                    MessageEncoder2 = argo_message_encoder:write_core_nullable_type(
                        MessageEncoder1, non_null, IsLabeled
                    ),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    ValueEncoder3 = encode_value(ValueEncoder2, Value),
                    ValueEncoder3;
                {field_errors, FieldErrors} when is_list(FieldErrors) ->
                    case argo_header:out_of_band_field_errors(MessageEncoder1#argo_message_encoder.header) of
                        false ->
                            FieldErrorsLength = length(FieldErrors),
                            MessageEncoder2 = argo_message_encoder:write_core_nullable_type(
                                MessageEncoder1, error, IsLabeled
                            ),
                            MessageEncoder3 = argo_message_encoder:write_core_length(
                                MessageEncoder2, FieldErrorsLength
                            ),
                            ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder3},
                            ValueEncoder3 = lists:foldl(
                                fun(ErrorValue = #argo_error_value{}, ValueEncoderAcc) ->
                                    encode_error_value(ValueEncoderAcc, ErrorValue)
                                end,
                                ValueEncoder2,
                                FieldErrors
                            ),
                            ValueEncoder3;
                        true ->
                            MessageEncoder2 = argo_message_encoder:write_core_nullable_type(
                                MessageEncoder1, error, IsLabeled
                            ),
                            ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                            ValueEncoder2
                    end
            end;
        true ->
            case NullableValue#argo_nullable_value.inner of
                null ->
                    encode_desc_value(ValueEncoder1, argo_desc_value:null());
                {non_null, Value = #argo_value{}} ->
                    encode_value(ValueEncoder1, Value);
                {field_errors, FieldErrors} when is_list(FieldErrors) ->
                    encode_desc_value(ValueEncoder1, argo_desc_value:null())
            end
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
            IsLabeled = argo_field_value:is_labeled(FieldValue),
            case FieldValue#argo_field_value.inner of
                {optional, none} ->
                    MessageEncoder2 = argo_message_encoder:write_core_omittable_type(
                        MessageEncoder1, absent, IsLabeled
                    ),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    ValueEncoder2;
                {optional, {some, Value}} ->
                    MessageEncoder2 = argo_message_encoder:write_core_omittable_type(
                        MessageEncoder1, non_null, IsLabeled
                    ),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    encode_value(ValueEncoder2, Value);
                {required, Value} ->
                    encode_value(ValueEncoder1, Value)
            end;
        true ->
            case argo_field_value:fetch(FieldValue) of
                {ok, Value} ->
                    Name = argo_field_value:name(FieldValue),
                    MessageEncoder2 = argo_message_encoder:encode_block_string(MessageEncoder1, Name),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
                    encode_value(ValueEncoder2, Value);
                error ->
                    ValueEncoder1
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
            ValueEncoder4 = lists:foldl(
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
encode_error_value(ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, ErrorValue = #argo_error_value{}) ->
    case
        argo_header:self_describing(MessageEncoder1#argo_message_encoder.header) orelse
            argo_header:self_describing_errors(MessageEncoder1#argo_message_encoder.header)
    of
        false ->
            MessageEncoder2 = argo_message_encoder:encode_block_string(
                MessageEncoder1, ErrorValue#argo_error_value.message
            ),
            MessageEncoder3 =
                case ErrorValue#argo_error_value.locations of
                    none ->
                        argo_message_encoder:write_core_omittable_type(MessageEncoder2, absent, true);
                    {some, Locations} when is_list(Locations) ->
                        ME2_1 = MessageEncoder2,
                        ME2_2 = argo_message_encoder:write_core_omittable_type(ME2_1, non_null, true),
                        ME2_3 = argo_message_encoder:write_core_length(ME2_2, length(Locations)),
                        ME2_4 = lists:foldl(
                            fun(LocationValue = #argo_location_value{}, ME2_3_Acc1) ->
                                ME2_3_Acc2 = argo_message_encoder:encode_block_varint(
                                    ME2_3_Acc1, LocationValue#argo_location_value.line
                                ),
                                ME2_3_Acc3 = argo_message_encoder:encode_block_varint(
                                    ME2_3_Acc2, LocationValue#argo_location_value.column
                                ),
                                ME2_3_Acc3
                            end,
                            ME2_3,
                            Locations
                        ),
                        ME2_4
                end,
            ValueEncoder2 =
                #argo_value_encoder{message = MessageEncoder4} =
                case ErrorValue#argo_error_value.path of
                    none ->
                        ME3_1 = MessageEncoder3,
                        ME3_2 = argo_message_encoder:write_core_omittable_type(ME3_1, absent, true),
                        VE1_1 = ValueEncoder1,
                        VE1_2 = VE1_1#argo_value_encoder{message = ME3_2},
                        VE1_2;
                    {some, Path = #argo_path_value{}} ->
                        ME3_1 = MessageEncoder3,
                        ME3_2 = argo_message_encoder:write_core_omittable_type(ME3_1, non_null, true),
                        VE1_1 = ValueEncoder1,
                        VE1_2 = VE1_1#argo_value_encoder{message = ME3_2},
                        VE1_3 = encode_path_value(VE1_2, Path),
                        VE1_3
                end,
            ValueEncoder3 =
                #argo_value_encoder{} =
                case ErrorValue#argo_error_value.extensions of
                    none ->
                        ME4_1 = MessageEncoder4,
                        ME4_2 = argo_message_encoder:write_core_omittable_type(ME4_1, absent, false),
                        VE2_1 = ValueEncoder2,
                        VE2_2 = VE2_1#argo_value_encoder{message = ME4_2},
                        VE2_2;
                    {some, Extensions = #argo_extensions_value{}} ->
                        ME4_1 = MessageEncoder4,
                        ME4_2 = argo_message_encoder:write_core_omittable_type(ME4_1, non_null, false),
                        VE2_1 = ValueEncoder2,
                        VE2_2 = VE2_1#argo_value_encoder{message = ME4_2},
                        VE2_3 = encode_extensions_value(VE2_2, Extensions),
                        VE2_3
                end,
            ValueEncoder3;
        true ->
            encode_self_describing_error_value(ValueEncoder1, ErrorValue)
    end.

%% @private
-spec encode_extensions_value(ValueEncoder, ExtensionsValue) -> ValueEncoder when
    ValueEncoder :: t(), ExtensionsValue :: argo_extensions_value:t().
encode_extensions_value(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1},
    _ExtensionsValue = #argo_extensions_value{inner = Extensions}
) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT),
    MessageEncoder3 = argo_message_encoder:write_core_length(MessageEncoder2, argo_index_map:size(Extensions)),
    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder3},
    ValueEncoder3 = argo_index_map:foldl(
        fun(_Index, Key, Value, ValueEncoderAcc1 = #argo_value_encoder{message = MessageEncoderAcc1}) ->
            MessageEncoderAcc2 = argo_message_encoder:encode_block_string(MessageEncoderAcc1, Key),
            ValueEncoderAcc2 = ValueEncoderAcc1#argo_value_encoder{message = MessageEncoderAcc2},
            encode_desc_value(ValueEncoderAcc2, Value)
        end,
        ValueEncoder2,
        Extensions
    ),
    ValueEncoder3.

%% @private
-spec encode_path_value(ValueEncoder, PathValue) -> ValueEncoder when
    ValueEncoder :: t(), PathValue :: argo_path_value:t().
encode_path_value(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1, wire_type = OptionWireType},
    PathValue = #argo_path_value{}
) ->
    case argo_header:self_describing(MessageEncoder1#argo_message_encoder.header) of
        false ->
            case OptionWireType of
                none ->
                    error_with_info(badarg, [ValueEncoder1, PathValue], #{1 => path_value_not_supported});
                {some, WireType = #argo_wire_type{}} ->
                    WirePath = argo_path_value:to_wire_path(PathValue, WireType),
                    MessageEncoder2 = argo_message_encoder:write_core_length(
                        MessageEncoder1, argo_wire_path:size(WirePath)
                    ),
                    MessageEncoder3 = argo_wire_path:foldl(
                        WirePath,
                        MessageEncoder2,
                        fun(WirePathSegment, MessageEncoderAcc1) when ?is_usize(WirePathSegment) ->
                            MessageEncoderAcc2 = argo_message_encoder:encode_block_varint(
                                MessageEncoderAcc1, WirePathSegment
                            ),
                            MessageEncoderAcc2
                        end
                    ),
                    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder3},
                    ValueEncoder2
            end;
        true ->
            encode_self_describing_path_value(ValueEncoder1, PathValue)
    end.

%% @private
-spec encode_self_describing_error_value(ValueEncoder, ErrorValue) -> ValueEncoder when
    ValueEncoder :: t(), ErrorValue :: argo_error_value:t().
encode_self_describing_error_value(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, ErrorValue = #argo_error_value{}
) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT),
    Length = argo_error_value:present_fields_count(ErrorValue),
    MessageEncoder3 = argo_message_encoder:write_core_length(MessageEncoder2, Length),
    MessageEncoder4 = argo_message_encoder:encode_block_string(MessageEncoder3, <<"message">>),
    MessageEncoder5 = argo_message_encoder:write_core_label(MessageEncoder4, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING),
    MessageEncoder6 = argo_message_encoder:encode_block_string(MessageEncoder5, ErrorValue#argo_error_value.message),
    MessageEncoder7 =
        case ErrorValue#argo_error_value.locations of
            none ->
                MessageEncoder6;
            {some, Locations} when is_list(Locations) ->
                ME6_1 = MessageEncoder6,
                ME6_2 = argo_message_encoder:encode_block_string(ME6_1, <<"locations">>),
                ME6_3 = argo_message_encoder:write_core_label(ME6_2, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST),
                ME6_4 = argo_message_encoder:write_core_length(ME6_3, length(Locations)),
                ME6_5 = lists:foldl(
                    fun(LocationValue = #argo_location_value{}, ME6_4_Acc1) ->
                        ME6_4_Acc2 = argo_message_encoder:write_core_label(
                            ME6_4_Acc1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT
                        ),
                        ME6_4_Acc3 = argo_message_encoder:write_core_length(ME6_4_Acc2, 2),
                        ME6_4_Acc4 = argo_message_encoder:encode_block_string(ME6_4_Acc3, <<"line">>),
                        ME6_4_Acc5 = argo_message_encoder:write_core_label(
                            ME6_4_Acc4, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT
                        ),
                        ME6_4_Acc6 = argo_message_encoder:encode_block_varint(
                            ME6_4_Acc5, LocationValue#argo_location_value.line
                        ),
                        ME6_4_Acc7 = argo_message_encoder:encode_block_string(ME6_4_Acc6, <<"column">>),
                        ME6_4_Acc8 = argo_message_encoder:write_core_label(
                            ME6_4_Acc7, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT
                        ),
                        ME6_4_Acc9 = argo_message_encoder:encode_block_varint(
                            ME6_4_Acc8, LocationValue#argo_location_value.column
                        ),
                        ME6_4_Acc9
                    end,
                    ME6_4,
                    Locations
                ),
                ME6_5
        end,
    ValueEncoder2 =
        #argo_value_encoder{message = MessageEncoder8} =
        case ErrorValue#argo_error_value.path of
            none ->
                ME7_1 = MessageEncoder7,
                VE1_1 = ValueEncoder1,
                VE1_2 = VE1_1#argo_value_encoder{message = ME7_1},
                VE1_2;
            {some, Path = #argo_path_value{}} ->
                ME7_1 = MessageEncoder7,
                ME7_2 = argo_message_encoder:encode_block_string(ME7_1, <<"path">>),
                VE1_1 = ValueEncoder1,
                VE1_2 = VE1_1#argo_value_encoder{message = ME7_2},
                VE1_3 = encode_self_describing_path_value(VE1_2, Path),
                VE1_3
        end,
    ValueEncoder3 =
        #argo_value_encoder{} =
        case ErrorValue#argo_error_value.extensions of
            none ->
                ME8_1 = MessageEncoder8,
                VE2_1 = ValueEncoder2,
                VE2_2 = VE2_1#argo_value_encoder{message = ME8_1},
                VE2_2;
            {some, ExtensionsValue = #argo_extensions_value{}} ->
                ME8_1 = MessageEncoder8,
                ME8_2 = argo_message_encoder:encode_block_string(ME8_1, <<"extensions">>),
                VE2_1 = ValueEncoder2,
                VE2_2 = VE2_1#argo_value_encoder{message = ME8_2},
                VE2_3 = encode_extensions_value(VE2_2, ExtensionsValue),
                VE2_3
        end,
    ValueEncoder3.

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
                argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES);
            {desc, _} ->
                MessageEncoder1
        end,
    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder2},
    ValueEncoder2.

%% @private
-spec encode_self_describing_path_value(ValueEncoder, PathValue) -> ValueEncoder when
    ValueEncoder :: t(), PathValue :: argo_path_value:t().
encode_self_describing_path_value(
    ValueEncoder1 = #argo_value_encoder{message = MessageEncoder1}, PathValue = #argo_path_value{}
) ->
    MessageEncoder2 = argo_message_encoder:write_core_label(MessageEncoder1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST),
    MessageEncoder3 = argo_message_encoder:write_core_length(MessageEncoder2, argo_path_value:size(PathValue)),
    MessageEncoder4 = argo_path_value:foldl(
        PathValue,
        MessageEncoder3,
        fun(PathSegment, MessageEncoderAcc1) ->
            case PathSegment of
                {field_name, Name} when is_binary(Name) andalso byte_size(Name) > 0 ->
                    MessageEncoderAcc2 = argo_message_encoder:write_core_label(
                        MessageEncoderAcc1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING
                    ),
                    MessageEncoderAcc3 = argo_message_encoder:encode_block_string(MessageEncoderAcc2, Name),
                    MessageEncoderAcc3;
                {list_index, Index} when ?is_usize(Index) ->
                    MessageEncoderAcc2 = argo_message_encoder:write_core_label(
                        MessageEncoderAcc1, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT
                    ),
                    MessageEncoderAcc3 = argo_message_encoder:encode_block_varint(MessageEncoderAcc2, Index),
                    MessageEncoderAcc3
            end
        end
    ),
    ValueEncoder2 = ValueEncoder1#argo_value_encoder{message = MessageEncoder4},
    ValueEncoder2.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-compile({inline, [error_with_info/3]}).
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, path_value_not_supported) ->
    "PathValue is not supported (root value must be a RecordValue, preferably with a \"data\" field)";
format_error_description(_Key, Value) ->
    Value.
