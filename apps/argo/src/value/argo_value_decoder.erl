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
-module(argo_value_decoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_message.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    from_reader/1,
    decode_wire_type/2
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_value_decoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(MessageDecoder) -> ValueDecoder when MessageDecoder :: argo_message_decoder:t(), ValueDecoder :: t().
new(MessageDecoder = #argo_message_decoder{}) ->
    #argo_value_decoder{message = MessageDecoder}.

-spec from_reader(Reader) -> {Reader, ValueDecoder} when Reader :: binary(), ValueDecoder :: t().
from_reader(Reader1) ->
    {Reader2, MessageDecoder} = argo_message_decoder:from_reader(Reader1),
    ValueDecoder = new(MessageDecoder),
    {Reader2, ValueDecoder}.

-spec decode_wire_type(ValueDecoder, WireType) -> {ValueDecoder, Value} when
    ValueDecoder :: t(), WireType :: argo_wire_type:t(), Value :: argo_value:t().
decode_wire_type(ValueDecoder1 = #argo_value_decoder{}, WireType = #argo_wire_type{}) ->
    case WireType#argo_wire_type.inner of
        ScalarWireType = #argo_scalar_wire_type{} ->
            {ValueDecoder2, ScalarValue} = decode_scalar_wire_type(ValueDecoder1, ScalarWireType),
            Value = argo_value:scalar(ScalarValue),
            {ValueDecoder2, Value};
        BlockWireType = #argo_block_wire_type{} ->
            {ValueDecoder2, BlockValue} = decode_block_wire_type(ValueDecoder1, BlockWireType),
            Value = argo_value:block(BlockValue),
            {ValueDecoder2, Value};
        NullableWireType = #argo_nullable_wire_type{} ->
            {ValueDecoder2, NullableValue} = decode_nullable_wire_type(ValueDecoder1, NullableWireType),
            Value = argo_value:nullable(NullableValue),
            {ValueDecoder2, Value};
        ArrayWireType = #argo_array_wire_type{} ->
            {ValueDecoder2, ArrayValue} = decode_array_wire_type(ValueDecoder1, ArrayWireType),
            Value = argo_value:array(ArrayValue),
            {ValueDecoder2, Value};
        RecordWireType = #argo_record_wire_type{} ->
            {ValueDecoder2, RecordValue} = decode_record_wire_type(ValueDecoder1, RecordWireType),
            Value = argo_value:record(RecordValue),
            {ValueDecoder2, Value};
        #argo_desc_wire_type{} ->
            {ValueDecoder2, DescValue} = decode_desc_wire_type(ValueDecoder1),
            Value = argo_value:desc(DescValue),
            {ValueDecoder2, Value};
        #argo_error_wire_type{} ->
            {ValueDecoder2, ErrorValue} = decode_error_wire_type(ValueDecoder1),
            Value = argo_value:error(ErrorValue),
            {ValueDecoder2, Value};
        #argo_path_wire_type{} ->
            {ValueDecoder2, PathValue} = decode_path_wire_type(ValueDecoder1),
            Value = argo_value:path(PathValue),
            {ValueDecoder2, Value}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_scalar_wire_type(ValueDecoder, ScalarWireType) -> {ValueDecoder, ScalarValue} when
    ValueDecoder :: t(), ScalarWireType :: argo_scalar_wire_type:t(), ScalarValue :: argo_scalar_value:t().
decode_scalar_wire_type(ValueDecoder1 = #argo_value_decoder{}, ScalarWireType = #argo_scalar_wire_type{}) ->
    ValueDecoder2 =
        #argo_value_decoder{message = MessageDecoder1} = maybe_decode_self_describing_label_for_scalar(
            ValueDecoder1, ScalarWireType
        ),
    case ScalarWireType#argo_scalar_wire_type.inner of
        string ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_string(MessageDecoder1),
            ScalarValue = argo_scalar_value:string(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        boolean ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_boolean(MessageDecoder1),
            ScalarValue = argo_scalar_value:boolean(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        varint ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_varint(MessageDecoder1),
            ScalarValue = argo_scalar_value:varint(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        float64 ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_float64(MessageDecoder1),
            ScalarValue = argo_scalar_value:float64(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        bytes ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_bytes(MessageDecoder1),
            ScalarValue = argo_scalar_value:bytes(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        #argo_fixed_wire_type{length = Length} ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_fixed(MessageDecoder1, Length),
            ScalarValue = argo_scalar_value:fixed(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue}
    end.

%% @private
-spec decode_block_wire_type(ValueDecoder, BlockWireType) -> {ValueDecoder, BlockValue} when
    ValueDecoder :: t(), BlockWireType :: argo_block_wire_type:t(), BlockValue :: argo_block_value:t().
decode_block_wire_type(ValueDecoder1 = #argo_value_decoder{}, BlockWireType = #argo_block_wire_type{}) ->
    ValueDecoder2 =
        #argo_value_decoder{message = MessageDecoder1} = maybe_decode_self_describing_label_for_scalar(
            ValueDecoder1, BlockWireType#argo_block_wire_type.'of'
        ),
    {MessageDecoder2, ScalarValue} = argo_message_decoder:decode_block_scalar(MessageDecoder1, BlockWireType),
    BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
    ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
    {ValueDecoder3, BlockValue}.

%% @private
-spec decode_nullable_wire_type(ValueDecoder, NullableWireType) -> {ValueDecoder, NullableValue} when
    ValueDecoder :: t(), NullableWireType :: argo_nullable_wire_type:t(), NullableValue :: argo_nullable_value:t().
decode_nullable_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, NullableWireType = #argo_nullable_wire_type{}
) ->
    IsLabeled = argo_nullable_wire_type:is_labeled(NullableWireType),
    {MessageDecoder2, NullableType} = argo_message_decoder:read_core_nullable_type(MessageDecoder1, IsLabeled),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
    case NullableType of
        null ->
            NullableValue = argo_nullable_value:null(NullableWireType),
            {ValueDecoder2, NullableValue};
        non_null ->
            {ValueDecoder3, Value} = decode_wire_type(ValueDecoder2, NullableWireType#argo_nullable_wire_type.'of'),
            NullableValue = argo_nullable_value:non_null(NullableWireType, Value),
            {ValueDecoder3, NullableValue};
        error ->
            NullableValue = argo_nullable_value:field_errors(NullableWireType, []),
            {ValueDecoder2, NullableValue}
    end.

%% @private
-spec decode_array_wire_type(ValueDecoder, ArrayWireType) -> {ValueDecoder, ArrayValue} when
    ValueDecoder :: t(), ArrayWireType :: argo_array_wire_type:t(), ArrayValue :: argo_array_value:t().
decode_array_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, ArrayWireType = #argo_array_wire_type{}
) ->
    MessageDecoder2 =
        case argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) of
            false ->
                MessageDecoder1;
            true ->
                {MD2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
                case Label of
                    ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST ->
                        MD2;
                    _ ->
                        error_with_info(badarg, [ValueDecoder1, ArrayWireType], #{1 => {invalid_array_label, Label}})
                end
        end,
    {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
    {ValueDecoder3, Items} = decode_array_wire_type_items(ValueDecoder2, ArrayWireType, Length, []),
    ArrayValue = argo_array_value:new(ArrayWireType, Items),
    {ValueDecoder3, ArrayValue}.

%% @private
-spec decode_array_wire_type_items(ValueDecoder, ArrayWireType, Length, Items) -> {ValueDecoder, Items} when
    ValueDecoder :: t(),
    ArrayWireType :: argo_array_wire_type:t(),
    Length :: non_neg_integer(),
    Items :: [argo_value:t()].
decode_array_wire_type_items(ValueDecoder1 = #argo_value_decoder{}, #argo_array_wire_type{}, 0, Items) ->
    {ValueDecoder1, lists:reverse(Items)};
decode_array_wire_type_items(
    ValueDecoder1 = #argo_value_decoder{}, ArrayWireType = #argo_array_wire_type{}, Length, Items
) when is_integer(Length) andalso Length > 0 ->
    {ValueDecoder2, Value} = decode_wire_type(ValueDecoder1, ArrayWireType#argo_array_wire_type.'of'),
    decode_array_wire_type_items(ValueDecoder2, ArrayWireType, Length - 1, [Value | Items]).

% push_caught(Value) ->
%     LastValues =
%         case erlang:get(caught) of
%             undefined ->
%                 [];
%             Values ->
%                 Values
%         end,
%     erlang:put(caught, [Value | LastValues]).

%% @private
-spec decode_record_wire_type(ValueDecoder, RecordWireType) -> {ValueDecoder, RecordValue} when
    ValueDecoder :: t(), RecordWireType :: argo_record_wire_type:t(), RecordValue :: argo_record_value:t().
decode_record_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, RecordWireType = #argo_record_wire_type{}
) ->
    case argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) of
        false ->
            argo_index_map:foldl(
                fun(_Index, _FieldName, FieldWireType, {ValueDecoderAcc1, RecordValueAcc1}) ->
                    % try decode_field_wire_type(ValueDecoderAcc1, FieldWireType) of
                    %     {ValueDecoderAcc2, FieldValue} ->
                    %         RecordValueAcc2 = argo_record_value:insert(RecordValueAcc1, FieldValue),
                    %         {ValueDecoderAcc2, RecordValueAcc2}
                    % catch
                    %     Class:Reason:Stacktrace ->
                    %         push_caught({FieldWireType, argo_value:record(RecordValueAcc1)}),
                    %         erlang:raise(error, badarg, Stacktrace)
                    % end
                    {ValueDecoderAcc2, FieldValue} = decode_field_wire_type(ValueDecoderAcc1, FieldWireType),
                    RecordValueAcc2 = argo_record_value:insert(RecordValueAcc1, FieldValue),
                    {ValueDecoderAcc2, RecordValueAcc2}
                end,
                {ValueDecoder1, argo_record_value:new()},
                RecordWireType#argo_record_wire_type.fields
            );
        true ->
            decode_self_describing_record_wire_type(ValueDecoder1, RecordWireType)
    end.

%% @private
-spec decode_field_wire_type(ValueDecoder, FieldWireType) -> {ValueDecoder, FieldValue} when
    ValueDecoder :: t(), FieldWireType :: argo_field_wire_type:t(), FieldValue :: argo_field_value:t().
decode_field_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, FieldWireType = #argo_field_wire_type{}
) ->
    case FieldWireType#argo_field_wire_type.omittable of
        false ->
            {ValueDecoder2, Value} = decode_wire_type(ValueDecoder1, FieldWireType#argo_field_wire_type.'of'),
            FieldValue = argo_field_value:required(FieldWireType, Value),
            {ValueDecoder2, FieldValue};
        true ->
            IsLabeled = argo_wire_type:is_labeled(FieldWireType#argo_field_wire_type.'of'),
            {MessageDecoder2, OmittableType} = argo_message_decoder:read_core_omittable_type(
                MessageDecoder1, IsLabeled
            ),
            case OmittableType of
                absent ->
                    FieldValue = argo_field_value:optional(FieldWireType, none),
                    ValueDecoder2 = #argo_value_decoder{message = MessageDecoder2},
                    {ValueDecoder2, FieldValue};
                non_null ->
                    ValueDecoder2 = #argo_value_decoder{message = MessageDecoder2},
                    {ValueDecoder3, Value} = decode_wire_type(
                        ValueDecoder2, FieldWireType#argo_field_wire_type.'of'
                    ),
                    FieldValue = argo_field_value:optional(FieldWireType, {some, Value}),
                    {ValueDecoder3, FieldValue}
            end
    end.

%% @private
-spec decode_desc_wire_type(ValueDecoder) -> {ValueDecoder, DescValue} when
    ValueDecoder :: t(), DescValue :: argo_desc_value:t().
decode_desc_wire_type(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_NULL ->
            DescValue = argo_desc_value:null(),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FALSE ->
            DescValue = argo_desc_value:boolean(false),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_TRUE ->
            DescValue = argo_desc_value:boolean(true),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_desc_wire_type_object(ValueDecoder2, Length, argo_index_map:new());
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_desc_wire_type_list(ValueDecoder2, Length, []);
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_string(MessageDecoder2),
            DescValue = argo_desc_value:string(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_bytes(MessageDecoder2),
            DescValue = argo_desc_value:bytes(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_varint(MessageDecoder2),
            DescValue = argo_desc_value:int(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_float64(MessageDecoder2),
            DescValue = argo_desc_value:float(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        _ ->
            error_with_info(badarg, [ValueDecoder1], #{1 => {invalid_self_describing_label, Label}})
    end.

%% @private
-spec decode_desc_wire_type_object(ValueDecoder, Length, Object) -> {ValueDecoder, Object} when
    ValueDecoder :: t(),
    Length :: non_neg_integer(),
    Object :: argo_index_map:t(unicode:unicode_binary(), argo_desc_value:t()).
decode_desc_wire_type_object(ValueDecoder1 = #argo_value_decoder{}, 0, Object1) ->
    DescValue = argo_desc_value:object(Object1),
    {ValueDecoder1, DescValue};
decode_desc_wire_type_object(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, Object1) when
    is_integer(Length) andalso Length > 0
->
    {MessageDecoder2, Key} = argo_message_decoder:decode_block_string(MessageDecoder1),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
    {ValueDecoder3, Val} = decode_desc_wire_type(ValueDecoder2),
    Object2 = argo_index_map:put(Key, Val, Object1),
    decode_desc_wire_type_object(ValueDecoder3, Length - 1, Object2).

%% @private
-spec decode_desc_wire_type_list(ValueDecoder, Length, List) -> {ValueDecoder, List} when
    ValueDecoder :: t(), Length :: non_neg_integer(), List :: [argo_desc_value:t()].
decode_desc_wire_type_list(ValueDecoder1 = #argo_value_decoder{}, 0, List1) ->
    DescValue = argo_desc_value:list(lists:reverse(List1)),
    {ValueDecoder1, DescValue};
decode_desc_wire_type_list(ValueDecoder1 = #argo_value_decoder{}, Length, List1) when
    is_integer(Length) andalso Length > 0
->
    {ValueDecoder2, Element} = decode_desc_wire_type(ValueDecoder1),
    List2 = [Element | List1],
    decode_desc_wire_type_list(ValueDecoder2, Length - 1, List2).

%% @private
-spec decode_error_wire_type(ValueDecoder) -> {ValueDecoder, ErrorValue} when
    ValueDecoder :: t(), ErrorValue :: argo_error_value:t().
decode_error_wire_type(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}) ->
    case
        argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) orelse
            argo_header:self_describing_errors(MessageDecoder1#argo_message_decoder.header)
    of
        false ->
            {MessageDecoder2, Message} = argo_message_decoder:decode_block_string(MessageDecoder1),
            {MessageDecoder3, LocationOmittableType} = argo_message_decoder:read_core_omittable_type(
                MessageDecoder2, true
            ),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder3 = #argo_value_decoder{message = MessageDecoder4}, Location} =
                case LocationOmittableType of
                    absent ->
                        {ValueDecoder2, none};
                    non_null ->
                        MD3_1 = MessageDecoder3,
                        {MD3_2, LocationLength} = argo_message_decoder:read_core_length(MD3_1),
                        VD2_1 = ValueDecoder2,
                        VD2_2 = VD2_1#argo_value_decoder{message = MD3_2},
                        {VD2_3, LocationList} = decode_error_wire_type_location(VD2_2, LocationLength, []),
                        {VD2_3, {some, LocationList}}
                end,
            {MessageDecoder5, PathOmittableType} = argo_message_decoder:read_core_omittable_type(MessageDecoder4, true),
            ValueDecoder4 = ValueDecoder3#argo_value_decoder{message = MessageDecoder5},
            {ValueDecoder5 = #argo_value_decoder{message = MessageDecoder6}, Path} =
                case PathOmittableType of
                    absent ->
                        {ValueDecoder4, none};
                    non_null ->
                        VD4_1 = ValueDecoder4,
                        {VD4_2, PathValue} = decode_path_wire_type(VD4_1),
                        {VD4_2, {some, PathValue}}
                end,
            {MessageDecoder7, ExtensionsOmittableType} = argo_message_decoder:read_core_omittable_type(
                MessageDecoder6, false
            ),
            ValueDecoder6 = ValueDecoder5#argo_value_decoder{message = MessageDecoder7},
            {ValueDecoder7 = #argo_value_decoder{}, Extensions} =
                case ExtensionsOmittableType of
                    absent ->
                        {ValueDecoder6, none};
                    non_null ->
                        MD7_1 = MessageDecoder7,
                        {MD7_2, ExtensionsLabel} = argo_message_decoder:read_core_label(MD7_1),
                        case ExtensionsLabel of
                            ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
                                {MD7_3, ExtensionsLength} = argo_message_decoder:read_core_length(MD7_2),
                                VD6_1 = ValueDecoder6,
                                VD6_2 = VD6_1#argo_value_decoder{message = MD7_3},
                                {VD6_3, #argo_desc_value{inner = {object, ExtensionsValue}}} = decode_desc_wire_type_object(
                                    VD6_2, ExtensionsLength, argo_index_map:new()
                                ),
                                {VD6_3, {some, ExtensionsValue}};
                            _ ->
                                error_with_info(badarg, [ValueDecoder1], #{1 => {invalid_record_label, ExtensionsLabel}})
                        end
                end,
            ErrorValue = argo_error_value:new(Message, Location, Path, Extensions),
            {ValueDecoder7, ErrorValue};
        true ->
            decode_self_describing_error_wire_type(ValueDecoder1)
    end.

%% @private
-spec decode_error_wire_type_location(ValueDecoder, Length, Location) -> {ValueDecoder, Location} when
    ValueDecoder :: t(), Length :: non_neg_integer(), Location :: [argo_location_value:t()].
decode_error_wire_type_location(ValueDecoder1 = #argo_value_decoder{}, 0, List1) ->
    {ValueDecoder1, lists:reverse(List1)};
decode_error_wire_type_location(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, List1) when
    is_integer(Length) andalso Length > 0
->
    {MessageDecoder2, Line} = argo_message_decoder:decode_block_varint(MessageDecoder1),
    {MessageDecoder3, Column} = argo_message_decoder:decode_block_varint(MessageDecoder2),
    LocationValue = argo_location_value:new(Line, Column),
    List2 = [LocationValue | List1],
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
    decode_error_wire_type_location(ValueDecoder2, Length - 1, List2).

%% @private
-spec decode_path_wire_type(ValueDecoder) -> {ValueDecoder, PathValue} when
    ValueDecoder :: t(), PathValue :: argo_path_value:t().
decode_path_wire_type(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}) ->
    case argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) of
        false ->
            {MessageDecoder2, Length} = argo_message_decoder:read_core_length(MessageDecoder1),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, PathValue} = decode_path_wire_type_segments(ValueDecoder2, Length, argo_path_value:new()),
            {ValueDecoder3, PathValue};
        true ->
            decode_self_describing_path_wire_type(ValueDecoder1)
    end.

%% @private
-spec decode_path_wire_type_segments(ValueDecoder, Length, PathValue) -> {ValueDecoder, PathValue} when
    ValueDecoder :: t(), Length :: non_neg_integer(), PathValue :: argo_path_value:t().
decode_path_wire_type_segments(ValueDecoder1 = #argo_value_decoder{}, 0, PathValue1) ->
    {ValueDecoder1, PathValue1};
decode_path_wire_type_segments(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, PathValue1) when
    is_integer(Length) andalso Length > 0
->
    case argo_message_decoder:read_core_label(MessageDecoder1) of
        {MessageDecoder2, ?ARGO_LABEL_MARKER_NON_NULL} ->
            {MessageDecoder3, Index} = argo_message_decoder:decode_block_varint(MessageDecoder2),
            PathValue2 = argo_path_value:push_list_index(PathValue1, Index),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_path_wire_type_segments(ValueDecoder2, Length - 1, PathValue2);
        {_MessageDecoder2Unused, _LabelUnused} ->
            {MessageDecoder2, Name} = argo_message_decoder:decode_block_string(MessageDecoder1),
            PathValue2 = argo_path_value:push_field_name(PathValue1, Name),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            decode_path_wire_type_segments(ValueDecoder2, Length - 1, PathValue2)
    end.

%% @private
-spec decode_self_describing_error_wire_type(ValueDecoder) -> {ValueDecoder, ErrorValue} when
    ValueDecoder :: t(), ErrorValue :: argo_error_value:t().
decode_self_describing_error_wire_type(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, ErrorLabel} = argo_message_decoder:read_core_label(MessageDecoder1),
    case ErrorLabel of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
            {MessageDecoder3, ErrorLength} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_self_describing_error_wire_type_fields(ValueDecoder2, ErrorLength, maps:new());
        _ ->
            error_with_info(badarg, [ValueDecoder1], #{1 => {invalid_record_label, ErrorLabel}})
    end.

%% @private
-spec decode_self_describing_error_wire_type_fields(ValueDecoder, Length, Map) -> {ValueDecoder, ErrorValue} when
    ValueDecoder :: t(),
    Length :: non_neg_integer(),
    Map :: #{binary() => dynamic()},
    ErrorValue :: argo_error_value:t().
decode_self_describing_error_wire_type_fields(ValueDecoder1 = #argo_value_decoder{}, 0, Map1) ->
    Message =
        case maps:find(<<"message">>, Map1) of
            {ok, MessageValue} when is_binary(MessageValue) ->
                MessageValue;
            error ->
                error_with_info(badarg, [ValueDecoder1, 0, Map1], #{1 => {missing_required_field, <<"message">>}})
        end,
    Location =
        case maps:find(<<"location">>, Map1) of
            {ok, LocationValue} when is_list(LocationValue) ->
                {some, LocationValue};
            error ->
                none
        end,
    Path =
        case maps:find(<<"path">>, Map1) of
            {ok, PathValue = #argo_path_value{}} ->
                {some, PathValue};
            error ->
                none
        end,
    Extensions =
        case maps:find(<<"extensions">>, Map1) of
            {ok, ExtensionsValue = #argo_index_map{}} ->
                {some, ExtensionsValue};
            error ->
                none
        end,
    ErrorValue = argo_error_value:new(Message, Location, Path, Extensions),
    {ValueDecoder1, ErrorValue};
decode_self_describing_error_wire_type_fields(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, Map1
) when is_integer(Length) andalso Length > 0 ->
    {MessageDecoder2, Key} = argo_message_decoder:decode_block_string(MessageDecoder1),
    case Key of
        _ when is_map_key(Key, Map1) ->
            error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {duplicate_field, Key}});
        <<"message">> ->
            {MessageDecoder3, MessageLabel} = argo_message_decoder:read_core_label(MessageDecoder2),
            case MessageLabel of
                ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING ->
                    {MessageDecoder4, MessageValue} = argo_message_decoder:decode_block_string(MessageDecoder3),
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder4},
                    Map2 = maps:put(Key, MessageValue, Map1),
                    decode_self_describing_error_wire_type_fields(ValueDecoder2, Length - 1, Map2);
                _ ->
                    error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {invalid_string_label, MessageLabel}})
            end;
        <<"location">> ->
            {MessageDecoder3, LocationLabel} = argo_message_decoder:read_core_label(MessageDecoder2),
            case LocationLabel of
                ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST ->
                    {MessageDecoder4, LocationLength} = argo_message_decoder:read_core_length(MessageDecoder3),
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder4},
                    {ValueDecoder3, LocationList} = decode_self_describing_error_wire_type_location(
                        ValueDecoder2, LocationLength, []
                    ),
                    Map2 = maps:put(Key, LocationList, Map1),
                    decode_self_describing_error_wire_type_fields(ValueDecoder3, Length - 1, Map2);
                _ ->
                    error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {invalid_array_label, LocationLabel}})
            end;
        <<"path">> ->
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, PathValue} = decode_self_describing_path_wire_type(ValueDecoder2),
            Map2 = maps:put(Key, PathValue, Map1),
            decode_self_describing_error_wire_type_fields(ValueDecoder3, Length - 1, Map2);
        <<"extensions">> ->
            {MessageDecoder3, ExtensionsLabel} = argo_message_decoder:read_core_label(MessageDecoder2),
            case ExtensionsLabel of
                ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
                    {MessageDecoder4, ExtensionsLength} = argo_message_decoder:read_core_length(MessageDecoder3),
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder4},
                    {ValueDecoder3, #argo_desc_value{inner = {object, ExtensionsObject}}} = decode_desc_wire_type_object(
                        ValueDecoder2, ExtensionsLength, argo_index_map:new()
                    ),
                    Map2 = maps:put(Key, ExtensionsObject, Map1),
                    decode_self_describing_error_wire_type_fields(ValueDecoder3, Length - 1, Map2);
                _ ->
                    error_with_info(badarg, [ValueDecoder1, Length, Map1], #{
                        1 => {invalid_record_label, ExtensionsLabel}
                    })
            end;
        _ ->
            error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {unknown_field, Key}})
    end.

%% @private
-spec decode_self_describing_error_wire_type_location(ValueDecoder, Length, Location) -> {ValueDecoder, Location} when
    ValueDecoder :: t(), Length :: non_neg_integer(), Location :: [argo_location_value:t()].
decode_self_describing_error_wire_type_location(ValueDecoder1 = #argo_value_decoder{}, 0, List1) ->
    {ValueDecoder1, lists:reverse(List1)};
decode_self_describing_error_wire_type_location(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, List1
) when
    is_integer(Length) andalso Length > 0
->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
            {MessageDecoder3, RecordLength} = argo_message_decoder:read_core_length(MessageDecoder2),
            case RecordLength of
                2 ->
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
                    {ValueDecoder3, LocationValue} = decode_self_describing_error_wire_type_location_fields(
                        ValueDecoder2, RecordLength, maps:new()
                    ),
                    List2 = [LocationValue | List1],
                    decode_self_describing_error_wire_type_location(ValueDecoder3, Length - 1, List2);
                _ ->
                    error_with_info(badarg, [ValueDecoder1, Length, List1], #{
                        1 => {invalid_record_length_exact, #{expected => 2, actual => RecordLength}}
                    })
            end;
        _ ->
            error_with_info(badarg, [ValueDecoder1, Length, List1], #{1 => {invalid_record_label, Label}})
    end.

%% @private
-spec decode_self_describing_error_wire_type_location_fields(ValueDecoder, Length, Map) ->
    {ValueDecoder, LocationValue}
when
    ValueDecoder :: t(),
    Length :: non_neg_integer(),
    Map :: #{binary() => dynamic()},
    LocationValue :: argo_location_value:t().
decode_self_describing_error_wire_type_location_fields(ValueDecoder1 = #argo_value_decoder{}, 0, Map1) ->
    Line =
        case maps:find(<<"line">>, Map1) of
            {ok, LineValue} when ?is_i64(LineValue) ->
                LineValue;
            error ->
                error_with_info(badarg, [ValueDecoder1, 0, Map1], #{1 => {missing_required_field, <<"line">>}})
        end,
    Column =
        case maps:find(<<"column">>, Map1) of
            {ok, ColumnValue} when ?is_i64(ColumnValue) ->
                ColumnValue;
            error ->
                error_with_info(badarg, [ValueDecoder1, 0, Map1], #{1 => {missing_required_field, <<"column">>}})
        end,
    LocationValue = argo_location_value:new(Line, Column),
    {ValueDecoder1, LocationValue};
decode_self_describing_error_wire_type_location_fields(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, Map1
) when is_integer(Length) andalso Length > 0 ->
    {MessageDecoder2, Key} = argo_message_decoder:decode_block_string(MessageDecoder1),
    case Key of
        _ when is_map_key(Key, Map1) ->
            error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {duplicate_field, Key}});
        <<"line">> ->
            {MessageDecoder3, LineLabel} = argo_message_decoder:read_core_label(MessageDecoder2),
            case LineLabel of
                ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT ->
                    {MessageDecoder4, LineValue} = argo_message_decoder:decode_block_varint(MessageDecoder3),
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder4},
                    Map2 = maps:put(Key, LineValue, Map1),
                    decode_self_describing_error_wire_type_location_fields(ValueDecoder2, Length - 1, Map2);
                _ ->
                    error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {invalid_array_label, LineLabel}})
            end;
        <<"column">> ->
            {MessageDecoder3, ColumnLabel} = argo_message_decoder:read_core_label(MessageDecoder2),
            case ColumnLabel of
                ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT ->
                    {MessageDecoder4, ColumnValue} = argo_message_decoder:decode_block_varint(MessageDecoder3),
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder4},
                    Map2 = maps:put(Key, ColumnValue, Map1),
                    decode_self_describing_error_wire_type_location_fields(ValueDecoder2, Length - 1, Map2);
                _ ->
                    error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {invalid_array_label, ColumnLabel}})
            end;
        _ ->
            error_with_info(badarg, [ValueDecoder1, Length, Map1], #{1 => {unknown_field, Key}})
    end.

%% @private
-spec maybe_decode_self_describing_label_for_scalar(ValueDecoder, ScalarWireType) -> ValueDecoder when
    ValueDecoder :: t(), ScalarWireType :: argo_scalar_wire_type:t().
maybe_decode_self_describing_label_for_scalar(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, ScalarWireType = #argo_scalar_wire_type{}
) ->
    case argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) of
        false ->
            ValueDecoder1;
        true ->
            decode_self_describing_label_for_scalar(ValueDecoder1, ScalarWireType)
    end.

%% @private
-spec decode_self_describing_label_for_scalar(ValueDecoder, ScalarWireType) -> ValueDecoder when
    ValueDecoder :: t(), ScalarWireType :: argo_scalar_wire_type:t().
decode_self_describing_label_for_scalar(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, ScalarWireType = #argo_scalar_wire_type{}
) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
    case ScalarWireType#argo_scalar_wire_type.inner of
        string when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING ->
            ValueDecoder2;
        boolean when
            (Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FALSE orelse
                Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_TRUE)
        ->
            % rollback read_core_label for boolean
            ValueDecoder1;
        varint when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT ->
            ValueDecoder2;
        float64 when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT ->
            ValueDecoder2;
        bytes when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES ->
            ValueDecoder2;
        #argo_fixed_wire_type{} when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES ->
            ValueDecoder2;
        string ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_string_label, Label}});
        boolean ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_boolean_label, Label}});
        varint ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_varint_label, Label}});
        float64 ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_float64_label, Label}});
        bytes ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_bytes_label, Label}});
        #argo_fixed_wire_type{} ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_fixed_label, Label}})
    end.

%% @private
-spec decode_self_describing_path_wire_type(ValueDecoder) -> {ValueDecoder, PathValue} when
    ValueDecoder :: t(), PathValue :: argo_path_value:t().
decode_self_describing_path_wire_type(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder3, PathValue} = decode_self_describing_path_wire_type_segments(
                ValueDecoder2, Length, argo_path_value:new()
            ),
            {ValueDecoder3, PathValue};
        _ ->
            error_with_info(badarg, [ValueDecoder1], #{1 => {invalid_array_label, Label}})
    end.

%% @private
-spec decode_self_describing_path_wire_type_segments(ValueDecoder, Length, PathValue) -> {ValueDecoder, PathValue} when
    ValueDecoder :: t(), Length :: non_neg_integer(), PathValue :: argo_path_value:t().
decode_self_describing_path_wire_type_segments(ValueDecoder1 = #argo_value_decoder{}, 0, PathValue1) ->
    {ValueDecoder1, PathValue1};
decode_self_describing_path_wire_type_segments(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, PathValue1
) when is_integer(Length) andalso Length > 0 ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT ->
            {MessageDecoder3, Index} = argo_message_decoder:decode_block_varint(MessageDecoder2),
            PathValue2 = argo_path_value:push_list_index(PathValue1, Index),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_self_describing_path_wire_type_segments(ValueDecoder2, Length - 1, PathValue2);
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING ->
            {MessageDecoder3, Name} = argo_message_decoder:decode_block_string(MessageDecoder2),
            PathValue2 = argo_path_value:push_field_name(PathValue1, Name),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_self_describing_path_wire_type_segments(ValueDecoder2, Length - 1, PathValue2);
        _ ->
            error_with_info(badarg, [ValueDecoder1], #{1 => {invalid_string_label, Label}})
    end.

%% @private
-spec decode_self_describing_record_wire_type(ValueDecoder, RecordWireType) -> ValueDecoder when
    ValueDecoder :: t(), RecordWireType :: argo_record_wire_type:t().
decode_self_describing_record_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, RecordWireType = #argo_record_wire_type{}
) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_self_describing_record_wire_type_fields(ValueDecoder2, RecordWireType, Length, maps:new());
        _ ->
            error_with_info(badarg, [ValueDecoder1, RecordWireType], #{1 => {invalid_record_label, Label}})
    end.

%% @private
-spec decode_self_describing_record_wire_type_fields(ValueDecoder, RecordWireType, Length, Map) ->
    {ValueDecoder, RecordValue}
when
    ValueDecoder :: t(),
    RecordWireType :: argo_record_wire_type:t(),
    Length :: non_neg_integer(),
    Map :: #{argo_types:name() => argo_field_value:t()},
    RecordValue :: argo_record_value:t().
decode_self_describing_record_wire_type_fields(
    ValueDecoder1 = #argo_value_decoder{}, RecordWireType = #argo_record_wire_type{}, 0, Map1
) ->
    RecordValue = argo_index_map:foldl(
        fun(_Index, FieldName, FieldWireType, RecordValueAcc) ->
            case maps:find(FieldName, Map1) of
                {ok, FieldValue} ->
                    argo_record_value:insert(RecordValueAcc, FieldValue);
                error ->
                    case FieldWireType#argo_field_wire_type.omittable of
                        false ->
                            error_with_info(badarg, [ValueDecoder1, RecordWireType, 0, Map1], #{
                                1 => {missing_required_field, FieldName}
                            });
                        true ->
                            FieldValue = argo_field_value:optional(FieldWireType, none),
                            argo_record_value:insert(RecordValueAcc, FieldValue)
                    end
            end
        end,
        argo_record_value:new(),
        RecordWireType#argo_record_wire_type.fields
    ),
    {ValueDecoder1, RecordValue};
decode_self_describing_record_wire_type_fields(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1},
    RecordWireType = #argo_record_wire_type{},
    Length,
    Map1
) when is_integer(Length) andalso Length > 0 ->
    {MessageDecoder2, FieldName} = argo_message_decoder:decode_block_string(MessageDecoder1),
    case maps:is_key(FieldName, Map1) of
        false ->
            case argo_index_map:find(FieldName, RecordWireType#argo_record_wire_type.fields) of
                {ok, FieldWireType = #argo_field_wire_type{}} ->
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
                    {ValueDecoder3, Value} = decode_wire_type(ValueDecoder2, FieldWireType#argo_field_wire_type.'of'),
                    case FieldWireType#argo_field_wire_type.omittable of
                        false ->
                            Map2 = maps:put(FieldName, argo_field_value:required(FieldWireType, Value), Map1),
                            decode_self_describing_record_wire_type_fields(
                                ValueDecoder3, RecordWireType, Length - 1, Map2
                            );
                        true ->
                            Map2 = maps:put(FieldName, argo_field_value:optional(FieldWireType, {some, Value}), Map1),
                            decode_self_describing_record_wire_type_fields(
                                ValueDecoder3, RecordWireType, Length - 1, Map2
                            )
                    end;
                error ->
                    error_with_info(badarg, [ValueDecoder1, RecordWireType, Length, Map1], #{
                        1 => {unknown_field, FieldName}
                    })
            end;
        true ->
            error_with_info(badarg, [ValueDecoder1, RecordWireType, Length, Map1], #{1 => {duplicate_field, FieldName}})
    end.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
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
format_error_description(_Key, {duplicate_field, FieldName}) ->
    io_lib:format("duplicate FIELD encountered: ~0tp", [FieldName]);
format_error_description(_Key, {invalid_array_label, Actual}) ->
    io_lib:format("invalid ARRAY label, expected ~w, but was ~w", [?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST, Actual]);
format_error_description(_Key, {invalid_boolean_label, Actual}) ->
    io_lib:format("invalid BOOLEAN label, expected ~w or ~w, but was ~w", [
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FALSE, ?ARGO_LABEL_SELF_DESCRIBING_MARKER_TRUE, Actual
    ]);
format_error_description(_Key, {invalid_bytes_label, Actual}) ->
    io_lib:format("invalid BYTES label, expected ~w, but was ~w", [?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES, Actual]);
format_error_description(_Key, {invalid_fixed_label, Actual}) ->
    io_lib:format("invalid FIXED label, expected ~w, but was ~w", [?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES, Actual]);
format_error_description(_Key, {invalid_float64_label, Actual}) ->
    io_lib:format("invalid FLOAT64 label, expected ~w, but was ~w", [?ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT, Actual]);
format_error_description(_Key, {invalid_nullable_label, Actual}) ->
    io_lib:format("invalid NULLABLE label, expected ~w or ~w, but was ~w", [
        ?ARGO_LABEL_MARKER_NULL, ?ARGO_LABEL_MARKER_NON_NULL, Actual
    ]);
format_error_description(_Key, {invalid_record_label, Actual}) ->
    io_lib:format("invalid RECORD label, expected ~w, but was ~w", [?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT, Actual]);
format_error_description(_Key, {invalid_record_length_exact, #{expected := Expected, actual := Actual}}) ->
    io_lib:format("invalid RECORD length, expected ~w, but was ~w", [Expected, Actual]);
format_error_description(_Key, {invalid_string_label, Actual}) ->
    io_lib:format("invalid STRING label, expected ~w, but was ~w", [?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING, Actual]);
format_error_description(_Key, {invalid_self_describing_label, Actual}) ->
    io_lib:format("unknown self-describing label: ~w", [Actual]);
format_error_description(_Key, {invalid_varint_label, Actual}) ->
    io_lib:format("invalid VARINT label, expected ~w, but was ~w", [?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT, Actual]);
format_error_description(_Key, {missing_required_field, FieldName}) ->
    io_lib:format("missing required FIELD encountered: ~0tp", [FieldName]);
format_error_description(_Key, {unknown_field, FieldName}) ->
    io_lib:format("unknown FIELD encountered: ~0tp", [FieldName]);
format_error_description(_Key, Value) ->
    Value.
