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
-module(argo_json_value_decoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/0,
    new/2,
    decode_wire_type/3
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_json_value_decoder{}.

-export_type([
    t/0
]).

%% Macros
-define(is_json_object(X),
    (is_map((X)) orelse is_record((X), argo_index_map) orelse
        (is_tuple((X)) andalso tuple_size((X)) =:= 1 andalso is_list(element(1, (X)))))
).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> JsonValueDecoder when JsonValueDecoder :: t().
new() ->
    new(argo_json_scalar_decoder_base64, #{}).

-spec new(JsonScalarDecoderModule, JsonScalarDecoderOptions) -> JsonValueDecoder when
    JsonScalarDecoderModule :: module(),
    JsonScalarDecoderOptions :: argo_json_scalar_decoder:options(),
    JsonValueDecoder :: t().
new(JsonScalarDecoderModule, JsonScalarDecoderOptions) when is_atom(JsonScalarDecoderModule) ->
    JsonScalarDecoder = JsonScalarDecoderModule:init(JsonScalarDecoderOptions),
    #argo_json_value_decoder{
        current_path = argo_path_value:new(),
        field_errors = argo_index_map:new(),
        response_errors = [],
        scalar_decoder = {JsonScalarDecoderModule, JsonScalarDecoder}
    }.

-spec decode_wire_type(JsonValueDecoder, WireType, JsonValue) -> {JsonValueDecoder, Value} when
    JsonValueDecoder :: t(),
    WireType :: argo_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    Value :: argo_value:t().
decode_wire_type(JsonValueDecoder1 = #argo_json_value_decoder{}, WireType = #argo_wire_type{}, JsonValue) ->
    case WireType#argo_wire_type.inner of
        ScalarWireType = #argo_scalar_wire_type{} ->
            {JsonValueDecoder2, ScalarValue} = decode_scalar_wire_type(JsonValueDecoder1, ScalarWireType, JsonValue),
            Value = argo_value:scalar(ScalarValue),
            {JsonValueDecoder2, Value};
        BlockWireType = #argo_block_wire_type{} ->
            {JsonValueDecoder2, BlockValue} = decode_block_wire_type(JsonValueDecoder1, BlockWireType, JsonValue),
            Value = argo_value:block(BlockValue),
            {JsonValueDecoder2, Value};
        NullableWireType = #argo_nullable_wire_type{} ->
            {JsonValueDecoder2, NullableValue} = decode_nullable_wire_type(
                JsonValueDecoder1, NullableWireType, JsonValue
            ),
            Value = argo_value:nullable(NullableValue),
            {JsonValueDecoder2, Value};
        ArrayWireType = #argo_array_wire_type{} ->
            {JsonValueDecoder2, ArrayValue} = decode_array_wire_type(JsonValueDecoder1, ArrayWireType, JsonValue),
            Value = argo_value:array(ArrayValue),
            {JsonValueDecoder2, Value};
        RecordWireType = #argo_record_wire_type{} ->
            {JsonValueDecoder2, RecordValue} = decode_record_wire_type(JsonValueDecoder1, RecordWireType, JsonValue),
            Value = argo_value:record(RecordValue),
            {JsonValueDecoder2, Value};
        #argo_desc_wire_type{} ->
            {JsonValueDecoder2, DescValue} = decode_desc_wire_type(JsonValueDecoder1, JsonValue),
            Value = argo_value:desc(DescValue),
            {JsonValueDecoder2, Value};
        #argo_error_wire_type{} ->
            {JsonValueDecoder2, ErrorValue} = decode_error_wire_type(JsonValueDecoder1, JsonValue),
            Value = argo_value:error(ErrorValue),
            {JsonValueDecoder2, Value};
        #argo_path_wire_type{} ->
            {JsonValueDecoder2, PathValue} = decode_path_wire_type(JsonValueDecoder1, JsonValue),
            Value = argo_value:path(PathValue),
            {JsonValueDecoder2, Value}
    end.

-spec decode_scalar_wire_type(JsonValueDecoder, ScalarWireType, JsonValue) -> {JsonValueDecoder, ScalarValue} when
    JsonValueDecoder :: t(),
    ScalarWireType :: argo_scalar_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    ScalarValue :: argo_scalar_value:t().
decode_scalar_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{scalar_decoder = {JsonScalarDecoderModule, JsonScalarDecoder1}},
    ScalarWireType = #argo_scalar_wire_type{},
    JsonValue
) ->
    ScalarHint =
        case ScalarWireType#argo_scalar_wire_type.inner of
            #argo_fixed_wire_type{length = FixedLength} ->
                {fixed, FixedLength};
            ScalarWireTypeInner when is_atom(ScalarWireTypeInner) ->
                ScalarWireTypeInner
        end,
    case JsonScalarDecoderModule:decode_scalar(JsonScalarDecoder1, ScalarHint, JsonValue) of
        {ok, JsonScalarDecoder2, {string, StringValue}} when is_binary(StringValue) andalso ScalarHint =:= string ->
            ScalarValue = argo_scalar_value:string(StringValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, ScalarValue};
        {ok, JsonScalarDecoder2, {boolean, BooleanValue}} when
            is_boolean(BooleanValue) andalso ScalarHint =:= boolean
        ->
            ScalarValue = argo_scalar_value:boolean(BooleanValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, ScalarValue};
        {ok, JsonScalarDecoder2, {varint, VarintValue}} when ?is_i64(VarintValue) andalso ScalarHint =:= varint ->
            ScalarValue = argo_scalar_value:varint(VarintValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, ScalarValue};
        {ok, JsonScalarDecoder2, {float64, Float64Value}} when is_float(Float64Value) andalso ScalarHint =:= float64 ->
            ScalarValue = argo_scalar_value:float64(Float64Value),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, ScalarValue};
        {ok, JsonScalarDecoder2, {bytes, BytesValue}} when is_binary(BytesValue) andalso ScalarHint =:= bytes ->
            ScalarValue = argo_scalar_value:bytes(BytesValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, ScalarValue};
        {ok, JsonScalarDecoder2, {fixed, FixedValue}} when
            is_binary(FixedValue) andalso element(1, ScalarHint) =:= fixed andalso
                element(2, ScalarHint) =:= byte_size(FixedValue)
        ->
            ScalarValue = argo_scalar_value:fixed(FixedValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, ScalarValue};
        {ok, _JsonScalarDecoder2, Scalar} ->
            error_scalar_type_mismatch([JsonValueDecoder1, ScalarWireType, JsonValue], ScalarHint, Scalar);
        {error, type_mismatch} ->
            error_scalar_type_mismatch([JsonValueDecoder1, ScalarWireType, JsonValue], ScalarHint, {json, JsonValue});
        {error, _} ->
            error_with_info(badarg, [JsonValueDecoder1, ScalarWireType, JsonValue], #{
                3 => {failed_to_decode_scalar, JsonValue}
            })
    end.

-spec decode_block_wire_type(JsonValueDecoder, BlockWireType, JsonValue) -> {JsonValueDecoder, BlockValue} when
    JsonValueDecoder :: t(),
    BlockWireType :: argo_block_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    BlockValue :: argo_block_value:t().
decode_block_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{scalar_decoder = {JsonScalarDecoderModule, JsonScalarDecoder1}},
    BlockWireType = #argo_block_wire_type{key = BlockKey},
    JsonValue
) ->
    BlockScalarHint =
        case BlockWireType#argo_block_wire_type.'of'#argo_scalar_wire_type.inner of
            #argo_fixed_wire_type{length = FixedLength} ->
                {fixed, FixedLength};
            ScalarWireTypeInner when is_atom(ScalarWireTypeInner) ->
                ScalarWireTypeInner
        end,
    case JsonScalarDecoderModule:decode_block_scalar(JsonScalarDecoder1, BlockKey, BlockScalarHint, JsonValue) of
        {ok, JsonScalarDecoder2, {string, StringValue}} when
            is_binary(StringValue) andalso BlockScalarHint =:= string
        ->
            ScalarValue = argo_scalar_value:string(StringValue),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, BlockValue};
        {ok, JsonScalarDecoder2, {boolean, BooleanValue}} when
            is_boolean(BooleanValue) andalso BlockScalarHint =:= boolean
        ->
            ScalarValue = argo_scalar_value:boolean(BooleanValue),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, BlockValue};
        {ok, JsonScalarDecoder2, {varint, VarintValue}} when ?is_i64(VarintValue) andalso BlockScalarHint =:= varint ->
            ScalarValue = argo_scalar_value:varint(VarintValue),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, BlockValue};
        {ok, JsonScalarDecoder2, {float64, Float64Value}} when
            is_float(Float64Value) andalso BlockScalarHint =:= float64
        ->
            ScalarValue = argo_scalar_value:float64(Float64Value),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, BlockValue};
        {ok, JsonScalarDecoder2, {bytes, BytesValue}} when is_binary(BytesValue) andalso BlockScalarHint =:= bytes ->
            ScalarValue = argo_scalar_value:bytes(BytesValue),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, BlockValue};
        {ok, JsonScalarDecoder2, {fixed, FixedValue}} when
            is_binary(FixedValue) andalso element(1, BlockScalarHint) =:= fixed andalso
                element(2, BlockScalarHint) =:= byte_size(FixedValue)
        ->
            ScalarValue = argo_scalar_value:fixed(FixedValue),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
            {JsonValueDecoder2, BlockValue};
        {ok, _JsonScalarDecoder2, BlockScalar} ->
            error_scalar_type_mismatch([JsonValueDecoder1, BlockWireType, JsonValue], BlockScalarHint, BlockScalar);
        {error, type_mismatch} ->
            error_scalar_type_mismatch(
                [JsonValueDecoder1, BlockWireType, JsonValue], BlockScalarHint, {json, JsonValue}
            );
        {error, _} ->
            error_with_info(badarg, [JsonValueDecoder1, BlockWireType, JsonValue], #{
                3 => {failed_to_decode_scalar, JsonValue}
            })
    end.

-spec decode_nullable_wire_type(JsonValueDecoder, NullableWireType, JsonValue) -> {JsonValueDecoder, NullableValue} when
    JsonValueDecoder :: t(),
    NullableWireType :: argo_nullable_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    NullableValue :: argo_nullable_value:t().
decode_nullable_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{}, NullableWireType = #argo_nullable_wire_type{}, JsonValue
) ->
    case JsonValue of
        null ->
            NullableValue = argo_nullable_value:null(NullableWireType),
            {JsonValueDecoder1, NullableValue};
        _ ->
            {JsonValueDecoder2, Value} = decode_wire_type(
                JsonValueDecoder1, NullableWireType#argo_nullable_wire_type.'of', JsonValue
            ),
            NullableValue = argo_nullable_value:non_null(NullableWireType, Value),
            {JsonValueDecoder2, NullableValue}
    end.

-spec decode_array_wire_type(JsonValueDecoder, ArrayWireType, JsonValue) -> {JsonValueDecoder, ArrayValue} when
    JsonValueDecoder :: t(),
    ArrayWireType :: argo_array_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    ArrayValue :: argo_array_value:t().
decode_array_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{}, ArrayWireType = #argo_array_wire_type{}, JsonValue
) ->
    case is_list(JsonValue) of
        false ->
            error_with_info(badarg, [JsonValueDecoder1, ArrayWireType, JsonValue], #{
                3 => {mismatch, expected_array, JsonValue}
            });
        true ->
            {JsonValueDecoder2, Items} = lists:foldl(
                fun(JsonVal, {JsonValueDecoderAcc1, Items1}) ->
                    {JsonValueDecoderAcc2, Item} = decode_wire_type(
                        JsonValueDecoderAcc1, ArrayWireType#argo_array_wire_type.'of', JsonVal
                    ),
                    {JsonValueDecoderAcc2, [Item | Items1]}
                end,
                {JsonValueDecoder1, []},
                JsonValue
            ),
            ArrayValue = argo_array_value:new(ArrayWireType, lists:reverse(Items)),
            {JsonValueDecoder2, ArrayValue}
    end.

-spec decode_record_wire_type(JsonValueDecoder, RecordWireType, JsonValue) -> {JsonValueDecoder, RecordValue} when
    JsonValueDecoder :: t(),
    RecordWireType :: argo_record_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    RecordValue :: argo_record_value:t().
decode_record_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{}, RecordWireType = #argo_record_wire_type{}, JsonValue
) ->
    case ?is_json_object(JsonValue) of
        false ->
            error_with_info(badarg, [JsonValueDecoder1, RecordWireType, JsonValue], #{
                3 => {mismatch, expected_object, JsonValue}
            });
        true ->
            JsonObject = argo_types:dynamic_cast(JsonValue),
            {JsonValueDecoder2, RecordValue} = argo_index_map:foldl(
                fun(_Index, _FieldName, FieldWireType, {JsonValueDecoderAcc1, RecordValueAcc1}) ->
                    {JsonValueDecoderAcc2, FieldValue} = decode_field_wire_type(
                        JsonValueDecoderAcc1, FieldWireType, JsonObject
                    ),
                    RecordValueAcc2 = argo_record_value:insert(RecordValueAcc1, FieldValue),
                    {JsonValueDecoderAcc2, RecordValueAcc2}
                end,
                {JsonValueDecoder1, argo_record_value:new()},
                RecordWireType#argo_record_wire_type.fields
            ),
            {JsonValueDecoder2, RecordValue}
    end.

-spec decode_field_wire_type(JsonValueDecoder, FieldWireType, JsonObject) -> {JsonValueDecoder, FieldValue} when
    JsonValueDecoder :: t(),
    FieldWireType :: argo_field_wire_type:t(),
    JsonObject :: argo_json:json_object(),
    FieldValue :: argo_field_value:t().
decode_field_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{}, FieldWireType = #argo_field_wire_type{name = Name}, JsonObject
) when ?is_json_object(JsonObject) ->
    case FieldWireType#argo_field_wire_type.omittable of
        false ->
            case argo_json:object_find(Name, JsonObject) of
                {ok, JsonValue} ->
                    {JsonValueDecoder2, Value} = decode_wire_type(
                        JsonValueDecoder1, FieldWireType#argo_field_wire_type.'of', JsonValue
                    ),
                    FieldValue = argo_field_value:required(FieldWireType, Value),
                    {JsonValueDecoder2, FieldValue};
                error ->
                    error_with_info(badarg, [JsonValueDecoder1, FieldWireType, JsonObject], #{
                        3 => {required_object_key_missing, Name}
                    })
            end;
        true ->
            case argo_json:object_find(Name, JsonObject) of
                {ok, JsonValue} ->
                    {JsonValueDecoder2, Value} = decode_wire_type(
                        JsonValueDecoder1, FieldWireType#argo_field_wire_type.'of', JsonValue
                    ),
                    FieldValue = argo_field_value:optional(FieldWireType, {some, Value}),
                    {JsonValueDecoder2, FieldValue};
                error ->
                    FieldValue = argo_field_value:optional(FieldWireType, none),
                    {JsonValueDecoder1, FieldValue}
            end
    end.

-spec decode_desc_wire_type(JsonValueDecoder, JsonValue) -> {JsonValueDecoder, DescValue} when
    JsonValueDecoder :: t(), JsonValue :: argo_json:json_value(), DescValue :: argo_desc_value:t().
decode_desc_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{scalar_decoder = {JsonScalarDecoderModule, JsonScalarDecoder1}},
    JsonValue
) ->
    DescHint =
        case JsonValue of
            null ->
                null;
            _ when is_boolean(JsonValue) ->
                boolean;
            _ when ?is_i64(JsonValue) ->
                int;
            _ when is_float(JsonValue) ->
                float;
            _ when is_binary(JsonValue) ->
                string;
            _ when is_list(JsonValue) ->
                list;
            _ when ?is_json_object(JsonValue) ->
                object
        end,
    case DescHint of
        list ->
            {JsonValueDecoder2, Items} = lists:foldl(
                fun(JsonVal, {JsonValueDecoderAcc1, Items1}) ->
                    {JsonValueDecoderAcc2, Item} = decode_desc_wire_type(JsonValueDecoderAcc1, JsonVal),
                    {JsonValueDecoderAcc2, [Item | Items1]}
                end,
                {JsonValueDecoder1, []},
                argo_types:dynamic_cast(JsonValue)
            ),
            DescValue = argo_desc_value:list(lists:reverse(Items)),
            {JsonValueDecoder2, DescValue};
        object ->
            {JsonValueDecoder2, Object1} = argo_json:object_fold(
                fun(JsonKey, JsonVal, {JsonValueDecoderAcc1, ObjectAcc1}) when is_binary(JsonKey) ->
                    {JsonValueDecoderAcc2, Val} = decode_desc_wire_type(JsonValueDecoderAcc1, JsonVal),
                    ObjectAcc2 = argo_index_map:put(JsonKey, Val, ObjectAcc1),
                    {JsonValueDecoderAcc2, ObjectAcc2}
                end,
                {JsonValueDecoder1, argo_index_map:new()},
                argo_types:dynamic_cast(JsonValue)
            ),
            DescValue = argo_desc_value:object(Object1),
            {JsonValueDecoder2, DescValue};
        _ ->
            case JsonScalarDecoderModule:decode_desc_scalar(JsonScalarDecoder1, DescHint, JsonValue) of
                {ok, JsonScalarDecoder2, null} ->
                    DescValue = argo_desc_value:null(),
                    JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
                    {JsonValueDecoder2, DescValue};
                {ok, JsonScalarDecoder2, {boolean, BooleanValue}} when is_boolean(BooleanValue) ->
                    DescValue = argo_desc_value:boolean(BooleanValue),
                    JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
                    {JsonValueDecoder2, DescValue};
                {ok, JsonScalarDecoder2, {int, IntValue}} when ?is_i64(IntValue) ->
                    DescValue = argo_desc_value:int(IntValue),
                    JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
                    {JsonValueDecoder2, DescValue};
                {ok, JsonScalarDecoder2, {float, FloatValue}} when is_float(FloatValue) ->
                    DescValue = argo_desc_value:float(FloatValue),
                    JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
                    {JsonValueDecoder2, DescValue};
                {ok, JsonScalarDecoder2, {bytes, BytesValue}} when is_binary(BytesValue) ->
                    DescValue = argo_desc_value:bytes(BytesValue),
                    JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
                    {JsonValueDecoder2, DescValue};
                {ok, JsonScalarDecoder2, {string, StringValue}} when is_binary(StringValue) ->
                    DescValue = argo_desc_value:string(StringValue),
                    JsonValueDecoder2 = maybe_update_json_scalar_decoder(JsonValueDecoder1, JsonScalarDecoder2),
                    {JsonValueDecoder2, DescValue};
                {error, _} ->
                    error_with_info(badarg, [JsonValueDecoder1, JsonValue], #{3 => {failed_to_decode_scalar, JsonValue}})
            end
    end.

-spec decode_error_wire_type(JsonValueDecoder, JsonValue) -> {JsonValueDecoder, ErrorValue} when
    JsonValueDecoder :: t(), JsonValue :: argo_json:json_value(), ErrorValue :: argo_error_value:t().
decode_error_wire_type(JsonValueDecoder1 = #argo_json_value_decoder{}, JsonObject) when ?is_json_object(JsonObject) ->
    Message =
        case argo_json:object_find(<<"message">>, JsonObject) of
            {ok, MessageString} when is_binary(MessageString) ->
                MessageString;
            {ok, MessageActual} ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {mismatch, expected_string, MessageActual}
                });
            error ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {required_object_key_missing, <<"message">>}
                })
        end,
    {JsonValueDecoder2, Location} =
        case argo_json:object_find(<<"location">>, JsonObject) of
            {ok, LocationList} when is_list(LocationList) ->
                Dec1_1 = JsonValueDecoder1,
                {Dec1_2, LocationAcc} = lists:foldl(
                    fun(JsonVal, {Dec1_Acc1, LocationAcc1}) ->
                        {Dec1_Acc2, LocationValue} = decode_error_wire_type_location(Dec1_Acc1, JsonVal),
                        LocationAcc2 = [LocationValue | LocationAcc1],
                        {Dec1_Acc2, LocationAcc2}
                    end,
                    {Dec1_1, []},
                    LocationList
                ),
                {Dec1_2, {some, lists:reverse(LocationAcc)}};
            {ok, LocationActual} ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {mismatch, expected_array, LocationActual}
                });
            error ->
                {JsonValueDecoder1, none}
        end,
    {JsonValueDecoder3, Path} =
        case argo_json:object_find(<<"path">>, JsonObject) of
            {ok, PathList} when is_list(PathList) ->
                Dec2_1 = JsonValueDecoder2,
                {Dec2_2, PathValue} = decode_path_wire_type(Dec2_1, PathList),
                {Dec2_2, {some, PathValue}};
            {ok, PathActual} ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{2 => {mismatch, expected_array, PathActual}});
            error ->
                {JsonValueDecoder2, none}
        end,
    {JsonValueDecoder4, Extensions} =
        case argo_json:object_find(<<"extensions">>, JsonObject) of
            {ok, ExtensionsObject} when ?is_json_object(ExtensionsObject) ->
                Dec3_1 = JsonValueDecoder3,
                {Dec3_2, #argo_desc_value{inner = {object, ExtensionsValue}}} = decode_desc_wire_type(
                    Dec3_1, ExtensionsObject
                ),
                {Dec3_2, {some, ExtensionsValue}};
            {ok, ExtensionsActual} ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {mismatch, expected_object, ExtensionsActual}
                });
            error ->
                {JsonValueDecoder3, none}
        end,
    ErrorValue = argo_error_value:new(Message, Location, Path, Extensions),
    {JsonValueDecoder4, ErrorValue};
decode_error_wire_type(JsonValueDecoder1 = #argo_json_value_decoder{}, JsonValue) ->
    error_with_info(badarg, [JsonValueDecoder1, JsonValue], #{2 => {mismatch, expected_object, JsonValue}}).

-spec decode_error_wire_type_location(JsonValueDecoder, JsonValue) -> {JsonValueDecoder, LocationValue} when
    JsonValueDecoder :: t(), JsonValue :: argo_json:json_value(), LocationValue :: argo_location_value:t().
decode_error_wire_type_location(JsonValueDecoder1 = #argo_json_value_decoder{}, JsonObject) when
    ?is_json_object(JsonObject)
->
    Line =
        case argo_json:object_find(<<"line">>, JsonObject) of
            {ok, LineInt} when is_integer(LineInt) ->
                LineInt;
            {ok, LineActual} ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {mismatch, expected_integer, LineActual}
                });
            error ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {required_object_key_missing, <<"line">>}
                })
        end,
    Column =
        case argo_json:object_find(<<"column">>, JsonObject) of
            {ok, ColumnInt} when is_integer(ColumnInt) ->
                ColumnInt;
            {ok, ColumnActual} ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {mismatch, expected_integer, ColumnActual}
                });
            error ->
                error_with_info(badarg, [JsonValueDecoder1, JsonObject], #{
                    2 => {required_object_key_missing, <<"column">>}
                })
        end,
    LocationValue = argo_location_value:new(Line, Column),
    {JsonValueDecoder1, LocationValue};
decode_error_wire_type_location(JsonValueDecoder1 = #argo_json_value_decoder{}, JsonValue) ->
    error_with_info(badarg, [JsonValueDecoder1, JsonValue], #{2 => {mismatch, expected_object, JsonValue}}).

-spec decode_path_wire_type(JsonValueDecoder, JsonValue) -> {JsonValueDecoder, PathValue} when
    JsonValueDecoder :: t(), JsonValue :: argo_json:json_value(), PathValue :: argo_path_value:t().
decode_path_wire_type(JsonValueDecoder1 = #argo_json_value_decoder{}, JsonArray) when is_list(JsonArray) ->
    PathValue = lists:foldl(
        fun
            (FieldName, PathValueAcc) when is_binary(FieldName) ->
                argo_path_value:push_field_name(PathValueAcc, FieldName);
            (ListIndex, PathValueAcc) when is_integer(ListIndex) ->
                argo_path_value:push_list_index(PathValueAcc, ListIndex)
        end,
        argo_path_value:new(),
        JsonArray
    ),
    {JsonValueDecoder1, PathValue};
decode_path_wire_type(JsonValueDecoder1 = #argo_json_value_decoder{}, JsonValue) ->
    error_with_info(badarg, [JsonValueDecoder1, JsonValue], #{2 => {mismatch, expected_array, JsonValue}}).

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-compile({inline, [error_with_info/3]}).
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

%% @private
-compile({inline, [error_scalar_type_mismatch/3]}).
-spec error_scalar_type_mismatch(Args, ScalarHint, Scalar) -> no_return() when
    Args :: [dynamic()], ScalarHint :: argo_json_scalar_decoder:scalar_hint(), Scalar :: dynamic().
error_scalar_type_mismatch(Args, ScalarHint, Scalar) ->
    case ScalarHint of
        string ->
            error_with_info(badarg, Args, #{
                3 => {mismatch, expected_string, Scalar}
            });
        boolean ->
            error_with_info(badarg, Args, #{
                3 => {mismatch, expected_boolean, Scalar}
            });
        varint ->
            error_with_info(badarg, Args, #{
                3 => {mismatch, expected_integer, Scalar}
            });
        float64 ->
            error_with_info(badarg, Args, #{
                3 => {mismatch, expected_float, Scalar}
            });
        bytes ->
            error_with_info(badarg, Args, #{
                3 => {mismatch, expected_bytes, Scalar}
            });
        {fixed, Length} ->
            error_with_info(badarg, Args, #{
                3 => {mismatch, {expected_fixed, Length}, Scalar}
            })
    end.

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, expected_array) ->
    "expected JSON array";
format_error_description(_Key, expected_boolean) ->
    "expected JSON boolean";
format_error_description(_Key, expected_bytes) ->
    "expected JSON bytes";
format_error_description(_Key, {expected_fixed, Length}) ->
    io_lib:format("expected JSON string of fixed-length ~w", [Length]);
format_error_description(_Key, expected_float) ->
    "expected JSON number as float";
format_error_description(_Key, expected_integer) ->
    "expected JSON number as integer";
format_error_description(_Key, expected_object) ->
    "expected JSON object";
format_error_description(_Key, expected_string) ->
    "expected JSON string";
format_error_description(_Key, {failed_to_decode_scalar, JsonValue}) ->
    io_lib:format("failed to decode Scalar: ~0tP", [JsonValue, 5]);
format_error_description(Key, {mismatch, Expected, Actual}) ->
    io_lib:format("~ts, but was: ~0tp", [format_error_description(Key, Expected), Actual]);
format_error_description(_Key, {required_object_key_missing, Key}) ->
    io_lib:format("required JSON object key is missing: ~0tp", [Key]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-compile({inline, [maybe_update_json_scalar_decoder/2]}).
-spec maybe_update_json_scalar_decoder(JsonValueDecoder, JsonScalarDecoder) -> JsonValueDecoder when
    JsonValueDecoder :: t(), JsonScalarDecoder :: argo_json_scalar_decoder:t().
maybe_update_json_scalar_decoder(
    JsonValueDecoder1 = #argo_json_value_decoder{scalar_decoder = {_JsonScalarDecoderModule, JsonScalarDecoder1}},
    JsonScalarDecoder1
) ->
    JsonValueDecoder1;
maybe_update_json_scalar_decoder(
    JsonValueDecoder1 = #argo_json_value_decoder{scalar_decoder = {JsonScalarDecoderModule, _JsonScalarDecoder1}},
    JsonScalarDecoder2
) ->
    JsonValueDecoder2 = JsonValueDecoder1#argo_json_value_decoder{
        scalar_decoder = {JsonScalarDecoderModule, JsonScalarDecoder2}
    },
    JsonValueDecoder2.
