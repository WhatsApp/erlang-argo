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
-module(argo_json_value_encoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/0,
    encode_value/2
]).

%% Types
-type t() :: #argo_json_value_encoder{}.

-export_type([
    t/0
]).

%% Macros
-define(is_json_object(X), (is_map((X)) orelse is_record((X), argo_index_map))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> JsonValueEncoder when JsonValueEncoder :: t().
new() ->
    #argo_json_value_encoder{
        current_path = argo_path_value:new(), field_errors = argo_index_map:new(), response_errors = []
    }.

-spec encode_value(JsonValueEncoder, Value) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), Value :: argo_value:t(), JsonValue :: argo_json:json_value().
encode_value(JsonValueEncoder1 = #argo_json_value_encoder{}, Value = #argo_value{}) ->
    case Value#argo_value.inner of
        ScalarValue = #argo_scalar_value{} ->
            encode_scalar_value(JsonValueEncoder1, ScalarValue);
        BlockValue = #argo_block_value{} ->
            encode_block_value(JsonValueEncoder1, BlockValue);
        NullableValue = #argo_nullable_value{} ->
            encode_nullable_value(JsonValueEncoder1, NullableValue);
        ArrayValue = #argo_array_value{} ->
            encode_array_value(JsonValueEncoder1, ArrayValue);
        RecordValue = #argo_record_value{} ->
            encode_record_value(JsonValueEncoder1, RecordValue);
        DescValue = #argo_desc_value{} ->
            encode_desc_value(JsonValueEncoder1, DescValue);
        ErrorValue = #argo_error_value{} ->
            encode_error_value(JsonValueEncoder1, ErrorValue);
        PathValue = #argo_path_value{} ->
            encode_path_value(JsonValueEncoder1, PathValue)
    end.

-spec encode_scalar_value(JsonValueEncoder, ScalarValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), ScalarValue :: argo_scalar_value:t(), JsonValue :: argo_json:json_value().
encode_scalar_value(JsonValueEncoder1 = #argo_json_value_encoder{}, ScalarValue = #argo_scalar_value{}) ->
    case ScalarValue#argo_scalar_value.inner of
        {string, JsonValue} when is_binary(JsonValue) ->
            {JsonValueEncoder1, JsonValue};
        {boolean, JsonValue} when is_boolean(JsonValue) ->
            {JsonValueEncoder1, JsonValue};
        {varint, JsonValue} when ?is_i64(JsonValue) ->
            {JsonValueEncoder1, JsonValue};
        {float64, JsonValue} when is_float(JsonValue) ->
            {JsonValueEncoder1, JsonValue};
        {bytes, JsonValue} when is_binary(JsonValue) ->
            {JsonValueEncoder1, JsonValue};
        {fixed, JsonValue} when is_binary(JsonValue) ->
            {JsonValueEncoder1, JsonValue}
    end.

-spec encode_block_value(JsonValueEncoder, BlockValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), BlockValue :: argo_block_value:t(), JsonValue :: argo_json:json_value().
encode_block_value(JsonValueEncoder1 = #argo_json_value_encoder{}, BlockValue = #argo_block_value{}) ->
    {JsonValueEncoder2, JsonValue} = encode_scalar_value(JsonValueEncoder1, BlockValue#argo_block_value.value),
    {JsonValueEncoder2, JsonValue}.

-spec encode_nullable_value(JsonValueEncoder, NullableValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), NullableValue :: argo_nullable_value:t(), JsonValue :: argo_json:json_value().
encode_nullable_value(JsonValueEncoder1 = #argo_json_value_encoder{}, NullableValue = #argo_nullable_value{}) ->
    case NullableValue#argo_nullable_value.inner of
        null ->
            JsonValue = argo_json:null(),
            {JsonValueEncoder1, JsonValue};
        {non_null, Value} ->
            {JsonValueEncoder2, JsonValue} = encode_value(JsonValueEncoder1, Value),
            {JsonValueEncoder2, JsonValue};
        {field_errors, _} ->
            JsonValue = argo_json:null(),
            {JsonValueEncoder1, JsonValue}
    end.

-spec encode_array_value(JsonValueEncoder, ArrayValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), ArrayValue :: argo_array_value:t(), JsonValue :: argo_json:json_value().
encode_array_value(JsonValueEncoder1 = #argo_json_value_encoder{}, ArrayValue = #argo_array_value{}) ->
    {JsonValueEncoder2, List1} = lists:foldl(
        fun(Value, {JsonValueEncoderAcc1, ListAcc1}) ->
            {JsonValueEncoderAcc2, JsonVal} = encode_value(JsonValueEncoderAcc1, Value),
            {JsonValueEncoderAcc2, [JsonVal | ListAcc1]}
        end,
        {JsonValueEncoder1, []},
        ArrayValue#argo_array_value.items
    ),
    JsonValue = lists:reverse(List1),
    {JsonValueEncoder2, JsonValue}.

-spec encode_record_value(JsonValueEncoder, RecordValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), RecordValue :: argo_record_value:t(), JsonValue :: argo_json:json_value().
encode_record_value(JsonValueEncoder1 = #argo_json_value_encoder{}, RecordValue = #argo_record_value{}) ->
    {JsonValueEncoder2, JsonValue} = argo_index_map:foldl(
        fun(_Index, FieldName, FieldValue, {JsonValueEncoderAcc1, JsonObjectAcc1}) ->
            case FieldValue#argo_field_value.inner of
                {optional, none} ->
                    {JsonValueEncoderAcc1, JsonObjectAcc1};
                {optional, {some, Value}} ->
                    {JsonValueEncoderAcc2, JsonValue} = encode_value(JsonValueEncoderAcc1, Value),
                    JsonObjectAcc2 = argo_index_map:put(FieldName, JsonValue, JsonObjectAcc1),
                    {JsonValueEncoderAcc2, JsonObjectAcc2};
                {required, Value} ->
                    {JsonValueEncoderAcc2, JsonValue} = encode_value(JsonValueEncoderAcc1, Value),
                    JsonObjectAcc2 = argo_index_map:put(FieldName, JsonValue, JsonObjectAcc1),
                    {JsonValueEncoderAcc2, JsonObjectAcc2}
            end
        end,
        {JsonValueEncoder1, argo_index_map:new()},
        RecordValue#argo_record_value.fields
    ),
    {JsonValueEncoder2, {argo_index_map:to_list(JsonValue)}}.

-spec encode_desc_value(JsonValueEncoder, DescValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), DescValue :: argo_desc_value:t(), JsonValue :: argo_json:json_value().
encode_desc_value(JsonValueEncoder1 = #argo_json_value_encoder{}, DescValue = #argo_desc_value{}) ->
    case DescValue#argo_desc_value.inner of
        null ->
            JsonValue = argo_json:null(),
            {JsonValueEncoder1, JsonValue};
        {boolean, V} when is_boolean(V) ->
            JsonValue = argo_json:boolean(V),
            {JsonValueEncoder1, JsonValue};
        {object, Object = #argo_index_map{}} ->
            {JsonValueEncoder2, JsonValue} = argo_index_map:foldl(
                fun(_Index, Key, Val, {JsonValueEncoderAcc1, ObjectAcc1}) ->
                    JsonKey = argo_json:string(Key),
                    {JsonValueEncoderAcc2, JsonVal} = encode_desc_value(JsonValueEncoderAcc1, Val),
                    ObjectAcc2 = argo_index_map:put(JsonKey, JsonVal, ObjectAcc1),
                    {JsonValueEncoderAcc2, ObjectAcc2}
                end,
                {JsonValueEncoder1, argo_index_map:new()},
                Object
            ),
            {JsonValueEncoder2, {argo_index_map:to_list(JsonValue)}};
        {list, List} when is_list(List) ->
            {JsonValueEncoder2, ArrayAcc} = lists:foldl(
                fun(Val, {JsonValueEncoderAcc1, ArrayAcc1}) ->
                    {JsonValueEncoderAcc2, JsonVal} = encode_desc_value(JsonValueEncoderAcc1, Val),
                    ArrayAcc2 = [JsonVal | ArrayAcc1],
                    {JsonValueEncoderAcc2, ArrayAcc2}
                end,
                {JsonValueEncoder1, []},
                List
            ),
            JsonValue = lists:reverse(ArrayAcc),
            {JsonValueEncoder2, JsonValue};
        {string, V} when is_binary(V) ->
            JsonValue = argo_json:string(V),
            {JsonValueEncoder1, JsonValue};
        {bytes, V} when is_binary(V) ->
            JsonValue = argo_json:string(V),
            {JsonValueEncoder1, JsonValue};
        {int, V} when ?is_i64(V) ->
            JsonValue = argo_json:number(V),
            {JsonValueEncoder1, JsonValue};
        {float, V} when is_float(V) ->
            JsonValue = argo_json:number(V),
            {JsonValueEncoder1, JsonValue}
    end.

-spec encode_error_value(JsonValueEncoder, ErrorValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), ErrorValue :: argo_error_value:t(), JsonValue :: argo_json:json_value().
encode_error_value(JsonValueEncoder1 = #argo_json_value_encoder{}, ErrorValue = #argo_error_value{}) ->
    JsonObject1 = argo_index_map:new(),
    JsonObject2 = argo_index_map:put(<<"message">>, argo_json:string(ErrorValue#argo_error_value.message), JsonObject1),
    {JsonValueEncoder2, JsonObject3} =
        case ErrorValue#argo_error_value.location of
            none ->
                {JsonValueEncoder1, JsonObject2};
            {some, Location} ->
                Enc1_1 = JsonValueEncoder1,
                {Enc1_2, JsonLocation} = lists:foldl(
                    fun(LocationValue, {Enc1_Acc1, LocationAcc1}) ->
                        {Enc1_Acc2, JsonLocation} = encode_location_value(Enc1_Acc1, LocationValue),
                        {Enc1_Acc2, [JsonLocation | LocationAcc1]}
                    end,
                    {Enc1_1, []},
                    Location
                ),
                {Enc1_2, argo_index_map:put(<<"location">>, lists:reverse(JsonLocation), JsonObject2)}
        end,
    {JsonValueEncoder3, JsonObject4} =
        case ErrorValue#argo_error_value.path of
            none ->
                {JsonValueEncoder2, JsonObject3};
            {some, PathValue} ->
                Enc2_1 = JsonValueEncoder2,
                {Enc2_2, JsonPath} = encode_path_value(Enc2_1, PathValue),
                {Enc2_2, argo_index_map:put(<<"path">>, JsonPath, JsonObject3)}
        end,
    {JsonValueEncoder4, JsonObject5} =
        case ErrorValue#argo_error_value.extensions of
            none ->
                {JsonValueEncoder3, JsonObject3};
            {some, Extensions} when ?is_json_object(Extensions) ->
                Enc3_1 = JsonValueEncoder3,
                {Enc3_2, JsonExtensions} = encode_desc_value(Enc3_1, argo_desc_value:object(Extensions)),
                {Enc3_2, argo_index_map:put(<<"extensions">>, JsonExtensions, JsonObject4)}
        end,
    JsonValue = JsonObject5,
    {JsonValueEncoder4, {argo_index_map:to_list(JsonValue)}}.

-spec encode_location_value(JsonValueEncoder, LocationValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), LocationValue :: argo_location_value:t(), JsonValue :: argo_json:json_value().
encode_location_value(JsonValueEncoder1 = #argo_json_value_encoder{}, LocationValue = #argo_location_value{}) ->
    JsonObject1 = argo_index_map:new(),
    JsonObject2 = argo_index_map:put(<<"line">>, argo_json:number(LocationValue#argo_location_value.line), JsonObject1),
    JsonObject3 = argo_index_map:put(
        <<"column">>, argo_json:number(LocationValue#argo_location_value.column), JsonObject2
    ),
    JsonValue = JsonObject3,
    {JsonValueEncoder1, {argo_index_map:to_list(JsonValue)}}.

-spec encode_path_value(JsonValueEncoder, PathValue) -> {JsonValueEncoder, JsonValue} when
    JsonValueEncoder :: t(), PathValue :: argo_path_value:t(), JsonValue :: argo_json:json_value().
encode_path_value(JsonValueEncoder1 = #argo_json_value_encoder{}, PathValue = #argo_path_value{}) ->
    JsonValue = argo_path_value:to_list(PathValue),
    {JsonValueEncoder1, JsonValue}.
