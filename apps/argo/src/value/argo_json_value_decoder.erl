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

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/0,
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
    #argo_json_value_decoder{
        current_path = argo_path_value:new(), field_errors = argo_index_map:new(), response_errors = []
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
    JsonValueDecoder1 = #argo_json_value_decoder{}, ScalarWireType = #argo_scalar_wire_type{}, JsonValue
) ->
    case ScalarWireType#argo_scalar_wire_type.inner of
        string when is_binary(JsonValue) ->
            ScalarValue = argo_scalar_value:string(JsonValue),
            {JsonValueDecoder1, ScalarValue};
        string ->
            error_with_info(badarg, [JsonValueDecoder1, ScalarWireType, JsonValue], #{
                3 => {mismatch, expected_string, JsonValue}
            });
        boolean when is_boolean(JsonValue) ->
            ScalarValue = argo_scalar_value:boolean(JsonValue),
            {JsonValueDecoder1, ScalarValue};
        boolean ->
            error_with_info(badarg, [JsonValueDecoder1, ScalarWireType, JsonValue], #{
                3 => {mismatch, expected_boolean, JsonValue}
            });
        varint when ?is_i64(JsonValue) ->
            ScalarValue = argo_scalar_value:varint(JsonValue),
            {JsonValueDecoder1, ScalarValue};
        varint ->
            error_with_info(badarg, [JsonValueDecoder1, ScalarWireType, JsonValue], #{
                3 => {mismatch, expected_integer, JsonValue}
            });
        float64 when is_float(JsonValue) ->
            ScalarValue = argo_scalar_value:float64(JsonValue),
            {JsonValueDecoder1, ScalarValue};
        float64 ->
            error_with_info(badarg, [JsonValueDecoder1, ScalarWireType, JsonValue], #{
                3 => {mismatch, expected_float, JsonValue}
            });
        bytes when is_binary(JsonValue) ->
            ScalarValue = argo_scalar_value:bytes(JsonValue),
            {JsonValueDecoder1, ScalarValue};
        bytes ->
            error_with_info(badarg, [JsonValueDecoder1, ScalarWireType, JsonValue], #{
                3 => {mismatch, expected_string, JsonValue}
            });
        #argo_fixed_wire_type{length = Length} when is_binary(JsonValue) andalso byte_size(JsonValue) =:= Length ->
            ScalarValue = argo_scalar_value:fixed(JsonValue),
            {JsonValueDecoder1, ScalarValue};
        #argo_fixed_wire_type{length = Length} ->
            error_with_info(badarg, [JsonValueDecoder1, ScalarWireType, JsonValue], #{
                3 => {mismatch, {expected_fixed, Length}, JsonValue}
            })
    end.

-spec decode_block_wire_type(JsonValueDecoder, BlockWireType, JsonValue) -> {JsonValueDecoder, BlockValue} when
    JsonValueDecoder :: t(),
    BlockWireType :: argo_block_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    BlockValue :: argo_block_value:t().
decode_block_wire_type(
    JsonValueDecoder1 = #argo_json_value_decoder{}, BlockWireType = #argo_block_wire_type{}, JsonValue
) ->
    {JsonValueDecoder2, ScalarValue} = decode_scalar_wire_type(
        JsonValueDecoder1, BlockWireType#argo_block_wire_type.'of', JsonValue
    ),
    BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
    {JsonValueDecoder2, BlockValue}.

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
            {JsonValueDecoder2, RecordValue} = argo_index_map:foldl(
                fun(_Index, _FieldName, FieldWireType, {JsonValueDecoderAcc1, RecordValueAcc1}) ->
                    {JsonValueDecoderAcc2, FieldValue} = decode_field_wire_type(
                        JsonValueDecoderAcc1, FieldWireType, JsonValue
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
decode_desc_wire_type(JsonValueDecoder1 = #argo_json_value_decoder{}, JsonValue) ->
    case JsonValue of
        null ->
            DescValue = argo_desc_value:null(),
            {JsonValueDecoder1, DescValue};
        false ->
            DescValue = argo_desc_value:boolean(false),
            {JsonValueDecoder1, DescValue};
        true ->
            DescValue = argo_desc_value:boolean(true),
            {JsonValueDecoder1, DescValue};
        _ when ?is_i64(JsonValue) ->
            DescValue = argo_desc_value:int(JsonValue),
            {JsonValueDecoder1, DescValue};
        _ when is_float(JsonValue) ->
            DescValue = argo_desc_value:float(JsonValue),
            {JsonValueDecoder1, DescValue};
        _ when is_binary(JsonValue) ->
            DescValue = argo_desc_value:string(JsonValue),
            {JsonValueDecoder1, DescValue};
        _ when is_list(JsonValue) ->
            {JsonValueDecoder2, Items} = lists:foldl(
                fun(JsonVal, {JsonValueDecoderAcc1, Items1}) ->
                    {JsonValueDecoderAcc2, Item} = decode_desc_wire_type(JsonValueDecoderAcc1, JsonVal),
                    {JsonValueDecoderAcc2, [Item | Items1]}
                end,
                {JsonValueDecoder1, []},
                JsonValue
            ),
            DescValue = argo_desc_value:list(lists:reverse(Items)),
            {JsonValueDecoder2, DescValue};
        _ when ?is_json_object(JsonValue) ->
            {JsonValueDecoder2, Object1} = argo_json:object_fold(
                fun(JsonKey, JsonVal, {JsonValueDecoderAcc1, ObjectAcc1}) when is_binary(JsonKey) ->
                    {JsonValueDecoderAcc2, Val} = decode_desc_wire_type(JsonValueDecoderAcc1, JsonVal),
                    ObjectAcc2 = argo_index_map:put(JsonKey, Val, ObjectAcc1),
                    {JsonValueDecoderAcc2, ObjectAcc2}
                end,
                {JsonValueDecoder1, argo_index_map:new()},
                JsonValue
            ),
            DescValue = argo_desc_value:object(Object1),
            {JsonValueDecoder2, DescValue}
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
format_error_description(_Key, expected_array) ->
    "expected JSON array";
format_error_description(_Key, expected_boolean) ->
    "expected JSON boolean";
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
format_error_description(Key, {mismatch, Expected, Actual}) ->
    io_lib:format("~ts, but was: ~0tp", [format_error_description(Key, Expected), Actual]);
format_error_description(_Key, {required_object_key_missing, Key}) ->
    io_lib:format("required JSON object key is missing: ~0tp", [Key]);
format_error_description(_Key, Value) ->
    Value.
