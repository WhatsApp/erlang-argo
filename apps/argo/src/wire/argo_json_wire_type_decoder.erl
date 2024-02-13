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
%%% Created :  26 Jan 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_json_wire_type_decoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/0,
    decode_wire_type/2,
    decode_wire_type_store/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_json_wire_type_decoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> JsonWireTypeDecoder when JsonWireTypeDecoder :: t().
new() ->
    #argo_json_wire_type_decoder{}.

-spec decode_wire_type(JsonWireTypeDecoder, JsonValue) -> {JsonWireTypeDecoder, WireType} when
    JsonWireTypeDecoder :: t(), JsonValue :: argo_json:json_value(), WireType :: argo_wire_type:t().
decode_wire_type(JsonWireTypeDecoder1 = #argo_json_wire_type_decoder{}, JsonValue) ->
    JsonObject = argo_json:as_object(JsonValue),
    case argo_json:object_get(<<"type">>, JsonObject) of
        <<"STRING">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:string(),
            WireType = argo_wire_type:scalar(ScalarWireType),
            {JsonWireTypeDecoder1, WireType};
        <<"BOOLEAN">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:boolean(),
            WireType = argo_wire_type:scalar(ScalarWireType),
            {JsonWireTypeDecoder1, WireType};
        <<"VARINT">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:varint(),
            WireType = argo_wire_type:scalar(ScalarWireType),
            {JsonWireTypeDecoder1, WireType};
        <<"FLOAT64">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:float64(),
            WireType = argo_wire_type:scalar(ScalarWireType),
            {JsonWireTypeDecoder1, WireType};
        <<"BYTES">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:bytes(),
            WireType = argo_wire_type:scalar(ScalarWireType),
            {JsonWireTypeDecoder1, WireType};
        <<"FIXED">> ->
            case argo_json:as_number(argo_json:object_get(<<"length">>, JsonObject)) of
                Length when is_integer(Length) andalso Length >= 0 ->
                    ok = check_for_unknown_keys(JsonObject, #{<<"type">> => [], <<"length">> => []}),
                    ScalarWireType = argo_scalar_wire_type:fixed(Length),
                    WireType = argo_wire_type:scalar(ScalarWireType),
                    {JsonWireTypeDecoder1, WireType};
                JsonLengthNumber ->
                    error_with_info(badarg, [JsonWireTypeDecoder1, JsonValue], #{
                        2 => {invalid_length, JsonLengthNumber}
                    })
            end;
        <<"BLOCK">> ->
            JsonOf = argo_json:object_get(<<"of">>, JsonObject),
            {JsonWireTypeDecoder2, Of} = decode_scalar_wire_type(JsonWireTypeDecoder1, JsonOf),
            Key = argo_json:as_string(argo_json:object_get(<<"key">>, JsonObject)),
            Dedupe = argo_json:as_boolean(argo_json:object_get(<<"dedupe">>, JsonObject)),
            ok = check_for_unknown_keys(JsonObject, #{
                <<"type">> => [], <<"of">> => [], <<"key">> => [], <<"dedupe">> => []
            }),
            BlockWireType = argo_block_wire_type:new(Of, Key, Dedupe),
            WireType = argo_wire_type:block(BlockWireType),
            {JsonWireTypeDecoder2, WireType};
        <<"NULLABLE">> ->
            JsonOf = argo_json:object_get(<<"of">>, JsonObject),
            {JsonWireTypeDecoder2, Of} = decode_wire_type(JsonWireTypeDecoder1, JsonOf),
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => [], <<"of">> => []}),
            NullableWireType = argo_nullable_wire_type:new(Of),
            WireType = argo_wire_type:nullable(NullableWireType),
            {JsonWireTypeDecoder2, WireType};
        <<"ARRAY">> ->
            JsonOf = argo_json:object_get(<<"of">>, JsonObject),
            {JsonWireTypeDecoder2, Of} = decode_wire_type(JsonWireTypeDecoder1, JsonOf),
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => [], <<"of">> => []}),
            ArrayWireType = argo_array_wire_type:new(Of),
            WireType = argo_wire_type:array(ArrayWireType),
            {JsonWireTypeDecoder2, WireType};
        <<"RECORD">> ->
            JsonFields = argo_json:as_array(argo_json:object_get(<<"fields">>, JsonObject)),
            RecordWireType1 = argo_record_wire_type:new(),
            {JsonWireTypeDecoder2, RecordWireType2} = lists:foldl(
                fun(JsonField, {JsonWireTypeDecoder1_Acc1, RecordWireType1_Acc1}) ->
                    {JsonWireTypeDecoder1_Acc2, FieldWireType} = decode_field_wire_type(
                        JsonWireTypeDecoder1_Acc1, JsonField
                    ),
                    RecordWireType1_Acc2 = argo_record_wire_type:insert(
                        RecordWireType1_Acc1, FieldWireType
                    ),
                    {JsonWireTypeDecoder1_Acc2, RecordWireType1_Acc2}
                end,
                {JsonWireTypeDecoder1, RecordWireType1},
                JsonFields
            ),
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => [], <<"fields">> => []}),
            WireType = argo_wire_type:record(RecordWireType2),
            {JsonWireTypeDecoder2, WireType};
        <<"DESC">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            WireType = argo_wire_type:desc(),
            {JsonWireTypeDecoder1, WireType};
        <<"ERROR">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            WireType = argo_wire_type:error(),
            {JsonWireTypeDecoder1, WireType};
        <<"EXTENSIONS">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            WireType = argo_wire_type:extensions(),
            {JsonWireTypeDecoder1, WireType};
        <<"PATH">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            WireType = argo_wire_type:path(),
            {JsonWireTypeDecoder1, WireType};
        JsonTypeString when is_binary(JsonTypeString) ->
            error_with_info(badarg, [JsonWireTypeDecoder1, JsonValue], #{2 => {invalid_type, JsonTypeString}})
    end.

-spec decode_wire_type_store(JsonWireTypeDecoder, JsonValue) -> {JsonWireTypeDecoder, WireTypeStore} when
    JsonWireTypeDecoder :: t(), JsonValue :: argo_json:json_value(), WireTypeStore :: argo_wire_type_store:t().
decode_wire_type_store(JsonWireTypeDecoder1 = #argo_json_wire_type_decoder{}, JsonValue) ->
    JsonObject = argo_json:as_object(JsonValue),
    case argo_json:as_string(argo_json:object_get(<<"type">>, JsonObject)) of
        <<"STORE">> ->
            JsonTypes = argo_json:as_array(argo_json:object_get(<<"types">>, JsonObject)),
            WireTypeStore1 = argo_wire_type_store:new(),
            {JsonWireTypeDecoder2, WireTypeStore2} = lists:foldl(
                fun(JsonField, {JsonWireTypeDecoder1_Acc1, WireTypeStore1_Acc1}) ->
                    {JsonWireTypeDecoder1_Acc2, TypeName, WireType} = decode_wire_type_store_entry(
                        JsonWireTypeDecoder1_Acc1, JsonField
                    ),
                    WireTypeStore1_Acc2 = argo_wire_type_store:insert(
                        WireTypeStore1_Acc1, TypeName, WireType
                    ),
                    {JsonWireTypeDecoder1_Acc2, WireTypeStore1_Acc2}
                end,
                {JsonWireTypeDecoder1, WireTypeStore1},
                JsonTypes
            ),
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => [], <<"types">> => []}),
            {JsonWireTypeDecoder2, WireTypeStore2};
        JsonTypeString when is_binary(JsonTypeString) ->
            error_with_info(badarg, [JsonWireTypeDecoder1, JsonValue], #{2 => {invalid_type, JsonTypeString}})
    end.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-compile({inline, [error_with_info/3]}).
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

%% @private
-spec check_for_unknown_keys(JsonObject, IgnoreKeys) -> ok when
    JsonObject :: argo_json:json_object(), IgnoreKeys :: #{binary() => []}.
check_for_unknown_keys(JsonObject, IgnoreKeys) when is_map(IgnoreKeys) ->
    UnknownKeys = argo_json:object_fold(
        fun(Key, _, UnknownKeysAcc) ->
            case maps:is_key(Key, IgnoreKeys) of
                false ->
                    maps:put(Key, [], UnknownKeysAcc);
                true ->
                    UnknownKeysAcc
            end
        end,
        maps:new(),
        JsonObject
    ),
    case maps:size(UnknownKeys) of
        0 ->
            ok;
        _ ->
            error_with_info(badarg, [JsonObject, IgnoreKeys], #{1 => {unknown_keys, lists:sort(maps:keys(UnknownKeys))}})
    end.

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, {invalid_length, JsonValue}) ->
    io_lib:format("invalid length key for JSON fixed wire type: ~0tP", [JsonValue, 5]);
format_error_description(_Key, {invalid_type, JsonValue}) ->
    io_lib:format("invalid type key for JSON wire type: ~0tP", [JsonValue, 5]);
format_error_description(_Key, {unknown_keys, UnknownKeys}) ->
    io_lib:format("unknown keys found in JSON object: ~0tP", [UnknownKeys, 5]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_scalar_wire_type(JsonWireTypeDecoder, JsonValue) -> {JsonWireTypeDecoder, ScalarWireType} when
    JsonWireTypeDecoder :: t(), JsonValue :: argo_json:json_value(), ScalarWireType :: argo_scalar_wire_type:t().
decode_scalar_wire_type(JsonWireTypeDecoder1 = #argo_json_wire_type_decoder{}, JsonValue) ->
    JsonObject = argo_json:as_object(JsonValue),
    case argo_json:as_string(argo_json:object_get(<<"type">>, JsonObject)) of
        <<"STRING">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:string(),
            {JsonWireTypeDecoder1, ScalarWireType};
        <<"BOOLEAN">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:boolean(),
            {JsonWireTypeDecoder1, ScalarWireType};
        <<"VARINT">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:varint(),
            {JsonWireTypeDecoder1, ScalarWireType};
        <<"FLOAT64">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:float64(),
            {JsonWireTypeDecoder1, ScalarWireType};
        <<"BYTES">> ->
            ok = check_for_unknown_keys(JsonObject, #{<<"type">> => []}),
            ScalarWireType = argo_scalar_wire_type:bytes(),
            {JsonWireTypeDecoder1, ScalarWireType};
        <<"FIXED">> ->
            case argo_json:as_number(argo_json:object_get(<<"length">>, JsonObject)) of
                Length when is_integer(Length) andalso Length >= 0 ->
                    ok = check_for_unknown_keys(JsonObject, #{<<"type">> => [], <<"length">> => []}),
                    ScalarWireType = argo_scalar_wire_type:fixed(Length),
                    {JsonWireTypeDecoder1, ScalarWireType};
                JsonLengthNumber ->
                    error_with_info(badarg, [JsonWireTypeDecoder1, JsonValue], #{
                        2 => {invalid_length, JsonLengthNumber}
                    })
            end;
        JsonTypeString when is_binary(JsonTypeString) ->
            error_with_info(badarg, [JsonWireTypeDecoder1, JsonValue], #{2 => {invalid_type, JsonTypeString}})
    end.

%% @private
-spec decode_field_wire_type(JsonWireTypeDecoder, JsonValue) -> {JsonWireTypeDecoder, FieldWireType} when
    JsonWireTypeDecoder :: t(), JsonValue :: argo_json:json_value(), FieldWireType :: argo_field_wire_type:t().
decode_field_wire_type(JsonWireTypeDecoder1 = #argo_json_wire_type_decoder{}, JsonValue) ->
    JsonObject = argo_json:as_object(JsonValue),
    Name = argo_json:as_string(argo_json:object_get(<<"name">>, JsonObject)),
    JsonOf = argo_json:object_get(<<"type">>, JsonObject),
    {JsonWireTypeDecoder2, Of} = decode_wire_type(JsonWireTypeDecoder1, JsonOf),
    Omittable = argo_json:as_boolean(argo_json:object_get(<<"omittable">>, JsonObject)),
    ok = check_for_unknown_keys(JsonObject, #{<<"name">> => [], <<"type">> => [], <<"omittable">> => []}),
    FieldWireType = argo_field_wire_type:new(Name, Of, Omittable),
    {JsonWireTypeDecoder2, FieldWireType}.

%% @private
-spec decode_wire_type_store_entry(JsonWireTypeDecoder, JsonValue) -> {JsonWireTypeDecoder, TypeName, WireType} when
    JsonWireTypeDecoder :: t(),
    JsonValue :: argo_json:json_value(),
    TypeName :: argo_types:name(),
    WireType :: argo_wire_type:t().
decode_wire_type_store_entry(JsonWireTypeDecoder1 = #argo_json_wire_type_decoder{}, JsonValue) ->
    JsonObject = argo_json:as_object(JsonValue),
    TypeName = argo_json:as_string(argo_json:object_get(<<"name">>, JsonObject)),
    JsonWireType = argo_json:object_get(<<"type">>, JsonObject),
    {JsonWireTypeDecoder2, WireType} = decode_wire_type(JsonWireTypeDecoder1, JsonWireType),
    ok = check_for_unknown_keys(JsonObject, #{<<"name">> => [], <<"type">> => []}),
    {JsonWireTypeDecoder2, TypeName, WireType}.
