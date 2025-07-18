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
-module(argo_json).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_index_map.hrl").

%% New API
-export([
    array/1,
    boolean/1,
    null/0,
    number/1,
    object/1,
    string/1,
    value/1
]).

%% Instance API
-export([
    as_array/1,
    as_boolean/1,
    as_null/1,
    as_number/1,
    as_object/1,
    as_string/1,
    decode/1,
    decode/2,
    encode/1,
    format/1,
    is_array/1,
    is_boolean/1,
    is_null/1,
    is_number/1,
    is_object/1,
    is_string/1,
    is_value/1,
    object_find/2,
    object_fold/3,
    object_get/2,
    object_size/1
]).

%% Errors API
-export([
    format_error/2
]).

%% Records
-record(argo_json_decode_state, {
    object_format :: index_map | map | tuple,
    stack :: [{array, [json_value()]} | {object, [{json_string(), json_value()}]}]
}).

%% Internal Types
-type decode_state() :: #argo_json_decode_state{}.

%% Types
-type decode_options() :: #{
    object_format => index_map | map | tuple
}.
-type encoded_value() :: unicode:unicode_binary().
-type json_array() :: [json_value()].
-type json_boolean() :: boolean().
-type json_null() :: null.
-type json_number() :: integer() | float().
-type json_object() ::
    #{json_string() => json_value()}
    | argo_index_map:t(json_string(), json_value())
    | {[{json_string(), json_value()}]}.
-type json_string() :: unicode:unicode_binary().
-type json_value() :: json_null() | json_boolean() | json_number() | json_string() | json_array() | json_object().

-export_type([
    decode_options/0,
    encoded_value/0,
    json_array/0,
    json_boolean/0,
    json_null/0,
    json_number/0,
    json_object/0,
    json_string/0,
    json_value/0
]).

%% Macros
-define(is_json_object(V),
    (erlang:is_map(V) orelse
        (erlang:is_tuple(V) andalso erlang:tuple_size(V) =:= 1 andalso erlang:is_list(erlang:element(1, V))) orelse
        erlang:is_record(V, argo_index_map))
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec array(json_array()) -> json_array().
array(Items) when erlang:is_list(Items) ->
    [value(Item) || Item <- Items].

-spec boolean(json_boolean()) -> json_boolean().
boolean(V) when erlang:is_boolean(V) -> V.

-spec null() -> json_null().
null() ->
    null.

-spec number(json_number()) -> json_number().
number(V) when ?is_i64(V) -> V;
number(V) when is_float(V) -> V.

-spec object(json_object()) -> json_object().
object(Map) when erlang:is_map(Map) ->
    maps:fold(
        fun(Key, Value, IndexMap) when is_binary(Key) ->
            argo_index_map:put(Key, value(Value), IndexMap)
        end,
        argo_index_map:new(),
        Map
    );
object(IndexMap = #argo_index_map{}) ->
    IndexMap;
object(V = {L}) when erlang:is_list(L) ->
    object_fold(
        fun(Key, Value, IndexMap) ->
            argo_index_map:put(Key, value(Value), IndexMap)
        end,
        argo_index_map:new(),
        V
    ).

-spec string(json_string()) -> json_string().
string(V) when erlang:is_binary(V) -> V.

-spec value(json_value()) -> json_value().
value(null) -> null();
value(V) when erlang:is_boolean(V) -> boolean(V);
value(V) when erlang:is_number(V) -> number(V);
value(V) when erlang:is_binary(V) -> string(V);
value(V) when erlang:is_list(V) -> array(V);
value(V) when erlang:is_map(V) -> object(V);
value(V = {L}) when erlang:is_list(L) -> object(V);
value(V = #argo_index_map{}) -> object(V).

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec as_array(json_value()) -> json_array().
as_array(JsonArray) when erlang:is_list(JsonArray) ->
    JsonArray;
as_array(JsonValue) ->
    error_with_info(badarg, [JsonValue], #{1 => expected_array}).

-spec as_boolean(json_value()) -> json_boolean().
as_boolean(JsonBoolean) when erlang:is_boolean(JsonBoolean) ->
    JsonBoolean;
as_boolean(JsonValue) ->
    error_with_info(badarg, [JsonValue], #{1 => expected_boolean}).

-spec as_null(json_value()) -> json_null().
as_null(null) ->
    null;
as_null(JsonValue) ->
    error_with_info(badarg, [JsonValue], #{1 => expected_null}).

-spec as_number(json_value()) -> json_number().
as_number(JsonNumber) when erlang:is_number(JsonNumber) ->
    JsonNumber;
as_number(JsonValue) ->
    error_with_info(badarg, [JsonValue], #{1 => expected_number}).

-spec as_object(json_value()) -> json_object().
as_object(JsonObject) when ?is_json_object(JsonObject) ->
    JsonObject;
as_object(JsonValue) ->
    error_with_info(badarg, [JsonValue], #{1 => expected_object}).

-spec as_string(json_value()) -> json_string().
as_string(JsonString) when erlang:is_binary(JsonString) ->
    JsonString;
as_string(JsonValue) ->
    error_with_info(badarg, [JsonValue], #{1 => expected_string}).

-spec decode(encoded_value()) -> json_value().
decode(JsonEncodedValue) when erlang:is_binary(JsonEncodedValue) ->
    decode(JsonEncodedValue, #{}).

-spec decode(encoded_value(), decode_options()) -> json_value().
decode(JsonEncodedValue, JsonDecodeOptions) when
    erlang:is_binary(JsonEncodedValue) andalso erlang:is_map(JsonDecodeOptions)
->
    JsonObjectFormat =
        case JsonDecodeOptions of
            #{object_format := V} when V =:= index_map orelse V =:= map orelse V =:= tuple ->
                V;
            #{} when erlang:map_size(JsonDecodeOptions) > 0 ->
                erlang:error(badarg, [JsonEncodedValue, JsonDecodeOptions]);
            #{} ->
                tuple
        end,
    DecodeState = #argo_json_decode_state{
        object_format = JsonObjectFormat,
        stack = []
    },
    case json:decode(JsonEncodedValue, DecodeState, decoders()) of
        {JsonValue, DecodeState, Remaining} ->
            ok = decode_remaining_check(Remaining),
            JsonValue
    end.

-spec encode(json_value()) -> encoded_value().
encode(JsonValue) ->
    Encoder = fun encode_value/2,
    argo_types:unicode_binary(encode_value(JsonValue, Encoder)).

-spec format(json_value()) -> encoded_value().
format(JsonValue) ->
    State = #{level => 0, col => 0, indent => 2, max => 100},
    Formatter = fun format_value/3,
    argo_types:unicode_binary(format_value(JsonValue, Formatter, State)).

-spec is_array(json_value()) -> boolean().
is_array(V) -> erlang:is_list(V).

-spec is_boolean(json_value()) -> boolean().
is_boolean(V) -> erlang:is_boolean(V).

-spec is_null(json_value()) -> boolean().
is_null(null) -> true;
is_null(_) -> false.

-spec is_number(json_value()) -> boolean().
is_number(V) when ?is_i64(V) orelse is_float(V) -> true;
is_number(_) -> false.

-spec is_object(json_value()) -> boolean().
is_object(V) when erlang:is_map(V) -> true;
is_object({V}) when erlang:is_list(V) -> true;
is_object(#argo_index_map{}) -> true;
is_object(_) -> false.

-spec is_string(json_value()) -> boolean().
is_string(V) when erlang:is_binary(V) -> true;
is_string(_) -> false.

-spec is_value(json_value()) -> boolean().
is_value(V) when erlang:is_list(V) -> true;
is_value(V) when erlang:is_boolean(V) -> true;
is_value(null) -> true;
is_value(V) when ?is_i64(V) orelse is_float(V) -> true;
is_value(V) when erlang:is_map(V) -> true;
is_value(#argo_index_map{}) -> true;
is_value(V) when erlang:is_binary(V) -> true;
is_value(_) -> false.

-spec object_find(Key, JsonObject) -> {ok, JsonValue} | error when
    Key :: json_string(), JsonObject :: json_object(), JsonValue :: json_value().
object_find(Key, Map) when is_binary(Key) andalso is_map(Map) ->
    maps:find(Key, Map);
object_find(Key, {TupleList}) when erlang:is_list(TupleList) ->
    case lists:keyfind(Key, 1, TupleList) of
        {Key, Value} ->
            {ok, Value};
        false ->
            error
    end;
object_find(Key, IndexMap = #argo_index_map{}) when is_binary(Key) ->
    argo_index_map:find(Key, IndexMap).

-spec object_fold(Function, Acc0, JsonObject) -> Acc1 when
    Function :: fun((Key :: json_string(), JsonValue :: json_value(), AccIn :: dynamic()) -> AccOut :: dynamic()),
    Acc0 :: dynamic(),
    JsonObject :: json_object(),
    Acc1 :: dynamic().
object_fold(Function, Init, Map) when is_function(Function, 3) andalso is_map(Map) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            Function(string(Key), Value, Acc)
        end,
        Init,
        Map
    );
object_fold(Function, Init, {TupleList}) when is_function(Function, 3) andalso erlang:is_list(TupleList) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            Function(string(Key), Value, Acc)
        end,
        Init,
        TupleList
    );
object_fold(Function, Init, IndexMap = #argo_index_map{}) when is_function(Function, 3) ->
    argo_index_map:foldl(
        fun(_Index, Key, Value, Acc) ->
            Function(string(Key), Value, Acc)
        end,
        Init,
        IndexMap
    ).

-spec object_get(Key, JsonObject) -> JsonValue when
    Key :: json_string(), JsonObject :: json_object(), JsonValue :: json_value().
object_get(Key, JsonObject) when is_binary(Key) andalso ?is_json_object(JsonObject) ->
    case object_find(Key, JsonObject) of
        {ok, Value} ->
            Value;
        error ->
            error_with_info({badkey, Key}, [Key, JsonObject], #{1 => {badkey, Key}})
    end.

-spec object_size(JsonObject) -> JsonObjectSize when
    JsonObject :: json_object(), JsonObjectSize :: non_neg_integer().
object_size(Map) when erlang:is_map(Map) ->
    maps:size(Map);
object_size({TupleList}) when erlang:is_list(TupleList) ->
    length(TupleList);
object_size(IndexMap = #argo_index_map{}) ->
    argo_index_map:size(IndexMap).

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
format_error_description(_Key, {badkey, JsonKey}) ->
    io_lib:format("not present in JSON object: ~0tP", [JsonKey, 5]);
format_error_description(_Key, expected_array) ->
    "expected JSON array";
format_error_description(_Key, expected_boolean) ->
    "expected JSON boolean";
format_error_description(_Key, expected_null) ->
    "expected JSON null";
format_error_description(_Key, expected_number) ->
    "expected JSON number";
format_error_description(_Key, expected_object) ->
    "expected JSON object";
format_error_description(_Key, expected_string) ->
    "expected JSON string";
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_array_finish(DecodeState, OldDecodeState) -> {JsonArray, OldDecodeState} when
    DecodeState :: decode_state(),
    OldDecodeState :: decode_state(),
    JsonArray :: json_array().
decode_array_finish(
    DecodeState = #argo_json_decode_state{stack = [{array, ArrayAcc} | Stack]},
    OldDecodeState = #argo_json_decode_state{}
) ->
    JsonArray = lists:reverse(ArrayAcc),
    case DecodeState#argo_json_decode_state{stack = Stack} of
        OldDecodeState ->
            {JsonArray, OldDecodeState}
    end.

%% @private
-spec decode_array_push(JsonValue, DecodeState) -> DecodeState when
    JsonValue :: json_value(), DecodeState :: decode_state().
decode_array_push(JsonValue, DecodeState1 = #argo_json_decode_state{stack = [{array, ArrayAcc1} | Stack1]}) ->
    ArrayAcc2 = [JsonValue | ArrayAcc1],
    Stack2 = [{array, ArrayAcc2} | Stack1],
    DecodeState2 = DecodeState1#argo_json_decode_state{stack = Stack2},
    DecodeState2.

%% @private
-spec decode_array_start(DecodeState) -> DecodeState when DecodeState :: decode_state().
decode_array_start(DecodeState1 = #argo_json_decode_state{stack = Stack1}) ->
    Stack2 = [{array, []} | Stack1],
    DecodeState2 = DecodeState1#argo_json_decode_state{stack = Stack2},
    DecodeState2.

%% @private
-spec decode_object_finish(DecodeState, OldDecodeState) -> {JsonObject, OldDecodeState} when
    DecodeState :: decode_state(),
    OldDecodeState :: decode_state(),
    JsonObject :: json_object().
decode_object_finish(
    DecodeState = #argo_json_decode_state{stack = [{object, ObjectAcc} | Stack]},
    OldDecodeState = #argo_json_decode_state{}
) ->
    KeyValueList = lists:reverse(ObjectAcc),
    JsonObject =
        case DecodeState#argo_json_decode_state.object_format of
            index_map ->
                argo_index_map:from_list(KeyValueList);
            map ->
                maps:from_list(KeyValueList);
            tuple ->
                {KeyValueList}
        end,
    case DecodeState#argo_json_decode_state{stack = Stack} of
        OldDecodeState ->
            {JsonObject, OldDecodeState}
    end.

%% @private
-spec decode_object_push(JsonKey, JsonValue, DecodeState) -> DecodeState when
    JsonKey :: json_string(), JsonValue :: json_value(), DecodeState :: decode_state().
decode_object_push(JsonKey, JsonValue, DecodeState1 = #argo_json_decode_state{stack = [{object, Object1} | Stack1]}) ->
    Object2 = [{JsonKey, JsonValue} | Object1],
    Stack2 = [{object, Object2} | Stack1],
    DecodeState2 = DecodeState1#argo_json_decode_state{stack = Stack2},
    DecodeState2.

%% @private
-spec decode_object_start(DecodeState) -> DecodeState when DecodeState :: decode_state().
decode_object_start(DecodeState1 = #argo_json_decode_state{stack = Stack1}) ->
    Stack2 = [{object, []} | Stack1],
    DecodeState2 = DecodeState1#argo_json_decode_state{stack = Stack2},
    DecodeState2.

%% @private
-spec decode_remaining_check(binary()) -> ok.
decode_remaining_check(<<>>) ->
    ok;
decode_remaining_check(<<$\s, Bytes/bytes>>) ->
    decode_remaining_check(Bytes);
decode_remaining_check(<<$\t, Bytes/bytes>>) ->
    decode_remaining_check(Bytes);
decode_remaining_check(<<$\r, Bytes/bytes>>) ->
    decode_remaining_check(Bytes);
decode_remaining_check(<<$\n, Bytes/bytes>>) ->
    decode_remaining_check(Bytes);
decode_remaining_check(<<Bytes/bytes>>) ->
    erlang:error(badarg, [Bytes]).

%% @private
-spec decoders() -> json:decoders().
decoders() ->
    #{
        array_start => fun decode_array_start/1,
        array_push => fun decode_array_push/2,
        array_finish => fun decode_array_finish/2,
        object_start => fun decode_object_start/1,
        object_push => fun decode_object_push/3,
        object_finish => fun decode_object_finish/2
    }.

%% @private
-spec encode_value(json_value(), json:encoder()) -> iodata().
encode_value(JsonValue, Encoder) ->
    case JsonValue of
        JsonNull = null ->
            json:encode_atom(JsonNull, Encoder);
        JsonBoolean when erlang:is_boolean(JsonBoolean) ->
            json:encode_atom(JsonBoolean, Encoder);
        JsonInteger when erlang:is_integer(JsonInteger) ->
            json:encode_integer(JsonInteger);
        JsonFloat when erlang:is_float(JsonFloat) ->
            json:encode_float(JsonFloat);
        JsonString when erlang:is_binary(JsonString) ->
            json:encode_binary(JsonString);
        JsonArray when erlang:is_list(JsonArray) ->
            json:encode_list(JsonArray, Encoder);
        JsonObject when erlang:is_map(JsonObject) ->
            json:encode_map(JsonObject, Encoder);
        {KeyValueList} when erlang:is_list(KeyValueList) ->
            json:encode_key_value_list_checked(KeyValueList, Encoder);
        JsonObject = #argo_index_map{} ->
            KeyValueList = argo_index_map:to_list(JsonObject),
            json:encode_key_value_list_checked(KeyValueList, Encoder)
    end.

%% @private
-spec format_value(json_value(), json:formatter(), dynamic()) -> iodata().
format_value(JsonValue, Formatter, State) ->
    case JsonValue of
        JsonNull = null ->
            json:format_value(JsonNull, Formatter, State);
        JsonBoolean when erlang:is_boolean(JsonBoolean) ->
            json:format_value(JsonBoolean, Formatter, State);
        JsonInteger when erlang:is_integer(JsonInteger) ->
            json:format_value(JsonInteger, Formatter, State);
        JsonFloat when erlang:is_float(JsonFloat) ->
            json:format_value(JsonFloat, Formatter, State);
        JsonString when erlang:is_binary(JsonString) ->
            json:format_value(JsonString, Formatter, State);
        JsonArray when erlang:is_list(JsonArray) ->
            json:format_value(JsonArray, Formatter, State);
        JsonObject when erlang:is_map(JsonObject) ->
            json:format_value(JsonObject, Formatter, State);
        {KeyValueList} when erlang:is_list(KeyValueList) ->
            json:format_key_value_list_checked(KeyValueList, Formatter, State);
        JsonObject = #argo_index_map{} ->
            KeyValueList = argo_index_map:to_list(JsonObject),
            json:format_key_value_list_checked(KeyValueList, Formatter, State)
    end.
