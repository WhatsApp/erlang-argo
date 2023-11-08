%%% % @format
-module(argo_json).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_common.hrl").
-include("argo_index_map.hrl").

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
    is_array/1,
    is_boolean/1,
    is_null/1,
    is_number/1,
    is_object/1,
    is_string/1,
    is_value/1,
    object_find/2,
    object_fold/3
]).

%% Types
-type json_array() :: [json_value()].
-type json_boolean() :: boolean().
-type json_null() :: null.
-type json_number() :: integer() | float().
-type json_object() :: #{json_string() => json_value()} | argo_index_map:t(json_string(), json_value()).
-type json_string() :: unicode:unicode_binary().
-type json_value() :: json_null() | json_boolean() | json_number() | json_string() | json_array() | json_object().

-export_type([
    json_array/0,
    json_boolean/0,
    json_null/0,
    json_number/0,
    json_object/0,
    json_string/0,
    json_value/0
]).

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
    IndexMap.

-spec string(json_string()) -> json_string().
string(V) when erlang:is_binary(V) -> V.

-spec value(json_value()) -> json_value().
value(null) -> null();
value(V) when erlang:is_boolean(V) -> boolean(V);
value(V) when erlang:is_number(V) -> number(V);
value(V) when erlang:is_binary(V) -> string(V);
value(V) when erlang:is_list(V) -> array(V);
value(V) when erlang:is_map(V) -> object(V);
value(V = #argo_index_map{}) -> object(V).

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

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
object_fold(Function, Init, IndexMap = #argo_index_map{}) when is_function(Function, 3) ->
    argo_index_map:foldl(
        fun(_Index, Key, Value, Acc) ->
            Function(string(Key), Value, Acc)
        end,
        Init,
        IndexMap
    ).
