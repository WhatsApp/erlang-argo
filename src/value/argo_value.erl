%%% % @format
-module(argo_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_header.hrl").
-include("argo_value.hrl").
-include("argo_wire_type.hrl").

%% Codec API
-export([
    from_json/2,
    from_reader/2,
    to_json/1,
    to_writer/1,
    to_writer/2
]).

%% New API
-export([
    array/1,
    block/1,
    desc/1,
    error/1,
    nullable/1,
    path/1,
    record/1,
    scalar/1
]).

%% Instance API
-export([
    is_array/1,
    is_block/1,
    is_desc/1,
    is_error/1,
    is_labeled/1,
    is_nullable/1,
    is_path/1,
    is_record/1,
    is_scalar/1
]).

-type inner() ::
    argo_array_value:t()
    | argo_block_value:t()
    | argo_desc_value:t()
    | argo_error_value:t()
    | argo_nullable_value:t()
    | argo_path_value:t()
    | argo_record_value:t()
    | argo_scalar_value:t().

-type t() :: #argo_value{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% Codec API functions
%%%=============================================================================

-spec from_json(WireType, JsonValue) -> Value when
    WireType :: argo_wire_type:t(), JsonValue :: argo_json:json_value(), Value :: t().
from_json(WireType = #argo_wire_type{}, JsonValue) ->
    JsonValueDecoder1 = argo_json_value_decoder:new(),
    {JsonValueDecoder2, Value} = argo_json_value_decoder:decode_wire_type(JsonValueDecoder1, WireType, JsonValue),
    _ = JsonValueDecoder2,
    Value.

-spec from_reader(WireType, Reader) -> {Reader, Value} when
    WireType :: argo_wire_type:t(), Reader :: binary(), Value :: t().
from_reader(WireType = #argo_wire_type{}, Reader1) when is_binary(Reader1) ->
    {Reader2, ValueDecoder1} = argo_value_decoder:from_reader(Reader1),
    {ValueDecoder2, Value} = argo_value_decoder:decode_wire_type(ValueDecoder1, WireType),
    _ = ValueDecoder2,
    {Reader2, Value}.

-spec to_json(Value) -> JsonValue when Value :: t(), JsonValue :: argo_json:json_value().
to_json(Value = #argo_value{}) ->
    JsonValueEncoder1 = argo_json_value_encoder:new(),
    {JsonValueEncoder2, JsonValue} = argo_json_value_encoder:encode_value(JsonValueEncoder1, Value),
    _ = JsonValueEncoder2,
    JsonValue.

-spec to_writer(Value) -> Writer when Value :: t(), Writer :: binary().
to_writer(Value = #argo_value{}) ->
    to_writer(Value, argo_header:new()).

-spec to_writer(Value, Header) -> Writer when Value :: t(), Header :: argo_header:t(), Writer :: binary().
to_writer(Value = #argo_value{}, Header = #argo_header{}) ->
    ValueEncoder1 = argo_value_encoder:new(Header),
    ValueEncoder2 = argo_value_encoder:encode_value(ValueEncoder1, Value),
    argo_value_encoder:to_writer(ValueEncoder2).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec array(ArrayValue) -> Value when ArrayValue :: argo_array_value:t(), Value :: t().
array(ArrayValue = #argo_array_value{}) ->
    #argo_value{inner = ArrayValue}.

-spec block(BlockValue) -> Value when BlockValue :: argo_block_value:t(), Value :: t().
block(BlockValue = #argo_block_value{}) ->
    #argo_value{inner = BlockValue}.

-spec desc(DescValue) -> Value when DescValue :: argo_desc_value:t(), Value :: t().
desc(DescValue = #argo_desc_value{}) ->
    #argo_value{inner = DescValue}.

-spec error(ErrorValue) -> Value when ErrorValue :: argo_error_value:t(), Value :: t().
error(ErrorValue = #argo_error_value{}) ->
    #argo_value{inner = ErrorValue}.

-spec nullable(NullableValue) -> Value when NullableValue :: argo_nullable_value:t(), Value :: t().
nullable(NullableValue = #argo_nullable_value{}) ->
    #argo_value{inner = NullableValue}.

-spec path(PathValue) -> Value when PathValue :: argo_path_value:t(), Value :: t().
path(PathValue = #argo_path_value{}) ->
    #argo_value{inner = PathValue}.

-spec record(RecordValue) -> Value when RecordValue :: argo_record_value:t(), Value :: t().
record(RecordValue = #argo_record_value{}) ->
    #argo_value{inner = RecordValue}.

-spec scalar(ScalarValue) -> Value when ScalarValue :: argo_scalar_value:t(), Value :: t().
scalar(ScalarValue = #argo_scalar_value{}) ->
    #argo_value{inner = ScalarValue}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec is_array(Value) -> boolean() when Value :: t().
is_array(#argo_value{inner = #argo_array_value{}}) -> true;
is_array(#argo_value{}) -> false.

-spec is_block(Value) -> boolean() when Value :: t().
is_block(#argo_value{inner = #argo_block_value{}}) -> true;
is_block(#argo_value{}) -> false.

-spec is_desc(Value) -> boolean() when Value :: t().
is_desc(#argo_value{inner = #argo_desc_value{}}) -> true;
is_desc(#argo_value{}) -> false.

-spec is_error(Value) -> boolean() when Value :: t().
is_error(#argo_value{inner = #argo_error_value{}}) -> true;
is_error(#argo_value{}) -> false.

-spec is_labeled(Value) -> boolean() when Value :: t().
is_labeled(#argo_value{inner = ScalarValue = #argo_scalar_value{}}) ->
    argo_scalar_value:is_labeled(ScalarValue);
is_labeled(#argo_value{inner = BlockValue = #argo_block_value{}}) ->
    argo_scalar_value:is_labeled(BlockValue#argo_block_value.'of');
is_labeled(#argo_value{inner = #argo_nullable_value{}}) ->
    true;
is_labeled(#argo_value{inner = #argo_array_value{}}) ->
    true;
is_labeled(#argo_value{}) ->
    false.

-spec is_nullable(Value) -> boolean() when Value :: t().
is_nullable(#argo_value{inner = #argo_nullable_value{}}) -> true;
is_nullable(#argo_value{}) -> false.

-spec is_path(Value) -> boolean() when Value :: t().
is_path(#argo_value{inner = #argo_path_value{}}) -> true;
is_path(#argo_value{}) -> false.

-spec is_record(Value) -> boolean() when Value :: t().
is_record(#argo_value{inner = #argo_record_value{}}) -> true;
is_record(#argo_value{}) -> false.

-spec is_scalar(Value) -> boolean() when Value :: t().
is_scalar(#argo_value{inner = #argo_scalar_value{}}) -> true;
is_scalar(#argo_value{}) -> false.
