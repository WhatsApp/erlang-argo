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
-module(argo_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Codec API
-export([
    display/1,
    display/2,
    format/1,
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
    is_scalar/1,
    to_wire_type/1
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

-spec display(Value) -> ok when Value :: t().
display(Value = #argo_value{}) ->
    display(Value, standard_io).

-spec display(Value, IoDevice) -> ok when Value :: t(), IoDevice :: io:device().
display(Value = #argo_value{}, IoDevice) when not is_list(IoDevice) ->
    Printer1 = argo_value_printer:new_io_device(IoDevice),
    Printer2 = argo_value_printer:print_value(Printer1, Value),
    case argo_value_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(Value) -> Output when Value :: t(), Output :: iolist().
format(Value = #argo_value{}) ->
    Printer1 = argo_value_printer:new_string(),
    Printer2 = argo_value_printer:print_value(Printer1, Value),
    case argo_value_printer:finalize(Printer2) of
        Output when is_list(Output) ->
            Output
    end.

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
    argo_scalar_value:is_labeled(BlockValue#argo_block_value.value);
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

-spec to_wire_type(Value) -> WireType when Value :: t(), WireType :: argo_wire_type:t().
to_wire_type(#argo_value{inner = ArrayValue = #argo_array_value{}}) ->
    ArrayWireType = argo_array_value:to_array_wire_type(ArrayValue),
    argo_wire_type:array(ArrayWireType);
to_wire_type(#argo_value{inner = BlockValue = #argo_block_value{}}) ->
    BlockWireType = argo_block_value:to_block_wire_type(BlockValue),
    argo_wire_type:block(BlockWireType);
to_wire_type(#argo_value{inner = #argo_desc_value{}}) ->
    argo_wire_type:desc();
to_wire_type(#argo_value{inner = #argo_error_value{}}) ->
    argo_wire_type:error();
to_wire_type(#argo_value{inner = NullableValue = #argo_nullable_value{}}) ->
    NullableWireType = argo_nullable_value:to_nullable_wire_type(NullableValue),
    argo_wire_type:nullable(NullableWireType);
to_wire_type(#argo_value{inner = #argo_path_value{}}) ->
    argo_wire_type:path();
to_wire_type(#argo_value{inner = RecordValue = #argo_record_value{}}) ->
    RecordWireType = argo_record_value:to_record_wire_type(RecordValue),
    argo_wire_type:record(RecordWireType);
to_wire_type(#argo_value{inner = ScalarValue = #argo_scalar_value{}}) ->
    ScalarWireType = argo_scalar_value:to_scalar_wire_type(ScalarValue),
    argo_wire_type:scalar(ScalarWireType).
