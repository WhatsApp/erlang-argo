%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_term_value_encoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-05-13", modified => "2025-07-16"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% New API
-export([
    new/2
]).
%% Instance API
-export([
    encode_array_value/2,
    encode_block_value/2,
    encode_desc_value/2,
    encode_error_value/2,
    encode_extensions_value/2,
    encode_field_value/2,
    encode_location_value/2,
    encode_nullable_value/2,
    encode_path_value/2,
    encode_record_value/2,
    encode_scalar_value/2,
    encode_value/2
]).

% %% Errors API
% -export([
%     format_error/2
% ]).

%% Types
-type error_reason() :: invalid | type_mismatch | dynamic().
-type options() :: dynamic().
-type result(Ok, Error) :: {ok, Ok} | {error, Error}.
-type state() :: dynamic().
-type t() :: #argo_term_value_encoder{}.

-export_type([
    error_reason/0,
    options/0,
    result/2,
    state/0,
    t/0
]).

%% Behaviour
-callback init(Options) -> EncoderState when Options :: options(), EncoderState :: state().

-callback encode_array(EncoderState, ArrayValueHint, ArrayValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ArrayValue :: argo_array_value:t(),
    Result :: result(ArrayTermValue, ErrorReason),
    ArrayTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_array_next(EncoderState, ArrayValueHint, ItemTermValue, ArrayTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ItemTermValue :: argo_term:term_value(),
    ArrayTermValue :: argo_term:term_value(),
    Result :: result(ArrayTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_array_stop(EncoderState, ArrayValueHint, ArrayTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ArrayTermValue :: argo_term:term_value(),
    Result :: result(ArrayTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_block(EncoderState, BlockWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_desc(EncoderState, DescValueHint, DescTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescValueHint :: argo_term:desc_value_hint(),
    DescTermValue :: argo_term:term_value(),
    Result :: result(DescTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_desc_list(EncoderState, DescList) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescList :: argo_desc_value:desc_list(),
    Result :: result(DescListTermValue, ErrorReason),
    DescListTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_desc_list_next(EncoderState, DescValueListHint, DescTermValue, DescListTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescValueListHint :: argo_term:desc_value_list_hint(),
    DescTermValue :: argo_term:term_value(),
    DescListTermValue :: argo_term:term_value(),
    Result :: result(DescListTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_desc_list_stop(EncoderState, DescListTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescListTermValue :: argo_term:term_value(),
    Result :: result(DescListTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_desc_object(EncoderState, DescObject) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescObject :: argo_desc_value:desc_object(),
    Result :: result(DescObjectTermValue, ErrorReason),
    DescObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_desc_object_next(EncoderState, DescValueObjectHint, DescTermValue, DescObjectTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescValueObjectHint :: argo_term:desc_value_object_hint(),
    DescTermValue :: argo_term:term_value(),
    DescObjectTermValue :: argo_term:term_value(),
    Result :: result(DescObjectTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_desc_object_stop(EncoderState, DescObjectTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescObjectTermValue :: argo_term:term_value(),
    Result :: result(DescObjectTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_desc_scalar(EncoderState, DescScalar) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescScalar :: argo_desc_value:inner_scalar(),
    Result :: result(DescScalarTermValue, ErrorReason),
    DescScalarTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_error(EncoderState, ErrorValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ErrorValue :: argo_error_value:t(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_error_extensions(EncoderState, OptionExtensionsTermValue, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    OptionExtensionsTermValue :: argo_types:option(ExtensionsTermValue),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_error_locations(EncoderState, OptionLocationsTermValue, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    OptionLocationsTermValue :: argo_types:option(LocationsTermValue),
    LocationsTermValue :: argo_term:term_value(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_error_message(EncoderState, Message, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Message :: unicode:unicode_binary(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_error_path(EncoderState, OptionPathTermValue, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    OptionPathTermValue :: argo_types:option(PathTermValue),
    PathTermValue :: argo_term:term_value(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_error_stop(EncoderState, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_extensions(EncoderState, ExtensionsValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ExtensionsValue :: argo_extensions_value:t(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_extensions_next(EncoderState, DescValueObjectHint, DescTermValue, ExtensionsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescValueObjectHint :: argo_term:desc_value_object_hint(),
    DescTermValue :: argo_term:term_value(),
    ExtensionsTermValue :: argo_term:term_value(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_extensions_stop(EncoderState, ExtensionsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ExtensionsTermValue :: argo_term:term_value(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_field(EncoderState, FieldWireTypeHint, OptionTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    FieldWireTypeHint :: argo_term:field_wire_type_hint(),
    OptionTermValue :: argo_types:option(TermValue),
    TermValue :: argo_term:term_value(),
    Result :: result(OptionFieldTermValue, ErrorReason),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_location(EncoderState, LocationValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationTermValue, ErrorReason),
    LocationTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_location_column(EncoderState, Column, LocationTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Column :: argo_types:varint(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_location_line(EncoderState, Line, LocationTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Line :: argo_types:varint(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_location_stop(EncoderState, LocationTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_locations(EncoderState, LocationsValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationsValue :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationsTermValue, ErrorReason),
    LocationsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_locations_next(EncoderState, Index, LocationTermValue, LocationsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Index :: argo_types:index(),
    LocationTermValue :: argo_term:term_value(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result(LocationsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_locations_stop(EncoderState, LocationsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result(LocationsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_nullable(EncoderState, NullableWireTypeHint, NullableTermEnum) -> {EncoderState, Result} when
    EncoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    NullableTermEnum :: null | {non_null, NonNullTermValue} | {field_errors, FieldErrorsTermValue},
    NonNullTermValue :: argo_term:term_value(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(NullableTermValue, ErrorReason),
    NullableTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_nullable_field_errors(EncoderState, NullableWireTypeHint, NullableValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    NullableValue :: argo_nullable_value:t(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    FieldErrorsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_nullable_field_errors_next(EncoderState, Index, NullableWireTypeHint, ErrorTermValue, FieldErrorsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Index :: argo_types:index(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    ErrorTermValue :: argo_term:term_value(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_nullable_field_errors_stop(EncoderState, FieldErrorsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_path(EncoderState, PathValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    PathValue :: argo_path_value:t(),
    Result :: result(PathTermValue, ErrorReason),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_path_next(EncoderState, Index, Segment, PathTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Index :: argo_types:index(),
    Segment :: argo_path_value:segment(),
    PathTermValue :: argo_term:term_value(),
    Result :: result(PathTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_path_stop(EncoderState, PathTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    PathTermValue :: argo_term:term_value(),
    Result :: result(PathTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_record(EncoderState, RecordWireTypeHint, RecordValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    RecordValue :: argo_record_value:t(),
    Result :: result(RecordTermValue, ErrorReason),
    RecordTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_record_next(EncoderState, Index, FieldWireTypeHint, OptionFieldTermValue, RecordTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Index :: argo_types:index(),
    FieldWireTypeHint :: argo_term:field_wire_type_hint(),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value(),
    RecordTermValue :: argo_term:term_value(),
    Result :: result(RecordTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_record_stop(EncoderState, RecordWireTypeHint, RecordTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    RecordTermValue :: argo_term:term_value(),
    Result :: result(RecordTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback encode_scalar(EncoderState, ScalarWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(ScalarTermValue, ErrorReason),
    ErrorReason :: error_reason().

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec new(TermValueEncoderModule, TermValueEncoderOptions) -> TermValueEncoder when
    TermValueEncoderModule :: module(),
    TermValueEncoderOptions :: options(),
    TermValueEncoder :: t().
new(TermValueEncoderModule, TermValueEncoderOptions) when is_atom(TermValueEncoderModule) ->
    TermValueEncoderState = TermValueEncoderModule:init(TermValueEncoderOptions),
    #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule,
        encoder_state = TermValueEncoderState
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec encode_array_value(TermValueEncoder, ArrayValue) -> {TermValueEncoder, ArrayTermValue} when
    TermValueEncoder :: t(),
    ArrayValue :: argo_array_value:t(),
    ArrayTermValue :: argo_term:term_value().
encode_array_value(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule,
        encoder_state = TermValueEncoderState1
    },
    ArrayValue = #argo_array_value{}
) ->
    ArrayWireTypeHint = argo_term:array_value_hint(ArrayValue, 0),
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_array(TermValueEncoderState1, ArrayWireTypeHint, ArrayValue),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, ArrayTermValue} ->
            ArrayValueIterator = argo_array_value:iterator(ArrayValue),
            encode_array_value_next(TermValueEncoder2, ArrayValueIterator, ArrayWireTypeHint, ArrayTermValue)
    end.

-spec encode_block_value(TermValueEncoder, BlockValue) -> {TermValueEncoder, BlockTermValue} when
    TermValueEncoder :: t(),
    BlockValue :: argo_block_value:t(),
    BlockTermValue :: argo_term:term_value().
encode_block_value(TermValueEncoder1 = #argo_term_value_encoder{}, BlockValue = #argo_block_value{}) ->
    BlockWireTypeHint = argo_term:block_wire_type_hint(BlockValue),
    ScalarValue = BlockValue#argo_block_value.value,
    {TermValueEncoder2, ScalarTermValue} = encode_scalar_value(TermValueEncoder1, ScalarValue),
    TermValueEncoderModule = TermValueEncoder2#argo_term_value_encoder.encoder_module,
    TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState3, Result} =
        TermValueEncoderModule:encode_block(TermValueEncoderState2, BlockWireTypeHint, ScalarTermValue),
    TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
    case Result of
        {ok, BlockTermValue} ->
            {TermValueEncoder3, BlockTermValue}
    end.

-spec encode_desc_value(TermValueEncoder, DescValue) -> {TermValueEncoder, DescTermValue} when
    TermValueEncoder :: t(), DescValue :: argo_desc_value:t(), DescTermValue :: argo_term:term_value().
encode_desc_value(TermValueEncoder1 = #argo_term_value_encoder{}, DescValue = #argo_desc_value{}) ->
    {TermValueEncoder2, DescTermValue1} =
        case DescValue#argo_desc_value.inner of
            DescScalar = null ->
                encode_desc_value_scalar(TermValueEncoder1, DescScalar);
            DescScalar = {boolean, _} ->
                encode_desc_value_scalar(TermValueEncoder1, DescScalar);
            DescScalar = {string, _} ->
                encode_desc_value_scalar(TermValueEncoder1, DescScalar);
            DescScalar = {bytes, _} ->
                encode_desc_value_scalar(TermValueEncoder1, DescScalar);
            DescScalar = {int, _} ->
                encode_desc_value_scalar(TermValueEncoder1, DescScalar);
            DescScalar = {float, _} ->
                encode_desc_value_scalar(TermValueEncoder1, DescScalar);
            {object, DescObject} ->
                encode_desc_value_object(TermValueEncoder1, DescObject);
            {list, DescList} ->
                encode_desc_value_list(TermValueEncoder1, DescList)
        end,
    DescValueHint = argo_term:desc_value_hint(DescValue),
    TermValueEncoderModule2 = TermValueEncoder2#argo_term_value_encoder.encoder_module,
    TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState3, Result} =
        TermValueEncoderModule2:encode_desc(TermValueEncoderState2, DescValueHint, DescTermValue1),
    TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
    case Result of
        {ok, DescTermValue2} ->
            {TermValueEncoder3, DescTermValue2}
    end.

-spec encode_error_value(TermValueEncoder, ErrorValue) -> {TermValueEncoder, ErrorTermValue} when
    TermValueEncoder :: t(), ErrorValue :: argo_error_value:t(), ErrorTermValue :: argo_term:term_value().
encode_error_value(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule1,
        encoder_state = TermValueEncoderState1
    },
    ErrorValue = #argo_error_value{}
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule1:encode_error(TermValueEncoderState1, ErrorValue),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, ErrorTermValue1} ->
            {TermValueEncoder3, ErrorTermValue2} = encode_error_value_message(
                TermValueEncoder2, ErrorValue#argo_error_value.message, ErrorTermValue1
            ),
            {TermValueEncoder4, ErrorTermValue3} = encode_error_value_locations(
                TermValueEncoder3, ErrorValue#argo_error_value.locations, ErrorTermValue2
            ),
            {TermValueEncoder5, ErrorTermValue4} = encode_error_value_path(
                TermValueEncoder4, ErrorValue#argo_error_value.path, ErrorTermValue3
            ),
            {TermValueEncoder6, ErrorTermValue5} = encode_error_value_extensions(
                TermValueEncoder5, ErrorValue#argo_error_value.extensions, ErrorTermValue4
            ),
            {TermValueEncoder7, ErrorTermValue6} = encode_error_value_stop(TermValueEncoder6, ErrorTermValue5),
            {TermValueEncoder7, ErrorTermValue6}
    end.

-spec encode_extensions_value(TermValueEncoder, ExtensionsValue) -> {TermValueEncoder, ExtensionsTermValue} when
    TermValueEncoder :: t(),
    ExtensionsValue :: argo_extensions_value:t(),
    ExtensionsTermValue :: argo_term:term_value().
encode_extensions_value(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule,
        encoder_state = TermValueEncoderState1
    },
    ExtensionsValue = #argo_extensions_value{}
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_extensions(TermValueEncoderState1, ExtensionsValue),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, ExtensionsTermValue} ->
            ExtensionsValueIterator = argo_extensions_value:iterator(ExtensionsValue),
            encode_extensions_value_next(TermValueEncoder2, ExtensionsValueIterator, ExtensionsTermValue)
    end.

-spec encode_field_value(TermValueEncoder, FieldValue) -> {TermValueEncoder, OptionFieldTermValue} when
    TermValueEncoder :: t(),
    FieldValue :: argo_field_value:t(),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value().
encode_field_value(
    TermValueEncoder1 = #argo_term_value_encoder{},
    FieldValue = #argo_field_value{}
) ->
    FieldWireTypeHint = argo_term:field_wire_type_hint(FieldValue),
    OptionValue = argo_field_value:get(FieldValue),
    {TermValueEncoder3, OptionTermValue} =
        case OptionValue of
            none ->
                {TermValueEncoder1, none};
            {some, Value} ->
                {TermValueEncoder2, TermValue} = encode_value(TermValueEncoder1, Value),
                {TermValueEncoder2, {some, TermValue}}
        end,
    TermValueEncoderModule3 = TermValueEncoder3#argo_term_value_encoder.encoder_module,
    TermValueEncoderState3 = TermValueEncoder3#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState4, Result} =
        TermValueEncoderModule3:encode_field(TermValueEncoderState3, FieldWireTypeHint, OptionTermValue),
    TermValueEncoder5 = maybe_update_encoder_state(TermValueEncoder3, TermValueEncoderState4),
    case Result of
        {ok, OptionFieldTermValue} ->
            {TermValueEncoder5, OptionFieldTermValue}
    end.

-spec encode_location_value(TermValueEncoder, LocationValue) -> {TermValueEncoder, LocationTermValue} when
    TermValueEncoder :: t(), LocationValue :: argo_location_value:t(), LocationTermValue :: argo_term:term_value().
encode_location_value(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule1,
        encoder_state = TermValueEncoderState1
    },
    LocationValue = #argo_location_value{}
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule1:encode_location(TermValueEncoderState1, LocationValue),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, LocationTermValue1} ->
            {TermValueEncoder3, LocationTermValue2} = encode_location_value_line(
                TermValueEncoder2, LocationValue#argo_location_value.line, LocationTermValue1
            ),
            {TermValueEncoder4, LocationTermValue3} = encode_location_value_column(
                TermValueEncoder3, LocationValue#argo_location_value.column, LocationTermValue2
            ),
            {TermValueEncoder5, LocationTermValue4} = encode_location_value_stop(TermValueEncoder4, LocationTermValue3),
            {TermValueEncoder5, LocationTermValue4}
    end.

-spec encode_nullable_value(TermValueEncoder, NullableValue) -> {TermValueEncoder, NullableTermValue} when
    TermValueEncoder :: t(),
    NullableValue :: argo_nullable_value:t(),
    NullableTermValue :: argo_term:term_value().
encode_nullable_value(TermValueEncoder1 = #argo_term_value_encoder{}, NullableValue = #argo_nullable_value{}) ->
    NullableWireTypeHint = argo_term:nullable_wire_type_hint(NullableValue),
    case NullableValue#argo_nullable_value.inner of
        null ->
            TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
            TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState2, Result} =
                TermValueEncoderModule:encode_nullable(TermValueEncoderState1, NullableWireTypeHint, null),
            TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
            case Result of
                {ok, NullableTermValue} ->
                    {TermValueEncoder2, NullableTermValue}
            end;
        {non_null, NonNullValue} ->
            {TermValueEncoder2, NonNullTermValue} = encode_value(TermValueEncoder1, NonNullValue),
            TermValueEncoderModule = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule:encode_nullable(TermValueEncoderState2, NullableWireTypeHint, {non_null, NonNullTermValue}),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, NullableTermValue} ->
                    {TermValueEncoder3, NullableTermValue}
            end;
        {field_errors, FieldErrorsValue} ->
            TermValueEncoderModule1 = TermValueEncoder1#argo_term_value_encoder.encoder_module,
            TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState2, Result1} =
                TermValueEncoderModule1:encode_nullable_field_errors(TermValueEncoderState1, NullableWireTypeHint, NullableValue),
            TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
            case Result1 of
                {ok, FieldErrorsTermValue1} ->
                    FieldErrorsValueIterator = {0, FieldErrorsValue},
                    {TermValueEncoder3, FieldErrorsTermValue2} =
                        encode_nullable_value_field_errors_next(TermValueEncoder2, NullableWireTypeHint, FieldErrorsValueIterator, FieldErrorsTermValue1),
                    TermValueEncoderModule3 = TermValueEncoder3#argo_term_value_encoder.encoder_module,
                    TermValueEncoderState3 = TermValueEncoder3#argo_term_value_encoder.encoder_state,
                    {TermValueEncoderState4, Result2} =
                        TermValueEncoderModule3:encode_nullable(TermValueEncoderState3, NullableWireTypeHint, {field_errors, FieldErrorsTermValue2}),
                    TermValueEncoder4 = maybe_update_encoder_state(TermValueEncoder3, TermValueEncoderState4),
                    case Result2 of
                        {ok, NullableTermValue} ->
                            {TermValueEncoder4, NullableTermValue}
                    end
            end
    end.

-spec encode_path_value(TermValueEncoder, PathValue) -> {TermValueEncoder, PathTermValue} when
    TermValueEncoder :: t(), PathValue :: argo_path_value:t(), PathTermValue :: argo_term:term_value().
encode_path_value(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule1,
        encoder_state = TermValueEncoderState1
    },
    PathValue = #argo_path_value{}
) ->
    {TermValueEncoderState2, Result} = TermValueEncoderModule1:encode_path(TermValueEncoderState1, PathValue),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, PathTermValue} ->
            PathValueIterator = argo_path_value:iterator(PathValue),
            encode_path_value_next(TermValueEncoder2, PathValueIterator, PathTermValue)
    end.

-spec encode_record_value(TermValueEncoder, RecordValue) -> {TermValueEncoder, RecordTermValue} when
    TermValueEncoder :: t(),
    RecordValue :: argo_record_value:t(),
    RecordTermValue :: argo_term:term_value().
encode_record_value(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule1,
        encoder_state = TermValueEncoderState1
    },
    RecordValue = #argo_record_value{}
) ->
    RecordWireTypeHint = argo_term:record_wire_type_hint(RecordValue),
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule1:encode_record(TermValueEncoderState1, RecordWireTypeHint, RecordValue),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, RecordTermValue} ->
            RecordValueIterator = argo_record_value:iterator(RecordValue),
            encode_record_value_next(TermValueEncoder2, RecordValueIterator, RecordWireTypeHint, RecordTermValue)
    end.

-spec encode_scalar_value(TermValueEncoder, ScalarValue) -> {TermValueEncoder, ScalarTermValue} when
    TermValueEncoder :: t(),
    ScalarValue :: argo_scalar_value:t(),
    ScalarTermValue :: argo_term:term_value().
encode_scalar_value(TermValueEncoder1 = #argo_term_value_encoder{}, ScalarValue = #argo_scalar_value{}) ->
    {TermValueEncoder3, {ScalarWireTypeHint, ScalarTermValue1}} =
        case ScalarValue#argo_scalar_value.inner of
            {desc, DescValue} ->
                {TermValueEncoder2, DescTermValue} = encode_desc_value(TermValueEncoder1, DescValue),
                {TermValueEncoder2, {desc, DescTermValue}};
            {fixed, FixedValue} ->
                {TermValueEncoder1, {{fixed, byte_size(FixedValue)}, FixedValue}};
            ScalarInner ->
                {TermValueEncoder1, ScalarInner}
        end,
    TermValueEncoderModule3 = TermValueEncoder3#argo_term_value_encoder.encoder_module,
    TermValueEncoderState3 = TermValueEncoder3#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState4, Result} =
        TermValueEncoderModule3:encode_scalar(TermValueEncoderState3, ScalarWireTypeHint, ScalarTermValue1),
    TermValueEncoder4 = maybe_update_encoder_state(TermValueEncoder3, TermValueEncoderState4),
    case Result of
        {ok, ScalarTermValue2} ->
            {TermValueEncoder4, ScalarTermValue2}
    end.

-spec encode_value(TermValueEncoder, Value) -> {TermValueEncoder, TermValue} when
    TermValueEncoder :: t(),
    Value :: argo_value:t(),
    TermValue :: argo_term:term_value().
encode_value(TermValueEncoder1 = #argo_term_value_encoder{}, Value = #argo_value{}) ->
    case Value#argo_value.inner of
        ScalarValue = #argo_scalar_value{} ->
            encode_scalar_value(TermValueEncoder1, ScalarValue);
        BlockValue = #argo_block_value{} ->
            encode_block_value(TermValueEncoder1, BlockValue);
        NullableValue = #argo_nullable_value{} ->
            encode_nullable_value(TermValueEncoder1, NullableValue);
        ArrayValue = #argo_array_value{} ->
            encode_array_value(TermValueEncoder1, ArrayValue);
        RecordValue = #argo_record_value{} ->
            encode_record_value(TermValueEncoder1, RecordValue);
        DescValue = #argo_desc_value{} ->
            encode_desc_value(TermValueEncoder1, DescValue);
        ErrorValue = #argo_error_value{} ->
            encode_error_value(TermValueEncoder1, ErrorValue);
        ExtensionsValue = #argo_extensions_value{} ->
            encode_extensions_value(TermValueEncoder1, ExtensionsValue);
        PathValue = #argo_path_value{} ->
            encode_path_value(TermValueEncoder1, PathValue)
    end.

% %%%=============================================================================
% %%% Errors API functions
% %%%=============================================================================

% %% @private
% -compile({inline, [error_with_info/3]}).
% -spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
% error_with_info(Reason, Args, Cause) ->
%     erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

% %% @private
% -compile({inline, [error_scalar_type_mismatch/3]}).
% -spec error_scalar_type_mismatch(Args, ScalarHint, Scalar) -> no_return() when
%     Args :: [dynamic()], ScalarHint :: scalar_wire_type_hint(), Scalar :: dynamic().
% error_scalar_type_mismatch(Args, ScalarHint, Scalar) ->
%     case ScalarHint of
%         string ->
%             error_with_info(badarg, Args, #{
%                 3 => {mismatch, expected_string, Scalar}
%             });
%         boolean ->
%             error_with_info(badarg, Args, #{
%                 3 => {mismatch, expected_boolean, Scalar}
%             });
%         varint ->
%             error_with_info(badarg, Args, #{
%                 3 => {mismatch, expected_integer, Scalar}
%             });
%         float64 ->
%             error_with_info(badarg, Args, #{
%                 3 => {mismatch, expected_float, Scalar}
%             });
%         bytes ->
%             error_with_info(badarg, Args, #{
%                 3 => {mismatch, expected_bytes, Scalar}
%             });
%         {fixed, Length} ->
%             error_with_info(badarg, Args, #{
%                 3 => {mismatch, {expected_fixed, Length}, Scalar}
%             });
%         desc ->
%             error_with_info(badarg, Args, #{
%                 3 => {mismatch, expected_desc, Scalar}
%             })
%     end.

% %% @private
% -compile({inline, [error_failed_to_decode/3]}).
% -spec error_failed_to_decode(Args, Type, ErrorReason) -> no_return() when
%     Args :: {TermValueEncoder, TermValue},
%     TermValueEncoder :: t(),
%     TermValue :: argo_term:term_value(),
%     Type :: error | extensions,
%     ErrorReason :: error_reason().
% error_failed_to_decode({TermValueEncoder, TermValue}, Type, ErrorReason) ->
%     Args = [TermValueEncoder, TermValue],
%     error_with_info(badarg, Args, #{
%         2 => {failed_to_decode, Type, ErrorReason, TermValue}
%     }).

% %% @private
% -compile({inline, [error_type_mismatch/2]}).
% -spec error_type_mismatch(Args, Expected) -> no_return() when
%     Args :: {TermValueEncoder, TermValue},
%     TermValueEncoder :: t(),
%     TermValue :: argo_term:term_value(),
%     Expected :: error | extensions.
% error_type_mismatch({TermValueEncoder, TermValue}, Expected) ->
%     Args = [TermValueEncoder, TermValue],
%     error_with_info(badarg, Args, #{
%         2 => {type_mismatch, Expected, TermValue}
%     }).

% -spec format_error(dynamic(), dynamic()) -> dynamic().
% format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
%     ErrorInfo = proplists:get_value(error_info, Info, #{}),
%     ErrorDescription1 = maps:get(cause, ErrorInfo),
%     ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
%     ErrorDescription2.

% %% @private
% -spec format_error_description(dynamic(), dynamic()) -> dynamic().
% format_error_description(_Key, expected_array) ->
%     "expected array";
% format_error_description(_Key, expected_boolean) ->
%     "expected boolean";
% format_error_description(_Key, expected_bytes) ->
%     "expected bytes";
% format_error_description(_Key, expected_desc) ->
%     "expected self-describing type";
% format_error_description(_Key, expected_extensions) ->
%     "expected extensions object";
% format_error_description(_Key, {expected_fixed, Length}) ->
%     io_lib:format("expected string of fixed-length ~w", [Length]);
% format_error_description(_Key, expected_float) ->
%     "expected number as float";
% format_error_description(_Key, expected_integer) ->
%     "expected number as integer";
% format_error_description(_Key, expected_object) ->
%     "expected object";
% format_error_description(_Key, expected_string) ->
%     "expected string";
% format_error_description(_Key, {failed_to_decode, Type, ErrorReason, TermValue}) ->
%     io_lib:format("failed to decode ~ts for reason ~0tP: ~0tP", [Type, ErrorReason, 5, TermValue, 5]);
% format_error_description(_Key, {failed_to_decode_scalar, TermValue}) ->
%     io_lib:format("failed to decode Scalar: ~0tP", [TermValue, 5]);
% format_error_description(Key, {mismatch, Expected, Actual}) ->
%     io_lib:format("~ts, but was: ~0tp", [format_error_description(Key, Expected), Actual]);
% format_error_description(_Key, {required_object_key_missing, Key}) ->
%     io_lib:format("required JSON object key is missing: ~0tp", [Key]);
% format_error_description(_Key, {type_mismatch, Expected, TermValue}) ->
%     io_lib:format("expected ~ts, but was ~0tP", [Expected, TermValue, 5]);
% format_error_description(_Key, Value) ->
%     Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec encode_array_value_next(TermValueEncoder, ArrayValueIterator, ArrayValueHint, ArrayTermValue) ->
    {TermValueEncoder, ArrayTermValue}
when
    TermValueEncoder :: t(),
    ArrayValueIterator :: argo_array_value:iterator(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ArrayTermValue :: argo_term:term_value().
encode_array_value_next(
    TermValueEncoder1 = #argo_term_value_encoder{},
    ArrayValueIterator1,
    ArrayValueHint1 = #{index := Index},
    ArrayTermValue1
) ->
    case argo_array_value:next(ArrayValueIterator1) of
        none ->
            encode_array_value_stop(TermValueEncoder1, ArrayValueHint1, ArrayTermValue1);
        {Index, ItemValue, ArrayValueIterator2} ->
            {TermValueEncoder2, ItemTermValue} = encode_value(TermValueEncoder1, ItemValue),
            TermValueEncoderModule = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule:encode_array_next(TermValueEncoderState2, ArrayValueHint1, ItemTermValue, ArrayTermValue1),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, ArrayTermValue2} ->
                    ArrayValueHint2 = ArrayValueHint1#{index := Index + 1},
                    encode_array_value_next(TermValueEncoder3, ArrayValueIterator2, ArrayValueHint2, ArrayTermValue2)
            end
    end.

%% @private
-spec encode_array_value_stop(TermValueEncoder, ArrayValueHint, ArrayTermValue) ->
    {TermValueEncoder, ArrayTermValue}
when
    TermValueEncoder :: t(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ArrayTermValue :: argo_term:term_value().
encode_array_value_stop(TermValueEncoder1 = #argo_term_value_encoder{}, ArrayValueHint, ArrayTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_array_stop(TermValueEncoderState1, ArrayValueHint, ArrayTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, ArrayTermValue2} ->
            {TermValueEncoder2, ArrayTermValue2}
    end.

%% @private
-spec encode_desc_value_list(TermValueEncoder, DescList) -> {TermValueEncoder, DescListTermValue} when
    TermValueEncoder :: t(),
    DescList :: argo_desc_value:desc_list(),
    DescListTermValue :: argo_term:term_value().
encode_desc_value_list(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule,
        encoder_state = TermValueEncoderState1
    },
    DescList
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_desc_list(TermValueEncoderState1, DescList),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, DescListTermValue} ->
            DescListIterator = {0, DescList},
            encode_desc_value_list_next(TermValueEncoder2, DescListIterator, DescListTermValue)
    end.

%% @private
-spec encode_desc_value_list_next(TermValueEncoder, DescListIterator, DescListTermValue) ->
    {TermValueEncoder, DescListTermValue}
when
    TermValueEncoder :: t(),
    DescListIterator :: {Index, DescList},
    Index :: non_neg_integer(),
    DescList :: [DescValue],
    DescValue :: argo_desc_value:t(),
    DescListTermValue :: argo_term:term_value().
encode_desc_value_list_next(TermValueEncoder1 = #argo_term_value_encoder{}, DescListIterator1, DescListTermValue1) ->
    case DescListIterator1 of
        {_Index, []} ->
            encode_desc_value_list_stop(TermValueEncoder1, DescListTermValue1);
        {Index, [DescValue | DescList]} ->
            DescValueListHint = argo_term:desc_value_list_hint(DescValue, Index),
            {TermValueEncoder2, DescTermValue} = encode_desc_value(TermValueEncoder1, DescValue),
            TermValueEncoderModule = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule:encode_desc_list_next(TermValueEncoderState2, DescValueListHint, DescTermValue, DescListTermValue1),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, DescListTermValue2} ->
                    DescListIterator2 = {Index + 1, DescList},
                    encode_desc_value_list_next(TermValueEncoder3, DescListIterator2, DescListTermValue2)
            end
    end.

%% @private
-spec encode_desc_value_list_stop(TermValueEncoder, DescListTermValue) -> {TermValueEncoder, DescListTermValue} when
    TermValueEncoder :: t(),
    DescListTermValue :: argo_term:term_value().
encode_desc_value_list_stop(TermValueEncoder1 = #argo_term_value_encoder{}, DescListTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_desc_list_stop(TermValueEncoderState1, DescListTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, DescListTermValue2} ->
            {TermValueEncoder2, DescListTermValue2}
    end.

%% @private
-spec encode_desc_value_object(TermValueEncoder, DescObject) -> {TermValueEncoder, DescObjectTermValue} when
    TermValueEncoder :: t(),
    DescObject :: argo_desc_value:desc_object(),
    DescObjectTermValue :: argo_term:term_value().
encode_desc_value_object(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule1,
        encoder_state = TermValueEncoderState1
    },
    DescObject
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule1:encode_desc_object(TermValueEncoderState1, DescObject),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, DescObjectTermValue} ->
            DescObjectIterator = argo_index_map:iterator(DescObject),
            encode_desc_value_object_next(TermValueEncoder2, DescObjectIterator, DescObjectTermValue)
    end.

%% @private
-spec encode_desc_value_object_next(TermValueEncoder, DescObjectIterator, DescObjectTermValue) ->
    {TermValueEncoder, DescObjectTermValue}
when
    TermValueEncoder :: t(),
    DescObjectIterator :: argo_index_map:iterator(DescKey, DescValue),
    DescKey :: unicode:unicode_binary(),
    DescValue :: argo_desc_value:t(),
    DescObjectTermValue :: argo_term:term_value().
encode_desc_value_object_next(
    TermValueEncoder1 = #argo_term_value_encoder{}, DescObjectIterator1, DescObjectTermValue1
) ->
    case argo_index_map:next(DescObjectIterator1) of
        none ->
            encode_desc_value_object_stop(TermValueEncoder1, DescObjectTermValue1);
        {Index, DescKey, DescValue, DescObjectIterator2} ->
            DescValueObjectHint = argo_term:desc_value_object_hint(DescValue, Index, DescKey),
            {TermValueEncoder2, DescTermValue} = encode_desc_value(TermValueEncoder1, DescValue),
            TermValueEncoderModule2 = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule2:encode_desc_object_next(TermValueEncoderState2, DescValueObjectHint, DescTermValue, DescObjectTermValue1),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, DescObjectTermValue2} ->
                    encode_desc_value_object_next(TermValueEncoder3, DescObjectIterator2, DescObjectTermValue2)
            end
    end.

%% @private
-spec encode_desc_value_object_stop(TermValueEncoder, DescObjectTermValue) ->
    {TermValueEncoder, DescObjectTermValue}
when
    TermValueEncoder :: t(),
    DescObjectTermValue :: argo_term:term_value().
encode_desc_value_object_stop(TermValueEncoder1 = #argo_term_value_encoder{}, DescObjectTermValue1) ->
    TermValueEncoderModule1 = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule1:encode_desc_object_stop(TermValueEncoderState1, DescObjectTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, DescObjectTermValue2} ->
            {TermValueEncoder2, DescObjectTermValue2}
    end.

%% @private
-spec encode_desc_value_scalar(TermValueEncoder, DescScalar) -> {TermValueEncoder, DescScalarTermValue} when
    TermValueEncoder :: t(),
    DescScalar :: argo_desc_value:inner_scalar(),
    DescScalarTermValue :: argo_term:term_value().
encode_desc_value_scalar(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule1,
        encoder_state = TermValueEncoderState1
    },
    DescScalar
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule1:encode_desc_scalar(TermValueEncoderState1, DescScalar),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, DescScalarTermValue} ->
            {TermValueEncoder2, DescScalarTermValue}
    end.

%% @private
-spec encode_error_value_extensions(TermValueEncoder, OptionExtensionsValue, ErrorTermValue) ->
    {TermValueEncoder, ErrorTermValue}
when
    TermValueEncoder :: t(),
    OptionExtensionsValue :: argo_types:option(ExtensionsValue),
    ExtensionsValue :: argo_extensions_value:t(),
    ErrorTermValue :: argo_term:term_value().
encode_error_value_extensions(TermValueEncoder1 = #argo_term_value_encoder{}, OptionExtensionsValue, ErrorTermValue1) ->
    {TermValueEncoder3, OptionExtensionsTermValue} =
        case OptionExtensionsValue of
            none ->
                {TermValueEncoder1, none};
            {some, ExtensionsValue} ->
                {TermValueEncoder2, PathTermValue} = encode_extensions_value(TermValueEncoder1, ExtensionsValue),
                {TermValueEncoder2, {some, PathTermValue}}
        end,
    TermValueEncoderModule3 = TermValueEncoder3#argo_term_value_encoder.encoder_module,
    TermValueEncoderState3 = TermValueEncoder3#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState4, Result} =
        TermValueEncoderModule3:encode_error_extensions(TermValueEncoderState3, OptionExtensionsTermValue, ErrorTermValue1),
    TermValueEncoder4 = maybe_update_encoder_state(TermValueEncoder3, TermValueEncoderState4),
    case Result of
        {ok, ErrorTermValue2} ->
            {TermValueEncoder4, ErrorTermValue2}
    end.

%% @private
-spec encode_error_value_locations(TermValueEncoder, OptionLocationsValue, ErrorTermValue) ->
    {TermValueEncoder, ErrorTermValue}
when
    TermValueEncoder :: t(),
    OptionLocationsValue :: argo_types:option(LocationsValue),
    LocationsValue :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    ErrorTermValue :: argo_term:term_value().
encode_error_value_locations(TermValueEncoder1 = #argo_term_value_encoder{}, OptionLocationsValue, ErrorTermValue1) ->
    {TermValueEncoder3, OptionLocationsTermValue} =
        case OptionLocationsValue of
            none ->
                {TermValueEncoder1, none};
            {some, LocationsValue} ->
                {TermValueEncoder2, PathTermValue} = encode_locations_value(TermValueEncoder1, LocationsValue),
                {TermValueEncoder2, {some, PathTermValue}}
        end,
    TermValueEncoderModule3 = TermValueEncoder3#argo_term_value_encoder.encoder_module,
    TermValueEncoderState3 = TermValueEncoder3#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState4, Result} =
        TermValueEncoderModule3:encode_error_locations(
            TermValueEncoderState3, OptionLocationsTermValue, ErrorTermValue1
        ),
    TermValueEncoder4 = maybe_update_encoder_state(TermValueEncoder3, TermValueEncoderState4),
    case Result of
        {ok, ErrorTermValue2} ->
            {TermValueEncoder4, ErrorTermValue2}
    end.

%% @private
-spec encode_error_value_message(TermValueEncoder, Message, ErrorTermValue) -> {TermValueEncoder, ErrorTermValue} when
    TermValueEncoder :: t(), Message :: unicode:unicode_binary(), ErrorTermValue :: argo_term:term_value().
encode_error_value_message(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule, encoder_state = TermValueEncoderState1
    },
    Message,
    ErrorTermValue1
) when is_binary(Message) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_error_message(TermValueEncoderState1, Message, ErrorTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, ErrorTermValue2} ->
            {TermValueEncoder2, ErrorTermValue2}
    end.

%% @private
-spec encode_error_value_path(TermValueEncoder, OptionPathValue, ErrorTermValue) ->
    {TermValueEncoder, ErrorTermValue}
when
    TermValueEncoder :: t(),
    OptionPathValue :: argo_types:option(PathValue),
    PathValue :: argo_path_value:t(),
    ErrorTermValue :: argo_term:term_value().
encode_error_value_path(TermValueEncoder1 = #argo_term_value_encoder{}, OptionPathValue, ErrorTermValue1) ->
    {TermValueEncoder3, OptionPathTermValue} =
        case OptionPathValue of
            none ->
                {TermValueEncoder1, none};
            {some, PathValue} ->
                {TermValueEncoder2, PathTermValue} = encode_path_value(TermValueEncoder1, PathValue),
                {TermValueEncoder2, {some, PathTermValue}}
        end,
    TermValueEncoderModule3 = TermValueEncoder3#argo_term_value_encoder.encoder_module,
    TermValueEncoderState3 = TermValueEncoder3#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState4, Result} =
        TermValueEncoderModule3:encode_error_path(TermValueEncoderState3, OptionPathTermValue, ErrorTermValue1),
    TermValueEncoder4 = maybe_update_encoder_state(TermValueEncoder3, TermValueEncoderState4),
    case Result of
        {ok, ErrorTermValue2} ->
            {TermValueEncoder4, ErrorTermValue2}
    end.

%% @private
-spec encode_error_value_stop(TermValueEncoder, ErrorTermValue) -> {TermValueEncoder, ErrorTermValue} when
    TermValueEncoder :: t(),
    ErrorTermValue :: argo_term:term_value().
encode_error_value_stop(TermValueEncoder1 = #argo_term_value_encoder{}, ErrorTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_error_stop(TermValueEncoderState1, ErrorTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, ErrorTermValue2} ->
            {TermValueEncoder2, ErrorTermValue2}
    end.

%% @private
-spec encode_extensions_value_next(TermValueEncoder, ExtensionsValueIterator, ExtensionsTermValue) ->
    {TermValueEncoder, ExtensionsTermValue}
when
    TermValueEncoder :: t(),
    ExtensionsValueIterator :: argo_extensions_value:iterator(),
    ExtensionsTermValue :: argo_term:term_value().
encode_extensions_value_next(
    TermValueEncoder1 = #argo_term_value_encoder{}, ExtensionsValueIterator1, ExtensionsTermValue1
) ->
    case argo_extensions_value:next(ExtensionsValueIterator1) of
        none ->
            encode_extensions_value_stop(TermValueEncoder1, ExtensionsTermValue1);
        {Index, ExtensionKey, DescValue, ExtensionsValueIterator2} ->
            DescValueObjectHint = argo_term:desc_value_object_hint(DescValue, Index, ExtensionKey),
            {TermValueEncoder2, DescTermValue} = encode_desc_value(TermValueEncoder1, DescValue),
            TermValueEncoderModule = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule:encode_extensions_next(TermValueEncoderState2, DescValueObjectHint, DescTermValue, ExtensionsTermValue1),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, ExtensionsTermValue2} ->
                    encode_extensions_value_next(TermValueEncoder3, ExtensionsValueIterator2, ExtensionsTermValue2)
            end
    end.

%% @private
-spec encode_extensions_value_stop(TermValueEncoder, ExtensionsTermValue) ->
    {TermValueEncoder, ExtensionsTermValue}
when
    TermValueEncoder :: t(),
    ExtensionsTermValue :: argo_term:term_value().
encode_extensions_value_stop(TermValueEncoder1 = #argo_term_value_encoder{}, ExtensionsTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_extensions_stop(TermValueEncoderState1, ExtensionsTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, ExtensionsTermValue2} ->
            {TermValueEncoder2, ExtensionsTermValue2}
    end.

%% @private
-spec encode_location_value_column(TermValueEncoder, Column, LocationTermValue) ->
    {TermValueEncoder, LocationTermValue}
when
    TermValueEncoder :: t(), Column :: argo_types:varint(), LocationTermValue :: argo_term:term_value().
encode_location_value_column(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule,
        encoder_state = TermValueEncoderState1
    },
    Column,
    LocationTermValue1
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_location_column(TermValueEncoderState1, Column, LocationTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, LocationTermValue2} ->
            {TermValueEncoder2, LocationTermValue2}
    end.

%% @private
-spec encode_location_value_line(TermValueEncoder, Line, LocationTermValue) ->
    {TermValueEncoder, LocationTermValue}
when
    TermValueEncoder :: t(), Line :: argo_types:varint(), LocationTermValue :: argo_term:term_value().
encode_location_value_line(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule,
        encoder_state = TermValueEncoderState1
    },
    Line,
    LocationTermValue1
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_location_line(TermValueEncoderState1, Line, LocationTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, LocationTermValue2} ->
            {TermValueEncoder2, LocationTermValue2}
    end.

%% @private
-spec encode_location_value_stop(TermValueEncoder, LocationTermValue) ->
    {TermValueEncoder, LocationTermValue}
when
    TermValueEncoder :: t(),
    LocationTermValue :: argo_term:term_value().
encode_location_value_stop(TermValueEncoder1 = #argo_term_value_encoder{}, LocationTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_location_stop(TermValueEncoderState1, LocationTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, LocationTermValue2} ->
            {TermValueEncoder2, LocationTermValue2}
    end.

%% @private
-spec encode_locations_value(TermValueEncoder, LocationsValue) -> {TermValueEncoder, LocationsTermValue} when
    TermValueEncoder :: t(),
    LocationsValue :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    LocationsTermValue :: argo_term:term_value().
encode_locations_value(
    TermValueEncoder1 = #argo_term_value_encoder{
        encoder_module = TermValueEncoderModule,
        encoder_state = TermValueEncoderState1
    },
    LocationsValue
) ->
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_locations(TermValueEncoderState1, LocationsValue),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, LocationsTermValue} ->
            LocationsValueIterator = {0, LocationsValue},
            encode_locations_value_next(TermValueEncoder2, LocationsValueIterator, LocationsTermValue)
    end.

%% @private
-spec encode_locations_value_next(TermValueEncoder, LocationsValueIterator, LocationsTermValue) ->
    {TermValueEncoder, LocationsTermValue}
when
    TermValueEncoder :: t(),
    LocationsValueIterator :: {Index, LocationsValue},
    Index :: non_neg_integer(),
    LocationsValue :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    LocationsTermValue :: argo_term:term_value().
encode_locations_value_next(
    TermValueEncoder1 = #argo_term_value_encoder{}, LocationsValueIterator1, LocationsTermValue1
) ->
    case LocationsValueIterator1 of
        {_Index, []} ->
            encode_locations_value_stop(TermValueEncoder1, LocationsTermValue1);
        {Index, [LocationValue | LocationsValue]} ->
            {TermValueEncoder2, LocationTermValue} = encode_location_value(TermValueEncoder1, LocationValue),
            TermValueEncoderModule2 = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule2:encode_locations_next(
                    TermValueEncoderState2, Index, LocationTermValue, LocationsTermValue1
                ),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, LocationsTermValue2} ->
                    LocationsValueIterator2 = {Index + 1, LocationsValue},
                    encode_locations_value_next(TermValueEncoder3, LocationsValueIterator2, LocationsTermValue2)
            end
    end.

%% @private
-spec encode_locations_value_stop(TermValueEncoder, LocationsTermValue) -> {TermValueEncoder, LocationsTermValue} when
    TermValueEncoder :: t(),
    LocationsTermValue :: argo_term:term_value().
encode_locations_value_stop(TermValueEncoder1 = #argo_term_value_encoder{}, LocationsTermValue1) ->
    TermValueEncoderModule1 = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule1:encode_locations_stop(TermValueEncoderState1, LocationsTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, LocationsTermValue2} ->
            {TermValueEncoder2, LocationsTermValue2}
    end.

%% @private
-spec encode_nullable_value_field_errors_next(
    TermValueEncoder, NullableWireTypeHint, FieldErrorsValueIterator, FieldErrorsTermValue
) -> {TermValueEncoder, FieldErrorsTermValue} when
    TermValueEncoder :: t(),
    NullableWireTypeHint :: argo_term:wire_type_hint(),
    FieldErrorsValueIterator :: {Index, FieldErrors},
    Index :: non_neg_integer(),
    FieldErrors :: [ErrorValue],
    ErrorValue :: argo_error_value:t(),
    FieldErrorsTermValue :: argo_term:term_value().
encode_nullable_value_field_errors_next(
    TermValueEncoder1 = #argo_term_value_encoder{},
    NullableWireTypeHint,
    FieldErrorsValueIterator1,
    FieldErrorsTermValue1
) ->
    case FieldErrorsValueIterator1 of
        {_Index, []} ->
            encode_nullable_value_field_errors_stop(TermValueEncoder1, FieldErrorsTermValue1);
        {Index, [ErrorValue | FieldErrors]} ->
            {TermValueEncoder2, ErrorTermValue} = encode_error_value(TermValueEncoder1, ErrorValue),
            TermValueEncoderModule = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule:encode_nullable_field_errors_next(TermValueEncoderState2, Index, NullableWireTypeHint, ErrorTermValue, FieldErrorsTermValue1),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, FieldErrorsTermValue2} ->
                    DescListIterator2 = {Index + 1, FieldErrors},
                    encode_nullable_value_field_errors_next(
                        TermValueEncoder3, NullableWireTypeHint, DescListIterator2, FieldErrorsTermValue2
                    )
            end
    end.

%% @private
-spec encode_nullable_value_field_errors_stop(TermValueEncoder, FieldErrorsTermValue) ->
    {TermValueEncoder, FieldErrorsTermValue}
when
    TermValueEncoder :: t(),
    FieldErrorsTermValue :: argo_term:term_value().
encode_nullable_value_field_errors_stop(TermValueEncoder1 = #argo_term_value_encoder{}, FieldErrorsTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_nullable_field_errors_stop(TermValueEncoderState1, FieldErrorsTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, FieldErrorsTermValue2} ->
            {TermValueEncoder2, FieldErrorsTermValue2}
    end.

%% @private
-spec encode_path_value_next(TermValueEncoder, PathIterator, PathTermValue) ->
    {TermValueEncoder, PathTermValue}
when
    TermValueEncoder :: t(),
    PathIterator :: argo_path_value:iterator(),
    PathTermValue :: argo_term:term_value().
encode_path_value_next(TermValueEncoder1 = #argo_term_value_encoder{}, PathIterator1, PathTermValue1) ->
    case argo_path_value:next(PathIterator1) of
        none ->
            encode_path_value_stop(TermValueEncoder1, PathTermValue1);
        {Index, Segment, PathIterator2} ->
            TermValueEncoderModule1 = TermValueEncoder1#argo_term_value_encoder.encoder_module,
            TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState2, Result} =
                TermValueEncoderModule1:encode_path_next(TermValueEncoderState1, Index, Segment, PathTermValue1),
            TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
            case Result of
                {ok, PathTermValue2} ->
                    encode_path_value_next(TermValueEncoder2, PathIterator2, PathTermValue2)
            end
    end.

%% @private
-spec encode_path_value_stop(TermValueEncoder, PathTermValue) -> {TermValueEncoder, PathTermValue} when
    TermValueEncoder :: t(),
    PathTermValue :: argo_term:term_value().
encode_path_value_stop(TermValueEncoder1 = #argo_term_value_encoder{}, PathTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_path_stop(TermValueEncoderState1, PathTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, PathTermValue2} ->
            {TermValueEncoder2, PathTermValue2}
    end.

%% @private
-spec encode_record_value_next(TermValueEncoder, RecordValueIterator, RecordWireTypeHint, RecordTermValue) ->
    {TermValueEncoder, RecordTermValue}
when
    TermValueEncoder :: t(),
    RecordValueIterator :: argo_record_value:iterator(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    RecordTermValue :: argo_term:term_value().
encode_record_value_next(
    TermValueEncoder1 = #argo_term_value_encoder{}, RecordValueIterator1, RecordWireTypeHint, RecordTermValue1
) ->
    case argo_record_value:next(RecordValueIterator1) of
        none ->
            encode_record_value_stop(TermValueEncoder1, RecordWireTypeHint, RecordTermValue1);
        {Index, _FieldName, FieldValue, RecordValueIterator2} ->
            FieldWireTypeHint = argo_term:field_wire_type_hint(FieldValue),
            {TermValueEncoder2, OptionFieldTermValue} = encode_field_value(TermValueEncoder1, FieldValue),
            TermValueEncoderModule2 = TermValueEncoder2#argo_term_value_encoder.encoder_module,
            TermValueEncoderState2 = TermValueEncoder2#argo_term_value_encoder.encoder_state,
            {TermValueEncoderState3, Result} =
                TermValueEncoderModule2:encode_record_next(TermValueEncoderState2, Index, FieldWireTypeHint, OptionFieldTermValue, RecordTermValue1),
            TermValueEncoder3 = maybe_update_encoder_state(TermValueEncoder2, TermValueEncoderState3),
            case Result of
                {ok, RecordTermValue2} ->
                    encode_record_value_next(
                        TermValueEncoder3, RecordValueIterator2, RecordWireTypeHint, RecordTermValue2
                    )
            end
    end.

%% @private
-spec encode_record_value_stop(TermValueEncoder, RecordWireTypeHint, RecordTermValue) ->
    {TermValueEncoder, RecordTermValue}
when
    TermValueEncoder :: t(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    RecordTermValue :: argo_term:term_value().
encode_record_value_stop(TermValueEncoder1 = #argo_term_value_encoder{}, RecordWireTypeHint, RecordTermValue1) ->
    TermValueEncoderModule = TermValueEncoder1#argo_term_value_encoder.encoder_module,
    TermValueEncoderState1 = TermValueEncoder1#argo_term_value_encoder.encoder_state,
    {TermValueEncoderState2, Result} =
        TermValueEncoderModule:encode_record_stop(TermValueEncoderState1, RecordWireTypeHint, RecordTermValue1),
    TermValueEncoder2 = maybe_update_encoder_state(TermValueEncoder1, TermValueEncoderState2),
    case Result of
        {ok, RecordTermValue2} ->
            {TermValueEncoder2, RecordTermValue2}
    end.

%% @private
-compile({inline, [maybe_update_encoder_state/2]}).
-spec maybe_update_encoder_state(TermValueEncoder, TermValueEncoderState) -> TermValueEncoder when
    TermValueEncoder :: t(), TermValueEncoderState :: state().
maybe_update_encoder_state(
    TermValueEncoder1 = #argo_term_value_encoder{encoder_state = TermValueEncoderState1},
    TermValueEncoderState1
) ->
    TermValueEncoder1;
maybe_update_encoder_state(
    TermValueEncoder1 = #argo_term_value_encoder{encoder_state = _TermValueEncoderState1},
    TermValueEncoderState2
) ->
    TermValueEncoder2 = TermValueEncoder1#argo_term_value_encoder{
        encoder_state = TermValueEncoderState2
    },
    TermValueEncoder2.
