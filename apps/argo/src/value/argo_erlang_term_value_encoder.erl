%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_erlang_term_value_encoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-05-13", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-behaviour(argo_term_value_encoder).

-include_lib("argo/include/argo_value.hrl").

%% argo_term_value_encoder callbacks
-export([
    init/1,
    encode_array/3,
    encode_array_next/4,
    encode_array_stop/3,
    encode_block/3,
    encode_desc/3,
    encode_desc_list/2,
    encode_desc_list_next/4,
    encode_desc_list_stop/2,
    encode_desc_object/2,
    encode_desc_object_next/4,
    encode_desc_object_stop/2,
    encode_desc_scalar/2,
    encode_error/2,
    encode_error_extensions/3,
    encode_error_locations/3,
    encode_error_message/3,
    encode_error_path/3,
    encode_error_stop/2,
    encode_extensions/2,
    encode_extensions_next/4,
    encode_extensions_stop/2,
    encode_field/3,
    encode_location/2,
    encode_location_column/3,
    encode_location_line/3,
    encode_location_stop/2,
    encode_locations/2,
    encode_locations_next/4,
    encode_locations_stop/2,
    encode_nullable/3,
    encode_nullable_field_errors/3,
    encode_nullable_field_errors_next/5,
    encode_nullable_field_errors_stop/2,
    encode_path/2,
    encode_path_next/4,
    encode_path_stop/2,
    encode_record/3,
    encode_record_next/5,
    encode_record_stop/3,
    encode_scalar/3
]).

%% Types
-type error_reason() :: dynamic().
-type options() :: #{
    scalar_encoder_module => module(),
    scalar_encoder_options => argo_erlang_term_scalar_encoder:options()
}.
-type result(Ok, Error) :: argo_term_value_encoder:result(Ok, Error).
-type state() :: #argo_erlang_term_value_encoder{}.

-export_type([
    error_reason/0,
    options/0,
    result/2,
    state/0
]).

%%%=============================================================================
%%% argo_term_value_encoder callbacks
%%%=============================================================================

-spec init(Options) -> EncoderState when Options :: options(), EncoderState :: state().
init(Options) ->
    ScalarEncoderModule = maps:get(scalar_encoder_module, Options, undefined),
    ScalarEncoderOptions = maps:get(scalar_encoder_options, Options, #{}),
    ScalarEncoderState = argo_erlang_term_scalar_encoder:init(ScalarEncoderModule, ScalarEncoderOptions),
    EncoderState = #argo_erlang_term_value_encoder{
        scalar_encoder_module = ScalarEncoderModule,
        scalar_encoder_state = ScalarEncoderState
    },
    EncoderState.

-spec encode_array(EncoderState, ArrayValueHint, ArrayValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ArrayValue :: argo_array_value:t(),
    Result :: result(ArrayTermValue, ErrorReason),
    ArrayTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_array(EncoderState, _ArrayValueHint, _ArrayValue) ->
    {EncoderState, {ok, []}}.

-spec encode_array_next(EncoderState, ArrayValueHint, ItemTermValue, ArrayTermValue) ->
    {EncoderState, Result}
when
    EncoderState :: state(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ItemTermValue :: argo_term:term_value(),
    ArrayTermValue :: argo_term:term_value(),
    Result :: result(ArrayTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_array_next(EncoderState, _ArrayValueHint, ItemTermValue, ArrayTermValue1) ->
    ArrayTermValue2 = [ItemTermValue | ArrayTermValue1],
    {EncoderState, {ok, ArrayTermValue2}}.

-spec encode_array_stop(EncoderState, ArrayValueHint, ArrayTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ArrayValueHint :: argo_term:array_value_hint(),
    ArrayTermValue :: argo_term:term_value(),
    Result :: result(ArrayTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_array_stop(EncoderState, _ArrayValueHint, ArrayTermValue1) ->
    ArrayTermValue2 = lists:reverse(ArrayTermValue1),
    {EncoderState, {ok, ArrayTermValue2}}.

-spec encode_block(EncoderState, BlockWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_block(
    EncoderState1 = #argo_erlang_term_value_encoder{
        scalar_encoder_module = ScalarEncoderModule,
        scalar_encoder_state = ScalarEncoderState1
    },
    BlockWireTypeHint,
    ScalarTermValue
) ->
    {ScalarEncoderState2, Result} =
        argo_erlang_term_scalar_encoder:encode_block(
            ScalarEncoderModule, ScalarEncoderState1, BlockWireTypeHint, ScalarTermValue
        ),
    EncoderState2 = maybe_update_scalar_encoder_state(EncoderState1, ScalarEncoderState2),
    {EncoderState2, Result}.

-spec encode_desc(EncoderState, DescValueHint, DescTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescValueHint :: argo_term:desc_value_hint(),
    DescTermValue :: argo_term:term_value(),
    Result :: result(DescTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_desc(EncoderState, _DescValueHint, DescTermValue) ->
    {EncoderState, {ok, DescTermValue}}.

-spec encode_desc_list(EncoderState, DescList) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescList :: argo_desc_value:desc_list(),
    Result :: result(DescListTermValue, ErrorReason),
    DescListTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_desc_list(EncoderState, _DescList) ->
    {EncoderState, {ok, []}}.

-spec encode_desc_list_next(EncoderState, DescValueListHint, DescTermValue, DescListTermValue) ->
    {EncoderState, Result}
when
    EncoderState :: state(),
    DescValueListHint :: argo_term:desc_value_list_hint(),
    DescTermValue :: argo_term:term_value(),
    DescListTermValue :: argo_term:term_value(),
    Result :: result(DescListTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_desc_list_next(EncoderState, _DescValueListHint, DescTermValue, DescListTermValue1) ->
    DescListTermValue2 = [DescTermValue | DescListTermValue1],
    {EncoderState, {ok, DescListTermValue2}}.

-spec encode_desc_list_stop(EncoderState, DescListTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescListTermValue :: argo_term:term_value(),
    Result :: result(DescListTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_desc_list_stop(EncoderState, DescListTermValue1) ->
    DescListTermValue2 = lists:reverse(DescListTermValue1),
    {EncoderState, {ok, DescListTermValue2}}.

-spec encode_desc_object(EncoderState, DescObject) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescObject :: argo_desc_value:desc_object(),
    Result :: result(DescObjectTermValue, ErrorReason),
    DescObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_desc_object(EncoderState, _DescObject) ->
    {EncoderState, {ok, #{}}}.

-spec encode_desc_object_next(EncoderState, DescValueObjectHint, DescTermValue, DescObjectTermValue) ->
    {EncoderState, Result}
when
    EncoderState :: state(),
    DescValueObjectHint :: argo_term:desc_value_object_hint(),
    DescTermValue :: argo_term:term_value(),
    DescObjectTermValue :: argo_term:term_value(),
    Result :: result(DescObjectTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_desc_object_next(
    EncoderState, _DescValueObjectHint = #{key := Key}, DescTermValue, DescObjectTermValue1
) ->
    DescObjectTermValue2 = DescObjectTermValue1#{Key => DescTermValue},
    {EncoderState, {ok, DescObjectTermValue2}}.

-spec encode_desc_object_stop(EncoderState, DescObjectTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescObjectTermValue :: argo_term:term_value(),
    Result :: result(DescObjectTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_desc_object_stop(EncoderState, DescObjectTermValue) ->
    {EncoderState, {ok, DescObjectTermValue}}.

-spec encode_desc_scalar(EncoderState, DescScalar) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescScalar :: argo_desc_value:inner_scalar(),
    Result :: result(DescScalarTermValue, ErrorReason),
    DescScalarTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_desc_scalar(
    EncoderState1 = #argo_erlang_term_value_encoder{
        scalar_encoder_module = ScalarEncoderModule,
        scalar_encoder_state = ScalarEncoderState1
    },
    DescScalar
) ->
    {ScalarEncoderState2, Result} =
        argo_erlang_term_scalar_encoder:encode_desc_scalar(ScalarEncoderModule, ScalarEncoderState1, DescScalar),
    EncoderState2 = maybe_update_scalar_encoder_state(EncoderState1, ScalarEncoderState2),
    {EncoderState2, Result}.

-spec encode_error(EncoderState, ErrorValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ErrorValue :: argo_error_value:t(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_error(EncoderState, _ErrorValue) ->
    {EncoderState, {ok, #{}}}.

-spec encode_error_extensions(EncoderState, OptionExtensionsTermValue, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    OptionExtensionsTermValue :: argo_types:option(ExtensionsTermValue),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_error_extensions(EncoderState, OptionExtensionsTermValue, ErrorTermValue1) ->
    case OptionExtensionsTermValue of
        none ->
            {EncoderState, {ok, ErrorTermValue1}};
        {some, ExtensionsTermValue} ->
            ErrorTermValue2 = ErrorTermValue1#{<<"extensions">> => ExtensionsTermValue},
            {EncoderState, {ok, ErrorTermValue2}}
    end.

-spec encode_error_locations(EncoderState, OptionLocationsTermValue, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    OptionLocationsTermValue :: argo_types:option(LocationsTermValue),
    LocationsTermValue :: argo_term:term_value(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_error_locations(EncoderState, OptionLocationsTermValue, ErrorTermValue1) ->
    case OptionLocationsTermValue of
        none ->
            {EncoderState, {ok, ErrorTermValue1}};
        {some, LocationsTermValue} ->
            ErrorTermValue2 = ErrorTermValue1#{<<"locations">> => LocationsTermValue},
            {EncoderState, {ok, ErrorTermValue2}}
    end.

-spec encode_error_message(EncoderState, Message, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Message :: unicode:unicode_binary(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_error_message(EncoderState, Message, ErrorTermValue1) ->
    ErrorTermValue2 = ErrorTermValue1#{<<"message">> => Message},
    {EncoderState, {ok, ErrorTermValue2}}.

-spec encode_error_path(EncoderState, OptionPathTermValue, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    OptionPathTermValue :: argo_types:option(PathTermValue),
    PathTermValue :: argo_term:term_value(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_error_path(EncoderState, OptionPathTermValue, ErrorTermValue1) ->
    case OptionPathTermValue of
        none ->
            {EncoderState, {ok, ErrorTermValue1}};
        {some, PathTermValue} ->
            ErrorTermValue2 = ErrorTermValue1#{<<"path">> => PathTermValue},
            {EncoderState, {ok, ErrorTermValue2}}
    end.

-spec encode_error_stop(EncoderState, ErrorTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_error_stop(EncoderState, ErrorTermValue) ->
    {EncoderState, {ok, ErrorTermValue}}.

-spec encode_extensions(EncoderState, ExtensionsValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ExtensionsValue :: argo_extensions_value:t(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_extensions(EncoderState, _ExtensionsValue) ->
    {EncoderState, {ok, #{}}}.

-spec encode_extensions_next(EncoderState, DescValueObjectHint, DescTermValue, ExtensionsTermValue) ->
    {EncoderState, Result}
when
    EncoderState :: state(),
    DescValueObjectHint :: argo_term:desc_value_object_hint(),
    DescTermValue :: argo_term:term_value(),
    ExtensionsTermValue :: argo_term:term_value(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_extensions_next(
    EncoderState, _DescValueObjectHint = #{key := Key}, DescTermValue, ExtensionsTermValue1
) ->
    ExtensionsTermValue2 = ExtensionsTermValue1#{Key => DescTermValue},
    {EncoderState, {ok, ExtensionsTermValue2}}.

-spec encode_extensions_stop(EncoderState, ExtensionsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ExtensionsTermValue :: argo_term:term_value(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_extensions_stop(EncoderState, ExtensionsTermValue) ->
    {EncoderState, {ok, ExtensionsTermValue}}.

-spec encode_field(EncoderState, FieldWireTypeHint, OptionTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    FieldWireTypeHint :: argo_term:field_wire_type_hint(),
    OptionTermValue :: argo_types:option(TermValue),
    TermValue :: argo_term:term_value(),
    Result :: result(OptionFieldTermValue, ErrorReason),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_field(EncoderState, _FieldWireTypeHint, OptionTermValue) ->
    OptionFieldTermValue =
        case OptionTermValue of
            none ->
                none;
            {some, TermValue} ->
                FieldTermValue = TermValue,
                {some, FieldTermValue}
        end,
    {EncoderState, {ok, OptionFieldTermValue}}.

-spec encode_location(EncoderState, LocationValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationTermValue, ErrorReason),
    LocationTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_location(EncoderState, _LocationValue) ->
    LocationTermValue = #{},
    {EncoderState, {ok, LocationTermValue}}.

-spec encode_location_column(EncoderState, Column, LocationTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Column :: argo_types:varint(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_location_column(EncoderState, Column, LocationTermValue1) ->
    LocationTermValue2 = LocationTermValue1#{<<"column">> => Column},
    {EncoderState, {ok, LocationTermValue2}}.

-spec encode_location_line(EncoderState, Line, LocationTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Line :: argo_types:varint(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_location_line(EncoderState, Line, LocationTermValue1) ->
    LocationTermValue2 = LocationTermValue1#{<<"line">> => Line},
    {EncoderState, {ok, LocationTermValue2}}.

-spec encode_location_stop(EncoderState, LocationTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_location_stop(EncoderState, LocationTermValue) ->
    {EncoderState, {ok, LocationTermValue}}.

-spec encode_locations(EncoderState, LocationsValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationsValue :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationsTermValue, ErrorReason),
    LocationsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_locations(EncoderState, _LocationsValue) ->
    LocationsTermValue = [],
    {EncoderState, {ok, LocationsTermValue}}.

-spec encode_locations_next(EncoderState, Index, LocationTermValue, LocationsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Index :: argo_types:index(),
    LocationTermValue :: argo_term:term_value(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result(LocationsTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_locations_next(EncoderState, _Index, LocationTermValue, LocationsTermValue1) ->
    LocationsTermValue2 = [LocationTermValue | LocationsTermValue1],
    {EncoderState, {ok, LocationsTermValue2}}.

-spec encode_locations_stop(EncoderState, LocationsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result(LocationsTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_locations_stop(EncoderState, LocationsTermValue1) ->
    LocationsTermValue2 = lists:reverse(LocationsTermValue1),
    {EncoderState, {ok, LocationsTermValue2}}.

-spec encode_nullable(EncoderState, NullableWireTypeHint, NullableTermEnum) -> {EncoderState, Result} when
    EncoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    NullableTermEnum :: null | {non_null, NonNullTermValue} | {field_errors, FieldErrorsTermValue},
    NonNullTermValue :: argo_term:term_value(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(NullableTermValue, ErrorReason),
    NullableTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_nullable(EncoderState, _NullableWireTypeHint, NullableTermEnum) ->
    case NullableTermEnum of
        null ->
            {EncoderState, {ok, null}};
        {non_null, NonNullTermValue} ->
            {EncoderState, {ok, NonNullTermValue}};
        {field_errors, FieldErrorsTermValue} ->
            {EncoderState, {ok, FieldErrorsTermValue}}
    end.

-spec encode_nullable_field_errors(EncoderState, NullableWireTypeHint, NullableValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    NullableValue :: argo_nullable_value:t(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    FieldErrorsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_nullable_field_errors(EncoderState, _NullableWireTypeHint, _NullableValue) ->
    FieldErrorsTermValue = [],
    {EncoderState, {ok, FieldErrorsTermValue}}.

-spec encode_nullable_field_errors_next(
    EncoderState, Index, NullableWireTypeHint, ErrorTermValue, FieldErrorsTermValue
) -> {EncoderState, Result} when
    EncoderState :: state(),
    Index :: argo_types:index(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    ErrorTermValue :: argo_term:term_value(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_nullable_field_errors_next(EncoderState, _Index, _NullableWireTypeHint, ErrorTermValue, FieldErrorsTermValue1) ->
    FieldErrorsTermValue2 = [ErrorTermValue | FieldErrorsTermValue1],
    {EncoderState, {ok, FieldErrorsTermValue2}}.

-spec encode_nullable_field_errors_stop(EncoderState, FieldErrorsTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_nullable_field_errors_stop(EncoderState, FieldErrorsTermValue1) ->
    FieldErrorsTermValue2 = lists:reverse(FieldErrorsTermValue1),
    {EncoderState, {ok, FieldErrorsTermValue2}}.

-spec encode_path(EncoderState, PathValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    PathValue :: argo_path_value:t(),
    Result :: result(PathTermValue, ErrorReason),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_path(EncoderState, _PathValue) ->
    PathTermValue = [],
    {EncoderState, {ok, PathTermValue}}.

-spec encode_path_next(EncoderState, Index, Segment, PathTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    Index :: argo_types:index(),
    Segment :: argo_path_value:segment(),
    PathTermValue :: argo_term:term_value(),
    Result :: result(PathTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_path_next(EncoderState, _Index, Segment, PathTermValue1) ->
    SegmentTerm =
        case Segment of
            {field_name, FieldName} ->
                FieldName;
            {list_index, ListIndex} ->
                ListIndex
        end,
    PathTermValue2 = [SegmentTerm | PathTermValue1],
    {EncoderState, {ok, PathTermValue2}}.

-spec encode_path_stop(EncoderState, PathTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    PathTermValue :: argo_term:term_value(),
    Result :: result(PathTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_path_stop(EncoderState, PathTermValue1) ->
    PathTermValue2 = lists:reverse(PathTermValue1),
    {EncoderState, {ok, PathTermValue2}}.

-spec encode_record(EncoderState, RecordWireTypeHint, RecordValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    RecordValue :: argo_record_value:t(),
    Result :: result(RecordTermValue, ErrorReason),
    RecordTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_record(EncoderState, _RecordWireTypeHint, _RecordValue) ->
    RecordTermValue = #{},
    {EncoderState, {ok, RecordTermValue}}.

-spec encode_record_next(EncoderState, Index, FieldWireTypeHint, OptionFieldTermValue, RecordTermValue) ->
    {EncoderState, Result}
when
    EncoderState :: state(),
    Index :: argo_types:index(),
    FieldWireTypeHint :: argo_term:field_wire_type_hint(),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value(),
    RecordTermValue :: argo_term:term_value(),
    Result :: result(RecordTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_record_next(
    EncoderState, _Index, _FieldWireTypeHint = #{name := Name}, OptionFieldTermValue, RecordTermValue1
) ->
    case OptionFieldTermValue of
        none ->
            {EncoderState, {ok, RecordTermValue1}};
        {some, FieldTermValue} ->
            RecordTermValue2 = RecordTermValue1#{Name => FieldTermValue},
            {EncoderState, {ok, RecordTermValue2}}
    end.

-spec encode_record_stop(EncoderState, RecordWireTypeHint, RecordTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    RecordTermValue :: argo_term:term_value(),
    Result :: result(RecordTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_record_stop(EncoderState, _RecordWireTypeHint, RecordTermValue) ->
    {EncoderState, {ok, RecordTermValue}}.

-spec encode_scalar(EncoderState, ScalarWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(ScalarTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_scalar(
    EncoderState1 = #argo_erlang_term_value_encoder{
        scalar_encoder_module = ScalarEncoderModule,
        scalar_encoder_state = ScalarEncoderState1
    },
    ScalarWireTypeHint,
    ScalarTermValue
) ->
    {ScalarEncoderState2, Result} =
        argo_erlang_term_scalar_encoder:encode_scalar(
            ScalarEncoderModule, ScalarEncoderState1, ScalarWireTypeHint, ScalarTermValue
        ),
    EncoderState2 = maybe_update_scalar_encoder_state(EncoderState1, ScalarEncoderState2),
    {EncoderState2, Result}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-compile({inline, [maybe_update_scalar_encoder_state/2]}).
-spec maybe_update_scalar_encoder_state(EncoderState, ScalarEncoderState) -> EncoderState when
    EncoderState :: state(), ScalarEncoderState :: argo_erlang_term_scalar_encoder:state().
maybe_update_scalar_encoder_state(
    EncoderState1 = #argo_erlang_term_value_encoder{scalar_encoder_state = ScalarEncoderState},
    ScalarEncoderState
) ->
    EncoderState1;
maybe_update_scalar_encoder_state(
    EncoderState1 = #argo_erlang_term_value_encoder{scalar_encoder_state = _ScalarEncoderState1},
    ScalarEncoderState2
) ->
    EncoderState2 = EncoderState1#argo_erlang_term_value_encoder{
        scalar_encoder_state = ScalarEncoderState2
    },
    EncoderState2.
