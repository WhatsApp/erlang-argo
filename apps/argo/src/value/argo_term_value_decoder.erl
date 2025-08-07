%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_term_value_decoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-05-13", modified => "2025-07-15"}.
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
    decode_array_wire_type/3,
    decode_block_wire_type/3,
    decode_desc_wire_type/2,
    decode_error_wire_type/2,
    decode_extensions_wire_type/2,
    decode_field_wire_type/3,
    decode_location_wire_type/2,
    decode_nullable_wire_type/3,
    decode_path_wire_type/2,
    decode_record_wire_type/3,
    decode_scalar_wire_type/3,
    decode_wire_type/3
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type error_reason() :: invalid | type_mismatch | dynamic().
-type options() :: dynamic().
-type result(Ok, Error) :: {ok, Ok} | {error, Error}.
-type state() :: dynamic().
-type t() :: #argo_term_value_decoder{}.

-export_type([
    error_reason/0,
    options/0,
    result/2,
    state/0,
    t/0
]).

%% Behaviour
-callback init(Options) -> DecoderState when Options :: options(), DecoderState :: state().

-callback decode_array(DecoderState, ArrayWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ArrayWireTypeHint :: argo_term:array_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(ArrayTermValue, ErrorReason),
    ArrayTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_array_next(DecoderState, Index, ArrayWireTypeHint, ArrayTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    ArrayWireTypeHint :: argo_term:array_wire_type_hint(),
    ArrayTermValue :: argo_term:term_value(),
    Result :: result({ArrayTermValue, OptionItemTermValue}, ErrorReason),
    OptionItemTermValue :: argo_types:option(ItemTermValue),
    ItemTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_array_stop(DecoderState, ArrayTermValue, ArrayValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ArrayTermValue :: argo_term:term_value(),
    ArrayValue :: argo_array_value:t(),
    Result :: result(ArrayValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_block(DecoderState, BlockWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_block_stop(DecoderState, BlockValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    BlockValue :: argo_block_value:t(),
    Result :: result(DecoderState, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_desc(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result({DescTermValue, DescValueHint}, ErrorReason),
    DescTermValue :: argo_term:term_value(),
    DescValueHint :: argo_term:desc_value_hint(),
    ErrorReason :: error_reason().

-callback decode_desc_list_next(DecoderState, Index, DescListTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    DescListTermValue :: argo_term:term_value(),
    Result :: result({DescListTermValue, OptionItemTermValue}, ErrorReason),
    OptionItemTermValue :: argo_types:option(ItemTermValue),
    ItemTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_desc_list_stop(DecoderState, DescListTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescListTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_desc_object_next(DecoderState, Index, DescObjectTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    DescObjectTermValue :: argo_term:term_value(),
    Result :: result({DescObjectTermValue, OptionEntry}, ErrorReason),
    OptionEntry :: argo_types:option({ObjectKey, ObjectTermValue}),
    ObjectKey :: unicode:unicode_binary(),
    ObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_desc_object_stop(DecoderState, DescObjectTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescObjectTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_desc_scalar(DecoderState, DescValueScalarHint, DescScalarTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescValueScalarHint :: argo_term:desc_value_scalar_hint(),
    DescScalarTermValue :: argo_term:term_value(),
    Result :: result({DescScalarTermValue, DescScalar}, ErrorReason),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: error_reason().

-callback decode_desc_scalar_stop(DecoderState, DescScalarTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescScalarTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_error(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_extensions(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionExtensionsTermValue}, ErrorReason),
    OptionExtensionsTermValue :: argo_types:option(ExtensionsTermValue),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_locations(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionLocationsTermValue}, ErrorReason),
    OptionLocationsTermValue :: argo_types:option(LocationsTermValue),
    LocationsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_message(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionMessageTermValue}, ErrorReason),
    OptionMessageTermValue :: argo_types:option(MessageTermValue),
    MessageTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_path(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionPathTermValue}, ErrorReason),
    OptionPathTermValue :: argo_types:option(PathTermValue),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_stop(DecoderState, ErrorTermValue, ErrorValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    ErrorValue :: argo_error_value:t(),
    Result :: result(ErrorValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_extensions(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_extensions_next(DecoderState, Index, ExtensionsTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    ExtensionsTermValue :: argo_term:term_value(),
    Result :: result({ExtensionsTermValue, OptionEntry}, ErrorReason),
    OptionEntry :: argo_types:option({ObjectKey, ObjectTermValue}),
    ObjectKey :: unicode:unicode_binary(),
    ObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_extensions_stop(DecoderState, ExtensionsTermValue, ExtensionsValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ExtensionsTermValue :: argo_term:term_value(),
    ExtensionsValue :: argo_extensions_value:t(),
    Result :: result(ExtensionsValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_location(DecoderState, LocationTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_location_column(DecoderState, LocationTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result({LocationTermValue, OptionColumnTermValue}, ErrorReason),
    OptionColumnTermValue :: argo_types:option(ColumnTermValue),
    ColumnTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_location_line(DecoderState, LocationTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result({LocationTermValue, OptionLineTermValue}, ErrorReason),
    OptionLineTermValue :: argo_types:option(LineTermValue),
    LineTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_location_stop(DecoderState, LocationTermValue, LocationValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_locations(DecoderState, LocationsTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result(LocationsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_locations_next(DecoderState, Index, LocationsTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result({LocationsTermValue, OptionLocationTermValue}, ErrorReason),
    OptionLocationTermValue :: argo_types:option(LocationTermValue),
    LocationTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_locations_stop(DecoderState, LocationsTermValue, LocationValueList) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationsTermValue :: argo_term:term_value(),
    LocationValueList :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationValueList, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_nullable(DecoderState, NullableWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(NullableTermValue, ErrorReason),
    NullableTermValue :: null | {non_null, NonNullTermValue} | {field_errors, FieldErrorsTermValue},
    NonNullTermValue :: argo_term:term_value(),
    FieldErrorsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_nullable_field_errors(DecoderState, NullableWireTypeHint, FieldErrorsTermValue) ->
    {DecoderState, Result}
when
    DecoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_nullable_field_errors_next(DecoderState, Index, NullableWireTypeHint, FieldErrorsTermValue) ->
    {DecoderState, Result}
when
    DecoderState :: state(),
    Index :: argo_types:index(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result({FieldErrorsTermValue, OptionFieldErrorTermValue}, ErrorReason),
    OptionFieldErrorTermValue :: argo_types:option(FieldErrorTermValue),
    FieldErrorTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_nullable_field_errors_stop(DecoderState, FieldErrorsTermValue, FieldErrorsValue) ->
    {DecoderState, Result}
when
    DecoderState :: state(),
    FieldErrorsTermValue :: argo_term:term_value(),
    FieldErrorsValue :: [FieldErrorValue],
    FieldErrorValue :: argo_error_value:t(),
    Result :: result(FieldErrorsValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_nullable_stop(DecoderState, NullableValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    NullableValue :: argo_nullable_value:t(),
    Result :: result(NullableValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_path(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result(PathTermValue, ErrorReason),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_path_next(DecoderState, Index, PathTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    PathTermValue :: argo_term:term_value(),
    Result :: result({PathTermValue, OptionSegment}, ErrorReason),
    OptionSegment :: argo_types:option(FieldName | ListIndex),
    FieldName :: argo_types:name(),
    ListIndex :: argo_types:index(),
    ErrorReason :: error_reason().

-callback decode_path_stop(DecoderState, PathTermValue, PathValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    PathTermValue :: argo_term:term_value(),
    PathValue :: argo_path_value:t(),
    Result :: result(PathValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_record(DecoderState, RecordWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(RecordTermValue, ErrorReason),
    RecordTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_record_next(DecoderState, Index, FieldWireTypeHint, RecordTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    FieldWireTypeHint :: argo_term:field_wire_type_hint(),
    Index :: argo_types:index(),
    RecordTermValue :: argo_term:term_value(),
    Result :: result({RecordTermValue, OptionFieldTermValue}, ErrorReason),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_record_stop(DecoderState, RecordTermValue, RecordValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    RecordTermValue :: argo_term:term_value(),
    RecordValue :: argo_record_value:t(),
    Result :: result(RecordValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_scalar(DecoderState, ScalarWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(Scalar, ErrorReason),
    Scalar :: boolean() | binary() | DescTermValue | float() | unicode:unicode_binary() | argo_types:i64(),
    DescTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_scalar_stop(DecoderState, ScalarValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ScalarValue :: argo_scalar_value:t(),
    Result :: result(ScalarValue, ErrorReason),
    ErrorReason :: error_reason().

%% Macros
-define(is_desc_value_scalar_hint(X),
    ((X) =:= null) orelse ((X) =:= boolean) orelse ((X) =:= string) orelse ((X) =:= bytes) orelse ((X) =:= int) orelse
        ((X) =:= float)
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec new(TermValueDecoderModule, TermValueDecoderOptions) -> TermValueDecoder when
    TermValueDecoderModule :: module(),
    TermValueDecoderOptions :: options(),
    TermValueDecoder :: t().
new(TermValueDecoderModule, TermValueDecoderOptions) when is_atom(TermValueDecoderModule) ->
    TermValueDecoderState = TermValueDecoderModule:init(TermValueDecoderOptions),
    #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec decode_array_wire_type(TermValueDecoder, ArrayWireType, TermValue) -> {TermValueDecoder, ArrayValue} when
    TermValueDecoder :: t(),
    ArrayWireType :: argo_array_wire_type:t(),
    TermValue :: argo_term:term_value(),
    ArrayValue :: argo_array_value:t().
decode_array_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ArrayWireType = #argo_array_wire_type{},
    TermValue
) ->
    ArrayWireTypeHint = argo_term:array_wire_type_hint(ArrayWireType),
    {TermValueDecoderState2, Result} = TermValueDecoderModule:decode_array(
        TermValueDecoderState1, ArrayWireTypeHint, TermValue
    ),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, ArrayTermValue} ->
            decode_array_wire_type_next(TermValueDecoder2, ArrayWireType, ArrayWireTypeHint, ArrayTermValue, 0, [])
    end.

-spec decode_block_wire_type(TermValueDecoder, BlockWireType, TermValue) -> {TermValueDecoder, BlockValue} when
    TermValueDecoder :: t(),
    BlockWireType :: argo_block_wire_type:t(),
    TermValue :: argo_term:term_value(),
    BlockValue :: argo_block_value:t().
decode_block_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule1,
        decoder_state = TermValueDecoderState1
    },
    BlockWireType = #argo_block_wire_type{'of' = Of},
    TermValue
) ->
    BlockWireTypeHint = argo_term:block_wire_type_hint(BlockWireType),
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule1:decode_block(TermValueDecoderState1, BlockWireTypeHint, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, BlockTermValue} ->
            {TermValueDecoder3, ScalarValue} = decode_scalar_wire_type(TermValueDecoder2, Of, BlockTermValue),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            decode_block_wire_type_stop(TermValueDecoder3, BlockValue);
        % {error, type_mismatch} ->
        %     error_scalar_type_mismatch(
        %         [TermValueDecoder1, BlockWireType, TermValue], ScalarWireTypeHint, {term, TermValue}
        %     );
        {error, _} ->
            error_with_info(badarg, [TermValueDecoder1, BlockWireType, TermValue], #{
                3 => {failed_to_decode_scalar, TermValue}
            })
    end.

-spec decode_desc_wire_type(TermValueDecoder, TermValue) -> {TermValueDecoder, DescValue} when
    TermValueDecoder :: t(), TermValue :: argo_term:term_value(), DescValue :: argo_desc_value:t().
decode_desc_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_desc(TermValueDecoderState1, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {DescListTermValue, list}} ->
            decode_desc_wire_type_list_next(TermValueDecoder2, DescListTermValue, 0, []);
        {ok, {DescObjectTermValue, object}} ->
            decode_desc_wire_type_object_next(TermValueDecoder2, DescObjectTermValue, 0, argo_index_map:new());
        {ok, {DescScalarTermValue, DescValueScalarHint}} when ?is_desc_value_scalar_hint(DescValueScalarHint) ->
            decode_desc_wire_type_scalar(TermValueDecoder2, DescScalarTermValue, DescValueScalarHint);
        {error, _} ->
            error_with_info(badarg, [TermValueDecoder1, TermValue], #{2 => {failed_to_decode_scalar, TermValue}})
    end.

-spec decode_error_wire_type(TermValueDecoder, TermValue) -> {TermValueDecoder, ErrorValue} when
    TermValueDecoder :: t(), TermValue :: argo_term:term_value(), ErrorValue :: argo_error_value:t().
decode_error_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_error(TermValueDecoderState1, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, ErrorTermValue1} ->
            {TermValueDecoder3, ErrorTermValue2, Message} =
                decode_error_wire_type_message(TermValueDecoder2, ErrorTermValue1),
            {TermValueDecoder4, ErrorTermValue3, Locations} =
                decode_error_wire_type_locations(TermValueDecoder3, ErrorTermValue2),
            {TermValueDecoder5, ErrorTermValue4, Path} =
                decode_error_wire_type_path(TermValueDecoder4, ErrorTermValue3),
            {TermValueDecoder6, ErrorTermValue5, Extensions} =
                decode_error_wire_type_extensions(TermValueDecoder5, ErrorTermValue4),
            ErrorValue1 = argo_error_value:new(Message, Locations, Path, Extensions),
            {TermValueDecoder7, ErrorValue2} =
                decode_error_wire_type_stop(TermValueDecoder6, ErrorTermValue5, ErrorValue1),
            {TermValueDecoder7, ErrorValue2};
        {error, type_mismatch} ->
            error_type_mismatch({TermValueDecoder1, TermValue}, error);
        {error, ErrorReason} ->
            error_failed_to_decode({TermValueDecoder1, TermValue}, error, ErrorReason)
    end.

-spec decode_extensions_wire_type(TermValueDecoder, TermValue) -> {TermValueDecoder, ExtensionsValue} when
    TermValueDecoder :: t(), TermValue :: argo_term:term_value(), ExtensionsValue :: argo_extensions_value:t().
decode_extensions_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_extensions(TermValueDecoderState1, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, ExtensionsTermValue} ->
            decode_extensions_wire_type_next(TermValueDecoder2, ExtensionsTermValue, 0, argo_extensions_value:new());
        {error, type_mismatch} ->
            error_type_mismatch({TermValueDecoder1, TermValue}, extensions);
        {error, ErrorReason} ->
            error_failed_to_decode({TermValueDecoder1, TermValue}, extensions, ErrorReason)
    end.

-spec decode_field_wire_type(TermValueDecoder, FieldWireType, OptionTermValue) -> {TermValueDecoder, FieldValue} when
    TermValueDecoder :: t(),
    FieldWireType :: argo_field_wire_type:t(),
    OptionTermValue :: argo_types:option(TermValue),
    TermValue :: argo_term:term_value(),
    FieldValue :: argo_field_value:t().
decode_field_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{},
    FieldWireType = #argo_field_wire_type{name = Name, 'of' = Of},
    OptionTermValue
) ->
    case argo_field_wire_type:is_omittable(FieldWireType) of
        false ->
            case OptionTermValue of
                none ->
                    error_with_info(badarg, [TermValueDecoder1, FieldWireType, OptionTermValue], #{
                        3 => {required_object_key_missing, Name}
                    });
                {some, TermValue} ->
                    {TermValueDecoder2, Value} = decode_wire_type(TermValueDecoder1, Of, TermValue),
                    FieldValue = argo_field_value:required(FieldWireType, Value),
                    {TermValueDecoder2, FieldValue}
            end;
        true ->
            case OptionTermValue of
                none ->
                    FieldValue = argo_field_value:optional(FieldWireType, none),
                    {TermValueDecoder1, FieldValue};
                {some, TermValue} ->
                    {TermValueDecoder2, Value} = decode_wire_type(TermValueDecoder1, Of, TermValue),
                    FieldValue = argo_field_value:optional(FieldWireType, {some, Value}),
                    {TermValueDecoder2, FieldValue}
            end
    end.

-spec decode_location_wire_type(TermValueDecoder, TermValue) -> {TermValueDecoder, LocationValue} when
    TermValueDecoder :: t(),
    TermValue :: argo_term:term_value(),
    LocationValue :: argo_location_value:t().
decode_location_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_location(TermValueDecoderState1, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, LocationTermValue1} ->
            {TermValueDecoder3, LocationTermValue2, Line} =
                decode_location_wire_type_line(TermValueDecoder2, LocationTermValue1),
            {TermValueDecoder4, LocationTermValue3, Column} =
                decode_location_wire_type_column(TermValueDecoder3, LocationTermValue2),
            LocationValue1 = argo_location_value:new(Line, Column),
            {TermValueDecoder5, LocationValue2} =
                decode_location_wire_type_stop(TermValueDecoder4, LocationTermValue3, LocationValue1),
            {TermValueDecoder5, LocationValue2};
        {error, type_mismatch} ->
            error_type_mismatch({TermValueDecoder1, TermValue}, error);
        {error, ErrorReason} ->
            error_failed_to_decode({TermValueDecoder1, TermValue}, error, ErrorReason)
    end.

-spec decode_nullable_wire_type(TermValueDecoder, NullableWireType, TermValue) -> {TermValueDecoder, NullableValue} when
    TermValueDecoder :: t(),
    NullableWireType :: argo_nullable_wire_type:t(),
    TermValue :: argo_term:term_value(),
    NullableValue :: argo_nullable_value:t().
decode_nullable_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    NullableWireType = #argo_nullable_wire_type{'of' = Of},
    TermValue
) ->
    NullableWireTypeHint = argo_term:nullable_wire_type_hint(NullableWireType),
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_nullable(TermValueDecoderState1, NullableWireTypeHint, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, null} ->
            NullableValue = argo_nullable_value:null(NullableWireType),
            decode_nullable_wire_type_stop(TermValueDecoder2, NullableValue);
        {ok, {non_null, NonNullTermValue}} ->
            {TermValueDecoder3, NonNullValue} = decode_wire_type(TermValueDecoder2, Of, NonNullTermValue),
            NullableValue = argo_nullable_value:non_null(NullableWireType, NonNullValue),
            decode_nullable_wire_type_stop(TermValueDecoder3, NullableValue);
        {ok, {field_errors, FieldErrorsTermValue}} ->
            {TermValueDecoder3, FieldErrorsValue} = decode_nullable_wire_type_field_errors(
                TermValueDecoder2, FieldErrorsTermValue
            ),
            NullableValue = argo_nullable_value:field_errors(NullableWireType, FieldErrorsValue),
            decode_nullable_wire_type_stop(TermValueDecoder3, NullableValue)
    end.

-spec decode_path_wire_type(TermValueDecoder, TermValue) -> {TermValueDecoder, PathValue} when
    TermValueDecoder :: t(), TermValue :: argo_term:term_value(), PathValue :: argo_path_value:t().
decode_path_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_path(TermValueDecoderState1, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, PathTermValue} ->
            decode_path_wire_type_next(TermValueDecoder2, PathTermValue, 0, argo_path_value:new())
    end.

-spec decode_scalar_wire_type(TermValueDecoder, ScalarWireType, TermValue) -> {TermValueDecoder, ScalarValue} when
    TermValueDecoder :: t(),
    ScalarWireType :: argo_scalar_wire_type:t(),
    TermValue :: argo_term:term_value(),
    ScalarValue :: argo_scalar_value:t().
decode_scalar_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ScalarWireType = #argo_scalar_wire_type{},
    TermValue
) ->
    ScalarWireTypeHint = argo_term:scalar_wire_type_hint(ScalarWireType),
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_scalar(TermValueDecoderState1, ScalarWireTypeHint, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, StringValue} when is_binary(StringValue) andalso ScalarWireTypeHint =:= string ->
            ScalarValue = argo_scalar_value:string(StringValue),
            decode_scalar_wire_type_stop(TermValueDecoder2, ScalarValue);
        {ok, BooleanValue} when
            is_boolean(BooleanValue) andalso ScalarWireTypeHint =:= boolean
        ->
            ScalarValue = argo_scalar_value:boolean(BooleanValue),
            decode_scalar_wire_type_stop(TermValueDecoder2, ScalarValue);
        {ok, VarintValue} when ?is_i64(VarintValue) andalso ScalarWireTypeHint =:= varint ->
            ScalarValue = argo_scalar_value:varint(VarintValue),
            decode_scalar_wire_type_stop(TermValueDecoder2, ScalarValue);
        {ok, Float64Value} when is_float(Float64Value) andalso ScalarWireTypeHint =:= float64 ->
            ScalarValue = argo_scalar_value:float64(Float64Value),
            decode_scalar_wire_type_stop(TermValueDecoder2, ScalarValue);
        {ok, BytesValue} when is_binary(BytesValue) andalso ScalarWireTypeHint =:= bytes ->
            ScalarValue = argo_scalar_value:bytes(BytesValue),
            decode_scalar_wire_type_stop(TermValueDecoder2, ScalarValue);
        {ok, FixedValue} when
            is_binary(FixedValue) andalso element(1, ScalarWireTypeHint) =:= fixed andalso
                element(2, ScalarWireTypeHint) =:= byte_size(FixedValue)
        ->
            ScalarValue = argo_scalar_value:fixed(FixedValue),
            decode_scalar_wire_type_stop(TermValueDecoder2, ScalarValue);
        {ok, DescTermValue} when ScalarWireTypeHint =:= desc ->
            {TermValueDecoder3, DescValue} = decode_desc_wire_type(TermValueDecoder2, DescTermValue),
            ScalarValue = argo_scalar_value:desc(DescValue),
            decode_scalar_wire_type_stop(TermValueDecoder3, ScalarValue);
        {ok, Scalar} ->
            error_scalar_type_mismatch([TermValueDecoder1, ScalarWireType, TermValue], ScalarWireTypeHint, Scalar);
        {error, type_mismatch} ->
            error_scalar_type_mismatch(
                [TermValueDecoder1, ScalarWireType, TermValue], ScalarWireTypeHint, {term, TermValue}
            );
        {error, _} ->
            error_with_info(badarg, [TermValueDecoder1, ScalarWireType, TermValue], #{
                3 => {failed_to_decode_scalar, TermValue}
            })
    end.

-spec decode_wire_type(TermValueDecoder, WireType, TermValue) -> {TermValueDecoder, Value} when
    TermValueDecoder :: t(),
    WireType :: argo_wire_type:t(),
    TermValue :: argo_term:term_value(),
    Value :: argo_value:t().
decode_wire_type(TermValueDecoder1 = #argo_term_value_decoder{}, WireType = #argo_wire_type{}, TermValue) ->
    case WireType#argo_wire_type.inner of
        ScalarWireType = #argo_scalar_wire_type{} ->
            {TermValueDecoder2, ScalarValue} = decode_scalar_wire_type(TermValueDecoder1, ScalarWireType, TermValue),
            Value = argo_value:scalar(ScalarValue),
            {TermValueDecoder2, Value};
        BlockWireType = #argo_block_wire_type{} ->
            {TermValueDecoder2, BlockValue} = decode_block_wire_type(TermValueDecoder1, BlockWireType, TermValue),
            Value = argo_value:block(BlockValue),
            {TermValueDecoder2, Value};
        NullableWireType = #argo_nullable_wire_type{} ->
            {TermValueDecoder2, NullableValue} = decode_nullable_wire_type(
                TermValueDecoder1, NullableWireType, TermValue
            ),
            Value = argo_value:nullable(NullableValue),
            {TermValueDecoder2, Value};
        ArrayWireType = #argo_array_wire_type{} ->
            {TermValueDecoder2, ArrayValue} = decode_array_wire_type(TermValueDecoder1, ArrayWireType, TermValue),
            Value = argo_value:array(ArrayValue),
            {TermValueDecoder2, Value};
        RecordWireType = #argo_record_wire_type{} ->
            {TermValueDecoder2, RecordValue} = decode_record_wire_type(TermValueDecoder1, RecordWireType, TermValue),
            Value = argo_value:record(RecordValue),
            {TermValueDecoder2, Value};
        #argo_desc_wire_type{} ->
            {TermValueDecoder2, DescValue} = decode_desc_wire_type(TermValueDecoder1, TermValue),
            Value = argo_value:desc(DescValue),
            {TermValueDecoder2, Value};
        #argo_error_wire_type{} ->
            {TermValueDecoder2, ErrorValue} = decode_error_wire_type(TermValueDecoder1, TermValue),
            Value = argo_value:error(ErrorValue),
            {TermValueDecoder2, Value};
        #argo_extensions_wire_type{} ->
            {TermValueDecoder2, ExtensionsValue} = decode_extensions_wire_type(TermValueDecoder1, TermValue),
            Value = argo_value:extensions(ExtensionsValue),
            {TermValueDecoder2, Value};
        #argo_path_wire_type{} ->
            {TermValueDecoder2, PathValue} = decode_path_wire_type(TermValueDecoder1, TermValue),
            Value = argo_value:path(PathValue),
            {TermValueDecoder2, Value}
    end.

-spec decode_record_wire_type(TermValueDecoder, RecordWireType, TermValue) -> {TermValueDecoder, RecordValue} when
    TermValueDecoder :: t(),
    RecordWireType :: argo_record_wire_type:t(),
    TermValue :: argo_term:term_value(),
    RecordValue :: argo_record_value:t().
decode_record_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    RecordWireType = #argo_record_wire_type{fields = Fields},
    TermValue
) ->
    RecordWireTypeHint = argo_term:record_wire_type_hint(RecordWireType),
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_record(TermValueDecoderState1, RecordWireTypeHint, TermValue),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, RecordTermValue} ->
            decode_record_wire_type_next(
                TermValueDecoder2,
                RecordWireType,
                RecordTermValue,
                argo_index_map:iterator(Fields),
                argo_record_value:new()
            )
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
-compile({inline, [error_scalar_type_mismatch/3]}).
-spec error_scalar_type_mismatch(Args, ScalarHint, Scalar) -> no_return() when
    Args :: [dynamic()], ScalarHint :: argo_term:scalar_wire_type_hint(), Scalar :: dynamic().
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
            });
        desc ->
            error_with_info(badarg, Args, #{
                3 => {mismatch, expected_desc, Scalar}
            })
    end.

%% @private
-compile({inline, [error_failed_to_decode/3]}).
-spec error_failed_to_decode(Args, Type, ErrorReason) -> no_return() when
    Args :: {TermValueDecoder, TermValue},
    TermValueDecoder :: t(),
    TermValue :: argo_term:term_value(),
    Type :: error | extensions,
    ErrorReason :: error_reason().
error_failed_to_decode({TermValueDecoder, TermValue}, Type, ErrorReason) ->
    Args = [TermValueDecoder, TermValue],
    error_with_info(badarg, Args, #{
        2 => {failed_to_decode, Type, ErrorReason, TermValue}
    }).

%% @private
-compile({inline, [error_type_mismatch/2]}).
-spec error_type_mismatch(Args, Expected) -> no_return() when
    Args :: {TermValueDecoder, TermValue},
    TermValueDecoder :: t(),
    TermValue :: argo_term:term_value(),
    Expected :: error | extensions.
error_type_mismatch({TermValueDecoder, TermValue}, Expected) ->
    Args = [TermValueDecoder, TermValue],
    error_with_info(badarg, Args, #{
        2 => {type_mismatch, Expected, TermValue}
    }).

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, expected_array) ->
    "expected array";
format_error_description(_Key, expected_boolean) ->
    "expected boolean";
format_error_description(_Key, expected_bytes) ->
    "expected bytes";
format_error_description(_Key, expected_desc) ->
    "expected self-describing type";
format_error_description(_Key, expected_extensions) ->
    "expected extensions object";
format_error_description(_Key, {expected_fixed, Length}) ->
    io_lib:format("expected string of fixed-length ~w", [Length]);
format_error_description(_Key, expected_float) ->
    "expected number as float";
format_error_description(_Key, expected_integer) ->
    "expected number as integer";
format_error_description(_Key, expected_object) ->
    "expected object";
format_error_description(_Key, expected_string) ->
    "expected string";
format_error_description(_Key, {failed_to_decode, Type, ErrorReason, TermValue}) ->
    io_lib:format("failed to decode ~ts for reason ~0tP: ~0tP", [Type, ErrorReason, 5, TermValue, 5]);
format_error_description(_Key, {failed_to_decode_scalar, TermValue}) ->
    io_lib:format("failed to decode Scalar: ~0tP", [TermValue, 5]);
format_error_description(Key, {mismatch, Expected, Actual}) ->
    io_lib:format("~ts, but was: ~0tp", [format_error_description(Key, Expected), Actual]);
format_error_description(_Key, {required_object_key_missing, Key}) ->
    io_lib:format("required JSON object key is missing: ~0tp", [Key]);
format_error_description(_Key, {type_mismatch, Expected, TermValue}) ->
    io_lib:format("expected ~ts, but was ~0tP", [Expected, TermValue, 5]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_array_wire_type_next(TermValueDecoder, ArrayWireType, ArrayWireTypeHint, ArrayTermValue, Index, Items) ->
    {TermValueDecoder, ArrayValue}
when
    TermValueDecoder :: t(),
    ArrayWireType :: argo_array_wire_type:t(),
    ArrayWireTypeHint :: argo_term:array_wire_type_hint(),
    ArrayTermValue :: argo_term:term_value(),
    Index :: argo_types:index(),
    Items :: [Item],
    Item :: argo_value:t(),
    ArrayValue :: argo_array_value:t().
decode_array_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ArrayWireType = #argo_array_wire_type{'of' = Of},
    ArrayWireTypeHint,
    ArrayTermValue1,
    Index1,
    Items1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_array_next(TermValueDecoderState1, Index1, ArrayWireTypeHint, ArrayTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {ArrayTermValue2, {some, ItemTermValue}}} ->
            {TermValueDecoder3, Item} = decode_wire_type(TermValueDecoder2, Of, ItemTermValue),
            Items2 = [Item | Items1],
            Index2 = Index1 + 1,
            decode_array_wire_type_next(
                TermValueDecoder3, ArrayWireType, ArrayWireTypeHint, ArrayTermValue2, Index2, Items2
            );
        {ok, {ArrayTermValue2, none}} ->
            Items2 = lists:reverse(Items1),
            ArrayValue1 = argo_array_value:new(ArrayWireType, Items2),
            {TermValueDecoder3, ArrayValue2} =
                decode_array_wire_type_stop(TermValueDecoder2, ArrayTermValue2, ArrayValue1),
            {TermValueDecoder3, ArrayValue2}
    end.

%% @private
-spec decode_array_wire_type_stop(TermValueDecoder, ArrayTermValue, ArrayValue) -> {TermValueDecoder, ArrayValue} when
    TermValueDecoder :: t(),
    ArrayTermValue :: argo_term:term_value(),
    ArrayValue :: argo_array_value:t().
decode_array_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ArrayTermValue,
    ArrayValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_array_stop(TermValueDecoderState1, ArrayTermValue, ArrayValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, ArrayValue2 = #argo_array_value{}} ->
            {TermValueDecoder2, ArrayValue2}
    end.

%% @private
-spec decode_block_wire_type_stop(TermValueDecoder, BlockValue) -> {TermValueDecoder, BlockValue} when
    TermValueDecoder :: t(),
    BlockValue :: argo_block_value:t().
decode_block_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    BlockValue1 = #argo_block_value{}
) ->
    {TermValueDecoderState2, Result} = TermValueDecoderModule:decode_block_stop(TermValueDecoderState1, BlockValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, BlockValue2 = #argo_block_value{}} ->
            {TermValueDecoder2, BlockValue2}
    end.

%% @private
-spec decode_desc_wire_type_list_next(TermValueDecoder, DescListTermValue, Index, Items) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescListTermValue :: argo_term:term_value(),
    Index :: argo_types:index(),
    Items :: [Item],
    Item :: argo_desc_value:t(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_list_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    DescListTermValue1,
    Index1,
    Items1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_desc_list_next(TermValueDecoderState1, Index1, DescListTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {DescListTermValue2, {some, ItemValue}}} ->
            {TermValueDecoder3, Item} = decode_desc_wire_type(TermValueDecoder2, ItemValue),
            Items2 = [Item | Items1],
            Index2 = Index1 + 1,
            decode_desc_wire_type_list_next(TermValueDecoder3, DescListTermValue2, Index2, Items2);
        {ok, {DescListTermValue2, none}} ->
            Items2 = lists:reverse(Items1),
            DescValue1 = argo_desc_value:list(Items2),
            {TermValueDecoder3, DescValue2} =
                decode_desc_wire_type_list_stop(TermValueDecoder2, DescListTermValue2, DescValue1),
            {TermValueDecoder3, DescValue2}
    end.

%% @private
-spec decode_desc_wire_type_list_stop(TermValueDecoder, DescListTermValue, DescValue) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescListTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_list_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    DescListTermValue,
    DescValue1 = #argo_desc_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_desc_list_stop(TermValueDecoderState1, DescListTermValue, DescValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, DescValue2 = #argo_desc_value{}} ->
            {TermValueDecoder2, DescValue2}
    end.

%% @private
-spec decode_desc_wire_type_object_next(TermValueDecoder, DescObjectTermValue, Index, Object) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescObjectTermValue :: argo_term:term_value(),
    Index :: argo_types:index(),
    Object :: argo_index_map:t(ObjectKey, ObjectValue),
    ObjectKey :: unicode:unicode_binary(),
    ObjectValue :: argo_desc_value:t(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_object_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    DescObjectTermValue1,
    Index1,
    Object1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_desc_object_next(TermValueDecoderState1, Index1, DescObjectTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {DescObjectTermValue2, {some, {ObjectKey, ObjectTermValue}}}} ->
            {TermValueDecoder3, ObjectValue} = decode_desc_wire_type(TermValueDecoder2, ObjectTermValue),
            Object2 = argo_index_map:put(ObjectKey, ObjectValue, Object1),
            Index2 = Index1 + 1,
            decode_desc_wire_type_object_next(TermValueDecoder3, DescObjectTermValue2, Index2, Object2);
        {ok, {DescObjectTermValue2, none}} ->
            DescValue1 = argo_desc_value:object(Object1),
            {TermValueDecoder3, DescValue2} =
                decode_desc_wire_type_object_stop(TermValueDecoder2, DescObjectTermValue2, DescValue1),
            {TermValueDecoder3, DescValue2}
    end.

%% @private
-spec decode_desc_wire_type_object_stop(TermValueDecoder, DescObjectTermValue, DescValue) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescObjectTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_object_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    DescObjectTermValue,
    DescValue1 = #argo_desc_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_desc_object_stop(TermValueDecoderState1, DescObjectTermValue, DescValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, DescValue2 = #argo_desc_value{}} ->
            {TermValueDecoder2, DescValue2}
    end.

%% @private
-spec decode_desc_wire_type_scalar(TermValueDecoder, DescScalarTermValue, DescValueScalarHint) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescScalarTermValue :: argo_term:term_value(),
    DescValueScalarHint :: argo_term:desc_value_scalar_hint(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_scalar(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    DescScalarTermValue1,
    DescValueScalarHint
) when ?is_desc_value_scalar_hint(DescValueScalarHint) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_desc_scalar(TermValueDecoderState1, DescValueScalarHint, DescScalarTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {DescScalarTermValue2, null}} when DescValueScalarHint =:= null ->
            DescValue = argo_desc_value:null(),
            decode_desc_wire_type_scalar_stop(TermValueDecoder2, DescScalarTermValue2, DescValue);
        {ok, {DescScalarTermValue2, {boolean, BooleanValue}}} when
            DescValueScalarHint =:= boolean andalso is_boolean(BooleanValue)
        ->
            DescValue = argo_desc_value:boolean(BooleanValue),
            decode_desc_wire_type_scalar_stop(TermValueDecoder2, DescScalarTermValue2, DescValue);
        {ok, {DescScalarTermValue2, {int, IntValue}}} when DescValueScalarHint =:= int andalso ?is_i64(IntValue) ->
            DescValue = argo_desc_value:int(IntValue),
            decode_desc_wire_type_scalar_stop(TermValueDecoder2, DescScalarTermValue2, DescValue);
        {ok, {DescScalarTermValue2, {float, FloatValue}}} when
            DescValueScalarHint =:= float andalso is_float(FloatValue)
        ->
            DescValue = argo_desc_value:float(FloatValue),
            decode_desc_wire_type_scalar_stop(TermValueDecoder2, DescScalarTermValue2, DescValue);
        {ok, {DescScalarTermValue2, {bytes, BytesValue}}} when
            DescValueScalarHint =:= bytes andalso is_binary(BytesValue)
        ->
            DescValue = argo_desc_value:bytes(BytesValue),
            decode_desc_wire_type_scalar_stop(TermValueDecoder2, DescScalarTermValue2, DescValue);
        {ok, {DescScalarTermValue2, {string, StringValue}}} when
            DescValueScalarHint =:= string andalso is_binary(StringValue)
        ->
            DescValue = argo_desc_value:string(StringValue),
            decode_desc_wire_type_scalar_stop(TermValueDecoder2, DescScalarTermValue2, DescValue)
    end.

%% @private
-spec decode_desc_wire_type_scalar_stop(TermValueDecoder, DescScalarTermValue, DescValue) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescScalarTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_scalar_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    DescScalarTermValue,
    DescValue1 = #argo_desc_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_desc_scalar_stop(TermValueDecoderState1, DescScalarTermValue, DescValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, DescValue2 = #argo_desc_value{}} ->
            {TermValueDecoder2, DescValue2}
    end.

%% @private
-spec decode_error_wire_type_extensions(TermValueDecoder, ErrorTermValue) ->
    {TermValueDecoder, ErrorTermValue, OptionExtensions}
when
    TermValueDecoder :: t(),
    ErrorTermValue :: argo_term:term_value(),
    OptionExtensions :: argo_types:option(Extensions),
    Extensions :: argo_extensions_value:t().
decode_error_wire_type_extensions(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_error_extensions(TermValueDecoderState1, ErrorTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {ErrorTermValue2, {some, ExtensionsTermValue}}} ->
            {TermValueDecoder3, Extensions} =
                decode_extensions_wire_type(TermValueDecoder2, ExtensionsTermValue),
            OptionExtensions = {some, Extensions},
            {TermValueDecoder3, ErrorTermValue2, OptionExtensions};
        {ok, {ErrorTermValue2, none}} ->
            {TermValueDecoder2, ErrorTermValue2, none}
    end.

%% @private
-spec decode_error_wire_type_locations(TermValueDecoder, ErrorTermValue) ->
    {TermValueDecoder, ErrorTermValue, OptionLocations}
when
    TermValueDecoder :: t(),
    ErrorTermValue :: argo_term:term_value(),
    OptionLocations :: argo_types:option(Locations),
    Locations :: [Location],
    Location :: argo_location_value:t().
decode_error_wire_type_locations(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_error_locations(TermValueDecoderState1, ErrorTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {ErrorTermValue2, {some, LocationsTermValue}}} ->
            {TermValueDecoder3, Locations} =
                decode_locations_wire_type(TermValueDecoder2, LocationsTermValue),
            OptionLocations = {some, Locations},
            {TermValueDecoder3, ErrorTermValue2, OptionLocations};
        {ok, {ErrorTermValue2, none}} ->
            {TermValueDecoder2, ErrorTermValue2, none}
    end.

%% @private
-spec decode_error_wire_type_message(TermValueDecoder, ErrorTermValue) ->
    {TermValueDecoder, ErrorTermValue, Message}
when
    TermValueDecoder :: t(), ErrorTermValue :: argo_term:term_value(), Message :: unicode:unicode_binary().
decode_error_wire_type_message(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_error_message(TermValueDecoderState1, ErrorTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {ErrorTermValue2, {some, MessageTermValue}}} ->
            MessageScalarWireType = argo_scalar_wire_type:string(),
            {TermValueDecoder3, MessageScalarValue} =
                decode_scalar_wire_type(TermValueDecoder2, MessageScalarWireType, MessageTermValue),
            #argo_scalar_value{inner = {string, Message}} = MessageScalarValue,
            {TermValueDecoder3, ErrorTermValue2, Message};
        {ok, {_ErrorTermValue2, none}} ->
            error_with_info(badarg, [TermValueDecoder1, ErrorTermValue1], #{
                2 => {required_object_key_missing, <<"message">>}
            })
    end.

%% @private
-spec decode_error_wire_type_path(TermValueDecoder, ErrorTermValue) ->
    {TermValueDecoder, ErrorTermValue, OptionPath}
when
    TermValueDecoder :: t(),
    ErrorTermValue :: argo_term:term_value(),
    OptionPath :: argo_types:option(Path),
    Path :: argo_path_value:t().
decode_error_wire_type_path(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_error_path(TermValueDecoderState1, ErrorTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {ErrorTermValue2, {some, PathTermValue}}} ->
            {TermValueDecoder3, Path} = decode_path_wire_type(TermValueDecoder2, PathTermValue),
            OptionPath = {some, Path},
            {TermValueDecoder3, ErrorTermValue2, OptionPath};
        {ok, {ErrorTermValue2, none}} ->
            {TermValueDecoder2, ErrorTermValue2, none}
    end.

%% @private
-spec decode_error_wire_type_stop(TermValueDecoder, ErrorTermValue, ErrorValue) -> {TermValueDecoder, ErrorValue} when
    TermValueDecoder :: t(), ErrorTermValue :: argo_term:term_value(), ErrorValue :: argo_error_value:t().
decode_error_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ErrorTermValue,
    ErrorValue1 = #argo_error_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_error_stop(TermValueDecoderState1, ErrorTermValue, ErrorValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, ErrorValue2 = #argo_error_value{}} ->
            {TermValueDecoder2, ErrorValue2}
    end.

%% @private
-spec decode_extensions_wire_type_next(TermValueDecoder, ExtensionsTermValue, Index, ExtensionsValue) ->
    {TermValueDecoder, ExtensionsValue}
when
    TermValueDecoder :: t(),
    ExtensionsTermValue :: argo_term:term_value(),
    Index :: argo_types:index(),
    ExtensionsValue :: argo_extensions_value:t().
decode_extensions_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ExtensionsTermValue1,
    Index1,
    ExtensionsValue1 = #argo_extensions_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_extensions_next(TermValueDecoderState1, Index1, ExtensionsTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {ExtensionsTermValue2, {some, {Key, DescTermValue}}}} when is_binary(Key) ->
            {TermValueDecoder3, DescValue} =
                decode_desc_wire_type(TermValueDecoder2, DescTermValue),
            ExtensionsValue2 = argo_extensions_value:insert(ExtensionsValue1, Key, DescValue),
            Index2 = Index1 + 1,
            decode_extensions_wire_type_next(TermValueDecoder3, ExtensionsTermValue2, Index2, ExtensionsValue2);
        {ok, {ExtensionsTermValue2, none}} ->
            {TermValueDecoder3, ExtensionsValue2} =
                decode_extensions_wire_type_stop(TermValueDecoder2, ExtensionsTermValue2, ExtensionsValue1),
            {TermValueDecoder3, ExtensionsValue2}
    end.

%% @private
-spec decode_extensions_wire_type_stop(TermValueDecoder, ExtensionsTermValue, ExtensionsValue) ->
    {TermValueDecoder, ExtensionsValue}
when
    TermValueDecoder :: t(),
    ExtensionsTermValue :: argo_term:term_value(),
    ExtensionsValue :: argo_extensions_value:t().
decode_extensions_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ExtensionsTermValue,
    ExtensionsValue1 = #argo_extensions_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_extensions_stop(TermValueDecoderState1, ExtensionsTermValue, ExtensionsValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, ExtensionsValue2 = #argo_extensions_value{}} ->
            {TermValueDecoder2, ExtensionsValue2}
    end.

%% @private
-spec decode_location_wire_type_column(TermValueDecoder, LocationTermValue) ->
    {TermValueDecoder, LocationTermValue, Column}
when
    TermValueDecoder :: t(), LocationTermValue :: argo_term:term_value(), Column :: integer().
decode_location_wire_type_column(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    LocationTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_location_column(TermValueDecoderState1, LocationTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {LocationTermValue2, {some, ColumnTermValue}}} ->
            ColumnBlockWireType = argo_label:self_describing_blocks_varint(),
            {TermValueDecoder3, #argo_block_value{value = #argo_scalar_value{inner = {varint, Column}}}} =
                decode_block_wire_type(TermValueDecoder2, ColumnBlockWireType, ColumnTermValue),
            {TermValueDecoder3, LocationTermValue2, Column};
        {ok, {_LocationTermValue2, none}} ->
            error_with_info(badarg, [TermValueDecoder1, LocationTermValue1], #{
                2 => {required_object_key_missing, <<"column">>}
            })
    end.

%% @private
-spec decode_location_wire_type_line(TermValueDecoder, LocationTermValue) ->
    {TermValueDecoder, LocationTermValue, Line}
when
    TermValueDecoder :: t(), LocationTermValue :: argo_term:term_value(), Line :: integer().
decode_location_wire_type_line(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    LocationTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_location_line(TermValueDecoderState1, LocationTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {LocationTermValue2, {some, LineTermValue}}} ->
            LineBlockWireType = argo_label:self_describing_blocks_varint(),
            {TermValueDecoder3, #argo_block_value{value = #argo_scalar_value{inner = {varint, Line}}}} =
                decode_block_wire_type(TermValueDecoder2, LineBlockWireType, LineTermValue),
            {TermValueDecoder3, LocationTermValue2, Line};
        {ok, {_LocationTermValue2, none}} ->
            error_with_info(badarg, [TermValueDecoder1, LocationTermValue1], #{
                2 => {required_object_key_missing, <<"line">>}
            })
    end.

%% @private
-spec decode_location_wire_type_stop(TermValueDecoder, LocationTermValue, LocationValue) ->
    {TermValueDecoder, LocationValue}
when
    TermValueDecoder :: t(),
    LocationTermValue :: argo_term:term_value(),
    LocationValue :: argo_location_value:t().
decode_location_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    LocationTermValue,
    LocationValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_location_stop(TermValueDecoderState1, LocationTermValue, LocationValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, LocationValue2 = #argo_location_value{}} ->
            {TermValueDecoder2, LocationValue2}
    end.

%% @private
-spec decode_locations_wire_type(TermValueDecoder, LocationsTermValue) ->
    {TermValueDecoder, Locations}
when
    TermValueDecoder :: t(),
    LocationsTermValue :: argo_term:term_value(),
    Locations :: [Location],
    Location :: argo_location_value:t().
decode_locations_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    LocationsTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_locations(TermValueDecoderState1, LocationsTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, LocationsTermValue2} ->
            decode_locations_wire_type_next(TermValueDecoder2, LocationsTermValue2, 0, [])
    end.

%% @private
-spec decode_locations_wire_type_next(TermValueDecoder, LocationsTermValue, Index, LocationValueList) ->
    {TermValueDecoder, LocationValueList}
when
    TermValueDecoder :: t(),
    LocationsTermValue :: argo_term:term_value(),
    Index :: argo_types:index(),
    LocationValueList :: [LocationValue],
    LocationValue :: argo_location_value:t().
decode_locations_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    LocationsTermValue1,
    Index1,
    LocationValueList1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_locations_next(TermValueDecoderState1, Index1, LocationsTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {LocationsTermValue2, {some, LocationTermValue}}} ->
            {TermValueDecoder3, LocationValue} =
                decode_location_wire_type(TermValueDecoder2, LocationTermValue),
            LocationValueList2 = [LocationValue | LocationValueList1],
            Index2 = Index1 + 1,
            decode_locations_wire_type_next(TermValueDecoder3, LocationsTermValue2, Index2, LocationValueList2);
        {ok, {LocationsTermValue2, none}} ->
            LocationValueList2 = lists:reverse(LocationValueList1),
            {TermValueDecoder3, LocationValueList3} =
                decode_locations_wire_type_stop(TermValueDecoder2, LocationsTermValue2, LocationValueList2),
            {TermValueDecoder3, LocationValueList3}
    end.

%% @private
-spec decode_locations_wire_type_stop(TermValueDecoder, LocationsTermValue, LocationValueList) ->
    {TermValueDecoder, LocationValueList}
when
    TermValueDecoder :: t(),
    LocationsTermValue :: argo_term:term_value(),
    LocationValueList :: [LocationValue],
    LocationValue :: argo_location_value:t().
decode_locations_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    LocationsTermValue,
    LocationValueList1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_locations_stop(TermValueDecoderState1, LocationsTermValue, LocationValueList1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, LocationValueList2 = []} ->
            {TermValueDecoder2, LocationValueList2};
        {ok, LocationValueList2 = [#argo_location_value{} | _]} ->
            {TermValueDecoder2, LocationValueList2}
    end.

%% @private
-spec decode_nullable_wire_type_field_errors(TermValueDecoder, FieldErrorsTermValue) ->
    {TermValueDecoder, FieldErrorsValue}
when
    TermValueDecoder :: t(),
    FieldErrorsTermValue :: argo_term:term_value(),
    FieldErrorsValue :: [FieldErrorValue],
    FieldErrorValue :: argo_error_value:t().
decode_nullable_wire_type_field_errors(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    FieldErrorsTermValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_nullable_field_errors(TermValueDecoderState1, FieldErrorsTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, FieldErrorsTermValue2} ->
            decode_nullable_wire_type_field_errors_next(TermValueDecoder2, FieldErrorsTermValue2, 0, [])
    end.

%% @private
-spec decode_nullable_wire_type_field_errors_next(TermValueDecoder, FieldErrorsTermValue, Index, FieldErrorsValue) ->
    {TermValueDecoder, FieldErrorsValue}
when
    TermValueDecoder :: t(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Index :: argo_types:index(),
    FieldErrorsValue :: [FieldErrorValue],
    FieldErrorValue :: argo_error_value:t().
decode_nullable_wire_type_field_errors_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    FieldErrorsTermValue1,
    Index1,
    FieldErrorsValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_nullable_field_errors_next(TermValueDecoderState1, Index1, FieldErrorsTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {FieldErrorsTermValue2, {some, FieldErrorTermValue}}} ->
            {TermValueDecoder3, FieldErrorValue} = decode_error_wire_type(TermValueDecoder2, FieldErrorTermValue),
            FieldErrorsValue2 = [FieldErrorValue | FieldErrorsValue1],
            Index2 = Index1 + 1,
            decode_nullable_wire_type_field_errors_next(
                TermValueDecoder3, FieldErrorsTermValue2, Index2, FieldErrorsValue2
            );
        {ok, {FieldErrorsTermValue2, none}} ->
            FieldErrorsValue2 = lists:reverse(FieldErrorsValue1),
            {TermValueDecoder3, FieldErrorsValue3} =
                decode_nullable_wire_type_field_errors_stop(
                    TermValueDecoder2, FieldErrorsTermValue2, FieldErrorsValue2
                ),
            {TermValueDecoder3, FieldErrorsValue3}
    end.

%% @private
-spec decode_nullable_wire_type_field_errors_stop(TermValueDecoder, FieldErrorsTermValue, FieldErrorsValue) ->
    {TermValueDecoder, FieldErrorsValue}
when
    TermValueDecoder :: t(),
    FieldErrorsTermValue :: argo_term:term_value(),
    FieldErrorsValue :: [FieldErrorValue],
    FieldErrorValue :: argo_error_value:t().
decode_nullable_wire_type_field_errors_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    FieldErrorsTermValue,
    FieldErrorsValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_nullable_field_errors_stop(
            TermValueDecoderState1, FieldErrorsTermValue, FieldErrorsValue1
        ),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, FieldErrorsValue2} ->
            {TermValueDecoder2, FieldErrorsValue2}
    end.

%% @private
-spec decode_nullable_wire_type_stop(TermValueDecoder, NullableValue) ->
    {TermValueDecoder, NullableValue}
when
    TermValueDecoder :: t(),
    NullableValue :: argo_nullable_value:t().
decode_nullable_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    NullableValue1 = #argo_nullable_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_nullable_stop(TermValueDecoderState1, NullableValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, NullableValue2 = #argo_nullable_value{}} ->
            {TermValueDecoder2, NullableValue2}
    end.

%% @private
-spec decode_path_wire_type_next(TermValueDecoder, PathTermValue, Index, PathValue) ->
    {TermValueDecoder, PathValue}
when
    TermValueDecoder :: t(),
    PathTermValue :: argo_term:term_value(),
    Index :: argo_types:index(),
    PathValue :: argo_path_value:t().
decode_path_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    PathTermValue1,
    Index1,
    PathValue1
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_path_next(TermValueDecoderState1, Index1, PathTermValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, {PathTermValue2, {some, FieldName}}} when is_binary(FieldName) ->
            Index2 = Index1 + 1,
            PathValue2 = argo_path_value:push_field_name(PathValue1, FieldName),
            decode_path_wire_type_next(TermValueDecoder2, PathTermValue2, Index2, PathValue2);
        {ok, {PathTermValue2, {some, ListIndex}}} when is_integer(ListIndex) ->
            Index2 = Index1 + 1,
            PathValue2 = argo_path_value:push_list_index(PathValue1, ListIndex),
            decode_path_wire_type_next(TermValueDecoder2, PathTermValue2, Index2, PathValue2);
        {ok, {PathTermValue2, none}} ->
            {TermValueDecoder3, PathValue2} = decode_path_wire_type_stop(TermValueDecoder2, PathTermValue2, PathValue1),
            {TermValueDecoder3, PathValue2}
    end.

%% @private
-spec decode_path_wire_type_stop(TermValueDecoder, PathTermValue, PathValue) -> {TermValueDecoder, PathValue} when
    TermValueDecoder :: t(), PathTermValue :: argo_term:term_value(), PathValue :: argo_path_value:t().
decode_path_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    PathTermValue,
    PathValue1 = #argo_path_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_path_stop(TermValueDecoderState1, PathTermValue, PathValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, PathValue2 = #argo_path_value{}} ->
            {TermValueDecoder2, PathValue2}
    end.

%% @private
-spec decode_record_wire_type_next(TermValueDecoder, RecordWireType, RecordTermValue, FieldsIterator, RecordValue) ->
    {TermValueDecoder, RecordValue}
when
    TermValueDecoder :: t(),
    RecordWireType :: argo_record_wire_type:t(),
    RecordTermValue :: argo_term:term_value(),
    FieldsIterator :: argo_index_map:iterator(FieldName, FieldWireType),
    FieldName :: argo_types:name(),
    FieldWireType :: argo_field_wire_type:t(),
    RecordValue :: argo_record_value:t().
decode_record_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    RecordWireType = #argo_record_wire_type{},
    RecordTermValue1,
    FieldsIterator1,
    RecordValue1
) ->
    case argo_index_map:next(FieldsIterator1) of
        {Index, FieldName, FieldWireType = #argo_field_wire_type{name = FieldName}, FieldsIterator2} ->
            FieldWireTypeHint = argo_term:field_wire_type_hint(FieldWireType),
            {TermValueDecoderState2, Result} =
                TermValueDecoderModule:decode_record_next(
                    TermValueDecoderState1, Index, FieldWireTypeHint, RecordTermValue1
                ),
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            case Result of
                {ok, {RecordTermValue2, OptionFieldTermValue}} ->
                    {TermValueDecoder3, FieldValue} =
                        decode_field_wire_type(TermValueDecoder2, FieldWireType, OptionFieldTermValue),
                    RecordValue2 = argo_record_value:insert(RecordValue1, FieldValue),
                    decode_record_wire_type_next(
                        TermValueDecoder3, RecordWireType, RecordTermValue2, FieldsIterator2, RecordValue2
                    );
                {error, _} ->
                    error_with_info(
                        badarg, [TermValueDecoder1, RecordWireType, RecordTermValue1, FieldsIterator1, RecordValue1], #{
                            3 => {failed_to_decode_record_field, FieldName}
                        }
                    )
            end;
        none ->
            {TermValueDecoder2, RecordValue2} =
                decode_record_wire_type_stop(TermValueDecoder1, RecordTermValue1, RecordValue1),
            {TermValueDecoder2, RecordValue2}
    end.

%% @private
-spec decode_record_wire_type_stop(TermValueDecoder, RecordTermValue, RecordValue) ->
    {TermValueDecoder, RecordValue}
when
    TermValueDecoder :: t(),
    RecordTermValue :: argo_term:term_value(),
    RecordValue :: argo_record_value:t().
decode_record_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    RecordTermValue,
    RecordValue1 = #argo_record_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_record_stop(TermValueDecoderState1, RecordTermValue, RecordValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, RecordValue2 = #argo_record_value{}} ->
            {TermValueDecoder2, RecordValue2}
    end.

%% @private
-spec decode_scalar_wire_type_stop(TermValueDecoder, ScalarValue) -> {TermValueDecoder, ScalarValue} when
    TermValueDecoder :: t(),
    ScalarValue :: argo_scalar_value:t().
decode_scalar_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule,
        decoder_state = TermValueDecoderState1
    },
    ScalarValue1 = #argo_scalar_value{}
) ->
    {TermValueDecoderState2, Result} =
        TermValueDecoderModule:decode_scalar_stop(TermValueDecoderState1, ScalarValue1),
    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
    case Result of
        {ok, ScalarValue2 = #argo_scalar_value{}} ->
            {TermValueDecoder2, ScalarValue2}
    end.

%% @private
-compile({inline, [maybe_update_decoder_state/2]}).
-spec maybe_update_decoder_state(TermValueDecoder, TermValueDecoderState) -> TermValueDecoder when
    TermValueDecoder :: t(), TermValueDecoderState :: state().
maybe_update_decoder_state(
    TermValueDecoder1 = #argo_term_value_decoder{decoder_state = TermValueDecoderState1},
    TermValueDecoderState1
) ->
    TermValueDecoder1;
maybe_update_decoder_state(
    TermValueDecoder1 = #argo_term_value_decoder{decoder_state = _TermValueDecoderState1},
    TermValueDecoderState2
) ->
    TermValueDecoder2 = TermValueDecoder1#argo_term_value_decoder{
        decoder_state = TermValueDecoderState2
    },
    TermValueDecoder2.
