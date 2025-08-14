%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_erlang_term_value_decoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-05-13", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_term_value_decoder).

-include_lib("argo/include/argo_value.hrl").

%% argo_term_value_decoder callbacks
-export([
    init/1,
    decode_array/3,
    decode_array_next/4,
    decode_array_stop/3,
    decode_block/3,
    decode_block_stop/2,
    decode_desc/2,
    decode_desc_list_next/3,
    decode_desc_list_stop/3,
    decode_desc_object_next/3,
    decode_desc_object_stop/3,
    decode_desc_scalar/3,
    decode_desc_scalar_stop/3,
    decode_error/2,
    decode_error_extensions/2,
    decode_error_locations/2,
    decode_error_message/2,
    decode_error_path/2,
    decode_error_stop/3,
    decode_extensions/2,
    decode_extensions_next/3,
    decode_extensions_stop/3,
    decode_location/2,
    decode_location_column/2,
    decode_location_line/2,
    decode_location_stop/3,
    decode_locations/2,
    decode_locations_next/3,
    decode_locations_stop/3,
    decode_nullable/3,
    decode_nullable_field_errors/3,
    decode_nullable_field_errors_next/4,
    decode_nullable_field_errors_stop/3,
    decode_nullable_stop/2,
    decode_path/2,
    decode_path_next/3,
    decode_path_stop/3,
    decode_record/3,
    decode_record_next/4,
    decode_record_stop/3,
    decode_scalar/3,
    decode_scalar_stop/2
]).

%% Types
-type error_reason() :: argo_term_value_decoder:error_reason().
-type options() :: #{
    scalar_decoder_module => module(),
    scalar_decoder_options => argo_erlang_term_scalar_decoder:options()
}.
-type result(Ok, Error) :: argo_term_value_decoder:result(Ok, Error).
-type state() :: #argo_erlang_term_value_decoder{}.

-export_type([
    error_reason/0,
    options/0,
    result/2,
    state/0
]).

%%%=============================================================================
%%% argo_term_value_decoder callbacks
%%%=============================================================================

-spec init(Options) -> DecoderState when Options :: options(), DecoderState :: state().
init(Options) ->
    ScalarDecoderModule = maps:get(scalar_decoder_module, Options, undefined),
    ScalarDecoderOptions = maps:get(scalar_decoder_options, Options, #{}),
    ScalarDecoderState = argo_erlang_term_scalar_decoder:init(ScalarDecoderModule, ScalarDecoderOptions),
    DecoderState = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState
    },
    DecoderState.

-spec decode_array(DecoderState, ArrayWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ArrayWireTypeHint :: argo_term:array_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(ArrayTermValue, ErrorReason),
    ArrayTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_array(DecoderState, _WireTypeHint, TermValue) when is_list(TermValue) ->
    ArrayTermValue = TermValue,
    {DecoderState, {ok, ArrayTermValue}};
decode_array(DecoderState, _WireTypeHint, _TermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_array_next(DecoderState, Index, ArrayWireTypeHint, ArrayTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    ArrayWireTypeHint :: argo_term:array_wire_type_hint(),
    ArrayTermValue :: argo_term:term_value(),
    Result :: result({ArrayTermValue, OptionItemTermValue}, ErrorReason),
    OptionItemTermValue :: argo_types:option(ItemTermValue),
    ItemTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_array_next(DecoderState, _Index, _ArrayWireTypeHint, [ItemTermValue | ArrayTermValue]) ->
    {DecoderState, {ok, {ArrayTermValue, {some, ItemTermValue}}}};
decode_array_next(DecoderState, _Index, _ArrayWireTypeHint, ArrayTermValue = []) ->
    {DecoderState, {ok, {ArrayTermValue, none}}}.

-spec decode_array_stop(DecoderState, ArrayTermValue, ArrayValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ArrayTermValue :: argo_term:term_value(),
    ArrayValue :: argo_array_value:t(),
    Result :: result(ArrayValue, ErrorReason),
    ErrorReason :: error_reason().
decode_array_stop(DecoderState, _ArrayTermValue, ArrayValue) ->
    {DecoderState, {ok, ArrayValue}}.

-spec decode_block(DecoderState, BlockWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_block(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    BlockWireTypeHint,
    TermValue
) ->
    {ScalarDecoderState2, Result} = argo_erlang_term_scalar_decoder:decode_block(
        ScalarDecoderModule, ScalarDecoderState1, BlockWireTypeHint, TermValue
    ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_block_stop(DecoderState, BlockValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    BlockValue :: argo_block_value:t(),
    Result :: result(DecoderState, ErrorReason),
    ErrorReason :: error_reason().
decode_block_stop(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    BlockValue
) ->
    {ScalarDecoderState2, Result} = argo_erlang_term_scalar_decoder:decode_block_stop(
        ScalarDecoderModule, ScalarDecoderState1, BlockValue
    ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_desc(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result({DescTermValue, DescValueHint}, ErrorReason),
    DescTermValue :: argo_term:term_value(),
    DescValueHint :: argo_term:desc_value_hint(),
    ErrorReason :: error_reason().
decode_desc(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    TermValue
) ->
    {ScalarDecoderState2, Result} = argo_erlang_term_scalar_decoder:decode_desc(
        ScalarDecoderModule, ScalarDecoderState1, TermValue
    ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_desc_list_next(DecoderState, Index, DescListTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    DescListTermValue :: argo_term:term_value(),
    Result :: result({DescListTermValue, OptionItemTermValue}, ErrorReason),
    OptionItemTermValue :: argo_types:option(ItemTermValue),
    ItemTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_desc_list_next(DecoderState, _Index, [ItemTermValue | DescListTermValue]) ->
    {DecoderState, {ok, {DescListTermValue, {some, ItemTermValue}}}};
decode_desc_list_next(DecoderState, _Index, DescListTermValue = []) ->
    {DecoderState, {ok, {DescListTermValue, none}}}.

-spec decode_desc_list_stop(DecoderState, DescListTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescListTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().
decode_desc_list_stop(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    DescListTermValue,
    DescValue
) ->
    {ScalarDecoderState2, Result} =
        argo_erlang_term_scalar_decoder:decode_desc_list_stop(
            ScalarDecoderModule, ScalarDecoderState1, DescListTermValue, DescValue
        ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_desc_object_next(DecoderState, Index, DescObjectTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    DescObjectTermValue :: argo_term:term_value(),
    Result :: result({DescObjectTermValue, OptionEntry}, ErrorReason),
    OptionEntry :: argo_types:option({ObjectKey, ObjectTermValue}),
    ObjectKey :: unicode:unicode_binary(),
    ObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_desc_object_next(DecoderState, Index, DescObjectTermValue) when is_map(DescObjectTermValue) ->
    decode_desc_object_next(DecoderState, Index, maps:iterator(DescObjectTermValue, ordered));
decode_desc_object_next(DecoderState, _Index, MapIterator1) ->
    case maps:next(MapIterator1) of
        {ObjectKey, ObjectTermValue, MapIterator2} ->
            {DecoderState, {ok, {MapIterator2, {some, {ObjectKey, ObjectTermValue}}}}};
        none ->
            {DecoderState, {ok, {MapIterator1, none}}}
    end.

-spec decode_desc_object_stop(DecoderState, DescObjectTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescObjectTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().
decode_desc_object_stop(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    DescListTermValue,
    DescValue
) ->
    {ScalarDecoderState2, Result} =
        argo_erlang_term_scalar_decoder:decode_desc_object_stop(
            ScalarDecoderModule, ScalarDecoderState1, DescListTermValue, DescValue
        ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_desc_scalar(DecoderState, DescValueScalarHint, DescScalarTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescValueScalarHint :: argo_term:desc_value_scalar_hint(),
    DescScalarTermValue :: argo_term:term_value(),
    Result :: result({DescScalarTermValue, DescScalar}, ErrorReason),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: error_reason().
decode_desc_scalar(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    DescValueScalarHint,
    DescScalarTermValue
) ->
    {ScalarDecoderState2, Result} = argo_erlang_term_scalar_decoder:decode_desc_scalar(
        ScalarDecoderModule, ScalarDecoderState1, DescValueScalarHint, DescScalarTermValue
    ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_desc_scalar_stop(DecoderState, DescScalarTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescScalarTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().
decode_desc_scalar_stop(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    DescScalarTermValue,
    DescValue
) ->
    {ScalarDecoderState2, Result} = argo_erlang_term_scalar_decoder:decode_desc_scalar_stop(
        ScalarDecoderModule, ScalarDecoderState1, DescScalarTermValue, DescValue
    ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_error(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result(ErrorTermValue, ErrorReason),
    ErrorTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_error(DecoderState, TermValue) when is_map(TermValue) ->
    ErrorTermValue = TermValue,
    {DecoderState, {ok, ErrorTermValue}};
decode_error(DecoderState, _TermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_error_extensions(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionExtensionsTermValue}, ErrorReason),
    OptionExtensionsTermValue :: argo_types:option(ExtensionsTermValue),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_error_extensions(DecoderState, ErrorTermValue) ->
    case ErrorTermValue of
        #{<<"extensions">> := ExtensionsTermValue} ->
            {DecoderState, {ok, {ErrorTermValue, {some, ExtensionsTermValue}}}};
        #{} ->
            {DecoderState, {ok, {ErrorTermValue, none}}}
    end.

-spec decode_error_locations(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionLocationsTermValue}, ErrorReason),
    OptionLocationsTermValue :: argo_types:option(LocationsTermValue),
    LocationsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_error_locations(DecoderState, ErrorTermValue) ->
    case ErrorTermValue of
        #{<<"locations">> := LocationsTermValue} ->
            {DecoderState, {ok, {ErrorTermValue, {some, LocationsTermValue}}}};
        #{} ->
            {DecoderState, {ok, {ErrorTermValue, none}}}
    end.

-spec decode_error_message(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionMessageTermValue}, ErrorReason),
    OptionMessageTermValue :: argo_types:option(MessageTermValue),
    MessageTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_error_message(DecoderState, ErrorTermValue) ->
    case ErrorTermValue of
        #{<<"message">> := MessageTermValue} ->
            {DecoderState, {ok, {ErrorTermValue, {some, MessageTermValue}}}};
        #{} ->
            {DecoderState, {ok, {ErrorTermValue, none}}}
    end.

-spec decode_error_path(DecoderState, ErrorTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    Result :: result({ErrorTermValue, OptionPathTermValue}, ErrorReason),
    OptionPathTermValue :: argo_types:option(PathTermValue),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_error_path(DecoderState, ErrorTermValue) ->
    case ErrorTermValue of
        #{<<"path">> := PathTermValue} ->
            {DecoderState, {ok, {ErrorTermValue, {some, PathTermValue}}}};
        #{} ->
            {DecoderState, {ok, {ErrorTermValue, none}}}
    end.

-spec decode_error_stop(DecoderState, ErrorTermValue, ErrorValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ErrorTermValue :: argo_term:term_value(),
    ErrorValue :: argo_error_value:t(),
    Result :: result(ErrorValue, ErrorReason),
    ErrorReason :: error_reason().
decode_error_stop(DecoderState, _ErrorTermValue, ErrorValue) ->
    {DecoderState, {ok, ErrorValue}}.

-spec decode_extensions(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result(ExtensionsTermValue, ErrorReason),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_extensions(DecoderState, TermValue) when is_map(TermValue) ->
    ExtensionsTermValue = TermValue,
    {DecoderState, {ok, ExtensionsTermValue}};
decode_extensions(DecoderState, _TermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_extensions_next(DecoderState, Index, ExtensionsTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    ExtensionsTermValue :: argo_term:term_value(),
    Result :: result({ExtensionsTermValue, OptionEntry}, ErrorReason),
    OptionEntry :: argo_types:option({ObjectKey, ObjectTermValue}),
    ObjectKey :: unicode:unicode_binary(),
    ObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_extensions_next(DecoderState, Index, ExtensionsTermValue) when is_map(ExtensionsTermValue) ->
    decode_extensions_next(DecoderState, Index, maps:iterator(ExtensionsTermValue, ordered));
decode_extensions_next(DecoderState, _Index, MapIterator1) ->
    case maps:next(MapIterator1) of
        {ObjectKey, ObjectTermValue, MapIterator2} ->
            {DecoderState, {ok, {MapIterator2, {some, {ObjectKey, ObjectTermValue}}}}};
        none ->
            {DecoderState, {ok, {MapIterator1, none}}}
    end.

-spec decode_extensions_stop(DecoderState, ExtensionsTermValue, ExtensionsValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ExtensionsTermValue :: argo_term:term_value(),
    ExtensionsValue :: argo_extensions_value:t(),
    Result :: result(ExtensionsValue, ErrorReason),
    ErrorReason :: error_reason().
decode_extensions_stop(DecoderState, _ExtensionsTermValue, ExtensionsValue) ->
    {DecoderState, {ok, ExtensionsValue}}.

-spec decode_location(DecoderState, LocationTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result(LocationTermValue, ErrorReason),
    ErrorReason :: error_reason().
decode_location(DecoderState, LocationTermValue) when is_map(LocationTermValue) ->
    {DecoderState, {ok, LocationTermValue}};
decode_location(DecoderState, _LocationTermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_location_column(DecoderState, LocationTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result({LocationTermValue, OptionColumnTermValue}, ErrorReason),
    OptionColumnTermValue :: argo_types:option(ColumnTermValue),
    ColumnTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_location_column(DecoderState, LocationTermValue) ->
    case LocationTermValue of
        #{<<"column">> := Column} when is_integer(Column) ->
            {DecoderState, {ok, {LocationTermValue, {some, Column}}}};
        #{} ->
            {DecoderState, {ok, {LocationTermValue, none}}}
    end.

-spec decode_location_line(DecoderState, LocationTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    Result :: result({LocationTermValue, OptionLineTermValue}, ErrorReason),
    OptionLineTermValue :: argo_types:option(LineTermValue),
    LineTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_location_line(DecoderState, LocationTermValue) ->
    case LocationTermValue of
        #{<<"line">> := Line} when is_integer(Line) ->
            {DecoderState, {ok, {LocationTermValue, {some, Line}}}};
        #{} ->
            {DecoderState, {ok, {LocationTermValue, none}}}
    end.

-spec decode_location_stop(DecoderState, LocationTermValue, LocationValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationTermValue :: argo_term:term_value(),
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationValue, ErrorReason),
    ErrorReason :: error_reason().
decode_location_stop(DecoderState, _LocationTermValue, LocationValue) ->
    {DecoderState, {ok, LocationValue}}.

-spec decode_locations(DecoderState, LocationsTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result(LocationsTermValue, ErrorReason),
    ErrorReason :: error_reason().
decode_locations(DecoderState, LocationsTermValue) when is_list(LocationsTermValue) ->
    {DecoderState, {ok, LocationsTermValue}};
decode_locations(DecoderState, _LocationsTermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_locations_next(DecoderState, Index, LocationsTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    LocationsTermValue :: argo_term:term_value(),
    Result :: result({LocationsTermValue, OptionLocationTermValue}, ErrorReason),
    OptionLocationTermValue :: argo_types:option(LocationTermValue),
    LocationTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_locations_next(DecoderState, _Index, [LocationTermValue | LocationsTermValue]) ->
    {DecoderState, {ok, {LocationsTermValue, {some, LocationTermValue}}}};
decode_locations_next(DecoderState, _Index, LocationsTermValue = []) ->
    {DecoderState, {ok, {LocationsTermValue, none}}}.

-spec decode_locations_stop(DecoderState, LocationsTermValue, LocationValueList) -> {DecoderState, Result} when
    DecoderState :: state(),
    LocationsTermValue :: argo_term:term_value(),
    LocationValueList :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    Result :: result(LocationValueList, ErrorReason),
    ErrorReason :: error_reason().
decode_locations_stop(DecoderState, _LocationsTermValue, LocationValueList) ->
    {DecoderState, {ok, LocationValueList}}.

-spec decode_nullable(DecoderState, NullableWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(NullableTermValue, ErrorReason),
    NullableTermValue :: null | {non_null, NonNullTermValue} | {field_errors, FieldErrorsTermValue},
    NonNullTermValue :: argo_term:term_value(),
    FieldErrorsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_nullable(DecoderState, _NullableWireTypeHint, TermValue) ->
    NullableTermValue =
        case TermValue of
            null -> null;
            #{<<"__field_errors">> := FieldErrorsTermValue} -> {field_errors, FieldErrorsTermValue};
            NonNullTermValue -> {non_null, NonNullTermValue}
        end,
    {DecoderState, {ok, NullableTermValue}}.

-spec decode_nullable_field_errors(DecoderState, NullableWireTypeHint, FieldErrorsTermValue) ->
    {DecoderState, Result}
when
    DecoderState :: state(),
    NullableWireTypeHint :: argo_term:nullable_wire_type_hint(),
    FieldErrorsTermValue :: argo_term:term_value(),
    Result :: result(FieldErrorsTermValue, ErrorReason),
    ErrorReason :: error_reason().
decode_nullable_field_errors(DecoderState, _NullableWireTypeHint, FieldErrorsTermValue) when
    is_list(FieldErrorsTermValue)
->
    {DecoderState, {ok, FieldErrorsTermValue}};
decode_nullable_field_errors(DecoderState, _NullableWireTypeHint, _FieldErrorsTermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_nullable_field_errors_next(DecoderState, Index, NullableWireTypeHint, FieldErrorsTermValue) ->
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
decode_nullable_field_errors_next(DecoderState, _Index, _NullableWireTypeHint, [
    FieldErrorTermValue | FieldErrorsTermValue
]) ->
    {DecoderState, {ok, {FieldErrorsTermValue, {some, FieldErrorTermValue}}}};
decode_nullable_field_errors_next(DecoderState, _Index, _NullableWireTypeHint, FieldErrorsTermValue = []) ->
    {DecoderState, {ok, {FieldErrorsTermValue, none}}}.

-spec decode_nullable_field_errors_stop(DecoderState, FieldErrorsTermValue, FieldErrorsValue) ->
    {DecoderState, Result}
when
    DecoderState :: state(),
    FieldErrorsTermValue :: argo_term:term_value(),
    FieldErrorsValue :: [FieldErrorValue],
    FieldErrorValue :: argo_error_value:t(),
    Result :: result(FieldErrorsValue, ErrorReason),
    ErrorReason :: error_reason().
decode_nullable_field_errors_stop(DecoderState, _FieldErrorsTermValue, FieldErrorsValue) ->
    {DecoderState, {ok, FieldErrorsValue}}.

-spec decode_nullable_stop(DecoderState, NullableValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    NullableValue :: argo_nullable_value:t(),
    Result :: result(NullableValue, ErrorReason),
    ErrorReason :: error_reason().
decode_nullable_stop(DecoderState, NullableValue) ->
    {DecoderState, {ok, NullableValue}}.

-spec decode_path(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result(PathTermValue, ErrorReason),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_path(DecoderState, TermValue) when is_list(TermValue) ->
    PathTermValue = TermValue,
    {DecoderState, {ok, PathTermValue}};
decode_path(DecoderState, _TermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_path_next(DecoderState, Index, PathTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    Index :: argo_types:index(),
    PathTermValue :: argo_term:term_value(),
    Result :: result({PathTermValue, OptionSegment}, ErrorReason),
    OptionSegment :: argo_types:option(FieldName | ListIndex),
    FieldName :: argo_types:name(),
    ListIndex :: argo_types:index(),
    ErrorReason :: error_reason().
decode_path_next(DecoderState, _Index, [FieldName | PathTermValue]) when is_binary(FieldName) ->
    {DecoderState, {ok, {PathTermValue, {some, FieldName}}}};
decode_path_next(DecoderState, _Index, [ListIndex | PathTermValue]) when is_integer(ListIndex) ->
    {DecoderState, {ok, {PathTermValue, {some, ListIndex}}}};
decode_path_next(DecoderState, _Index, PathTermValue = []) ->
    {DecoderState, {ok, {PathTermValue, none}}}.

-spec decode_path_stop(DecoderState, PathTermValue, PathValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    PathTermValue :: argo_term:term_value(),
    PathValue :: argo_path_value:t(),
    Result :: result(PathValue, ErrorReason),
    ErrorReason :: error_reason().
decode_path_stop(DecoderState, _PathTermValue, PathValue) ->
    {DecoderState, {ok, PathValue}}.

-spec decode_record(DecoderState, RecordWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    RecordWireTypeHint :: argo_term:record_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(RecordTermValue, ErrorReason),
    RecordTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_record(DecoderState, _RecordWireTypeHint, TermValue) when is_map(TermValue) ->
    RecordTermValue = TermValue,
    {DecoderState, {ok, RecordTermValue}};
decode_record(DecoderState, _RecordWireTypeHint, _TermValue) ->
    {DecoderState, {error, type_mismatch}}.

-spec decode_record_next(DecoderState, Index, FieldWireTypeHint, RecordTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    FieldWireTypeHint :: argo_term:field_wire_type_hint(),
    Index :: argo_types:index(),
    RecordTermValue :: argo_term:term_value(),
    Result :: result({RecordTermValue, OptionFieldTermValue}, ErrorReason),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_record_next(DecoderState, _Index, _FieldWireTypeHint = #{name := Name}, RecordTermValue) when
    is_map(RecordTermValue)
->
    OptionFieldTermValue =
        case RecordTermValue of
            #{Name := FieldTermValue} ->
                {some, FieldTermValue};
            #{} ->
                none
        end,
    {DecoderState, {ok, {RecordTermValue, OptionFieldTermValue}}}.

-spec decode_record_stop(DecoderState, RecordTermValue, RecordValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    RecordTermValue :: argo_term:term_value(),
    RecordValue :: argo_record_value:t(),
    Result :: result(RecordValue, ErrorReason),
    ErrorReason :: error_reason().
decode_record_stop(DecoderState, _RecordTermValue, RecordValue) ->
    {DecoderState, {ok, RecordValue}}.

-spec decode_scalar(DecoderState, ScalarWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(Scalar, ErrorReason),
    Scalar :: boolean() | binary() | DescTermValue | float() | unicode:unicode_binary() | argo_types:i64(),
    DescTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_scalar(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    ScalarWireTypeHint,
    TermValue
) ->
    {ScalarDecoderState2, Result} = argo_erlang_term_scalar_decoder:decode_scalar(
        ScalarDecoderModule, ScalarDecoderState1, ScalarWireTypeHint, TermValue
    ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

-spec decode_scalar_stop(DecoderState, ScalarValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ScalarValue :: argo_scalar_value:t(),
    Result :: result(ScalarValue, ErrorReason),
    ErrorReason :: error_reason().
decode_scalar_stop(
    DecoderState1 = #argo_erlang_term_value_decoder{
        scalar_decoder_module = ScalarDecoderModule,
        scalar_decoder_state = ScalarDecoderState1
    },
    ScalarValue
) ->
    {ScalarDecoderState2, Result} = argo_erlang_term_scalar_decoder:decode_scalar_stop(
        ScalarDecoderModule, ScalarDecoderState1, ScalarValue
    ),
    DecoderState2 = maybe_update_scalar_decoder_state(DecoderState1, ScalarDecoderState2),
    {DecoderState2, Result}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-compile({inline, [maybe_update_scalar_decoder_state/2]}).
-spec maybe_update_scalar_decoder_state(DecoderState, ScalarDecoderState) -> DecoderState when
    DecoderState :: state(), ScalarDecoderState :: argo_erlang_term_scalar_decoder:state().
maybe_update_scalar_decoder_state(
    DecoderState1 = #argo_erlang_term_value_decoder{scalar_decoder_state = ScalarDecoderState},
    ScalarDecoderState
) ->
    DecoderState1;
maybe_update_scalar_decoder_state(
    DecoderState1 = #argo_erlang_term_value_decoder{scalar_decoder_state = _ScalarDecoderState1},
    ScalarDecoderState2
) ->
    DecoderState2 = DecoderState1#argo_erlang_term_value_decoder{
        scalar_decoder_state = ScalarDecoderState2
    },
    DecoderState2.
