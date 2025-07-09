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
-module(argo_term_value_decoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Hint API
-export([
    block_wire_type_hint/1,
    field_wire_type_hint/1,
    record_wire_type_hint/1,
    scalar_wire_type_hint/1,
    wire_type_hint/1
]).
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

%% Hint Types
-type block_wire_type_hint() :: {Of :: scalar_wire_type_hint(), Key :: argo_types:name(), Dedupe :: boolean()}.
-type desc_container_hint() :: list | object.
-type desc_hint() :: desc_container_hint() | desc_scalar_hint().
-type desc_scalar_hint() :: null | boolean | string | bytes | int | float.
-type field_wire_type_hint() :: {Of :: wire_type_hint(), Name :: argo_types:name(), Omittable :: boolean()}.
-type record_wire_type_hint() :: [argo_types:name()].
-type scalar_wire_type_hint() :: boolean | bytes | desc | {fixed, argo_types:length()} | float64 | string | varint.
-type wire_type_hint() ::
    array
    | {block, block_wire_type_hint()}
    | desc
    | error
    | extensions
    | nullable
    | path
    | {record, record_wire_type_hint()}
    | {scalar, scalar_wire_type_hint()}.

-export_type([
    block_wire_type_hint/0,
    desc_container_hint/0,
    desc_hint/0,
    desc_scalar_hint/0,
    field_wire_type_hint/0,
    record_wire_type_hint/0,
    scalar_wire_type_hint/0,
    wire_type_hint/0
]).

%% Types
-type error_reason() :: invalid | type_mismatch | dynamic().
-type options() :: dynamic().
-type state() :: dynamic().
-type t() :: #argo_term_value_decoder{}.

-export_type([
    error_reason/0,
    options/0,
    state/0,
    t/0
]).

%% Behaviour
-callback init(Options) -> State when Options :: options(), State :: state().

-callback decode_array(State, WireTypeHint, TermValue) ->
    {ok, State, ArrayTermValue} | {error, ErrorReason}
when
    State :: state(),
    WireTypeHint :: wire_type_hint(),
    TermValue :: argo_term:term_value(),
    ArrayTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_array_next(State, WireTypeHint, Index, ArrayTermValue) ->
    {ok, State, ArrayTermValue, OptionItemTermValue} | {error, ErrorReason}
when
    State :: state(),
    WireTypeHint :: wire_type_hint(),
    Index :: non_neg_integer(),
    ArrayTermValue :: argo_term:term_value(),
    OptionItemTermValue :: argo_types:option(ItemTermValue),
    ItemTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_array_stop(State, ArrayTermValue, ArrayValue) ->
    {ok, State, ArrayValue} | {error, ErrorReason}
when
    State :: state(),
    ArrayTermValue :: argo_term:term_value(),
    ArrayValue :: argo_array_value:t(),
    ErrorReason :: error_reason().

-callback decode_block(State, BlockWireTypeHint, TermValue) ->
    {ok, State, BlockTermValue} | {error, ErrorReason}
when
    State :: state(),
    BlockWireTypeHint :: block_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_desc(State, TermValue) ->
    {ok, State, DescTermValue, DescHint} | {error, ErrorReason}
when
    State :: state(),
    TermValue :: argo_term:term_value(),
    DescTermValue :: argo_term:term_value(),
    DescHint :: desc_hint(),
    ErrorReason :: error_reason().

-callback decode_desc_list_next(State, Index, DescListTermValue) ->
    {ok, State, DescListTermValue, OptionItemTermValue} | {error, ErrorReason}
when
    State :: state(),
    Index :: non_neg_integer(),
    DescListTermValue :: argo_term:term_value(),
    OptionItemTermValue :: argo_types:option(ItemTermValue),
    ItemTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_desc_list_stop(State, DescListTermValue, DescValue) ->
    {ok, State, DescValue} | {error, ErrorReason}
when
    State :: state(),
    DescListTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    ErrorReason :: error_reason().

-callback decode_desc_object_next(State, Index, DescObjectTermValue) ->
    {ok, State, DescObjectTermValue, OptionEntry} | {error, ErrorReason}
when
    State :: state(),
    Index :: non_neg_integer(),
    DescObjectTermValue :: argo_term:term_value(),
    OptionEntry :: argo_types:option({ObjectKey, ObjectTermValue}),
    ObjectKey :: unicode:unicode_binary(),
    ObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_desc_object_stop(State, DescObjectTermValue, DescValue) ->
    {ok, State, DescValue} | {error, ErrorReason}
when
    State :: state(),
    DescObjectTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    ErrorReason :: error_reason().

-callback decode_desc_scalar(State, DescScalarHint, TermValue) ->
    {ok, State, DescScalar} | {error, ErrorReason}
when
    State :: state(),
    DescScalarHint :: desc_scalar_hint(),
    TermValue :: argo_term:term_value(),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: error_reason().

-callback decode_error(State, TermValue) ->
    {ok, State, ErrorTermValue} | {error, ErrorReason}
when
    State :: state(),
    TermValue :: argo_term:term_value(),
    ErrorTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_extensions(State, ErrorTermValue) ->
    {ok, State, ErrorTermValue, OptionExtensionsTermValue} | {error, ErrorReason}
when
    State :: state(),
    ErrorTermValue :: argo_term:term_value(),
    OptionExtensionsTermValue :: argo_types:option(ExtensionsTermValue),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_location_column(State, LocationTermValue) ->
    {ok, State, LocationTermValue, OptionColumn} | {error, ErrorReason}
when
    State :: state(),
    LocationTermValue :: argo_term:term_value(),
    OptionColumn :: argo_types:option(Column),
    Column :: integer(),
    ErrorReason :: error_reason().

-callback decode_error_location_line(State, LocationTermValue) ->
    {ok, State, LocationTermValue, OptionLine} | {error, ErrorReason}
when
    State :: state(),
    LocationTermValue :: argo_term:term_value(),
    OptionLine :: argo_types:option(Line),
    Line :: integer(),
    ErrorReason :: error_reason().

-callback decode_error_location_stop(State, LocationTermValue, LocationValue) ->
    {ok, State, LocationValue} | {error, ErrorReason}
when
    State :: state(),
    LocationTermValue :: argo_term:term_value(),
    LocationValue :: argo_location_value:t(),
    ErrorReason :: error_reason().

-callback decode_error_locations(State, ErrorTermValue) ->
    {ok, State, ErrorTermValue, OptionLocationsTermValue} | {error, ErrorReason}
when
    State :: state(),
    ErrorTermValue :: argo_term:term_value(),
    OptionLocationsTermValue :: argo_types:option(LocationsTermValue),
    LocationsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_locations_next(State, Index, LocationsTermValue) ->
    {ok, State, LocationsTermValue, OptionLocationTermValue} | {error, ErrorReason}
when
    State :: state(),
    Index :: non_neg_integer(),
    LocationsTermValue :: argo_term:term_value(),
    OptionLocationTermValue :: argo_types:option(LocationTermValue),
    LocationTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_locations_stop(State, LocationsTermValue, LocationValueList) ->
    {ok, State, LocationValueList} | {error, ErrorReason}
when
    State :: state(),
    LocationsTermValue :: argo_term:term_value(),
    LocationValueList :: [LocationValue],
    LocationValue :: argo_location_value:t(),
    ErrorReason :: error_reason().

-callback decode_error_message(State, ErrorTermValue) ->
    {ok, State, ErrorTermValue, OptionMessageTermValue} | {error, ErrorReason}
when
    State :: state(),
    ErrorTermValue :: argo_term:term_value(),
    OptionMessageTermValue :: argo_types:option(MessageTermValue),
    MessageTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_path(State, ErrorTermValue) ->
    {ok, State, ErrorTermValue, OptionPathTermValue} | {error, ErrorReason}
when
    State :: state(),
    ErrorTermValue :: argo_term:term_value(),
    OptionPathTermValue :: argo_types:option(PathTermValue),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_error_stop(State, ErrorTermValue, ErrorValue) ->
    {ok, State, ErrorValue} | {error, ErrorReason}
when
    State :: state(),
    ErrorTermValue :: argo_term:term_value(),
    ErrorValue :: argo_error_value:t(),
    ErrorReason :: error_reason().

-callback decode_extensions(State, TermValue) ->
    {ok, State, ExtensionsTermValue} | {error, ErrorReason}
when
    State :: state(),
    TermValue :: argo_term:term_value(),
    ExtensionsTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_extensions_next(State, Index, ExtensionsTermValue) ->
    {ok, State, ExtensionsTermValue, OptionEntry} | {error, ErrorReason}
when
    State :: state(),
    Index :: non_neg_integer(),
    ExtensionsTermValue :: argo_term:term_value(),
    OptionEntry :: argo_types:option({ObjectKey, ObjectTermValue}),
    ObjectKey :: unicode:unicode_binary(),
    ObjectTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_extensions_stop(State, ExtensionsTermValue, ExtensionsValue) ->
    {ok, State, ExtensionsValue} | {error, ErrorReason}
when
    State :: state(),
    ExtensionsTermValue :: argo_term:term_value(),
    ExtensionsValue :: argo_extensions_value:t(),
    ErrorReason :: error_reason().

-callback decode_nullable(State, WireTypeHint, TermValue) ->
    {ok, State, NullableTermValue} | {error, ErrorReason}
when
    State :: state(),
    WireTypeHint :: wire_type_hint(),
    TermValue :: argo_term:term_value(),
    NullableTermValue :: null | {non_null, NonNullTermValue},
    NonNullTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_path(State, TermValue) ->
    {ok, State, PathTermValue} | {error, ErrorReason}
when
    State :: state(),
    TermValue :: argo_term:term_value(),
    PathTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_path_next(State, Index, PathTermValue) ->
    {ok, State, PathTermValue, OptionSegment} | {error, ErrorReason}
when
    State :: state(),
    Index :: non_neg_integer(),
    PathTermValue :: argo_term:term_value(),
    OptionSegment :: argo_types:option(FieldName | ListIndex),
    FieldName :: argo_types:name(),
    ListIndex :: non_neg_integer(),
    ErrorReason :: error_reason().

-callback decode_path_stop(State, PathTermValue, PathValue) ->
    {ok, State, PathValue} | {error, ErrorReason}
when
    State :: state(),
    PathTermValue :: argo_term:term_value(),
    PathValue :: argo_path_value:t(),
    ErrorReason :: error_reason().

-callback decode_record(State, RecordWireTypeHint, TermValue) ->
    {ok, State, RecordTermValue} | {error, ErrorReason}
when
    State :: state(),
    RecordWireTypeHint :: record_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    RecordTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_record_next(State, FieldWireTypeHint, Index, RecordTermValue) ->
    {ok, State, RecordTermValue, OptionFieldTermValue} | {error, ErrorReason}
when
    State :: state(),
    FieldWireTypeHint :: field_wire_type_hint(),
    Index :: non_neg_integer(),
    RecordTermValue :: argo_term:term_value(),
    OptionFieldTermValue :: argo_types:option(FieldTermValue),
    FieldTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_record_stop(State, RecordTermValue, RecordValue) ->
    {ok, State, RecordValue} | {error, ErrorReason}
when
    State :: state(),
    RecordTermValue :: argo_term:term_value(),
    RecordValue :: argo_record_value:t(),
    ErrorReason :: error_reason().

-callback decode_scalar(State, ScalarWireTypeHint, TermValue) ->
    {ok, State, ScalarTermValue} | {error, ErrorReason}
when
    State :: state(),
    ScalarWireTypeHint :: scalar_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    ScalarTermValue :: boolean() | binary() | DescTermValue | float() | unicode:unicode_binary() | argo_types:i64(),
    DescTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

%%%=============================================================================
%%% Hint API functions
%%%=============================================================================

-spec block_wire_type_hint(BlockWireType) -> BlockWireTypeHint when
    BlockWireType :: argo_block_wire_type:t(), BlockWireTypeHint :: block_wire_type_hint().
block_wire_type_hint(#argo_block_wire_type{'of' = Of, key = Key, dedupe = Dedupe}) ->
    {scalar_wire_type_hint(Of), Key, Dedupe}.

-spec field_wire_type_hint(FieldWireType) -> FieldWireTypeHint when
    FieldWireType :: argo_field_wire_type:t(), FieldWireTypeHint :: field_wire_type_hint().
field_wire_type_hint(#argo_field_wire_type{'of' = Of, name = Name, omittable = Omittable}) ->
    {wire_type_hint(Of), Name, Omittable}.

-spec record_wire_type_hint(RecordWireType) -> RecordWireTypeHint when
    RecordWireType :: argo_record_wire_type:t(), RecordWireTypeHint :: record_wire_type_hint().
record_wire_type_hint(#argo_record_wire_type{fields = Fields}) ->
    argo_index_map:keys(Fields).

-spec scalar_wire_type_hint(ScalarWireType) -> ScalarWireTypeHint when
    ScalarWireType :: argo_scalar_wire_type:t(), ScalarWireTypeHint :: scalar_wire_type_hint().
scalar_wire_type_hint(#argo_scalar_wire_type{inner = Inner}) ->
    case Inner of
        boolean -> boolean;
        bytes -> bytes;
        desc -> desc;
        #argo_fixed_wire_type{length = FixedLength} -> {fixed, FixedLength};
        float64 -> float64;
        string -> string;
        varint -> varint
    end.

-spec wire_type_hint(WireType) -> WireTypeHint when WireType :: argo_wire_type:t(), WireTypeHint :: wire_type_hint().
wire_type_hint(#argo_wire_type{inner = Inner}) ->
    case Inner of
        #argo_array_wire_type{} -> array;
        BlockWireType = #argo_block_wire_type{} -> {block, block_wire_type_hint(BlockWireType)};
        #argo_desc_wire_type{} -> desc;
        #argo_error_wire_type{} -> error;
        #argo_extensions_wire_type{} -> extensions;
        #argo_nullable_wire_type{} -> nullable;
        #argo_path_wire_type{} -> path;
        RecordWireType = #argo_record_wire_type{} -> {record, record_wire_type_hint(RecordWireType)};
        ScalarWireType = #argo_scalar_wire_type{} -> {scalar, scalar_wire_type_hint(ScalarWireType)}
    end.

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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ArrayWireType = #argo_array_wire_type{'of' = Of},
    TermValue
) ->
    WireTypeHint = wire_type_hint(Of),
    case TermValueDecoderModule:decode_array(TermValueDecoderState1, WireTypeHint, TermValue) of
        {ok, TermValueDecoderState2, ArrayTermValue} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            decode_array_wire_type_next(TermValueDecoder2, ArrayWireType, WireTypeHint, ArrayTermValue, 0, [])
    end.

-spec decode_block_wire_type(TermValueDecoder, BlockWireType, TermValue) -> {TermValueDecoder, BlockValue} when
    TermValueDecoder :: t(),
    BlockWireType :: argo_block_wire_type:t(),
    TermValue :: argo_term:term_value(),
    BlockValue :: argo_block_value:t().
decode_block_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    BlockWireType = #argo_block_wire_type{'of' = Of},
    TermValue
) ->
    BlockWireTypeHint = block_wire_type_hint(BlockWireType),
    case TermValueDecoderModule:decode_block(TermValueDecoderState1, BlockWireTypeHint, TermValue) of
        {ok, TermValueDecoderState2, BlockTermValue} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, ScalarValue} = decode_scalar_wire_type(TermValueDecoder2, Of, BlockTermValue),
            BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
            {TermValueDecoder3, BlockValue};
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    case TermValueDecoderModule:decode_desc(TermValueDecoderState1, TermValue) of
        {ok, TermValueDecoderState2, DescListTermValue, list} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            decode_desc_wire_type_list_next(TermValueDecoder2, DescListTermValue, 0, []);
        {ok, TermValueDecoderState2, DescObjectTermValue, object} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            decode_desc_wire_type_object_next(TermValueDecoder2, DescObjectTermValue, 0, argo_index_map:new());
        {ok, TermValueDecoderState2, DescScalarTermValue, DescHint} when
            (DescHint =:= null) orelse (DescHint =:= boolean) orelse (DescHint =:= string) orelse (DescHint =:= bytes) orelse
                (DescHint =:= int) orelse (DescHint =:= float)
        ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            decode_desc_wire_type_scalar(TermValueDecoder2, DescScalarTermValue, DescHint);
        {error, _} ->
            error_with_info(badarg, [TermValueDecoder1, TermValue], #{2 => {failed_to_decode_scalar, TermValue}})
    end.

-spec decode_error_wire_type(TermValueDecoder, TermValue) -> {TermValueDecoder, ErrorValue} when
    TermValueDecoder :: t(), TermValue :: argo_term:term_value(), ErrorValue :: argo_error_value:t().
decode_error_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    case TermValueDecoderModule:decode_error(TermValueDecoderState1, TermValue) of
        {ok, TermValueDecoderState2, ErrorTermValue1} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, ErrorTermValue2, Message} = decode_error_wire_type_message(
                TermValueDecoder2, ErrorTermValue1
            ),
            {TermValueDecoder4, ErrorTermValue3, Locations} = decode_error_wire_type_locations(
                TermValueDecoder3, ErrorTermValue2
            ),
            {TermValueDecoder5, ErrorTermValue4, Path} = decode_error_wire_type_path(
                TermValueDecoder4, ErrorTermValue3
            ),
            {TermValueDecoder6, ErrorTermValue5, Extensions} = decode_error_wire_type_extensions(
                TermValueDecoder5, ErrorTermValue4
            ),
            ErrorValue1 = argo_error_value:new(Message, Locations, Path, Extensions),
            {TermValueDecoder7, ErrorValue2} = decode_error_wire_type_stop(
                TermValueDecoder6, ErrorTermValue5, ErrorValue1
            ),
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    case TermValueDecoderModule:decode_extensions(TermValueDecoderState1, TermValue) of
        {ok, TermValueDecoderState2, ExtensionsTermValue} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
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

-spec decode_nullable_wire_type(TermValueDecoder, NullableWireType, TermValue) -> {TermValueDecoder, NullableValue} when
    TermValueDecoder :: t(),
    NullableWireType :: argo_nullable_wire_type:t(),
    TermValue :: argo_term:term_value(),
    NullableValue :: argo_nullable_value:t().
decode_nullable_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    NullableWireType = #argo_nullable_wire_type{'of' = Of},
    TermValue
) ->
    WireTypeHint = wire_type_hint(Of),
    case TermValueDecoderModule:decode_nullable(TermValueDecoderState1, WireTypeHint, TermValue) of
        {ok, TermValueDecoderState2, null} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            NullableValue = argo_nullable_value:null(NullableWireType),
            {TermValueDecoder2, NullableValue};
        {ok, TermValueDecoderState2, {non_null, NonNullTermValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, Value} = decode_wire_type(TermValueDecoder2, Of, NonNullTermValue),
            NullableValue = argo_nullable_value:non_null(NullableWireType, Value),
            {TermValueDecoder3, NullableValue}
    end.

-spec decode_path_wire_type(TermValueDecoder, TermValue) -> {TermValueDecoder, PathValue} when
    TermValueDecoder :: t(), TermValue :: argo_term:term_value(), PathValue :: argo_path_value:t().
decode_path_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    TermValue
) ->
    case TermValueDecoderModule:decode_path(TermValueDecoderState1, TermValue) of
        {ok, TermValueDecoderState2, PathTermValue} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            decode_path_wire_type_next(TermValueDecoder2, PathTermValue, 0, argo_path_value:new())
    end.

-spec decode_scalar_wire_type(TermValueDecoder, ScalarWireType, TermValue) -> {TermValueDecoder, ScalarValue} when
    TermValueDecoder :: t(),
    ScalarWireType :: argo_scalar_wire_type:t(),
    TermValue :: argo_term:term_value(),
    ScalarValue :: argo_scalar_value:t().
decode_scalar_wire_type(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ScalarWireType = #argo_scalar_wire_type{},
    TermValue
) ->
    ScalarWireTypeHint = scalar_wire_type_hint(ScalarWireType),
    case TermValueDecoderModule:decode_scalar(TermValueDecoderState1, ScalarWireTypeHint, TermValue) of
        {ok, TermValueDecoderState2, StringValue} when is_binary(StringValue) andalso ScalarWireTypeHint =:= string ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            ScalarValue = argo_scalar_value:string(StringValue),
            {TermValueDecoder2, ScalarValue};
        {ok, TermValueDecoderState2, BooleanValue} when
            is_boolean(BooleanValue) andalso ScalarWireTypeHint =:= boolean
        ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            ScalarValue = argo_scalar_value:boolean(BooleanValue),
            {TermValueDecoder2, ScalarValue};
        {ok, TermValueDecoderState2, VarintValue} when ?is_i64(VarintValue) andalso ScalarWireTypeHint =:= varint ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            ScalarValue = argo_scalar_value:varint(VarintValue),
            {TermValueDecoder2, ScalarValue};
        {ok, TermValueDecoderState2, Float64Value} when is_float(Float64Value) andalso ScalarWireTypeHint =:= float64 ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            ScalarValue = argo_scalar_value:float64(Float64Value),
            {TermValueDecoder2, ScalarValue};
        {ok, TermValueDecoderState2, BytesValue} when is_binary(BytesValue) andalso ScalarWireTypeHint =:= bytes ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            ScalarValue = argo_scalar_value:bytes(BytesValue),
            {TermValueDecoder2, ScalarValue};
        {ok, TermValueDecoderState2, FixedValue} when
            is_binary(FixedValue) andalso element(1, ScalarWireTypeHint) =:= fixed andalso
                element(2, ScalarWireTypeHint) =:= byte_size(FixedValue)
        ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            ScalarValue = argo_scalar_value:fixed(FixedValue),
            {TermValueDecoder2, ScalarValue};
        {ok, TermValueDecoderState2, DescTermValue} when ScalarWireTypeHint =:= desc ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, DescValue} = decode_desc_wire_type(TermValueDecoder2, DescTermValue),
            ScalarValue = argo_scalar_value:desc(DescValue),
            {TermValueDecoder3, ScalarValue};
        {ok, _TermValueDecoderState2, Scalar} ->
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    RecordWireType = #argo_record_wire_type{fields = Fields},
    TermValue
) ->
    RecordWireTypeHint = record_wire_type_hint(RecordWireType),
    case TermValueDecoderModule:decode_record(TermValueDecoderState1, RecordWireTypeHint, TermValue) of
        {ok, TermValueDecoderState2, RecordTermValue} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
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
    Args :: [dynamic()], ScalarHint :: scalar_wire_type_hint(), Scalar :: dynamic().
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
-spec decode_array_wire_type_next(TermValueDecoder, ArrayWireType, WireTypeHint, ArrayTermValue, Index, Items) ->
    {TermValueDecoder, ArrayValue}
when
    TermValueDecoder :: t(),
    ArrayWireType :: argo_array_wire_type:t(),
    WireTypeHint :: wire_type_hint(),
    ArrayTermValue :: argo_term:term_value(),
    Index :: non_neg_integer(),
    Items :: [Item],
    Item :: argo_value:t(),
    ArrayValue :: argo_array_value:t().
decode_array_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ArrayWireType = #argo_array_wire_type{'of' = Of},
    WireTypeHint,
    ArrayTermValue1,
    Index1,
    Items1
) ->
    case TermValueDecoderModule:decode_array_next(TermValueDecoderState1, WireTypeHint, Index1, ArrayTermValue1) of
        {ok, TermValueDecoderState2, ArrayTermValue2, {some, ItemTermValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, Item} = decode_wire_type(TermValueDecoder2, Of, ItemTermValue),
            Items2 = [Item | Items1],
            Index2 = Index1 + 1,
            decode_array_wire_type_next(
                TermValueDecoder3, ArrayWireType, WireTypeHint, ArrayTermValue2, Index2, Items2
            );
        {ok, TermValueDecoderState2, ArrayTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            Items2 = lists:reverse(Items1),
            ArrayValue1 = argo_array_value:new(ArrayWireType, Items2),
            {TermValueDecoder3, ArrayValue2} = decode_array_wire_type_stop(
                TermValueDecoder2, ArrayTermValue2, ArrayValue1
            ),
            {TermValueDecoder3, ArrayValue2}
    end.

%% @private
-spec decode_array_wire_type_stop(TermValueDecoder, ArrayTermValue, ArrayValue) -> {TermValueDecoder, ArrayValue} when
    TermValueDecoder :: t(),
    ArrayTermValue :: argo_term:term_value(),
    ArrayValue :: argo_array_value:t().
decode_array_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ArrayTermValue,
    ArrayValue1
) ->
    case TermValueDecoderModule:decode_array_stop(TermValueDecoderState1, ArrayTermValue, ArrayValue1) of
        {ok, TermValueDecoderState2, ArrayValue2 = #argo_array_value{}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, ArrayValue2}
    end.

%% @private
-spec decode_desc_wire_type_list_next(TermValueDecoder, DescListTermValue, Index, Items) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescListTermValue :: argo_term:term_value(),
    Index :: non_neg_integer(),
    Items :: [Item],
    Item :: argo_desc_value:t(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_list_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    DescListTermValue1,
    Index1,
    Items1
) ->
    case TermValueDecoderModule:decode_desc_list_next(TermValueDecoderState1, Index1, DescListTermValue1) of
        {ok, TermValueDecoderState2, DescListTermValue2, {some, ItemValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, Item} = decode_desc_wire_type(TermValueDecoder2, ItemValue),
            Items2 = [Item | Items1],
            Index2 = Index1 + 1,
            decode_desc_wire_type_list_next(TermValueDecoder3, DescListTermValue2, Index2, Items2);
        {ok, TermValueDecoderState2, DescListTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            Items2 = lists:reverse(Items1),
            DescValue1 = argo_desc_value:list(Items2),
            {TermValueDecoder3, DescValue2} = decode_desc_wire_type_list_stop(
                TermValueDecoder2, DescListTermValue2, DescValue1
            ),
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    DescListTermValue,
    DescValue1
) ->
    case TermValueDecoderModule:decode_desc_list_stop(TermValueDecoderState1, DescListTermValue, DescValue1) of
        {ok, TermValueDecoderState2, DescValue2 = #argo_desc_value{}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, DescValue2}
    end.

%% @private
-spec decode_desc_wire_type_object_next(TermValueDecoder, DescObjectTermValue, Index, Object) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescObjectTermValue :: argo_term:term_value(),
    Index :: non_neg_integer(),
    Object :: argo_index_map:t(ObjectKey, ObjectValue),
    ObjectKey :: unicode:unicode_binary(),
    ObjectValue :: argo_desc_value:t(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_object_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    DescObjectTermValue1,
    Index1,
    Object1
) ->
    case TermValueDecoderModule:decode_desc_object_next(TermValueDecoderState1, Index1, DescObjectTermValue1) of
        {ok, TermValueDecoderState2, DescObjectTermValue2, {some, {ObjectKey, ObjectTermValue}}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, ObjectValue} = decode_desc_wire_type(TermValueDecoder2, ObjectTermValue),
            Object2 = argo_index_map:put(ObjectKey, ObjectValue, Object1),
            Index2 = Index1 + 1,
            decode_desc_wire_type_object_next(TermValueDecoder3, DescObjectTermValue2, Index2, Object2);
        {ok, TermValueDecoderState2, DescObjectTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            DescValue1 = argo_desc_value:object(Object1),
            {TermValueDecoder3, DescValue2} = decode_desc_wire_type_object_stop(
                TermValueDecoder2, DescObjectTermValue2, DescValue1
            ),
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    DescObjectTermValue,
    DescValue1
) ->
    case TermValueDecoderModule:decode_desc_object_stop(TermValueDecoderState1, DescObjectTermValue, DescValue1) of
        {ok, TermValueDecoderState2, DescValue2 = #argo_desc_value{}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, DescValue2}
    end.

%% @private
-spec decode_desc_wire_type_scalar(TermValueDecoder, DescScalarTermValue, DescScalarHint) ->
    {TermValueDecoder, DescValue}
when
    TermValueDecoder :: t(),
    DescScalarTermValue :: argo_term:term_value(),
    DescScalarHint :: desc_scalar_hint(),
    DescValue :: argo_desc_value:t().
decode_desc_wire_type_scalar(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    DescScalarTermValue,
    DescScalarHint
) ->
    case TermValueDecoderModule:decode_desc_scalar(TermValueDecoderState1, DescScalarHint, DescScalarTermValue) of
        {ok, TermValueDecoderState2, null} when DescScalarHint =:= null ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            DescValue = argo_desc_value:null(),
            {TermValueDecoder2, DescValue};
        {ok, TermValueDecoderState2, {boolean, BooleanValue}} when
            DescScalarHint =:= boolean andalso is_boolean(BooleanValue)
        ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            DescValue = argo_desc_value:boolean(BooleanValue),
            {TermValueDecoder2, DescValue};
        {ok, TermValueDecoderState2, {int, IntValue}} when DescScalarHint =:= int andalso ?is_i64(IntValue) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            DescValue = argo_desc_value:int(IntValue),
            {TermValueDecoder2, DescValue};
        {ok, TermValueDecoderState2, {float, FloatValue}} when DescScalarHint =:= float andalso is_float(FloatValue) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            DescValue = argo_desc_value:float(FloatValue),
            {TermValueDecoder2, DescValue};
        {ok, TermValueDecoderState2, {bytes, BytesValue}} when DescScalarHint =:= bytes andalso is_binary(BytesValue) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            DescValue = argo_desc_value:bytes(BytesValue),
            {TermValueDecoder2, DescValue};
        {ok, TermValueDecoderState2, {string, StringValue}} when
            DescScalarHint =:= string andalso is_binary(StringValue)
        ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            DescValue = argo_desc_value:string(StringValue),
            {TermValueDecoder2, DescValue}
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    case TermValueDecoderModule:decode_error_extensions(TermValueDecoderState1, ErrorTermValue1) of
        {ok, TermValueDecoderState2, ErrorTermValue2, {some, ExtensionsTermValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, Extensions} = decode_extensions_wire_type(TermValueDecoder2, ExtensionsTermValue),
            OptionExtensions = {some, Extensions},
            {TermValueDecoder3, ErrorTermValue2, OptionExtensions};
        {ok, TermValueDecoderState2, ErrorTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, ErrorTermValue2, none}
    end.

%% @private
-spec decode_error_wire_type_location(TermValueDecoder, TermValue) -> {TermValueDecoder, LocationValue} when
    TermValueDecoder :: t(), TermValue :: argo_term:term_value(), LocationValue :: argo_location_value:t().
decode_error_wire_type_location(TermValueDecoder1 = #argo_term_value_decoder{}, LocationTermValue1) ->
    {TermValueDecoder2, LocationTermValue2, Line} = decode_error_wire_type_location_line(
        TermValueDecoder1, LocationTermValue1
    ),
    {TermValueDecoder3, LocationTermValue3, Column} = decode_error_wire_type_location_column(
        TermValueDecoder2, LocationTermValue2
    ),
    LocationValue1 = argo_location_value:new(Line, Column),
    {TermValueDecoder4, LocationValue2} = decode_error_wire_type_location_stop(
        TermValueDecoder3, LocationTermValue3, LocationValue1
    ),
    {TermValueDecoder4, LocationValue2}.

%% @private
-spec decode_error_wire_type_location_column(TermValueDecoder, LocationTermValue) ->
    {TermValueDecoder, LocationTermValue, Column}
when
    TermValueDecoder :: t(), LocationTermValue :: argo_term:term_value(), Column :: integer().
decode_error_wire_type_location_column(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    LocationTermValue1
) ->
    case TermValueDecoderModule:decode_error_location_column(TermValueDecoderState1, LocationTermValue1) of
        {ok, TermValueDecoderState2, LocationTermValue2, {some, Column}} when is_integer(Column) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, LocationTermValue2, Column};
        {ok, _TermValueDecoderState2, _LocationTermValue2, {some, ColumnActual}} ->
            error_with_info(badarg, [TermValueDecoder1, LocationTermValue1], #{
                2 => {mismatch, expected_integer, ColumnActual}
            });
        {ok, _TermValueDecoderState2, _LocationTermValue3, none} ->
            error_with_info(badarg, [TermValueDecoder1, LocationTermValue1], #{
                2 => {required_object_key_missing, <<"column">>}
            })
    end.

%% @private
-spec decode_error_wire_type_location_line(TermValueDecoder, LocationTermValue) ->
    {TermValueDecoder, LocationTermValue, Line}
when
    TermValueDecoder :: t(), LocationTermValue :: argo_term:term_value(), Line :: integer().
decode_error_wire_type_location_line(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    LocationTermValue1
) ->
    case TermValueDecoderModule:decode_error_location_line(TermValueDecoderState1, LocationTermValue1) of
        {ok, TermValueDecoderState2, LocationTermValue2, {some, Line}} when is_integer(Line) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, LocationTermValue2, Line};
        {ok, _TermValueDecoderState2, _LocationTermValue2, {some, LineActual}} ->
            error_with_info(badarg, [TermValueDecoder1, LocationTermValue1], #{
                2 => {mismatch, expected_integer, LineActual}
            });
        {ok, _TermValueDecoderState2, _LocationTermValue3, none} ->
            error_with_info(badarg, [TermValueDecoder1, LocationTermValue1], #{
                2 => {required_object_key_missing, <<"line">>}
            })
    end.

%% @private
-spec decode_error_wire_type_location_stop(TermValueDecoder, LocationTermValue, LocationValue) ->
    {TermValueDecoder, LocationValue}
when
    TermValueDecoder :: t(), LocationTermValue :: argo_term:term_value(), LocationValue :: argo_location_value:t().
decode_error_wire_type_location_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    LocationTermValue,
    LocationValue1
) ->
    case TermValueDecoderModule:decode_error_location_stop(TermValueDecoderState1, LocationTermValue, LocationValue1) of
        {ok, TermValueDecoderState2, LocationValue2 = #argo_location_value{}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, LocationValue2}
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    case TermValueDecoderModule:decode_error_locations(TermValueDecoderState1, ErrorTermValue1) of
        {ok, TermValueDecoderState2, ErrorTermValue2, {some, LocationsTermValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, Locations} = decode_error_wire_type_locations_items(
                TermValueDecoder2, LocationsTermValue, 0, []
            ),
            OptionLocations = {some, Locations},
            {TermValueDecoder3, ErrorTermValue2, OptionLocations};
        {ok, TermValueDecoderState2, ErrorTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, ErrorTermValue2, none}
    end.

%% @private
-spec decode_error_wire_type_locations_items(TermValueDecoder, LocationsTermValue, Index, LocationValueList) ->
    {TermValueDecoder, LocationValueList}
when
    TermValueDecoder :: t(),
    LocationsTermValue :: argo_term:term_value(),
    Index :: non_neg_integer(),
    LocationValueList :: [LocationValue],
    LocationValue :: argo_location_value:t().
decode_error_wire_type_locations_items(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    LocationsTermValue1,
    Index1,
    LocationValueList1
) ->
    case TermValueDecoderModule:decode_error_locations_next(TermValueDecoderState1, Index1, LocationsTermValue1) of
        {ok, TermValueDecoderState2, LocationsTermValue2, {some, LocationTermValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, LocationValue} = decode_error_wire_type_location(TermValueDecoder2, LocationTermValue),
            LocationValueList2 = [LocationValue | LocationValueList1],
            Index2 = Index1 + 1,
            decode_error_wire_type_locations_items(TermValueDecoder3, LocationsTermValue2, Index2, LocationValueList2);
        {ok, TermValueDecoderState2, LocationsTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            LocationValueList2 = lists:reverse(LocationValueList1),
            {TermValueDecoder3, LocationValueList3} = decode_error_wire_type_locations_stop(
                TermValueDecoder2, LocationsTermValue2, LocationValueList2
            ),
            {TermValueDecoder3, LocationValueList3}
    end.

%% @private
-spec decode_error_wire_type_locations_stop(TermValueDecoder, LocationsTermValue, LocationValueList) ->
    {TermValueDecoder, LocationValueList}
when
    TermValueDecoder :: t(),
    LocationsTermValue :: argo_term:term_value(),
    LocationValueList :: [LocationValue],
    LocationValue :: argo_location_value:t().
decode_error_wire_type_locations_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    LocationsTermValue,
    LocationValueList1
) ->
    case
        TermValueDecoderModule:decode_error_locations_stop(
            TermValueDecoderState1, LocationsTermValue, LocationValueList1
        )
    of
        {ok, TermValueDecoderState2, LocationValueList2 = []} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, LocationValueList2};
        {ok, TermValueDecoderState2, LocationValueList2 = [#argo_location_value{} | _]} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, LocationValueList2}
    end.

%% @private
-spec decode_error_wire_type_message(TermValueDecoder, ErrorTermValue) ->
    {TermValueDecoder, ErrorTermValue, Message}
when
    TermValueDecoder :: t(), ErrorTermValue :: argo_term:term_value(), Message :: unicode:unicode_binary().
decode_error_wire_type_message(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    case TermValueDecoderModule:decode_error_message(TermValueDecoderState1, ErrorTermValue1) of
        {ok, TermValueDecoderState2, ErrorTermValue2, {some, MessageTermValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            MessageScalarWireType = argo_scalar_wire_type:string(),
            {TermValueDecoder3, MessageScalarValue} = decode_scalar_wire_type(
                TermValueDecoder2, MessageScalarWireType, MessageTermValue
            ),
            #argo_scalar_value{inner = {string, Message}} = MessageScalarValue,
            {TermValueDecoder3, ErrorTermValue2, Message};
        {ok, _TermValueDecoderState2, _ErrorTermValue2, none} ->
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ErrorTermValue1
) ->
    case TermValueDecoderModule:decode_error_path(TermValueDecoderState1, ErrorTermValue1) of
        {ok, TermValueDecoderState2, ErrorTermValue2, {some, PathTermValue}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, Path} = decode_path_wire_type(TermValueDecoder2, PathTermValue),
            OptionPath = {some, Path},
            {TermValueDecoder3, ErrorTermValue2, OptionPath};
        {ok, TermValueDecoderState2, ErrorTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, ErrorTermValue2, none}
    end.

%% @private
-spec decode_error_wire_type_stop(TermValueDecoder, ErrorTermValue, ErrorValue) -> {TermValueDecoder, ErrorValue} when
    TermValueDecoder :: t(), ErrorTermValue :: argo_term:term_value(), ErrorValue :: argo_error_value:t().
decode_error_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ErrorTermValue,
    ErrorValue1
) ->
    case TermValueDecoderModule:decode_error_stop(TermValueDecoderState1, ErrorTermValue, ErrorValue1) of
        {ok, TermValueDecoderState2, ErrorValue2} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, ErrorValue2}
    end.

%% @private
-spec decode_extensions_wire_type_next(TermValueDecoder, ExtensionsTermValue, Index, ExtensionsValue) ->
    {TermValueDecoder, ExtensionsValue}
when
    TermValueDecoder :: t(),
    ExtensionsTermValue :: argo_term:term_value(),
    Index :: non_neg_integer(),
    ExtensionsValue :: argo_extensions_value:t().
decode_extensions_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ExtensionsTermValue1,
    Index1,
    ExtensionsValue1
) ->
    case TermValueDecoderModule:decode_extensions_next(TermValueDecoderState1, Index1, ExtensionsTermValue1) of
        {ok, TermValueDecoderState2, ExtensionsTermValue2, {some, {Key, DescTermValue}}} when is_binary(Key) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, DescValue} = decode_desc_wire_type(TermValueDecoder2, DescTermValue),
            ExtensionsValue2 = argo_extensions_value:insert(ExtensionsValue1, Key, DescValue),
            Index2 = Index1 + 1,
            decode_extensions_wire_type_next(TermValueDecoder3, ExtensionsTermValue2, Index2, ExtensionsValue2);
        {ok, TermValueDecoderState2, ExtensionsTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, ExtensionsValue2} = decode_extensions_wire_type_stop(
                TermValueDecoder2, ExtensionsTermValue2, ExtensionsValue1
            ),
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    ExtensionsTermValue,
    ExtensionsValue1
) ->
    case TermValueDecoderModule:decode_extensions_stop(TermValueDecoderState1, ExtensionsTermValue, ExtensionsValue1) of
        {ok, TermValueDecoderState2, ExtensionsValue2 = #argo_extensions_value{}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, ExtensionsValue2}
    end.

%% @private
-spec decode_path_wire_type_next(TermValueDecoder, PathTermValue, Index, PathValue) ->
    {TermValueDecoder, PathValue}
when
    TermValueDecoder :: t(),
    PathTermValue :: argo_term:term_value(),
    Index :: non_neg_integer(),
    PathValue :: argo_path_value:t().
decode_path_wire_type_next(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    PathTermValue1,
    Index1,
    PathValue1
) ->
    case TermValueDecoderModule:decode_path_next(TermValueDecoderState1, Index1, PathTermValue1) of
        {ok, TermValueDecoderState2, PathTermValue2, {some, FieldName}} when is_binary(FieldName) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            Index2 = Index1 + 1,
            PathValue2 = argo_path_value:push_field_name(PathValue1, FieldName),
            decode_path_wire_type_next(TermValueDecoder2, PathTermValue2, Index2, PathValue2);
        {ok, TermValueDecoderState2, PathTermValue2, {some, ListIndex}} when is_integer(ListIndex) ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            Index2 = Index1 + 1,
            PathValue2 = argo_path_value:push_list_index(PathValue1, ListIndex),
            decode_path_wire_type_next(TermValueDecoder2, PathTermValue2, Index2, PathValue2);
        {ok, TermValueDecoderState2, PathTermValue2, none} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder3, PathValue2} = decode_path_wire_type_stop(TermValueDecoder2, PathTermValue2, PathValue1),
            {TermValueDecoder3, PathValue2}
    end.

%% @private
-spec decode_path_wire_type_stop(TermValueDecoder, PathTermValue, PathValue) -> {TermValueDecoder, PathValue} when
    TermValueDecoder :: t(), PathTermValue :: argo_term:term_value(), PathValue :: argo_path_value:t().
decode_path_wire_type_stop(
    TermValueDecoder1 = #argo_term_value_decoder{
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    PathTermValue,
    PathValue1
) ->
    case TermValueDecoderModule:decode_path_stop(TermValueDecoderState1, PathTermValue, PathValue1) of
        {ok, TermValueDecoderState2, PathValue2 = #argo_path_value{}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    RecordWireType = #argo_record_wire_type{},
    RecordTermValue1,
    FieldsIterator1,
    RecordValue1
) ->
    case argo_index_map:next(FieldsIterator1) of
        {Index, FieldName, FieldWireType = #argo_field_wire_type{name = FieldName}, FieldsIterator2} ->
            FieldWireTypeHint = field_wire_type_hint(FieldWireType),
            case
                TermValueDecoderModule:decode_record_next(
                    TermValueDecoderState1, FieldWireTypeHint, Index, RecordTermValue1
                )
            of
                {ok, TermValueDecoderState2, RecordTermValue2, OptionFieldTermValue} ->
                    TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
                    {TermValueDecoder3, FieldValue} = decode_field_wire_type(
                        TermValueDecoder2, FieldWireType, OptionFieldTermValue
                    ),
                    RecordValue2 = argo_record_value:insert(RecordValue1, FieldValue),
                    decode_record_wire_type_next(
                        TermValueDecoder3, RecordWireType, RecordTermValue2, FieldsIterator2, RecordValue2
                    )
            end;
        none ->
            {TermValueDecoder2, RecordValue2} = decode_record_wire_type_stop(
                TermValueDecoder1, RecordTermValue1, RecordValue1
            ),
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
        decoder_module = TermValueDecoderModule, decoder_state = TermValueDecoderState1
    },
    RecordTermValue,
    RecordValue1
) ->
    case TermValueDecoderModule:decode_record_stop(TermValueDecoderState1, RecordTermValue, RecordValue1) of
        {ok, TermValueDecoderState2, RecordValue2 = #argo_record_value{}} ->
            TermValueDecoder2 = maybe_update_decoder_state(TermValueDecoder1, TermValueDecoderState2),
            {TermValueDecoder2, RecordValue2}
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
