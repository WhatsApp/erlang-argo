%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_term).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-04-09", modified => "2025-07-15"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Hint API
-export([
    array_value_hint/2,
    array_wire_type_hint/1,
    block_wire_type_hint/1,
    desc_value_hint/1,
    desc_value_list_hint/2,
    desc_value_object_hint/3,
    field_wire_type_hint/1,
    nullable_wire_type_hint/1,
    record_wire_type_hint/1,
    scalar_wire_type_hint/1,
    wire_type_hint/1
]).

%% Hint Types
-type array_value_hint() :: #{'of' := array_wire_type_hint(), size := argo_types:usize(), index := argo_types:index()}.
-type array_wire_type_hint() :: #{'of' := wire_type_hint()}.
-type block_wire_type_hint() :: #{'of' := scalar_wire_type_hint(), key := argo_types:name(), dedupe := boolean()}.
-type desc_value_container_hint() :: list | object.
-type desc_value_hint() :: desc_value_container_hint() | desc_value_scalar_hint().
-type desc_value_list_hint() :: #{'of' := desc_value_hint(), index := argo_types:index()}.
-type desc_value_object_hint() :: #{'of' := desc_value_hint(), index := argo_types:index(), key := argo_types:name()}.
-type desc_value_scalar_hint() :: null | boolean | string | bytes | int | float.
-type field_wire_type_hint() :: #{'of' := wire_type_hint(), name := argo_types:name(), omittable := boolean()}.
-type non_null_wire_type_hint() ::
    array
    | {block, block_wire_type_hint()}
    | desc
    | error
    | extensions
    | path
    | {record, record_wire_type_hint()}
    | {scalar, scalar_wire_type_hint()}.
-type nullable_wire_type_hint() :: #{'of' := non_null_wire_type_hint()}.
-type record_wire_type_hint() :: #{fields := [argo_types:name()]}.
-type scalar_wire_type_hint() :: boolean | bytes | desc | {fixed, argo_types:length()} | float64 | string | varint.
-type wire_type_hint() ::
    nullable
    | non_null_wire_type_hint().

-export_type([
    array_value_hint/0,
    array_wire_type_hint/0,
    block_wire_type_hint/0,
    desc_value_container_hint/0,
    desc_value_hint/0,
    desc_value_list_hint/0,
    desc_value_object_hint/0,
    desc_value_scalar_hint/0,
    field_wire_type_hint/0,
    non_null_wire_type_hint/0,
    nullable_wire_type_hint/0,
    record_wire_type_hint/0,
    scalar_wire_type_hint/0,
    wire_type_hint/0
]).

%% Types
-type term_value() :: dynamic().

-export_type([
    term_value/0
]).

%%%=============================================================================
%%% Hint API functions
%%%=============================================================================

-spec array_value_hint(ArrayValue, Index) -> ArrayValueHint when
    ArrayValue :: argo_array_value:t(), Index :: argo_types:index(), ArrayValueHint :: array_value_hint().
array_value_hint(ArrayValue = #argo_array_value{}, Index) when ?is_usize(Index) ->
    (array_wire_type_hint(ArrayValue))#{size => argo_array_value:size(ArrayValue), index => Index}.

-spec array_wire_type_hint(ArrayValue | ArrayWireType) -> ArrayWireTypeHint when
    ArrayValue :: argo_array_value:t(),
    ArrayWireType :: argo_array_wire_type:t(),
    ArrayWireTypeHint :: array_wire_type_hint().
array_wire_type_hint(#argo_array_value{wire_type = ArrayWireType}) ->
    array_wire_type_hint(ArrayWireType);
array_wire_type_hint(#argo_array_wire_type{'of' = Of}) ->
    #{'of' => wire_type_hint(Of)}.

-spec block_wire_type_hint(BlockValue | BlockWireType) -> BlockWireTypeHint when
    BlockValue :: argo_block_value:t(),
    BlockWireType :: argo_block_wire_type:t(),
    BlockWireTypeHint :: block_wire_type_hint().
block_wire_type_hint(#argo_block_value{wire_type = BlockWireType}) ->
    block_wire_type_hint(BlockWireType);
block_wire_type_hint(#argo_block_wire_type{'of' = Of, key = Key, dedupe = Dedupe}) ->
    #{'of' => scalar_wire_type_hint(Of), key => Key, dedupe => Dedupe}.

-spec desc_value_hint(DescInner | DescValue) -> DescValueHint when
    DescInner :: argo_desc_value:inner(), DescValue :: argo_desc_value:t(), DescValueHint :: desc_value_hint().
desc_value_hint(null) -> null;
desc_value_hint({boolean, _}) -> boolean;
desc_value_hint({string, _}) -> string;
desc_value_hint({bytes, _}) -> bytes;
desc_value_hint({int, _}) -> int;
desc_value_hint({float, _}) -> float;
desc_value_hint({object, _}) -> object;
desc_value_hint({list, _}) -> list;
desc_value_hint(#argo_desc_value{inner = DescInner}) -> desc_value_hint(DescInner).

-spec desc_value_list_hint(DescValue, Index) -> DescValueListHint when
    DescValue :: argo_desc_value:t(), Index :: argo_types:index(), DescValueListHint :: desc_value_list_hint().
desc_value_list_hint(DescValue = #argo_desc_value{}, Index) when ?is_usize(Index) ->
    #{'of' => desc_value_hint(DescValue), index => Index}.

-spec desc_value_object_hint(DescValue, Index, Key) -> DescValueObjectHint when
    DescValue :: argo_desc_value:t(),
    Index :: argo_types:index(),
    Key :: argo_types:name(),
    DescValueObjectHint :: desc_value_object_hint().
desc_value_object_hint(DescValue = #argo_desc_value{}, Index, Key) when ?is_usize(Index) andalso is_binary(Key) ->
    #{'of' => desc_value_hint(DescValue), index => Index, key => Key}.

-spec field_wire_type_hint(FieldValue | FieldWireType) -> FieldWireTypeHint when
    FieldValue :: argo_field_value:t(),
    FieldWireType :: argo_field_wire_type:t(),
    FieldWireTypeHint :: field_wire_type_hint().
field_wire_type_hint(#argo_field_value{wire_type = FieldWireType}) ->
    field_wire_type_hint(FieldWireType);
field_wire_type_hint(#argo_field_wire_type{'of' = Of, name = Name, omittable = Omittable}) ->
    #{'of' => wire_type_hint(Of), name => Name, omittable => Omittable}.

-spec nullable_wire_type_hint(NullableValue | NullableWireType) -> NullableWireTypeHint when
    NullableValue :: argo_nullable_value:t(),
    NullableWireType :: argo_nullable_wire_type:t(),
    NullableWireTypeHint :: nullable_wire_type_hint().
nullable_wire_type_hint(#argo_nullable_value{wire_type = NullableWireType}) ->
    nullable_wire_type_hint(NullableWireType);
nullable_wire_type_hint(#argo_nullable_wire_type{
    'of' = #argo_wire_type{inner = NullableWireType = #argo_nullable_wire_type{}}
}) ->
    nullable_wire_type_hint(NullableWireType);
nullable_wire_type_hint(NullableWireType = #argo_nullable_wire_type{'of' = Of}) ->
    case wire_type_hint(Of) of
        nullable ->
            %% unreachable
            erlang:error(badarg, [NullableWireType]);
        NullableWireTypeHint ->
            NullableWireTypeHint
    end.

-spec record_wire_type_hint(RecordValue | RecordWireType) -> RecordWireTypeHint when
    RecordValue :: argo_record_value:t(),
    RecordWireType :: argo_record_wire_type:t(),
    RecordWireTypeHint :: record_wire_type_hint().
record_wire_type_hint(#argo_record_value{fields = Fields}) ->
    #{fields => argo_index_map:keys(Fields)};
record_wire_type_hint(#argo_record_wire_type{fields = Fields}) ->
    #{fields => argo_index_map:keys(Fields)}.

-spec scalar_wire_type_hint(ScalarValue | ScalarWireType) -> ScalarWireTypeHint when
    ScalarValue :: argo_scalar_value:t(),
    ScalarWireType :: argo_scalar_wire_type:t(),
    ScalarWireTypeHint :: scalar_wire_type_hint().
scalar_wire_type_hint(#argo_scalar_value{inner = Inner}) ->
    case Inner of
        {boolean, _} -> boolean;
        {bytes, _} -> bytes;
        {desc, _} -> desc;
        {fixed, FixedScalarValue} -> {fixed, byte_size(FixedScalarValue)};
        {float64, _} -> float64;
        {string, _} -> string;
        {varint, _} -> varint
    end;
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

-spec wire_type_hint(Value | WireType) -> WireTypeHint when
    Value :: argo_value:t(), WireType :: argo_wire_type:t(), WireTypeHint :: wire_type_hint().
wire_type_hint(Value = #argo_value{}) ->
    WireType = argo_value:to_wire_type(Value),
    wire_type_hint(WireType);
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
