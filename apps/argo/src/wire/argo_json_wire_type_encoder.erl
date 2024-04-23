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
%%% Created :  26 Jan 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_json_wire_type_encoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    encode_wire_type/2,
    encode_wire_type_store/2
]).

%% Types
-type options() :: #{
    strict => boolean()
}.
-type t() :: #argo_json_wire_type_encoder{}.

-export_type([
    options/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Options) -> JsonWireTypeEncoder when Options :: options(), JsonWireTypeEncoder :: t().
new(Options) when is_map(Options) ->
    Strict = maps:get(strict, Options, false),
    #argo_json_wire_type_encoder{strict = Strict}.

-spec encode_wire_type(JsonWireTypeEncoder, WireType) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), WireType :: argo_wire_type:t(), JsonValue :: argo_json:json_value().
encode_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, WireType = #argo_wire_type{}) ->
    case WireType#argo_wire_type.inner of
        ScalarWireType = #argo_scalar_wire_type{} ->
            encode_scalar_wire_type(JsonWireTypeEncoder1, ScalarWireType);
        BlockWireType = #argo_block_wire_type{} ->
            encode_block_wire_type(JsonWireTypeEncoder1, BlockWireType);
        NullableWireType = #argo_nullable_wire_type{} ->
            encode_nullable_wire_type(JsonWireTypeEncoder1, NullableWireType);
        ArrayWireType = #argo_array_wire_type{} ->
            encode_array_wire_type(JsonWireTypeEncoder1, ArrayWireType);
        RecordWireType = #argo_record_wire_type{} ->
            encode_record_wire_type(JsonWireTypeEncoder1, RecordWireType);
        #argo_desc_wire_type{} ->
            encode_desc_wire_type(JsonWireTypeEncoder1);
        #argo_error_wire_type{} ->
            encode_error_wire_type(JsonWireTypeEncoder1);
        #argo_extensions_wire_type{} ->
            encode_extensions_wire_type(JsonWireTypeEncoder1);
        #argo_path_wire_type{} ->
            encode_path_wire_type(JsonWireTypeEncoder1)
    end.

-spec encode_wire_type_store(JsonWireTypeEncoder, WireTypeStore) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), WireTypeStore :: argo_wire_type_store:t(), JsonValue :: argo_json:json_value().
encode_wire_type_store(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, WireTypeStore = #argo_wire_type_store{}) ->
    Types = WireTypeStore#argo_wire_type_store.types,
    {JsonWireTypeEncoder2, JsonTypes1} = argo_index_map:foldl(
        fun(
            _Index,
            TypeName,
            #argo_wire_type_store_entry{name = TypeName, type = WireType},
            {JsonWireTypeEncoder1_Acc1, JsonTypesAcc1}
        ) ->
            {JsonWireTypeEncoder1_Acc2, JsonType} = encode_wire_type(JsonWireTypeEncoder1_Acc1, WireType),
            JsonTypesAcc2 = [{[{<<"name">>, TypeName}, {<<"type">>, JsonType}]} | JsonTypesAcc1],
            {JsonWireTypeEncoder1_Acc2, JsonTypesAcc2}
        end,
        {JsonWireTypeEncoder1, []},
        Types
    ),
    JsonTypes2 = lists:reverse(JsonTypes1),
    {JsonWireTypeEncoder2, {[{<<"type">>, <<"STORE">>}, {<<"types">>, JsonTypes2}]}}.

%% @private
-spec encode_scalar_wire_type(JsonWireTypeEncoder, ScalarWireType) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), ScalarWireType :: argo_scalar_wire_type:t(), JsonValue :: argo_json:json_value().
encode_scalar_wire_type(
    JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, ScalarWireType = #argo_scalar_wire_type{}
) ->
    case ScalarWireType#argo_scalar_wire_type.inner of
        string ->
            {JsonWireTypeEncoder1, {[{<<"type">>, <<"STRING">>}]}};
        boolean ->
            {JsonWireTypeEncoder1, {[{<<"type">>, <<"BOOLEAN">>}]}};
        varint ->
            {JsonWireTypeEncoder1, {[{<<"type">>, <<"VARINT">>}]}};
        float64 ->
            {JsonWireTypeEncoder1, {[{<<"type">>, <<"FLOAT64">>}]}};
        bytes ->
            {JsonWireTypeEncoder1, {[{<<"type">>, <<"BYTES">>}]}};
        #argo_fixed_wire_type{length = Length} ->
            {JsonWireTypeEncoder1, {[{<<"type">>, <<"FIXED">>}, {<<"length">>, Length}]}}
    end.

%% @private
-spec encode_block_wire_type(JsonWireTypeEncoder, BlockWireType) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), BlockWireType :: argo_block_wire_type:t(), JsonValue :: argo_json:json_value().
encode_block_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, #argo_block_wire_type{
    'of' = Of, key = Key, dedupe = Dedupe
}) ->
    {JsonWireTypeEncoder2, JsonOf} = encode_scalar_wire_type(JsonWireTypeEncoder1, Of),
    {JsonWireTypeEncoder2, {[{<<"type">>, <<"BLOCK">>}, {<<"of">>, JsonOf}, {<<"key">>, Key}, {<<"dedupe">>, Dedupe}]}}.

%% @private
-spec encode_nullable_wire_type(JsonWireTypeEncoder, NullableWireType) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), NullableWireType :: argo_nullable_wire_type:t(), JsonValue :: argo_json:json_value().
encode_nullable_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, #argo_nullable_wire_type{'of' = Of}) ->
    {JsonWireTypeEncoder2, JsonOf} = encode_wire_type(JsonWireTypeEncoder1, Of),
    {JsonWireTypeEncoder2, {[{<<"type">>, <<"NULLABLE">>}, {<<"of">>, JsonOf}]}}.

%% @private
-spec encode_array_wire_type(JsonWireTypeEncoder, ArrayWireType) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), ArrayWireType :: argo_array_wire_type:t(), JsonValue :: argo_json:json_value().
encode_array_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, #argo_array_wire_type{'of' = Of}) ->
    {JsonWireTypeEncoder2, JsonOf} = encode_wire_type(JsonWireTypeEncoder1, Of),
    {JsonWireTypeEncoder2, {[{<<"type">>, <<"ARRAY">>}, {<<"of">>, JsonOf}]}}.

%% @private
-spec encode_record_wire_type(JsonWireTypeEncoder, RecordWireType) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), RecordWireType :: argo_record_wire_type:t(), JsonValue :: argo_json:json_value().
encode_record_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, #argo_record_wire_type{fields = Fields}) ->
    {JsonWireTypeEncoder2, JsonFields1} = argo_index_map:foldl(
        fun(_Index, _Name, FieldWireType, {JsonWireTypeEncoder1_Acc1, JsonFieldsAcc1}) ->
            {JsonWireTypeEncoder1_Acc2, JsonField} = encode_field_wire_type(JsonWireTypeEncoder1_Acc1, FieldWireType),
            JsonFieldsAcc2 = [JsonField | JsonFieldsAcc1],
            {JsonWireTypeEncoder1_Acc2, JsonFieldsAcc2}
        end,
        {JsonWireTypeEncoder1, []},
        Fields
    ),
    JsonFields2 = lists:reverse(JsonFields1),
    {JsonWireTypeEncoder2, {[{<<"type">>, <<"RECORD">>}, {<<"fields">>, JsonFields2}]}}.

%% @private
-spec encode_field_wire_type(JsonWireTypeEncoder, FieldWireType) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), FieldWireType :: argo_field_wire_type:t(), JsonValue :: argo_json:json_value().
encode_field_wire_type(
    JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}, FieldWireType = #argo_field_wire_type{name = Name, 'of' = Of}
) ->
    Omittable = argo_field_wire_type:is_omittable(FieldWireType),
    {JsonWireTypeEncoder2, JsonOf} = encode_wire_type(JsonWireTypeEncoder1, Of),
    {JsonWireTypeEncoder2, {[{<<"name">>, Name}, {<<"of">>, JsonOf}, {<<"omittable">>, Omittable}]}}.

%% @private
-spec encode_desc_wire_type(JsonWireTypeEncoder) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), JsonValue :: argo_json:json_value().
encode_desc_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}) ->
    {JsonWireTypeEncoder1, {[{<<"type">>, <<"DESC">>}]}}.

%% @private
-spec encode_error_wire_type(JsonWireTypeEncoder) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), JsonValue :: argo_json:json_value().
encode_error_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{strict = false}) ->
    {JsonWireTypeEncoder1, {[{<<"type">>, <<"ERROR">>}]}};
encode_error_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{strict = true}) ->
    JsonStringWireType = {[{<<"type">>, <<"STRING">>}]},
    JsonVarintWireType = {[{<<"type">>, <<"VARINT">>}]},
    JsonPathWireType = {[{<<"type">>, <<"PATH">>}]},
    JsonExtensionsWireType = {[{<<"type">>, <<"DESC">>}]},
    JsonLocationRecordWireType =
        {[
            {<<"type">>, <<"RECORD">>},
            {<<"fields">>, [
                {[{<<"name">>, <<"line">>}, {<<"type">>, JsonVarintWireType}, {<<"omittable">>, false}]},
                {[{<<"name">>, <<"column">>}, {<<"type">>, JsonVarintWireType}, {<<"omittable">>, false}]}
            ]}
        ]},
    JsonLocationsWireType =
        {[
            {<<"type">>, <<"ARRAY">>},
            {<<"of">>, JsonLocationRecordWireType}
        ]},
    JsonErrorWireType =
        {[
            {<<"type">>, <<"RECORD">>},
            {<<"fields">>, [
                {[{<<"name">>, <<"message">>}, {<<"type">>, JsonStringWireType}, {<<"omittable">>, false}]},
                {[{<<"name">>, <<"locations">>}, {<<"type">>, JsonLocationsWireType}, {<<"omittable">>, true}]},
                {[{<<"name">>, <<"path">>}, {<<"type">>, JsonPathWireType}, {<<"omittable">>, true}]},
                {[{<<"name">>, <<"extensions">>}, {<<"type">>, JsonExtensionsWireType}, {<<"omittable">>, true}]}
            ]}
        ]},
    {JsonWireTypeEncoder1, JsonErrorWireType}.

%% @private
-spec encode_extensions_wire_type(JsonWireTypeEncoder) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), JsonValue :: argo_json:json_value().
encode_extensions_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{strict = false}) ->
    {JsonWireTypeEncoder1, {[{<<"type">>, <<"EXTENSIONS">>}]}};
encode_extensions_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{strict = true}) ->
    {JsonWireTypeEncoder1, {[{<<"type">>, <<"DESC">>}]}}.

%% @private
-spec encode_path_wire_type(JsonWireTypeEncoder) -> {JsonWireTypeEncoder, JsonValue} when
    JsonWireTypeEncoder :: t(), JsonValue :: argo_json:json_value().
encode_path_wire_type(JsonWireTypeEncoder1 = #argo_json_wire_type_encoder{}) ->
    {JsonWireTypeEncoder1, {[{<<"type">>, <<"PATH">>}]}}.
