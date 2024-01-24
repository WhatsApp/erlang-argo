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
-module(argo_record_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_value.hrl").

%% New API
-export([
    new/0
]).
%% Instance API
-export([
    find/2,
    find_index/2,
    find_index_of/2,
    insert/2,
    present_fields_count/1,
    to_record_wire_type/1
]).

%% Types
-type t() :: #argo_record_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec new() -> RecordValue when RecordValue :: t().
new() ->
    #argo_record_value{fields = argo_index_map:new()}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec find(RecordValue, Name) -> {ok, FieldValue} | error when
    RecordValue :: t(), Name :: argo_types:name(), FieldValue :: argo_field_value:t().
find(#argo_record_value{fields = Fields}, Name) when is_binary(Name) ->
    argo_index_map:find(Name, Fields).

-spec find_index(RecordValue, Index) -> {ok, FieldValue} | error when
    RecordValue :: t(), Index :: argo_index_map:index(), FieldValue :: argo_field_value:t().
find_index(#argo_record_value{fields = Fields}, Index) when ?is_u64(Index) ->
    case argo_index_map:find_index(Index, Fields) of
        {ok, {_Name, FieldValue}} ->
            {ok, FieldValue};
        error ->
            error
    end.

-spec find_index_of(RecordValue, Name) -> {ok, Index, FieldValue} | error when
    RecordValue :: t(), Name :: argo_types:name(), Index :: argo_index_map:index(), FieldValue :: argo_field_value:t().
find_index_of(#argo_record_value{fields = Fields}, Name) when is_binary(Name) ->
    case argo_index_map:find_full(Name, Fields) of
        {ok, {Index, Name, FieldValue}} ->
            {ok, Index, FieldValue};
        error ->
            error
    end.

-spec insert(RecordValue, FieldValue) -> RecordValue when RecordValue :: t(), FieldValue :: argo_field_value:t().
insert(RecordValue0 = #argo_record_value{fields = Fields0}, FieldValue = #argo_field_value{}) ->
    Name = argo_field_value:name(FieldValue),
    Fields1 = argo_index_map:put(Name, FieldValue, Fields0),
    RecordValue1 = RecordValue0#argo_record_value{fields = Fields1},
    RecordValue1.

-spec present_fields_count(RecordValue) -> non_neg_integer() when RecordValue :: t().
present_fields_count(#argo_record_value{fields = Fields}) ->
    argo_index_map:foldl(
        fun(_Index, _Key, FieldValue, Count) ->
            case FieldValue of
                #argo_field_value{inner = {optional, none}} ->
                    Count;
                #argo_field_value{inner = {optional, {some, _}}} ->
                    Count + 1;
                #argo_field_value{inner = {required, _}} ->
                    Count + 1
            end
        end,
        0,
        Fields
    ).

-spec to_record_wire_type(RecordValue) -> RecordWireType when
    RecordValue :: t(), RecordWireType :: argo_record_wire_type:t().
to_record_wire_type(#argo_record_value{fields = Fields}) ->
    argo_index_map:foldl(
        fun(_Index, _FieldName, FieldValue, RecordWireTypeAcc) ->
            FieldWireType = argo_field_value:to_field_wire_type(FieldValue),
            argo_record_wire_type:insert(RecordWireTypeAcc, FieldWireType)
        end,
        argo_record_wire_type:new(),
        Fields
    ).
