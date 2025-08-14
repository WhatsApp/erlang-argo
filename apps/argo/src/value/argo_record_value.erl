%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_record_value).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
    iterator/1,
    iterator/2,
    next/1,
    present_fields_count/1,
    to_record_wire_type/1
]).

%% Records
-record(argo_record_value_iterator, {
    iterator :: argo_index_map:iterator(key(), value())
}).

%% Types
-type index() :: argo_index_map:index().
-type key() :: argo_types:name().
-opaque iterator() :: argo_types:option(#argo_record_value_iterator{}).
-type iterator_order() :: ordered | reversed | key_func() | order_func().
-type key_func() ::
    fun((Index :: index(), Key :: key(), Value :: value()) -> Key :: dynamic()).
-type order_func() :: argo_index_map:iterator_order_func(key(), value()).
-type t() :: #argo_record_value{}.
-type value() :: argo_field_value:t().

-export_type([
    index/0,
    key/0,
    iterator/0,
    iterator_order/0,
    key_func/0,
    order_func/0,
    t/0,
    value/0
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

-spec iterator(RecordValue) -> Iterator when RecordValue :: t(), Iterator :: iterator().
iterator(RecordValue = #argo_record_value{}) ->
    iterator(RecordValue, ordered).

-spec iterator(RecordValue, Order) -> Iterator when
    RecordValue :: t(), Order :: iterator_order(), Iterator :: iterator().
iterator(RecordValue = #argo_record_value{}, ordered) ->
    Iterator = argo_index_map:iterator(RecordValue#argo_record_value.fields, ordered),
    {some, #argo_record_value_iterator{iterator = Iterator}};
iterator(RecordValue = #argo_record_value{}, reversed) ->
    Iterator = argo_index_map:iterator(RecordValue#argo_record_value.fields, reversed),
    {some, #argo_record_value_iterator{iterator = Iterator}};
iterator(RecordValue = #argo_record_value{}, KeyFun) when is_function(KeyFun, 3) ->
    OrderFun = key_func_to_order_func(KeyFun),
    iterator(RecordValue, OrderFun);
iterator(RecordValue = #argo_record_value{}, OrderFun) when is_function(OrderFun, 6) ->
    Iterator = argo_index_map:iterator(RecordValue#argo_record_value.fields, OrderFun),
    {some, #argo_record_value_iterator{iterator = Iterator}}.

-spec next(Iterator) -> none | {Index, Key, Value, NextIterator} when
    Index :: index(),
    Key :: key(),
    Value :: value(),
    Iterator :: iterator(),
    NextIterator :: iterator().
next({some, #argo_record_value_iterator{iterator = Iterator1}}) ->
    case argo_index_map:next(Iterator1) of
        none ->
            none;
        {Index, Key, Value, Iterator2} ->
            {Index, Key, Value, {some, #argo_record_value_iterator{iterator = Iterator2}}}
    end;
next(none) ->
    none.

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

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec key_func_to_order_func(KeyFun) -> OrderFun when
    KeyFun :: key_func(), OrderFun :: order_func().
key_func_to_order_func(KeyFun) when is_function(KeyFun, 3) ->
    OrderFun = fun(AIndex, AKey, AValue, BIndex, BKey, BValue) ->
        AOrder = KeyFun(AIndex, AKey, AValue),
        BOrder = KeyFun(BIndex, BKey, BValue),
        AOrder =< BOrder
    end,
    OrderFun.
