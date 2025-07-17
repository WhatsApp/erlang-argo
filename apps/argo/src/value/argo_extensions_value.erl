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
%%% Created :  13 Feb 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_extensions_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").

%% New API
-export([
    new/0,
    new/1
]).

%% Instance API
-export([
    insert/3,
    iterator/1,
    iterator/2,
    next/1
]).

%% Records
-record(argo_extensions_value_iterator, {
    iterator :: argo_index_map:iterator(key(), value())
}).

%% Types
-type index() :: argo_index_map:index().
-type inner() :: argo_desc_value:desc_object().
-type key() :: argo_types:name().
-opaque iterator() :: argo_types:option(#argo_extensions_value_iterator{}).
-type iterator_order() :: ordered | reversed | key_func() | order_func().
-type key_func() ::
    fun((Index :: index(), Key :: key(), Value :: value()) -> Key :: dynamic()).
-type order_func() :: argo_index_map:iterator_order_func(key(), value()).
-type t() :: #argo_extensions_value{}.
-type value() :: argo_desc_value:t().

-export_type([
    index/0,
    inner/0,
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

-spec new() -> ExtensionsValue when ExtensionsValue :: t().
new() ->
    #argo_extensions_value{inner = argo_index_map:new()}.

-spec new(Extensions) -> ExtensionsValue when Extensions :: inner(), ExtensionsValue :: t().
new(Extensions = #argo_index_map{}) ->
    #argo_extensions_value{inner = Extensions}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec insert(ExtensionsValue, Key, DescValue) -> ExtensionsValue when
    ExtensionsValue :: t(), Key :: key(), DescValue :: argo_desc_value:t().
insert(ExtensionsValue1 = #argo_extensions_value{inner = Extensions1}, Key, DescValue = #argo_desc_value{}) when
    is_binary(Key)
->
    Extensions2 = argo_index_map:put(Key, DescValue, Extensions1),
    ExtensionsValue2 = ExtensionsValue1#argo_extensions_value{inner = Extensions2},
    ExtensionsValue2.

-spec iterator(ExtensionsValue) -> Iterator when ExtensionsValue :: t(), Iterator :: iterator().
iterator(ExtensionsValue = #argo_extensions_value{}) ->
    iterator(ExtensionsValue, ordered).

-spec iterator(ExtensionsValue, Order) -> Iterator when
    ExtensionsValue :: t(), Order :: iterator_order(), Iterator :: iterator().
iterator(ExtensionsValue = #argo_extensions_value{}, ordered) ->
    Iterator = argo_index_map:iterator(ExtensionsValue#argo_extensions_value.inner, ordered),
    {some, #argo_extensions_value_iterator{iterator = Iterator}};
iterator(ExtensionsValue = #argo_extensions_value{}, reversed) ->
    Iterator = argo_index_map:iterator(ExtensionsValue#argo_extensions_value.inner, reversed),
    {some, #argo_extensions_value_iterator{iterator = Iterator}};
iterator(ExtensionsValue = #argo_extensions_value{}, KeyFun) when is_function(KeyFun, 3) ->
    OrderFun = key_func_to_order_func(KeyFun),
    iterator(ExtensionsValue, OrderFun);
iterator(ExtensionsValue = #argo_extensions_value{}, OrderFun) when is_function(OrderFun, 6) ->
    Iterator = argo_index_map:iterator(ExtensionsValue#argo_extensions_value.inner, OrderFun),
    {some, #argo_extensions_value_iterator{iterator = Iterator}}.

-spec next(Iterator) -> none | {Index, Key, Value, NextIterator} when
    Index :: index(),
    Key :: key(),
    Value :: value(),
    Iterator :: iterator(),
    NextIterator :: iterator().
next({some, #argo_extensions_value_iterator{iterator = Iterator1}}) ->
    case argo_index_map:next(Iterator1) of
        none ->
            none;
        {Index, Key, Value, Iterator2} ->
            {Index, Key, Value, {some, #argo_extensions_value_iterator{iterator = Iterator2}}}
    end;
next(none) ->
    none.

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
