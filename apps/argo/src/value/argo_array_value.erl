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
-module(argo_array_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    iterator/1,
    iterator/2,
    new/2,
    next/1,
    size/1,
    to_array_wire_type/1
]).

%% Records
-record(argo_array_value_iterator, {
    kind :: {ordered, index(), [item()]} | {reversed, index(), [item()]} | {custom, [{index(), item()}]}
}).

%% Types
-type index() :: non_neg_integer().
-type item() :: argo_value:t().
-opaque iterator() :: argo_types:option(#argo_array_value_iterator{}).
-type iterator_order() :: ordered | reversed | key_func() | order_func().
-type key_func() ::
    fun((Index :: index(), Item :: item()) -> Key :: dynamic()).
-type order_func() ::
    fun((AIndex :: index(), AItem :: item(), BIndex :: index(), BItem :: item()) -> boolean()).
-type t() :: #argo_array_value{}.

-export_type([
    index/0,
    item/0,
    iterator/0,
    iterator_order/0,
    key_func/0,
    order_func/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec iterator(ArrayValue) -> Iterator when ArrayValue :: t(), Iterator :: iterator().
iterator(ArrayValue = #argo_array_value{}) ->
    iterator(ArrayValue, ordered).

-spec iterator(ArrayValue, Order) -> Iterator when
    ArrayValue :: t(), Order :: iterator_order(), Iterator :: iterator().
iterator(ArrayValue = #argo_array_value{}, ordered) ->
    Kind = {ordered, 0, ArrayValue#argo_array_value.items},
    {some, #argo_array_value_iterator{kind = Kind}};
iterator(ArrayValue = #argo_array_value{}, reversed) ->
    Kind = {reversed, ?MODULE:size(ArrayValue), lists:reverse(ArrayValue#argo_array_value.items)},
    {some, #argo_array_value_iterator{kind = Kind}};
iterator(ArrayValue = #argo_array_value{}, KeyFun) when is_function(KeyFun, 2) ->
    OrderFun = key_func_to_order_func(KeyFun),
    iterator(ArrayValue, OrderFun);
iterator(ArrayValue = #argo_array_value{}, OrderFun) when is_function(OrderFun, 4) ->
    IteratorInternal = iterator_internal(ArrayValue, OrderFun),
    Kind = {custom, IteratorInternal},
    {some, #argo_array_value_iterator{kind = Kind}}.

-spec new(ArrayWireType, Items) -> ArrayValue when
    ArrayWireType :: argo_array_wire_type:t(), Items :: [argo_value:t()], ArrayValue :: t().
new(ArrayWireType = #argo_array_wire_type{}, Items) when is_list(Items) ->
    #argo_array_value{wire_type = ArrayWireType, items = Items}.

-spec next(Iterator) -> none | {Index, Item, NextIterator} when
    Index :: index(),
    Item :: item(),
    Iterator :: iterator(),
    NextIterator :: iterator().
next({some, #argo_array_value_iterator{kind = Kind}}) ->
    case Kind of
        {ordered, _Index, []} ->
            none;
        {reversed, _Index, []} ->
            none;
        {custom, []} ->
            none;
        {ordered, Index, [Item | Items]} ->
            NextIndex = Index + 1,
            NextIterator =
                case Items of
                    [] ->
                        none;
                    [_ | _] ->
                        {some, #argo_array_value_iterator{kind = {ordered, NextIndex, Items}}}
                end,
            {Index, Item, NextIterator};
        {reversed, Index, [Item | Items]} ->
            NextIndex = Index - 1,
            NextIterator =
                case Items of
                    [] ->
                        none;
                    [_ | _] ->
                        {some, #argo_array_value_iterator{kind = {reversed, NextIndex, Items}}}
                end,
            {NextIndex, Item, NextIterator};
        {custom, [{Index, Item} | Items]} ->
            NextIterator =
                case Items of
                    [] ->
                        none;
                    [_ | _] ->
                        {some, #argo_array_value_iterator{kind = {custom, Items}}}
                end,
            {Index, Item, NextIterator}
    end;
next(none) ->
    none.

-spec size(ArrayWireType) -> Size when ArrayWireType :: t(), Size :: non_neg_integer().
size(#argo_array_value{items = Items}) ->
    length(Items).

-spec to_array_wire_type(ArrayValue) -> ArrayWireType when ArrayValue :: t(), ArrayWireType :: argo_array_wire_type:t().
to_array_wire_type(#argo_array_value{wire_type = ArrayWireType = #argo_array_wire_type{}}) ->
    ArrayWireType.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec iterator_internal(ArrayValue, OrderFun) -> InternalIterator when
    ArrayValue :: t(), OrderFun :: order_func(), InternalIterator :: [{index(), item()}].
iterator_internal(_ArrayValue = #argo_array_value{items = Items}, OrderFun) when is_function(OrderFun, 4) ->
    InternalIterator = lists:sort(
        fun({AIndex, AItem}, {BIndex, BItem}) ->
            OrderFun(AIndex, AItem, BIndex, BItem)
        end,
        lists:enumerate(0, 1, Items)
    ),
    InternalIterator.

%% @private
-spec key_func_to_order_func(KeyFun) -> OrderFun when
    KeyFun :: key_func(), OrderFun :: order_func().
key_func_to_order_func(KeyFun) when is_function(KeyFun, 2) ->
    OrderFun = fun(AIndex, AItem, BIndex, BItem) ->
        AOrder = KeyFun(AIndex, AItem),
        BOrder = KeyFun(BIndex, BItem),
        AOrder =< BOrder
    end,
    OrderFun.
