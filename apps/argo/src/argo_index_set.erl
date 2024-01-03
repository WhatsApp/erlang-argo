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
-module(argo_index_set).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_index_set.hrl").

%% API
-export([
    add_element/2,
    del_element/2,
    del_index/2,
    find_index/2,
    find_index_of/2,
    first/1,
    foldl/3,
    foldr/3,
    from_list/1,
    is_element/2,
    is_index/2,
    iterator/1,
    iterator/2,
    last/1,
    new/0,
    next/1,
    size/1,
    take/2,
    take_full/2,
    take_index/2,
    take_index_of/2,
    to_list/1
]).

%% Types
-type element() :: dynamic().
-type index() :: argo_index_map:index().
-type iterator() :: iterator(element()).
-opaque iterator(T) ::
    none
    | {{ordered, index()} | {reversed, index()} | {custom, [{index(), T}]}, t(T)}.
-type iterator_order() :: iterator_order(element()).
-type iterator_order(ElementType) ::
    ordered | reversed | iterator_order_func(ElementType).
-type iterator_order_func(T) :: fun((AIndex :: index(), AItem :: T, BIndex :: index(), BItem :: T) -> boolean()).
-type t(ElementType) :: #argo_index_set{map :: argo_index_map:t(ElementType, [])}.
-type t() :: t(element()).

-export_type([
    element/0,
    index/0,
    iterator/0,
    iterator/1,
    iterator_order/0,
    iterator_order/1,
    iterator_order_func/1,
    t/0,
    t/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec add_element(Element, IndexSet1) -> IndexSet2 when
    Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
add_element(Element, IndexSet1 = #argo_index_set{map = IndexMap1}) ->
    IndexMap2 = argo_index_map:put(Element, [], IndexMap1),
    IndexSet2 = IndexSet1#argo_index_set{map = IndexMap2},
    IndexSet2.

-spec del_element(Element, IndexSet1) -> IndexSet2 when
    Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
del_element(Element, IndexSet1 = #argo_index_set{map = IndexMap1}) ->
    case argo_index_map:remove(Element, IndexMap1) of
        IndexMap1 ->
            IndexSet1;
        IndexMap2 ->
            IndexSet2 = IndexSet1#argo_index_set{map = IndexMap2},
            IndexSet2
    end.

-spec del_index(Index, IndexSet1) -> IndexSet2 when
    Index :: index(), Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
del_index(Index, IndexSet1 = #argo_index_set{map = IndexMap1}) when is_integer(Index) andalso Index >= 0 ->
    case argo_index_map:remove_index(Index, IndexMap1) of
        IndexMap1 ->
            IndexSet1;
        IndexMap2 ->
            IndexSet2 = IndexSet1#argo_index_set{map = IndexMap2},
            IndexSet2
    end.

-spec find_index(Index, IndexSet) -> {ok, Element} | error when
    Index :: index(), Element :: element(), IndexSet :: t(Element).
find_index(Index, #argo_index_set{map = IndexMap}) when is_integer(Index) andalso Index >= 0 ->
    case argo_index_map:find_index(Index, IndexMap) of
        {ok, {Element, []}} ->
            {ok, Element};
        error ->
            error
    end.

-spec find_index_of(Element, IndexSet) -> {ok, Index} | error when
    Index :: index(), Element :: element(), IndexSet :: t(Element).
find_index_of(Element, #argo_index_set{map = IndexMap}) ->
    case argo_index_map:find_index_of(Element, IndexMap) of
        {ok, Index} ->
            {ok, Index};
        error ->
            error
    end.

-spec first(IndexSet) -> {ok, Element} | error when Element :: element(), IndexSet :: t(Element).
first(IndexSet = #argo_index_set{}) ->
    find_index(0, IndexSet).

-spec foldl(Function, Acc0, IndexSetOrIterator) -> Acc1 when
    Function :: fun((index(), Element, AccIn) -> AccOut),
    Acc0 :: dynamic(),
    Element :: element(),
    IndexSetOrIterator :: t(Element) | iterator(Element),
    Acc1 :: dynamic(),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldl(Function, Init, IndexSet = #argo_index_set{}) when is_function(Function, 3) ->
    Iterator = iterator(IndexSet, ordered),
    foldl_iterator(Iterator, Function, Init);
foldl(Function, Init, Iterator = {{_, _}, #argo_index_set{}}) when is_function(Function, 3) ->
    foldl_iterator(Iterator, Function, Init).

-spec foldr(Function, Acc0, IndexSetOrIterator) -> Acc1 when
    Function :: fun((index(), Element, AccIn) -> AccOut),
    Acc0 :: dynamic(),
    Element :: element(),
    IndexSetOrIterator :: t(Element) | iterator(Element),
    Acc1 :: dynamic(),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldr(Function, Init, IndexSet = #argo_index_set{}) when is_function(Function, 3) ->
    Iterator = iterator(IndexSet, reversed),
    foldl_iterator(Iterator, Function, Init);
foldr(Function, Init, Iterator = {{_, _}, #argo_index_set{}}) when is_function(Function, 3) ->
    foldr_iterator(Iterator, Function, Init, []).

-spec from_list(ElementList) -> IndexSet when ElementList :: [Element], Element :: element(), IndexSet :: t(Element).
from_list([]) ->
    new();
from_list(List) when is_list(List) ->
    lists:foldl(fun from_list/2, new(), List).

-spec is_index(Index, IndexSet) -> boolean() when Index :: index(), IndexSet :: t().
is_index(Index, IndexSet = #argo_index_set{}) ->
    Index < ?MODULE:size(IndexSet).

-spec is_element(Element, IndexSet) -> boolean() when Element :: element(), IndexSet :: t(Element).
is_element(Element, #argo_index_set{map = IndexMap}) ->
    argo_index_map:is_key(Element, IndexMap).

-spec iterator(IndexSet) -> Iterator when Element :: element(), IndexSet :: t(Element), Iterator :: iterator(Element).
iterator(IndexSet = #argo_index_set{}) ->
    iterator(IndexSet, ordered).

-spec iterator(IndexSet, Order) -> Iterator when
    Element :: element(), Order :: iterator_order(Element), IndexSet :: t(Element), Iterator :: iterator(Element).
iterator(IndexSet = #argo_index_set{}, ordered) ->
    {{ordered, 0}, IndexSet};
iterator(IndexSet = #argo_index_set{}, reversed) ->
    {{reversed, ?MODULE:size(IndexSet)}, IndexSet};
iterator(IndexSet = #argo_index_set{map = IndexMap}, OrderFun) when is_function(OrderFun, 4) ->
    IteratorInternal = argo_index_map:iterator_internal(IndexMap, OrderFun),
    {{custom, argo_types:dynamic_cast(IteratorInternal)}, IndexSet}.

-spec last(IndexSet) -> {ok, Element} | error when Element :: element(), IndexSet :: t(Element).
last(IndexSet = #argo_index_set{}) ->
    case ?MODULE:size(IndexSet) of
        0 ->
            error;
        Size when Size > 0 ->
            find_index(Size - 1, IndexSet)
    end.

-spec new() -> IndexSet when IndexSet :: t().
new() ->
    #argo_index_set{map = argo_index_map:new()}.

-spec next(Iterator) -> none | {Index, Element, NextIterator} when
    Index :: index(),
    Element :: element(),
    Iterator :: iterator(Element),
    NextIterator :: iterator(Element).
next({{ordered, Index}, IndexSet = #argo_index_set{}}) when is_integer(Index) andalso Index >= 0 ->
    case find_index(Index, IndexSet) of
        {ok, Element} ->
            NextIterator =
                case is_index(Index + 1, IndexSet) of
                    true ->
                        {{ordered, Index + 1}, IndexSet};
                    false ->
                        none
                end,
            {Index, Element, NextIterator};
        error ->
            none
    end;
next({{reversed, Index}, IndexSet = #argo_index_set{}}) when is_integer(Index) andalso Index >= 0 ->
    case find_index(Index - 1, IndexSet) of
        {ok, Element} ->
            NextIterator =
                case Index - 2 >= 0 andalso is_index(Index - 2, IndexSet) of
                    true ->
                        {{reversed, Index - 1}, IndexSet};
                    false ->
                        none
                end,
            {Index, Element, NextIterator};
        error ->
            none
    end;
next({{custom, [{Index, {Element, []}} | Custom]}, IndexSet = #argo_index_set{}}) ->
    NextIterator =
        case length(Custom) > 0 of
            true ->
                {{custom, Custom}, IndexSet};
            false ->
                none
        end,
    {Index, Element, NextIterator};
next({{custom, []}, #argo_index_set{}}) ->
    none;
next(none) ->
    none.

-spec size(IndexSet) -> non_neg_integer() when IndexSet :: t().
size(#argo_index_set{map = IndexMap}) ->
    argo_index_map:size(IndexMap).

-spec take(Element, IndexSet1) -> {Element, IndexSet2} | error when
    Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
take(Element, IndexSet1 = #argo_index_set{}) ->
    case take_full(Element, IndexSet1) of
        {{_Index, Element}, IndexSet2} ->
            {Element, IndexSet2};
        error ->
            error
    end.

-spec take_full(Element, IndexSet1) -> {{Index, Element}, IndexSet2} | error when
    Index :: index(), Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
take_full(Element, IndexSet1 = #argo_index_set{map = IndexMap1}) ->
    case argo_index_map:take_full(Element, IndexMap1) of
        {{Index, Element, []}, IndexMap2} ->
            IndexSet2 = IndexSet1#argo_index_set{map = IndexMap2},
            {{Index, Element}, IndexSet2};
        error ->
            error
    end.

-spec take_index(Index, IndexSet1) -> {Element, IndexSet2} | error when
    Index :: index(), Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
take_index(Index, IndexSet1 = #argo_index_set{map = IndexMap1}) when is_integer(Index) andalso Index >= 0 ->
    case argo_index_map:take_index(Index, IndexMap1) of
        {{Element, []}, IndexMap2} ->
            IndexSet2 = IndexSet1#argo_index_set{map = IndexMap2},
            {Element, IndexSet2};
        error ->
            error
    end.

-spec take_index_of(Element, IndexSet1) -> {Index, IndexSet2} | error when
    Index :: index(), Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
take_index_of(Element, IndexSet1 = #argo_index_set{}) ->
    case take_full(Element, IndexSet1) of
        {{Index, _Value}, IndexSet2} ->
            {Index, IndexSet2};
        error ->
            error
    end.

-spec to_list(IndexSetOrIterator) -> ElementList when
    Element :: element(), IndexSetOrIterator :: t(Element) | iterator(Element), ElementList :: [Element].
to_list(#argo_index_set{map = IndexMap}) ->
    argo_index_map:keys(IndexMap);
to_list(Iterator = {{_, _}, #argo_index_set{}}) ->
    lists:reverse(foldl_iterator(Iterator, fun collect_elements/3, [])).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec collect_elements(Index, Element, Elements) -> Elements when
    Index :: index(), Element :: element(), Elements :: [Element].
collect_elements(_Index, Element, Elements) ->
    [Element | Elements].

%% @private
-spec foldl_iterator(Iterator, Function, AccIn) -> AccOut when
    Index :: index(),
    Element :: element(),
    Iterator :: iterator(Element),
    Function :: fun((Index, Element, AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldl_iterator(Iterator, Function, Acc1) ->
    case next(Iterator) of
        none ->
            Acc1;
        {Index, Element, NextIterator} ->
            Acc2 = Function(Index, Element, Acc1),
            foldl_iterator(NextIterator, Function, Acc2)
    end.

%% @private
-spec foldr_iterator(Entries, Function, AccIn) -> AccOut when
    Index :: index(),
    Element :: element(),
    Entries :: [{Index, Element}],
    Function :: fun((Index, Element, AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldr_iterator([{Index, Element} | Entries], Function, Acc1) ->
    Acc2 = Function(Index, Element, Acc1),
    foldr_iterator(Entries, Function, Acc2);
foldr_iterator([], _Function, Acc1) ->
    Acc1.

%% @private
-spec foldr_iterator(Iterator, Function, AccIn, Entries) -> AccOut when
    Index :: index(),
    Element :: element(),
    Iterator :: iterator(Element),
    Function :: fun((Index, Element, AccIn) -> AccOut),
    AccIn :: dynamic(),
    Entries :: [{Index, Element}],
    AccOut :: dynamic().
foldr_iterator(Iterator, Function, Acc1, Entries1) ->
    case next(Iterator) of
        none ->
            foldr_iterator(Entries1, Function, Acc1);
        {Index, Element, NextIterator} ->
            Entries2 = [{Index, Element} | Entries1],
            foldr_iterator(NextIterator, Function, Acc1, Entries2)
    end.

%% @private
-spec from_list(Element, IndexSet1) -> IndexSet2 when
    Element :: element(), IndexSet1 :: t(Element), IndexSet2 :: t(Element).
from_list(Element, IndexSet) ->
    ?MODULE:add_element(Element, IndexSet).
