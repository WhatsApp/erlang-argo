%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_path_value).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    foldl/3,
    foldr/3,
    from_list/1,
    get/2,
    iterator/1,
    iterator/2,
    new/0,
    next/1,
    pop/1,
    push_field_name/2,
    push_list_index/2,
    size/1,
    to_dotted_string/1,
    to_list/1,
    to_wire_path/2
]).

%% Records
-record(argo_path_value_iterator, {
    kind :: {ordered, index()} | {reversed, index()} | {custom, [index()]},
    path :: t()
}).

%% Types
-type index() :: non_neg_integer().
-type item() :: segment().
-opaque iterator() :: argo_types:option(#argo_path_value_iterator{}).
-type iterator_order() :: ordered | reversed | key_func() | order_func().
-type key_func() ::
    fun((Index :: index(), Item :: item()) -> Key :: dynamic()).
-type order_func() ::
    fun((AIndex :: index(), AItem :: item(), BIndex :: index(), BItem :: item()) -> boolean()).
-type segment() :: {field_name, argo_types:name()} | {list_index, non_neg_integer()}.
-type segment_list() :: [argo_types:name() | non_neg_integer()].
-type t() :: #argo_path_value{}.

-export_type([
    index/0,
    item/0,
    iterator/0,
    iterator_order/0,
    key_func/0,
    order_func/0,
    segment/0,
    segment_list/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec foldl(PathValue, Acc0, Function) -> Acc1 when
    PathValue :: t(),
    Acc0 :: dynamic(),
    Function :: fun((segment(), AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic(),
    Acc1 :: dynamic().
foldl(#argo_path_value{segments = Segments}, Init, Function) when is_function(Function, 2) ->
    array:foldl(
        fun(_Index, Segment, Acc) ->
            Function(Segment, Acc)
        end,
        Init,
        Segments
    ).

-spec foldr(PathValue, Acc0, Function) -> Acc1 when
    PathValue :: t(),
    Acc0 :: dynamic(),
    Function :: fun((segment(), AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic(),
    Acc1 :: dynamic().
foldr(#argo_path_value{segments = Segments}, Init, Function) when is_function(Function, 2) ->
    array:foldr(
        fun(_Index, Segment, Acc) ->
            Function(Segment, Acc)
        end,
        Init,
        Segments
    ).

-spec from_list(SegmentList) -> PathValue when SegmentList :: segment_list(), PathValue :: t().
from_list(SegmentList) when is_list(SegmentList) ->
    lists:foldl(fun from_list/2, new(), SegmentList).

-spec get(PathValue, Index) -> Segment when PathValue :: t(), Index :: index(), Segment :: segment().
get(PathValue = #argo_path_value{segments = Segments}, Index) when is_integer(Index) andalso Index >= 0 ->
    Size = ?MODULE:size(PathValue),
    case Index >= Size of
        true ->
            erlang:error(badarg, [PathValue, Index]);
        false ->
            Segment = array:get(Index, Segments),
            Segment
    end.

-spec iterator(PathValue) -> Iterator when PathValue :: t(), Iterator :: iterator().
iterator(PathValue = #argo_path_value{}) ->
    iterator(PathValue, ordered).

-spec iterator(PathValue, Order) -> Iterator when
    PathValue :: t(), Order :: iterator_order(), Iterator :: iterator().
iterator(PathValue = #argo_path_value{}, ordered) ->
    Kind = {ordered, 0},
    {some, #argo_path_value_iterator{kind = Kind, path = PathValue}};
iterator(PathValue = #argo_path_value{}, reversed) ->
    Kind = {reversed, ?MODULE:size(PathValue)},
    {some, #argo_path_value_iterator{kind = Kind, path = PathValue}};
iterator(PathValue = #argo_path_value{}, KeyFun) when is_function(KeyFun, 2) ->
    OrderFun = key_func_to_order_func(KeyFun),
    iterator(PathValue, OrderFun);
iterator(PathValue = #argo_path_value{}, OrderFun) when is_function(OrderFun, 4) ->
    IteratorInternal = iterator_internal(PathValue, OrderFun),
    Kind = {custom, IteratorInternal},
    {some, #argo_path_value_iterator{kind = Kind, path = PathValue}}.

-spec new() -> PathValue when PathValue :: t().
new() ->
    #argo_path_value{segments = argo_types:dynamic_cast(array:new(0, fixed))}.

-spec next(Iterator) -> none | {Index, Item, NextIterator} when
    Index :: index(),
    Item :: item(),
    Iterator :: iterator(),
    NextIterator :: iterator().
next({some, Iterator = #argo_path_value_iterator{kind = Kind, path = Path}}) ->
    Size = ?MODULE:size(Path),
    case Kind of
        {ordered, Index} when Index >= Size ->
            none;
        {ordered, Index} when Index < Size ->
            NextIndex = Index + 1,
            NextIterator =
                case NextIndex >= Size of
                    true ->
                        none;
                    false ->
                        {some, Iterator#argo_path_value_iterator{kind = {ordered, NextIndex}}}
                end,
            Item = ?MODULE:get(Path, Index),
            {Index, Item, NextIterator};
        {reversed, Index} when Index =< 0 ->
            none;
        {reversed, Index} when Index =< Size ->
            NextIndex = Index - 1,
            NextIterator =
                case NextIndex =< 0 of
                    true ->
                        none;
                    false ->
                        {some, Iterator#argo_path_value_iterator{kind = {reversed, NextIndex}}}
                end,
            Item = ?MODULE:get(Path, NextIndex),
            {Index, Item, NextIterator};
        {custom, [Index | NextIndexes]} ->
            NextIterator =
                case NextIndexes of
                    [] ->
                        none;
                    [_ | _] ->
                        {some, Iterator#argo_path_value_iterator{kind = {custom, NextIndexes}}}
                end,
            Item = ?MODULE:get(Path, Index),
            {Index, Item, NextIterator}
    end;
next(none) ->
    none.

-spec pop(PathValue) -> {PathValue, none | {some, Segment}} when PathValue :: t(), Segment :: segment().
pop(PathValue0 = #argo_path_value{segments = Segments0}) ->
    case array:size(Segments0) of
        0 ->
            {PathValue0, none};
        Size when is_integer(Size) andalso Size > 0 ->
            Index = Size - 1,
            Segment = array:get(Index, Segments0),
            Segments1 = array:set(Index, array:default(Segments0), Segments0),
            Segments2 = array:resize(Size - 1, Segments1),
            Segments3 = shrink(Segments2),
            PathValue1 = PathValue0#argo_path_value{segments = Segments3},
            {PathValue1, {some, Segment}}
    end.

-spec push_field_name(PathValue, Name) -> PathValue when PathValue :: t(), Name :: argo_types:name().
push_field_name(PathValue0 = #argo_path_value{}, Name) when is_binary(Name) ->
    push(PathValue0, {field_name, Name}).

-spec push_list_index(PathValue, Index) -> PathValue when PathValue :: t(), Index :: non_neg_integer().
push_list_index(PathValue0 = #argo_path_value{}, Index) when ?is_usize(Index) ->
    push(PathValue0, {list_index, Index}).

-spec size(PathValue) -> non_neg_integer() when PathValue :: t().
size(#argo_path_value{segments = Segments}) ->
    array:size(Segments).

-spec to_dotted_string(PathValue) -> DottedString when PathValue :: t(), DottedString :: unicode:unicode_binary().
to_dotted_string(PathValue = #argo_path_value{}) ->
    erlang:iolist_to_binary(
        lists:join(<<".">>, [
            case Segment of
                _ when is_binary(Segment) ->
                    Segment;
                _ when is_integer(Segment) ->
                    integer_to_binary(Segment)
            end
         || Segment <- to_list(PathValue)
        ])
    ).

-spec to_list(PathValue) -> SegmentList when PathValue :: t(), SegmentList :: segment_list().
to_list(#argo_path_value{segments = Segments}) ->
    array:foldr(fun to_list/3, [], Segments).

-spec to_wire_path(PathValue, WireType) -> WirePath when
    PathValue :: t(), WireType :: argo_wire_type:t(), WirePath :: argo_wire_path:t().
to_wire_path(PathValue = #argo_path_value{}, WireType = #argo_wire_type{}) ->
    PathValueSegmentList = to_list(PathValue),
    WirePathSegmentList = argo_typer:path_to_wire_path(WireType, PathValueSegmentList),
    WirePath = argo_wire_path:from_list(WirePathSegmentList),
    WirePath.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec from_list(NameOrIndex, Acc) -> Acc when NameOrIndex :: argo_types:name() | non_neg_integer(), Acc :: t().
from_list(Name, Acc) when is_binary(Name) ->
    push_field_name(Acc, Name);
from_list(Index, Acc) when ?is_usize(Index) ->
    push_list_index(Acc, Index).

%% @private
-spec iterator_internal(PathValue, OrderFun) -> InternalIterator when
    PathValue :: t(), OrderFun :: order_func(), InternalIterator :: [index()].
iterator_internal(PathValue = #argo_path_value{segments = Segments}, OrderFun) when is_function(OrderFun, 4) ->
    InternalIterator = lists:sort(
        fun(AIndex, BIndex) ->
            AItem = array:get(AIndex, Segments),
            BItem = array:get(BIndex, Segments),
            OrderFun(AIndex, AItem, BIndex, BItem)
        end,
        lists:seq(0, ?MODULE:size(PathValue) - 1)
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

%% @private
-spec push(PathValue, Segment) -> PathValue when PathValue :: t(), Segment :: segment().
push(PathValue0 = #argo_path_value{segments = Segments0}, Segment) ->
    Size = array:size(Segments0),
    Segments1 = array:resize(Size + 1, Segments0),
    Segments2 = array:set(Size, Segment, Segments1),
    PathValue1 = PathValue0#argo_path_value{segments = Segments2},
    PathValue1.

%% @private
-spec shrink(Segments) -> Segments when
    Segments :: array:array(Segment),
    Segment :: segment().
shrink(Segments) ->
    %% TODO: make this less expensive, maybe upstream fix
    List = array:to_list(Segments),
    argo_types:dynamic_cast(array:fix(array:from_list(List, undefined))).

%% @private
-spec to_list(Index, Segment, Acc) -> Acc when Index :: non_neg_integer(), Segment :: segment(), Acc :: segment_list().
to_list(_Index, {field_name, Name}, Acc) ->
    [Name | Acc];
to_list(_Index, {list_index, Index}, Acc) ->
    [Index | Acc].
