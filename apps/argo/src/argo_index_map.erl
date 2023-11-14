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
-module(argo_index_map).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_index_map.hrl").

%% API
-export([
    find/2,
    find_full/2,
    find_index/2,
    find_index_of/2,
    first/1,
    foldl/3,
    foldr/3,
    from_list/1,
    get/2,
    get_full/2,
    get_index/2,
    get_index_of/2,
    is_index/2,
    is_key/2,
    iterator/1,
    iterator/2,
    keys/1,
    last/1,
    new/0,
    next/1,
    put/3,
    size/1,
    remove/2,
    remove_index/2,
    take/2,
    take_full/2,
    take_index/2,
    take_index_of/2,
    to_list/1
]).

%% Types
-type index() :: non_neg_integer().
-type key() :: dynamic().
-type value() :: dynamic().
-type t(KeyType, ValueType) :: #argo_index_map{
    indices :: #{KeyType => index()}, entries :: array:array({KeyType, ValueType})
}.
-type t(KeyType) :: t(KeyType, value()).
-type t() :: t(key(), value()).
-type iterator() :: iterator(key(), value()).
-opaque iterator(KeyType, ValueType) ::
    none
    | {{ordered, index()} | {reversed, index()} | {custom, [{index(), {KeyType, ValueType}}]}, t(KeyType, ValueType)}.
-type iterator_order() :: iterator_order(key()).
-type iterator_order(Key) ::
    ordered | reversed | iterator_order_func(Key).
-type iterator_order_func(Key) :: fun((AIndex :: index(), AKey :: Key, BIndex :: index(), BKey :: Key) -> boolean()).

-export_type([
    index/0,
    iterator/0,
    iterator/2,
    iterator_order/0,
    iterator_order/1,
    iterator_order_func/1,
    key/0,
    t/0,
    t/1,
    t/2,
    value/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec find(Key, IndexMap) -> {ok, Value} | error when Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
find(Key, IndexMap) ->
    case find_full(Key, IndexMap) of
        {ok, {_Index, Key, Value}} ->
            {ok, Value};
        error ->
            error
    end.

-spec find_full(Key, IndexMap) -> {ok, {Index, Key, Value}} | error when
    Index :: index(), Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
find_full(Key, IndexMap = #argo_index_map{entries = Entries}) ->
    case find_index_of(Key, IndexMap) of
        {ok, Index} ->
            {Key, Value} = array:get(Index, Entries),
            {ok, {Index, Key, Value}};
        error ->
            error
    end.

-spec find_index(Index, IndexMap) -> {ok, {Key, Value}} | error when
    Index :: index(), Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
find_index(Index, #argo_index_map{entries = Entries}) when is_integer(Index) andalso Index >= 0 ->
    case Index < array:size(Entries) of
        true ->
            {Key, Value} = array:get(Index, Entries),
            {ok, {Key, Value}};
        false ->
            error
    end.

-spec find_index_of(Key, IndexMap) -> {ok, Index} | error when Index :: index(), Key :: key(), IndexMap :: t(Key).
find_index_of(Key, #argo_index_map{indices = Indices}) ->
    case maps:find(Key, Indices) of
        {ok, Index} ->
            {ok, Index};
        error ->
            error
    end.

-spec first(IndexMap) -> {ok, {Key, Value}} | error when IndexMap :: t(), Key :: key(), Value :: value().
first(IndexMap = #argo_index_map{}) ->
    find_index(0, IndexMap).

-spec foldl(Function, Acc0, IndexMapOrIterator) -> Acc1 when
    Function :: fun((index(), Key, Value, AccIn) -> AccOut),
    Acc0 :: dynamic(),
    Key :: key(),
    Value :: value(),
    IndexMapOrIterator :: t(Key, Value) | iterator(Key, Value),
    Acc1 :: dynamic(),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldl(Function, Init, IndexMap = #argo_index_map{}) when is_function(Function, 4) ->
    Iterator = iterator(IndexMap, ordered),
    foldl_iterator(Iterator, Function, Init);
foldl(Function, Init, Iterator = {{_, _}, #argo_index_map{}}) when is_function(Function, 4) ->
    foldl_iterator(Iterator, Function, Init).

-spec foldr(Function, Acc0, IndexMapOrIterator) -> Acc1 when
    Function :: fun((index(), Key, Value, AccIn) -> AccOut),
    Acc0 :: dynamic(),
    Key :: key(),
    Value :: value(),
    IndexMapOrIterator :: t(Key, Value) | iterator(Key, Value),
    Acc1 :: dynamic(),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldr(Function, Init, IndexMap = #argo_index_map{}) when is_function(Function, 4) ->
    Iterator = iterator(IndexMap, reversed),
    foldl_iterator(Iterator, Function, Init);
foldr(Function, Init, Iterator = {{_, _}, #argo_index_map{}}) when is_function(Function, 4) ->
    foldr_iterator(Iterator, Function, Init, []).

-spec from_list(KeyValueList) -> IndexMap when
    KeyValueList :: [{Key, Value}], Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
from_list([]) ->
    new();
from_list(List) when is_list(List) ->
    lists:foldl(fun from_list/2, new(), List).

-spec get(Key, IndexMap) -> Value when Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
get(Key, IndexMap = #argo_index_map{}) ->
    case find(Key, IndexMap) of
        {ok, Value} ->
            Value;
        error ->
            erlang:error({badkey, Key}, [Key, IndexMap], [{error_info, #{module => ?MODULE}}])
    end.

-spec get_full(Key, IndexMap) -> {Index, Key, Value} when
    Index :: index(), Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
get_full(Key, IndexMap = #argo_index_map{}) ->
    case find_full(Key, IndexMap) of
        {ok, {Index, Key, Value}} ->
            {Index, Key, Value};
        error ->
            erlang:error({badkey, Key}, [Key, IndexMap], [{error_info, #{module => ?MODULE}}])
    end.

-spec get_index(Index, IndexMap) -> {Key, Value} when
    Index :: index(), Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
get_index(Index, IndexMap = #argo_index_map{}) when is_integer(Index) andalso Index >= 0 ->
    case find_index(Index, IndexMap) of
        {ok, {Key, Value}} ->
            {Key, Value};
        error ->
            erlang:error({badkey, Index}, [Index, IndexMap], [{error_info, #{module => ?MODULE}}])
    end.

-spec get_index_of(Key, IndexMap) -> Index when Index :: index(), Key :: key(), IndexMap :: t(Key).
get_index_of(Key, IndexMap = #argo_index_map{}) ->
    case find_index_of(Key, IndexMap) of
        {ok, Index} ->
            Index;
        error ->
            erlang:error({badkey, Key}, [Key, IndexMap], [{error_info, #{module => ?MODULE}}])
    end.

-spec is_index(Index, IndexMap) -> boolean() when Index :: index(), IndexMap :: t().
is_index(Index, IndexMap = #argo_index_map{}) ->
    Index < ?MODULE:size(IndexMap).

-spec is_key(Key, IndexMap) -> boolean() when Key :: key(), IndexMap :: t(Key).
is_key(Key, #argo_index_map{indices = Indices}) ->
    maps:is_key(Key, Indices).

-spec iterator(IndexMap) -> Iterator when
    Key :: key(), Value :: value(), IndexMap :: t(Key, Value), Iterator :: iterator(Key, Value).
iterator(IndexMap = #argo_index_map{}) ->
    iterator(IndexMap, ordered).

-spec iterator(IndexMap, Order) -> Iterator when
    Key :: key(),
    Value :: value(),
    IndexMap :: t(Key, Value),
    Order :: iterator_order(Key),
    Iterator :: iterator(Key, Value).
iterator(IndexMap = #argo_index_map{}, ordered) ->
    {{ordered, 0}, IndexMap};
iterator(IndexMap = #argo_index_map{}, reversed) ->
    {{reversed, ?MODULE:size(IndexMap)}, IndexMap};
iterator(IndexMap = #argo_index_map{entries = Entries}, OrderFun) when is_function(OrderFun, 4) ->
    Custom = lists:sort(
        fun({AIndex, {AKey, _AValue}}, {BIndex, {BKey, _BValue}}) ->
            OrderFun(AIndex, AKey, BIndex, BKey)
        end,
        array:to_orddict(Entries)
    ),
    {{custom, Custom}, IndexMap}.

-spec keys(IndexMapOrIterator) -> Keys when
    Key :: key(), Value :: value(), IndexMapOrIterator :: t(Key, Value) | iterator(Key, Value), Keys :: [Key].
keys(#argo_index_map{entries = Entries}) ->
    array:foldr(fun collect_keys/3, [], Entries);
keys(Iterator = {{_, _}, #argo_index_map{}}) ->
    lists:reverse(foldl_iterator(Iterator, fun collect_keys/4, [])).

-spec last(IndexMap) -> {ok, {Key, Value}} | error when Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
last(IndexMap = #argo_index_map{entries = Entries}) ->
    case array:size(Entries) of
        0 ->
            error;
        Size when Size > 0 ->
            find_index(Size - 1, IndexMap)
    end.

-spec new() -> IndexMap when IndexMap :: t().
new() ->
    #argo_index_map{
        indices = maps:new(),
        entries = dynamic_cast(array:new(0, fixed))
    }.

-spec next(Iterator) -> none | {Index, Key, Value, NextIterator} when
    Index :: index(),
    Key :: key(),
    Value :: value(),
    Iterator :: iterator(Key, Value),
    NextIterator :: iterator(Key, Value).
next({{ordered, Index}, IndexMap = #argo_index_map{}}) when is_integer(Index) andalso Index >= 0 ->
    case find_index(Index, IndexMap) of
        {ok, {Key, Value}} ->
            NextIterator =
                case is_index(Index + 1, IndexMap) of
                    true ->
                        {{ordered, Index + 1}, IndexMap};
                    false ->
                        none
                end,
            {Index, Key, Value, NextIterator};
        error ->
            none
    end;
next({{reversed, Index}, IndexMap = #argo_index_map{}}) when is_integer(Index) andalso Index >= 0 ->
    case find_index(Index - 1, IndexMap) of
        {ok, {Key, Value}} ->
            NextIterator =
                case Index - 2 >= 0 andalso is_index(Index - 2, IndexMap) of
                    true ->
                        {{reversed, Index - 1}, IndexMap};
                    false ->
                        none
                end,
            {Index, Key, Value, NextIterator};
        error ->
            none
    end;
next({{custom, [{Index, {Key, Value}} | Custom]}, IndexMap = #argo_index_map{}}) ->
    NextIterator =
        case length(Custom) > 0 of
            true ->
                {{custom, Custom}, IndexMap};
            false ->
                none
        end,
    {Index, Key, Value, NextIterator};
next({{custom, []}, #argo_index_map{}}) ->
    none;
next(none) ->
    none.

-spec put(Key, Value, IndexMap1) -> IndexMap2 when
    Key :: key(), Value :: value(), IndexMap1 :: t(Key, Value), IndexMap2 :: t(Key, Value).
put(Key, Value, IndexMap1 = #argo_index_map{indices = Indices1, entries = Entries1}) ->
    case find_full(Key, IndexMap1) of
        {ok, {_Index, Key, Value}} ->
            IndexMap1;
        {ok, {Index, Key, _OldValue}} ->
            Entries2 = array:set(Index, {Key, Value}, Entries1),
            IndexMap2 = IndexMap1#argo_index_map{entries = Entries2},
            IndexMap2;
        error ->
            Index = array:size(Entries1),
            Indices2 = maps:put(Key, Index, Indices1),
            Entries2 = array:fix(array:set(Index, {Key, Value}, array:relax(Entries1))),
            IndexMap2 = IndexMap1#argo_index_map{indices = Indices2, entries = Entries2},
            IndexMap2
    end.

-spec remove(Key, IndexMap1) -> IndexMap2 when Key :: key(), IndexMap1 :: t(Key), IndexMap2 :: t(Key).
remove(Key, IndexMap1 = #argo_index_map{}) ->
    case take_full(Key, IndexMap1) of
        {{_Index, Key, _Value}, IndexMap2} ->
            IndexMap2;
        error ->
            IndexMap1
    end.

-spec remove_index(Index, IndexMap1) -> IndexMap2 when
    Index :: index(), Key :: key(), IndexMap1 :: t(Key), IndexMap2 :: t(Key).
remove_index(Index, IndexMap1 = #argo_index_map{}) when is_integer(Index) andalso Index >= 0 ->
    case take_index(Index, IndexMap1) of
        {{Index, _Key, _Value}, IndexMap2} ->
            IndexMap2;
        error ->
            IndexMap1
    end.

-spec size(IndexMap) -> non_neg_integer() when IndexMap :: t().
size(#argo_index_map{entries = Entries}) ->
    array:size(Entries).

-spec take(Key, IndexMap1) -> {Value, IndexMap2} | error when
    Key :: key(), Value :: value(), IndexMap1 :: t(Key, Value), IndexMap2 :: t(Key, Value).
take(Key, IndexMap1 = #argo_index_map{}) ->
    case take_full(Key, IndexMap1) of
        {{_Index, Key, Value}, IndexMap2} ->
            {Value, IndexMap2};
        error ->
            error
    end.

-spec take_full(Key, IndexMap1) -> {{Index, Key, Value}, IndexMap2} | error when
    Index :: index(), Key :: key(), Value :: value(), IndexMap1 :: t(Key, Value), IndexMap2 :: t(Key, Value).
take_full(Key, IndexMap1 = #argo_index_map{indices = Indices1, entries = Entries1}) ->
    case maps:take(Key, Indices1) of
        {Index, Indices2} ->
            {Key, Value} = array:get(Index, Entries1),
            Entries2 = array:set(Index, array:default(Entries1), Entries1),
            OldSize = array:size(Entries1),
            NewSize = OldSize - 1,
            {halt, Indices3, Entries3} = array:foldr(fun repair/3, {cont, Indices2, Entries2, NewSize}, Entries2),
            IndexMap2 = IndexMap1#argo_index_map{indices = Indices3, entries = dynamic_cast(Entries3)},
            {{Index, Key, Value}, IndexMap2};
        error ->
            error
    end.

-spec take_index(Index, IndexMap1) -> {{Key, Value}, IndexMap2} | error when
    Index :: index(), Key :: key(), Value :: value(), IndexMap1 :: t(Key, Value), IndexMap2 :: t(Key, Value).
take_index(Index, IndexMap1 = #argo_index_map{indices = Indices1, entries = Entries1}) when
    is_integer(Index) andalso Index >= 0
->
    case Index < array:size(Entries1) of
        true ->
            {Key, Value} = array:get(Index, Entries1),
            {Index, Indices2} = maps:take(Key, Indices1),
            Entries2 = array:set(Index, array:default(Entries1), Entries1),
            OldSize = array:size(Entries1),
            NewSize = OldSize - 1,
            {halt, Indices3, Entries3} = array:foldr(fun repair/3, {cont, Indices2, Entries2, NewSize}, Entries2),
            IndexMap2 = IndexMap1#argo_index_map{indices = Indices3, entries = dynamic_cast(Entries3)},
            {{Key, Value}, IndexMap2};
        false ->
            error
    end.

-spec take_index_of(Key, IndexMap1) -> {Index, IndexMap2} | error when
    Index :: index(), Key :: key(), IndexMap1 :: t(Key), IndexMap2 :: t(Key).
take_index_of(Key, IndexMap1 = #argo_index_map{}) ->
    case take_full(Key, IndexMap1) of
        {{Index, Key, _Value}, IndexMap2} ->
            {Index, IndexMap2};
        error ->
            error
    end.

-spec to_list(IndexMapOrIterator) -> KeyValueList when
    Key :: key(),
    Value :: value(),
    IndexMapOrIterator :: t(Key, Value) | iterator(Key, Value),
    KeyValueList :: [{Key, Value}].
to_list(#argo_index_map{entries = Entries}) ->
    array:foldr(fun collect_pairs/3, [], Entries);
to_list(Iterator = {{_, _}, #argo_index_map{}}) ->
    lists:reverse(foldl_iterator(Iterator, fun collect_pairs/4, [])).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec collect_keys(Index, {Key, Value}, Keys) -> Keys when
    Index :: index(), Key :: key(), Value :: value(), Keys :: [Key].
collect_keys(_Index, {Key, _Value}, Keys) ->
    [Key | Keys].

%% @private
-spec collect_keys(Index, Key, Value, Keys) -> Keys when
    Index :: index(), Key :: key(), Value :: value(), Keys :: [Key].
collect_keys(_Index, Key, _Value, Keys) ->
    [Key | Keys].

%% @private
-spec collect_pairs(Index, {Key, Value}, Pairs) -> Pairs when
    Index :: index(), Key :: key(), Value :: value(), Pairs :: [{Key, Value}].
collect_pairs(_Index, {Key, Value}, Pairs) ->
    [{Key, Value} | Pairs].

%% @private
-spec collect_pairs(Index, Key, Value, Pairs) -> Pairs when
    Index :: index(), Key :: key(), Value :: value(), Pairs :: [{Key, Value}].
collect_pairs(_Index, Key, Value, Pairs) ->
    [{Key, Value} | Pairs].

%% @private
-compile({inline, [dynamic_cast/1]}).
-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.

%% @private
-spec foldl_iterator(Iterator, Function, AccIn) -> AccOut when
    Index :: index(),
    Key :: key(),
    Value :: value(),
    Iterator :: iterator(Key, Value),
    Function :: fun((Index, Key, Value, AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldl_iterator(Iterator, Function, Acc1) ->
    case next(Iterator) of
        none ->
            Acc1;
        {Index, Key, Value, NextIterator} ->
            Acc2 = Function(Index, Key, Value, Acc1),
            foldl_iterator(NextIterator, Function, Acc2)
    end.

%% @private
-spec foldr_iterator(Entries, Function, AccIn) -> AccOut when
    Index :: index(),
    Key :: key(),
    Value :: value(),
    Entries :: [{Index, Key, Value}],
    Function :: fun((Index, Key, Value, AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldr_iterator([{Index, Key, Value} | Entries], Function, Acc1) ->
    Acc2 = Function(Index, Key, Value, Acc1),
    foldr_iterator(Entries, Function, Acc2);
foldr_iterator([], _Function, Acc1) ->
    Acc1.

%% @private
-spec foldr_iterator(Iterator, Function, AccIn, Entries) -> AccOut when
    Index :: index(),
    Key :: key(),
    Value :: value(),
    Iterator :: iterator(Key, Value),
    Function :: fun((Index, Key, Value, AccIn) -> AccOut),
    AccIn :: dynamic(),
    Entries :: [{Index, Key, Value}],
    AccOut :: dynamic().
foldr_iterator(Iterator, Function, Acc1, Entries1) ->
    case next(Iterator) of
        none ->
            foldr_iterator(Entries1, Function, Acc1);
        {Index, Key, Value, NextIterator} ->
            Entries2 = [{Index, Key, Value} | Entries1],
            foldr_iterator(NextIterator, Function, Acc1, Entries2)
    end.

%% @private
-spec from_list({Key, Value}, IndexMap) -> IndexMap when Key :: key(), Value :: value(), IndexMap :: t(Key, Value).
from_list({Key, Value}, IndexMap) ->
    ?MODULE:put(Key, Value, IndexMap).

%% @private
-spec repair(Index, undefined | {Key, Value}, Acc) -> Acc when
    Index :: index(),
    Key :: key(),
    Value :: value(),
    Acc :: {cont, Indices, Entries, NewSize} | {halt, Indices, Entries},
    Indices :: #{Key => Index},
    Entries :: array:array(undefined | {Key, Value}),
    NewSize :: non_neg_integer().
repair(_Index, undefined, {cont, Indices, Entries1, NewSize}) ->
    Entries2 = array:set(NewSize, undefined, Entries1),
    Entries3 = array:resize(NewSize, Entries2),
    {halt, Indices, Entries3};
repair(Index, {Key, Value}, {cont, Indices1, Entries1, NewSize}) ->
    Indices2 = maps:put(Key, Index - 1, Indices1),
    Entries2 = array:set(Index - 1, {Key, Value}, Entries1),
    {cont, Indices2, Entries2, NewSize};
repair(_Index, {_Key, _Value}, Acc = {halt, _Indices, _Entries}) ->
    Acc.
