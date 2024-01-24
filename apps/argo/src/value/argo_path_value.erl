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
-module(argo_path_value).
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
    new/0,
    pop/1,
    push_field_name/2,
    push_list_index/2,
    size/1,
    to_list/1,
    to_wire_path/2
]).

%% Types
-type segment() :: {field_name, argo_types:name()} | {list_index, non_neg_integer()}.
-type segment_list() :: [argo_types:name() | non_neg_integer()].
-type t() :: #argo_path_value{}.

-export_type([
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

-spec new() -> PathValue when PathValue :: t().
new() ->
    #argo_path_value{segments = argo_types:dynamic_cast(array:new(0, fixed))}.

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
            PathValue1 = PathValue0#argo_path_value{segments = Segments2},
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
-spec push(PathValue, Segment) -> PathValue when PathValue :: t(), Segment :: segment().
push(PathValue0 = #argo_path_value{segments = Segments0}, Segment) ->
    Size = array:size(Segments0),
    Segments1 = array:resize(Size + 1, Segments0),
    Segments2 = array:set(Size, Segment, Segments1),
    PathValue1 = PathValue0#argo_path_value{segments = Segments2},
    PathValue1.

%% @private
-spec to_list(Index, Segment, Acc) -> Acc when Index :: non_neg_integer(), Segment :: segment(), Acc :: segment_list().
to_list(_Index, {field_name, Name}, Acc) ->
    [Name | Acc];
to_list(_Index, {list_index, Index}, Acc) ->
    [Index | Acc].
