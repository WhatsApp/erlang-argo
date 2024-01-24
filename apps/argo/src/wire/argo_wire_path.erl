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
%%% Created :  24 Jan 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_wire_path).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    foldl/3,
    foldr/3,
    from_list/1,
    new/0,
    pop/1,
    push/2,
    size/1,
    to_list/1,
    to_path_value/2
]).

%% Types
-type segment() :: argo_types:usize().
-type segment_list() :: [segment()].
-type t() :: #argo_wire_path{}.

-export_type([
    segment/0,
    segment_list/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec foldl(WirePath, Acc0, Function) -> Acc1 when
    WirePath :: t(),
    Acc0 :: dynamic(),
    Function :: fun((segment(), AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic(),
    Acc1 :: dynamic().
foldl(#argo_wire_path{segments = Segments}, Init, Function) when is_function(Function, 2) ->
    array:foldl(
        fun(_Index, Segment, Acc) ->
            Function(Segment, Acc)
        end,
        Init,
        Segments
    ).

-spec foldr(WirePath, Acc0, Function) -> Acc1 when
    WirePath :: t(),
    Acc0 :: dynamic(),
    Function :: fun((segment(), AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic(),
    Acc1 :: dynamic().
foldr(#argo_wire_path{segments = Segments}, Init, Function) when is_function(Function, 2) ->
    array:foldr(
        fun(_Index, Segment, Acc) ->
            Function(Segment, Acc)
        end,
        Init,
        Segments
    ).

-spec from_list(SegmentList) -> WirePath when SegmentList :: segment_list(), WirePath :: t().
from_list(SegmentList) when is_list(SegmentList) ->
    lists:foldl(fun from_list/2, new(), SegmentList).

-spec new() -> WirePath when WirePath :: t().
new() ->
    #argo_wire_path{segments = argo_types:dynamic_cast(array:new(0, fixed))}.

-spec pop(WirePath) -> {WirePath, none | {some, Segment}} when WirePath :: t(), Segment :: segment().
pop(WirePath1 = #argo_wire_path{segments = Segments1}) ->
    case array:size(Segments1) of
        0 ->
            {WirePath1, none};
        Size when is_integer(Size) andalso Size > 0 ->
            Index = Size - 1,
            Segment = array:get(Index, Segments1),
            Segments2 = array:set(Index, array:default(Segments1), Segments1),
            Segments3 = array:resize(Size - 1, Segments2),
            WirePath2 = WirePath1#argo_wire_path{segments = Segments3},
            {WirePath2, {some, Segment}}
    end.

-spec push(WirePath, Segment) -> WirePath when WirePath :: t(), Segment :: segment().
push(WirePath1 = #argo_wire_path{segments = Segments1}, Segment) when ?is_usize(Segment) ->
    Size = array:size(Segments1),
    Segments2 = array:resize(Size + 1, Segments1),
    Segments3 = array:set(Size, Segment, Segments2),
    WirePath2 = WirePath1#argo_wire_path{segments = Segments3},
    WirePath2.

-spec size(WirePath) -> non_neg_integer() when WirePath :: t().
size(#argo_wire_path{segments = Segments}) ->
    array:size(Segments).

-spec to_list(WirePath) -> SegmentList when WirePath :: t(), SegmentList :: segment_list().
to_list(#argo_wire_path{segments = Segments}) ->
    array:to_list(Segments).

-spec to_path_value(WirePath, WireType) -> PathValue when
    WirePath :: t(), WireType :: argo_wire_type:t(), PathValue :: argo_path_value:t().
to_path_value(WirePath = #argo_wire_path{}, WireType = #argo_wire_type{}) ->
    WirePathSegmentList = to_list(WirePath),
    PathValueSegmentList = argo_typer:wire_path_to_path(WireType, WirePathSegmentList),
    PathValue = argo_path_value:from_list(PathValueSegmentList),
    PathValue.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec from_list(Segment, Acc) -> Acc when Segment :: segment(), Acc :: t().
from_list(Segment, Acc) when ?is_usize(Segment) ->
    push(Acc, Segment).
