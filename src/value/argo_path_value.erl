%%% % @format
-module(argo_path_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_common.hrl").
-include("argo_value.hrl").

%% API
-export([
    from_list/1,
    new/0,
    pop/1,
    push_field_name/2,
    push_list_index/2,
    to_list/1
]).

%% Types
-type segment() :: {field_name, argo_types:name()} | {list_index, non_neg_integer()}.
-type segments_list() :: [argo_types:name() | non_neg_integer()].
-type t() :: #argo_path_value{}.

-export_type([
    segment/0,
    segments_list/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec from_list(SegmentsList) -> PathValue when SegmentsList :: segments_list(), PathValue :: t().
from_list(SegmentsList) when is_list(SegmentsList) ->
    lists:foldl(fun from_list/2, new(), SegmentsList).

-spec new() -> PathValue when PathValue :: t().
new() ->
    #argo_path_value{segments = array:new(0, fixed)}.

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

-spec to_list(PathValue) -> SegmentsList when PathValue :: t(), SegmentsList :: segments_list().
to_list(#argo_path_value{segments = Segments}) ->
    lists:foldr(fun to_list/3, [], Segments).

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
-spec to_list(Index, Segment, Acc) -> Acc when Index :: non_neg_integer(), Segment :: segment(), Acc :: segments_list().
to_list(_Index, {field_name, Name}, Acc) ->
    [Name | Acc];
to_list(_Index, {list_index, Index}, Acc) ->
    [Index | Acc].
