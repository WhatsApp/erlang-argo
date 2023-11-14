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
-module(argo_error_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").

%% API
-export([
    new/4,
    present_fields_count/1
]).

%% Types
-type t() :: #argo_error_value{}.

-export_type([
    t/0
]).

%% Macros
-define(is_option_none(X), ((X) =:= none)).
-define(is_option_some(X), (is_tuple((X)) andalso tuple_size((X)) =:= 2 andalso element(1, (X)) =:= some)).
-define(is_option_list(X), (?is_option_none(X) orelse (?is_option_some(X) andalso is_list(element(2, (X)))))).
-define(is_option_record(X, T), (?is_option_none(X) orelse (?is_option_some(X) andalso is_record(element(2, (X)), T)))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Message, none | {some, Location}, none | {some, Path}, none | {some, Extensions}) -> ErrorValue when
    Message :: argo_desc_value:desc_string(),
    Location :: [argo_location_value:t()],
    Path :: argo_path_value:t(),
    Extensions :: argo_desc_value:desc_object(),
    ErrorValue :: t().
new(Message, Location, Path, Extensions) when
    is_binary(Message) andalso ?is_option_list(Location) andalso ?is_option_record(Path, argo_path_value) andalso
        ?is_option_record(Extensions, argo_index_map)
->
    #argo_error_value{message = Message, location = Location, path = Path, extensions = Extensions}.

-spec present_fields_count(ErrorValue) -> non_neg_integer() when ErrorValue :: t().
present_fields_count(#argo_error_value{location = Location, path = Path, extensions = Extensions}) ->
    Count1 = 1,
    Count2 =
        case Location of
            none ->
                Count1;
            {some, _} ->
                Count1 + 1
        end,
    Count3 =
        case Path of
            none ->
                Count2;
            {some, _} ->
                Count2 + 1
        end,
    Count4 =
        case Extensions of
            none ->
                Count3;
            {some, _} ->
                Count3 + 1
        end,
    Count4.
