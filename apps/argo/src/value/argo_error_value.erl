%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_error_value).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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

-spec new(Message, none | {some, Locations}, none | {some, Path}, none | {some, Extensions}) -> ErrorValue when
    Message :: argo_desc_value:desc_string(),
    Locations :: [argo_location_value:t()],
    Path :: argo_path_value:t(),
    Extensions :: argo_extensions_value:t(),
    ErrorValue :: t().
new(Message, Locations, Path, Extensions) when
    is_binary(Message) andalso ?is_option_list(Locations) andalso ?is_option_record(Path, argo_path_value) andalso
        ?is_option_record(Extensions, argo_extensions_value)
->
    #argo_error_value{message = Message, locations = Locations, path = Path, extensions = Extensions}.

-spec present_fields_count(ErrorValue) -> non_neg_integer() when ErrorValue :: t().
present_fields_count(#argo_error_value{locations = Locations, path = Path, extensions = Extensions}) ->
    Count1 = 1,
    Count2 =
        case Locations of
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
