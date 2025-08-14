%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_non_null_type).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/1
]).
%% Instance API
-export([
    get_type_name/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_non_null_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec from_language(LanguageNonNullType) -> NonNullType when
    LanguageNonNullType :: argo_graphql_language_non_null_type:t(), NonNullType :: t().
from_language(#argo_graphql_language_non_null_type{type = #argo_graphql_language_named_type{name = NamedType}}) when
    is_binary(NamedType)
->
    new(NamedType);
from_language(#argo_graphql_language_non_null_type{type = LanguageListType = #argo_graphql_language_list_type{}}) ->
    ListType = argo_graphql_list_type:from_language(LanguageListType),
    new(ListType).

-compile({inline, [new/1]}).
-spec new(Type) -> NonNullType when Type :: argo_types:name() | argo_graphql_list_type:t(), NonNullType :: t().
new(NamedType) when is_binary(NamedType) ->
    #argo_graphql_non_null_type{
        type = NamedType
    };
new(ListType = #argo_graphql_list_type{}) ->
    #argo_graphql_non_null_type{
        type = ListType
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec get_type_name(NonNullType) -> NamedType when NonNullType :: t(), NamedType :: argo_types:name().
get_type_name(#argo_graphql_non_null_type{type = NamedType}) when is_binary(NamedType) ->
    NamedType;
get_type_name(#argo_graphql_non_null_type{type = ListType = #argo_graphql_list_type{}}) ->
    argo_graphql_list_type:get_type_name(ListType).

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_non_null_type{type = Type}) ->
    Formatter2 =
        case Type of
            NamedType when is_binary(NamedType) ->
                argo_graphql_formatter:write(Formatter1, "~ts", [NamedType]);
            ListType = #argo_graphql_list_type{} ->
                argo_graphql_list_type:format(Formatter1, ListType)
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "!", []),
    Formatter3.
