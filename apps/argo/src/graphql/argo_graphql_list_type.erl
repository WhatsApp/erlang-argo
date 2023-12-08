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
%%% Created :  16 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_graphql_list_type).
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
-type t() :: #argo_graphql_list_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageListType) -> ListType when
    LanguageListType :: argo_graphql_language_list_type:t(), ListType :: t().
from_language(#argo_graphql_language_list_type{type = LanguageType}) ->
    Type = argo_graphql_type:from_language(LanguageType),
    new(Type).

-compile({inline, [new/1]}).
-spec new(Type) -> ListType when Type :: argo_graphql_type:t(), ListType :: t().
new(Type = #argo_graphql_type{}) ->
    #argo_graphql_list_type{
        type = Type
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec get_type_name(ListType) -> NamedType when ListType :: t(), NamedType :: argo_types:name().
get_type_name(#argo_graphql_list_type{type = Type}) ->
    argo_graphql_type:get_type_name(Type).

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_list_type{type = Type}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "[", []),
    Formatter3 = argo_graphql_type:format(Formatter2, Type),
    Formatter4 = argo_graphql_formatter:write(Formatter3, "]", []),
    Formatter4.
