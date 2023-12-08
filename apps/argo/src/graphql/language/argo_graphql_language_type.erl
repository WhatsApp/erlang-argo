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
-module(argo_graphql_language_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    named_type/2,
    list_type/2,
    non_null_type/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_graphql_language_named_type:t()
    | argo_graphql_language_list_type:t()
    | argo_graphql_language_non_null_type:t().
-type t() :: #argo_graphql_language_type{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [named_type/2]}).
-spec named_type(NamedType, Location) -> Type when
    NamedType :: argo_graphql_language_named_type:t(), Location :: erl_anno:location(), Type :: t().
named_type(NamedType = #argo_graphql_language_named_type{}, Location) ->
    #argo_graphql_language_type{location = Location, inner = NamedType}.

-compile({inline, [list_type/2]}).
-spec list_type(ListType, Location) -> Type when
    ListType :: argo_graphql_language_list_type:t(), Location :: erl_anno:location(), Type :: t().
list_type(ListType = #argo_graphql_language_list_type{}, Location) ->
    #argo_graphql_language_type{location = Location, inner = ListType}.

-compile({inline, [non_null_type/2]}).
-spec non_null_type(NonNullType, Location) -> Type when
    NonNullType :: argo_graphql_language_non_null_type:t(), Location :: erl_anno:location(), Type :: t().
non_null_type(NonNullType = #argo_graphql_language_non_null_type{}, Location) ->
    #argo_graphql_language_type{location = Location, inner = NonNullType}.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_type{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_named_type{} ->
            argo_graphql_language_named_type:format(Formatter1, Inner);
        #argo_graphql_language_list_type{} ->
            argo_graphql_language_list_type:format(Formatter1, Inner);
        #argo_graphql_language_non_null_type{} ->
            argo_graphql_language_non_null_type:format(Formatter1, Inner)
    end.
