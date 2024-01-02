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
-module(argo_graphql_language_directive_location).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    executable_directive_location/2,
    type_system_directive_location/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_graphql_language_executable_directive_location:t()
    | argo_graphql_language_type_system_directive_location:t().
-type t() :: #argo_graphql_language_directive_location{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [executable_directive_location/2]}).
-spec executable_directive_location(ExecutableDirectiveLocation, Location) -> Type when
    ExecutableDirectiveLocation :: argo_graphql_language_executable_directive_location:t(),
    Location :: erl_anno:location(),
    Type :: t().
executable_directive_location(
    ExecutableDirectiveLocation = #argo_graphql_language_executable_directive_location{}, Location
) ->
    #argo_graphql_language_directive_location{location = Location, inner = ExecutableDirectiveLocation}.

-compile({inline, [type_system_directive_location/2]}).
-spec type_system_directive_location(TypeSystemDirectiveLocation, Location) -> Type when
    TypeSystemDirectiveLocation :: argo_graphql_language_type_system_directive_location:t(),
    Location :: erl_anno:location(),
    Type :: t().
type_system_directive_location(
    TypeSystemDirectiveLocation = #argo_graphql_language_type_system_directive_location{}, Location
) ->
    #argo_graphql_language_directive_location{location = Location, inner = TypeSystemDirectiveLocation}.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_directive_location{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_executable_directive_location{} ->
            argo_graphql_language_executable_directive_location:format(Formatter1, Inner);
        #argo_graphql_language_type_system_directive_location{} ->
            argo_graphql_language_type_system_directive_location:format(Formatter1, Inner)
    end.
