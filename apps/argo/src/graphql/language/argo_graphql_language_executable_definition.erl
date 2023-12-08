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
-module(argo_graphql_language_executable_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    operation_definition/2,
    fragment_definition/2
]).
%% Instance API
-export([
    is_shorthand/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_graphql_language_operation_definition:t()
    | argo_graphql_language_fragment_definition:t().
-type t() :: #argo_graphql_language_executable_definition{}.

-export_executable_definition([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [operation_definition/2]}).
-spec operation_definition(OperationDefinition, Location) -> Type when
    OperationDefinition :: argo_graphql_language_operation_definition:t(), Location :: erl_anno:location(), Type :: t().
operation_definition(OperationDefinition = #argo_graphql_language_operation_definition{}, Location) ->
    #argo_graphql_language_executable_definition{location = Location, inner = OperationDefinition}.

-compile({inline, [fragment_definition/2]}).
-spec fragment_definition(FragmentDefinition, Location) -> Type when
    FragmentDefinition :: argo_graphql_language_fragment_definition:t(), Location :: erl_anno:location(), Type :: t().
fragment_definition(FragmentDefinition = #argo_graphql_language_fragment_definition{}, Location) ->
    #argo_graphql_language_executable_definition{location = Location, inner = FragmentDefinition}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec is_shorthand(Definition) -> boolean() when Definition :: t().
is_shorthand(#argo_graphql_language_executable_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_operation_definition{} ->
            argo_graphql_language_operation_definition:is_shorthand(Inner);
        #argo_graphql_language_fragment_definition{} ->
            false
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_executable_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_operation_definition{} ->
            argo_graphql_language_operation_definition:format(Formatter1, Inner);
        #argo_graphql_language_fragment_definition{} ->
            argo_graphql_language_fragment_definition:format(Formatter1, Inner)
    end.
