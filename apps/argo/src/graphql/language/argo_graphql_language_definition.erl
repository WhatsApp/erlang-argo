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
-module(argo_graphql_language_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    executable_definition/2,
    type_system_definition/2,
    type_system_extension/2
]).
%% Instance API
-export([
    is_ambiguous/1,
    is_shorthand/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_graphql_language_executable_definition:t()
    | argo_graphql_language_type_system_definition:t()
    | argo_graphql_language_type_system_extension:t().
-type t() :: #argo_graphql_language_definition{}.

-export_definition([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [executable_definition/2]}).
-spec executable_definition(ExecutableDefinition, Location) -> Definition when
    ExecutableDefinition :: argo_graphql_language_executable_definition:t(),
    Location :: erl_anno:location(),
    Definition :: t().
executable_definition(ExecutableDefinition = #argo_graphql_language_executable_definition{}, Location) ->
    #argo_graphql_language_definition{location = Location, inner = ExecutableDefinition}.

-compile({inline, [type_system_definition/2]}).
-spec type_system_definition(TypeSystemDefinition, Location) -> Definition when
    TypeSystemDefinition :: argo_graphql_language_type_system_definition:t(),
    Location :: erl_anno:location(),
    Definition :: t().
type_system_definition(TypeSystemDefinition = #argo_graphql_language_type_system_definition{}, Location) ->
    #argo_graphql_language_definition{location = Location, inner = TypeSystemDefinition}.

-compile({inline, [type_system_extension/2]}).
-spec type_system_extension(TypeSystemExtension, Location) -> Definition when
    TypeSystemExtension :: argo_graphql_language_type_system_extension:t(),
    Location :: erl_anno:location(),
    Definition :: t().
type_system_extension(TypeSystemExtension = #argo_graphql_language_type_system_extension{}, Location) ->
    #argo_graphql_language_definition{location = Location, inner = TypeSystemExtension}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_language_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_executable_definition{} ->
            false;
        #argo_graphql_language_type_system_definition{} ->
            argo_graphql_language_type_system_definition:is_ambiguous(Inner);
        #argo_graphql_language_type_system_extension{} ->
            argo_graphql_language_type_system_extension:is_ambiguous(Inner)
    end.

-spec is_shorthand(Definition) -> boolean() when Definition :: t().
is_shorthand(#argo_graphql_language_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_executable_definition{} ->
            argo_graphql_language_executable_definition:is_shorthand(Inner);
        #argo_graphql_language_type_system_definition{} ->
            false;
        #argo_graphql_language_type_system_extension{} ->
            false
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_executable_definition{} ->
            argo_graphql_language_executable_definition:format(Formatter1, Inner);
        #argo_graphql_language_type_system_definition{} ->
            argo_graphql_language_type_system_definition:format(Formatter1, Inner);
        #argo_graphql_language_type_system_extension{} ->
            argo_graphql_language_type_system_extension:format(Formatter1, Inner)
    end.
