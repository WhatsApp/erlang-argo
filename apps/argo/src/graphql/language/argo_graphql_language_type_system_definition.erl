%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_type_system_definition).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    schema_definition/2,
    type_definition/2,
    directive_definition/2
]).
%% Instance API
-export([
    is_ambiguous/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_graphql_language_schema_definition:t()
    | argo_graphql_language_type_definition:t()
    | argo_graphql_language_directive_definition:t().
-type t() :: #argo_graphql_language_type_system_definition{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [schema_definition/2]}).
-spec schema_definition(SchemaDefinition, Location) -> TypeSystemDefinition when
    SchemaDefinition :: argo_graphql_language_schema_definition:t(),
    Location :: erl_anno:location(),
    TypeSystemDefinition :: t().
schema_definition(SchemaDefinition = #argo_graphql_language_schema_definition{}, Location) ->
    #argo_graphql_language_type_system_definition{location = Location, inner = SchemaDefinition}.

-compile({inline, [type_definition/2]}).
-spec type_definition(TypeDefinition, Location) -> TypeSystemDefinition when
    TypeDefinition :: argo_graphql_language_type_definition:t(),
    Location :: erl_anno:location(),
    TypeSystemDefinition :: t().
type_definition(TypeDefinition = #argo_graphql_language_type_definition{}, Location) ->
    #argo_graphql_language_type_system_definition{location = Location, inner = TypeDefinition}.

-compile({inline, [directive_definition/2]}).
-spec directive_definition(DirectiveDefinition, Location) -> TypeSystemDefinition when
    DirectiveDefinition :: argo_graphql_language_directive_definition:t(),
    Location :: erl_anno:location(),
    TypeSystemDefinition :: t().
directive_definition(DirectiveDefinition = #argo_graphql_language_directive_definition{}, Location) ->
    #argo_graphql_language_type_system_definition{location = Location, inner = DirectiveDefinition}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_language_type_system_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_schema_definition{} ->
            argo_graphql_language_schema_definition:is_ambiguous(Inner);
        #argo_graphql_language_type_definition{} ->
            argo_graphql_language_type_definition:is_ambiguous(Inner);
        #argo_graphql_language_directive_definition{} ->
            false
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_type_system_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_schema_definition{} ->
            argo_graphql_language_schema_definition:format(Formatter1, Inner);
        #argo_graphql_language_type_definition{} ->
            argo_graphql_language_type_definition:format(Formatter1, Inner);
        #argo_graphql_language_directive_definition{} ->
            argo_graphql_language_directive_definition:format(Formatter1, Inner)
    end.
