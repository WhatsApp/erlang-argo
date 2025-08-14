%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_type_system_extension).
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
    schema_extension/2,
    type_extension/2
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
    argo_graphql_language_schema_extension:t()
    | argo_graphql_language_type_extension:t().
-type t() :: #argo_graphql_language_type_system_extension{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [schema_extension/2]}).
-spec schema_extension(SchemaExtension, Location) -> TypeSystemExtension when
    SchemaExtension :: argo_graphql_language_schema_extension:t(),
    Location :: erl_anno:location(),
    TypeSystemExtension :: t().
schema_extension(SchemaExtension = #argo_graphql_language_schema_extension{}, Location) ->
    #argo_graphql_language_type_system_extension{location = Location, inner = SchemaExtension}.

-compile({inline, [type_extension/2]}).
-spec type_extension(TypeExtension, Location) -> TypeSystemExtension when
    TypeExtension :: argo_graphql_language_type_extension:t(),
    Location :: erl_anno:location(),
    TypeSystemExtension :: t().
type_extension(TypeExtension = #argo_graphql_language_type_extension{}, Location) ->
    #argo_graphql_language_type_system_extension{location = Location, inner = TypeExtension}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_language_type_system_extension{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_schema_extension{} ->
            argo_graphql_language_schema_extension:is_ambiguous(Inner);
        #argo_graphql_language_type_extension{} ->
            argo_graphql_language_type_extension:is_ambiguous(Inner)
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_type_system_extension{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_schema_extension{} ->
            argo_graphql_language_schema_extension:format(Formatter1, Inner);
        #argo_graphql_language_type_extension{} ->
            argo_graphql_language_type_extension:format(Formatter1, Inner)
    end.
