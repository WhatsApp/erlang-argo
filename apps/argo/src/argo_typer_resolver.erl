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
%%% Created :  22 Apr 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_typer_resolver).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_graphql.hrl").

%% Callbacks
-callback find_field_definition(TypeDefinition, FieldName, ServiceDocument) -> {ok, FieldDefinition} | error when
    TypeDefinition :: argo_graphql_type_definition:t(),
    FieldName :: argo_types:name(),
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldDefinition :: argo_graphql_field_definition:t().
-callback find_type_definition(ServiceDocument, TypeName) -> {ok, TypeDefinition} | error when
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeName :: argo_types:name(),
    TypeDefinition :: argo_graphql_type_definition:t().

-optional_callbacks([
    find_field_definition/3,
    find_type_definition/2
]).

%% Public API
-export([
    find_field_definition/4,
    find_type_definition/3
]).

%% Types
-type t() :: module().

-export_type([
    t/0
]).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec find_field_definition(Resolver, TypeDefinition, FieldName, ServiceDocument) -> {ok, FieldDefinition} | error when
    Resolver :: t(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    FieldName :: argo_types:name(),
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldDefinition :: argo_graphql_field_definition:t().
find_field_definition(
    Resolver,
    TypeDefinition = #argo_graphql_type_definition{},
    FieldName,
    ServiceDocument = #argo_graphql_service_document{}
) when is_atom(Resolver) andalso is_binary(FieldName) ->
    {module, Resolver} = code:ensure_loaded(Resolver),
    case erlang:function_exported(Resolver, find_field_definition, 3) of
        false ->
            error;
        true ->
            Resolver:find_field_definition(TypeDefinition, FieldName, ServiceDocument)
    end.

-spec find_type_definition(Resolver, ServiceDocument, TypeName) -> {ok, TypeDefinition} | error when
    Resolver :: t(),
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeName :: argo_types:name(),
    TypeDefinition :: argo_graphql_type_definition:t().
find_type_definition(Resolver, ServiceDocument = #argo_graphql_service_document{}, TypeName) when
    is_atom(Resolver) andalso is_binary(TypeName)
->
    {module, Resolver} = code:ensure_loaded(Resolver),
    case erlang:function_exported(Resolver, find_type_definition, 2) of
        false ->
            error;
        true ->
            Resolver:find_type_definition(ServiceDocument, TypeName)
    end.
