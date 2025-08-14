%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_typer_relay_resolver).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2024-04-22", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_typer_resolver).

-include_lib("argo/include/argo_graphql.hrl").

%% argo_typer_resolver callbacks
-export([
    find_field_definition/3
]).
%% Public API
-export([
    builtin_field_definition/1
]).

%%%=============================================================================
%%% argo_typer_resolver callbacks
%%%=============================================================================

-spec find_field_definition(TypeDefinition, FieldName, ServiceDocument) -> {ok, FieldDefinition} | error when
    TypeDefinition :: argo_graphql_type_definition:t(),
    FieldName :: argo_types:name(),
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldDefinition :: argo_graphql_field_definition:t().
find_field_definition(
    _TypeDefinition = #argo_graphql_type_definition{kind = Kind},
    FieldName,
    _ServiceDocument = #argo_graphql_service_document{}
) when
    is_binary(FieldName)
->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            error;
        #argo_graphql_object_type_definition{} ->
            builtin_field_definition(FieldName);
        #argo_graphql_interface_type_definition{} ->
            builtin_field_definition(FieldName);
        #argo_graphql_union_type_definition{} ->
            builtin_field_definition(FieldName);
        #argo_graphql_enum_type_definition{} ->
            error;
        #argo_graphql_input_object_type_definition{} ->
            error
    end.

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec builtin_field_definition(FieldName) -> {ok, FieldDefinition} | error when
    FieldName :: argo_types:name(), FieldDefinition :: argo_graphql_field_definition:t().
builtin_field_definition(FieldName = <<"strong_id__">>) ->
    Type = argo_graphql_type:named_type(<<"ID">>),
    FieldDefinition = argo_graphql_field_definition:new(FieldName, Type),
    {ok, FieldDefinition};
builtin_field_definition(_FieldName) ->
    error.
