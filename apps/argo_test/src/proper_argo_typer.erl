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
-module(proper_argo_typer).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Argo Typer API
-export([
    derive_wire_type/0,
    derive_wire_type/1,
    derive_wire_type/2,
    value/0,
    value/1,
    value/2,
    value/4
]).

%%%=============================================================================
%%% GraphQL ExecutableDocument API functions
%%%=============================================================================

-spec derive_wire_type() -> proper_types:type().
derive_wire_type() ->
    ?LET(
        ServiceDocument,
        proper_argo_graphql_service_document:service_document(),
        derive_wire_type(ServiceDocument)
    ).

-spec derive_wire_type(ServiceDocument) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t().
derive_wire_type(ServiceDocument = #argo_graphql_service_document{}) ->
    ?LET(
        ExecutableDocument,
        proper_argo_graphql_executable_document:executable_document(ServiceDocument),
        derive_wire_type(ServiceDocument, ExecutableDocument)
    ).

-spec derive_wire_type(ServiceDocument, ExecutableDocument) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), ExecutableDocument :: argo_graphql_executable_document:t().
derive_wire_type(
    ServiceDocument = #argo_graphql_service_document{}, ExecutableDocument = #argo_graphql_executable_document{}
) ->
    {OptionOperationName, WireType} = argo_typer:derive_wire_type(ServiceDocument, ExecutableDocument, none),
    exactly({ServiceDocument, ExecutableDocument, OptionOperationName, WireType}).

-spec value() -> proper_types:type().
value() ->
    ?LET(
        ServiceDocument,
        proper_argo_graphql_service_document:service_document(),
        value(ServiceDocument)
    ).

-spec value(ServiceDocument) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t().
value(ServiceDocument = #argo_graphql_service_document{}) ->
    ?LET(
        ExecutableDocument,
        proper_argo_graphql_executable_document:executable_document(ServiceDocument),
        value(ServiceDocument, ExecutableDocument)
    ).

-spec value(ServiceDocument, ExecutableDocument) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), ExecutableDocument :: argo_graphql_executable_document:t().
value(ServiceDocument1 = #argo_graphql_service_document{}, ExecutableDocument1 = #argo_graphql_executable_document{}) ->
    ?LET(
        {ServiceDocument2, ExecutableDocument2, OptionOperationName, WireType},
        derive_wire_type(ServiceDocument1, ExecutableDocument1),
        value(ServiceDocument2, ExecutableDocument2, OptionOperationName, WireType)
    ).

-spec value(ServiceDocument, ExecutableDocument, OptionOperationName, WireType) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    OptionOperationName :: none | {some, OperationName},
    OperationName :: argo_types:name(),
    WireType :: argo_wire_type:t().
value(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument = #argo_graphql_executable_document{},
    OptionOperationName,
    WireType = #argo_wire_type{}
) when ?is_option_binary(OptionOperationName) ->
    ?LET(
        Value,
        proper_argo:value(WireType),
        exactly({ServiceDocument, ExecutableDocument, OptionOperationName, WireType, Value})
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
