%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(proper_argo_typer).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-08-14"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Context API
-export([
    derive_wire_type_context/4,
    value_context/5
]).
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

%% Types
-type derive_wire_type_context() :: #{
    service_document := argo_graphql_service_document:t(),
    executable_document := argo_graphql_executable_document:t(),
    operation_name := none | {some, argo_types:name()},
    wire_type := argo_wire_type:t()
}.
-type value_context() :: #{
    service_document := argo_graphql_service_document:t(),
    executable_document := argo_graphql_executable_document:t(),
    operation_name := none | {some, argo_types:name()},
    wire_type := argo_wire_type:t(),
    value := argo_value:t()
}.

-export_type([
    derive_wire_type_context/0,
    value_context/0
]).

%%%=============================================================================
%%% Context API functions
%%%=============================================================================

-spec derive_wire_type_context(ServiceDocument, ExecutableDocument, OptionOperationName, WireType) ->
    DeriveWireTypeContext
when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    OptionOperationName :: none | {some, argo_types:name()},
    WireType :: argo_wire_type:t(),
    DeriveWireTypeContext :: derive_wire_type_context().
derive_wire_type_context(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument = #argo_graphql_executable_document{},
    OptionOperationName,
    WireType = #argo_wire_type{}
) when ?is_option_binary(OptionOperationName) ->
    #{
        service_document => ServiceDocument,
        executable_document => ExecutableDocument,
        operation_name => OptionOperationName,
        wire_type => WireType
    }.

-spec value_context(ServiceDocument, ExecutableDocument, OptionOperationName, WireType, Value) -> ValueContext when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    OptionOperationName :: none | {some, argo_types:name()},
    WireType :: argo_wire_type:t(),
    Value :: argo_value:t(),
    ValueContext :: value_context().
value_context(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument = #argo_graphql_executable_document{},
    OptionOperationName,
    WireType = #argo_wire_type{},
    Value = #argo_value{}
) when ?is_option_binary(OptionOperationName) ->
    #{
        service_document => ServiceDocument,
        executable_document => ExecutableDocument,
        operation_name => OptionOperationName,
        wire_type => WireType,
        value => Value
    }.

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
    exactly(derive_wire_type_context(ServiceDocument, ExecutableDocument, OptionOperationName, WireType)).

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
        #{
            service_document := ServiceDocument2,
            executable_document := ExecutableDocument2,
            operation_name := OptionOperationName,
            wire_type := WireType
        },
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
    WireType = #argo_wire_type{inner = RecordWireType = #argo_record_wire_type{}}
) when ?is_option_binary(OptionOperationName) ->
    {ok, #argo_field_wire_type{'of' = DataWireType}} = argo_record_wire_type:find(RecordWireType, <<"data">>),
    ?LET(
        Value,
        proper_argo:maybe_root_wire_type(DataWireType, proper_argo:value(WireType)),
        exactly(value_context(ServiceDocument, ExecutableDocument, OptionOperationName, WireType, Value))
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
