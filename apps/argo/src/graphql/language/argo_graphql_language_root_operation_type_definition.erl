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
-module(argo_graphql_language_root_operation_type_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% API
-export([
    parse/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type operation_type() :: 'query' | 'mutation' | 'subscription'.
-type context() :: #{
    operation_type := operation_type(),
    named_type := argo_graphql_language_named_type:t()
}.
-type t() :: #argo_graphql_language_root_operation_type_definition{}.

-export_type([
    operation_type/0,
    context/0,
    t/0
]).

%% Macros
-define(is_operation_type(T), ((T) =:= 'query' orelse (T) =:= 'mutation' orelse (T) =:= 'subscription')).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> RootOperationTypeDefinition when
    Context :: context(), Location :: erl_anno:location(), RootOperationTypeDefinition :: t().
parse(
    _Context = #{operation_type := OperationType, named_type := NamedType = #argo_graphql_language_named_type{}},
    Location
) when ?is_operation_type(OperationType) ->
    #argo_graphql_language_root_operation_type_definition{
        location = Location,
        operation_type = OperationType,
        named_type = NamedType
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_root_operation_type_definition{
    operation_type = OperationType, named_type = NamedType
}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "~ts: ", [OperationType]),
    Formatter3 = argo_graphql_language_named_type:format(Formatter2, NamedType),
    Formatter3.
