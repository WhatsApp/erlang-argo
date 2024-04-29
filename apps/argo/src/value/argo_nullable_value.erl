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
%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_nullable_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% New API
-export([
    null/1,
    non_null/2,
    field_errors/2
]).

%% Instance API
-export([
    is_labeled/1,
    is_null/1,
    is_non_null/1,
    is_field_errors/1,
    to_nullable_wire_type/1
]).

%% Types
-type inner() :: null | {non_null, argo_value:t()} | {field_errors, [argo_error_value:t()]}.
-type t() :: #argo_nullable_value{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec null(NullableWireType) -> NullableValue when
    NullableWireType :: argo_nullable_wire_type:t(), NullableValue :: t().
null(NullableWireType = #argo_nullable_wire_type{}) ->
    #argo_nullable_value{wire_type = NullableWireType, inner = null}.

-spec non_null(NullableWireType, Value) -> NullableValue when
    NullableWireType :: argo_nullable_wire_type:t(), Value :: argo_value:t(), NullableValue :: t().
non_null(NullableWireType = #argo_nullable_wire_type{}, Value = #argo_value{}) ->
    case argo_value:is_null(Value) of
        false ->
            #argo_nullable_value{wire_type = NullableWireType, inner = {non_null, Value}};
        true ->
            null(NullableWireType)
    end.

-spec field_errors(NullableWireType, FieldErrors) -> NullableValue when
    NullableWireType :: argo_nullable_wire_type:t(), FieldErrors :: [argo_error_value:t()], NullableValue :: t().
field_errors(NullableWireType = #argo_nullable_wire_type{}, FieldErrors) when is_list(FieldErrors) ->
    #argo_nullable_value{wire_type = NullableWireType, inner = {field_errors, FieldErrors}}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-compile({inline, [is_labeled/1]}).
-spec is_labeled(NullableValue) -> boolean() when NullableValue :: t().
is_labeled(#argo_nullable_value{wire_type = NullableWireType}) ->
    argo_nullable_wire_type:is_labeled(NullableWireType).

-spec is_null(NullableValue) -> boolean() when NullableValue :: t().
is_null(#argo_nullable_value{inner = null}) -> true;
is_null(#argo_nullable_value{}) -> false.

-spec is_non_null(NullableValue) -> boolean() when NullableValue :: t().
is_non_null(#argo_nullable_value{inner = {non_null, _}}) -> true;
is_non_null(#argo_nullable_value{}) -> false.

-spec is_field_errors(NullableValue) -> boolean() when NullableValue :: t().
is_field_errors(#argo_nullable_value{inner = {field_errors, _}}) -> true;
is_field_errors(#argo_nullable_value{}) -> false.

-spec to_nullable_wire_type(NullableValue) -> NullableWireType when
    NullableValue :: t(), NullableWireType :: argo_nullable_wire_type:t().
to_nullable_wire_type(#argo_nullable_value{wire_type = NullableWireType = #argo_nullable_wire_type{}}) ->
    NullableWireType.
