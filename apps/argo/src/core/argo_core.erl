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
-module(argo_core).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").

%% New API
-export([
    absent/0,
    backreference/1,
    error/0,
    length/1,
    non_null/0,
    null/0
]).

%% Instance API
-export([
    is_absent/1,
    is_backreference/1,
    is_error/1,
    is_length/1,
    is_non_null/1,
    is_null/1
]).

%% Types
-type labeled_type() :: {backreference, argo_types:backreference()} | {length, argo_types:length()}.
-type nullable_type() :: null | non_null | error.
-type omittable_type() :: absent | non_null.

-export_type([
    labeled_type/0,
    nullable_type/0,
    omittable_type/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [absent/0]}).
-spec absent() -> absent.
absent() ->
    absent.

-compile({inline, [backreference/1]}).
-spec backreference(Backreference) -> {backreference, Backreference} when Backreference :: argo_types:backreference().
backreference(Backreference) when ?is_u32(Backreference) ->
    {backreference, Backreference}.

-compile({inline, [error/0]}).
-spec error() -> error.
error() ->
    error.

-spec length(Length) -> {length, Length} when Length :: argo_types:length().
length(Length) when ?is_u32(Length) ->
    {length, Length}.

-compile({inline, [non_null/0]}).
-spec non_null() -> non_null.
non_null() ->
    non_null.

-compile({inline, [null/0]}).
-spec null() -> null.
null() ->
    null.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-compile({inline, [is_absent/1]}).
-spec is_absent(OmittableType) -> boolean() when OmittableType :: omittable_type().
is_absent(absent) -> true;
is_absent(_) -> false.

-compile({inline, [is_backreference/1]}).
-spec is_backreference(LabeledType) -> boolean() when LabeledType :: labeled_type().
is_backreference({backreference, _}) -> true;
is_backreference(_) -> false.

-compile({inline, [is_error/1]}).
-spec is_error(NullableType) -> boolean() when NullableType :: nullable_type().
is_error(error) -> true;
is_error(_) -> false.

-compile({inline, [is_length/1]}).
-spec is_length(LabeledType) -> boolean() when LabeledType :: labeled_type().
is_length({length, _}) -> true;
is_length(_) -> false.

-compile({inline, [is_non_null/1]}).
-spec is_non_null(NullableType | OmittableType) -> boolean() when
    NullableType :: nullable_type(), OmittableType :: omittable_type().
is_non_null(non_null) -> true;
is_non_null(_) -> false.

-compile({inline, [is_null/1]}).
-spec is_null(NullableType) -> boolean() when NullableType :: nullable_type().
is_null(non_null) -> true;
is_null(_) -> false.
