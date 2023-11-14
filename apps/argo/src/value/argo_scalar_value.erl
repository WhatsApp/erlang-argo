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
-module(argo_scalar_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_value.hrl").

%% New API
-export([
    boolean/1,
    bytes/1,
    fixed/1,
    float64/1,
    string/1,
    varint/1
]).

%% Instance API
-export([
    deduplicate_by_default/1,
    is_boolean/1,
    is_bytes/1,
    is_fixed/1,
    is_fixed_length/2,
    is_float64/1,
    is_labeled/1,
    is_string/1,
    is_varint/1,
    supports_deduplication/1,
    to_scalar_wire_type/1
]).

%% Types
-type inner() ::
    {boolean, boolean()}
    | {bytes, binary()}
    | {fixed, binary()}
    | {float64, float()}
    | {string, unicode:unicode_binary()}
    | {varint, argo_types:i64()}.
-type t() :: #argo_scalar_value{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec boolean(boolean()) -> ScalarValue when ScalarValue :: t().
boolean(V) when erlang:is_boolean(V) -> #argo_scalar_value{inner = {boolean, V}}.

-spec bytes(binary()) -> ScalarValue when ScalarValue :: t().
bytes(V) when is_binary(V) -> #argo_scalar_value{inner = {bytes, V}}.

-spec fixed(binary()) -> ScalarValue when ScalarValue :: t().
fixed(V) when is_binary(V) -> #argo_scalar_value{inner = {fixed, V}}.

-spec float64(float()) -> ScalarValue when ScalarValue :: t().
float64(V) when is_float(V) -> #argo_scalar_value{inner = {float64, V}}.

-spec string(binary()) -> ScalarValue when ScalarValue :: t().
string(V) when is_binary(V) -> #argo_scalar_value{inner = {string, V}}.

-spec varint(argo_types:i64()) -> ScalarValue when ScalarValue :: t().
varint(V) when ?is_i64(V) -> #argo_scalar_value{inner = {varint, V}}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec deduplicate_by_default(ScalarValue) -> boolean() when ScalarValue :: t().
deduplicate_by_default(#argo_scalar_value{inner = {string, _}}) -> true;
deduplicate_by_default(#argo_scalar_value{inner = {bytes, _}}) -> true;
deduplicate_by_default(#argo_scalar_value{}) -> false.

-spec is_boolean(ScalarValue) -> boolean() when ScalarValue :: t().
is_boolean(#argo_scalar_value{inner = {T, _}}) -> T =:= boolean.
-spec is_bytes(ScalarValue) -> boolean() when ScalarValue :: t().
is_bytes(#argo_scalar_value{inner = {T, _}}) -> T =:= bytes.
-spec is_fixed(ScalarValue) -> boolean() when ScalarValue :: t().
is_fixed(#argo_scalar_value{inner = {T, _}}) -> T =:= fixed.
-spec is_fixed_length(ScalarValue, Length) -> boolean() when ScalarValue :: t(), Length :: argo_types:length().
is_fixed_length(#argo_scalar_value{inner = {fixed, V}}, Length) -> byte_size(V) =:= Length;
is_fixed_length(#argo_scalar_value{}, _Length) -> false.
-spec is_float64(ScalarValue) -> boolean() when ScalarValue :: t().
is_float64(#argo_scalar_value{inner = {T, _}}) -> T =:= float64.

-spec is_labeled(ScalarValue) -> boolean() when ScalarValue :: t().
is_labeled(#argo_scalar_value{inner = {string, _}}) -> true;
is_labeled(#argo_scalar_value{inner = {boolean, _}}) -> true;
is_labeled(#argo_scalar_value{inner = {bytes, _}}) -> true;
is_labeled(#argo_scalar_value{}) -> false.

-spec is_string(ScalarValue) -> boolean() when ScalarValue :: t().
is_string(#argo_scalar_value{inner = {T, _}}) -> T =:= string.

-spec is_varint(ScalarValue) -> boolean() when ScalarValue :: t().
is_varint(#argo_scalar_value{inner = {T, _}}) -> T =:= varint.

-spec supports_deduplication(ScalarValue) -> boolean() when ScalarValue :: t().
supports_deduplication(#argo_scalar_value{inner = {string, _}}) -> true;
supports_deduplication(#argo_scalar_value{inner = {bytes, _}}) -> true;
supports_deduplication(#argo_scalar_value{}) -> false.

-spec to_scalar_wire_type(ScalarValue) -> ScalarWireType when
    ScalarValue :: t(), ScalarWireType :: argo_scalar_wire_type:t().
to_scalar_wire_type(#argo_scalar_value{inner = {boolean, _}}) ->
    argo_scalar_wire_type:boolean();
to_scalar_wire_type(#argo_scalar_value{inner = {bytes, _}}) ->
    argo_scalar_wire_type:bytes();
to_scalar_wire_type(#argo_scalar_value{inner = {fixed, Fixed}}) ->
    argo_scalar_wire_type:fixed(byte_size(Fixed));
to_scalar_wire_type(#argo_scalar_value{inner = {float64, _}}) ->
    argo_scalar_wire_type:float64();
to_scalar_wire_type(#argo_scalar_value{inner = {string, _}}) ->
    argo_scalar_wire_type:string();
to_scalar_wire_type(#argo_scalar_value{inner = {varint, _}}) ->
    argo_scalar_wire_type:varint().
