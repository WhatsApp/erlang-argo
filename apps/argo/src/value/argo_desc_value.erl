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
-module(argo_desc_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").

%% New API
-export([
    null/0,
    boolean/1,
    object/1,
    list/1,
    string/1,
    bytes/1,
    int/1,
    float/1
]).

%% Instance API
-export([
    is_null/1,
    is_boolean/1,
    is_object/1,
    is_list/1,
    is_string/1,
    is_bytes/1,
    is_int/1,
    is_float/1
]).

%% Types
-type desc_null() :: null.
-type desc_boolean() :: boolean().
-type desc_object() :: argo_index_map:t(unicode:unicode_binary(), t()).
-type desc_list() :: [t()].
-type desc_string() :: unicode:unicode_binary().
-type desc_bytes() :: binary().
-type desc_int() :: argo_types:i64().
-type desc_float() :: float().
-type inner_nested() ::
    {object, desc_object()}
    | {list, desc_list()}.
-type inner_scalar() ::
    desc_null()
    | {boolean, desc_boolean()}
    | {string, desc_string()}
    | {bytes, desc_bytes()}
    | {int, desc_int()}
    | {float, desc_float()}.
-type inner() ::
    inner_nested()
    | inner_scalar().

-type t() :: #argo_desc_value{}.

-export_type([
    desc_null/0,
    desc_boolean/0,
    desc_object/0,
    desc_list/0,
    desc_string/0,
    desc_bytes/0,
    desc_int/0,
    desc_float/0,
    inner_nested/0,
    inner_scalar/0,
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec null() -> DescValue when DescValue :: t().
null() ->
    #argo_desc_value{inner = null}.

-spec boolean(desc_boolean()) -> DescValue when DescValue :: t().
boolean(V) when erlang:is_boolean(V) ->
    #argo_desc_value{inner = {boolean, V}}.

-spec object(desc_object()) -> DescValue when DescValue :: t().
object(V = #argo_index_map{}) ->
    #argo_desc_value{inner = {object, V}}.

-spec list(desc_list()) -> DescValue when DescValue :: t().
list(V) when erlang:is_list(V) ->
    #argo_desc_value{inner = {list, V}}.

-spec string(desc_string()) -> DescValue when DescValue :: t().
string(V) when is_binary(V) ->
    #argo_desc_value{inner = {string, V}}.

-spec bytes(desc_bytes()) -> DescValue when DescValue :: t().
bytes(V) when is_binary(V) ->
    #argo_desc_value{inner = {bytes, V}}.

-spec int(desc_int()) -> DescValue when DescValue :: t().
int(V) when ?is_i64(V) ->
    #argo_desc_value{inner = {int, V}}.

-spec float(desc_float()) -> DescValue when DescValue :: t().
float(V) when erlang:is_float(V) ->
    #argo_desc_value{inner = {float, V}}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec is_null(DescValue) -> boolean() when DescValue :: t().
is_null(#argo_desc_value{inner = null}) -> true;
is_null(#argo_desc_value{}) -> false.

-spec is_boolean(DescValue) -> boolean() when DescValue :: t().
is_boolean(#argo_desc_value{inner = {boolean, _}}) -> true;
is_boolean(#argo_desc_value{}) -> false.

-spec is_object(DescValue) -> boolean() when DescValue :: t().
is_object(#argo_desc_value{inner = {object, _}}) -> true;
is_object(#argo_desc_value{}) -> false.

-spec is_list(DescValue) -> boolean() when DescValue :: t().
is_list(#argo_desc_value{inner = {list, _}}) -> true;
is_list(#argo_desc_value{}) -> false.

-spec is_string(DescValue) -> boolean() when DescValue :: t().
is_string(#argo_desc_value{inner = {string, _}}) -> true;
is_string(#argo_desc_value{}) -> false.

-spec is_bytes(DescValue) -> boolean() when DescValue :: t().
is_bytes(#argo_desc_value{inner = {bytes, _}}) -> true;
is_bytes(#argo_desc_value{}) -> false.

-spec is_int(DescValue) -> boolean() when DescValue :: t().
is_int(#argo_desc_value{inner = {int, _}}) -> true;
is_int(#argo_desc_value{}) -> false.

-spec is_float(DescValue) -> boolean() when DescValue :: t().
is_float(#argo_desc_value{inner = {float, _}}) -> true;
is_float(#argo_desc_value{}) -> false.
