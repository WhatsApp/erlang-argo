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
%%% Created :  13 Feb 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_extensions_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_value.hrl").

%% New API
-export([
    new/0,
    new/1
]).

%% Instance API
-export([
    insert/3
]).

%% Types
-type inner() :: argo_desc_value:desc_object().
-type t() :: #argo_extensions_value{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec new() -> ExtensionsValue when ExtensionsValue :: t().
new() ->
    #argo_extensions_value{inner = argo_index_map:new()}.

-spec new(Extensions) -> ExtensionsValue when Extensions :: inner(), ExtensionsValue :: t().
new(Extensions = #argo_index_map{}) ->
    #argo_extensions_value{inner = Extensions}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec insert(ExtensionsValue, Key, DescValue) -> ExtensionsValue when
    ExtensionsValue :: t(), Key :: unicode:unicode_binary(), DescValue :: argo_desc_value:t().
insert(ExtensionsValue1 = #argo_extensions_value{inner = Extensions1}, Key, DescValue = #argo_desc_value{}) when
    is_binary(Key)
->
    Extensions2 = argo_index_map:put(Key, DescValue, Extensions1),
    ExtensionsValue2 = ExtensionsValue1#argo_extensions_value{inner = Extensions2},
    ExtensionsValue2.
