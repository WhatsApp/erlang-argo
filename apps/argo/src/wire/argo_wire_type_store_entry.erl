%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_wire_type_store_entry).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2024-03-21", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/2
]).

%% Types
-type t() :: #argo_wire_type_store_entry{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Name, Type) -> WireTypeStoreEntry when
    Name :: argo_types:name(), Type :: argo_wire_type:t(), WireTypeStoreEntry :: t().
new(Name, Type = #argo_wire_type{}) when is_binary(Name) ->
    #argo_wire_type_store_entry{name = Name, type = Type}.
