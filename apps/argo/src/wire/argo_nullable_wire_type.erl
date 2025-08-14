%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_nullable_wire_type).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    is_labeled/1
]).

%% Types
-type t() :: #argo_nullable_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Of) -> NullableWireType when Of :: argo_wire_type:t(), NullableWireType :: t().
new(#argo_wire_type{inner = #argo_nullable_wire_type{'of' = Of}}) ->
    new(Of);
new(Of = #argo_wire_type{}) ->
    #argo_nullable_wire_type{'of' = Of}.

-compile({inline, [is_labeled/1]}).
-spec is_labeled(NullableWireType) -> boolean() when NullableWireType :: t().
is_labeled(#argo_nullable_wire_type{'of' = WireType}) ->
    argo_wire_type:is_labeled(WireType).
