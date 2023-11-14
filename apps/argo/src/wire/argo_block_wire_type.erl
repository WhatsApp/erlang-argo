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
-module(argo_block_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/3
]).

%% Types
-type t() :: #argo_block_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Of, Key, Dedupe) -> BlockWireType when
    Of :: argo_scalar_wire_type:t(), Key :: argo_types:name(), Dedupe :: boolean(), BlockWireType :: t().
new(Of = #argo_scalar_wire_type{}, Key, Dedupe) when is_binary(Key) andalso is_boolean(Dedupe) ->
    #argo_block_wire_type{'of' = Of, key = Key, dedupe = Dedupe}.
