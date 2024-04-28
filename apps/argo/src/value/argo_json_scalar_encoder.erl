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
%%% Created :  19 Jan 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_json_scalar_encoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Behaviour
-callback init(Options) -> JsonScalarEncoder when Options :: options(), JsonScalarEncoder :: t().

-callback encode_block_scalar(JsonScalarEncoder, BlockKey, BlockScalar) -> {JsonScalarEncoder, JsonValue} when
    JsonScalarEncoder :: t(),
    BlockKey :: argo_types:name(),
    BlockScalar :: argo_scalar_value:inner(),
    JsonValue :: argo_json:json_value().

-callback encode_desc_scalar(JsonScalarEncoder, DescScalar) -> {JsonScalarEncoder, JsonValue} when
    JsonScalarEncoder :: t(), DescScalar :: argo_desc_value:inner_scalar(), JsonValue :: argo_json:json_value().

-callback encode_scalar(JsonScalarEncoder, Scalar) -> {JsonScalarEncoder, JsonValue} when
    JsonScalarEncoder :: t(), Scalar :: argo_scalar_value:inner(), JsonValue :: argo_json:json_value().

%% Types
-type options() :: dynamic().
-type t() :: dynamic().

-export_type([
    options/0,
    t/0
]).
