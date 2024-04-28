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
-module(argo_json_scalar_decoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Behaviour
-callback init(Options) -> JsonScalarDecoder when Options :: options(), JsonScalarDecoder :: t().

-callback decode_block_scalar(JsonScalarDecoder, BlockKey, BlockScalarHint, JsonValue) ->
    {ok, JsonScalarDecoder, BlockScalar} | {error, ErrorReason}
when
    JsonScalarDecoder :: t(),
    BlockKey :: argo_types:name(),
    BlockScalarHint :: scalar_hint(),
    JsonValue :: argo_json:json_value(),
    BlockScalar :: argo_scalar_value:inner(),
    ErrorReason :: error_reason().

-callback decode_desc_scalar(JsonScalarDecoder, DescHint, JsonValue) ->
    {ok, JsonScalarDecoder, DescScalar} | {error, ErrorReason}
when
    JsonScalarDecoder :: t(),
    DescHint :: desc_hint(),
    JsonValue :: argo_json:json_value(),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: error_reason().

-callback decode_scalar(JsonScalarDecoder, ScalarHint, JsonValue) ->
    {ok, JsonScalarDecoder, Scalar} | {error, ErrorReason}
when
    JsonScalarDecoder :: t(),
    ScalarHint :: scalar_hint(),
    JsonValue :: argo_json:json_value(),
    Scalar :: argo_scalar_value:inner(),
    ErrorReason :: error_reason().

%% Types
-type desc_hint() :: null | boolean | string | bytes | int | float.
-type error_reason() :: invalid | type_mismatch | dynamic().
-type options() :: dynamic().
-type scalar_hint() :: boolean | bytes | desc | {fixed, argo_types:length()} | float64 | string | varint.
-type t() :: dynamic().

-export_type([
    desc_hint/0,
    error_reason/0,
    options/0,
    scalar_hint/0,
    t/0
]).
