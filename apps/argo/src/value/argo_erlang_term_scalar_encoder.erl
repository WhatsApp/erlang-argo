%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_erlang_term_scalar_encoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-07-18", modified => "2025-08-07"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-include_lib("argo/include/argo_common.hrl").

%% API
-export([
    init/2,
    encode_block/4,
    encode_desc_scalar/3,
    encode_scalar/4
]).

%% Types
-type error_reason() :: argo_erlang_term_value_encoder:error_reason().
-type options() :: dynamic().
-type result(Ok, Error) :: argo_erlang_term_value_encoder:result(Ok, Error).
-type state() :: dynamic().

-export_type([
    error_reason/0,
    options/0,
    result/2,
    state/0
]).

%% Behaviour
-callback init(Options) -> EncoderState when Options :: options(), EncoderState :: state().

-callback encode_block(EncoderState, BlockWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_desc_scalar(EncoderState, DescScalar) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescScalar :: argo_desc_value:inner_scalar(),
    Result :: result(DescScalarTermValue, ErrorReason),
    DescScalarTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback encode_scalar(EncoderState, ScalarWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(ScalarTermValue, ErrorReason),
    ErrorReason :: error_reason().

-optional_callbacks([
    init/1,
    encode_block/3,
    encode_desc_scalar/2,
    encode_scalar/3
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec init(EncoderModule, EncoderOptions) -> EncoderState when
    EncoderModule :: module(), EncoderOptions :: options(), EncoderState :: state().
init(EncoderModule, EncoderOptions) ->
    case EncoderModule =/= undefined andalso erlang:function_exported(EncoderModule, init, 1) of
        false ->
            EncoderOptions;
        true ->
            EncoderModule:init(EncoderOptions)
    end.

-spec encode_block(EncoderModule, EncoderState, BlockWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderModule :: module(),
    EncoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_block(EncoderModule, EncoderState, BlockWireTypeHint, ScalarTermValue) ->
    case EncoderModule =/= undefined andalso erlang:function_exported(EncoderModule, encode_block, 3) of
        false ->
            BlockTermValue = ScalarTermValue,
            {EncoderState, {ok, BlockTermValue}};
        true ->
            EncoderModule:encode_block(EncoderState, BlockWireTypeHint, ScalarTermValue)
    end.

-spec encode_desc_scalar(EncoderModule, EncoderState, DescScalar) ->
    {EncoderState, Result}
when
    EncoderModule :: module(),
    EncoderState :: state(),
    DescScalar :: argo_desc_value:inner_scalar(),
    Result :: result(DescScalarTermValue, ErrorReason),
    DescScalarTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
encode_desc_scalar(EncoderModule, EncoderState, DescScalar) ->
    case EncoderModule =/= undefined andalso erlang:function_exported(EncoderModule, encode_desc_scalar, 2) of
        false ->
            DescScalarTermValue =
                case DescScalar of
                    null -> null;
                    {boolean, V} -> V;
                    {string, V} -> V;
                    {bytes, V} -> V;
                    {int, V} -> V;
                    {float, V} -> V
                end,
            {EncoderState, {ok, DescScalarTermValue}};
        true ->
            EncoderModule:encode_desc_scalar(EncoderState, DescScalar)
    end.

-spec encode_scalar(EncoderModule, EncoderState, ScalarWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderModule :: module(),
    EncoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: result(ScalarTermValue, ErrorReason),
    ErrorReason :: error_reason().
encode_scalar(EncoderModule, EncoderState, ScalarWireTypeHint, ScalarTermValue) ->
    case EncoderModule =/= undefined andalso erlang:function_exported(EncoderModule, encode_scalar, 3) of
        false ->
            {EncoderState, {ok, ScalarTermValue}};
        true ->
            EncoderModule:encode_scalar(EncoderState, ScalarWireTypeHint, ScalarTermValue)
    end.
