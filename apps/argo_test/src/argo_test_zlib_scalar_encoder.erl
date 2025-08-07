%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_test_zlib_scalar_encoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-08-07", modified => "2025-08-07"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-behaviour(argo_erlang_term_scalar_encoder).

-include_lib("argo/include/argo_common.hrl").

-export([
    init/1,
    encode_desc_scalar/2,
    encode_scalar/3
]).

-record(state, {}).

-type options() :: #{}.
-type state() :: #state{}.

-spec init(Options) -> EncoderState when Options :: options(), EncoderState :: state().
init(_Options) ->
    EncoderState = #state{},
    EncoderState.

-spec encode_desc_scalar(EncoderState, DescScalar) -> {EncoderState, Result} when
    EncoderState :: state(),
    DescScalar :: argo_desc_value:inner_scalar(),
    Result :: argo_erlang_term_scalar_encoder:result(DescScalarTermValue, ErrorReason),
    DescScalarTermValue :: argo_term:term_value(),
    ErrorReason :: argo_erlang_term_scalar_encoder:error_reason().
encode_desc_scalar(EncoderState = #state{}, DescScalar) ->
    DescScalarTermValue =
        case DescScalar of
            null -> null;
            {boolean, V} -> V;
            {string, V} -> inflate(V);
            {bytes, V} -> inflate(V);
            {int, V} -> V;
            {float, V} -> V
        end,
    {EncoderState, {ok, DescScalarTermValue}}.

-spec encode_scalar(EncoderState, ScalarWireTypeHint, ScalarTermValue) -> {EncoderState, Result} when
    EncoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    ScalarTermValue :: argo_term:term_value(),
    Result :: argo_erlang_term_scalar_encoder:result(ScalarTermValue, ErrorReason),
    ErrorReason :: argo_erlang_term_scalar_encoder:error_reason().
encode_scalar(EncoderState = #state{}, ScalarWireTypeHint, ScalarTermValue1) ->
    IsCompressible =
        case ScalarWireTypeHint of
            bytes -> true;
            {fixed, _} -> true;
            string -> true;
            _ -> false
        end,
    case IsCompressible of
        false ->
            {EncoderState, {ok, ScalarTermValue1}};
        true ->
            ScalarTermValue2 = inflate(ScalarTermValue1),
            {EncoderState, {ok, ScalarTermValue2}}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec inflate(In) -> Out when In :: binary(), Out :: binary().
inflate(In) ->
    ZStream = zlib:open(),
    try
        ok = zlib:inflateInit(ZStream),
        Out = erlang:iolist_to_binary(zlib:inflate(ZStream, In)),
        _ = zlib:inflateEnd(ZStream),
        Out
    after
        zlib:close(ZStream)
    end.
