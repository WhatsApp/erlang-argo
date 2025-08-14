%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_test_zlib_scalar_decoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-08-07", modified => "2025-08-07"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_erlang_term_scalar_decoder).

-include_lib("argo/include/argo_common.hrl").

-export([
    init/1,
    decode_desc_scalar/3,
    decode_scalar/3
]).

-record(state, {
    level :: zlib:zlevel()
}).

-type options() :: #{
    level => zlib:zlevel()
}.
-type state() :: #state{}.

-spec init(Options) -> DecoderState when Options :: options(), DecoderState :: state().
init(Options) ->
    Level = maps:get(level, Options, default),
    DecoderState = #state{level = Level},
    DecoderState.

-spec decode_desc_scalar(DecoderState, DescValueScalarHint, DescScalarTermValue) ->
    {DecoderState, Result}
when
    DecoderState :: state(),
    DescValueScalarHint :: argo_term:desc_value_scalar_hint(),
    DescScalarTermValue :: argo_term:term_value(),
    Result :: argo_erlang_term_scalar_decoder:result({DescScalarTermValue, DescScalar}, ErrorReason),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: argo_erlang_term_scalar_decoder:error_reason().
decode_desc_scalar(DecoderState = #state{level = Level}, DescValueScalarHint, DescScalarTermValue) ->
    DescScalar =
        case DescValueScalarHint of
            null when DescScalarTermValue =:= null -> null;
            boolean when is_boolean(DescScalarTermValue) -> {boolean, DescScalarTermValue};
            string when is_binary(DescScalarTermValue) -> {string, deflate(Level, DescScalarTermValue)};
            bytes when is_binary(DescScalarTermValue) -> {bytes, deflate(Level, DescScalarTermValue)};
            int when is_integer(DescScalarTermValue) -> {int, DescScalarTermValue};
            float when is_float(DescScalarTermValue) -> {float, DescScalarTermValue}
        end,
    {DecoderState, {ok, {DescScalarTermValue, DescScalar}}}.

-spec decode_scalar(DecoderState, ScalarWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: argo_erlang_term_scalar_decoder:result(Scalar, ErrorReason),
    Scalar :: boolean() | binary() | DescTermValue | float() | unicode:unicode_binary() | argo_types:i64(),
    DescTermValue :: argo_term:term_value(),
    ErrorReason :: argo_erlang_term_scalar_decoder:error_reason().
decode_scalar(DecoderState = #state{level = Level}, ScalarWireTypeHint, TermValue) ->
    IsCompressible =
        case ScalarWireTypeHint of
            bytes -> true;
            {fixed, _} -> true;
            string -> true;
            _ -> false
        end,
    case IsCompressible of
        false ->
            Scalar = TermValue,
            {DecoderState, {ok, Scalar}};
        true ->
            Scalar = deflate(Level, TermValue),
            {DecoderState, {ok, Scalar}}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec deflate(Level, In) -> Out when Level :: zlib:zlevel(), In :: binary(), Out :: binary().
deflate(Level, In) ->
    ZStream = zlib:open(),
    try
        ok = zlib:deflateInit(ZStream, Level),
        Out = erlang:iolist_to_binary(zlib:deflate(ZStream, In, finish)),
        _ = zlib:deflateEnd(ZStream),
        Out
    after
        zlib:close(ZStream)
    end.
