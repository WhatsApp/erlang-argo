%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_erlang_term_scalar_decoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-07-18", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").

%% API
-export([
    init/2,
    decode_block/4,
    decode_block_stop/3,
    decode_desc/3,
    decode_desc_list_stop/4,
    decode_desc_object_stop/4,
    decode_desc_scalar/4,
    decode_desc_scalar_stop/4,
    decode_scalar/4,
    decode_scalar_stop/3
]).

%% Types
-type error_reason() :: argo_erlang_term_value_decoder:error_reason().
-type options() :: dynamic().
-type result(Ok, Error) :: argo_erlang_term_value_decoder:result(Ok, Error).
-type state() :: dynamic().

-export_type([
    error_reason/0,
    options/0,
    result/2,
    state/0
]).

%% Behaviour
-callback init(Options) -> DecoderState when Options :: options(), DecoderState :: state().

-callback decode_block(DecoderState, BlockWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_block_stop(DecoderState, BlockValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    BlockValue :: argo_block_value:t(),
    Result :: result(DecoderState, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_desc(DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result({DescTermValue, DescValueHint}, ErrorReason),
    DescTermValue :: argo_term:term_value(),
    DescValueHint :: argo_term:desc_value_hint(),
    ErrorReason :: error_reason().

-callback decode_desc_list_stop(DecoderState, DescListTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescListTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_desc_object_stop(DecoderState, DescObjectTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescObjectTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_desc_scalar(DecoderState, DescValueScalarHint, DescScalarTermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescValueScalarHint :: argo_term:desc_value_scalar_hint(),
    DescScalarTermValue :: argo_term:term_value(),
    Result :: result({DescScalarTermValue, DescScalar}, ErrorReason),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: error_reason().

-callback decode_desc_scalar_stop(DecoderState, DescScalarTermValue, DescValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    DescScalarTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().

-callback decode_scalar(DecoderState, ScalarWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(Scalar, ErrorReason),
    Scalar :: boolean() | binary() | DescTermValue | float() | unicode:unicode_binary() | argo_types:i64(),
    DescTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().

-callback decode_scalar_stop(DecoderState, ScalarValue) -> {DecoderState, Result} when
    DecoderState :: state(),
    ScalarValue :: argo_scalar_value:t(),
    Result :: result(ScalarValue, ErrorReason),
    ErrorReason :: error_reason().

-optional_callbacks([
    init/1,
    decode_block/3,
    decode_block_stop/2,
    decode_desc/2,
    decode_desc_list_stop/3,
    decode_desc_object_stop/3,
    decode_desc_scalar/3,
    decode_desc_scalar_stop/3,
    decode_scalar/3,
    decode_scalar_stop/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec init(DecoderModule, DecoderOptions) -> DecoderState when
    DecoderModule :: module(), DecoderOptions :: options(), DecoderState :: state().
init(DecoderModule, DecoderOptions) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, init, 1) of
        false ->
            DecoderOptions;
        true ->
            DecoderModule:init(DecoderOptions)
    end.

-spec decode_block(DecoderModule, DecoderState, BlockWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderModule :: module(),
    DecoderState :: state(),
    BlockWireTypeHint :: argo_term:block_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(BlockTermValue, ErrorReason),
    BlockTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_block(DecoderModule, DecoderState, BlockWireTypeHint, TermValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_block, 3) of
        false ->
            BlockTermValue = TermValue,
            {DecoderState, {ok, BlockTermValue}};
        true ->
            DecoderModule:decode_block(DecoderState, BlockWireTypeHint, TermValue)
    end.

-spec decode_block_stop(DecoderModule, DecoderState, BlockValue) -> {DecoderState, Result} when
    DecoderModule :: module(),
    DecoderState :: state(),
    BlockValue :: argo_block_value:t(),
    Result :: result(DecoderState, ErrorReason),
    ErrorReason :: error_reason().
decode_block_stop(DecoderModule, DecoderState, BlockValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_block_stop, 2) of
        false ->
            {DecoderState, {ok, BlockValue}};
        true ->
            DecoderModule:decode_block_stop(DecoderState, BlockValue)
    end.

-spec decode_desc(DecoderModule, DecoderState, TermValue) -> {DecoderState, Result} when
    DecoderModule :: module(),
    DecoderState :: state(),
    TermValue :: argo_term:term_value(),
    Result :: result({DescTermValue, DescValueHint}, ErrorReason),
    DescTermValue :: argo_term:term_value(),
    DescValueHint :: argo_term:desc_value_hint(),
    ErrorReason :: error_reason().
decode_desc(DecoderModule, DecoderState, TermValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_desc, 2) of
        false ->
            DescValueHint =
                case TermValue of
                    null ->
                        null;
                    _ when is_boolean(TermValue) -> boolean;
                    _ when ?is_i64(TermValue) -> int;
                    _ when is_float(TermValue) -> float;
                    _ when is_binary(TermValue) ->
                        try argo_types:unicode_length(TermValue) of
                            Length when is_integer(Length) ->
                                string
                        catch
                            error:badarg ->
                                bytes
                        end;
                    _ when is_list(TermValue) -> list;
                    _ when is_map(TermValue) -> object
                end,
            DescTermValue = TermValue,
            {DecoderState, {ok, {DescTermValue, DescValueHint}}};
        true ->
            DecoderModule:decode_desc(DecoderState, TermValue)
    end.

-spec decode_desc_list_stop(DecoderModule, DecoderState, DescListTermValue, DescValue) ->
    {DecoderState, Result}
when
    DecoderModule :: module(),
    DecoderState :: state(),
    DescListTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().
decode_desc_list_stop(DecoderModule, DecoderState, DescListTermValue, DescValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_desc_list_stop, 3) of
        false ->
            {DecoderState, {ok, DescValue}};
        true ->
            DecoderModule:decode_desc_list_stop(DecoderState, DescListTermValue, DescValue)
    end.

-spec decode_desc_object_stop(DecoderModule, DecoderState, DescObjectTermValue, DescValue) ->
    {DecoderState, Result}
when
    DecoderModule :: module(),
    DecoderState :: state(),
    DescObjectTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().
decode_desc_object_stop(DecoderModule, DecoderState, DescObjectTermValue, DescValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_desc_object_stop, 3) of
        false ->
            {DecoderState, {ok, DescValue}};
        true ->
            DecoderModule:decode_desc_object_stop(DecoderState, DescObjectTermValue, DescValue)
    end.

-spec decode_desc_scalar(DecoderModule, DecoderState, DescValueScalarHint, DescScalarTermValue) ->
    {DecoderState, Result}
when
    DecoderModule :: module(),
    DecoderState :: state(),
    DescValueScalarHint :: argo_term:desc_value_scalar_hint(),
    DescScalarTermValue :: argo_term:term_value(),
    Result :: result({DescScalarTermValue, DescScalar}, ErrorReason),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: error_reason().
decode_desc_scalar(DecoderModule, DecoderState, DescValueScalarHint, DescScalarTermValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_desc_scalar, 3) of
        false ->
            DescScalar =
                case DescValueScalarHint of
                    null when DescScalarTermValue =:= null -> null;
                    boolean when is_boolean(DescScalarTermValue) -> {boolean, DescScalarTermValue};
                    string when is_binary(DescScalarTermValue) -> {string, DescScalarTermValue};
                    bytes when is_binary(DescScalarTermValue) -> {bytes, DescScalarTermValue};
                    int when ?is_i64(DescScalarTermValue) -> {int, DescScalarTermValue};
                    float when is_float(DescScalarTermValue) -> {float, DescScalarTermValue}
                end,
            {DecoderState, {ok, {DescScalarTermValue, DescScalar}}};
        true ->
            DecoderModule:decode_desc_scalar(DecoderState, DescValueScalarHint, DescScalarTermValue)
    end.

-spec decode_desc_scalar_stop(DecoderModule, DecoderState, DescScalarTermValue, DescValue) ->
    {DecoderState, Result}
when
    DecoderModule :: module(),
    DecoderState :: state(),
    DescScalarTermValue :: argo_term:term_value(),
    DescValue :: argo_desc_value:t(),
    Result :: result(DescValue, ErrorReason),
    ErrorReason :: error_reason().
decode_desc_scalar_stop(DecoderModule, DecoderState, DescScalarTermValue, DescValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_desc_scalar_stop, 3) of
        false ->
            {DecoderState, {ok, DescValue}};
        true ->
            DecoderModule:decode_desc_scalar_stop(DecoderState, DescScalarTermValue, DescValue)
    end.

-spec decode_scalar(DecoderModule, DecoderState, ScalarWireTypeHint, TermValue) -> {DecoderState, Result} when
    DecoderModule :: module(),
    DecoderState :: state(),
    ScalarWireTypeHint :: argo_term:scalar_wire_type_hint(),
    TermValue :: argo_term:term_value(),
    Result :: result(Scalar, ErrorReason),
    Scalar :: boolean() | binary() | DescTermValue | float() | unicode:unicode_binary() | argo_types:i64(),
    DescTermValue :: argo_term:term_value(),
    ErrorReason :: error_reason().
decode_scalar(DecoderModule, DecoderState, ScalarWireTypeHint, TermValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_scalar, 3) of
        false ->
            ScalarTermValue = TermValue,
            {DecoderState, {ok, ScalarTermValue}};
        true ->
            DecoderModule:decode_scalar(DecoderState, ScalarWireTypeHint, TermValue)
    end.

-spec decode_scalar_stop(DecoderModule, DecoderState, ScalarValue) -> {DecoderState, Result} when
    DecoderModule :: module(),
    DecoderState :: state(),
    ScalarValue :: argo_scalar_value:t(),
    Result :: result(ScalarValue, ErrorReason),
    ErrorReason :: error_reason().
decode_scalar_stop(DecoderModule, DecoderState, ScalarValue) ->
    case DecoderModule =/= undefined andalso erlang:function_exported(DecoderModule, decode_scalar_stop, 2) of
        false ->
            {DecoderState, {ok, ScalarValue}};
        true ->
            DecoderModule:decode_scalar_stop(DecoderState, ScalarValue)
    end.
