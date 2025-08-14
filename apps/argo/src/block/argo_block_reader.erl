%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_block_reader).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_block.hrl").

%% API
-export([
    new/1,
    length_bounds_check/2,
    read_bytes/2,
    read_float64/1,
    read_string/3,
    read_varint/1
]).

%% Errors API
-export([
    format_error/2
]).

-type t() :: #argo_block_reader{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Block) -> BlockWriter when Block :: binary(), BlockWriter :: t().
new(Block) when is_binary(Block) ->
    #argo_block_reader{block = Block}.

-spec length_bounds_check(BlockReader, Length) -> ok when BlockReader :: t(), Length :: integer().
length_bounds_check(R = #argo_block_reader{block = Block}, Length) when is_integer(Length) ->
    ok = length_sanity_check(Length),
    case byte_size(Block) of
        BlockLength when BlockLength < Length ->
            error_with_info(badarg, [R, Length], #{2 => not_enough_data});
        _ ->
            ok
    end.

%% @private
-spec length_sanity_check(Length) -> ok when Length :: integer().
length_sanity_check(Length) when ?is_u32(Length) ->
    ok;
length_sanity_check(Length) when is_integer(Length) andalso Length < 0 ->
    error_with_info(badarg, [Length], #{1 => {invalid_length_negative, Length}});
length_sanity_check(Length) when is_integer(Length) ->
    error_with_info(badarg, [Length], #{1 => {invalid_length_too_large, Length}}).

-spec read_bytes(BlockReader, Length) -> {BlockReader, Bytes} when
    BlockReader :: t(), Length :: argo_types:length(), Bytes :: binary().
read_bytes(R0 = #argo_block_reader{block = Block0}, Length) when ?is_usize(Length) ->
    ok = length_bounds_check(R0, Length),
    <<Bytes:Length/bytes, Block1/bytes>> = Block0,
    R1 = R0#argo_block_reader{block = Block1},
    {R1, Bytes}.

-spec read_float64(BlockReader) -> {BlockReader, Float64} when BlockReader :: t(), Float64 :: float().
read_float64(R0 = #argo_block_reader{block = Block0}) ->
    ok = length_bounds_check(R0, 8),
    <<Float64:1/float-little-unit:64, Block1/bytes>> = Block0,
    R1 = R0#argo_block_reader{block = Block1},
    {R1, Float64}.

-spec read_string(BlockReader, Length, NullTerminatedStrings) -> {BlockReader, String} when
    BlockReader :: t(),
    Length :: argo_types:length(),
    NullTerminatedStrings :: boolean(),
    String :: unicode:unicode_binary().
read_string(R0 = #argo_block_reader{}, Length, NullTerminatedStrings) when
    ?is_usize(Length) andalso is_boolean(NullTerminatedStrings)
->
    {R1, Bytes} = read_bytes(R0, Length),
    String =
        case unicode:characters_to_binary(Bytes, utf8, utf8) of
            Bytes ->
                Bytes;
            {incomplete, _Encoded, _Rest} ->
                error_with_info(badarg, [R0, Length, NullTerminatedStrings], #{general => from_utf8_error});
            {error, _Encoded, _Rest} ->
                error_with_info(badarg, [R0, Length, NullTerminatedStrings], #{general => from_utf8_error})
        end,
    R2 =
        case NullTerminatedStrings of
            true ->
                ok = length_bounds_check(R1, 1),
                case R1 of
                    #argo_block_reader{block = <<0:8, Block1/bytes>>} ->
                        R1#argo_block_reader{block = Block1};
                    #argo_block_reader{block = <<Actual:8, _/bytes>>} ->
                        error_with_info(badarg, [R0, Length, NullTerminatedStrings], #{
                            general => {expected_null_terminated_string, Actual}
                        })
                end;
            false ->
                R1
        end,
    {R2, String}.

-spec read_varint(BlockReader) -> {BlockReader, Varint} when BlockReader :: t(), Varint :: argo_types:i64().
read_varint(R0 = #argo_block_reader{block = Block0}) ->
    ok = length_bounds_check(R0, 1),
    {Block1, Varint} = argo_varint:read_zigzag_i64(Block0),
    R1 = R0#argo_block_reader{block = Block1},
    {R1, Varint}.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, {expected_null_terminated_string, Actual}) ->
    io_lib:format("expected NULL byte after STRING when using NullTerminatedStrings mode, but was ~w", [Actual]);
format_error_description(_Key, from_utf8_error) ->
    "failed to decode string: invalid UTF-8 string";
format_error_description(_Key, {invalid_length_negative, Actual}) ->
    io_lib:format("invalid decoded length, expected non-negative value, but was ~w", [Actual]);
format_error_description(_Key, {invalid_length_too_large, Actual}) ->
    io_lib:format("invalid decoded length, expected value to be <= u32::MAX, but was ~w", [Actual]);
format_error_description(_Key, not_enough_data) ->
    "not enough data";
format_error_description(_Key, Value) ->
    Value.
