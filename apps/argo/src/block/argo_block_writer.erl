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
-module(argo_block_writer).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include_lib("argo/include/argo_block.hrl").
-include_lib("argo/include/argo_common.hrl").

%% API
-export([
    new/1,
    to_writer/1,
    write_bytes/2,
    write_float64/2,
    write_string/3,
    write_varint/2
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_block_writer{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Block) -> BlockWriter when Block :: binary(), BlockWriter :: t().
new(Block) when is_binary(Block) ->
    #argo_block_writer{block = Block}.

-spec to_writer(BlockWriter) -> Writer when BlockWriter :: t(), Writer :: binary().
% to_writer(#argo_block_writer{block = <<>>}) ->
%     <<>>;
to_writer(#argo_block_writer{block = Block}) ->
    <<(argo_varint:write_zigzag_i64(byte_size(Block)))/bytes, Block/bytes>>.

%% @private
-spec length_sanity_check(Length) -> ok when Length :: integer().
length_sanity_check(Length) when ?is_u32(Length) ->
    ok;
length_sanity_check(Length) when is_integer(Length) ->
    error_with_info(badarg, [Length], #{1 => {invalid_length_too_large, Length}}).

-spec write_bytes(BlockWriter, Bytes) -> BlockWriter when BlockWriter :: t(), Bytes :: binary().
write_bytes(W0 = #argo_block_writer{block = Block0}, Bytes) when is_binary(Bytes) ->
    ok = length_sanity_check(byte_size(Bytes)),
    Block1 = <<Block0/bytes, Bytes/bytes>>,
    W1 = W0#argo_block_writer{block = Block1},
    W1.

-spec write_float64(BlockWriter, Float64) -> BlockWriter when BlockWriter :: t(), Float64 :: float().
write_float64(W0 = #argo_block_writer{block = Block0}, Float64) when is_float(Float64) ->
    Block1 = <<Block0/bytes, Float64:1/float-little-unit:64>>,
    W1 = W0#argo_block_writer{block = Block1},
    W1.

-spec write_string(BlockWriter, String, NullTerminatedStrings) -> BlockWriter when
    BlockWriter :: t(), String :: unicode:unicode_binary(), NullTerminatedStrings :: boolean().
write_string(W0 = #argo_block_writer{block = Block0}, String, NullTerminatedStrings) when
    is_binary(String) andalso is_boolean(NullTerminatedStrings)
->
    ok = length_sanity_check(byte_size(String)),
    Block1 =
        case NullTerminatedStrings of
            true ->
                <<Block0/bytes, String/bytes, 0:8>>;
            false ->
                <<Block0/bytes, String/bytes>>
        end,
    W1 = W0#argo_block_writer{block = Block1},
    W1.

-spec write_varint(BlockWriter, Varint) -> BlockWriter when BlockWriter :: t(), Varint :: argo_types:i64().
write_varint(W0 = #argo_block_writer{block = Block0}, Varint) when ?is_i64(Varint) ->
    Block1 = <<Block0/bytes, (argo_varint:write_zigzag_i64(Varint))/bytes>>,
    W1 = W0#argo_block_writer{block = Block1},
    W1.

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
format_error_description(_Key, {invalid_length_too_large, Actual}) ->
    io_lib:format("invalid encoded length, expected value to be <= u32::MAX, but was ~w", [Actual]);
format_error_description(_Key, Value) ->
    Value.
