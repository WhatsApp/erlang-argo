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
-module(argo_varint).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").

%% API
-export([
    peek_zigzag_i64/1,
    peek_zigzag_u64/1,
    read_zigzag_i64/1,
    read_zigzag_u64/1,
    write_zigzag_i64/1,
    write_zigzag_u64/1
]).

%% Errors API
-export([
    format_error/2
]).

%% Macros
-define(is_nonempty_bytes(X), (is_binary((X)) andalso byte_size((X)) > 0)).
-define(MASK64(X), ((X) band 16#FFFFFFFFFFFFFFFF)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec peek_zigzag_i64(Bytes) -> {ok, argo_types:i64()} | error when Bytes :: binary().
peek_zigzag_i64(Bytes) when is_binary(Bytes) ->
    case peek_zigzag_u64(Bytes) of
        {ok, N} ->
            {ok, zigzag_decode(N)};
        error ->
            error
    end.

-spec peek_zigzag_u64(Bytes) -> {ok, argo_types:u64()} | error when Bytes :: binary().
peek_zigzag_u64(Bytes) when is_binary(Bytes) ->
    try read(Bytes, 0, 0) of
        {_Rest, N} ->
            {ok, N}
    catch
        throw:not_enough_data ->
            error;
        throw:too_large ->
            error
    end.

-spec read_zigzag_i64(Bytes) -> {Rest, argo_types:i64()} when Bytes :: binary(), Rest :: binary().
read_zigzag_i64(Bytes) when ?is_nonempty_bytes(Bytes) ->
    {Rest, N} = read_zigzag_u64(Bytes),
    {Rest, zigzag_decode(N)}.

-spec read_zigzag_u64(Bytes) -> {Rest, argo_types:u64()} when Bytes :: binary(), Rest :: binary().
read_zigzag_u64(Bytes) when ?is_nonempty_bytes(Bytes) ->
    try read(Bytes, 0, 0) of
        {Rest, N} ->
            {Rest, N}
    catch
        throw:not_enough_data ->
            error_with_info(badarg, [Bytes], #{1 => not_enough_data});
        throw:too_large ->
            error_with_info(badarg, [Bytes], #{1 => invalid_encoded_varint, general => too_large})
    end.

-spec write_zigzag_i64(argo_types:i64()) -> Bytes when Bytes :: binary().
write_zigzag_i64(N) when ?is_i64(N) ->
    write(zigzag_encode(N)).

-spec write_zigzag_u64(argo_types:u64()) -> Bytes when Bytes :: binary().
write_zigzag_u64(N) when ?is_u64(N) ->
    write(N).

%% @private
-spec read(Bytes, Shift, Acc) -> {Rest, Varint} when
    Bytes :: binary(),
    Shift :: non_neg_integer(),
    Acc :: argo_types:u64(),
    Rest :: binary(),
    Varint :: argo_types:u64().
read(_, Shift, _) when Shift > (9 * 7) ->
    throw(too_large);
read(<<>>, _, _) ->
    throw(not_enough_data);
read(<<1:1, MsbDropped:7, Rest/bytes>>, Shift, Acc) ->
    read(Rest, Shift + 7, (MsbDropped bsl Shift) + Acc);
read(<<0:1, MsbDropped:7, Rest/bytes>>, Shift, Acc) ->
    {Rest, (MsbDropped bsl Shift) + Acc}.

%% @private
-spec write(Varint) -> Bytes when Varint :: argo_types:u64(), Bytes :: binary().
write(N) when N > 16#7F ->
    <<((N band 16#7F) bor 16#80):8, (write(N bsr 7))/bytes>>;
write(N) when N >= 0 andalso N =< 16#7F ->
    <<N:8>>.

%% @private
-spec zigzag_decode(argo_types:u64()) -> argo_types:i64().
zigzag_decode(From) when ?is_u64(From) ->
    (From bsr 1) bxor (-(From band 1)).

%% @private
-spec zigzag_encode(argo_types:i64()) -> argo_types:u64().
zigzag_encode(From) when ?is_i64(From) ->
    ?MASK64((From bsl 1) bxor (From bsr 63)).

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
format_error_description(_Key, invalid_encoded_varint) ->
    "invalid encoded varint";
format_error_description(_Key, not_enough_data) ->
    "not enough data";
format_error_description(_Key, too_large) ->
    "varint must be encoded as 10-bytes or less";
format_error_description(_Key, Value) ->
    Value.
