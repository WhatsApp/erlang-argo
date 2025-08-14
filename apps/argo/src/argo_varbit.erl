%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_varbit).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2024-05-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Public API
-export([
    read_varbit/1,
    write_varbit/1
]).
%% NOTE: remove export once https://github.com/erlang/otp/pull/10048 is available
-export([
    write_varbit_chunks/1
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: bitstring().

-export_type([
    t/0
]).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec read_varbit(Reader) -> {Rest, Varbit} when Reader :: binary(), Rest :: binary(), Varbit :: t().
read_varbit(Reader) ->
    Limit = argo_limits:varbit_limit(),
    try read_varbit_chunks(Reader, <<>>, Limit) of
        {Rest, Varbit} ->
            {Rest, Varbit}
    catch
        throw:Cause ->
            error_with_info(badarg, [Reader], #{1 => Cause})
    end.

-spec write_varbit(undefined | t()) -> binary().
write_varbit(undefined) ->
    <<>>;
write_varbit(Varbit) when is_bitstring(Varbit) ->
    Limit = argo_limits:varbit_limit(),
    case bit_size(Varbit) > Limit of
        false ->
            write_varbit_chunks(Varbit);
        true ->
            error_with_info(badarg, [Varbit], #{1 => {varbit_too_large, Limit}})
    end.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-compile({inline, [error_with_info/3]}).
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
format_error_description(_Key, not_enough_data) ->
    "not enough data";
format_error_description(_Key, {varbit_too_large, Limit}) ->
    io_lib:format("varbit is too large, maximum bit-size is: ~0tp", [Limit]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec read_varbit_chunks(Reader, Varbit, Limit) -> {Reader, Varbit} when
    Reader :: binary(), Varbit :: bitstring(), Limit :: argo_limits:varbit_limit().
read_varbit_chunks(_Reader, Varbit, Limit) when bit_size(Varbit) =:= Limit ->
    throw({varbit_too_large, Limit});
read_varbit_chunks(<<Head:7/bits, 1:1, Rest/bits>>, Varbit, Limit) ->
    read_varbit_chunks(Rest, <<Varbit/bits, Head:7/bits>>, Limit);
read_varbit_chunks(<<Head:7/bits, 0:1, Rest/bits>>, Varbit, _Limit) ->
    {Rest, <<Varbit/bits, Head:7/bits>>};
read_varbit_chunks(<<>>, _Varbit, _Limit) ->
    throw(not_enough_data).

%% @private
-spec write_varbit_chunks(t()) -> binary().
write_varbit_chunks(<<Chunk:7/bits>>) ->
    <<Chunk:7/bits, 0:1>>;
write_varbit_chunks(<<Chunk:6/bits>>) ->
    <<Chunk:6/bits, 0:2>>;
write_varbit_chunks(<<Chunk:5/bits>>) ->
    <<Chunk:5/bits, 0:3>>;
write_varbit_chunks(<<Chunk:4/bits>>) ->
    <<Chunk:4/bits, 0:4>>;
write_varbit_chunks(<<Chunk:3/bits>>) ->
    <<Chunk:3/bits, 0:5>>;
write_varbit_chunks(<<Chunk:2/bits>>) ->
    <<Chunk:2/bits, 0:6>>;
write_varbit_chunks(<<Chunk:1/bits>>) ->
    <<Chunk:1/bits, 0:7>>;
write_varbit_chunks(<<>>) ->
    <<0:8>>;
write_varbit_chunks(<<Chunk:7/bits, Rest/bits>>) ->
    <<Chunk:7/bits, 1:1, (write_varbit_chunks(Rest))/bits>>.
