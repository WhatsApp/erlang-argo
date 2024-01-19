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
-module(argo_types).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    dynamic_cast/1,
    format_with_lines/1,
    unicode_binary/1,
    unicode_length/1,
    unicode_string/1
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type backreference() :: usize().
-type i8() :: -16#80..16#7F.
-type i16() :: -16#8000..16#7FFF.
-type i32() :: -16#80000000..16#7FFFFFFF.
-type i64() :: -16#8000000000000000..16#7FFFFFFFFFFFFFFF.
-type isize() :: i64().
-type label() :: i64().
-type length() :: usize().
-type name() :: unicode:unicode_binary().
-type u8() :: 16#00..16#FF.
-type u16() :: 16#0000..16#FFFF.
-type u32() :: 16#00000000..16#FFFFFFFF.
-type u64() :: 16#0000000000000000..16#FFFFFFFFFFFFFFFF.
-type usize() :: u64().
-type varint() :: i64().

-export_type([
    backreference/0,
    i8/0,
    i16/0,
    i32/0,
    i64/0,
    isize/0,
    label/0,
    length/0,
    name/0,
    u8/0,
    u16/0,
    u32/0,
    u64/0,
    usize/0,
    varint/0
]).

%% Macros
-define(is_unicode_codepoint1(C), ((C) >= 16#00 andalso (C) =< 16#7F)).
-define(is_unicode_codepoint2(C), ((C) >= 16#80 andalso (C) =< 16#7FF)).
-define(is_unicode_codepoint3(C),
    (((C) >= 16#800 andalso (C) =< 16#D7FF) orelse ((C) >= 16#E000 andalso (C) =< 16#FFFD))
).
-define(is_unicode_codepoint4(C), ((C) >= 16#10000 andalso (C) =< 16#10FFFF)).
-define(is_unicode_codepoint(C),
    (?is_unicode_codepoint1(C) orelse ?is_unicode_codepoint2(C) orelse ?is_unicode_codepoint3(C) orelse
        ?is_unicode_codepoint4(C))
).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-compile({inline, [dynamic_cast/1]}).
-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.

-spec format_with_lines(unicode:chardata()) -> unicode:unicode_binary().
format_with_lines(Chardata) when is_binary(Chardata) orelse is_list(Chardata) ->
    Binary = unicode_binary(Chardata),
    Lines = binary:split(Binary, <<$\n>>, [global]),
    Width = byte_size(erlang:iolist_to_binary(io_lib:format("~w", [length(Lines)]))),
    unicode_binary(
        lists:map(
            fun({Row, Line}) ->
                io_lib:format("~*w: ~ts~n", [Width, Row, Line])
            end,
            lists:enumerate(Lines)
        )
    ).

-spec unicode_binary(unicode:chardata()) -> unicode:unicode_binary().
unicode_binary(Chardata) when is_binary(Chardata) orelse is_list(Chardata) ->
    case unicode:characters_to_binary(Chardata, utf8) of
        UnicodeBinary when is_binary(UnicodeBinary) ->
            UnicodeBinary;
        {error, Encoded, Rest} ->
            error_with_info(badarg, [Chardata], #{
                1 => {unicode_error, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            });
        {incomplete, Encoded, Rest} ->
            error_with_info(badarg, [Chardata], #{
                1 => {unicode_incomplete, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            })
    end.

-spec unicode_length(unicode:unicode_binary() | string()) -> non_neg_integer().
unicode_length(UnicodeBinary) when is_binary(UnicodeBinary) ->
    unicode_binary_length(UnicodeBinary, <<>>, 0);
unicode_length(UnicodeString) when is_list(UnicodeString) ->
    unicode_string_length(UnicodeString, <<>>, 0).

%% @private
-spec unicode_binary_length(unicode:unicode_binary(), unicode:unicode_binary(), non_neg_integer()) -> non_neg_integer().
unicode_binary_length(<<C/utf8, Rest/bytes>>, Encoded, Length) ->
    unicode_binary_length(Rest, <<C/utf8, Encoded/bytes>>, Length + 1);
unicode_binary_length(<<>>, _Encoded, Length) ->
    Length;
unicode_binary_length(<<Rest/bytes>>, Encoded, Length) ->
    error_with_info(badarg, [Rest, Encoded, Length], #{
        1 => {unicode_length_error, #{encoded => Encoded, length => Length, rest => Rest}}
    }).

%% @private
-spec unicode_string_length(string(), unicode:unicode_binary(), non_neg_integer()) -> non_neg_integer().
unicode_string_length([C | Rest], Encoded, Length) when ?is_unicode_codepoint(C) ->
    unicode_string_length(Rest, <<C/utf8, Encoded/bytes>>, Length + 1);
unicode_string_length([], _Encoded, Length) ->
    Length;
unicode_string_length(Rest, Encoded, Length) ->
    error_with_info(badarg, [Rest, Encoded, Length], #{
        1 => {unicode_length_error, #{encoded => Encoded, length => Length, rest => Rest}}
    }).

-spec unicode_string(unicode:chardata()) -> string().
unicode_string(Chardata) when is_binary(Chardata) orelse is_list(Chardata) ->
    case unicode:characters_to_list(Chardata, utf8) of
        UnicodeString when is_list(UnicodeString) ->
            UnicodeString;
        {error, Encoded, Rest} ->
            error_with_info(badarg, [Chardata], #{
                1 => {unicode_error, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            });
        {incomplete, Encoded, Rest} ->
            error_with_info(badarg, [Chardata], #{
                1 => {unicode_incomplete, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            })
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
format_error_description(_Key, {unicode_error, #{encoded := _Encoded, length := Length, rest := Rest}}) ->
    io_lib:format("invalid Unicode chardata at position ~w: ~0tP", [Length, Rest, 5]);
format_error_description(_Key, {unicode_incomplete, #{encoded := _Encoded, length := Length, rest := Rest}}) ->
    io_lib:format("incomplete Unicode chardata at position ~w: ~0tP", [Length, Rest, 5]);
format_error_description(_Key, {unicode_length_error, #{encoded := _Encoded, length := Length, rest := Rest}}) ->
    io_lib:format("invalid length calculation of Unicode chardata at position ~w: ~0tP", [Length, Rest, 5]);
format_error_description(_Key, Value) ->
    Value.
