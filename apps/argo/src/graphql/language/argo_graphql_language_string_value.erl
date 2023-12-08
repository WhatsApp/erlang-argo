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
%%% Created :  16 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_graphql_language_string_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% StringValue API
-export([
    string_character_escape/1,
    string_character_escape/2,
    string_value_escape/1,
    string_value_escape/2,
    string_value_unescape/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type string_character_escape_options() :: #{legacy => boolean(), 'case' => lowercase | uppercase}.
-type t() :: unicode:unicode_binary().

-export_type([
    string_character_escape_options/0,
    t/0
]).

%% Macros
-define(DEFAULT_STRING_CHARACTER_ESCAPE_OPTIONS, #{legacy => false, 'case' => uppercase}).
-define(is_invalid_string_character(C),
    ((C) =:= $" orelse (C) =:= $\\ orelse (C) =:= $\b orelse (C) =:= $\f orelse (C) =:= $\n orelse (C) =:= $\r orelse
        (C) =:= $\t)
).
-define(is_hex_digit(C), ((C >= $0 andalso C =< $9) orelse (C >= $A andalso C =< $F) orelse (C >= $a andalso C =< $f))).
-define(is_escaped_unicode(A), (?is_hex_digit(A))).
-define(is_escaped_unicode(A, B), (?is_hex_digit(A) andalso ?is_hex_digit(B))).
-define(is_escaped_unicode(A, B, C), (?is_hex_digit(A) andalso ?is_hex_digit(B) andalso ?is_hex_digit(C))).
-define(is_escaped_unicode(A, B, C, D),
    (?is_hex_digit(A) andalso ?is_hex_digit(B) andalso ?is_hex_digit(C) andalso ?is_hex_digit(D))
).
-define(is_escaped_unicode(A, B, C, D, E),
    (?is_hex_digit(A) andalso ?is_hex_digit(B) andalso ?is_hex_digit(C) andalso ?is_hex_digit(D) andalso
        ?is_hex_digit(E))
).
-define(is_escaped_unicode(A, B, C, D, E, F),
    (?is_hex_digit(A) andalso ?is_hex_digit(B) andalso ?is_hex_digit(C) andalso ?is_hex_digit(D) andalso
        ?is_hex_digit(E) andalso ?is_hex_digit(F))
).

%%%=============================================================================
%%% StringValue API functions
%%%=============================================================================

-spec string_character_escape(Character :: char()) -> unicode:unicode_binary().
string_character_escape(Character) ->
    string_character_escape(Character, ?DEFAULT_STRING_CHARACTER_ESCAPE_OPTIONS).

-spec string_character_escape(Character :: char(), Options :: string_character_escape_options()) ->
    unicode:unicode_binary().
string_character_escape(Character, Options) ->
    Legacy = maps:get(legacy, Options, false),
    Case = maps:get('case', Options, uppercase),
    case Character of
        $" ->
            <<$\\, $">>;
        $\\ ->
            <<$\\, $\\>>;
        $\b ->
            <<$\\, $b>>;
        $\f ->
            <<$\\, $f>>;
        $\n ->
            <<$\\, $n>>;
        $\r ->
            <<$\\, $r>>;
        $\t ->
            <<$\\, $t>>;
        % $\/ ->
        %     <<$\\, $\/>>;
        _ when (Character band 16#80) =:= 16#00 andalso Character > 16#1F andalso Character =< 16#7F ->
            <<Character:8>>;
        _ when Legacy =:= true ->
            case Character of
                _ when Character < 16#20 ->
                    <<$\\, $u, (binary:encode_hex(<<0:8, Character:8>>, Case))/bytes>>;
                _ when Character =< 16#FFFF ->
                    <<$\\, $u, (binary:encode_hex(<<(Character bsr 8):8, (Character band 16#FF):8>>, Case))/bytes>>;
                _ ->
                    <<H1:8, H2:8, L1:8, L2:8>> = <<Character/utf16>>,
                    <<
                        $\\,
                        $u,
                        (binary:encode_hex(<<H1:8, H2:8>>, Case))/bytes,
                        $\\,
                        $u,
                        (binary:encode_hex(<<L1:8, L2:8>>, Case))/bytes
                    >>
            end;
        _ when Case =:= lowercase ->
            <<$\\, $u, ${, (list_to_binary(string:to_lower(integer_to_list(Character, 16))))/bytes, $}>>;
        _ when Case =:= uppercase ->
            <<$\\, $u, ${, (integer_to_binary(Character, 16))/bytes, $}>>
    end.

-spec string_value_escape(StringValue) -> EscapedStringValue when
    StringValue :: unicode:unicode_binary(), EscapedStringValue :: unicode:unicode_binary().
string_value_escape(StringValue) when is_binary(StringValue) ->
    string_value_escape(StringValue, ?DEFAULT_STRING_CHARACTER_ESCAPE_OPTIONS).

-spec string_value_escape(StringValue, Options) -> EscapedStringValue when
    StringValue :: unicode:unicode_binary(),
    Options :: string_character_escape_options(),
    EscapedStringValue :: unicode:unicode_binary().
string_value_escape(<<C/utf8, Rest/bytes>>, Options) ->
    <<(string_character_escape(C, Options))/bytes, (string_value_escape(Rest, Options))/bytes>>;
string_value_escape(<<>>, _Options) ->
    <<>>.

-spec string_value_unescape(EscapedStringValue) -> StringValue when
    EscapedStringValue :: unicode:unicode_binary(), StringValue :: unicode:unicode_binary().
string_value_unescape(EscapedStringValue = <<$\\, Rest1/bytes>>) ->
    case Rest1 of
        <<$", Rest2/bytes>> ->
            <<$", (string_value_unescape(Rest2))/bytes>>;
        <<$\\, Rest2/bytes>> ->
            <<$\\, (string_value_unescape(Rest2))/bytes>>;
        <<$b, Rest2/bytes>> ->
            <<$\b, (string_value_unescape(Rest2))/bytes>>;
        <<$f, Rest2/bytes>> ->
            <<$\f, (string_value_unescape(Rest2))/bytes>>;
        <<$n, Rest2/bytes>> ->
            <<$\n, (string_value_unescape(Rest2))/bytes>>;
        <<$r, Rest2/bytes>> ->
            <<$\r, (string_value_unescape(Rest2))/bytes>>;
        <<$t, Rest2/bytes>> ->
            <<$\t, (string_value_unescape(Rest2))/bytes>>;
        <<$\/, Rest2/bytes>> ->
            <<$\/, (string_value_unescape(Rest2))/bytes>>;
        <<$u, ${, A:8, $}, Rest2/bytes>> when ?is_escaped_unicode(A) ->
            Character = binary_to_integer(<<A:8>>, 16),
            <<Character/utf8, (string_value_unescape(Rest2))/bytes>>;
        <<$u, ${, A:8, B:8, $}, Rest2/bytes>> when ?is_escaped_unicode(A, B) ->
            Character = binary_to_integer(<<A:8, B:8>>, 16),
            <<Character/utf8, (string_value_unescape(Rest2))/bytes>>;
        <<$u, ${, A:8, B:8, C:8, $}, Rest2/bytes>> when ?is_escaped_unicode(A, B, C) ->
            Character = binary_to_integer(<<A:8, B:8, C:8>>, 16),
            <<Character/utf8, (string_value_unescape(Rest2))/bytes>>;
        <<$u, ${, A:8, B:8, C:8, D:8, $}, Rest2/bytes>> when ?is_escaped_unicode(A, B, C, D) ->
            Character = binary_to_integer(<<A:8, B:8, C:8, D:8>>, 16),
            <<Character/utf8, (string_value_unescape(Rest2))/bytes>>;
        <<$u, ${, A:8, B:8, C:8, D:8, E:8, $}, Rest2/bytes>> when ?is_escaped_unicode(A, B, C, D, E) ->
            Character = binary_to_integer(<<A:8, B:8, C:8, D:8, E:8>>, 16),
            <<Character/utf8, (string_value_unescape(Rest2))/bytes>>;
        <<$u, ${, A:8, B:8, C:8, D:8, E:8, F:8, $}, Rest2/bytes>> when ?is_escaped_unicode(A, B, C, D, E, F) ->
            case binary_to_integer(<<A:8, B:8, C:8, D:8, E:8>>, 16) of
                Character when
                    (Character >= 16#0000 andalso Character =< 16#D7FF) orelse
                        (Character >= 16#E000 andalso Character =< 16#10FFFF)
                ->
                    <<Character/utf8, (string_value_unescape(Rest2))/bytes>>;
                Character ->
                    error_with_info(badarg, [EscapedStringValue], #{
                        1 => {invalid_unicode_character, Character}, general => invalid_escaped_unicode
                    })
            end;
        <<$u, H1a:8, H2a:8, L1a:8, L2a:8, Rest2/bytes>> when ?is_escaped_unicode(H1a, H2a, L1a, L2a) ->
            <<Ha:8, La:8>> = binary:decode_hex(<<H1a:8, H2a:8, L1a:8, L2a:8>>),
            CharacterA = (Ha bsl 8) bor (La),
            case CharacterA of
                _ when CharacterA >= 16#D800 andalso CharacterA =< 16#DBFF ->
                    case Rest2 of
                        <<$\\, $u, H1b:8, H2b:8, L1b:8, L2b:8, Rest3/bytes>> when
                            ?is_escaped_unicode(H1b, H2b, L1b, L2b)
                        ->
                            <<Hb:8, Lb:8>> = binary:decode_hex(<<H1b:8, H2b:8, L1b:8, L2b:8>>),
                            CharacterB = (Hb bsl 8) bor (Lb),
                            case CharacterB of
                                _ when CharacterB >= 16#DC00 andalso CharacterB =< 16#DFFF ->
                                    % surrogate pair
                                    <<Character/utf16>> = <<Ha:8, La:8, Hb:8, Lb:8>>,
                                    <<Character/utf8, (string_value_unescape(Rest3))/bytes>>;
                                _ ->
                                    % invalid second part of surrogate pair
                                    error_with_info(badarg, [EscapedStringValue], #{
                                        1 => {invalid_second_part_of_surrogate_pair, CharacterA, CharacterB},
                                        general => invalid_escaped_unicode
                                    })
                            end;
                        _ ->
                            % first part of surrogate pair without second part
                            error_with_info(badarg, [EscapedStringValue], #{
                                1 => first_part_of_surrogate_pair_without_second_part,
                                general => invalid_escaped_unicode
                            })
                    end;
                _ when CharacterA >= 16#DC00 andalso CharacterA =< 16#DFFF ->
                    % second part of surrogate pair without first part
                    error_with_info(badarg, [EscapedStringValue], #{
                        1 => second_part_of_surrogate_pair_without_first_part, general => invalid_escaped_unicode
                    });
                _ ->
                    <<CharacterA/utf8, (string_value_unescape(Rest2))/bytes>>
            end;
        _ ->
            error_with_info(badarg, [EscapedStringValue], #{general => invalid_escaped_character})
    end;
string_value_unescape(<<Character/utf8, Rest/bytes>>) when not ?is_invalid_string_character(Character) ->
    <<Character/utf8, (string_value_unescape(Rest))/bytes>>;
string_value_unescape(<<>>) ->
    <<>>.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, StringValue) when is_binary(StringValue) ->
    argo_graphql_formatter:write(Formatter1, "\"~ts\"", [string_value_escape(StringValue)]).

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
format_error_description(_Key, invalid_escaped_character) ->
    "invalid EscapedCharacter in StringValue";
format_error_description(_Key, invalid_escaped_unicode) ->
    "invalid EscapedUnicode in StringValue";
format_error_description(_Key, {invalid_second_part_of_surrogate_pair, CharacterA, CharacterB}) ->
    io_lib:format(
        "invalid second part of surrogate pair (first part = 16#~ts, second part = 16#~ts; expected second part to be between 16#DC00 and 16#DFFF)",
        [integer_to_binary(CharacterA, 16), integer_to_binary(CharacterB, 16)]
    );
format_error_description(_Key, {invalid_unicode_character, Character}) ->
    io_lib:format(
        "invalid unicode character (character = 16#~ts; expected ≥ 16#0000 and ≤ 16#D7FF or ≥ 16#E000 and ≤ 16#10FFFF)",
        [integer_to_binary(Character, 16)]
    );
format_error_description(_Key, first_part_of_surrogate_pair_without_second_part) ->
    "first part of surrogate pair encountered without second part";
format_error_description(_Key, second_part_of_surrogate_pair_without_first_part) ->
    "second part of surrogate pair encountered without first part";
format_error_description(_Key, Value) ->
    Value.
