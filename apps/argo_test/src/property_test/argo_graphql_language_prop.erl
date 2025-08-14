%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_prop).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-08-14"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_graphql_language.hrl").

%% Helpers
-export([
    report_not_equal/2
]).
%% Properties
-export([
    prop_roundtrip_formatter_and_parser/1
]).

%% Macros
-define(EQUALS(A, B), ?WHENFAIL(report_not_equal(A, B), A =:= B)).
% -define(SHOULD_LOG_SIZE, true).

%%%=============================================================================
%%% Helpers
%%%=============================================================================

-spec report_not_equal(A, B) -> ok when A :: term(), B :: term().
report_not_equal(A, B) ->
    io:format("Expected:~n~0tp~nActual:~n~0tp~n", [A, B]).

%%%=============================================================================
%%% Properties
%%%=============================================================================

-spec prop_roundtrip_formatter_and_parser(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_formatter_and_parser(_Config) ->
    ?FORALL(
        Document,
        maybe_log_size(proper_argo_graphql_language:document()),
        begin
            Expected = argo_graphql:format(Document),
            String = argo_types:unicode_string(Expected),
            case argo_graphql_language_scanner:string(String) of
                {ok, Tokens, _} ->
                    case argo_graphql_language_document:from_tokens(Tokens) of
                        {ok, ParsedDocument} ->
                            Actual = argo_graphql:format(ParsedDocument),
                            ?WHENFAIL(
                                begin
                                    io:format(
                                        "FAILURE: Expected (String) does not match Actual (String)~n"
                                        "Expected (Record):~n~0tp~n"
                                        "Actual (Record):~n~0tp~n"
                                        "Expected (String):~n~ts~n"
                                        "Actual (String):~n~ts~n",
                                        [
                                            Document,
                                            strip_location(ParsedDocument),
                                            argo:format_with_lines(Document),
                                            argo:format_with_lines(ParsedDocument)
                                        ]
                                    )
                                end,
                                Expected =:= Actual
                            );
                        ParserError ->
                            ?WHENFAIL(
                                begin
                                    io:format(
                                        "FAILURE: Parser Error due to ~0tp~n"
                                        "Expected (Record):~n~0tp~n"
                                        "Expected (String):~n~ts~n",
                                        [ParserError, Document, argo:format_with_lines(Document)]
                                    )
                                end,
                                false
                            )
                    end;
                LexerError ->
                    ?WHENFAIL(
                        begin
                            io:format(
                                "FAILURE: Lexer Error due to ~0tp~n"
                                "Expected (Record):~n~0tp~n"
                                "Expected (String):~n~ts~n",
                                [LexerError, Document, argo:format_with_lines(Document)]
                            )
                        end,
                        false
                    )
            end
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-compile({inline, [maybe_log_size/1]}).
-spec maybe_log_size(RawType :: proper_types:raw_type()) -> proper_types:type().
-ifdef(SHOULD_LOG_SIZE).
maybe_log_size(RawType) ->
    ?SIZED(
        Size,
        begin
            io:format(user, "Size = ~p~n", [Size]),
            RawType
        end
    ).
-else.
maybe_log_size(RawType) ->
    proper_types:cook_outer(RawType).
-endif.

%% @private
-spec strip_location(dynamic()) -> dynamic().
strip_location(Record) when
    is_tuple(Record) andalso tuple_size(Record) >= 2 andalso is_atom(element(1, Record)) andalso
        ((is_tuple(element(2, Record)) andalso tuple_size(element(2, Record)) =:= 2) orelse
            is_integer(element(2, Record)))
->
    [Name, _OldLocation | Fields1] = tuple_to_list(Record),
    Fields2 = strip_location(Fields1),
    list_to_tuple([Name, 0 | Fields2]);
strip_location({Atom, Value}) when is_atom(Atom) ->
    {Atom, strip_location(Value)};
strip_location(List) when is_list(List) ->
    [strip_location(Value) || Value <- List];
strip_location(Binary) when is_binary(Binary) ->
    Binary;
strip_location(Atom) when is_atom(Atom) ->
    Atom;
strip_location(Number) when is_number(Number) ->
    Number;
strip_location(Boolean) when is_boolean(Boolean) ->
    Boolean.
