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
-module(argo_graphql_language_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).

-include_lib("proper/include/proper.hrl").

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
-ifdef(WHENFAIL).
-undef(WHENFAIL).
-endif.
% eqWAlizer gets angry about `fun(() -> boolean())` not being a subtype of `fun(() -> proper:test())`
-define(WHENFAIL(Action, Prop), proper:whenfail(?DELAY(Action), dynamic_cast(?DELAY(Prop)))).
-define(EQUALS(A, B), ?WHENFAIL(report_not_equal(A, B), A =:= B)).
-define(SHOULD_LOG_SIZE, false).

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
            Expected = dump_document(Document),
            String = unicode:characters_to_list(Expected, utf8),
            case argo_graphql_language_scanner:string(String) of
                {ok, Tokens, _} ->
                    case argo_graphql_language_parser:parse(Tokens) of
                        {ok, ParsedDocument} ->
                            Actual = dump_document(ParsedDocument),
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
                                            dump_with_lines(Expected),
                                            dump_with_lines(Actual)
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
                                        [ParserError, Document, dump_with_lines(Expected)]
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
                                [LexerError, Document, dump_with_lines(Expected)]
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
-spec dump_document(argo_graphql_language_document:t()) -> unicode:unicode_binary().
dump_document(Document = #argo_graphql_language_document{}) ->
    Printer1 = argo_graphql_printer:new_string(),
    Printer2 = argo_graphql_language_document:format(Printer1, Document),
    dynamic_cast(argo_graphql_printer:finalize(Printer2)).

%% @private
-spec dump_with_lines(String :: unicode:chardata()) -> unicode:chardata().
dump_with_lines(String) ->
    Binary = unicode:characters_to_binary(String),
    Lines = binary:split(Binary, <<$\n>>, [global]),
    Width = byte_size(erlang:iolist_to_binary(io_lib:format("~w", [length(Lines)]))),
    unicode:characters_to_binary(
        lists:map(
            fun({Row, Line}) ->
                io_lib:format("~*w: ~ts~n", [Width, Row, Line])
            end,
            lists:enumerate(Lines)
        )
    ).

%% @private
-compile({inline, [dynamic_cast/1]}).
-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.

%% @private
-compile({inline, [maybe_log_size/1]}).
-spec maybe_log_size(RawType :: proper_types:raw_type()) -> proper_types:type().
maybe_log_size(RawType) ->
    case ?SHOULD_LOG_SIZE of
        false ->
            RawType;
        true ->
            ?SIZED(
                Size,
                begin
                    io:format(user, "Size = ~p~n", [Size]),
                    RawType
                end
            )
    end.

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
