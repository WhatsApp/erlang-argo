%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_formatter).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Behaviour
-callback shift_left(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
-callback shift_right(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
-callback write(Formatter1, Format :: io:format(), Data :: [term()]) -> Formatter2 when
    Formatter1 :: t(), Formatter2 :: t().
-callback write_indent(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().

%% API
-export([
    shift_left/1,
    shift_right/1,
    write/3,
    write_description/2,
    write_indent/1
]).

%% Types
-type t() :: dynamic().

-export_type([
    t/0
]).

%% Macros
-define(is_record(X), (is_tuple((X)) andalso tuple_size((X)) > 0 andalso is_atom(element(1, (X))))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec shift_left(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
shift_left(Formatter) when ?is_record(Formatter) ->
    Module = element(1, Formatter),
    Module:shift_left(Formatter).

-spec shift_right(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
shift_right(Formatter) when ?is_record(Formatter) ->
    Module = element(1, Formatter),
    Module:shift_right(Formatter).

-spec write(Formatter1, Format :: io:format(), Data :: [term()]) -> Formatter2 when
    Formatter1 :: t(), Formatter2 :: t().
write(Formatter, Format, Data) when ?is_record(Formatter) ->
    Module = element(1, Formatter),
    Module:write(Formatter, Format, Data).

-spec write_description(Formatter1, Description :: unicode:unicode_binary()) -> Formatter2 when
    Formatter1 :: t(), Formatter2 :: t().
write_description(Formatter1, Description) when is_binary(Description) ->
    Formatter2 = write(Formatter1, "\"\"\"~n", []),
    Lines = binary:split(Description, <<$\n>>, [global]),
    Formatter3 = lists:foldl(
        fun(Line, Formatter2_Acc1) ->
            Formatter2_Acc2 = write_indent(Formatter2_Acc1),
            Formatter2_Acc3 = write(Formatter2_Acc2, "~ts~n", [Line]),
            Formatter2_Acc3
        end,
        Formatter2,
        Lines
    ),
    Formatter4 = write_indent(Formatter3),
    Formatter5 = write(Formatter4, "\"\"\"~n", []),
    Formatter5.

-spec write_indent(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
write_indent(Formatter) when ?is_record(Formatter) ->
    Module = element(1, Formatter),
    Module:write_indent(Formatter).
