%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_printer).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_formatter).

%% API
-export([
    new_io_device/2,
    new_string/1,
    finalize/1
]).
%% argo_graphql_formatter callbacks
-export([
    shift_left/1,
    shift_right/1,
    write/3,
    write_indent/1
]).

%% Records
-record(argo_graphql_printer, {
    depth = 0 :: non_neg_integer(),
    output = [] :: iolist() | io:device(),
    strict = false :: boolean()
}).

%% Types
-type options() :: #{
    strict => boolean()
}.
-type t() :: #argo_graphql_printer{}.

-export_type([
    options/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new_io_device(IoDevice, Options) -> Printer when IoDevice :: io:device(), Options :: options(), Printer :: t().
new_io_device(IoDevice, Options) when not is_list(IoDevice) andalso is_map(Options) ->
    Strict = maps:get(strict, Options, false),
    #argo_graphql_printer{depth = 0, output = IoDevice, strict = Strict}.

-spec new_string(Options) -> Printer when Options :: options(), Printer :: t().
new_string(Options) when is_map(Options) ->
    Strict = maps:get(strict, Options, false),
    #argo_graphql_printer{depth = 0, output = [], strict = Strict}.

-spec finalize(Printer) -> ok | iolist() when Printer :: t().
finalize(#argo_graphql_printer{output = Output}) when is_list(Output) ->
    Output;
finalize(Printer = #argo_graphql_printer{}) ->
    _ = write(Printer, "~n", []),
    ok.

%%%=============================================================================
%%% argo_graphql_formatter callbacks
%%%=============================================================================

-spec shift_left(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
shift_left(Formatter1 = #argo_graphql_printer{depth = Depth1}) ->
    Depth2 = Depth1 - 1,
    Formatter2 = Formatter1#argo_graphql_printer{depth = Depth2},
    Formatter2.

-spec shift_right(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
shift_right(Formatter1 = #argo_graphql_printer{depth = Depth1}) ->
    Depth2 = Depth1 + 1,
    Formatter2 = Formatter1#argo_graphql_printer{depth = Depth2},
    Formatter2.

-spec write(Formatter1, Format :: io:format(), Data :: [term()]) -> Formatter2 when
    Formatter1 :: t(), Formatter2 :: t().
write(Formatter1 = #argo_graphql_printer{output = Output1}, Format, Data) when is_list(Output1) ->
    Output2 = [Output1 | io_lib:format(Format, Data)],
    Formatter2 = Formatter1#argo_graphql_printer{output = Output2},
    Formatter2;
write(Formatter1 = #argo_graphql_printer{output = IoDevice}, Format, Data) ->
    ok = io:format(IoDevice, Format, Data),
    Formatter1.

-spec write_indent(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
write_indent(Formatter1 = #argo_graphql_printer{depth = Depth}) ->
    Formatter2 = write(Formatter1, "~ts", [binary:copy(<<"  ">>, Depth)]),
    Formatter2.
