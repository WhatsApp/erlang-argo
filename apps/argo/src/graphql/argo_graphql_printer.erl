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
-module(argo_graphql_printer).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_formatter).

%% API
-export([
    new_io_device/1,
    new_string/0,
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
    output = [] :: iolist() | io:device()
}).

%% Types
-type t() :: #argo_graphql_printer{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new_io_device(IoDevice) -> Printer when IoDevice :: io:device(), Printer :: t().
new_io_device(IoDevice) when not is_list(IoDevice) ->
    #argo_graphql_printer{depth = 0, output = IoDevice}.

-spec new_string() -> Printer when Printer :: t().
new_string() ->
    #argo_graphql_printer{depth = 0, output = []}.

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
