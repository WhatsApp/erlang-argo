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
-module(argo_graphql_arguments).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/0
]).
%% Instance API
-export([
    add_argument/2,
    find_argument/2,
    get_argument/2
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
-type t() :: #argo_graphql_arguments{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageArguments) -> Arguments when
    LanguageArguments :: argo_graphql_language_arguments:t(), Arguments :: t().
from_language(#argo_graphql_language_arguments{arguments = LanguageArgumentList}) ->
    Arguments1 = new(),
    Arguments2 = lists:foldl(
        fun(LanguageArgument, Arguments1Acc1) ->
            Argument = argo_graphql_argument:from_language(LanguageArgument),
            Arguments1Acc2 = add_argument(Arguments1Acc1, Argument),
            Arguments1Acc2
        end,
        Arguments1,
        LanguageArgumentList
    ),
    Arguments2.

-spec new() -> Arguments when Arguments :: t().
new() ->
    #argo_graphql_arguments{
        arguments = argo_index_map:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_argument(Arguments, Argument) -> Arguments when Arguments :: t(), Argument :: argo_graphql_argument:t().
add_argument(
    Arguments1 = #argo_graphql_arguments{arguments = ArgumentsMap1},
    Argument = #argo_graphql_argument{name = ArgumentName}
) ->
    case argo_index_map:is_key(ArgumentName, ArgumentsMap1) of
        false ->
            ArgumentsMap2 = argo_index_map:put(ArgumentName, Argument, ArgumentsMap1),
            Arguments2 = Arguments1#argo_graphql_arguments{arguments = ArgumentsMap2},
            Arguments2;
        true ->
            error_with_info(badarg, [Arguments1, Argument], #{2 => {duplicate_argument_name, ArgumentName}})
    end.

-spec find_argument(Arguments, ArgumentName) -> {ok, Argument} | error when
    Arguments :: t(), ArgumentName :: argo_types:name(), Argument :: argo_graphql_argument:t().
find_argument(#argo_graphql_arguments{arguments = ArgumentsMap}, ArgumentName) when is_binary(ArgumentName) ->
    argo_index_map:find(ArgumentName, ArgumentsMap).

-spec get_argument(Arguments, ArgumentName) -> Argument when
    Arguments :: t(), ArgumentName :: argo_types:name(), Argument :: argo_graphql_argument:t().
get_argument(Arguments = #argo_graphql_arguments{}, ArgumentName) when is_binary(ArgumentName) ->
    case find_argument(Arguments, ArgumentName) of
        {ok, Argument} ->
            Argument;
        error ->
            error_with_info(badarg, [Arguments, ArgumentName], #{2 => {missing_argument_name, ArgumentName}})
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_arguments{arguments = ArgumentsMap}) ->
    case argo_index_map:size(ArgumentsMap) of
        0 ->
            Formatter1;
        Length ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, "(", []),
            Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
            Formatter4 = argo_index_map:foldl(
                fun(Index, _ArgumentName, Argument, Formatter3_Acc1) ->
                    Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
                    Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
                    Formatter3_Acc4 = argo_graphql_argument:format(Formatter3_Acc3, Argument),
                    Formatter3_Acc5 =
                        case (Index + 1) =:= Length of
                            false ->
                                argo_graphql_formatter:write(Formatter3_Acc4, ",", []);
                            true ->
                                Formatter3_Acc4
                        end,
                    Formatter3_Acc5
                end,
                Formatter3,
                ArgumentsMap
            ),
            Formatter5 = argo_graphql_formatter:write(Formatter4, "~n", []),
            Formatter6 = argo_graphql_formatter:shift_left(Formatter5),
            Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
            Formatter8 = argo_graphql_formatter:write(Formatter7, ")", []),
            Formatter8
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
format_error_description(_Key, {duplicate_argument_name, ArgumentName}) ->
    io_lib:format("duplicate Argument name: ~0tp", [ArgumentName]);
format_error_description(_Key, {missing_argument_name, ArgumentName}) ->
    io_lib:format("missing Argument name: ~0tp", [ArgumentName]);
format_error_description(_Key, Value) ->
    Value.
