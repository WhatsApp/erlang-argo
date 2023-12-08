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
-module(argo_graphql_arguments_const).
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
    add_argument_const/2,
    find_argument_const/2,
    get_argument_const/2
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
-type t() :: #argo_graphql_arguments_const{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageArgumentsConst) -> ArgumentsConst when
    LanguageArgumentsConst :: argo_graphql_language_arguments_const:t(), ArgumentsConst :: t().
from_language(#argo_graphql_language_arguments_const{arguments = LanguageArgumentConstList}) ->
    ArgumentsConst1 = new(),
    ArgumentsConst2 = lists:foldl(
        fun(LanguageArgumentConst, ArgumentsConst1Acc1) ->
            ArgumentConst = argo_graphql_argument_const:from_language(LanguageArgumentConst),
            ArgumentsConst1Acc2 = add_argument_const(ArgumentsConst1Acc1, ArgumentConst),
            ArgumentsConst1Acc2
        end,
        ArgumentsConst1,
        LanguageArgumentConstList
    ),
    ArgumentsConst2.

-spec new() -> ArgumentsConst when ArgumentsConst :: t().
new() ->
    #argo_graphql_arguments_const{
        arguments = argo_index_map:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_argument_const(ArgumentsConst, ArgumentConst) -> ArgumentsConst when
    ArgumentsConst :: t(), ArgumentConst :: argo_graphql_argument_const:t().
add_argument_const(
    ArgumentsConst1 = #argo_graphql_arguments_const{arguments = ArgumentsConstMap1},
    ArgumentConst = #argo_graphql_argument_const{name = ArgumentName}
) ->
    case argo_index_map:is_key(ArgumentName, ArgumentsConstMap1) of
        false ->
            ArgumentsConstMap2 = argo_index_map:put(ArgumentName, ArgumentConst, ArgumentsConstMap1),
            ArgumentsConst2 = ArgumentsConst1#argo_graphql_arguments_const{arguments = ArgumentsConstMap2},
            ArgumentsConst2;
        true ->
            error_with_info(badarg, [ArgumentsConst1, ArgumentConst], #{2 => {duplicate_argument_name, ArgumentName}})
    end.

-spec find_argument_const(ArgumentsConst, ArgumentConstName) -> {ok, ArgumentConst} | error when
    ArgumentsConst :: t(), ArgumentConstName :: argo_types:name(), ArgumentConst :: argo_graphql_argument_const:t().
find_argument_const(#argo_graphql_arguments_const{arguments = ArgumentsConstMap}, ArgumentConstName) when
    is_binary(ArgumentConstName)
->
    argo_index_map:find(ArgumentConstName, ArgumentsConstMap).

-spec get_argument_const(ArgumentsConst, ArgumentConstName) -> ArgumentConst when
    ArgumentsConst :: t(), ArgumentConstName :: argo_types:name(), ArgumentConst :: argo_graphql_argument_const:t().
get_argument_const(ArgumentsConst = #argo_graphql_arguments_const{}, ArgumentConstName) when
    is_binary(ArgumentConstName)
->
    case find_argument_const(ArgumentsConst, ArgumentConstName) of
        {ok, ArgumentConst} ->
            ArgumentConst;
        error ->
            error_with_info(badarg, [ArgumentsConst, ArgumentConstName], #{
                2 => {missing_argument_name, ArgumentConstName}
            })
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_arguments_const{arguments = ArgumentsConstMap}) ->
    case argo_index_map:size(ArgumentsConstMap) of
        0 ->
            Formatter1;
        Length ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, "(", []),
            Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
            Formatter4 = argo_index_map:foldl(
                fun(Index, _ArgumentName, ArgumentConst, Formatter3_Acc1) ->
                    Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
                    Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
                    Formatter3_Acc4 = argo_graphql_argument_const:format(Formatter3_Acc3, ArgumentConst),
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
                ArgumentsConstMap
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
    io_lib:format("duplicate ArgumentConst name: ~0tp", [ArgumentName]);
format_error_description(_Key, {missing_argument_name, ArgumentName}) ->
    io_lib:format("missing ArgumentConst name: ~0tp", [ArgumentName]);
format_error_description(_Key, Value) ->
    Value.
