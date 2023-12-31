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
-module(argo_graphql_arguments_definition).
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
    add_input_value_definition/2
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
-type t() :: #argo_graphql_arguments_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageArgumentsDefinition) -> ArgumentsDefinition when
    LanguageArgumentsDefinition :: argo_graphql_language_arguments_definition:t(), ArgumentsDefinition :: t().
from_language(#argo_graphql_language_arguments_definition{inputs = LanguageInputValueDefinitionList}) ->
    ArgumentsDefinition1 = new(),
    ArgumentsDefinition2 = lists:foldl(
        fun(LanguageInputValueDefinition, ArgumentsDefinition1Acc1) ->
            InputValueDefinition = argo_graphql_input_value_definition:from_language(LanguageInputValueDefinition),
            ArgumentsDefinition1Acc2 = add_input_value_definition(ArgumentsDefinition1Acc1, InputValueDefinition),
            ArgumentsDefinition1Acc2
        end,
        ArgumentsDefinition1,
        LanguageInputValueDefinitionList
    ),
    ArgumentsDefinition2.

-spec new() -> ArgumentsDefinition when ArgumentsDefinition :: t().
new() ->
    #argo_graphql_arguments_definition{
        inputs = argo_index_map:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_input_value_definition(ArgumentsDefinition, InputValueDefinition) -> ArgumentsDefinition when
    ArgumentsDefinition :: t(), InputValueDefinition :: argo_graphql_input_value_definition:t().
add_input_value_definition(
    ArgumentsDefinition1 = #argo_graphql_arguments_definition{inputs = InputsMap1},
    InputValueDefinition = #argo_graphql_input_value_definition{name = InputName}
) ->
    case argo_index_map:is_key(InputName, InputsMap1) of
        false ->
            InputsMap2 = argo_index_map:put(InputName, InputValueDefinition, InputsMap1),
            ArgumentsDefinition2 = ArgumentsDefinition1#argo_graphql_arguments_definition{inputs = InputsMap2},
            ArgumentsDefinition2;
        true ->
            error_with_info(badarg, [ArgumentsDefinition1, InputValueDefinition], #{
                2 => {duplicate_input_name, InputName}
            })
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_arguments_definition{inputs = InputsMap}) ->
    case argo_index_map:size(InputsMap) of
        0 ->
            Formatter1;
        Length ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, "(", []),
            Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
            Formatter4 = argo_index_map:foldl(
                fun(Index, _InputName, InputValueDefinition, Formatter3_Acc1) ->
                    Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
                    Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
                    Formatter3_Acc4 = argo_graphql_input_value_definition:format(
                        Formatter3_Acc3, InputValueDefinition
                    ),
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
                InputsMap
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
format_error_description(_Key, {duplicate_input_name, InputName}) ->
    io_lib:format("duplicate InputValueDefinition name: ~0tp", [InputName]);
format_error_description(_Key, Value) ->
    Value.
