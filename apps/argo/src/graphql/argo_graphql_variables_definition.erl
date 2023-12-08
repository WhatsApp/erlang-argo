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
-module(argo_graphql_variables_definition).
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
    add_variable_definition/2
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
-type t() :: #argo_graphql_variables_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageVariablesDefinition) -> VariablesDefinition when
    LanguageVariablesDefinition :: argo_graphql_variables_definition:t(), VariablesDefinition :: t().
from_language(#argo_graphql_language_variables_definition{variables = LanguageVariableDefinitionList}) ->
    VariablesDefinition1 = new(),
    VariablesDefinition2 = lists:foldl(
        fun(LanguageVariableDefinition, VariablesDefinition1Acc1) ->
            VariableDefinition = argo_graphql_variable_definition:from_language(LanguageVariableDefinition),
            VariablesDefinition1Acc2 = add_variable_definition(VariablesDefinition1Acc1, VariableDefinition),
            VariablesDefinition1Acc2
        end,
        VariablesDefinition1,
        LanguageVariableDefinitionList
    ),
    VariablesDefinition2.

-spec new() -> VariablesDefinition when VariablesDefinition :: t().
new() ->
    #argo_graphql_variables_definition{
        variables = argo_index_map:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_variable_definition(VariablesDefinition, VariableDefinition) -> VariablesDefinition when
    VariablesDefinition :: t(), VariableDefinition :: argo_graphql_variable_definition:t().
add_variable_definition(
    VariablesDefinition1 = #argo_graphql_variables_definition{variables = VariablesMap1},
    VariableDefinition = #argo_graphql_variable_definition{variable = VariableName}
) ->
    case argo_index_map:is_key(VariableName, VariablesMap1) of
        false ->
            VariablesMap2 = argo_index_map:put(VariableName, VariableDefinition, VariablesMap1),
            VariablesDefinition2 = VariablesDefinition1#argo_graphql_variables_definition{variables = VariablesMap2},
            VariablesDefinition2;
        true ->
            error_with_info(badarg, [VariablesDefinition1, VariableDefinition], #{
                2 => {duplicate_variable_name, VariableName}
            })
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_variables_definition{variables = VariablesMap}) ->
    case argo_index_map:size(VariablesMap) of
        0 ->
            Formatter1;
        Length ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, "(", []),
            Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
            Formatter4 = argo_index_map:foldl(
                fun(Index, _VariableName, VariableDefinition, Formatter3_Acc1) ->
                    Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
                    Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
                    Formatter3_Acc4 = argo_graphql_variable_definition:format(Formatter3_Acc3, VariableDefinition),
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
                VariablesMap
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
format_error_description(_Key, {duplicate_variable_name, VariableName}) ->
    io_lib:format("duplicate VariableDefinition name: ~0tp", [VariableName]);
format_error_description(_Key, Value) ->
    Value.
