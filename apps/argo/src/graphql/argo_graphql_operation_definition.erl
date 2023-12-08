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
-module(argo_graphql_operation_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/1,
    new/3
]).
%% Instance API
-export([
    add_directive/2,
    add_selection/2,
    add_variable_definition/2,
    is_shorthand/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type operation_type() :: argo_graphql_language_root_operation_type_definition:operation_type().
-type t() :: #argo_graphql_operation_definition{}.

-export_type([
    operation_type/0,
    t/0
]).

%% Macros
-define(is_operation_type(T), ((T) =:= 'query' orelse (T) =:= 'mutation' orelse (T) =:= 'subscription')).
-define(is_selection(X),
    (is_record((X), argo_graphql_field) orelse is_record((X), argo_graphql_fragment_spread) orelse
        is_record((X), argo_graphql_inline_fragment))
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageOperationDefinition) -> OperationDefinition when
    LanguageOperationDefinition :: argo_graphql_language_operation_definition:t(), OperationDefinition :: t().
from_language(#argo_graphql_language_operation_definition{
    operation = Operation,
    name = LanguageOptionName,
    variables_definition = LanguageOptionVariablesDefinition,
    directives = LanguageOptionDirectives,
    selection_set = LanguageSelectionSet,
    shorthand = Shorthand
}) ->
    SelectionSet = argo_graphql_selection_set:from_language(LanguageSelectionSet),
    case Shorthand of
        false ->
            OperationDefinition1 = new(Operation, LanguageOptionName, SelectionSet),
            OperationDefinition2 =
                case LanguageOptionDirectives of
                    none ->
                        OperationDefinition1;
                    {some, LanguageDirectives} ->
                        Directives = argo_graphql_directives:from_language(LanguageDirectives),
                        OperationDefinition1#argo_graphql_operation_definition{directives = Directives}
                end,
            OperationDefinition3 =
                case LanguageOptionVariablesDefinition of
                    none ->
                        OperationDefinition2;
                    {some, LanguageVariablesDefinition} ->
                        VariablesDefinition = argo_graphql_variables_definition:from_language(
                            LanguageVariablesDefinition
                        ),
                        OperationDefinition2#argo_graphql_operation_definition{
                            variables_definition = VariablesDefinition
                        }
                end,
            OperationDefinition3;
        true ->
            new(SelectionSet)
    end.

-compile({inline, [new/1]}).
-spec new(SelectionSet) -> OperationDefinition when
    SelectionSet :: argo_graphql_selection_set:t(),
    OperationDefinition :: t().
new(SelectionSet = #argo_graphql_selection_set{}) ->
    #argo_graphql_operation_definition{
        operation = 'query',
        name = none,
        variables_definition = argo_graphql_variables_definition:new(),
        directives = argo_graphql_directives:new(),
        selection_set = SelectionSet,
        shorthand = true
    }.

-compile({inline, [new/3]}).
-spec new(Operation, OptionName, SelectionSet) -> OperationDefinition when
    Operation :: operation_type(),
    OptionName :: none | {some, argo_types:name()},
    SelectionSet :: argo_graphql_selection_set:t(),
    OperationDefinition :: t().
new(Operation, OptionName, SelectionSet = #argo_graphql_selection_set{}) when
    ?is_operation_type(Operation) andalso ?is_option_binary(OptionName)
->
    #argo_graphql_operation_definition{
        operation = Operation,
        name = OptionName,
        variables_definition = argo_graphql_variables_definition:new(),
        directives = argo_graphql_directives:new(),
        selection_set = SelectionSet,
        shorthand = false
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive(OperationDefinition, Directive) -> OperationDefinition when
    OperationDefinition :: t(), Directive :: argo_graphql_directive:t().
add_directive(
    OperationDefinition1 = #argo_graphql_operation_definition{directives = Directives1, shorthand = false},
    Directive = #argo_graphql_directive{}
) ->
    Directives2 = argo_graphql_directives:add_directive(Directives1, Directive),
    OperationDefinition2 = OperationDefinition1#argo_graphql_operation_definition{directives = Directives2},
    OperationDefinition2.

-spec add_selection(OperationDefinition, Selection) -> OperationDefinition when
    OperationDefinition :: t(), Selection :: argo_graphql_selection_set:selection().
add_selection(OperationDefinition1 = #argo_graphql_operation_definition{selection_set = SelectionSet1}, Selection) when
    ?is_selection(Selection)
->
    SelectionSet2 = argo_graphql_selection_set:add_selection(SelectionSet1, Selection),
    OperationDefinition2 = OperationDefinition1#argo_graphql_operation_definition{selection_set = SelectionSet2},
    OperationDefinition2.

-spec add_variable_definition(OperationDefinition, VariableDefinition) -> OperationDefinition when
    OperationDefinition :: t(), VariableDefinition :: argo_graphql_variable_definition:t().
add_variable_definition(
    OperationDefinition1 = #argo_graphql_operation_definition{
        variables_definition = VariablesDefinition1, shorthand = false
    },
    VariableDefinition = #argo_graphql_variable_definition{}
) ->
    VariablesDefinition2 = argo_graphql_variables_definition:add_variable_definition(
        VariablesDefinition1, VariableDefinition
    ),
    OperationDefinition2 = OperationDefinition1#argo_graphql_operation_definition{
        variables_definition = VariablesDefinition2
    },
    OperationDefinition2.

-spec is_shorthand(OperationDefinition) -> boolean() when OperationDefinition :: t().
is_shorthand(#argo_graphql_operation_definition{shorthand = Shorthand}) ->
    Shorthand =:= true.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_operation_definition{
    operation = Operation,
    name = OptionName,
    variables_definition = VariablesDefinition,
    directives = Directives,
    selection_set = SelectionSet,
    shorthand = Shorthand
}) ->
    case Shorthand of
        false ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, "~ts", [Operation]),
            Formatter3 =
                case OptionName of
                    none ->
                        Formatter2;
                    {some, Name} ->
                        argo_graphql_formatter:write(Formatter2, " ~ts", [Name])
                end,
            Formatter4 = argo_graphql_variables_definition:format(Formatter3, VariablesDefinition),
            Formatter5 = argo_graphql_directives:format(Formatter4, Directives),
            Formatter6 = argo_graphql_selection_set:format(Formatter5, SelectionSet),
            Formatter7 = argo_graphql_formatter:write(Formatter6, "~n", []),
            Formatter7;
        true ->
            Formatter2 = argo_graphql_selection_set:format_shorthand(Formatter1, SelectionSet),
            Formatter3 = argo_graphql_formatter:write(Formatter2, "~n", []),
            Formatter3
    end.
