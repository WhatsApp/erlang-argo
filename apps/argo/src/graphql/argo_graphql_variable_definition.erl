%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_variable_definition).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/2
]).
%% Instance API
-export([
    add_directive_const/2,
    set_default_value/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_variable_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageVariableDefinition) -> VariableDefinition when
    LanguageVariableDefinition :: argo_graphql_language_variable_definition:t(), VariableDefinition :: t().
from_language(#argo_graphql_language_variable_definition{
    variable = #argo_graphql_language_variable{name = VariableName},
    type = LanguageType,
    default_value = LanguageOptionDefaultValue,
    directives = LanguageOptionDirectivesConst
}) ->
    Type = argo_graphql_type:from_language(LanguageType),
    VariableDefinition1 = new(VariableName, Type),
    VariableDefinition2 =
        case LanguageOptionDefaultValue of
            none ->
                VariableDefinition1;
            {some, LanguageDefaultValue} ->
                DefaultValue = argo_graphql_value_const:from_language(LanguageDefaultValue),
                set_default_value(VariableDefinition1, {some, DefaultValue})
        end,
    VariableDefinition3 =
        case LanguageOptionDirectivesConst of
            none ->
                VariableDefinition2;
            {some, LanguageDirectivesConst} ->
                DirectivesConst = argo_graphql_directives_const:from_language(LanguageDirectivesConst),
                VariableDefinition2#argo_graphql_variable_definition{directives = DirectivesConst}
        end,
    VariableDefinition3.

-compile({inline, [new/2]}).
-spec new(VariableName, Type) -> VariableDefinition when
    VariableName :: argo_types:name(), Type :: argo_graphql_type:t(), VariableDefinition :: t().
new(VariableName, Type = #argo_graphql_type{}) when is_binary(VariableName) ->
    #argo_graphql_variable_definition{
        variable = VariableName,
        type = Type,
        default_value = none,
        directives = argo_graphql_directives_const:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive_const(VariableDefinition, DirectiveConst) -> VariableDefinition when
    VariableDefinition :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    VariableDefinition1 = #argo_graphql_variable_definition{directives = DirectivesConst1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConst2 = argo_graphql_directives_const:add_directive_const(DirectivesConst1, DirectiveConst),
    VariableDefinition2 = VariableDefinition1#argo_graphql_variable_definition{directives = DirectivesConst2},
    VariableDefinition2.

-spec set_default_value(VariableDefinition, OptionDefaultValue) -> VariableDefinition when
    VariableDefinition :: t(), OptionDefaultValue :: none | {some, argo_graphql_value_const:t()}.
set_default_value(VariableDefinition1 = #argo_graphql_variable_definition{}, OptionDefaultValue) when
    ?is_option_record(OptionDefaultValue, argo_graphql_value_const)
->
    VariableDefinition2 = VariableDefinition1#argo_graphql_variable_definition{
        default_value = OptionDefaultValue
    },
    VariableDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_variable_definition{
    variable = VariableName,
    type = Type,
    default_value = OptionDefaultValue,
    directives = DirectivesConst
}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "$~ts: ", [VariableName]),
    Formatter3 = argo_graphql_type:format(Formatter2, Type),
    Formatter4 =
        case OptionDefaultValue of
            {some, DefaultValue} ->
                F3_1 = Formatter3,
                F3_2 = argo_graphql_formatter:write(F3_1, " = ", []),
                F3_3 = argo_graphql_value_const:format(F3_2, DefaultValue),
                F3_3;
            none ->
                Formatter3
        end,
    Formatter5 = argo_graphql_directives_const:format(Formatter4, DirectivesConst),
    Formatter5.
