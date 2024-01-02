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
-module(argo_graphql_input_value_definition).
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
    is_required/1,
    set_default_value/2,
    set_description/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_input_value_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageInputValueDefinition) -> InputValueDefinition when
    LanguageInputValueDefinition :: argo_graphql_language_input_value_definition:t(), InputValueDefinition :: t().
from_language(#argo_graphql_language_input_value_definition{
    name = Name,
    type = LanguageType,
    description = LanguageOptionDescription,
    directives = LanguageOptionDirectivesConst,
    default_value = LanguageOptionDefaultValue
}) ->
    Type = argo_graphql_type:from_language(LanguageType),
    InputValueDefinition1 = new(Name, Type),
    InputValueDefinition2 =
        case LanguageOptionDescription of
            none ->
                InputValueDefinition1;
            {some, Description} ->
                set_description(InputValueDefinition1, {some, Description})
        end,
    InputValueDefinition3 =
        case LanguageOptionDirectivesConst of
            none ->
                InputValueDefinition2;
            {some, LanguageDirectivesConst} ->
                DirectivesConst = argo_graphql_directives_const:from_language(LanguageDirectivesConst),
                InputValueDefinition2#argo_graphql_input_value_definition{directives = DirectivesConst}
        end,
    InputValueDefinition4 =
        case LanguageOptionDefaultValue of
            none ->
                InputValueDefinition3;
            {some, LanguageDefaultValue} ->
                DefaultValue = argo_graphql_value_const:from_language(LanguageDefaultValue),
                set_default_value(InputValueDefinition3, {some, DefaultValue})
        end,
    InputValueDefinition4.

-compile({inline, [new/2]}).
-spec new(Name, Type) -> InputValueDefinition when
    Name :: argo_types:name(), Type :: argo_graphql_type:t(), InputValueDefinition :: t().
new(Name, Type = #argo_graphql_type{}) when is_binary(Name) ->
    #argo_graphql_input_value_definition{
        name = Name,
        type = Type,
        description = none,
        directives = argo_graphql_directives_const:new(),
        default_value = none
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive_const(InputValueDefinition, DirectiveConst) -> InputValueDefinition when
    InputValueDefinition :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    InputValueDefinition1 = #argo_graphql_input_value_definition{directives = DirectivesConst1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConst2 = argo_graphql_directives_const:add_directive_const(DirectivesConst1, DirectiveConst),
    InputValueDefinition2 = InputValueDefinition1#argo_graphql_input_value_definition{directives = DirectivesConst2},
    InputValueDefinition2.

-spec is_required(InputValueDefinition) -> boolean() when InputValueDefinition :: t().
is_required(#argo_graphql_input_value_definition{type = Type, default_value = none}) ->
    case Type of
        #argo_graphql_type{inner = #argo_graphql_non_null_type{}} ->
            true;
        #argo_graphql_type{} ->
            false
    end;
is_required(#argo_graphql_input_value_definition{default_value = {some, _}}) ->
    false.

-spec set_default_value(InputValueDefinition, OptionDefaultValue) -> InputValueDefinition when
    InputValueDefinition :: t(), OptionDefaultValue :: none | {some, argo_graphql_value_const:t()}.
set_default_value(InputValueDefinition1 = #argo_graphql_input_value_definition{}, OptionDefaultValue) when
    ?is_option_record(OptionDefaultValue, argo_graphql_value_const)
->
    InputValueDefinition2 = InputValueDefinition1#argo_graphql_input_value_definition{
        default_value = OptionDefaultValue
    },
    InputValueDefinition2.

-spec set_description(InputValueDefinition, OptionDescription) -> InputValueDefinition when
    InputValueDefinition :: t(), OptionDescription :: none | {some, unicode:unicode_binary()}.
set_description(InputValueDefinition1 = #argo_graphql_input_value_definition{}, OptionDescription) when
    ?is_option_binary(OptionDescription)
->
    InputValueDefinition2 = InputValueDefinition1#argo_graphql_input_value_definition{description = OptionDescription},
    InputValueDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_input_value_definition{
    name = Name,
    type = Type,
    description = OptionDescription,
    directives = DirectivesConst,
    default_value = OptionDefaultValue
}) ->
    Formatter2 =
        case OptionDescription of
            {some, Description} ->
                F1_1 = Formatter1,
                F1_2 = argo_graphql_formatter:write_description(F1_1, Description),
                F1_3 = argo_graphql_formatter:write_indent(F1_2),
                F1_3;
            none ->
                Formatter1
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~ts: ", [Name]),
    Formatter4 = argo_graphql_type:format(Formatter3, Type),
    Formatter5 =
        case OptionDefaultValue of
            {some, DefaultValue} ->
                F4_1 = Formatter4,
                F4_2 = argo_graphql_formatter:write(F4_1, " = ", []),
                F4_3 = argo_graphql_value_const:format(F4_2, DefaultValue),
                F4_3;
            none ->
                Formatter4
        end,
    Formatter6 = argo_graphql_directives_const:format(Formatter5, DirectivesConst),
    Formatter6.
