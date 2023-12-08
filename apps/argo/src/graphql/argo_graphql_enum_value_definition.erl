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
-module(argo_graphql_enum_value_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/1
]).
%% Instance API
-export([
    add_directive_const/2,
    set_description/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_enum_value_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageEnumValueDefinition) -> EnumValueDefinition when
    LanguageEnumValueDefinition :: argo_graphql_language_enum_value_definition:t(), EnumValueDefinition :: t().
from_language(#argo_graphql_language_enum_value_definition{
    value = #argo_graphql_language_enum_value{name = EnumValue},
    description = LanguageOptionDescription,
    directives = LanguageOptionDirectivesConst
}) ->
    EnumValueDefinition1 = new(EnumValue),
    EnumValueDefinition2 =
        case LanguageOptionDescription of
            none ->
                EnumValueDefinition1;
            {some, Description} ->
                set_description(EnumValueDefinition1, {some, Description})
        end,
    EnumValueDefinition3 =
        case LanguageOptionDirectivesConst of
            none ->
                EnumValueDefinition2;
            {some, LanguageDirectivesConst} ->
                DirectivesConst = argo_graphql_directives_const:from_language(LanguageDirectivesConst),
                EnumValueDefinition2#argo_graphql_enum_value_definition{directives = DirectivesConst}
        end,
    EnumValueDefinition3.

-compile({inline, [new/1]}).
-spec new(EnumValue) -> EnumValueDefinition when EnumValue :: argo_types:name(), EnumValueDefinition :: t().
new(EnumValue) when is_binary(EnumValue) ->
    #argo_graphql_enum_value_definition{
        value = EnumValue,
        description = none,
        directives = argo_graphql_directives_const:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive_const(EnumValueDefinition, DirectiveConst) -> EnumValueDefinition when
    EnumValueDefinition :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    EnumValueDefinition1 = #argo_graphql_enum_value_definition{directives = DirectivesConst1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConst2 = argo_graphql_directives_const:add_directive_const(DirectivesConst1, DirectiveConst),
    EnumValueDefinition2 = EnumValueDefinition1#argo_graphql_enum_value_definition{directives = DirectivesConst2},
    EnumValueDefinition2.

-spec set_description(EnumValueDefinition, OptionDescription) -> EnumValueDefinition when
    EnumValueDefinition :: t(), OptionDescription :: none | {some, unicode:unicode_binary()}.
set_description(EnumValueDefinition1 = #argo_graphql_enum_value_definition{}, OptionDescription) when
    ?is_option_binary(OptionDescription)
->
    EnumValueDefinition2 = EnumValueDefinition1#argo_graphql_enum_value_definition{description = OptionDescription},
    EnumValueDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_enum_value_definition{
    value = EnumValue,
    description = OptionDescription,
    directives = DirectivesConst
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
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~ts", [EnumValue]),
    Formatter4 = argo_graphql_directives_const:format(Formatter3, DirectivesConst),
    Formatter4.
