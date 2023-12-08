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
-module(argo_graphql_field_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    builtin/1,
    from_language/1,
    new/2
]).
%% Instance API
-export([
    add_argument_definition/2,
    add_directive_const/2,
    set_description/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_field_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec builtin(FieldName) -> {ok, FieldDefinition} | error when FieldName :: argo_types:name(), FieldDefinition :: t().
builtin(FieldName = <<"__typename">>) ->
    % See: https://spec.graphql.org/draft/#sec-Type-Name-Introspection
    Type = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"String">>)),
    FieldDefinition = new(FieldName, Type),
    {ok, FieldDefinition};
builtin(FieldDefinition) when is_binary(FieldDefinition) ->
    error.

-spec from_language(LanguageFieldDefinition) -> FieldDefinition when
    LanguageFieldDefinition :: argo_graphql_language_field_definition:t(), FieldDefinition :: t().
from_language(#argo_graphql_language_field_definition{
    name = Name,
    type = LanguageType,
    arguments = LanguageOptionArgumentsDefinition,
    description = LanguageOptionDescription,
    directives = LanguageOptionDirectivesConst
}) ->
    Type = argo_graphql_type:from_language(LanguageType),
    FieldDefinition1 = new(Name, Type),
    FieldDefinition2 =
        case LanguageOptionDescription of
            none ->
                FieldDefinition1;
            {some, Description} ->
                set_description(FieldDefinition1, {some, Description})
        end,
    FieldDefinition3 =
        case LanguageOptionDirectivesConst of
            none ->
                FieldDefinition2;
            {some, LanguageDirectivesConst} ->
                DirectivesConst = argo_graphql_directives_const:from_language(LanguageDirectivesConst),
                FieldDefinition2#argo_graphql_field_definition{directives = DirectivesConst}
        end,
    FieldDefinition4 =
        case LanguageOptionArgumentsDefinition of
            none ->
                FieldDefinition3;
            {some, LanguageArgumentsDefinition} ->
                ArgumentsDefinition = argo_graphql_arguments_definition:from_language(LanguageArgumentsDefinition),
                FieldDefinition3#argo_graphql_field_definition{arguments = ArgumentsDefinition}
        end,
    FieldDefinition4.

-compile({inline, [new/2]}).
-spec new(Name, Type) -> FieldDefinition when
    Name :: argo_types:name(), Type :: argo_graphql_type:t(), FieldDefinition :: t().
new(Name, Type = #argo_graphql_type{}) when is_binary(Name) ->
    #argo_graphql_field_definition{
        name = Name,
        type = Type,
        arguments = argo_graphql_arguments_definition:new(),
        description = none,
        directives = argo_graphql_directives_const:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_argument_definition(FieldDefinition, InputValueDefinition) -> FieldDefinition when
    FieldDefinition :: t(), InputValueDefinition :: argo_graphql_input_value_definition:t().
add_argument_definition(
    FieldDefinition1 = #argo_graphql_field_definition{arguments = ArgumentsDefinition1},
    InputValueDefinition = #argo_graphql_input_value_definition{}
) ->
    ArgumentsDefinition2 = argo_graphql_arguments_definition:add_input_value_definition(
        ArgumentsDefinition1, InputValueDefinition
    ),
    FieldDefinition2 = FieldDefinition1#argo_graphql_field_definition{arguments = ArgumentsDefinition2},
    FieldDefinition2.

-spec add_directive_const(FieldDefinition, DirectiveConst) -> FieldDefinition when
    FieldDefinition :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    FieldDefinition1 = #argo_graphql_field_definition{directives = DirectivesConst1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConst2 = argo_graphql_directives_const:add_directive_const(DirectivesConst1, DirectiveConst),
    FieldDefinition2 = FieldDefinition1#argo_graphql_field_definition{directives = DirectivesConst2},
    FieldDefinition2.

-spec set_description(FieldDefinition, OptionDescription) -> FieldDefinition when
    FieldDefinition :: t(), OptionDescription :: none | {some, unicode:unicode_binary()}.
set_description(FieldDefinition1 = #argo_graphql_field_definition{}, OptionDescription) when
    ?is_option_binary(OptionDescription)
->
    FieldDefinition2 = FieldDefinition1#argo_graphql_field_definition{description = OptionDescription},
    FieldDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_field_definition{
    name = Name,
    type = Type,
    arguments = ArgumentsDefinition,
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
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~ts", [Name]),
    Formatter4 = argo_graphql_arguments_definition:format(Formatter3, ArgumentsDefinition),
    Formatter5 = argo_graphql_formatter:write(Formatter4, ": ", []),
    Formatter6 = argo_graphql_type:format(Formatter5, Type),
    Formatter7 = argo_graphql_directives_const:format(Formatter6, DirectivesConst),
    Formatter7.
