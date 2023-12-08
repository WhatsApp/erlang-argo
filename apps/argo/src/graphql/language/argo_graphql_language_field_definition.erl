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
-module(argo_graphql_language_field_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% API
-export([
    parse/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type context() :: #{
    name := argo_types:name(),
    type := argo_graphql_language_type:t(),
    arguments => argo_graphql_language_arguments_definition:t(),
    description => unicode:unicode_binary(),
    directives => argo_graphql_language_directives_const:t()
}.
-type t() :: #argo_graphql_language_field_definition{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> FieldDefinition when
    Context :: context(), Location :: erl_anno:location(), FieldDefinition :: t().
parse(Context = #{name := Name, type := Type = #argo_graphql_language_type{}}, Location) when is_binary(Name) ->
    OptionArguments =
        case maps:find(arguments, Context) of
            {ok, ArgumentsDefinition = #argo_graphql_language_arguments_definition{}} ->
                {some, ArgumentsDefinition};
            error ->
                none
        end,
    OptionDescription =
        case maps:find(description, Context) of
            {ok, Description} when is_binary(Description) ->
                {some, Description};
            error ->
                none
        end,
    OptionDirectives =
        case maps:find(directives, Context) of
            {ok, DirectivesConst = #argo_graphql_language_directives_const{}} ->
                {some, DirectivesConst};
            error ->
                none
        end,
    #argo_graphql_language_field_definition{
        location = Location,
        name = Name,
        type = Type,
        arguments = OptionArguments,
        description = OptionDescription,
        directives = OptionDirectives
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_field_definition{
    name = Name,
    type = Type,
    arguments = OptionArguments,
    description = OptionDescription,
    directives = OptionDirectives
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
    Formatter4 =
        case OptionArguments of
            {some, ArgumentsDefinition} ->
                argo_graphql_language_arguments_definition:format(Formatter3, ArgumentsDefinition);
            none ->
                Formatter3
        end,
    Formatter5 = argo_graphql_formatter:write(Formatter4, ": ", []),
    Formatter6 = argo_graphql_language_type:format(Formatter5, Type),
    Formatter7 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives_const:format(Formatter6, Directives);
            none ->
                Formatter6
        end,
    Formatter7.
