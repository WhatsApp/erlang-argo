%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_input_value_definition).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
    description => unicode:unicode_binary(),
    directives => argo_graphql_language_directives_const:t(),
    default_value => argo_graphql_language_value_const:t()
}.
-type t() :: #argo_graphql_language_input_value_definition{}.

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
    OptionDefaultValue =
        case maps:find(default_value, Context) of
            {ok, DefaultValue = #argo_graphql_language_value_const{}} ->
                {some, DefaultValue};
            error ->
                none
        end,
    #argo_graphql_language_input_value_definition{
        location = Location,
        name = Name,
        type = Type,
        description = OptionDescription,
        directives = OptionDirectives,
        default_value = OptionDefaultValue
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_input_value_definition{
    name = Name,
    type = Type,
    description = OptionDescription,
    directives = OptionDirectives,
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
    Formatter4 = argo_graphql_language_type:format(Formatter3, Type),
    Formatter5 =
        case OptionDefaultValue of
            {some, DefaultValue} ->
                F4_1 = Formatter4,
                F4_2 = argo_graphql_formatter:write(F4_1, " = ", []),
                F4_3 = argo_graphql_language_value_const:format(F4_2, DefaultValue),
                F4_3;
            none ->
                Formatter4
        end,
    Formatter6 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives_const:format(Formatter5, Directives);
            none ->
                Formatter5
        end,
    Formatter6.
