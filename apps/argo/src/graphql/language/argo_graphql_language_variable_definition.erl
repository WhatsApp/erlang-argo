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
-module(argo_graphql_language_variable_definition).
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
    variable := argo_graphql_language_variable:t(),
    type := argo_graphql_language_type:t(),
    default_value => argo_graphql_language_value_const:t(),
    directives => argo_graphql_language_directives_const:t()
}.
-type t() :: #argo_graphql_language_variable_definition{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> FieldDefinition when
    Context :: context(), Location :: erl_anno:location(), FieldDefinition :: t().
parse(
    Context = #{variable := Variable = #argo_graphql_language_variable{}, type := Type = #argo_graphql_language_type{}},
    Location
) ->
    OptionDefaultValue =
        case maps:find(default_value, Context) of
            {ok, DefaultValue = #argo_graphql_language_value_const{}} ->
                {some, DefaultValue};
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
    #argo_graphql_language_variable_definition{
        location = Location,
        variable = Variable,
        type = Type,
        default_value = OptionDefaultValue,
        directives = OptionDirectives
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_variable_definition{
    variable = Variable,
    type = Type,
    default_value = OptionDefaultValue,
    directives = OptionDirectives
}) ->
    Formatter2 = argo_graphql_language_variable:format(Formatter1, Variable),
    Formatter3 = argo_graphql_formatter:write(Formatter2, ": ", []),
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
