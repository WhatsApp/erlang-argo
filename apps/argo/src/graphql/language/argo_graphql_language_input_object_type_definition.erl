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
-module(argo_graphql_language_input_object_type_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% API
-export([
    parse/2
]).
%% Instance API
-export([
    is_ambiguous/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type context() :: #{
    name := argo_types:name(),
    description => unicode:unicode_binary(),
    directives => argo_graphql_language_directives_const:t(),
    fields => argo_graphql_language_input_fields_definition:t()
}.
-type t() :: #argo_graphql_language_input_object_type_definition{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> InputObjectTypeDefinition when
    Context :: context(), Location :: erl_anno:location(), InputObjectTypeDefinition :: t().
parse(Context = #{name := Name}, Location) when is_binary(Name) ->
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
    OptionFields =
        case maps:find(fields, Context) of
            {ok, InputFieldsDefinition = #argo_graphql_language_input_fields_definition{}} ->
                {some, InputFieldsDefinition};
            error ->
                none
        end,
    #argo_graphql_language_input_object_type_definition{
        location = Location,
        name = Name,
        description = OptionDescription,
        directives = OptionDirectives,
        fields = OptionFields
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_language_input_object_type_definition{fields = none}) ->
    true;
is_ambiguous(#argo_graphql_language_input_object_type_definition{fields = {some, _}}) ->
    false.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_input_object_type_definition{
    name = Name,
    description = OptionDescription,
    directives = OptionDirectives,
    fields = OptionFields
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
    Formatter3 = argo_graphql_formatter:write(Formatter2, "input ~ts", [Name]),
    Formatter4 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives_const:format(Formatter3, Directives);
            none ->
                Formatter3
        end,
    Formatter5 =
        case OptionFields of
            {some, InputFieldsDefinition} ->
                argo_graphql_language_input_fields_definition:format(Formatter4, InputFieldsDefinition);
            none ->
                Formatter4
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter6.
