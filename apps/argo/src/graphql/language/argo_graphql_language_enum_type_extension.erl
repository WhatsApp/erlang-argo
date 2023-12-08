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
-module(argo_graphql_language_enum_type_extension).
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
    directives => argo_graphql_language_directives_const:t(),
    values => argo_graphql_language_enum_values_definition:t()
}.
-type t() :: #argo_graphql_language_enum_type_extension{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> EnumTypeExtension when
    Context :: context(), Location :: erl_anno:location(), EnumTypeExtension :: t().
parse(Context = #{name := Name}, Location) when
    is_binary(Name) andalso (is_map_key(directives, Context) orelse is_map_key(values, Context))
->
    OptionDirectives =
        case maps:find(directives, Context) of
            {ok, Directives = #argo_graphql_language_directives_const{}} ->
                {some, Directives};
            error ->
                none
        end,
    OptionValues =
        case maps:find(values, Context) of
            {ok, EnumValuesDefinition = #argo_graphql_language_enum_values_definition{}} ->
                {some, EnumValuesDefinition};
            error ->
                none
        end,
    #argo_graphql_language_enum_type_extension{
        location = Location,
        name = Name,
        directives = OptionDirectives,
        values = OptionValues
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_language_enum_type_extension{values = none}) ->
    true;
is_ambiguous(#argo_graphql_language_enum_type_extension{values = {some, _}}) ->
    false.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_enum_type_extension{
    name = Name,
    directives = OptionDirectives,
    values = OptionValues
}) ->
    Formatter2 = Formatter1,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "extend enum ~ts", [Name]),
    Formatter4 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives_const:format(Formatter3, Directives);
            none ->
                Formatter3
        end,
    Formatter5 =
        case OptionValues of
            {some, EnumValuesDefinition} ->
                argo_graphql_language_enum_values_definition:format(Formatter4, EnumValuesDefinition);
            none ->
                Formatter4
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter6.
