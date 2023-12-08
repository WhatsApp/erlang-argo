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
-module(argo_graphql_language_field).
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
    'alias' => argo_types:name(),
    arguments => argo_graphql_language_arguments:t(),
    directives => argo_graphql_language_directives:t(),
    selection_set => argo_graphql_language_selection_set:t()
}.
-type t() :: #argo_graphql_language_field{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> Field when Context :: context(), Location :: erl_anno:location(), Field :: t().
parse(Context = #{name := Name}, Location) when is_binary(Name) ->
    OptionAlias =
        case maps:find('alias', Context) of
            {ok, Alias} when is_binary(Alias) ->
                {some, Alias};
            error ->
                none
        end,
    OptionArguments =
        case maps:find(arguments, Context) of
            {ok, Arguments = #argo_graphql_language_arguments{}} ->
                {some, Arguments};
            error ->
                none
        end,
    OptionDirectives =
        case maps:find(directives, Context) of
            {ok, Directives = #argo_graphql_language_directives{}} ->
                {some, Directives};
            error ->
                none
        end,
    OptionSelectionSet =
        case maps:find(selection_set, Context) of
            {ok, SelectionSet = #argo_graphql_language_selection_set{}} ->
                {some, SelectionSet};
            error ->
                none
        end,
    #argo_graphql_language_field{
        location = Location,
        name = Name,
        'alias' = OptionAlias,
        arguments = OptionArguments,
        directives = OptionDirectives,
        selection_set = OptionSelectionSet
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_field{
    name = Name,
    'alias' = OptionAlias,
    arguments = OptionArguments,
    directives = OptionDirectives,
    selection_set = OptionSelectionSet
}) ->
    Formatter2 =
        case OptionAlias of
            {some, Alias} ->
                argo_graphql_formatter:write(Formatter1, "~ts: ", [Alias]);
            none ->
                Formatter1
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~ts", [Name]),
    Formatter4 =
        case OptionArguments of
            {some, Arguments} ->
                argo_graphql_language_arguments:format(Formatter3, Arguments);
            none ->
                Formatter3
        end,
    Formatter5 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives:format(Formatter4, Directives);
            none ->
                Formatter4
        end,
    Formatter6 =
        case OptionSelectionSet of
            {some, SelectionSet} ->
                F5_1 = Formatter5,
                F5_2 = argo_graphql_formatter:write(F5_1, " ", []),
                F5_3 = argo_graphql_language_selection_set:format(F5_2, SelectionSet),
                F5_3;
            none ->
                Formatter5
        end,
    Formatter6.
