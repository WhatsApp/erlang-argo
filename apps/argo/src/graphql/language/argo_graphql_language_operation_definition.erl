%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_operation_definition).
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
%% Instance API
-export([
    is_shorthand/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).
%% Types
-type context() :: #{
    operation := argo_graphql_language_root_operation_type_definition:operation_type(),
    selection_set := argo_graphql_language_selection_set:t(),
    name => argo_types:name(),
    variables_definition => argo_graphql_language_variables_definition:t(),
    directives => argo_graphql_language_directives:t(),
    shorthand => boolean()
}.
-type t() :: #argo_graphql_language_operation_definition{}.

-export_type([
    context/0,
    t/0
]).

%% Macros
-define(is_operation_type(T), ((T) =:= 'query' orelse (T) =:= 'mutation' orelse (T) =:= 'subscription')).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> OperationDefinition when
    Context :: context(), Location :: erl_anno:location(), OperationDefinition :: t().
parse(
    Context = #{operation := Operation, selection_set := SelectionSet = #argo_graphql_language_selection_set{}},
    Location
) when
    ?is_operation_type(Operation)
->
    OptionName =
        case maps:find(name, Context) of
            {ok, Name} when is_binary(Name) ->
                {some, Name};
            error ->
                none
        end,
    OptionVariablesDefinition =
        case maps:find(variables_definition, Context) of
            {ok, VariablesDefinition = #argo_graphql_language_variables_definition{}} ->
                {some, VariablesDefinition};
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
    Shorthand =
        case maps:find(shorthand, Context) of
            {ok, true} when OptionName =:= none andalso Operation =:= 'query' ->
                true;
            {ok, false} ->
                false;
            error ->
                false
        end,
    #argo_graphql_language_operation_definition{
        location = Location,
        operation = Operation,
        name = OptionName,
        variables_definition = OptionVariablesDefinition,
        directives = OptionDirectives,
        selection_set = SelectionSet,
        shorthand = Shorthand
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec is_shorthand(Definition) -> boolean() when Definition :: t().
is_shorthand(#argo_graphql_language_operation_definition{shorthand = Shorthand}) ->
    Shorthand =:= true.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_operation_definition{
    operation = Operation,
    name = OptionName,
    variables_definition = OptionVariablesDefinition,
    directives = OptionDirectives,
    selection_set = SelectionSet,
    shorthand = Shorthand
}) ->
    Formatter2 =
        case Shorthand of
            false ->
                F1_1 = Formatter1,
                F1_2 = argo_graphql_formatter:write(F1_1, "~ts", [Operation]),
                F1_3 =
                    case OptionName of
                        {some, Name} ->
                            argo_graphql_formatter:write(F1_2, " ~ts", [Name]);
                        none ->
                            F1_2
                    end,
                F1_4 =
                    case OptionVariablesDefinition of
                        {some, VariablesDefinition} ->
                            argo_graphql_language_variables_definition:format(F1_3, VariablesDefinition);
                        none ->
                            F1_3
                    end,
                F1_5 =
                    case OptionDirectives of
                        {some, Directives} ->
                            argo_graphql_language_directives:format(F1_4, Directives);
                        none ->
                            F1_4
                    end,
                F1_6 = argo_graphql_formatter:write(F1_5, " ", []),
                F1_6;
            true ->
                Formatter1
        end,
    Formatter3 = argo_graphql_language_selection_set:format(Formatter2, SelectionSet),
    Formatter4 = argo_graphql_formatter:write(Formatter3, "~n", []),
    Formatter4.
