%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_inline_fragment).
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
    selection_set := argo_graphql_language_selection_set:t(),
    type_condition => argo_graphql_language_type_condition:t(),
    directives => argo_graphql_language_directives:t()
}.
-type t() :: #argo_graphql_language_inline_fragment{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> Field when Context :: context(), Location :: erl_anno:location(), Field :: t().
parse(Context = #{selection_set := SelectionSet = #argo_graphql_language_selection_set{}}, Location) ->
    OptionTypeCondition =
        case maps:find(type_condition, Context) of
            {ok, TypeCondition = #argo_graphql_language_type_condition{}} ->
                {some, TypeCondition};
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
    #argo_graphql_language_inline_fragment{
        location = Location,
        selection_set = SelectionSet,
        type_condition = OptionTypeCondition,
        directives = OptionDirectives
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_inline_fragment{
    selection_set = SelectionSet,
    type_condition = OptionTypeCondition,
    directives = OptionDirectives
}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "...", []),
    Formatter3 =
        case OptionTypeCondition of
            {some, TypeCondition} ->
                argo_graphql_language_type_condition:format(Formatter2, TypeCondition);
            none ->
                Formatter2
        end,
    Formatter4 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives:format(Formatter3, Directives);
            none ->
                Formatter3
        end,
    Formatter5 = argo_graphql_formatter:write(Formatter4, " ", []),
    Formatter6 = argo_graphql_language_selection_set:format(Formatter5, SelectionSet),
    Formatter6.
