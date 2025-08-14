%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_fragment_definition).
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
    name := argo_graphql_language_fragment_name:t(),
    type_condition := argo_graphql_language_type_condition:t(),
    selection_set := argo_graphql_language_selection_set:t(),
    directives => argo_graphql_language_directives:t()
}.
-type t() :: #argo_graphql_language_fragment_definition{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> Field when Context :: context(), Location :: erl_anno:location(), Field :: t().
parse(
    Context = #{
        name := FragmentName = #argo_graphql_language_fragment_name{},
        type_condition := TypeCondition = #argo_graphql_language_type_condition{},
        selection_set := SelectionSet = #argo_graphql_language_selection_set{}
    },
    Location
) ->
    OptionDirectives =
        case maps:find(directives, Context) of
            {ok, Directives = #argo_graphql_language_directives{}} ->
                {some, Directives};
            error ->
                none
        end,
    #argo_graphql_language_fragment_definition{
        location = Location,
        name = FragmentName,
        type_condition = TypeCondition,
        selection_set = SelectionSet,
        directives = OptionDirectives
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_fragment_definition{
    name = FragmentName,
    type_condition = TypeCondition,
    selection_set = SelectionSet,
    directives = OptionDirectives
}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "fragment ", []),
    Formatter3 = argo_graphql_language_fragment_name:format(Formatter2, FragmentName),
    Formatter4 = argo_graphql_language_type_condition:format(Formatter3, TypeCondition),
    Formatter5 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives:format(Formatter4, Directives);
            none ->
                Formatter4
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, " ", []),
    Formatter7 = argo_graphql_language_selection_set:format(Formatter6, SelectionSet),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "~n", []),
    Formatter8.
