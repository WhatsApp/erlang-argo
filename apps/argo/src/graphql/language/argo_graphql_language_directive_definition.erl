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
-module(argo_graphql_language_directive_definition).
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
    locations := [argo_graphql_language_directive_location:t()],
    description => unicode:unicode_binary(),
    arguments => argo_graphql_language_arguments_definition:t(),
    repeatable => boolean()
}.
-type t() :: #argo_graphql_language_directive_definition{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> DirectiveDefinition when
    Context :: context(), Location :: erl_anno:location(), DirectiveDefinition :: t().
parse(
    Context = #{name := Name, locations := Locations = [#argo_graphql_language_directive_location{} | _]}, Location
) when
    is_binary(Name)
->
    OptionDescription =
        case maps:find(description, Context) of
            {ok, Description} when is_binary(Description) ->
                {some, Description};
            error ->
                none
        end,
    OptionArguments =
        case maps:find(arguments, Context) of
            {ok, ArgumentsDefinition = #argo_graphql_language_arguments_definition{}} ->
                {some, ArgumentsDefinition};
            error ->
                none
        end,
    Repeatable =
        case maps:find(repeatable, Context) of
            {ok, RepeatableBoolean} when is_boolean(RepeatableBoolean) ->
                RepeatableBoolean;
            error ->
                false
        end,
    #argo_graphql_language_directive_definition{
        location = Location,
        name = Name,
        locations = Locations,
        description = OptionDescription,
        arguments = OptionArguments,
        repeatable = Repeatable
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_directive_definition{
    name = Name,
    locations = Locations,
    description = OptionDescription,
    arguments = OptionArguments,
    repeatable = Repeatable
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
    Formatter3 = argo_graphql_formatter:write(Formatter2, "directive @~ts", [Name]),
    Formatter4 =
        case OptionArguments of
            {some, ArgumentsDefinition} ->
                argo_graphql_language_arguments_definition:format(Formatter3, ArgumentsDefinition);
            none ->
                Formatter3
        end,
    Formatter5 =
        case Repeatable of
            false ->
                Formatter4;
            true ->
                argo_graphql_formatter:write(Formatter4, " repeatable", [])
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, " on ", []),
    {_Index, Formatter7} = lists:foldl(
        fun(DirectiveLocation, {Index, Formatter6_Acc1}) ->
            Formatter6_Acc2 = argo_graphql_language_directive_location:format(Formatter6_Acc1, DirectiveLocation),
            Formatter6_Acc3 =
                case Index =:= length(Locations) of
                    false ->
                        argo_graphql_formatter:write(Formatter6_Acc2, " | ", []);
                    true ->
                        Formatter6_Acc2
                end,
            {Index + 1, Formatter6_Acc3}
        end,
        {1, Formatter6},
        Locations
    ),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "~n", []),
    Formatter8.
