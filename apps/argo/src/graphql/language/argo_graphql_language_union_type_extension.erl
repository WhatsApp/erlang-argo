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
-module(argo_graphql_language_union_type_extension).
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
    directives => argo_graphql_language_directives_const:t(),
    types => [argo_graphql_language_named_type:t()]
}.
-type t() :: #argo_graphql_language_union_type_extension{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> UnionTypeExtension when
    Context :: context(), Location :: erl_anno:location(), UnionTypeExtension :: t().
parse(Context = #{name := Name}, Location) when
    is_binary(Name) andalso (is_map_key(directives, Context) orelse is_map_key(types, Context))
->
    OptionDirectives =
        case maps:find(directives, Context) of
            {ok, Directives = #argo_graphql_language_directives_const{}} ->
                {some, Directives};
            error ->
                none
        end,
    OptionTypes =
        case maps:find(types, Context) of
            {ok, UnionMemberTypes = [#argo_graphql_language_named_type{} | _]} ->
                {some, UnionMemberTypes};
            error ->
                none
        end,
    #argo_graphql_language_union_type_extension{
        location = Location,
        name = Name,
        directives = OptionDirectives,
        types = OptionTypes
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_union_type_extension{
    name = Name,
    directives = OptionDirectives,
    types = OptionTypes
}) ->
    Formatter2 = Formatter1,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "extend union ~ts", [Name]),
    Formatter4 =
        case OptionDirectives of
            {some, Directives} ->
                argo_graphql_language_directives_const:format(Formatter3, Directives);
            none ->
                Formatter3
        end,
    Formatter5 =
        case OptionTypes of
            {some, UnionMemberTypes} ->
                F4_1 = Formatter4,
                F4_2 = argo_graphql_formatter:write(F4_1, " = ", []),
                {_Index, F4_3} = lists:foldl(
                    fun(UnionMemberType, {Index, F4_2_Acc1}) ->
                        F4_2_Acc2 = argo_graphql_language_named_type:format(F4_2_Acc1, UnionMemberType),
                        F4_2_Acc3 =
                            case Index =:= length(UnionMemberTypes) of
                                false ->
                                    argo_graphql_formatter:write(F4_2_Acc2, " | ", []);
                                true ->
                                    F4_2_Acc2
                            end,
                        {Index + 1, F4_2_Acc3}
                    end,
                    {1, F4_2},
                    UnionMemberTypes
                ),
                F4_3;
            none ->
                Formatter4
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter6.
