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
-module(argo_graphql_language_arguments_const).
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
    arguments := [argo_graphql_language_argument_const:t()]
}.
-type t() :: #argo_graphql_language_arguments_const{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> ArgumentsConst when
    Context :: context(), Location :: erl_anno:location(), ArgumentsConst :: t().
parse(_Context = #{arguments := Arguments = [#argo_graphql_language_argument_const{} | _]}, Location) ->
    #argo_graphql_language_arguments_const{
        location = Location,
        arguments = Arguments
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_arguments_const{arguments = Arguments}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "(", []),
    Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
    {_Index, Formatter4} = lists:foldl(
        fun(ArgumentConst, {Index, Formatter3_Acc1}) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
            Formatter3_Acc4 = argo_graphql_language_argument_const:format(Formatter3_Acc3, ArgumentConst),
            Formatter3_Acc5 =
                case Index =:= length(Arguments) of
                    false ->
                        argo_graphql_formatter:write(Formatter3_Acc4, ",", []);
                    true ->
                        Formatter3_Acc4
                end,
            {Index + 1, Formatter3_Acc5}
        end,
        {1, Formatter3},
        Arguments
    ),
    Formatter5 = argo_graphql_formatter:write(Formatter4, "~n", []),
    Formatter6 = argo_graphql_formatter:shift_left(Formatter5),
    Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
    Formatter8 = argo_graphql_formatter:write(Formatter7, ")", []),
    Formatter8.
