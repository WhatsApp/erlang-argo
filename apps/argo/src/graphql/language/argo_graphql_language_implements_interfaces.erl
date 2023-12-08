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
-module(argo_graphql_language_implements_interfaces).
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
    interfaces := [argo_graphql_language_named_type:t()]
}.
-type t() :: #argo_graphql_language_implements_interfaces{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> ImplementsInterfaces when
    Context :: context(), Location :: erl_anno:location(), ImplementsInterfaces :: t().
parse(_Context = #{interfaces := Interfaces = [#argo_graphql_language_named_type{} | _]}, Location) ->
    #argo_graphql_language_implements_interfaces{
        location = Location,
        interfaces = Interfaces
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_implements_interfaces{interfaces = Interfaces}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " implements ", []),
    {_Index, Formatter3} = lists:foldl(
        fun(Interface, {Index, Formatter2_Acc1}) ->
            Formatter2_Acc2 = argo_graphql_language_named_type:format(Formatter2_Acc1, Interface),
            Formatter2_Acc3 =
                case Index =:= length(Interfaces) of
                    false ->
                        argo_graphql_formatter:write(Formatter2_Acc2, " & ", []);
                    true ->
                        Formatter2_Acc2
                end,
            {Index + 1, Formatter2_Acc3}
        end,
        {1, Formatter2},
        Interfaces
    ),
    Formatter3.
