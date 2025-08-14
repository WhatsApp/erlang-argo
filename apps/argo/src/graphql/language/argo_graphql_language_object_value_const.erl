%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_object_value_const).
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
    fields := [argo_graphql_language_object_field_const:t()]
}.
-type t() :: #argo_graphql_language_object_value_const{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> ObjectValueConst when
    Context :: context(), Location :: erl_anno:location(), ObjectValueConst :: t().
parse(_Context = #{fields := Fields}, Location) when is_list(Fields) ->
    #argo_graphql_language_object_value_const{
        location = Location,
        fields = Fields
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_object_value_const{fields = []}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "{}", []),
    Formatter2;
format(Formatter1, #argo_graphql_language_object_value_const{fields = Fields}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "{", []),
    Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
    {_Index, Formatter4} = lists:foldl(
        fun(ObjectFieldConst, {Index, Formatter3_Acc1}) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
            Formatter3_Acc4 = argo_graphql_language_object_field_const:format(Formatter3_Acc3, ObjectFieldConst),
            Formatter3_Acc5 =
                case Index =:= length(Fields) of
                    false ->
                        argo_graphql_formatter:write(Formatter3_Acc4, ",", []);
                    true ->
                        Formatter3_Acc4
                end,
            {Index + 1, Formatter3_Acc5}
        end,
        {1, Formatter3},
        Fields
    ),
    Formatter5 = argo_graphql_formatter:write(Formatter4, "~n", []),
    Formatter6 = argo_graphql_formatter:shift_left(Formatter5),
    Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "}", []),
    Formatter8.
