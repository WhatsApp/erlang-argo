%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_input_fields_definition).
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
    inputs := [argo_graphql_language_input_value_definition:t()]
}.
-type t() :: #argo_graphql_language_input_fields_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> FieldsDefinition when
    Context :: context(), Location :: erl_anno:location(), FieldsDefinition :: t().
parse(_Context = #{inputs := Inputs = []}, Location) ->
    #argo_graphql_language_input_fields_definition{
        location = Location,
        inputs = Inputs
    };
parse(_Context = #{inputs := Inputs = [#argo_graphql_language_input_value_definition{} | _]}, Location) ->
    #argo_graphql_language_input_fields_definition{
        location = Location,
        inputs = Inputs
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_input_fields_definition{inputs = []}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " {}", []),
    Formatter2;
format(Formatter1, #argo_graphql_language_input_fields_definition{inputs = Inputs}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " {", []),
    Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
    Formatter4 = lists:foldl(
        fun(InputValueDefinition, Formatter3_Acc1) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
            Formatter3_Acc4 = argo_graphql_language_input_value_definition:format(
                Formatter3_Acc3, InputValueDefinition
            ),
            Formatter3_Acc4
        end,
        Formatter3,
        Inputs
    ),
    Formatter5 = argo_graphql_formatter:shift_left(Formatter4),
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "}", []),
    Formatter8.
