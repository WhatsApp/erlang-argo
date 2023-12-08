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
-module(argo_graphql_language_fields_definition).
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
    fields := [argo_graphql_language_field_definition:t()]
}.
-type t() :: #argo_graphql_language_fields_definition{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> FieldsDefinition when
    Context :: context(), Location :: erl_anno:location(), FieldsDefinition :: t().
parse(_Context = #{fields := Fields = []}, Location) ->
    #argo_graphql_language_fields_definition{
        location = Location,
        fields = Fields
    };
parse(_Context = #{fields := Fields = [#argo_graphql_language_field_definition{} | _]}, Location) ->
    #argo_graphql_language_fields_definition{
        location = Location,
        fields = Fields
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_fields_definition{fields = []}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " {}", []),
    Formatter2;
format(Formatter1, #argo_graphql_language_fields_definition{fields = Fields}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " {", []),
    Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
    Formatter4 = lists:foldl(
        fun(FieldDefinition, Formatter3_Acc1) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
            Formatter3_Acc4 = argo_graphql_language_field_definition:format(Formatter3_Acc3, FieldDefinition),
            Formatter3_Acc4
        end,
        Formatter3,
        Fields
    ),
    Formatter5 = argo_graphql_formatter:shift_left(Formatter4),
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "}", []),
    Formatter8.
