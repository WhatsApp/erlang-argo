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
-module(argo_graphql_argument_const).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_argument_const{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageArgumentConst) -> ArgumentConst when
    LanguageArgumentConst :: argo_graphql_language_argument_const:t(), ArgumentConst :: t().
from_language(#argo_graphql_language_argument_const{name = Name, value = LanguageValueConst}) ->
    ValueConst = argo_graphql_value_const:from_language(LanguageValueConst),
    new(Name, ValueConst).

-compile({inline, [new/2]}).
-spec new(Name, ValueConst) -> ArgumentConst when
    Name :: argo_types:name(), ValueConst :: argo_graphql_value_const:t(), ArgumentConst :: t().
new(Name, ValueConst = #argo_graphql_value_const{}) when is_binary(Name) ->
    #argo_graphql_argument_const{
        name = Name,
        value = ValueConst
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_argument_const{name = Name, value = ValueConst}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "~ts: ", [Name]),
    Formatter3 = argo_graphql_value_const:format(Formatter2, ValueConst),
    Formatter3.
