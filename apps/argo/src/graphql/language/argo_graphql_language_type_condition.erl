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
-module(argo_graphql_language_type_condition).
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
    type := argo_graphql_language_named_type:t()
}.
-type t() :: #argo_graphql_language_type_condition{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> TypeCondition when
    Context :: context(), Location :: erl_anno:location(), TypeCondition :: t().
parse(_Context = #{type := Type = #argo_graphql_language_named_type{}}, Location) ->
    #argo_graphql_language_type_condition{
        location = Location,
        type = Type
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_type_condition{type = Type}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " on ", []),
    Formatter3 = argo_graphql_language_named_type:format(Formatter2, Type),
    Formatter3.
