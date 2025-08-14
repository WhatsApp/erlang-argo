%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_directives_const).
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
    directives := [argo_graphql_language_directive_const:t()]
}.
-type t() :: #argo_graphql_language_directives_const{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse(Context, Location) -> DirectivesConst when
    Context :: context(), Location :: erl_anno:location(), DirectivesConst :: t().
parse(_Context = #{directives := Directives = [#argo_graphql_language_directive_const{} | _]}, Location) ->
    #argo_graphql_language_directives_const{
        location = Location,
        directives = Directives
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_directives_const{directives = Directives}) ->
    Formatter2 = argo_graphql_formatter:shift_right(Formatter1),
    Formatter3 = lists:foldl(
        fun(Directive, Formatter2_Acc1) ->
            Formatter2_Acc2 = argo_graphql_formatter:write(Formatter2_Acc1, "~n", []),
            Formatter2_Acc3 = argo_graphql_formatter:write_indent(Formatter2_Acc2),
            Formatter2_Acc4 = argo_graphql_language_directive_const:format(Formatter2_Acc3, Directive),
            Formatter2_Acc4
        end,
        Formatter2,
        Directives
    ),
    Formatter4 = argo_graphql_formatter:shift_left(Formatter3),
    Formatter4.
