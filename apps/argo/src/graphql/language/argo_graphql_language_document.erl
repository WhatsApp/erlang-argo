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
-module(argo_graphql_language_document).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% API
-export([
    from_tokens/1,
    parse/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type context() :: #{
    definitions := [argo_graphql_language_definition:t()]
}.
-type t() :: #argo_graphql_language_document{}.

-export_type([
    context/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-compile({inline, [from_tokens/1]}).
-spec from_tokens(Tokens) -> {ok, LanguageDocument} | {error, Reason} when
    Tokens :: dynamic(), LanguageDocument :: t(), Reason :: dynamic().
from_tokens(Tokens) ->
    argo_types:dynamic_cast(argo_graphql_language_parser:parse(Tokens)).

-spec parse(Context, Location) -> Document when Context :: context(), Location :: erl_anno:location(), Document :: t().
parse(_Context = #{definitions := Definitions = [#argo_graphql_language_definition{} | _]}, Location) ->
    #argo_graphql_language_document{
        location = Location,
        definitions = Definitions
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_document{definitions = Definitions}) ->
    Formatter2 = lists:foldl(
        fun(Definition = #argo_graphql_language_definition{}, Formatter1Acc1) ->
            Formatter1Acc2 = argo_graphql_language_definition:format(Formatter1Acc1, Definition),
            Formatter1Acc3 = argo_graphql_formatter:write(Formatter1Acc2, "~n", []),
            Formatter1Acc3
        end,
        Formatter1,
        Definitions
    ),
    Formatter2.
