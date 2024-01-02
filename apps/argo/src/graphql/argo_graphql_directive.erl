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
-module(argo_graphql_directive).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/1
]).
%% Instance API
-export([
    add_argument/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_directive{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageDirective) -> Directive when
    LanguageDirective :: argo_graphql_language_directive:t(), Directive :: t().
from_language(#argo_graphql_language_directive{name = Name, arguments = OptionLanguageArguments}) ->
    Directive1 = new(Name),
    Directive2 =
        case OptionLanguageArguments of
            none ->
                Directive1;
            {some, LanguageArguments} ->
                Arguments = argo_graphql_arguments:from_language(LanguageArguments),
                Directive1#argo_graphql_directive{arguments = Arguments}
        end,
    Directive2.

-compile({inline, [new/1]}).
-spec new(Name) -> Directive when Name :: argo_types:name(), Directive :: t().
new(Name) when is_binary(Name) ->
    #argo_graphql_directive{
        name = Name,
        arguments = argo_graphql_arguments:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_argument(Directives, Argument) -> Directives when Directives :: t(), Argument :: argo_graphql_argument:t().
add_argument(Directives1 = #argo_graphql_directive{arguments = Arguments1}, Argument = #argo_graphql_argument{}) ->
    Arguments2 = argo_graphql_arguments:add_argument(Arguments1, Argument),
    Directives2 = Directives1#argo_graphql_directive{arguments = Arguments2},
    Directives2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_directive{name = Name, arguments = Arguments}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "@~ts", [Name]),
    Formatter3 = argo_graphql_arguments:format(Formatter2, Arguments),
    Formatter3.
