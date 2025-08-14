%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_directive_const).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
    add_argument_const/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_directive_const{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageDirectiveConst) -> DirectiveConst when
    LanguageDirectiveConst :: argo_graphql_language_directive_const:t(), DirectiveConst :: t().
from_language(#argo_graphql_language_directive_const{name = Name, arguments = OptionLanguageArgumentsConst}) ->
    DirectiveConst1 = new(Name),
    DirectiveConst2 =
        case OptionLanguageArgumentsConst of
            none ->
                DirectiveConst1;
            {some, LanguageArgumentsConst} ->
                ArgumentsConst = argo_graphql_arguments_const:from_language(LanguageArgumentsConst),
                DirectiveConst1#argo_graphql_directive_const{arguments = ArgumentsConst}
        end,
    DirectiveConst2.

-compile({inline, [new/1]}).
-spec new(Name) -> DirectiveConst when Name :: argo_types:name(), DirectiveConst :: t().
new(Name) when is_binary(Name) ->
    #argo_graphql_directive_const{
        name = Name,
        arguments = argo_graphql_arguments_const:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_argument_const(DirectivesConst, ArgumentConst) -> DirectivesConst when
    DirectivesConst :: t(), ArgumentConst :: argo_graphql_argument_const:t().
add_argument_const(
    DirectivesConst1 = #argo_graphql_directive_const{arguments = ArgumentsConst1},
    ArgumentConst = #argo_graphql_argument_const{}
) ->
    ArgumentsConst2 = argo_graphql_arguments_const:add_argument_const(ArgumentsConst1, ArgumentConst),
    DirectivesConst2 = DirectivesConst1#argo_graphql_directive_const{arguments = ArgumentsConst2},
    DirectivesConst2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_directive_const{name = Name, arguments = ArgumentsConst}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "@~ts", [Name]),
    Formatter3 = argo_graphql_arguments_const:format(Formatter2, ArgumentsConst),
    Formatter3.
