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
-module(argo_graphql_inline_fragment).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/1
]).
%% Instance API
-export([
    add_directive/2,
    add_selection/2,
    set_type_condition/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_inline_fragment{}.

-export_type([
    t/0
]).

%% Macros
-define(is_selection(X),
    (is_record((X), argo_graphql_field) orelse is_record((X), argo_graphql_fragment_spread) orelse
        is_record((X), argo_graphql_inline_fragment))
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageInlineFragment) -> InlineFragment when
    LanguageInlineFragment :: argo_graphql_language_inline_fragment:t(), InlineFragment :: t().
from_language(#argo_graphql_language_inline_fragment{
    selection_set = LanguageSelectionSet,
    type_condition = LanguageOptionTypeCondition,
    directives = LanguageOptionDirectives
}) ->
    SelectionSet = argo_graphql_selection_set:from_language(LanguageSelectionSet),
    InlineFragment1 = new(SelectionSet),
    InlineFragment2 =
        case LanguageOptionDirectives of
            none ->
                InlineFragment1;
            {some, LanguageDirectives} ->
                Directives = argo_graphql_directives:from_language(LanguageDirectives),
                InlineFragment1#argo_graphql_inline_fragment{directives = Directives}
        end,
    InlineFragment3 =
        case LanguageOptionTypeCondition of
            none ->
                InlineFragment2;
            {some, #argo_graphql_language_type_condition{
                type = #argo_graphql_language_named_type{name = TypeCondition}
            }} ->
                set_type_condition(InlineFragment2, {some, TypeCondition})
        end,
    InlineFragment3.

-compile({inline, [new/1]}).
-spec new(SelectionSet) -> InlineFragment when SelectionSet :: argo_graphql_selection_set:t(), InlineFragment :: t().
new(SelectionSet = #argo_graphql_selection_set{}) ->
    #argo_graphql_inline_fragment{
        selection_set = SelectionSet, type_condition = none, directives = argo_graphql_directives:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive(InlineFragment, Directive) -> InlineFragment when
    InlineFragment :: t(), Directive :: argo_graphql_directive:t().
add_directive(
    InlineFragment1 = #argo_graphql_inline_fragment{directives = Directives1}, Directive = #argo_graphql_directive{}
) ->
    Directives2 = argo_graphql_directives:add_directive(Directives1, Directive),
    InlineFragment2 = InlineFragment1#argo_graphql_inline_fragment{directives = Directives2},
    InlineFragment2.

-spec add_selection(InlineFragment, Selection) -> InlineFragment when
    InlineFragment :: t(), Selection :: argo_graphql_selection_set:selection().
add_selection(InlineFragment1 = #argo_graphql_inline_fragment{selection_set = SelectionSet1}, Selection) when
    ?is_selection(Selection)
->
    SelectionSet2 = argo_graphql_selection_set:add_selection(SelectionSet1, Selection),
    InlineFragment2 = InlineFragment1#argo_graphql_inline_fragment{selection_set = SelectionSet2},
    InlineFragment2.

-spec set_type_condition(InlineFragment, OptionTypeCondition) -> InlineFragment when
    InlineFragment :: t(), OptionTypeCondition :: none | {some, unicode:unicode_binary()}.
set_type_condition(InlineFragment1 = #argo_graphql_inline_fragment{}, OptionTypeCondition) when
    ?is_option_binary(OptionTypeCondition)
->
    InlineFragment2 = InlineFragment1#argo_graphql_inline_fragment{type_condition = OptionTypeCondition},
    InlineFragment2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_inline_fragment{
    selection_set = SelectionSet, type_condition = OptionTypeCondition, directives = Directives
}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "...", []),
    Formatter3 =
        case OptionTypeCondition of
            {some, TypeCondition} ->
                argo_graphql_formatter:write(Formatter2, " on ~ts", [TypeCondition]);
            none ->
                Formatter2
        end,
    Formatter4 = argo_graphql_directives:format(Formatter3, Directives),
    Formatter5 = argo_graphql_formatter:write(Formatter4, " ", []),
    Formatter6 = argo_graphql_selection_set:format(Formatter5, SelectionSet),
    Formatter6.
