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
-module(argo_graphql_selection_set).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/0
]).
%% Instance API
-export([
    add_field/2,
    add_fragment_spread/2,
    add_inline_fragment/2,
    add_selection/2
]).
%% argo_graphql_display callbacks
-export([
    format/2,
    format_shorthand/2
]).

%% Types
-type selection() ::
    argo_graphql_field:t()
    | argo_graphql_fragment_spread:t()
    | argo_graphql_inline_fragment:t().
-type t() :: #argo_graphql_selection_set{}.

-export_type([
    selection/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageSelectionSet) -> SelectionSet when
    LanguageSelectionSet :: argo_graphql_language_selection_set:t(), SelectionSet :: t().
from_language(#argo_graphql_language_selection_set{selections = LanguageSelectionList}) ->
    SelectionSet1 = new(),
    SelectionSet2 = lists:foldl(
        fun(LanguageSelection, SelectionSet1_Acc1) ->
            case LanguageSelection of
                #argo_graphql_language_selection{inner = LanguageField = #argo_graphql_language_field{}} ->
                    Field = argo_graphql_field:from_language(LanguageField),
                    add_field(SelectionSet1_Acc1, Field);
                #argo_graphql_language_selection{
                    inner = LanguageFragmentSpread = #argo_graphql_language_fragment_spread{}
                } ->
                    FragmentSpread = argo_graphql_fragment_spread:from_language(LanguageFragmentSpread),
                    add_fragment_spread(SelectionSet1_Acc1, FragmentSpread);
                #argo_graphql_language_selection{
                    inner = LanguageInlineFragment = #argo_graphql_language_inline_fragment{}
                } ->
                    InlineFragment = argo_graphql_inline_fragment:from_language(LanguageInlineFragment),
                    add_inline_fragment(SelectionSet1_Acc1, InlineFragment)
            end
        end,
        SelectionSet1,
        LanguageSelectionList
    ),
    SelectionSet2.

-compile({inline, [new/0]}).
-spec new() -> SelectionSet when SelectionSet :: t().
new() ->
    #argo_graphql_selection_set{selections = []}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-compile({inline, [add_field/2]}).
-spec add_field(SelectionSet, Field) -> SelectionSet when
    SelectionSet :: t(), Field :: argo_graphql_field:t().
add_field(
    SelectionSet1 = #argo_graphql_selection_set{selections = Selections1},
    Field = #argo_graphql_field{}
) ->
    Selections2 = Selections1 ++ [Field],
    SelectionSet2 = SelectionSet1#argo_graphql_selection_set{selections = Selections2},
    SelectionSet2.

-compile({inline, [add_fragment_spread/2]}).
-spec add_fragment_spread(SelectionSet, FragmentSpread) -> SelectionSet when
    SelectionSet :: t(), FragmentSpread :: argo_graphql_fragment_spread:t().
add_fragment_spread(
    SelectionSet1 = #argo_graphql_selection_set{selections = Selections1},
    FragmentSpread = #argo_graphql_fragment_spread{}
) ->
    Selections2 = Selections1 ++ [FragmentSpread],
    SelectionSet2 = SelectionSet1#argo_graphql_selection_set{selections = Selections2},
    SelectionSet2.

-compile({inline, [add_inline_fragment/2]}).
-spec add_inline_fragment(SelectionSet, InlineFragment) -> SelectionSet when
    SelectionSet :: t(), InlineFragment :: argo_graphql_inline_fragment:t().
add_inline_fragment(
    SelectionSet1 = #argo_graphql_selection_set{selections = Selections1},
    InlineFragment = #argo_graphql_inline_fragment{}
) ->
    Selections2 = Selections1 ++ [InlineFragment],
    SelectionSet2 = SelectionSet1#argo_graphql_selection_set{selections = Selections2},
    SelectionSet2.

-spec add_selection(SelectionSet, Selection) -> SelectionSet when
    SelectionSet :: t(), Selection :: selection().
add_selection(SelectionSet1 = #argo_graphql_selection_set{}, Field = #argo_graphql_field{}) ->
    add_field(SelectionSet1, Field);
add_selection(SelectionSet1 = #argo_graphql_selection_set{}, FragmentSpread = #argo_graphql_fragment_spread{}) ->
    add_fragment_spread(SelectionSet1, FragmentSpread);
add_selection(SelectionSet1 = #argo_graphql_selection_set{}, InlineFragment = #argo_graphql_inline_fragment{}) ->
    add_inline_fragment(SelectionSet1, InlineFragment).

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_selection_set{selections = []}) ->
    Formatter1;
format(Formatter1, SelectionSet = #argo_graphql_selection_set{selections = [_ | _]}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " ", []),
    Formatter3 = format_shorthand(Formatter2, SelectionSet),
    Formatter3.

-spec format_shorthand(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format_shorthand(Formatter1, #argo_graphql_selection_set{selections = SelectionList}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "{", []),
    Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
    Formatter4 = lists:foldl(
        fun(Selection, Formatter3_Acc1) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
            Formatter3_Acc4 =
                case Selection of
                    #argo_graphql_field{} ->
                        argo_graphql_field:format(Formatter3_Acc3, Selection);
                    #argo_graphql_fragment_spread{} ->
                        argo_graphql_fragment_spread:format(Formatter3_Acc3, Selection);
                    #argo_graphql_inline_fragment{} ->
                        argo_graphql_inline_fragment:format(Formatter3_Acc3, Selection)
                end,
            Formatter3_Acc4
        end,
        Formatter3,
        SelectionList
    ),
    Formatter5 = argo_graphql_formatter:shift_left(Formatter4),
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "}", []),
    Formatter8.
