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
-module(argo_graphql_fragment_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_language/1,
    new/3
]).
%% Instance API
-export([
    add_directive/2,
    add_selection/2,
    find_field/3,
    fold_fields/4,
    get_shape/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_fragment_definition{}.

-export_type([
    t/0
]).

%% Macros
-define(is_fragment_name(X), (is_binary(X) andalso (X) =/= <<"on">>)).
-define(is_selection(X),
    (is_record((X), argo_graphql_field) orelse is_record((X), argo_graphql_fragment_spread) orelse
        is_record((X), argo_graphql_inline_fragment))
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageFragmentDefinition) -> FragmentDefinition when
    LanguageFragmentDefinition :: argo_graphql_language_fragment_definition:t(), FragmentDefinition :: t().
from_language(#argo_graphql_language_fragment_definition{
    name = #argo_graphql_language_fragment_name{name = FragmentName},
    type_condition = #argo_graphql_language_type_condition{
        type = #argo_graphql_language_named_type{name = TypeCondition}
    },
    selection_set = LanguageSelectionSet,
    directives = LanguageOptionDirectives
}) ->
    SelectionSet = argo_graphql_selection_set:from_language(LanguageSelectionSet),
    FragmentDefinition1 = new(FragmentName, TypeCondition, SelectionSet),
    FragmentDefinition2 =
        case LanguageOptionDirectives of
            none ->
                FragmentDefinition1;
            {some, LanguageDirectives} ->
                Directives = argo_graphql_directives:from_language(LanguageDirectives),
                FragmentDefinition1#argo_graphql_fragment_definition{directives = Directives}
        end,
    FragmentDefinition2.

-compile({inline, [new/3]}).
-spec new(FragmentName, TypeCondition, SelectionSet) -> FragmentDefinition when
    FragmentName :: argo_types:name(),
    TypeCondition :: argo_types:name(),
    SelectionSet :: argo_graphql_selection_set:t(),
    FragmentDefinition :: t().
new(FragmentName, TypeCondition, SelectionSet = #argo_graphql_selection_set{}) when
    ?is_fragment_name(FragmentName) andalso is_binary(TypeCondition)
->
    #argo_graphql_fragment_definition{
        name = FragmentName,
        type_condition = TypeCondition,
        selection_set = SelectionSet,
        directives = argo_graphql_directives:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive(FragmentDefinition, Directive) -> FragmentDefinition when
    FragmentDefinition :: t(), Directive :: argo_graphql_directive:t().
add_directive(
    FragmentDefinition1 = #argo_graphql_fragment_definition{directives = Directives1},
    Directive = #argo_graphql_directive{}
) ->
    Directives2 = argo_graphql_directives:add_directive(Directives1, Directive),
    FragmentDefinition2 = FragmentDefinition1#argo_graphql_fragment_definition{directives = Directives2},
    FragmentDefinition2.

-spec add_selection(FragmentDefinition, Selection) -> FragmentDefinition when
    FragmentDefinition :: t(), Selection :: argo_graphql_selection_set:selection().
add_selection(FragmentDefinition1 = #argo_graphql_fragment_definition{selection_set = SelectionSet1}, Selection) when
    ?is_selection(Selection)
->
    SelectionSet2 = argo_graphql_selection_set:add_selection(SelectionSet1, Selection),
    FragmentDefinition2 = FragmentDefinition1#argo_graphql_fragment_definition{selection_set = SelectionSet2},
    FragmentDefinition2.

-spec find_field(FragmentDefinition, FieldAliasOrName, ExecutableDocument) -> {ok, Field} | error when
    FragmentDefinition :: t(),
    FieldAliasOrName :: argo_types:name(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Field :: argo_graphql_field:t().
find_field(
    _FragmentDefinition = #argo_graphql_fragment_definition{selection_set = SelectionSet},
    FieldAliasOrName,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_binary(FieldAliasOrName) ->
    argo_graphql_selection_set:find_field(SelectionSet, FieldAliasOrName, ExecutableDocument).

-spec fold_fields(FragmentDefinition, AccIn, Fun, ExecutableDocument) -> AccOut when
    FragmentDefinition :: t(),
    AccIn :: dynamic(),
    Fun :: argo_graphql_selection_set:fold_fields_func(AccIn, AccOut),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    AccOut :: dynamic().
fold_fields(
    _FragmentDefinition = #argo_graphql_fragment_definition{selection_set = SelectionSet},
    AccIn,
    Fun,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    argo_graphql_selection_set:fold_fields(SelectionSet, AccIn, Fun, ExecutableDocument).

-spec get_shape(FragmentDefinition, ExecutableDocument) -> Shape when
    FragmentDefinition :: t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Shape :: argo_graphql_selection_set:shape().
get_shape(
    _FragmentDefinition = #argo_graphql_fragment_definition{selection_set = SelectionSet},
    ExecutableDocument = #argo_graphql_executable_document{}
) ->
    argo_graphql_selection_set:get_shape(SelectionSet, ExecutableDocument).

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_fragment_definition{
    name = FragmentName,
    type_condition = TypeCondition,
    selection_set = SelectionSet,
    directives = Directives
}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "fragment ~ts on ~ts", [FragmentName, TypeCondition]),
    Formatter3 = argo_graphql_directives:format(Formatter2, Directives),
    Formatter4 = argo_graphql_selection_set:format(Formatter3, SelectionSet),
    Formatter5 = argo_graphql_formatter:write(Formatter4, "~n", []),
    Formatter5.
