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
-module(argo_graphql_field).
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
    add_argument/2,
    add_directive/2,
    add_selection/2,
    find_field/3,
    fold_fields/4,
    get_shape/2,
    set_alias/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_field{}.

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

-spec from_language(LanguageField) -> Field when LanguageField :: argo_graphql_language_field:t(), Field :: t().
from_language(#argo_graphql_language_field{
    name = Name,
    'alias' = LanguageOptionAlias,
    arguments = LanguageOptionArguments,
    directives = LanguageOptionDirectives,
    selection_set = LanguageOptionSelectionSet
}) ->
    Field1 = new(Name),
    Field2 =
        case LanguageOptionDirectives of
            none ->
                Field1;
            {some, LanguageDirectives} ->
                Directives = argo_graphql_directives:from_language(LanguageDirectives),
                Field1#argo_graphql_field{directives = Directives}
        end,
    Field3 =
        case LanguageOptionAlias of
            none ->
                Field2;
            {some, Alias} when is_binary(Alias) ->
                set_alias(Field2, {some, Alias})
        end,
    Field4 =
        case LanguageOptionArguments of
            none ->
                Field3;
            {some, LanguageArguments} ->
                Arguments = argo_graphql_arguments:from_language(LanguageArguments),
                Field3#argo_graphql_field{arguments = Arguments}
        end,
    Field5 =
        case LanguageOptionSelectionSet of
            none ->
                Field4;
            {some, LanguageSelectionSet} ->
                SelectionSet = argo_graphql_selection_set:from_language(LanguageSelectionSet),
                Field4#argo_graphql_field{selection_set = SelectionSet}
        end,
    Field5.

-compile({inline, [new/1]}).
-spec new(Name) -> Field when Name :: argo_types:name(), Field :: t().
new(Name) when is_binary(Name) ->
    #argo_graphql_field{
        name = Name,
        'alias' = none,
        arguments = argo_graphql_arguments:new(),
        directives = argo_graphql_directives:new(),
        selection_set = argo_graphql_selection_set:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_argument(Field, Argument) -> Field when Field :: t(), Argument :: argo_graphql_argument:t().
add_argument(Field1 = #argo_graphql_field{arguments = Arguments1}, Argument = #argo_graphql_argument{}) ->
    Arguments2 = argo_graphql_arguments:add_argument(Arguments1, Argument),
    Field2 = Field1#argo_graphql_field{arguments = Arguments2},
    Field2.

-spec add_directive(Field, Directive) -> Field when Field :: t(), Directive :: argo_graphql_directive:t().
add_directive(Field1 = #argo_graphql_field{directives = Directives1}, Directive = #argo_graphql_directive{}) ->
    Directives2 = argo_graphql_directives:add_directive(Directives1, Directive),
    Field2 = Field1#argo_graphql_field{directives = Directives2},
    Field2.

-spec add_selection(Field, Selection) -> Field when
    Field :: t(), Selection :: argo_graphql_selection_set:selection().
add_selection(Field1 = #argo_graphql_field{selection_set = SelectionSet1}, Selection) when ?is_selection(Selection) ->
    SelectionSet2 = argo_graphql_selection_set:add_selection(SelectionSet1, Selection),
    Field2 = Field1#argo_graphql_field{selection_set = SelectionSet2},
    Field2.

-spec find_field(Field, FieldAliasOrName, ExecutableDocument) -> {ok, Field} | error when
    Field :: t(),
    FieldAliasOrName :: argo_types:name(),
    ExecutableDocument :: argo_graphql_executable_document:t().
find_field(
    _Field = #argo_graphql_field{selection_set = SelectionSet},
    FieldAliasOrName,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_binary(FieldAliasOrName) ->
    argo_graphql_selection_set:find_field(SelectionSet, FieldAliasOrName, ExecutableDocument).

-spec fold_fields(Field, AccIn, Fun, ExecutableDocument) -> AccOut when
    Field :: t(),
    AccIn :: dynamic(),
    Fun :: argo_graphql_selection_set:fold_fields_func(AccIn, AccOut),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    AccOut :: dynamic().
fold_fields(
    _Field = #argo_graphql_field{selection_set = SelectionSet},
    AccIn,
    Fun,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    argo_graphql_selection_set:fold_fields(SelectionSet, AccIn, Fun, ExecutableDocument).

-spec get_shape(Field, ExecutableDocument) -> Shape when
    Field :: t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Shape :: argo_graphql_selection_set:shape().
get_shape(
    _Field = #argo_graphql_field{selection_set = SelectionSet}, ExecutableDocument = #argo_graphql_executable_document{}
) ->
    argo_graphql_selection_set:get_shape(SelectionSet, ExecutableDocument).

-spec set_alias(Field, OptionAlias) -> Field when
    Field :: t(), OptionAlias :: none | {some, unicode:unicode_binary()}.
set_alias(Field1 = #argo_graphql_field{}, OptionAlias) when
    ?is_option_binary(OptionAlias)
->
    Field2 = Field1#argo_graphql_field{'alias' = OptionAlias},
    Field2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_field{
    name = Name, 'alias' = OptionAlias, arguments = Arguments, directives = Directives, selection_set = SelectionSet
}) ->
    Formatter2 =
        case OptionAlias of
            {some, Alias} ->
                argo_graphql_formatter:write(Formatter1, "~ts: ", [Alias]);
            none ->
                Formatter1
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~ts", [Name]),
    Formatter4 = argo_graphql_arguments:format(Formatter3, Arguments),
    Formatter5 = argo_graphql_directives:format(Formatter4, Directives),
    Formatter6 = argo_graphql_selection_set:format(Formatter5, SelectionSet),
    Formatter6.
