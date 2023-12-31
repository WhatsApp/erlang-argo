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
-module(argo_graphql_fragment_spread).
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
    add_directive/2,
    find_field/3,
    fold_fields/4,
    get_shape/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type t() :: #argo_graphql_fragment_spread{}.

-export_type([
    t/0
]).

%% Macros
-define(is_fragment_name(X), (is_binary(X) andalso (X) =/= <<"on">>)).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageFragmentSpread) -> FragmentSpread when
    LanguageFragmentSpread :: argo_graphql_language_fragment_spread:t(), FragmentSpread :: t().
from_language(#argo_graphql_language_fragment_spread{
    name = #argo_graphql_language_fragment_name{name = FragmentName}, directives = LanguageOptionDirectives
}) ->
    FragmentSpread1 = new(FragmentName),
    FragmentSpread2 =
        case LanguageOptionDirectives of
            none ->
                FragmentSpread1;
            {some, LanguageDirectives} ->
                Directives = argo_graphql_directives:from_language(LanguageDirectives),
                FragmentSpread1#argo_graphql_fragment_spread{directives = Directives}
        end,
    FragmentSpread2.

-compile({inline, [new/1]}).
-spec new(FragmentName) -> FragmentSpread when FragmentName :: argo_types:name(), FragmentSpread :: t().
new(FragmentName) when ?is_fragment_name(FragmentName) ->
    #argo_graphql_fragment_spread{name = FragmentName, directives = argo_graphql_directives:new()}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive(FragmentSpread, Directive) -> FragmentSpread when
    FragmentSpread :: t(), Directive :: argo_graphql_directive:t().
add_directive(
    FragmentSpread1 = #argo_graphql_fragment_spread{directives = Directives1}, Directive = #argo_graphql_directive{}
) ->
    Directives2 = argo_graphql_directives:add_directive(Directives1, Directive),
    FragmentSpread2 = FragmentSpread1#argo_graphql_fragment_spread{directives = Directives2},
    FragmentSpread2.

-spec find_field(FragmentSpread, FieldAliasOrName, ExecutableDocument) -> {ok, Field} | error when
    FragmentSpread :: t(),
    FieldAliasOrName :: argo_types:name(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Field :: argo_graphql_field:t().
find_field(
    _FragmentSpread = #argo_graphql_fragment_spread{name = FragmentName},
    FieldAliasOrName,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_binary(FieldAliasOrName) ->
    FragmentDefinition = argo_graphql_executable_document:get_fragment_definition(ExecutableDocument, FragmentName),
    argo_graphql_fragment_definition:find_field(FragmentDefinition, FieldAliasOrName, ExecutableDocument).

-spec fold_fields(FragmentSpread, AccIn, Fun, ExecutableDocument) -> AccOut when
    FragmentSpread :: t(),
    AccIn :: dynamic(),
    Fun :: argo_graphql_selection_set:fold_fields_func(AccIn, AccOut),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    AccOut :: dynamic().
fold_fields(
    _FragmentSpread = #argo_graphql_fragment_spread{name = FragmentName},
    AccIn,
    Fun,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    FragmentDefinition = argo_graphql_executable_document:get_fragment_definition(ExecutableDocument, FragmentName),
    argo_graphql_fragment_definition:fold_fields(FragmentDefinition, AccIn, Fun, ExecutableDocument).

-spec get_shape(FragmentSpread, ExecutableDocument) -> Shape when
    FragmentSpread :: t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Shape :: argo_graphql_selection_set:shape().
get_shape(
    _FragmentSpread = #argo_graphql_fragment_spread{name = FragmentName},
    ExecutableDocument = #argo_graphql_executable_document{}
) ->
    FragmentDefinition = argo_graphql_executable_document:get_fragment_definition(ExecutableDocument, FragmentName),
    argo_graphql_fragment_definition:get_shape(FragmentDefinition, ExecutableDocument).

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_fragment_spread{name = FragmentName, directives = Directives}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "...~ts", [FragmentName]),
    Formatter3 = argo_graphql_directives:format(Formatter2, Directives),
    Formatter3.
