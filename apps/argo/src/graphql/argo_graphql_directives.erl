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
-module(argo_graphql_directives).
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
    add_directive/2,
    find_directive_non_repeatable/2,
    find_directive_repeatable/2,
    get_directive_list/2,
    get_directive_non_repeatable/2,
    get_directive_repeatable/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_graphql_directives{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageDirectives) -> Directives when
    LanguageDirectives :: argo_graphql_language_directives:t(), Directives :: t().
from_language(#argo_graphql_language_directives{directives = LanguageDirectiveList}) ->
    Directives1 = new(),
    Directives2 = lists:foldl(
        fun(LanguageDirective, Directives1Acc1) ->
            Directive = argo_graphql_directive:from_language(LanguageDirective),
            Directives1Acc2 = add_directive(Directives1Acc1, Directive),
            Directives1Acc2
        end,
        Directives1,
        LanguageDirectiveList
    ),
    Directives2.

-spec new() -> Directives when Directives :: t().
new() ->
    #argo_graphql_directives{
        directives = []
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive(Directives, Directive) -> Directives when
    Directives :: t(), Directive :: argo_graphql_directive:t().
add_directive(
    Directives1 = #argo_graphql_directives{directives = DirectivesList1}, Directive = #argo_graphql_directive{}
) ->
    DirectivesList2 = DirectivesList1 ++ [Directive],
    Directives2 = Directives1#argo_graphql_directives{directives = DirectivesList2},
    Directives2.

-spec find_directive_non_repeatable(Directives, DirectiveName) -> {ok, Directive} | error when
    Directives :: t(), DirectiveName :: argo_types:name(), Directive :: argo_graphql_directive:t().
find_directive_non_repeatable(Directives = #argo_graphql_directives{}, DirectiveName) when is_binary(DirectiveName) ->
    case get_directive_list(Directives, DirectiveName) of
        [] ->
            error;
        [Directive] ->
            {ok, Directive};
        [_ | _] ->
            error_with_info(badarg, [Directives, DirectiveName], #{2 => {expected_non_repeatable, DirectiveName}})
    end.

-spec find_directive_repeatable(Directives, DirectiveName) -> {ok, DirectiveList} | error when
    Directives :: t(),
    DirectiveName :: argo_types:name(),
    DirectiveList :: [Directive],
    Directive :: argo_graphql_directive:t().
find_directive_repeatable(Directives = #argo_graphql_directives{}, DirectiveName) when is_binary(DirectiveName) ->
    case get_directive_list(Directives, DirectiveName) of
        [] ->
            error;
        DirectiveList = [_ | _] ->
            {ok, DirectiveList}
    end.

-spec get_directive_list(Directives, DirectiveName) -> DirectiveList when
    Directives :: t(),
    DirectiveName :: argo_types:name(),
    DirectiveList :: [Directive],
    Directive :: argo_graphql_directive:t().
get_directive_list(#argo_graphql_directives{directives = DirectiveList}, DirectiveName) when is_binary(DirectiveName) ->
    [Directive || Directive = #argo_graphql_directive{name = Name} <- DirectiveList, Name =:= DirectiveName].

-spec get_directive_non_repeatable(Directives, DirectiveName) -> Directive when
    Directives :: t(), DirectiveName :: argo_types:name(), Directive :: argo_graphql_directive:t().
get_directive_non_repeatable(Directives = #argo_graphql_directives{}, DirectiveName) when is_binary(DirectiveName) ->
    case find_directive_non_repeatable(Directives, DirectiveName) of
        {ok, Directive = #argo_graphql_directive{}} ->
            Directive;
        error ->
            error_with_info(badarg, [Directives, DirectiveName], #{2 => {missing_directive_name, DirectiveName}})
    end.

-spec get_directive_repeatable(Directives, DirectiveName) -> DirectiveList when
    Directives :: t(),
    DirectiveName :: argo_types:name(),
    DirectiveList :: [Directive],
    Directive :: argo_graphql_directive:t().
get_directive_repeatable(Directives = #argo_graphql_directives{}, DirectiveName) when is_binary(DirectiveName) ->
    case find_directive_repeatable(Directives, DirectiveName) of
        {ok, DirectiveList = [#argo_graphql_directive{} | _]} ->
            DirectiveList;
        error ->
            error_with_info(badarg, [Directives, DirectiveName], #{2 => {missing_directive_name, DirectiveName}})
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_directives{directives = DirectivesList}) ->
    case length(DirectivesList) of
        0 ->
            Formatter1;
        _ ->
            Formatter2 = argo_graphql_formatter:shift_right(Formatter1),
            Formatter3 = lists:foldl(
                fun(Directive, Formatter2_Acc1) ->
                    Formatter2_Acc2 = argo_graphql_formatter:write(Formatter2_Acc1, "~n", []),
                    Formatter2_Acc3 = argo_graphql_formatter:write_indent(Formatter2_Acc2),
                    Formatter2_Acc4 = argo_graphql_directive:format(Formatter2_Acc3, Directive),
                    Formatter2_Acc4
                end,
                Formatter2,
                DirectivesList
            ),
            Formatter4 = argo_graphql_formatter:shift_left(Formatter3),
            Formatter4
    end.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-compile({inline, [error_with_info/3]}).
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, {expected_non_repeatable, DirectiveName}) ->
    io_lib:format("expected non-repeatable Directive name: ~0tp", [DirectiveName]);
format_error_description(_Key, {missing_directive_name, DirectiveName}) ->
    io_lib:format("missing Directive name: ~0tp", [DirectiveName]);
format_error_description(_Key, Value) ->
    Value.
