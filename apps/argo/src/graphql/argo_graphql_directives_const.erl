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
-module(argo_graphql_directives_const).
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
    add_directive_const/2,
    find_directive_const_non_repeatable/2,
    find_directive_const_repeatable/2,
    get_directive_const_list/2,
    get_directive_const_non_repeatable/2,
    get_directive_const_repeatable/2
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
-type t() :: #argo_graphql_directives_const{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageDirectivesConst) -> DirectivesConst when
    LanguageDirectivesConst :: argo_graphql_language_directives_const:t(), DirectivesConst :: t().
from_language(#argo_graphql_language_directives_const{directives = LanguageDirectiveConstList}) ->
    DirectivesConst1 = new(),
    DirectivesConst2 = lists:foldl(
        fun(LanguageDirectiveConst, DirectivesConst1Acc1) ->
            DirectiveConst = argo_graphql_directive_const:from_language(LanguageDirectiveConst),
            DirectivesConst1Acc2 = add_directive_const(DirectivesConst1Acc1, DirectiveConst),
            DirectivesConst1Acc2
        end,
        DirectivesConst1,
        LanguageDirectiveConstList
    ),
    DirectivesConst2.

-spec new() -> DirectivesConst when DirectivesConst :: t().
new() ->
    #argo_graphql_directives_const{
        directives = []
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive_const(DirectivesConst, DirectiveConst) -> DirectivesConst when
    DirectivesConst :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    DirectivesConst1 = #argo_graphql_directives_const{directives = DirectivesConstList1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConstList2 = DirectivesConstList1 ++ [DirectiveConst],
    DirectivesConst2 = DirectivesConst1#argo_graphql_directives_const{directives = DirectivesConstList2},
    DirectivesConst2.

-spec find_directive_const_non_repeatable(DirectivesConst, DirectiveConstName) -> {ok, DirectiveConst} | error when
    DirectivesConst :: t(), DirectiveConstName :: argo_types:name(), DirectiveConst :: argo_graphql_directive_const:t().
find_directive_const_non_repeatable(DirectivesConst = #argo_graphql_directives_const{}, DirectiveConstName) when
    is_binary(DirectiveConstName)
->
    case get_directive_const_list(DirectivesConst, DirectiveConstName) of
        [] ->
            error;
        [DirectiveConst] ->
            {ok, DirectiveConst};
        [_ | _] ->
            error_with_info(badarg, [DirectivesConst, DirectiveConstName], #{
                2 => {expected_non_repeatable, DirectiveConstName}
            })
    end.

-spec find_directive_const_repeatable(DirectivesConst, DirectiveConstName) -> {ok, DirectiveConstList} | error when
    DirectivesConst :: t(),
    DirectiveConstName :: argo_types:name(),
    DirectiveConstList :: [DirectiveConst],
    DirectiveConst :: argo_graphql_directive_const:t().
find_directive_const_repeatable(DirectivesConst = #argo_graphql_directives_const{}, DirectiveConstName) when
    is_binary(DirectiveConstName)
->
    case get_directive_const_list(DirectivesConst, DirectiveConstName) of
        [] ->
            error;
        DirectiveConstList = [_ | _] ->
            {ok, DirectiveConstList}
    end.

-spec get_directive_const_list(DirectivesConst, DirectiveConstName) -> DirectiveConstList when
    DirectivesConst :: t(),
    DirectiveConstName :: argo_types:name(),
    DirectiveConstList :: [DirectiveConst],
    DirectiveConst :: argo_graphql_directive_const:t().
get_directive_const_list(#argo_graphql_directives_const{directives = DirectiveConstList}, DirectiveConstName) when
    is_binary(DirectiveConstName)
->
    [
        DirectiveConst
     || DirectiveConst = #argo_graphql_directive_const{name = Name} <- DirectiveConstList, Name =:= DirectiveConstName
    ].

-spec get_directive_const_non_repeatable(DirectivesConst, DirectiveConstName) -> DirectiveConst when
    DirectivesConst :: t(), DirectiveConstName :: argo_types:name(), DirectiveConst :: argo_graphql_directive_const:t().
get_directive_const_non_repeatable(DirectivesConst = #argo_graphql_directives_const{}, DirectiveConstName) when
    is_binary(DirectiveConstName)
->
    case find_directive_const_non_repeatable(DirectivesConst, DirectiveConstName) of
        {ok, DirectiveConst = #argo_graphql_directive_const{}} ->
            DirectiveConst;
        error ->
            error_with_info(badarg, [DirectivesConst, DirectiveConstName], #{
                2 => {missing_directive_name, DirectiveConstName}
            })
    end.

-spec get_directive_const_repeatable(DirectivesConst, DirectiveConstName) -> DirectiveConstList when
    DirectivesConst :: t(),
    DirectiveConstName :: argo_types:name(),
    DirectiveConstList :: [DirectiveConst],
    DirectiveConst :: argo_graphql_directive_const:t().
get_directive_const_repeatable(DirectivesConst = #argo_graphql_directives_const{}, DirectiveConstName) when
    is_binary(DirectiveConstName)
->
    case find_directive_const_repeatable(DirectivesConst, DirectiveConstName) of
        {ok, DirectiveConstList = [#argo_graphql_directive_const{} | _]} ->
            DirectiveConstList;
        error ->
            error_with_info(badarg, [DirectivesConst, DirectiveConstName], #{
                2 => {missing_directive_name, DirectiveConstName}
            })
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_directives_const{directives = DirectivesConstList}) ->
    case length(DirectivesConstList) of
        0 ->
            Formatter1;
        _ ->
            Formatter2 = argo_graphql_formatter:shift_right(Formatter1),
            Formatter3 = lists:foldl(
                fun(DirectiveConst, Formatter2_Acc1) ->
                    Formatter2_Acc2 = argo_graphql_formatter:write(Formatter2_Acc1, "~n", []),
                    Formatter2_Acc3 = argo_graphql_formatter:write_indent(Formatter2_Acc2),
                    Formatter2_Acc4 = argo_graphql_directive_const:format(Formatter2_Acc3, DirectiveConst),
                    Formatter2_Acc4
                end,
                Formatter2,
                DirectivesConstList
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
format_error_description(_Key, {expected_non_repeatable, DirectiveConstName}) ->
    io_lib:format("expected non-repeatable DirectiveConst name: ~0tp", [DirectiveConstName]);
format_error_description(_Key, {missing_directive_name, DirectiveConstName}) ->
    io_lib:format("missing DirectiveConst name: ~0tp", [DirectiveConstName]);
format_error_description(_Key, Value) ->
    Value.
