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
-module(argo_graphql_language_executable_directive_location).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% API
-export([
    name_from_string/1,
    parse/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type name() ::
    'QUERY'
    | 'MUTATION'
    | 'SUBSCRIPTION'
    | 'FIELD'
    | 'FRAGMENT_DEFINITION'
    | 'FRAGMENT_SPREAD'
    | 'INLINE_FRAGMENT'
    | 'VARIABLE_DEFINITION'.
-type context() :: #{
    name := name()
}.
-type t() :: #argo_graphql_language_executable_directive_location{}.

-export_type([
    name/0,
    context/0,
    t/0
]).

%% Macros
-define(is_name(T),
    ((T) =:= 'QUERY' orelse (T) =:= 'MUTATION' orelse (T) =:= 'SUBSCRIPTION' orelse (T) =:= 'FIELD' orelse
        (T) =:= 'FRAGMENT_DEFINITION' orelse (T) =:= 'FRAGMENT_SPREAD' orelse (T) =:= 'INLINE_FRAGMENT' orelse
        (T) =:= 'VARIABLE_DEFINITION')
).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec name_from_string(String) -> Name when String :: unicode:chardata(), Name :: name().
name_from_string(String) ->
    case erlang:binary_to_existing_atom(argo_types:unicode_binary(String), utf8) of
        Name when ?is_name(Name) ->
            Name;
        _ ->
            erlang:error(badarg, [String])
    end.

-spec parse(Context, Location) -> ExecutableDirectiveLocation when
    Context :: context(), Location :: erl_anno:location(), ExecutableDirectiveLocation :: t().
parse(_Context = #{name := Name}, Location) when ?is_name(Name) ->
    #argo_graphql_language_executable_directive_location{
        location = Location,
        name = Name
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_executable_directive_location{name = Name}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "~ts", [Name]),
    Formatter2.
