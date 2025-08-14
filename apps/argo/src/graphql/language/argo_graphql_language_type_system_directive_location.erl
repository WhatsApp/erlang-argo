%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_type_system_directive_location).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
    'SCHEMA'
    | 'SCALAR'
    | 'OBJECT'
    | 'FIELD_DEFINITION'
    | 'ARGUMENT_DEFINITION'
    | 'INTERFACE'
    | 'UNION'
    | 'ENUM'
    | 'ENUM_VALUE'
    | 'INPUT_OBJECT'
    | 'INPUT_FIELD_DEFINITION'.
-type context() :: #{
    name := name()
}.
-type t() :: #argo_graphql_language_type_system_directive_location{}.

-export_type([
    name/0,
    context/0,
    t/0
]).

%% Macros
-define(is_name(T),
    ((T) =:= 'SCHEMA' orelse
        (T) =:= 'SCALAR' orelse
        (T) =:= 'OBJECT' orelse
        (T) =:= 'FIELD_DEFINITION' orelse
        (T) =:= 'ARGUMENT_DEFINITION' orelse
        (T) =:= 'INTERFACE' orelse
        (T) =:= 'UNION' orelse
        (T) =:= 'ENUM' orelse
        (T) =:= 'ENUM_VALUE' orelse
        (T) =:= 'INPUT_OBJECT' orelse
        (T) =:= 'INPUT_FIELD_DEFINITION')
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

-spec parse(Context, Location) -> TypeSystemDirectiveLocation when
    Context :: context(), Location :: erl_anno:location(), TypeSystemDirectiveLocation :: t().
parse(_Context = #{name := Name}, Location) when ?is_name(Name) ->
    #argo_graphql_language_type_system_directive_location{
        location = Location,
        name = Name
    }.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_type_system_directive_location{name = Name}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "~ts", [Name]),
    Formatter2.
