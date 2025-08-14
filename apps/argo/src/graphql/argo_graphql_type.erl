%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_type).
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
    named_type/1,
    list_type/1,
    non_null_type/1
]).
%% Instance API
-export([
    get_base_type/1,
    get_type_name/1,
    is_list_type/1,
    is_named_type/1,
    is_non_null_type/1,
    is_nullable_type/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_types:name()
    | argo_graphql_list_type:t()
    | argo_graphql_non_null_type:t().
-type t() :: #argo_graphql_type{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageType) -> Type when LanguageType :: argo_graphql_language_type:t(), Type :: t().
from_language(#argo_graphql_language_type{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_named_type{name = NamedType} ->
            ?MODULE:named_type(NamedType);
        #argo_graphql_language_list_type{} ->
            ListType = argo_graphql_list_type:from_language(Inner),
            ?MODULE:list_type(ListType);
        #argo_graphql_language_non_null_type{} ->
            NonNullType = argo_graphql_non_null_type:from_language(Inner),
            ?MODULE:non_null_type(NonNullType)
    end.

-compile({inline, [named_type/1]}).
-spec named_type(NamedType) -> Type when NamedType :: argo_types:name(), Type :: t().
named_type(NamedType) when is_binary(NamedType) ->
    #argo_graphql_type{inner = NamedType}.

-compile({inline, [list_type/1]}).
-spec list_type(ListType) -> Type when ListType :: argo_graphql_list_type:t(), Type :: t().
list_type(ListType = #argo_graphql_list_type{}) ->
    #argo_graphql_type{inner = ListType}.

-compile({inline, [non_null_type/1]}).
-spec non_null_type(NonNullType) -> Type when NonNullType :: argo_graphql_non_null_type:t(), Type :: t().
non_null_type(NonNullType = #argo_graphql_non_null_type{}) ->
    #argo_graphql_type{inner = NonNullType}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec get_base_type(Type) -> Type when Type :: t().
get_base_type(Type = #argo_graphql_type{inner = NamedType}) when is_binary(NamedType) ->
    Type;
get_base_type(Type = #argo_graphql_type{inner = #argo_graphql_list_type{}}) ->
    Type;
get_base_type(#argo_graphql_type{inner = #argo_graphql_non_null_type{type = NamedType}}) when is_binary(NamedType) ->
    #argo_graphql_type{inner = NamedType};
get_base_type(#argo_graphql_type{inner = #argo_graphql_non_null_type{type = ListType = #argo_graphql_list_type{}}}) ->
    #argo_graphql_type{inner = ListType}.

-spec get_type_name(Type) -> NamedType when Type :: t(), NamedType :: argo_types:name().
get_type_name(#argo_graphql_type{inner = NamedType}) when is_binary(NamedType) ->
    NamedType;
get_type_name(#argo_graphql_type{inner = ListType = #argo_graphql_list_type{}}) ->
    argo_graphql_list_type:get_type_name(ListType);
get_type_name(#argo_graphql_type{inner = NonNullType = #argo_graphql_non_null_type{}}) ->
    argo_graphql_non_null_type:get_type_name(NonNullType).

-compile({inline, [is_list_type/1]}).
-spec is_list_type(Type) -> boolean() when Type :: t().
is_list_type(#argo_graphql_type{inner = #argo_graphql_list_type{}}) ->
    true;
is_list_type(#argo_graphql_type{inner = #argo_graphql_non_null_type{type = #argo_graphql_list_type{}}}) ->
    true;
is_list_type(#argo_graphql_type{}) ->
    false.

-compile({inline, [is_named_type/1]}).
-spec is_named_type(Type) -> boolean() when Type :: t().
is_named_type(#argo_graphql_type{inner = NamedType}) when is_binary(NamedType) ->
    true;
is_named_type(#argo_graphql_type{inner = #argo_graphql_non_null_type{type = NamedType}}) when is_binary(NamedType) ->
    true;
is_named_type(#argo_graphql_type{}) ->
    false.

-compile({inline, [is_non_null_type/1]}).
-spec is_non_null_type(Type) -> boolean() when Type :: t().
is_non_null_type(#argo_graphql_type{inner = #argo_graphql_non_null_type{}}) ->
    true;
is_non_null_type(#argo_graphql_type{}) ->
    false.

-compile({inline, [is_nullable_type/1]}).
-spec is_nullable_type(Type) -> boolean() when Type :: t().
is_nullable_type(Type = #argo_graphql_type{}) ->
    not is_non_null_type(Type).

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_type{inner = Inner}) ->
    case Inner of
        NamedType when is_binary(NamedType) ->
            argo_graphql_formatter:write(Formatter1, "~ts", [NamedType]);
        #argo_graphql_list_type{} ->
            argo_graphql_list_type:format(Formatter1, Inner);
        #argo_graphql_non_null_type{} ->
            argo_graphql_non_null_type:format(Formatter1, Inner)
    end.
