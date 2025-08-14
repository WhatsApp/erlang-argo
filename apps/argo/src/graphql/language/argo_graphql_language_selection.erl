%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_language_selection).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    field/2,
    fragment_spread/2,
    inline_fragment/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_graphql_language_field:t()
    | argo_graphql_language_fragment_spread:t()
    | argo_graphql_language_inline_fragment:t().
-type t() :: #argo_graphql_language_selection{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [field/2]}).
-spec field(Field, Location) -> TypeDefinition when
    Field :: argo_graphql_language_field:t(), Location :: erl_anno:location(), TypeDefinition :: t().
field(Field = #argo_graphql_language_field{}, Location) ->
    #argo_graphql_language_selection{location = Location, inner = Field}.

-compile({inline, [fragment_spread/2]}).
-spec fragment_spread(FragmentSpread, Location) -> TypeDefinition when
    FragmentSpread :: argo_graphql_language_fragment_spread:t(), Location :: erl_anno:location(), TypeDefinition :: t().
fragment_spread(FragmentSpread = #argo_graphql_language_fragment_spread{}, Location) ->
    #argo_graphql_language_selection{location = Location, inner = FragmentSpread}.

-compile({inline, [inline_fragment/2]}).
-spec inline_fragment(InlineFragment, Location) -> TypeDefinition when
    InlineFragment :: argo_graphql_language_inline_fragment:t(), Location :: erl_anno:location(), TypeDefinition :: t().
inline_fragment(InlineFragment = #argo_graphql_language_inline_fragment{}, Location) ->
    #argo_graphql_language_selection{location = Location, inner = InlineFragment}.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_selection{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_field{} ->
            argo_graphql_language_field:format(Formatter1, Inner);
        #argo_graphql_language_fragment_spread{} ->
            argo_graphql_language_fragment_spread:format(Formatter1, Inner);
        #argo_graphql_language_inline_fragment{} ->
            argo_graphql_language_inline_fragment:format(Formatter1, Inner)
    end.
