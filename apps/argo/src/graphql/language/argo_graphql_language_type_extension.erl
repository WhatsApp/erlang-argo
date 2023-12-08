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
-module(argo_graphql_language_type_extension).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    scalar_type_extension/2,
    object_type_extension/2,
    interface_type_extension/2,
    union_type_extension/2,
    enum_type_extension/2,
    input_object_type_extension/2
]).
%% Instance API
-export([
    is_ambiguous/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    argo_graphql_language_scalar_type_extension:t()
    | argo_graphql_language_object_type_extension:t()
    | argo_graphql_language_interface_type_extension:t()
    | argo_graphql_language_union_type_extension:t()
    | argo_graphql_language_enum_type_extension:t()
    | argo_graphql_language_input_object_type_extension:t().
-type t() :: #argo_graphql_language_type_extension{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [scalar_type_extension/2]}).
-spec scalar_type_extension(ScalarTypeExtension, Location) -> TypeExtension when
    ScalarTypeExtension :: argo_graphql_language_scalar_type_extension:t(),
    Location :: erl_anno:location(),
    TypeExtension :: t().
scalar_type_extension(ScalarTypeExtension = #argo_graphql_language_scalar_type_extension{}, Location) ->
    #argo_graphql_language_type_extension{location = Location, inner = ScalarTypeExtension}.

-compile({inline, [object_type_extension/2]}).
-spec object_type_extension(ObjectTypeExtension, Location) -> TypeExtension when
    ObjectTypeExtension :: argo_graphql_language_object_type_extension:t(),
    Location :: erl_anno:location(),
    TypeExtension :: t().
object_type_extension(ObjectTypeExtension = #argo_graphql_language_object_type_extension{}, Location) ->
    #argo_graphql_language_type_extension{location = Location, inner = ObjectTypeExtension}.

-compile({inline, [interface_type_extension/2]}).
-spec interface_type_extension(InterfaceTypeExtension, Location) -> TypeExtension when
    InterfaceTypeExtension :: argo_graphql_language_interface_type_extension:t(),
    Location :: erl_anno:location(),
    TypeExtension :: t().
interface_type_extension(InterfaceTypeExtension = #argo_graphql_language_interface_type_extension{}, Location) ->
    #argo_graphql_language_type_extension{location = Location, inner = InterfaceTypeExtension}.

-compile({inline, [union_type_extension/2]}).
-spec union_type_extension(UnionTypeExtension, Location) -> TypeExtension when
    UnionTypeExtension :: argo_graphql_language_union_type_extension:t(),
    Location :: erl_anno:location(),
    TypeExtension :: t().
union_type_extension(UnionTypeExtension = #argo_graphql_language_union_type_extension{}, Location) ->
    #argo_graphql_language_type_extension{location = Location, inner = UnionTypeExtension}.

-compile({inline, [enum_type_extension/2]}).
-spec enum_type_extension(EnumTypeExtension, Location) -> TypeExtension when
    EnumTypeExtension :: argo_graphql_language_enum_type_extension:t(),
    Location :: erl_anno:location(),
    TypeExtension :: t().
enum_type_extension(EnumTypeExtension = #argo_graphql_language_enum_type_extension{}, Location) ->
    #argo_graphql_language_type_extension{location = Location, inner = EnumTypeExtension}.

-compile({inline, [input_object_type_extension/2]}).
-spec input_object_type_extension(InputObjectTypeExtension, Location) -> TypeExtension when
    InputObjectTypeExtension :: argo_graphql_language_input_object_type_extension:t(),
    Location :: erl_anno:location(),
    TypeExtension :: t().
input_object_type_extension(InputObjectTypeExtension = #argo_graphql_language_input_object_type_extension{}, Location) ->
    #argo_graphql_language_type_extension{location = Location, inner = InputObjectTypeExtension}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_language_type_extension{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_scalar_type_extension{} ->
            false;
        #argo_graphql_language_object_type_extension{} ->
            argo_graphql_language_object_type_extension:is_ambiguous(Inner);
        #argo_graphql_language_interface_type_extension{} ->
            argo_graphql_language_interface_type_extension:is_ambiguous(Inner);
        #argo_graphql_language_union_type_extension{} ->
            false;
        #argo_graphql_language_enum_type_extension{} ->
            argo_graphql_language_enum_type_extension:is_ambiguous(Inner);
        #argo_graphql_language_input_object_type_extension{} ->
            argo_graphql_language_input_object_type_extension:is_ambiguous(Inner)
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_type_extension{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_scalar_type_extension{} ->
            argo_graphql_language_scalar_type_extension:format(Formatter1, Inner);
        #argo_graphql_language_object_type_extension{} ->
            argo_graphql_language_object_type_extension:format(Formatter1, Inner);
        #argo_graphql_language_interface_type_extension{} ->
            argo_graphql_language_interface_type_extension:format(Formatter1, Inner);
        #argo_graphql_language_union_type_extension{} ->
            argo_graphql_language_union_type_extension:format(Formatter1, Inner);
        #argo_graphql_language_enum_type_extension{} ->
            argo_graphql_language_enum_type_extension:format(Formatter1, Inner);
        #argo_graphql_language_input_object_type_extension{} ->
            argo_graphql_language_input_object_type_extension:format(Formatter1, Inner)
    end.
