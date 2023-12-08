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
-module(argo_graphql_language_type_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    scalar_type_definition/2,
    object_type_definition/2,
    interface_type_definition/2,
    union_type_definition/2,
    enum_type_definition/2,
    input_object_type_definition/2
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
    argo_graphql_language_scalar_type_definition:t()
    | argo_graphql_language_object_type_definition:t()
    | argo_graphql_language_interface_type_definition:t()
    | argo_graphql_language_union_type_definition:t()
    | argo_graphql_language_enum_type_definition:t()
    | argo_graphql_language_input_object_type_definition:t().
-type t() :: #argo_graphql_language_type_definition{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [scalar_type_definition/2]}).
-spec scalar_type_definition(ScalarTypeDefinition, Location) -> TypeDefinition when
    ScalarTypeDefinition :: argo_graphql_language_scalar_type_definition:t(),
    Location :: erl_anno:location(),
    TypeDefinition :: t().
scalar_type_definition(ScalarTypeDefinition = #argo_graphql_language_scalar_type_definition{}, Location) ->
    #argo_graphql_language_type_definition{location = Location, inner = ScalarTypeDefinition}.

-compile({inline, [object_type_definition/2]}).
-spec object_type_definition(ObjectTypeDefinition, Location) -> TypeDefinition when
    ObjectTypeDefinition :: argo_graphql_language_object_type_definition:t(),
    Location :: erl_anno:location(),
    TypeDefinition :: t().
object_type_definition(ObjectTypeDefinition = #argo_graphql_language_object_type_definition{}, Location) ->
    #argo_graphql_language_type_definition{location = Location, inner = ObjectTypeDefinition}.

-compile({inline, [interface_type_definition/2]}).
-spec interface_type_definition(InterfaceTypeDefinition, Location) -> TypeDefinition when
    InterfaceTypeDefinition :: argo_graphql_language_interface_type_definition:t(),
    Location :: erl_anno:location(),
    TypeDefinition :: t().
interface_type_definition(InterfaceTypeDefinition = #argo_graphql_language_interface_type_definition{}, Location) ->
    #argo_graphql_language_type_definition{location = Location, inner = InterfaceTypeDefinition}.

-compile({inline, [union_type_definition/2]}).
-spec union_type_definition(UnionTypeDefinition, Location) -> TypeDefinition when
    UnionTypeDefinition :: argo_graphql_language_union_type_definition:t(),
    Location :: erl_anno:location(),
    TypeDefinition :: t().
union_type_definition(UnionTypeDefinition = #argo_graphql_language_union_type_definition{}, Location) ->
    #argo_graphql_language_type_definition{location = Location, inner = UnionTypeDefinition}.

-compile({inline, [enum_type_definition/2]}).
-spec enum_type_definition(EnumTypeDefinition, Location) -> TypeDefinition when
    EnumTypeDefinition :: argo_graphql_language_enum_type_definition:t(),
    Location :: erl_anno:location(),
    TypeDefinition :: t().
enum_type_definition(EnumTypeDefinition = #argo_graphql_language_enum_type_definition{}, Location) ->
    #argo_graphql_language_type_definition{location = Location, inner = EnumTypeDefinition}.

-compile({inline, [input_object_type_definition/2]}).
-spec input_object_type_definition(InputObjectTypeDefinition, Location) -> TypeDefinition when
    InputObjectTypeDefinition :: argo_graphql_language_input_object_type_definition:t(),
    Location :: erl_anno:location(),
    TypeDefinition :: t().
input_object_type_definition(
    InputObjectTypeDefinition = #argo_graphql_language_input_object_type_definition{}, Location
) ->
    #argo_graphql_language_type_definition{location = Location, inner = InputObjectTypeDefinition}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_language_type_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_scalar_type_definition{} ->
            false;
        #argo_graphql_language_object_type_definition{} ->
            argo_graphql_language_object_type_definition:is_ambiguous(Inner);
        #argo_graphql_language_interface_type_definition{} ->
            argo_graphql_language_interface_type_definition:is_ambiguous(Inner);
        #argo_graphql_language_union_type_definition{} ->
            false;
        #argo_graphql_language_enum_type_definition{} ->
            argo_graphql_language_enum_type_definition:is_ambiguous(Inner);
        #argo_graphql_language_input_object_type_definition{} ->
            argo_graphql_language_input_object_type_definition:is_ambiguous(Inner)
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_type_definition{inner = Inner}) ->
    case Inner of
        #argo_graphql_language_scalar_type_definition{} ->
            argo_graphql_language_scalar_type_definition:format(Formatter1, Inner);
        #argo_graphql_language_object_type_definition{} ->
            argo_graphql_language_object_type_definition:format(Formatter1, Inner);
        #argo_graphql_language_interface_type_definition{} ->
            argo_graphql_language_interface_type_definition:format(Formatter1, Inner);
        #argo_graphql_language_union_type_definition{} ->
            argo_graphql_language_union_type_definition:format(Formatter1, Inner);
        #argo_graphql_language_enum_type_definition{} ->
            argo_graphql_language_enum_type_definition:format(Formatter1, Inner);
        #argo_graphql_language_input_object_type_definition{} ->
            argo_graphql_language_input_object_type_definition:format(Formatter1, Inner)
    end.
