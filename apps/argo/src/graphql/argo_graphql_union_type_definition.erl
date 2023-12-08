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
-module(argo_graphql_union_type_definition).
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
    add_field_definition/3,
    add_union_member_type/2,
    resolve_field_definitions/2
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
-type t() :: #argo_graphql_union_type_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageUnionTypeDefinition) -> UnionTypeDefinition when
    LanguageUnionTypeDefinition :: argo_graphql_language_union_type_definition:t(), UnionTypeDefinition :: t().
from_language(#argo_graphql_language_union_type_definition{types = LanguageOptionImplements}) ->
    UnionTypeDefinition1 = new(),
    UnionTypeDefinition2 =
        case LanguageOptionImplements of
            none ->
                UnionTypeDefinition1;
            {some, []} ->
                UnionTypeDefinition1;
            {some, LanguageUnionMemberTypeList = [#argo_graphql_language_named_type{} | _]} ->
                lists:foldl(
                    fun(#argo_graphql_language_named_type{name = UnionMemberType}, UnionTypeDefinition1Acc1) ->
                        UnionTypeDefinition1Acc2 = add_union_member_type(UnionTypeDefinition1Acc1, UnionMemberType),
                        UnionTypeDefinition1Acc2
                    end,
                    UnionTypeDefinition1,
                    LanguageUnionMemberTypeList
                )
        end,
    UnionTypeDefinition2.

-spec new() -> UnionTypeDefinition when UnionTypeDefinition :: t().
new() ->
    #argo_graphql_union_type_definition{
        types = argo_index_set:new(),
        fields = argo_index_map:new(),
        resolved = false
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_field_definition(UnionTypeDefinition, UnionMemberType, FieldDefinition) -> UnionTypeDefinition when
    UnionTypeDefinition :: t(), UnionMemberType :: argo_types:name(), FieldDefinition :: argo_graphql_field_const:t().
add_field_definition(
    UnionTypeDefinition1 = #argo_graphql_union_type_definition{fields = FieldsMap1},
    UnionMemberType,
    FieldDefinition = #argo_graphql_field_definition{name = FieldName}
) when is_binary(UnionMemberType) ->
    case argo_index_map:find(FieldName, FieldsMap1) of
        {ok, ExistingFieldDefinition = #argo_graphql_field_definition{}} ->
            Type = FieldDefinition#argo_graphql_field_definition.type,
            ExistingType = ExistingFieldDefinition#argo_graphql_field_definition.type,
            case Type =:= ExistingType of
                false ->
                    error_with_info(badarg, [UnionTypeDefinition1, UnionMemberType, FieldDefinition], #{
                        2 =>
                            {shape_mismatch, #{
                                union_member_type => UnionMemberType,
                                field_name => FieldName,
                                existing_type => ExistingType,
                                type => Type
                            }}
                    });
                true ->
                    UnionTypeDefinition1
            end;
        error ->
            FieldsMap2 = argo_index_map:put(FieldName, FieldDefinition, FieldsMap1),
            UnionTypeDefinition2 = UnionTypeDefinition1#argo_graphql_union_type_definition{fields = FieldsMap2},
            UnionTypeDefinition2
    end.

-spec add_union_member_type(UnionTypeDefinition, UnionMemberType) -> UnionTypeDefinition when
    UnionTypeDefinition :: t(), UnionMemberType :: argo_types:name().
add_union_member_type(
    UnionTypeDefinition1 = #argo_graphql_union_type_definition{types = UnionMemberTypes1}, UnionMemberType
) when is_binary(UnionMemberType) ->
    UnionMemberTypes2 = argo_index_set:add_element(UnionMemberType, UnionMemberTypes1),
    UnionTypeDefinition2 = UnionTypeDefinition1#argo_graphql_union_type_definition{types = UnionMemberTypes2},
    UnionTypeDefinition2.

-spec resolve_field_definitions(UnionTypeDefinition, ServiceDocument) -> UnionTypeDefinition when
    UnionTypeDefinition :: t(), ServiceDocument :: argo_graphql_service_document:t().
resolve_field_definitions(
    UnionTypeDefinition1 = #argo_graphql_union_type_definition{types = UnionMemberTypes, resolved = false},
    ServiceDocument = #argo_graphql_service_document{}
) ->
    {UnionTypeDefinition2, ServiceDocument} = argo_index_set:foldl(
        fun resolve_union_member_type/3, {UnionTypeDefinition1, ServiceDocument}, UnionMemberTypes
    ),
    UnionTypeDefinition3 = UnionTypeDefinition2#argo_graphql_union_type_definition{resolved = true},
    UnionTypeDefinition3;
resolve_field_definitions(
    UnionTypeDefinition = #argo_graphql_union_type_definition{resolved = true},
    _ServiceDocument = #argo_graphql_service_document{}
) ->
    UnionTypeDefinition.

%% @private
-spec resolve_union_member_type(Index, UnionMemberType, {UnionTypeDefinition, ServiceDocument}) ->
    {UnionTypeDefinition, ServiceDocument}
when
    Index :: argo_index_set:index(),
    UnionMemberType :: argo_types:name(),
    UnionTypeDefinition :: t(),
    ServiceDocument :: argo_graphql_service_document:t().
resolve_union_member_type(_Index, UnionMemberType, {
    UnionTypeDefinition1 = #argo_graphql_union_type_definition{}, ServiceDocument = #argo_graphql_service_document{}
}) ->
    UnionMemberTypeDefinition = argo_graphql_service_document:get_union_member_type_definition(
        ServiceDocument, UnionMemberType
    ),
    {UnionTypeDefinition2, UnionMemberType} = argo_index_map:foldl(
        fun resolve_union_member_type_field_definition/4,
        {UnionTypeDefinition1, UnionMemberType},
        UnionMemberTypeDefinition#argo_graphql_type_definition.kind#argo_graphql_object_type_definition.fields
    ),
    {UnionTypeDefinition2, ServiceDocument}.

%% @private
-spec resolve_union_member_type_field_definition(
    Index, FieldName, FieldDefinition, {UnionTypeDefinition, UnionMemberType}
) -> {UnionTypeDefinition, UnionMemberType} when
    Index :: argo_index_map:index(),
    FieldName :: argo_types:name(),
    FieldDefinition :: argo_graphql_field_definition:t(),
    UnionTypeDefinition :: t(),
    UnionMemberType :: argo_types:name().
resolve_union_member_type_field_definition(
    _Index, _FieldName, FieldDefinition, {UnionTypeDefinition1, UnionMemberType}
) ->
    UnionTypeDefinition2 = add_field_definition(UnionTypeDefinition1, UnionMemberType, FieldDefinition),
    {UnionTypeDefinition2, UnionMemberType}.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_union_type_definition{types = UnionMemberTypes}) ->
    case argo_index_set:size(UnionMemberTypes) of
        0 ->
            Formatter1;
        Length ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, " = ", []),
            Formatter3 = argo_index_set:foldl(
                fun(Index, UnionMemberType, Formatter2Acc1) ->
                    Formatter2Acc2 = argo_graphql_formatter:write(Formatter2Acc1, "~ts", [UnionMemberType]),
                    Formatter2Acc3 =
                        case (Index + 1) =:= Length of
                            false ->
                                argo_graphql_formatter:write(Formatter2Acc2, " | ", []);
                            true ->
                                Formatter2Acc2
                        end,
                    Formatter2Acc3
                end,
                Formatter2,
                UnionMemberTypes
            ),
            Formatter3
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
format_error_description(
    _Key,
    {shape_mismatch, #{
        union_member_type := UnionMemberType, field_name := FieldName, existing_type := ExistingType, type := Type
    }}
) ->
    io_lib:format(
        "shape mismatch for UnionMemberType ~0tp for FieldDefinition ~0tp (existing Type ~ts does not match the shape of Type ~ts)",
        [UnionMemberType, FieldName, argo_graphql:format(ExistingType), argo_graphql:format(Type)]
    );
format_error_description(_Key, Value) ->
    Value.
