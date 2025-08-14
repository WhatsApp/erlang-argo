%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_union_type_definition).
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
    new/0
]).
%% Instance API
-export([
    add_union_member_type/2,
    get_shape/2
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
-type shape() :: #{
    argo_types:name() => argo_graphql_type:t(),
    type := union
}.
-type t() :: #argo_graphql_union_type_definition{}.

-export_type([
    shape/0,
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
        types = argo_index_set:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_union_member_type(UnionTypeDefinition, UnionMemberType) -> UnionTypeDefinition when
    UnionTypeDefinition :: t(), UnionMemberType :: argo_types:name().
add_union_member_type(
    UnionTypeDefinition1 = #argo_graphql_union_type_definition{types = UnionMemberTypes1}, UnionMemberType
) when is_binary(UnionMemberType) ->
    UnionMemberTypes2 = argo_index_set:add_element(UnionMemberType, UnionMemberTypes1),
    UnionTypeDefinition2 = UnionTypeDefinition1#argo_graphql_union_type_definition{types = UnionMemberTypes2},
    UnionTypeDefinition2.

-spec get_shape(UnionTypeDefinition, ServiceDocument) -> UnionMemberShape when
    UnionTypeDefinition :: t(), ServiceDocument :: argo_graphql_service_document:t(), UnionMemberShape :: shape().
get_shape(
    UnionTypeDefinition = #argo_graphql_union_type_definition{types = UnionMemberTypes},
    ServiceDocument = #argo_graphql_service_document{}
) ->
    argo_index_set:foldl(
        fun(_, UnionMemberType, Shape_Acc1) ->
            #argo_graphql_type_definition{
                kind = #argo_graphql_object_type_definition{fields = UnionMemberTypeDefinitionFields}
            } = argo_graphql_service_document:get_union_member_type_definition(
                ServiceDocument, UnionMemberType
            ),
            argo_index_map:foldl(
                fun(_, FieldName, #argo_graphql_field_definition{type = FieldType}, Shape_Acc1_Acc1) ->
                    case maps:find(FieldName, Shape_Acc1_Acc1) of
                        {ok, FieldType} ->
                            Shape_Acc1_Acc1;
                        {ok, ExistingFieldType} ->
                            error_with_info(badarg, [UnionTypeDefinition, ServiceDocument], #{
                                1 =>
                                    {shape_mismatch, #{
                                        union_member_type => UnionMemberType,
                                        field_name => FieldName,
                                        existing_type => ExistingFieldType,
                                        type => FieldType
                                    }}
                            });
                        error ->
                            maps:put(FieldName, FieldType, Shape_Acc1_Acc1)
                    end
                end,
                Shape_Acc1,
                UnionMemberTypeDefinitionFields
            )
        end,
        #{type => union},
        UnionMemberTypes
    ).

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
        "union shape mismatch for UnionMemberType ~0tp for FieldDefinition ~0tp (existing Type ~ts does not match the shape of Type ~ts)",
        [UnionMemberType, FieldName, argo_graphql:format(ExistingType), argo_graphql:format(Type)]
    );
format_error_description(_Key, Value) ->
    Value.
