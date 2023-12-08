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
-module(argo_graphql_object_type_definition).
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
    add_field_definition/2,
    add_interface/2
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
-type t() :: #argo_graphql_object_type_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageObjectTypeDefinition) -> ObjectTypeDefinition when
    LanguageObjectTypeDefinition :: argo_graphql_language_object_type_definition:t(), ObjectTypeDefinition :: t().
from_language(#argo_graphql_language_object_type_definition{
    implements = LanguageOptionImplements, fields = OptionLanguageFields
}) ->
    ObjectTypeDefinition1 = new(),
    ObjectTypeDefinition2 =
        case OptionLanguageFields of
            none ->
                ObjectTypeDefinition1;
            {some, #argo_graphql_language_fields_definition{fields = []}} ->
                ObjectTypeDefinition1;
            {some, #argo_graphql_language_fields_definition{
                fields = LanguageFieldDefinitionList = [#argo_graphql_language_field_definition{} | _]
            }} ->
                lists:foldl(
                    fun(LanguageFieldDefinition, ObjectTypeDefinition1Acc1) ->
                        FieldDefinition = argo_graphql_field_definition:from_language(LanguageFieldDefinition),
                        ObjectTypeDefinition1Acc2 = add_field_definition(ObjectTypeDefinition1Acc1, FieldDefinition),
                        ObjectTypeDefinition1Acc2
                    end,
                    ObjectTypeDefinition1,
                    LanguageFieldDefinitionList
                )
        end,
    ObjectTypeDefinition3 =
        case LanguageOptionImplements of
            none ->
                ObjectTypeDefinition2;
            {some, #argo_graphql_language_implements_interfaces{interfaces = []}} ->
                ObjectTypeDefinition2;
            {some, #argo_graphql_language_implements_interfaces{
                interfaces = LanguageInterfaceList = [#argo_graphql_language_named_type{} | _]
            }} ->
                lists:foldl(
                    fun(#argo_graphql_language_named_type{name = InterfaceName}, ObjectTypeDefinition2Acc1) ->
                        ObjectTypeDefinition2Acc2 = add_interface(ObjectTypeDefinition2Acc1, InterfaceName),
                        ObjectTypeDefinition2Acc2
                    end,
                    ObjectTypeDefinition2,
                    LanguageInterfaceList
                )
        end,
    ObjectTypeDefinition3.

-spec new() -> ObjectTypeDefinition when ObjectTypeDefinition :: t().
new() ->
    #argo_graphql_object_type_definition{
        implements = argo_index_set:new(),
        fields = argo_index_map:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_field_definition(ObjectTypeDefinition, FieldDefinition) -> ObjectTypeDefinition when
    ObjectTypeDefinition :: t(), FieldDefinition :: argo_graphql_field_const:t().
add_field_definition(
    ObjectTypeDefinition1 = #argo_graphql_object_type_definition{fields = FieldsMap1},
    FieldDefinition = #argo_graphql_field_definition{name = FieldName}
) ->
    case argo_index_map:is_key(FieldName, FieldsMap1) of
        false ->
            FieldsMap2 = argo_index_map:put(FieldName, FieldDefinition, FieldsMap1),
            ObjectTypeDefinition2 = ObjectTypeDefinition1#argo_graphql_object_type_definition{fields = FieldsMap2},
            ObjectTypeDefinition2;
        true ->
            error_with_info(badarg, [ObjectTypeDefinition1, FieldDefinition], #{2 => {duplicate_field_name, FieldName}})
    end.

-spec add_interface(ObjectTypeDefinition, InterfaceName) -> ObjectTypeDefinition when
    ObjectTypeDefinition :: t(), InterfaceName :: argo_types:name().
add_interface(
    ObjectTypeDefinition1 = #argo_graphql_object_type_definition{implements = Implements1}, InterfaceName
) when is_binary(InterfaceName) ->
    Implements2 = argo_index_set:add_element(InterfaceName, Implements1),
    ObjectTypeDefinition2 = ObjectTypeDefinition1#argo_graphql_object_type_definition{implements = Implements2},
    ObjectTypeDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_object_type_definition{implements = Implements, fields = FieldsMap}) ->
    Formatter2 = format_implements(Formatter1, Implements),
    case argo_index_map:size(FieldsMap) of
        0 ->
            Formatter3 = argo_graphql_formatter:write(Formatter2, " {}", []),
            Formatter3;
        _ ->
            Formatter3 = argo_graphql_formatter:write(Formatter2, " {", []),
            Formatter4 = argo_graphql_formatter:shift_right(Formatter3),
            Formatter5 = argo_index_map:foldl(
                fun(_Index, _FieldName, FieldDefinition, Formatter4_Acc1) ->
                    Formatter4_Acc2 = argo_graphql_formatter:write(Formatter4_Acc1, "~n", []),
                    Formatter4_Acc3 = argo_graphql_formatter:write_indent(Formatter4_Acc2),
                    Formatter4_Acc4 = argo_graphql_field_definition:format(
                        Formatter4_Acc3, FieldDefinition
                    ),
                    Formatter4_Acc4
                end,
                Formatter4,
                FieldsMap
            ),
            Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
            Formatter7 = argo_graphql_formatter:shift_left(Formatter6),
            Formatter8 = argo_graphql_formatter:write_indent(Formatter7),
            Formatter9 = argo_graphql_formatter:write(Formatter8, "}", []),
            Formatter9
    end.

%% @private
-spec format_implements(Formatter1, Type :: argo_index_set:t(argo_types:name())) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format_implements(Formatter1, Implements) ->
    case argo_index_set:size(Implements) of
        0 ->
            Formatter1;
        Length ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, " implements ", []),
            Formatter3 = argo_index_set:foldl(
                fun(Index, InterfaceName, Formatter2_Acc1) ->
                    Formatter2_Acc2 = argo_graphql_formatter:write(Formatter2_Acc1, "~ts", [InterfaceName]),
                    Formatter2_Acc3 =
                        case (Index + 1) =:= Length of
                            false ->
                                argo_graphql_formatter:write(Formatter2_Acc2, " & ", []);
                            true ->
                                Formatter2_Acc2
                        end,
                    Formatter2_Acc3
                end,
                Formatter2,
                Implements
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
format_error_description(_Key, {duplicate_field_name, FieldName}) ->
    io_lib:format("duplicate FieldDefinition name: ~0tp", [FieldName]);
format_error_description(_Key, Value) ->
    Value.
