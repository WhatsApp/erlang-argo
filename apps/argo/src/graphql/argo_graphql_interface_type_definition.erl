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
-module(argo_graphql_interface_type_definition).
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
    add_interface/2,
    get_implements_interfaces/2,
    get_shape/1,
    get_shape/2,
    is_ambiguous/1
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
    type := interface
}.
-type t() :: #argo_graphql_interface_type_definition{}.

-export_type([
    shape/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageInterfaceTypeDefinition) -> InterfaceTypeDefinition when
    LanguageInterfaceTypeDefinition :: argo_graphql_language_interface_type_definition:t(),
    InterfaceTypeDefinition :: t().
from_language(#argo_graphql_language_interface_type_definition{
    implements = LanguageOptionImplements, fields = OptionLanguageFields
}) ->
    InterfaceTypeDefinition1 = new(),
    InterfaceTypeDefinition2 =
        case OptionLanguageFields of
            none ->
                InterfaceTypeDefinition1;
            {some, #argo_graphql_language_fields_definition{fields = []}} ->
                InterfaceTypeDefinition1;
            {some, #argo_graphql_language_fields_definition{
                fields = LanguageFieldDefinitionList = [#argo_graphql_language_field_definition{} | _]
            }} ->
                lists:foldl(
                    fun(LanguageFieldDefinition, InterfaceTypeDefinition1Acc1) ->
                        FieldDefinition = argo_graphql_field_definition:from_language(LanguageFieldDefinition),
                        InterfaceTypeDefinition1Acc2 = add_field_definition(
                            InterfaceTypeDefinition1Acc1, FieldDefinition
                        ),
                        InterfaceTypeDefinition1Acc2
                    end,
                    InterfaceTypeDefinition1,
                    LanguageFieldDefinitionList
                )
        end,
    InterfaceTypeDefinition3 =
        case LanguageOptionImplements of
            none ->
                InterfaceTypeDefinition2;
            {some, #argo_graphql_language_implements_interfaces{interfaces = []}} ->
                InterfaceTypeDefinition2;
            {some, #argo_graphql_language_implements_interfaces{
                interfaces = LanguageInterfaceList = [#argo_graphql_language_named_type{} | _]
            }} ->
                lists:foldl(
                    fun(#argo_graphql_language_named_type{name = InterfaceName}, InterfaceTypeDefinition2Acc1) ->
                        InterfaceTypeDefinition2Acc2 = add_interface(InterfaceTypeDefinition2Acc1, InterfaceName),
                        InterfaceTypeDefinition2Acc2
                    end,
                    InterfaceTypeDefinition2,
                    LanguageInterfaceList
                )
        end,
    InterfaceTypeDefinition3.

-spec new() -> InterfaceTypeDefinition when InterfaceTypeDefinition :: t().
new() ->
    #argo_graphql_interface_type_definition{
        implements = argo_index_set:new(),
        fields = argo_index_map:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_field_definition(InterfaceTypeDefinition, FieldDefinition) -> InterfaceTypeDefinition when
    InterfaceTypeDefinition :: t(), FieldDefinition :: argo_graphql_field_definition:t().
add_field_definition(
    InterfaceTypeDefinition1 = #argo_graphql_interface_type_definition{fields = FieldsMap1},
    FieldDefinition = #argo_graphql_field_definition{name = FieldName}
) ->
    case argo_index_map:is_key(FieldName, FieldsMap1) of
        false ->
            FieldsMap2 = argo_index_map:put(FieldName, FieldDefinition, FieldsMap1),
            InterfaceTypeDefinition2 = InterfaceTypeDefinition1#argo_graphql_interface_type_definition{
                fields = FieldsMap2
            },
            InterfaceTypeDefinition2;
        true ->
            error_with_info(badarg, [InterfaceTypeDefinition1, FieldDefinition], #{
                2 => {duplicate_field_name, FieldName}
            })
    end.

-spec add_interface(InterfaceTypeDefinition, InterfaceName) -> InterfaceTypeDefinition when
    InterfaceTypeDefinition :: t(), InterfaceName :: argo_types:name().
add_interface(
    InterfaceTypeDefinition1 = #argo_graphql_interface_type_definition{implements = Implements1}, InterfaceName
) when is_binary(InterfaceName) ->
    Implements2 = argo_index_set:add_element(InterfaceName, Implements1),
    InterfaceTypeDefinition2 = InterfaceTypeDefinition1#argo_graphql_interface_type_definition{
        implements = Implements2
    },
    InterfaceTypeDefinition2.

-spec get_implements_interfaces(InterfaceTypeDefinition, ServiceDocument) -> ImplementsInterfaces when
    InterfaceTypeDefinition :: t(),
    ServiceDocument :: argo_graphql_service_document:t(),
    ImplementsInterfaces :: [argo_types:name()].
get_implements_interfaces(
    _InterfaceTypeDefinition = #argo_graphql_interface_type_definition{implements = Implements},
    ServiceDocument = #argo_graphql_service_document{}
) ->
    get_implements_interfaces(argo_index_set:to_list(Implements), ServiceDocument, maps:new()).

%% @private
-spec get_implements_interfaces(ImplementsInterfaces, ServiceDocument, Seen) -> ImplementsInterfaces when
    ImplementsInterfaces :: [argo_types:name()],
    ServiceDocument :: argo_graphql_service_document:t(),
    Seen :: #{argo_types:name() => []}.
get_implements_interfaces([InterfaceName | Implements], ServiceDocument = #argo_graphql_service_document{}, Seen1) when
    is_map_key(InterfaceName, Seen1)
->
    get_implements_interfaces(Implements, ServiceDocument, Seen1);
get_implements_interfaces([InterfaceName | Implements1], ServiceDocument = #argo_graphql_service_document{}, Seen1) ->
    #argo_graphql_type_definition{kind = #argo_graphql_interface_type_definition{implements = MoreImplements}} = argo_graphql_service_document:get_interface_type_definition(
        ServiceDocument, InterfaceName
    ),
    Seen2 = maps:put(InterfaceName, [], Seen1),
    Implements2 = Implements1 ++ argo_index_set:to_list(MoreImplements),
    get_implements_interfaces(Implements2, ServiceDocument, Seen2);
get_implements_interfaces([], _ServiceDocument = #argo_graphql_service_document{}, Seen) ->
    lists:sort(maps:keys(Seen)).

-spec get_shape(InterfaceTypeDefinition) -> InterfaceShape when
    InterfaceTypeDefinition :: t(), InterfaceShape :: shape().
get_shape(_InterfaceTypeDefinition = #argo_graphql_interface_type_definition{fields = FieldsMap}) ->
    Shape1 = #{type => interface},
    Shape2 =
        argo_index_map:foldl(
            fun(_, FieldName, #argo_graphql_field_definition{type = FieldType}, Shape1_Acc1) ->
                maps:put(FieldName, FieldType, Shape1_Acc1)
            end,
            Shape1,
            FieldsMap
        ),
    Shape2.

-spec get_shape(InterfaceTypeDefinition, ServiceDocument) -> InterfaceShape when
    InterfaceTypeDefinition :: t(), ServiceDocument :: argo_graphql_service_document:t(), InterfaceShape :: shape().
get_shape(
    InterfaceTypeDefinition = #argo_graphql_interface_type_definition{},
    ServiceDocument = #argo_graphql_service_document{}
) ->
    Shape1 = get_shape(InterfaceTypeDefinition),
    ImplementsInterfaces = get_implements_interfaces(InterfaceTypeDefinition, ServiceDocument),
    ok = lists:foreach(
        fun(InterfaceName) ->
            #argo_graphql_type_definition{
                kind = ImplementsInterfaceTypeDefinition = #argo_graphql_interface_type_definition{}
            } = argo_graphql_service_document:get_interface_type_definition(ServiceDocument, InterfaceName),
            ImplementsInterfaceShape = get_shape(ImplementsInterfaceTypeDefinition),
            ok = maps:foreach(
                fun
                    (type, interface) ->
                        ok;
                    (FieldName, FieldType) ->
                        case maps:find(FieldName, Shape1) of
                            {ok, FieldType} ->
                                ok;
                            {ok, ExistingFieldType} ->
                                error_with_info(badarg, [InterfaceTypeDefinition, ServiceDocument], #{
                                    1 =>
                                        {shape_mismatch, #{
                                            interface_name => InterfaceName,
                                            field_name => FieldName,
                                            existing_type => ExistingFieldType,
                                            type => FieldType
                                        }}
                                });
                            error ->
                                error_with_info(badarg, [InterfaceTypeDefinition, ServiceDocument], #{
                                    1 =>
                                        {shape_mismatch_missing, #{
                                            interface_name => InterfaceName,
                                            field_name => FieldName,
                                            type => FieldType
                                        }}
                                })
                        end
                end,
                argo_types:dynamic_cast(maps:iterator(ImplementsInterfaceShape, ordered))
            ),
            ok
        end,
        ImplementsInterfaces
    ),
    Shape1.

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_interface_type_definition{fields = FieldsMap}) ->
    argo_index_map:size(FieldsMap) =:= 0.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_interface_type_definition{implements = Implements, fields = FieldsMap}) ->
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
format_error_description(
    _Key,
    {shape_mismatch, #{
        interface_name := InterfaceName, field_name := FieldName, existing_type := ExistingType, type := Type
    }}
) ->
    io_lib:format(
        "interface shape mismatch for Interface ~0tp for FieldDefinition ~0tp (existing Type ~ts does not match the shape of Type ~ts)",
        [InterfaceName, FieldName, argo_graphql:format(ExistingType), argo_graphql:format(Type)]
    );
format_error_description(
    _Key,
    {shape_mismatch_missing, #{
        interface_name := InterfaceName, field_name := FieldName, type := Type
    }}
) ->
    io_lib:format(
        "interface shape mismatch for Interface ~0tp for FieldDefinition ~0tp (missing expected Type ~ts)",
        [InterfaceName, FieldName, argo_graphql:format(Type)]
    );
format_error_description(_Key, Value) ->
    Value.
