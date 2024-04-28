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
-module(proper_argo_graphql_service_document).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% Primitive API
-export([
    builtin_scalar_type_name/0,
    enum_value/1,
    field_name/1,
    input_type_graph/1,
    non_builtin_type_name/0,
    optional_description/0,
    unique_type_name/1,
    unique_type_name/2,
    with_input_type_graph/2
]).
%% GraphQL ServiceDocument API
-export([
    arguments_definition/1,
    enum_type_definition/2,
    enum_value_definition/2,
    field_definition/2,
    field_definition/3,
    input_object_type_definition/2,
    input_value_definition/3,
    input_value_definition/4,
    interface_type_definition/2,
    interface_type_definition/3,
    list_type/1,
    object_type_definition/2,
    object_type_definition/3,
    root_operation_type/1,
    scalar_type_definition/2,
    service_document/0,
    type/1,
    type_definition/2,
    type_definition/3,
    type_kind/2,
    type_name/2,
    type_report/1,
    union_type_definition/2,
    value_const/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type type_definition_kind() :: scalar | object | interface | union | enum | input_object.
-type type_definition_options() :: #{
    exclude => #{argo_types:name() => []},
    % Default: false
    include_root_operation_types => boolean(),
    % Default: output
    kinds => output | input | [type_definition_kind()],
    % Default: false
    must_be_new => boolean(),
    shape => argo_graphql_type_definition:shape()
}.

-export_type([
    type_definition_kind/0,
    type_definition_options/0
]).

%% Macros
-define(CONTEXT(KeyValueListType),
    ?LET(
        KeyValueList,
        KeyValueListType,
        lists:foldl(
            fun({Key, OptionValue}, ContextAcc) ->
                case OptionValue of
                    none ->
                        ContextAcc;
                    {some, Value} ->
                        maps:put(Key, Value, ContextAcc);
                    Value ->
                        maps:put(Key, Value, ContextAcc)
                end
            end,
            maps:new(),
            KeyValueList
        )
    )
).
-define(LANG, proper_argo_graphql_language).
-define(is_empty_shape(X), ((X) =:= none orelse (is_map(X) andalso map_size(X) < 2))).

%%%=============================================================================
%%% Primitive API functions
%%%=============================================================================

-spec builtin_scalar_type_name() -> proper_types:type().
builtin_scalar_type_name() ->
    oneof([
        <<"Boolean">>,
        <<"Float">>,
        <<"ID">>,
        <<"Int">>,
        <<"String">>
    ]).

-spec enum_value(ExcludeEnumValues) -> proper_types:type() when ExcludeEnumValues :: #{argo_types:name() => []}.
enum_value(ExcludeEnumValues) when is_map(ExcludeEnumValues) ->
    MostlyAllCapsGen = mostly(
        ?LET(Name, ?LANG:name(), argo_types:unicode_binary(string:to_upper(argo_types:unicode_string(Name)))),
        ?LANG:name()
    ),
    ?SUCHTHAT(EnumValue, MostlyAllCapsGen, not maps:is_key(EnumValue, ExcludeEnumValues)).

-spec field_name(ExcludeFieldNames) -> proper_types:type() when ExcludeFieldNames :: #{argo_types:name() => []}.
field_name(ExcludeFieldNames) when is_map(ExcludeFieldNames) ->
    ?SUCHTHAT(Name, ?LANG:name(), not maps:is_key(Name, ExcludeFieldNames)).

-spec input_type_graph(ServiceDocument) -> InputTypeGraph when
    ServiceDocument :: argo_graphql_service_document:t(),
    InputTypeGraph :: argo_graphql_input_type_graph:t().
input_type_graph(ServiceDocument = #argo_graphql_service_document{}) ->
    InputTypeGraph1 = argo_graphql_service_document:get_input_type_graph(ServiceDocument),
    case parameter('$argo_graphql_input_type_graph', none) of
        none ->
            InputTypeGraph1;
        InputTypeGraph2 = #argo_graphql_input_type_graph{} ->
            InputTypeGraph3 = argo_graphql_input_type_graph:merge(InputTypeGraph1, InputTypeGraph2),
            InputTypeGraph3
    end.

-spec non_builtin_type_name() -> proper_types:type().
non_builtin_type_name() ->
    Exclude = #{
        <<"ArgoCodecType">> => [],
        <<"Boolean">> => [],
        <<"Float">> => [],
        <<"ID">> => [],
        <<"Int">> => [],
        <<"String">> => []
    },
    ?SUCHTHAT(Name, ?LANG:name(), not maps:is_key(Name, Exclude)).

-spec optional_description() -> proper_types:type().
optional_description() ->
    frequency([
        {2, none},
        {1, {some, ?LANG:block_string_value()}}
    ]).

-spec unique_type_name(ServiceDocument) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t().
unique_type_name(#argo_graphql_service_document{type_definitions = TypeDefinitions}) ->
    ?SUCHTHAT(Name, non_builtin_type_name(), not maps:is_key(Name, TypeDefinitions)).

-spec unique_type_name(ServiceDocument, ExcludeTypeNames) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), ExcludeTypeNames :: #{argo_types:name() => []}.
unique_type_name(ServiceDocument, ExcludeTypeNames) when is_map(ExcludeTypeNames) ->
    ?SUCHTHAT(Name, unique_type_name(ServiceDocument), not maps:is_key(Name, ExcludeTypeNames)).

-spec with_input_type_graph(InputTypeGraph, RawType) -> proper_types:type() when
    InputTypeGraph :: argo_graphql_input_type_graph:t(), RawType :: proper_types:raw_type().
with_input_type_graph(InputTypeGraph = #argo_graphql_input_type_graph{}, RawType) ->
    with_parameter('$argo_graphql_input_type_graph', InputTypeGraph, RawType).

%%%=============================================================================
%%% GraphQL ServiceDocument API functions
%%%=============================================================================

%% @private
-spec add_argo_directives(TypeDefinition) -> proper_types:type() when
    TypeDefinition :: argo_graphql_type_definition:t().
add_argo_directives(TypeDefinition1 = #argo_graphql_type_definition{kind = Kind}) when
    is_record(Kind, argo_graphql_scalar_type_definition) orelse is_record(Kind, argo_graphql_enum_type_definition)
->
    ?LET(
        {ArgoCodecType1, OptionDeduplicate},
        oneof([
            {string, option(oneof([default, true, false]))},
            {int, none},
            {float, none},
            {boolean, none},
            {bytes, option(oneof([default, true, false]))},
            {{fixed, non_neg_integer()}, none},
            {desc, none}
        ]),
        begin
            ArgoCodecType2 =
                case ArgoCodecType1 of
                    string ->
                        <<"String">>;
                    int ->
                        <<"Int">>;
                    float ->
                        <<"Float">>;
                    boolean ->
                        <<"Boolean">>;
                    bytes ->
                        <<"BYTES">>;
                    {fixed, _} ->
                        <<"FIXED">>;
                    desc ->
                        <<"DESC">>
                end,
            ArgoCodec1 = argo_graphql_directive_const:new(<<"ArgoCodec">>),
            ArgoCodec2 = argo_graphql_directive_const:add_argument_const(
                ArgoCodec1, argo_graphql_argument_const:new(<<"codec">>, argo_graphql_value_const:enum(ArgoCodecType2))
            ),
            ArgoCodec3 =
                case ArgoCodecType1 of
                    {fixed, FixedLength} ->
                        argo_graphql_directive_const:add_argument_const(
                            ArgoCodec2,
                            argo_graphql_argument_const:new(
                                <<"fixedLength">>, argo_graphql_value_const:int(FixedLength)
                            )
                        );
                    _ ->
                        ArgoCodec2
                end,
            TypeDefinition2 = argo_graphql_type_definition:add_directive_const(TypeDefinition1, ArgoCodec3),
            TypeDefinition3 =
                case OptionDeduplicate of
                    none ->
                        TypeDefinition2;
                    {some, default} ->
                        argo_graphql_type_definition:add_directive_const(
                            TypeDefinition2, argo_graphql_directive_const:new(<<"ArgoDeduplicate">>)
                        );
                    {some, Deduplicate} when is_boolean(Deduplicate) ->
                        ArgoDeduplicate1 = argo_graphql_directive_const:new(<<"ArgoDeduplicate">>),
                        ArgoDeduplicate2 = argo_graphql_directive_const:add_argument_const(
                            ArgoDeduplicate1,
                            argo_graphql_argument_const:new(
                                <<"deduplicate">>, argo_graphql_value_const:boolean(Deduplicate)
                            )
                        ),
                        argo_graphql_type_definition:add_directive_const(TypeDefinition2, ArgoDeduplicate2)
                end,
            TypeDefinition3
        end
    ).

%% @private
-spec add_enum_values_definition(ServiceDocument, EnumTypeDefinition) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    EnumTypeDefinition :: argo_graphql_enum_type_definition:t().
add_enum_values_definition(
    ServiceDocument1 = #argo_graphql_service_document{},
    EnumTypeDefinition1 = #argo_graphql_enum_type_definition{values = ValuesMap}
) ->
    ?LET(
        {ServiceDocument2, EnumValueDefinitionList},
        enum_value_definition_list(ServiceDocument1, #{EnumValue => [] || EnumValue <- argo_index_map:keys(ValuesMap)}),
        {ServiceDocument2,
            lists:foldl(
                fun(EnumValueDefinition, EnumTypeDefinition1_Acc1) ->
                    EnumTypeDefinition1_Acc2 = argo_graphql_enum_type_definition:add_enum_value_definition(
                        EnumTypeDefinition1_Acc1, EnumValueDefinition
                    ),
                    EnumTypeDefinition1_Acc2
                end,
                EnumTypeDefinition1,
                EnumValueDefinitionList
            )}
    ).

%% @private
-spec add_input_object_fields_definition(ServiceDocument, InputObjectTypeName, InputObjectTypeDefinition) ->
    proper_types:type()
when
    ServiceDocument :: argo_graphql_service_document:t(),
    InputObjectTypeName :: argo_types:name(),
    InputObjectTypeDefinition :: argo_graphql_input_object_type_definition:t().
add_input_object_fields_definition(
    ServiceDocument1 = #argo_graphql_service_document{},
    InputObjectTypeName,
    InputObjectTypeDefinition1 = #argo_graphql_input_object_type_definition{inputs = InputsMap}
) when is_binary(InputObjectTypeName) ->
    InputTypeGraph1 = input_type_graph(ServiceDocument1),
    InputTypeGraph2 = argo_graphql_input_type_graph:add_input(InputTypeGraph1, InputObjectTypeName),
    ExcludeInputNames = #{InputName => [] || InputName <- argo_index_map:keys(InputsMap)},
    with_input_type_graph(
        InputTypeGraph2,
        ?LET(
            {ServiceDocument2, InputValueDefinitionList},
            input_value_definition_list(ServiceDocument1, InputObjectTypeName, ExcludeInputNames),
            {ServiceDocument2,
                lists:foldl(
                    fun(InputValueDefinition, InputObjectTypeDefinition1_Acc1) ->
                        InputObjectTypeDefinition1_Acc2 = argo_graphql_input_object_type_definition:add_input_value_definition(
                            InputObjectTypeDefinition1_Acc1, InputValueDefinition
                        ),
                        InputObjectTypeDefinition1_Acc2
                    end,
                    InputObjectTypeDefinition1,
                    InputValueDefinitionList
                )}
        )
    ).

%% @private
-spec add_interface_fields_definition(ServiceDocument, InterfaceTypeDefinition) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    InterfaceTypeDefinition :: argo_graphql_interface_type_definition:t().
add_interface_fields_definition(
    ServiceDocument1 = #argo_graphql_service_document{},
    InterfaceTypeDefinition1 = #argo_graphql_interface_type_definition{fields = FieldsMap}
) ->
    ?LET(
        {ServiceDocument2, FieldDefinitionList},
        field_definition_list(ServiceDocument1, #{FieldName => [] || FieldName <- argo_index_map:keys(FieldsMap)}),
        {ServiceDocument2,
            lists:foldl(
                fun(FieldDefinition, InterfaceTypeDefinition1_Acc1) ->
                    InterfaceTypeDefinition1_Acc2 = argo_graphql_interface_type_definition:add_field_definition(
                        InterfaceTypeDefinition1_Acc1, FieldDefinition
                    ),
                    InterfaceTypeDefinition1_Acc2
                end,
                InterfaceTypeDefinition1,
                FieldDefinitionList
            )}
    ).

%% @private
-spec add_object_fields_definition(ServiceDocument, ObjectTypeDefinition, ExcludeFieldNames) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    ObjectTypeDefinition :: argo_graphql_object_type_definition:t(),
    ExcludeFieldNames :: #{argo_types:name() => []}.
add_object_fields_definition(
    ServiceDocument1 = #argo_graphql_service_document{},
    ObjectTypeDefinition1 = #argo_graphql_object_type_definition{fields = FieldsMap},
    ExcludeFieldNames1
) when is_map(ExcludeFieldNames1) ->
    ExcludeFieldNames2 = maps:merge(
        ExcludeFieldNames1, #{FieldName => [] || FieldName <- argo_index_map:keys(FieldsMap)}
    ),
    ?LET(
        {ServiceDocument2, FieldDefinitionList},
        field_definition_list(ServiceDocument1, ExcludeFieldNames2),
        {ServiceDocument2,
            lists:foldl(
                fun(FieldDefinition, ObjectTypeDefinition1_Acc1) ->
                    ObjectTypeDefinition1_Acc2 = argo_graphql_object_type_definition:add_field_definition(
                        ObjectTypeDefinition1_Acc1, FieldDefinition
                    ),
                    ObjectTypeDefinition1_Acc2
                end,
                ObjectTypeDefinition1,
                FieldDefinitionList
            )}
    ).

%% @private
-spec add_union_member_types(ServiceDocument, UnionTypeDefinition) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    UnionTypeDefinition :: argo_graphql_union_type_definition:t().
add_union_member_types(
    ServiceDocument1 = #argo_graphql_service_document{},
    UnionTypeDefinition1 = #argo_graphql_union_type_definition{types = TypesSet}
) ->
    UnionMemberShape = argo_graphql_union_type_definition:get_shape(UnionTypeDefinition1, ServiceDocument1),
    ExistingUnionMemberTypes = #{UnionMemberType => [] || UnionMemberType <- argo_index_set:to_list(TypesSet)},
    ?LET(
        {ServiceDocument2, UnionMemberTypeDefinitionList},
        union_member_type_definition_list(ServiceDocument1, UnionMemberShape, ExistingUnionMemberTypes),
        begin
            UnionTypeDefinition2 = lists:foldl(
                fun(#argo_graphql_type_definition{name = UnionMemberType}, UnionTypeDefinition1_Acc1) ->
                    UnionTypeDefinition1_Acc2 = argo_graphql_union_type_definition:add_union_member_type(
                        UnionTypeDefinition1_Acc1, UnionMemberType
                    ),
                    UnionTypeDefinition1_Acc2
                end,
                UnionTypeDefinition1,
                UnionMemberTypeDefinitionList
            ),
            % UnionTypeDefinition3 = argo_graphql_union_type_definition:resolve_field_definitions(
            %     UnionTypeDefinition2, ServiceDocument2
            % ),
            {ServiceDocument2, UnionTypeDefinition2}
        end
    ).

-spec arguments_definition(ServiceDocument) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t().
arguments_definition(ServiceDocument1 = #argo_graphql_service_document{}) ->
    ArgumentsDefinition1 = argo_graphql_arguments_definition:new(),
    ExcludeInputNames = #{},
    frequency([
        {1 bsl complexity(), exactly({ServiceDocument1, argo_graphql_arguments_definition:new()})},
        {1,
            ?LET(
                {ServiceDocument2, InputValueDefinitionList},
                input_value_definition_list(ServiceDocument1, none, ExcludeInputNames),
                {ServiceDocument2,
                    lists:foldl(
                        fun(InputValueDefinition, ArgumentsDefinition1_Acc1) ->
                            ArgumentsDefinition1_Acc2 = argo_graphql_arguments_definition:add_input_value_definition(
                                ArgumentsDefinition1_Acc1, InputValueDefinition
                            ),
                            ArgumentsDefinition1_Acc2
                        end,
                        ArgumentsDefinition1,
                        InputValueDefinitionList
                    )}
            )}
    ]).

-spec enum_value_definition(ServiceDocument, EnumValue) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), EnumValue :: argo_types:name().
enum_value_definition(ServiceDocument1, EnumValue) ->
    ?LET(
        EnumValueDefinition,
        enum_value_definition_(EnumValue),
        {ServiceDocument1, EnumValueDefinition}
    ).

%% @private
-spec enum_value_definition_(EnumValue) -> proper_types:type() when EnumValue :: argo_types:name().
enum_value_definition_(EnumValue) ->
    ?LET(
        {
            OptionDescription
        },
        {
            optional_description()
        },
        exactly(#argo_graphql_enum_value_definition{
            value = EnumValue,
            description = OptionDescription,
            directives = argo_graphql_directives_const:new()
        })
    ).

%% @private
-spec enum_value_definition_list(ServiceDocument, ExcludeEnumValues) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), ExcludeEnumValues :: #{argo_types:name() => []}.
enum_value_definition_list(ServiceDocument1 = #argo_graphql_service_document{}, ExcludeEnumValues) ->
    complex(
        ?LET(
            EnumValueList,
            enum_value_list(ExcludeEnumValues),
            enum_value_definition_list_from_enum_value_list(ServiceDocument1, EnumValueList)
        )
    ).

%% @private
-spec enum_value_definition_list_from_enum_value_list(ServiceDocument, EnumValueList) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), EnumValueList :: [EnumValue], EnumValue :: argo_types:name().
enum_value_definition_list_from_enum_value_list(ServiceDocument = #argo_graphql_service_document{}, []) ->
    exactly({ServiceDocument, []});
enum_value_definition_list_from_enum_value_list(ServiceDocument1 = #argo_graphql_service_document{}, [
    EnumValue | EnumValueList
]) ->
    ?LAZY(
        ?LET(
            {ServiceDocument2, EnumValueDefinition},
            enum_value_definition(ServiceDocument1, EnumValue),
            ?LET(
                {ServiceDocument3, EnumValueDefinitionList},
                enum_value_definition_list_from_enum_value_list(ServiceDocument2, EnumValueList),
                {ServiceDocument3, [EnumValueDefinition | EnumValueDefinitionList]}
            )
        )
    ).

%% @private
-spec enum_value_list(ExcludeEnumValues) -> proper_types:type() when ExcludeEnumValues :: #{argo_types:name() => []}.
enum_value_list(ExcludeEnumValues) when is_map(ExcludeEnumValues) ->
    ?LET(
        InitialEnumValue,
        enum_value(ExcludeEnumValues),
        ?SUCHTHAT(
            EnumValueList,
            ?LET(
                List,
                ?SIZED(
                    Size,
                    proper_types:noshrink(
                        enum_value_list(Size, InitialEnumValue, ExcludeEnumValues#{InitialEnumValue => []})
                    )
                ),
                proper_types:shrink_list(List)
            ),
            enum_value_list_is_valid(EnumValueList)
        )
    ).

%% @private
-spec enum_value_list_is_valid(EnumValueList) -> boolean() when
    EnumValueList :: [EnumValue], EnumValue :: argo_types:name().
enum_value_list_is_valid(EnumValueList = [_ | _]) ->
    length(EnumValueList) =:= sets:size(sets:from_list(EnumValueList, [{version, 2}]));
enum_value_list_is_valid([]) ->
    false.

%% @private
-spec enum_value_list(Size, PreviousEnumValue, ExcludeEnumValues) -> proper_types:type() when
    Size :: non_neg_integer(), PreviousEnumValue :: argo_types:name(), ExcludeEnumValues :: #{argo_types:name() => []}.
enum_value_list(Size, PreviousEnumValue, ExcludeEnumValues) ->
    ?LAZY(
        frequency([
            {1, [PreviousEnumValue]},
            {Size,
                ?LET(
                    NextEnumValue,
                    enum_value(ExcludeEnumValues),
                    ?LET(
                        EnumValueList,
                        enum_value_list(Size - 1, NextEnumValue, ExcludeEnumValues#{NextEnumValue => []}),
                        [
                            PreviousEnumValue | EnumValueList
                        ]
                    )
                )}
        ])
    ).

-spec field_definition(ServiceDocument, FieldName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), FieldName :: argo_types:name().
field_definition(ServiceDocument1, FieldName) ->
    field_definition(ServiceDocument1, FieldName, #{}).

-spec field_definition(ServiceDocument, FieldName, FieldType | Options) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldName :: argo_types:name(),
    FieldType :: argo_graphql_type:t(),
    Options :: type_definition_options().
field_definition(ServiceDocument1 = #argo_graphql_service_document{}, FieldName, FieldType = #argo_graphql_type{}) when
    is_binary(FieldName)
->
    ?LET(
        FieldDefinition,
        field_definition_(FieldName, FieldType),
        {ServiceDocument1, FieldDefinition}
    );
field_definition(ServiceDocument1 = #argo_graphql_service_document{}, FieldName, Options) when
    is_binary(FieldName) andalso is_map(Options)
->
    ?LET(
        {ServiceDocument2, _TypeDefinition = #argo_graphql_type_definition{name = TypeName}},
        type_definition(ServiceDocument1, Options),
        ?LET(
            {ServiceDocument3, ArgumentsDefinition},
            arguments_definition(ServiceDocument2),
            ?LET(
                FieldDefinition1,
                field_definition_(FieldName, TypeName),
                begin
                    FieldDefinition2 = FieldDefinition1#argo_graphql_field_definition{
                        arguments = ArgumentsDefinition
                    },
                    {ServiceDocument3, FieldDefinition2}
                end
            )
        )
    ).

%% @private
-spec field_definition_(FieldName, FieldType | TypeName) -> proper_types:type() when
    FieldName :: argo_types:name(),
    FieldType :: argo_graphql_type:t(),
    TypeName :: argo_types:name().
field_definition_(FieldName, FieldType = #argo_graphql_type{}) when is_binary(FieldName) ->
    ?LET(
        {
            OptionDescription
        },
        {
            optional_description()
        },
        exactly(#argo_graphql_field_definition{
            name = FieldName,
            type = FieldType,
            arguments = argo_graphql_arguments_definition:new(),
            description = OptionDescription,
            directives = argo_graphql_directives_const:new()
        })
    );
field_definition_(FieldName, TypeName) when is_binary(FieldName) andalso is_binary(TypeName) ->
    ?LET(
        {
            FieldType = #argo_graphql_type{}
        },
        {
            type(TypeName)
        },
        field_definition_(FieldName, FieldType)
    ).

%% @private
-spec field_definition_list(ServiceDocument, ExcludeFieldNames) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), ExcludeFieldNames :: #{argo_types:name() => []}.
field_definition_list(ServiceDocument1 = #argo_graphql_service_document{}, ExcludeFieldNames) ->
    complex(
        ?LET(
            FieldNameList,
            field_name_list(ExcludeFieldNames),
            field_definition_list_from_field_name_list(ServiceDocument1, FieldNameList)
        )
    ).

%% @private
-spec field_definition_list_from_field_name_list(ServiceDocument, FieldNameList) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), FieldNameList :: [FieldName], FieldName :: argo_types:name().
field_definition_list_from_field_name_list(ServiceDocument = #argo_graphql_service_document{}, []) ->
    exactly({ServiceDocument, []});
field_definition_list_from_field_name_list(ServiceDocument1 = #argo_graphql_service_document{}, [
    FieldName | FieldNameList
]) ->
    ?LAZY(
        ?LET(
            {ServiceDocument2, FieldDefinition},
            field_definition(ServiceDocument1, FieldName),
            ?LET(
                {ServiceDocument3, FieldDefinitionList},
                field_definition_list_from_field_name_list(ServiceDocument2, FieldNameList),
                {ServiceDocument3, [FieldDefinition | FieldDefinitionList]}
            )
        )
    ).

%% @private
-spec field_definition_list_from_field_name_and_type_list(ServiceDocument, FieldNameAndTypeList) ->
    proper_types:type()
when
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldNameAndTypeList :: [{FieldName, FieldType}],
    FieldName :: argo_types:name(),
    FieldType :: argo_graphql_type:t().
field_definition_list_from_field_name_and_type_list(ServiceDocument = #argo_graphql_service_document{}, []) ->
    exactly({ServiceDocument, []});
field_definition_list_from_field_name_and_type_list(ServiceDocument1 = #argo_graphql_service_document{}, [
    {FieldName, FieldType} | FieldNameAndTypeList
]) ->
    ?LAZY(
        ?LET(
            {ServiceDocument2, FieldDefinition},
            field_definition(ServiceDocument1, FieldName, FieldType),
            ?LET(
                {ServiceDocument3, FieldDefinitionList},
                field_definition_list_from_field_name_and_type_list(ServiceDocument2, FieldNameAndTypeList),
                {ServiceDocument3, [FieldDefinition | FieldDefinitionList]}
            )
        )
    ).

%% @private
-spec field_name_list(ExcludeFieldNames) -> proper_types:type() when ExcludeFieldNames :: #{argo_types:name() => []}.
field_name_list(ExcludeFieldNames) when is_map(ExcludeFieldNames) ->
    ?LET(
        InitialFieldName,
        field_name(ExcludeFieldNames),
        ?SUCHTHAT(
            FieldNameList,
            ?LET(
                List,
                ?SIZED(
                    Size,
                    proper_types:noshrink(
                        field_name_list(Size, InitialFieldName, ExcludeFieldNames#{InitialFieldName => []})
                    )
                ),
                proper_types:shrink_list(List)
            ),
            field_name_list_is_valid(FieldNameList)
        )
    ).

%% @private
-spec field_name_list_is_valid(FieldNameList) -> boolean() when
    FieldNameList :: [FieldName], FieldName :: argo_types:name().
field_name_list_is_valid(FieldNameList = [_ | _]) ->
    length(FieldNameList) =:= sets:size(sets:from_list(FieldNameList, [{version, 2}]));
field_name_list_is_valid([]) ->
    false.

%% @private
-spec field_name_list(Size, PreviousFieldName, ExcludeFieldNames) -> proper_types:type() when
    Size :: non_neg_integer(), PreviousFieldName :: argo_types:name(), ExcludeFieldNames :: #{argo_types:name() => []}.
field_name_list(Size, PreviousFieldName, ExcludeFieldNames) ->
    ?LAZY(
        frequency([
            {1, [PreviousFieldName]},
            {Size,
                ?LET(
                    NextFieldName,
                    field_name(ExcludeFieldNames),
                    ?LET(
                        FieldNameList,
                        field_name_list(Size - 1, NextFieldName, ExcludeFieldNames#{NextFieldName => []}),
                        [
                            PreviousFieldName | FieldNameList
                        ]
                    )
                )}
        ])
    ).

-spec enum_type_definition(ServiceDocument, TypeName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
enum_type_definition(ServiceDocument1, TypeName) ->
    EnumTypeDefinition1 = argo_graphql_enum_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:enum_type_definition(TypeName, EnumTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    ?LET(
        {ServiceDocument3, EnumTypeDefinition2},
        ?LAZY(add_enum_values_definition(ServiceDocument2, EnumTypeDefinition1)),
        begin
            TypeDefinition2 = argo_graphql_type_definition:enum_type_definition(TypeName, EnumTypeDefinition2),
            ServiceDocument4 = argo_graphql_service_document:replace_type_definition(ServiceDocument3, TypeDefinition2),
            {ServiceDocument4, TypeDefinition2}
        end
    ).

-spec input_object_type_definition(ServiceDocument, TypeName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
input_object_type_definition(ServiceDocument1, TypeName) ->
    InputObjectTypeDefinition1 = argo_graphql_input_object_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:input_object_type_definition(TypeName, InputObjectTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    ?LET(
        {ServiceDocument3, InputObjectTypeDefinition2},
        ?LAZY(add_input_object_fields_definition(ServiceDocument2, TypeName, InputObjectTypeDefinition1)),
        begin
            TypeDefinition2 = argo_graphql_type_definition:input_object_type_definition(
                TypeName, InputObjectTypeDefinition2
            ),
            ServiceDocument4 = argo_graphql_service_document:replace_type_definition(ServiceDocument3, TypeDefinition2),
            {ServiceDocument4, TypeDefinition2}
        end
    ).

-spec input_value_definition(ServiceDocument, MaybeSourceName, InputName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    MaybeSourceName :: none | argo_types:name(),
    InputName :: argo_types:name().
input_value_definition(ServiceDocument1, MaybeSourceName, InputName) ->
    input_value_definition(ServiceDocument1, MaybeSourceName, InputName, #{kinds => input}).

-spec input_value_definition(ServiceDocument, MaybeSourceName, InputName, Options) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    MaybeSourceName :: none | argo_types:name(),
    InputName :: argo_types:name(),
    Options :: type_definition_options().
input_value_definition(ServiceDocument1, MaybeSourceName, InputName, Options) when is_map(Options) ->
    ?LET(
        {ServiceDocument2, InputTypeDefinition},
        type_definition(ServiceDocument1, Options),
        begin
            InputTypeName = InputTypeDefinition#argo_graphql_type_definition.name,
            InputTypeGraph1 = input_type_graph(ServiceDocument2),
            TypeGen =
                case MaybeSourceName of
                    none ->
                        type(InputTypeName);
                    SourceName when is_binary(SourceName) ->
                        case
                            argo_graphql_input_type_graph:is_valid_dependency(
                                InputTypeGraph1, SourceName, InputTypeName
                            )
                        of
                            false ->
                                ?SUCHTHAT(Type, type(InputTypeName), argo_graphql_type:is_nullable_type(Type));
                            true ->
                                type(InputTypeName)
                        end
                end,
            ?LET(
                InputType,
                TypeGen,
                ?LET(
                    InputValueDefinition,
                    input_value_definition_(ServiceDocument2, InputName, InputType),
                    {ServiceDocument2, InputValueDefinition}
                )
            )
        end
    ).

%% @private
-spec input_value_definition_(ServiceDocument, InputName, InputType) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    InputName :: argo_types:name(),
    InputType :: argo_graphql_type:t().
input_value_definition_(
    ServiceDocument = #argo_graphql_service_document{}, InputName, InputType = #argo_graphql_type{}
) when is_binary(InputName) ->
    ?LET(
        {
            OptionDescription,
            OptionDefaultValue
        },
        {
            optional_description(),
            option(value_const(ServiceDocument, InputType))
        },
        exactly(#argo_graphql_input_value_definition{
            name = InputName,
            type = InputType,
            description = OptionDescription,
            directives = argo_graphql_directives_const:new(),
            default_value = OptionDefaultValue
        })
    ).

%% @private
-spec input_value_definition_list(ServiceDocument, MaybeSourceName, ExcludeInputNames) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    MaybeSourceName :: none | argo_types:name(),
    ExcludeInputNames :: #{argo_types:name() => []}.
input_value_definition_list(ServiceDocument1 = #argo_graphql_service_document{}, MaybeSourceName, ExcludeInputNames) ->
    complex(
        ?LET(
            InputNameList,
            field_name_list(ExcludeInputNames),
            input_value_definition_list_from_input_name_list(ServiceDocument1, MaybeSourceName, InputNameList)
        )
    ).

%% @private
-spec input_value_definition_list_from_input_name_list(ServiceDocument, MaybeSourceName, InputNameList) ->
    proper_types:type()
when
    ServiceDocument :: argo_graphql_service_document:t(),
    MaybeSourceName :: none | argo_types:name(),
    InputNameList :: [InputName],
    InputName :: argo_types:name().
input_value_definition_list_from_input_name_list(
    ServiceDocument = #argo_graphql_service_document{}, _MaybeSourceName, []
) ->
    exactly({ServiceDocument, []});
input_value_definition_list_from_input_name_list(
    ServiceDocument1 = #argo_graphql_service_document{}, MaybeSourceName, [
        InputName | InputNameList
    ]
) ->
    ?LAZY(
        ?LET(
            {ServiceDocument2, InputValueDefinition},
            input_value_definition(ServiceDocument1, MaybeSourceName, InputName),
            ?LET(
                {ServiceDocument3, InputValueDefinitionList},
                input_value_definition_list_from_input_name_list(ServiceDocument2, MaybeSourceName, InputNameList),
                {ServiceDocument3, [InputValueDefinition | InputValueDefinitionList]}
            )
        )
    ).

-spec interface_type_definition(ServiceDocument, TypeName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
interface_type_definition(ServiceDocument1, TypeName) ->
    InterfaceTypeDefinition1 = argo_graphql_interface_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:interface_type_definition(TypeName, InterfaceTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    ?LET(
        {ServiceDocument3, InterfaceTypeDefinition2},
        ?LAZY(add_interface_fields_definition(ServiceDocument2, InterfaceTypeDefinition1)),
        begin
            TypeDefinition2 = argo_graphql_type_definition:interface_type_definition(
                TypeName, InterfaceTypeDefinition2
            ),
            ServiceDocument4 = argo_graphql_service_document:replace_type_definition(ServiceDocument3, TypeDefinition2),
            {ServiceDocument4, TypeDefinition2}
        end
    ).

-spec interface_type_definition(ServiceDocument, TypeName, Shape) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeName :: argo_types:name(),
    Shape :: none | argo_graphql_type_definition:shape().
interface_type_definition(ServiceDocument, TypeName, Shape) when ?is_empty_shape(Shape) ->
    interface_type_definition(ServiceDocument, TypeName);
interface_type_definition(ServiceDocument1, TypeName, Shape = #{type := interface}) ->
    InterfaceTypeDefinition1 = argo_graphql_interface_type_definition:new(),
    ?LET(
        {ServiceDocument2, FieldDefinitionList},
        field_definition_list_from_field_name_and_type_list(
            ServiceDocument1, maps:to_list(maps:without([type], Shape))
        ),
        begin
            InterfaceTypeDefinition2 = lists:foldl(
                fun(FieldDefinition, InterfaceTypeDefinition1_Acc1) ->
                    InterfaceTypeDefinition1_Acc2 = argo_graphql_interface_type_definition:add_field_definition(
                        InterfaceTypeDefinition1_Acc1, FieldDefinition
                    ),
                    InterfaceTypeDefinition1_Acc2
                end,
                InterfaceTypeDefinition1,
                FieldDefinitionList
            ),
            TypeDefinition1 = argo_graphql_type_definition:interface_type_definition(
                TypeName, InterfaceTypeDefinition2
            ),
            ServiceDocument3 = argo_graphql_service_document:add_type_definition(ServiceDocument2, TypeDefinition1),
            ?LET(
                {ServiceDocument4, InterfaceTypeDefinition3},
                ?LAZY(add_interface_fields_definition(ServiceDocument3, InterfaceTypeDefinition2)),
                begin
                    TypeDefinition2 = argo_graphql_type_definition:interface_type_definition(
                        TypeName, InterfaceTypeDefinition3
                    ),
                    ServiceDocument5 = argo_graphql_service_document:replace_type_definition(
                        ServiceDocument4, TypeDefinition2
                    ),
                    {ServiceDocument5, TypeDefinition2}
                end
            )
        end
    );
interface_type_definition(ServiceDocument1, TypeName, Shape = #{type := union}) ->
    InterfaceTypeDefinition1 = argo_graphql_interface_type_definition:new(),
    ?LET(
        FieldNameAndTypeList1,
        list(oneof(maps:to_list(maps:without([type], Shape)))),
        begin
            FieldNameAndTypeList2 = lists:uniq(FieldNameAndTypeList1),
            ?LET(
                {ServiceDocument2, FieldDefinitionList},
                field_definition_list_from_field_name_and_type_list(ServiceDocument1, FieldNameAndTypeList2),
                begin
                    InterfaceTypeDefinition2 = lists:foldl(
                        fun(FieldDefinition, InterfaceTypeDefinition1_Acc1) ->
                            InterfaceTypeDefinition1_Acc2 = argo_graphql_interface_type_definition:add_field_definition(
                                InterfaceTypeDefinition1_Acc1, FieldDefinition
                            ),
                            InterfaceTypeDefinition1_Acc2
                        end,
                        InterfaceTypeDefinition1,
                        FieldDefinitionList
                    ),
                    TypeDefinition1 = argo_graphql_type_definition:interface_type_definition(
                        TypeName, InterfaceTypeDefinition2
                    ),
                    ServiceDocument3 = argo_graphql_service_document:add_type_definition(
                        ServiceDocument2, TypeDefinition1
                    ),
                    ?LET(
                        {ServiceDocument4, InterfaceTypeDefinition3},
                        ?LAZY(add_interface_fields_definition(ServiceDocument3, InterfaceTypeDefinition2)),
                        begin
                            TypeDefinition2 = argo_graphql_type_definition:interface_type_definition(
                                TypeName, InterfaceTypeDefinition3
                            ),
                            ServiceDocument5 = argo_graphql_service_document:replace_type_definition(
                                ServiceDocument4, TypeDefinition2
                            ),
                            {ServiceDocument5, TypeDefinition2}
                        end
                    )
                end
            )
        end
    ).

-spec list_type(TypeName) -> proper_types:type() when TypeName :: argo_types:name().
list_type(TypeName) when is_binary(TypeName) ->
    ?LAZY(?LET(Type, type(TypeName), exactly(argo_graphql_type:list_type(argo_graphql_list_type:new(Type))))).

-spec object_type_definition(ServiceDocument, TypeName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
object_type_definition(ServiceDocument1, TypeName) ->
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    ExcludeFieldNames = #{},
    ?LET(
        {ServiceDocument3, ObjectTypeDefinition2},
        ?LAZY(add_object_fields_definition(ServiceDocument2, ObjectTypeDefinition1, ExcludeFieldNames)),
        begin
            TypeDefinition2 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition2),
            ServiceDocument4 = argo_graphql_service_document:replace_type_definition(ServiceDocument3, TypeDefinition2),
            {ServiceDocument4, TypeDefinition2}
        end
    ).

-spec object_type_definition(ServiceDocument, TypeName, Shape) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeName :: argo_types:name(),
    Shape :: none | argo_graphql_type_definition:shape().
object_type_definition(ServiceDocument, TypeName, Shape) when ?is_empty_shape(Shape) ->
    object_type_definition(ServiceDocument, TypeName);
object_type_definition(ServiceDocument1, TypeName, Shape = #{type := interface}) ->
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ?LET(
        {ServiceDocument2, FieldDefinitionList},
        field_definition_list_from_field_name_and_type_list(
            ServiceDocument1, maps:to_list(maps:without([type], Shape))
        ),
        begin
            ObjectTypeDefinition2 = lists:foldl(
                fun(FieldDefinition, ObjectTypeDefinition1_Acc1) ->
                    ObjectTypeDefinition1_Acc2 = argo_graphql_object_type_definition:add_field_definition(
                        ObjectTypeDefinition1_Acc1, FieldDefinition
                    ),
                    ObjectTypeDefinition1_Acc2
                end,
                ObjectTypeDefinition1,
                FieldDefinitionList
            ),
            TypeDefinition1 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition2),
            ServiceDocument3 = argo_graphql_service_document:add_type_definition(ServiceDocument2, TypeDefinition1),
            ExcludeFieldNames =
                #{ShapeFieldName => [] || ShapeFieldName <- maps:keys(Shape), is_binary(ShapeFieldName)},
            ?LET(
                {ServiceDocument4, ObjectTypeDefinition3},
                ?LAZY(add_object_fields_definition(ServiceDocument3, ObjectTypeDefinition2, ExcludeFieldNames)),
                begin
                    TypeDefinition2 = argo_graphql_type_definition:object_type_definition(
                        TypeName, ObjectTypeDefinition3
                    ),
                    ServiceDocument5 = argo_graphql_service_document:replace_type_definition(
                        ServiceDocument4, TypeDefinition2
                    ),
                    {ServiceDocument5, TypeDefinition2}
                end
            )
        end
    );
object_type_definition(ServiceDocument1, TypeName, Shape = #{type := union}) ->
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ?LET(
        FieldNameAndTypeList1,
        list(oneof(maps:to_list(maps:without([type], Shape)))),
        begin
            FieldNameAndTypeList2 = lists:uniq(FieldNameAndTypeList1),
            ?LET(
                {ServiceDocument2, FieldDefinitionList},
                field_definition_list_from_field_name_and_type_list(ServiceDocument1, FieldNameAndTypeList2),
                begin
                    ObjectTypeDefinition2 = lists:foldl(
                        fun(FieldDefinition, ObjectTypeDefinition1_Acc1) ->
                            ObjectTypeDefinition1_Acc2 = argo_graphql_object_type_definition:add_field_definition(
                                ObjectTypeDefinition1_Acc1, FieldDefinition
                            ),
                            ObjectTypeDefinition1_Acc2
                        end,
                        ObjectTypeDefinition1,
                        FieldDefinitionList
                    ),
                    TypeDefinition1 = argo_graphql_type_definition:object_type_definition(
                        TypeName, ObjectTypeDefinition2
                    ),
                    ServiceDocument3 = argo_graphql_service_document:add_type_definition(
                        ServiceDocument2, TypeDefinition1
                    ),
                    ExcludeFieldNames =
                        #{ShapeFieldName => [] || ShapeFieldName <- maps:keys(Shape), is_binary(ShapeFieldName)},
                    ?LET(
                        {ServiceDocument4, ObjectTypeDefinition3},
                        ?LAZY(add_object_fields_definition(ServiceDocument3, ObjectTypeDefinition2, ExcludeFieldNames)),
                        begin
                            TypeDefinition2 = argo_graphql_type_definition:object_type_definition(
                                TypeName, ObjectTypeDefinition3
                            ),
                            ServiceDocument5 = argo_graphql_service_document:replace_type_definition(
                                ServiceDocument4, TypeDefinition2
                            ),
                            {ServiceDocument5, TypeDefinition2}
                        end
                    )
                end
            )
        end
    ).

-spec root_operation_type(ServiceDocument) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t().
root_operation_type(
    _ServiceDocument = #argo_graphql_service_document{
        'query' = OptionQuery, mutation = OptionMutation, subscription = OptionSubscription
    }
) ->
    ?SUCHTHAT(
        RootOperationType,
        oneof(['query', mutation, subscription]),
        case RootOperationType of
            'query' when OptionQuery =/= none ->
                true;
            mutation when OptionMutation =/= none ->
                true;
            subscription when OptionSubscription =/= none ->
                true;
            _ ->
                false
        end
    ).

-spec scalar_type_definition(ServiceDocument, TypeName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
scalar_type_definition(ServiceDocument1, TypeName) ->
    ScalarTypeDefinition = argo_graphql_scalar_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:scalar_type_definition(TypeName, ScalarTypeDefinition),
    ?LET(TypeDefinition2, add_argo_directives(TypeDefinition1), begin
        ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition2),
        {ServiceDocument2, TypeDefinition2}
    end).

-spec service_document() -> proper_types:type().
service_document() ->
    ?LET(
        ServiceDocument1,
        service_document_is_instance(service_document_()),
        begin
            RootOperationTypes = ['query', mutation, subscription],
            ServiceDocument2 = service_document_reserve_root_operation_types(ServiceDocument1, RootOperationTypes),
            service_document_generate_root_operation_types(ServiceDocument2, RootOperationTypes)
        end
    ).

%% @private
-spec service_document_() -> proper_types:type().
service_document_() ->
    ?LET(
        {
            OptionDescription,
            OptionQuery,
            OptionMutation,
            OptionSubscription
        },
        {
            optional_description(),
            option(exactly(<<"object_query">>)),
            option(exactly(<<"object_mutation">>)),
            option(exactly(<<"object_subscription">>))
        },
        exactly(#argo_graphql_service_document{
            description = OptionDescription,
            directives = argo_graphql_directives_const:new(),
            schema_defined = true,
            'query' = OptionQuery,
            mutation = OptionMutation,
            subscription = OptionSubscription,
            directive_definitions = maps:new(),
            type_definitions = maps:new()
        })
    ).

%% @private
-spec service_document_reserve_root_operation_types(ServiceDocument, RootOperationTypes) -> ServiceDocument when
    ServiceDocument :: argo_graphql_service_document:t(),
    RootOperationTypes :: [argo_graphql_service_document:operation_type()].
service_document_reserve_root_operation_types(
    ServiceDocument1 = #argo_graphql_service_document{'query' = {some, TypeName}}, ['query' | RootOperationTypes]
) ->
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    service_document_reserve_root_operation_types(ServiceDocument2, RootOperationTypes);
service_document_reserve_root_operation_types(
    ServiceDocument1 = #argo_graphql_service_document{mutation = {some, TypeName}}, [mutation | RootOperationTypes]
) ->
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    service_document_reserve_root_operation_types(ServiceDocument2, RootOperationTypes);
service_document_reserve_root_operation_types(
    ServiceDocument1 = #argo_graphql_service_document{subscription = {some, TypeName}}, [
        subscription | RootOperationTypes
    ]
) ->
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    service_document_reserve_root_operation_types(ServiceDocument2, RootOperationTypes);
service_document_reserve_root_operation_types(ServiceDocument1 = #argo_graphql_service_document{}, [
    _ | RootOperationTypes
]) ->
    service_document_reserve_root_operation_types(ServiceDocument1, RootOperationTypes);
service_document_reserve_root_operation_types(ServiceDocument1 = #argo_graphql_service_document{}, []) ->
    ServiceDocument1.

%% @private
-spec service_document_generate_root_operation_types(ServiceDocument, RootOperationTypes) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    RootOperationTypes :: [argo_graphql_service_document:operation_type()].
service_document_generate_root_operation_types(
    ServiceDocument1 = #argo_graphql_service_document{'query' = {some, TypeName}, type_definitions = TypeDefinitions1},
    ['query' | RootOperationTypes]
) ->
    TypeDefinitions2 = maps:remove(TypeName, TypeDefinitions1),
    ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{type_definitions = TypeDefinitions2},
    ?LET(
        {ServiceDocument3, _ObjectTypeDefinition},
        type_definition(ServiceDocument2, TypeName, #{kinds => [object], must_be_new => true}),
        service_document_generate_root_operation_types(ServiceDocument3, RootOperationTypes)
    );
service_document_generate_root_operation_types(
    ServiceDocument1 = #argo_graphql_service_document{
        'mutation' = {some, TypeName}, type_definitions = TypeDefinitions1
    },
    ['mutation' | RootOperationTypes]
) ->
    TypeDefinitions2 = maps:remove(TypeName, TypeDefinitions1),
    ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{type_definitions = TypeDefinitions2},
    ?LET(
        {ServiceDocument3, _ObjectTypeDefinition},
        type_definition(ServiceDocument2, TypeName, #{kinds => [object], must_be_new => true}),
        service_document_generate_root_operation_types(ServiceDocument3, RootOperationTypes)
    );
service_document_generate_root_operation_types(
    ServiceDocument1 = #argo_graphql_service_document{
        'subscription' = {some, TypeName}, type_definitions = TypeDefinitions1
    },
    ['subscription' | RootOperationTypes]
) ->
    TypeDefinitions2 = maps:remove(TypeName, TypeDefinitions1),
    ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{type_definitions = TypeDefinitions2},
    ?LET(
        {ServiceDocument3, _ObjectTypeDefinition},
        type_definition(ServiceDocument2, TypeName, #{kinds => [object], must_be_new => true}),
        service_document_generate_root_operation_types(ServiceDocument3, RootOperationTypes)
    );
service_document_generate_root_operation_types(ServiceDocument1 = #argo_graphql_service_document{}, [
    _ | RootOperationTypes
]) ->
    service_document_generate_root_operation_types(ServiceDocument1, RootOperationTypes);
service_document_generate_root_operation_types(ServiceDocument1 = #argo_graphql_service_document{}, []) ->
    exactly(ServiceDocument1).

%% @private
-spec service_document_is_instance(RawType :: proper_types:raw_type()) -> proper_types:type().
service_document_is_instance(RawType) ->
    ?SUCHTHAT(Type, RawType, service_document_is_valid(Type)).

%% @private
-spec service_document_is_valid(argo_graphql_service_document:t()) -> boolean().
service_document_is_valid(#argo_graphql_service_document{'query' = none, mutation = none, subscription = none}) ->
    false;
service_document_is_valid(#argo_graphql_service_document{'query' = Q, mutation = M, subscription = S}) ->
    Q =/= M andalso M =/= S andalso Q =/= S.

-spec type(TypeName) -> proper_types:type() when TypeName :: argo_types:name().
type(TypeName) when is_binary(TypeName) ->
    oneof([
        exactly(argo_graphql_type:named_type(TypeName)),
        exactly(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(TypeName))),
        list_type(TypeName),
        ?LET(
            #argo_graphql_type{inner = ListType = #argo_graphql_list_type{}},
            list_type(TypeName),
            exactly(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(ListType)))
        )
    ]).

-spec type_definition(ServiceDocument, Options | TypeName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    Options :: type_definition_options(),
    TypeName :: argo_types:name().
type_definition(ServiceDocument = #argo_graphql_service_document{type_definitions = TypeDefinitions}, Options) when
    is_map(Options)
->
    % ExcludeTypeNames1 = maps:get(exclude, Options, #{}),
    % NewTypeDefinitionGen = ?LAZY(
    %     ?LET(
    %         TypeName,
    %         unique_type_name(ServiceDocument, ExcludeTypeNames1),
    %         complex(type_definition(ServiceDocument, TypeName, Options))
    %     )
    % ),
    NewTypeDefinitionGen = ?LAZY(
        ?LET(
            TypeName,
            type_name(ServiceDocument, Options),
            complex(type_definition(ServiceDocument, TypeName, Options))
        )
    ),
    MustBeNew = maps:get(must_be_new, Options, false),
    case MustBeNew of
        false ->
            ChanceOfExistingType = max(1 bsl complexity(), 1 + maps:size(TypeDefinitions)),
            Kinds = type_definition_option_kinds(Options),
            case type_definition_list(ServiceDocument, Options) of
                [] when is_map_key(scalar, Kinds) ->
                    BuiltinTypeDefinitionGen = ?LET(
                        TypeName, builtin_scalar_type_name(), type_definition(ServiceDocument, TypeName, Options)
                    ),
                    frequency([
                        {ChanceOfExistingType, BuiltinTypeDefinitionGen},
                        {1, NewTypeDefinitionGen}
                    ]);
                [] ->
                    NewTypeDefinitionGen;
                TypeDefinitionList = [_ | _] ->
                    ExistingTypeDefinitionGen = ?LET(
                        TypeDefinition, oneof(TypeDefinitionList), {ServiceDocument, TypeDefinition}
                    ),
                    frequency([
                        {ChanceOfExistingType, ExistingTypeDefinitionGen},
                        {1, NewTypeDefinitionGen}
                    ])
            end;
        true ->
            NewTypeDefinitionGen
    end;
type_definition(ServiceDocument, TypeName) when is_binary(TypeName) ->
    type_definition(ServiceDocument, TypeName, #{}).

-spec type_definition(ServiceDocument, TypeName, Options) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeName :: argo_types:name(),
    Options :: type_definition_options().
type_definition(ServiceDocument1, TypeName, Options) ->
    Kinds = type_definition_option_kinds(Options),
    MustBeNew = maps:get(must_be_new, Options, false),
    Shape = maps:get(shape, Options, none),
    case argo_graphql_service_document:find_type_definition(ServiceDocument1, TypeName) of
        {ok, _} when MustBeNew =:= true ->
            error_with_info(badarg, [ServiceDocument1, TypeName, Options], #{
                2 => {must_be_new_violation, #{type_name => TypeName}}
            });
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_scalar_type_definition{}}} when
            is_map_key(scalar, Kinds)
        ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}}} when
            is_map_key(object, Kinds)
        ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_interface_type_definition{}}} when
            is_map_key(interface, Kinds)
        ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_union_type_definition{}}} when
            is_map_key(union, Kinds)
        ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_enum_type_definition{}}} when
            is_map_key(enum, Kinds)
        ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_input_object_type_definition{}}} when
            is_map_key(input_object, Kinds)
        ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, _} ->
            error_with_info(badarg, [ServiceDocument1, TypeName, Options], #{
                2 => {kinds_violation, #{kinds => Kinds, type_name => TypeName}}
            });
        error ->
            case TypeName of
                <<"scalar", _/bytes>> ->
                    ?LAZY(scalar_type_definition(ServiceDocument1, TypeName));
                <<"object", _/bytes>> ->
                    ?LAZY(object_type_definition(ServiceDocument1, TypeName, Shape));
                <<"interface", _/bytes>> ->
                    ?LAZY(interface_type_definition(ServiceDocument1, TypeName, Shape));
                <<"union", _/bytes>> ->
                    ?LAZY(union_type_definition(ServiceDocument1, TypeName));
                <<"enum", _/bytes>> ->
                    ?LAZY(enum_type_definition(ServiceDocument1, TypeName));
                <<"input_object", _/bytes>> ->
                    ?LAZY(input_object_type_definition(ServiceDocument1, TypeName));
                _ ->
                    ?LET(
                        Kind,
                        oneof(maps:keys(Kinds)),
                        case Kind of
                            scalar ->
                                ?LAZY(scalar_type_definition(ServiceDocument1, TypeName));
                            object ->
                                ?LAZY(object_type_definition(ServiceDocument1, TypeName, Shape));
                            interface ->
                                ?LAZY(interface_type_definition(ServiceDocument1, TypeName, Shape));
                            union ->
                                ?LAZY(union_type_definition(ServiceDocument1, TypeName));
                            enum ->
                                ?LAZY(enum_type_definition(ServiceDocument1, TypeName));
                            input_object ->
                                ?LAZY(input_object_type_definition(ServiceDocument1, TypeName))
                        end
                    )
            end
    end.

%% @private
-spec type_definition_list(ServiceDocument, Options) -> TypeDefinitionList when
    ServiceDocument :: argo_graphql_service_document:t(),
    Options :: type_definition_options(),
    TypeDefinitionList :: [TypeDefinition],
    TypeDefinition :: argo_graphql_type_definition:t().
type_definition_list(
    ServiceDocument = #argo_graphql_service_document{type_definitions = TypeDefinitionsMap}, Options
) when is_map(Options) ->
    MustBeNew = maps:get(must_be_new, Options, false),
    case MustBeNew of
        false ->
            ExcludeTypeNames1 = maps:get(exclude, Options, #{}),
            ExcludeTypeNames2 =
                case maps:get(include_root_operation_types, Options, false) of
                    false ->
                        OptionQuery = ServiceDocument#argo_graphql_service_document.'query',
                        OptionMutation = ServiceDocument#argo_graphql_service_document.mutation,
                        OptionSubscription = ServiceDocument#argo_graphql_service_document.subscription,
                        maps:merge(
                            ExcludeTypeNames1,
                            #{
                                RootOperationTypeName => []
                             || {some, RootOperationTypeName} <- [OptionQuery, OptionMutation, OptionSubscription]
                            }
                        );
                    true ->
                        ExcludeTypeNames1
                end,
            Kinds = type_definition_option_kinds(Options),
            Shape = maps:get(shape, Options, none),
            TypeDefinitionList1 = type_definition_list(maps:to_list(TypeDefinitionsMap), Kinds, ExcludeTypeNames2, []),
            TypeDefinitionList2 =
                case Shape of
                    none ->
                        TypeDefinitionList1;
                    #{type := _} ->
                        type_definition_list_shape_filter(TypeDefinitionList1, Shape, [])
                end,
            TypeDefinitionList2;
        true ->
            []
    end.

%% @private
-spec type_definition_list([{TypeName, TypeDefinition}], Kinds, ExcludeTypeNames, Acc) -> Acc when
    TypeName :: argo_types:name(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    Kinds :: #{type_definition_kind() => []},
    ExcludeTypeNames :: #{TypeName => []},
    Acc :: [TypeDefinition].
type_definition_list([{TypeName, _TypeDefinition} | TypeDefinitions], Kinds, ExcludeTypeNames, Acc) when
    is_map_key(TypeName, ExcludeTypeNames)
->
    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, Acc);
type_definition_list(
    [{_TypeName, TypeDefinition = #argo_graphql_type_definition{kind = Kind}} | TypeDefinitions],
    Kinds,
    ExcludeTypeNames,
    Acc
) ->
    case Kind of
        #argo_graphql_scalar_type_definition{} when is_map_key(scalar, Kinds) ->
            type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, [TypeDefinition | Acc]);
        #argo_graphql_object_type_definition{fields = FieldsMap} when is_map_key(object, Kinds) ->
            case argo_index_map:size(FieldsMap) of
                0 ->
                    % exclude empty ObjectTypeDefinition
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, Acc);
                _ ->
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, [TypeDefinition | Acc])
            end;
        #argo_graphql_interface_type_definition{fields = FieldsMap} when
            is_map_key(interface, Kinds)
        ->
            case argo_index_map:size(FieldsMap) of
                0 ->
                    % exclude empty InterfaceTypeDefinition
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, Acc);
                _ ->
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, [TypeDefinition | Acc])
            end;
        #argo_graphql_union_type_definition{types = TypesSet} when is_map_key(union, Kinds) ->
            case argo_index_set:size(TypesSet) of
                0 ->
                    % exclude empty UnionTypeDefinition
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, Acc);
                _ ->
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, [TypeDefinition | Acc])
            end;
        #argo_graphql_enum_type_definition{} when is_map_key(enum, Kinds) ->
            type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, [TypeDefinition | Acc]);
        #argo_graphql_input_object_type_definition{inputs = InputsMap} when
            is_map_key(input_object, Kinds)
        ->
            case argo_index_map:size(InputsMap) of
                0 ->
                    % exclude empty InputObjectTypeDefinition
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, Acc);
                _ ->
                    type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, [TypeDefinition | Acc])
            end;
        _ ->
            type_definition_list(TypeDefinitions, Kinds, ExcludeTypeNames, Acc)
    end;
type_definition_list([], _Kinds, _ExcludeTypeNames, Acc) ->
    Acc.

%% @private
-spec type_definition_list_shape_filter(TypeDefinitionList, Shape, Acc) -> Acc when
    TypeDefinitionList :: [TypeDefinition],
    TypeDefinition :: argo_graphql_type_definition:t(),
    Shape :: argo_graphql_type_definition:shape(),
    Acc :: [TypeDefinition].
type_definition_list_shape_filter([TypeDefinition | TypeDefinitionList], Shape, Acc) ->
    case type_definition_shape_is_match(TypeDefinition, Shape) of
        false ->
            type_definition_list_shape_filter(TypeDefinitionList, Shape, Acc);
        true ->
            type_definition_list_shape_filter(TypeDefinitionList, Shape, [TypeDefinition | Acc])
    end;
type_definition_list_shape_filter([], _Shape, Acc) ->
    Acc.

%% @private
-spec type_definition_option_kinds(Options) -> Kinds when
    Options :: type_definition_options(), Kinds :: #{type_definition_kind() => []}.
type_definition_option_kinds(Options) ->
    Kinds = #{Kind => [] || Kind <- [scalar, object, interface, union, enum, input_object]},
    case maps:get(kinds, Options, output) of
        output ->
            maps:with([scalar, object, interface, union, enum], Kinds);
        input ->
            maps:with([scalar, enum, input_object], Kinds);
        KindsList when is_list(KindsList) andalso length(KindsList) > 0 ->
            Invalid = lists:foldl(
                fun
                    (Kind, InvalidAcc) when is_map_key(Kind, Kinds) ->
                        InvalidAcc;
                    (Kind, InvalidAcc) ->
                        InvalidAcc#{Kind => []}
                end,
                maps:new(),
                KindsList
            ),
            case maps:size(Invalid) of
                0 ->
                    maps:with(KindsList, Kinds);
                _ ->
                    error_with_info(badarg, [Options], #{
                        1 => {invalid_kinds_option, #{allowed => Kinds, invalid => Invalid}}
                    })
            end
    end.

%% @private
-spec type_definition_shape_is_match(TypeDefinition, Shape) -> boolean() when
    TypeDefinition :: argo_graphql_type_definition:t(), Shape :: argo_graphql_type_definition:shape().
type_definition_shape_is_match(
    _TypeDefinition = #argo_graphql_type_definition{kind = Kind}, Shape = #{type := interface}
) ->
    case Kind of
        #argo_graphql_object_type_definition{} ->
            ObjectShape = argo_graphql_object_type_definition:get_shape(Kind),
            maps:fold(
                fun
                    (_FieldName, _FieldType, false) ->
                        false;
                    (FieldName, FieldType, true) ->
                        case maps:find(FieldName, ObjectShape) of
                            {ok, FieldType} ->
                                true;
                            {ok, _ExistingFieldType} ->
                                false;
                            error ->
                                false
                        end
                end,
                true,
                maps:without([type], Shape)
            );
        #argo_graphql_interface_type_definition{} ->
            InterfaceShape = argo_graphql_interface_type_definition:get_shape(Kind),
            maps:fold(
                fun
                    (_FieldName, _FieldType, false) ->
                        false;
                    (FieldName, FieldType, true) ->
                        case maps:find(FieldName, InterfaceShape) of
                            {ok, FieldType} ->
                                true;
                            {ok, _ExistingFieldType} ->
                                false;
                            error ->
                                false
                        end
                end,
                true,
                maps:without([type], Shape)
            );
        _ ->
            false
    end;
type_definition_shape_is_match(_TypeDefinition = #argo_graphql_type_definition{kind = Kind}, Shape = #{type := union}) ->
    case Kind of
        #argo_graphql_object_type_definition{} ->
            ObjectShape = argo_graphql_object_type_definition:get_shape(Kind),
            maps:fold(
                fun
                    (_FieldName, _FieldType, false) ->
                        false;
                    (FieldName, FieldType, true) ->
                        case maps:find(FieldName, ObjectShape) of
                            {ok, FieldType} ->
                                true;
                            {ok, _ExistingFieldType} ->
                                false;
                            error ->
                                true
                        end
                end,
                true,
                maps:without([type], Shape)
            );
        _ ->
            false
    end.

-spec type_kind(ServiceDocument, Options) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), Options :: type_definition_options().
type_kind(_ServiceDocument = #argo_graphql_service_document{type_definitions = TypeDefinitions}, Options) when
    is_map(Options)
->
    % TypeReport = type_report(ServiceDocument),
    Size = maps:size(TypeDefinitions),
    Kinds = type_definition_option_kinds(Options),
    KindWeights = #{
        scalar => 1 + Size,
        object => 1,
        interface => 1,
        union => 1,
        enum => 1,
        input_object => 1
    },
    Weights = [{Weight, Kind} || Kind := Weight <- KindWeights, is_map_key(Kind, Kinds)],
    frequency(Weights).

-spec type_name(ServiceDocument, Options) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), Options :: type_definition_options().
type_name(ServiceDocument = #argo_graphql_service_document{}, Options) when is_map(Options) ->
    TypeReport = type_report(ServiceDocument),
    ?LET(
        Kind,
        type_kind(ServiceDocument, Options),
        begin
            Count = maps:size(maps:get(Kind, TypeReport)),
            unicode:characters_to_binary(io_lib:format("~ts_~w", [Kind, Count]), utf8)
        end
    ).

-spec type_report(ServiceDocument) -> TypeReport when
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeReport :: #{
        scalar := TypeNames,
        object := TypeNames,
        interface := TypeNames,
        union := TypeNames,
        enum := TypeNames,
        input_object := TypeNames
    },
    TypeNames :: #{argo_types:name() => []}.
type_report(ServiceDocument = #argo_graphql_service_document{}) ->
    Report1 = #{
        scalar => maps:new(),
        object => maps:new(),
        interface => maps:new(),
        union => maps:new(),
        enum => maps:new(),
        input_object => maps:new()
    },
    {_, Report2} =
        argo_graphql:xform(ServiceDocument, Report1, fun
            (#argo_graphql_directive_definition{}, Report1_Acc1) ->
                {skip, Report1_Acc1};
            (#argo_graphql_type_definition{name = TypeName, kind = Kind}, Report1_Acc1) ->
                case Kind of
                    #argo_graphql_scalar_type_definition{} ->
                        {skip,
                            maps:update_with(scalar, fun(TypeNames) -> TypeNames#{TypeName => []} end, Report1_Acc1)};
                    #argo_graphql_object_type_definition{} ->
                        {skip,
                            maps:update_with(object, fun(TypeNames) -> TypeNames#{TypeName => []} end, Report1_Acc1)};
                    #argo_graphql_interface_type_definition{} ->
                        {skip,
                            maps:update_with(interface, fun(TypeNames) -> TypeNames#{TypeName => []} end, Report1_Acc1)};
                    #argo_graphql_union_type_definition{} ->
                        {skip, maps:update_with(union, fun(TypeNames) -> TypeNames#{TypeName => []} end, Report1_Acc1)};
                    #argo_graphql_enum_type_definition{} ->
                        {skip, maps:update_with(enum, fun(TypeNames) -> TypeNames#{TypeName => []} end, Report1_Acc1)};
                    #argo_graphql_input_object_type_definition{} ->
                        {skip,
                            maps:update_with(
                                input_object, fun(TypeNames) -> TypeNames#{TypeName => []} end, Report1_Acc1
                            )}
                end;
            (_, Report1_Acc1) ->
                {cont, Report1_Acc1}
        end),
    Report2.

%% @private
-spec union_member_type_definition(ServiceDocument, UnionMemberShape, ExcludeUnionMemberTypes) ->
    proper_types:type()
when
    ServiceDocument :: argo_graphql_service_document:t(),
    UnionMemberShape :: argo_graphql_union_type_definition:shape(),
    ExcludeUnionMemberTypes :: #{argo_types:name() => []}.
union_member_type_definition(
    ServiceDocument1 = #argo_graphql_service_document{}, UnionMemberShape1 = #{type := union}, ExcludeUnionMemberTypes1
) when is_map(ExcludeUnionMemberTypes1) ->
    ?LET(
        {ServiceDocument2, UnionMemberTypeDefinition = #argo_graphql_type_definition{name = UnionMemberType}},
        type_definition(ServiceDocument1, #{
            exclude => ExcludeUnionMemberTypes1, kinds => [object], shape => UnionMemberShape1
        }),
        begin
            UnionMemberShape2 = union_member_type_definition_merge_object_shape(
                ServiceDocument2, UnionMemberShape1, UnionMemberTypeDefinition
            ),
            ExcludeUnionMemberTypes2 = ExcludeUnionMemberTypes1#{UnionMemberType => []},
            {ServiceDocument2, UnionMemberShape2, ExcludeUnionMemberTypes2, UnionMemberTypeDefinition}
        end
    ).

union_member_type_definition_merge_object_shape(
    ServiceDocument, UnionMemberShape1, ObjectTypeDefinition = #argo_graphql_type_definition{name = ObjectTypeName}
) ->
    ObjectShape = argo_graphql_service_document:get_shape(ServiceDocument, ObjectTypeName),
    UnionMemberShape2 = maps:fold(
        fun(Name, Type, UnionMemberShape1_Acc1) ->
            case maps:find(Name, UnionMemberShape1_Acc1) of
                {ok, Type} ->
                    UnionMemberShape1_Acc1;
                {ok, ExistingType} ->
                    error_with_info(badarg, [ServiceDocument, UnionMemberShape1, ObjectTypeDefinition], #{
                        3 =>
                            {union_shape_mismatch, #{
                                union_member_type => ObjectTypeName,
                                field_name => Name,
                                existing_type => ExistingType,
                                type => Type
                            }}
                    });
                error ->
                    maps:put(Name, Type, UnionMemberShape1_Acc1)
            end
        end,
        UnionMemberShape1,
        maps:without([type], ObjectShape)
    ),
    UnionMemberShape2.

%% @private
-spec union_member_type_definition_list(ServiceDocument, UnionMemberShape, ExistingUnionMemberTypes) ->
    proper_types:type()
when
    ServiceDocument :: argo_graphql_service_document:t(),
    UnionMemberShape :: argo_graphql_union_type_definition:shape(),
    ExistingUnionMemberTypes :: #{argo_types:name() => []}.
union_member_type_definition_list(
    ServiceDocument1 = #argo_graphql_service_document{}, UnionMemberShape1 = #{type := union}, ExcludeUnionMemberTypes1
) when is_map(ExcludeUnionMemberTypes1) ->
    complex(
        ?LET(
            {ServiceDocument2, UnionMemberShape2, ExcludeUnionMemberTypes2, InitialUnionMemberTypeDefinition},
            union_member_type_definition(ServiceDocument1, UnionMemberShape1, ExcludeUnionMemberTypes1),
            ?LET(
                Size,
                minsized(mostly(range(0, 2), range(3, 8))),
                union_member_type_definition_list(
                    Size,
                    ServiceDocument2,
                    InitialUnionMemberTypeDefinition,
                    UnionMemberShape2,
                    ExcludeUnionMemberTypes2
                )
            )
        )
    ).

%% @private
-spec union_member_type_definition_list(
    Size, ServiceDocument, PreviousUnionMemberTypeDefinition, UnionMemberShape, ExcludeUnionMemberTypes
) -> proper_types:type() when
    Size :: non_neg_integer(),
    ServiceDocument :: argo_graphql_service_document:t(),
    PreviousUnionMemberTypeDefinition :: argo_graphql_type_definition:t(),
    UnionMemberShape :: argo_graphql_union_type_definition:shape(),
    ExcludeUnionMemberTypes :: #{argo_types:name() => []}.
union_member_type_definition_list(
    0, ServiceDocument1, PreviousUnionMemberTypeDefinition, _UnionMemberShape1, _ExcludeUnionMemberTypes
) ->
    exactly({ServiceDocument1, [PreviousUnionMemberTypeDefinition]});
union_member_type_definition_list(
    Size, ServiceDocument1, PreviousUnionMemberTypeDefinition, UnionMemberShape1, ExcludeUnionMemberTypes1
) when is_integer(Size) andalso Size > 0 ->
    ?LAZY(
        ?LET(
            {ServiceDocument2, UnionMemberShape2, ExcludeUnionMemberTypes2, NextUnionMemberTypeDefinition},
            union_member_type_definition(ServiceDocument1, UnionMemberShape1, ExcludeUnionMemberTypes1),
            ?LET(
                {ServiceDocument3, UnionMemberTypeDefinitionList},
                union_member_type_definition_list(
                    Size - 1,
                    ServiceDocument2,
                    NextUnionMemberTypeDefinition,
                    UnionMemberShape2,
                    ExcludeUnionMemberTypes2
                ),
                exactly({ServiceDocument3, [PreviousUnionMemberTypeDefinition | UnionMemberTypeDefinitionList]})
            )
        )
    ).

-spec union_type_definition(ServiceDocument, TypeName) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
union_type_definition(ServiceDocument1, TypeName) ->
    UnionTypeDefinition1 = argo_graphql_union_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:union_type_definition(TypeName, UnionTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    ?LET(
        {ServiceDocument3, UnionTypeDefinition2},
        ?LAZY(add_union_member_types(ServiceDocument2, UnionTypeDefinition1)),
        begin
            TypeDefinition2 = argo_graphql_type_definition:union_type_definition(TypeName, UnionTypeDefinition2),
            ServiceDocument4 = argo_graphql_service_document:replace_type_definition(ServiceDocument3, TypeDefinition2),
            {ServiceDocument4, TypeDefinition2}
        end
    ).

-spec value_const(ServiceDocument, Type) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(), Type :: argo_graphql_type:t().
value_const(ServiceDocument, Type = #argo_graphql_type{}) ->
    value_const(ServiceDocument, Type, argo_graphql_input_type_graph:new()).

value_const(ServiceDocument, Type = #argo_graphql_type{}, InputTypeGraph1) ->
    case Type of
        #argo_graphql_type{inner = NamedType} when is_binary(NamedType) ->
            InputTypeDefinition = argo_graphql_service_document:get_input_type_definition(ServiceDocument, NamedType),
            InputTypeGraph2 = argo_graphql_input_type_graph:add_input(InputTypeGraph1, NamedType),
            oneof([
                exactly(argo_graphql_value_const:null()),
                ?LAZY(complex(value_const(ServiceDocument, InputTypeDefinition, InputTypeGraph2)))
            ]);
        #argo_graphql_type{inner = #argo_graphql_list_type{type = NestedType}} ->
            oneof([
                exactly(argo_graphql_value_const:null()),
                ?LET(
                    ListValue,
                    ?LAZY(complex(mostly_small_list(value_const(ServiceDocument, NestedType, InputTypeGraph1)))),
                    argo_graphql_value_const:list(ListValue)
                )
            ]);
        #argo_graphql_type{inner = #argo_graphql_non_null_type{type = NamedType}} when is_binary(NamedType) ->
            InputTypeDefinition = argo_graphql_service_document:get_input_type_definition(ServiceDocument, NamedType),
            InputTypeGraph2 = argo_graphql_input_type_graph:add_input(InputTypeGraph1, NamedType),
            ?LAZY(complex(value_const(ServiceDocument, InputTypeDefinition, InputTypeGraph2)));
        #argo_graphql_type{inner = #argo_graphql_non_null_type{type = #argo_graphql_list_type{type = NestedType}}} ->
            ?LET(
                ListValue,
                ?LAZY(complex(mostly_small_list(value_const(ServiceDocument, NestedType, InputTypeGraph1)))),
                argo_graphql_value_const:list(ListValue)
            )
    end;
value_const(
    ServiceDocument, _TypeDefinition = #argo_graphql_type_definition{name = Name, kind = Kind}, InputTypeGraph1
) ->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            case Name of
                <<"Boolean">> ->
                    ?LET(V, boolean(), argo_graphql_value_const:boolean(V));
                <<"Float">> ->
                    ?LET(V, float(), argo_graphql_value_const:float(V));
                <<"Int">> ->
                    ?LET(V, integer(), argo_graphql_value_const:int(V));
                _ ->
                    ?LET(V, ?LANG:block_string_value(), argo_graphql_value_const:string(V))
            end;
        #argo_graphql_enum_type_definition{values = EnumValueDefinitionMap} ->
            ?LET(V, oneof(argo_index_map:keys(EnumValueDefinitionMap)), argo_graphql_value_const:enum(V));
        #argo_graphql_input_object_type_definition{inputs = InputsMap} ->
            {InputTypeGraph2, ReqInputs, OptInputs} =
                argo_index_map:foldl(
                    fun(_Index, _InputName, InputValueDefinition, {InputTypeGraph1_Acc1, ReqAcc, OptAcc}) ->
                        case argo_graphql_input_value_definition:is_required(InputValueDefinition) of
                            false ->
                                {InputTypeGraph1_Acc1, ReqAcc, [InputValueDefinition | OptAcc]};
                            true ->
                                Dependency = argo_graphql_type:get_type_name(
                                    InputValueDefinition#argo_graphql_input_value_definition.type
                                ),
                                InputTypeGraph1_Acc2 = argo_graphql_input_type_graph:add_dependency(
                                    InputTypeGraph1_Acc1, Name, Dependency
                                ),
                                {InputTypeGraph1_Acc2, [InputValueDefinition | ReqAcc], OptAcc}
                        end
                    end,
                    {InputTypeGraph1, [], []},
                    InputsMap
                ),
            ObjectValue1 = argo_index_map:new(),
            ?LET(
                ObjectValue2,
                value_const_object_value_required(ServiceDocument, InputTypeGraph2, ObjectValue1, ReqInputs),
                ?LET(
                    ObjectValue3,
                    value_const_object_value_optional(ServiceDocument, InputTypeGraph2, ObjectValue2, OptInputs),
                    argo_graphql_value_const:object(ObjectValue3)
                )
            )
    end.

value_const_object_value_optional(_ServiceDocument, _InputTypeGraph, ObjectValue, []) ->
    exactly(ObjectValue);
value_const_object_value_optional(ServiceDocument, InputTypeGraph, ObjectValue, InputValueDefinitionList) ->
    ?LET(
        List,
        list(oneof(InputValueDefinitionList)),
        value_const_object_value_required(ServiceDocument, InputTypeGraph, ObjectValue, lists:uniq(List))
    ).

value_const_object_value_required(_ServiceDocument, _InputTypeGraph, ObjectValue, []) ->
    exactly(ObjectValue);
value_const_object_value_required(ServiceDocument, InputTypeGraph, ObjectValue1, [
    InputValueDefinition | InputValueDefinitionList
]) ->
    #argo_graphql_input_value_definition{name = InputName, type = InputType} = InputValueDefinition,
    ?LAZY(
        ?LET(
            ValueConst,
            value_const(ServiceDocument, InputType, InputTypeGraph),
            begin
                ObjectValue2 = argo_index_map:put(InputName, ValueConst, ObjectValue1),
                value_const_object_value_required(
                    ServiceDocument, InputTypeGraph, ObjectValue2, InputValueDefinitionList
                )
            end
        )
    ).

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
format_error_description(_Key, {kinds_violation, #{kinds := Kinds, type_name := TypeName}}) ->
    io_lib:format("type named ~ts violated the kinds specified: ~0tp", [TypeName, lists:sort(maps:keys(Kinds))]);
format_error_description(_Key, {invalid_kinds_option, #{allowed := Kinds, invalid := Invalid}}) ->
    io_lib:format("invalid option for 'kinds', unsupported values: ~0tp (only ~0tp are allowed)", [
        lists:sort(maps:keys(Invalid)), lists:sort(maps:keys(Kinds))
    ]);
format_error_description(_Key, {must_be_new_violation, #{type_name := TypeName}}) ->
    io_lib:format(
        "must_be_new violation: generator for TypeDefinition called with TypeName ~0tp, but a TypeDefinition with this name already exists",
        [TypeName]
    );
format_error_description(
    _Key,
    {union_shape_mismatch, #{
        union_member_type := UnionMemberType, field_name := FieldName, existing_type := ExistingType, type := Type
    }}
) ->
    io_lib:format(
        "union shape mismatch for UnionMemberType ~0tp for FieldDefinition ~0tp (existing Type ~ts does not match the shape of Type ~ts)",
        [UnionMemberType, FieldName, argo_graphql:format(ExistingType), argo_graphql:format(Type)]
    );
format_error_description(_Key, Value) ->
    Value.
