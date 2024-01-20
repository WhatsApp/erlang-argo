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
-module(argo_graphql_service_document).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_file/1,
    from_language/1,
    from_string/1,
    new/0
]).
%% Instance API
-export([
    add_directive_const/2,
    add_directive_definition/2,
    add_language_definition/2,
    add_language_schema_definition/2,
    add_language_schema_extension/2,
    add_language_type_extension/2,
    add_root_operation_type_definition/3,
    add_type_definition/2,
    find_directive_definition/2,
    find_type_definition/2,
    get_directive_definition/2,
    get_input_type_definition/2,
    get_input_type_graph/1,
    get_input_type_graph/2,
    get_interface_type_definition/2,
    get_root_operation_type_definition/2,
    get_shape/2,
    get_type_definition/2,
    get_union_member_type_definition/2,
    replace_type_definition/2,
    resolve_type_definitions/1,
    set_description/2
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
-type operation_type() :: argo_graphql_language_root_operation_type_definition:operation_type().
-type t() :: #argo_graphql_service_document{}.

-export_type([
    operation_type/0,
    t/0
]).

%% Macros
-define(is_operation_type(T), ((T) =:= 'query' orelse (T) =:= 'mutation' orelse (T) =:= 'subscription')).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_file(Filename) -> ServiceDocument when Filename :: file:filename_all(), ServiceDocument :: t().
from_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Contents} ->
            from_string(Contents);
        {error, ReadError} ->
            error_with_info(badarg, [Filename], #{1 => {read_error, ReadError}})
    end.

-spec from_language(LanguageDocument) -> ServiceDocument when
    LanguageDocument :: argo_graphql_language_document:t(), ServiceDocument :: t().
from_language(#argo_graphql_language_document{definitions = LanguageDefinitionList}) ->
    ServiceDocument1 = new(),
    ServiceDocument2 = lists:foldl(
        fun(LanguageDefinition, ServiceDocument1Acc1) ->
            add_language_definition(ServiceDocument1Acc1, LanguageDefinition)
        end,
        ServiceDocument1,
        LanguageDefinitionList
    ),
    ServiceDocument3 = resolve_type_definitions(ServiceDocument2),
    ServiceDocument3.

-spec from_string(Input) -> ServiceDocument when Input :: unicode:chardata(), ServiceDocument :: t().
from_string(Input) ->
    String = argo_types:unicode_string(Input),
    case argo_graphql_language_scanner:string(String) of
        {ok, Tokens, _EndLoc} ->
            case argo_graphql_language_document:from_tokens(Tokens) of
                {ok, LanguageDocument = #argo_graphql_language_document{}} ->
                    from_language(LanguageDocument);
                {error, ParserError} ->
                    error_with_info(badarg, [Input], #{1 => {parser_error, ParserError}})
            end;
        {error, ErrorInfo, EndLoc} ->
            error_with_info(badarg, [Input], #{1 => {scanner_error, #{error_info => ErrorInfo, end_loc => EndLoc}}})
    end.

-spec new() -> ServiceDocument when ServiceDocument :: t().
new() ->
    #argo_graphql_service_document{
        description = none,
        directives = argo_graphql_directives_const:new(),
        schema_defined = false,
        'query' = none,
        mutation = none,
        subscription = none,
        directive_definitions = maps:new(),
        type_definitions = maps:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive_const(ServiceDocument, DirectiveConst) -> ServiceDocument when
    ServiceDocument :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    ServiceDocument1 = #argo_graphql_service_document{directives = DirectivesConst1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConst2 = argo_graphql_directives_const:add_directive_const(DirectivesConst1, DirectiveConst),
    ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{directives = DirectivesConst2},
    ServiceDocument2.

-spec add_directive_definition(ServiceDocument, DirectiveDefinition) -> ServiceDocument when
    ServiceDocument :: t(), DirectiveDefinition :: argo_graphql_directive_definition:t().
add_directive_definition(
    ServiceDocument1 = #argo_graphql_service_document{directive_definitions = DirectiveDefinitions1},
    DirectiveDefinition = #argo_graphql_directive_definition{name = DirectiveName}
) ->
    case maps:is_key(DirectiveName, DirectiveDefinitions1) of
        false ->
            DirectiveDefinitions2 = maps:put(DirectiveName, DirectiveDefinition, DirectiveDefinitions1),
            ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{
                directive_definitions = DirectiveDefinitions2
            },
            ServiceDocument2;
        true ->
            error_with_info(badarg, [ServiceDocument1, DirectiveDefinition], #{
                2 => {duplicate_directive_name, DirectiveName}
            })
    end.

-spec add_language_definition(ServiceDocument, LanguageDefinition) -> ServiceDocument when
    ServiceDocument :: t(), LanguageDefinition :: argo_graphql_language_definition:t().
add_language_definition(
    ServiceDocument1 = #argo_graphql_service_document{},
    LanguageDefinition = #argo_graphql_language_definition{inner = Inner}
) ->
    case Inner of
        #argo_graphql_language_executable_definition{} ->
            error_with_info(badarg, [ServiceDocument1, LanguageDefinition], #{2 => executable_definition_not_supported});
        #argo_graphql_language_type_system_definition{inner = LanguageTypeSystemDefinition} ->
            case LanguageTypeSystemDefinition of
                LanguageSchemaDefinition = #argo_graphql_language_schema_definition{} ->
                    add_language_schema_definition(ServiceDocument1, LanguageSchemaDefinition);
                LanguageTypeDefinition = #argo_graphql_language_type_definition{} ->
                    TypeDefinition = argo_graphql_type_definition:from_language(LanguageTypeDefinition),
                    ServiceDocument2 = add_type_definition(ServiceDocument1, TypeDefinition),
                    ServiceDocument2;
                LanguageDirectiveDefinition = #argo_graphql_language_directive_definition{} ->
                    DirectiveDefinition = argo_graphql_directive_definition:from_language(
                        LanguageDirectiveDefinition
                    ),
                    ServiceDocument2 = add_directive_definition(ServiceDocument1, DirectiveDefinition),
                    ServiceDocument2
            end;
        #argo_graphql_language_type_system_extension{inner = LanguageTypeSystemExtension} ->
            case LanguageTypeSystemExtension of
                LanguageSchemaExtension = #argo_graphql_language_schema_extension{} ->
                    add_language_schema_extension(ServiceDocument1, LanguageSchemaExtension);
                LanguageTypeExtension = #argo_graphql_language_type_extension{} ->
                    add_language_type_extension(ServiceDocument1, LanguageTypeExtension)
            end
    end.

-spec add_language_schema_definition(ServiceDocument, LanguageSchemaDefinition) -> ServiceDocument when
    ServiceDocument :: t(), LanguageSchemaDefinition :: argo_graphql_language_schema_definition:t().
add_language_schema_definition(
    ServiceDocument1 = #argo_graphql_service_document{schema_defined = false}, #argo_graphql_language_schema_definition{
        description = LanguageOptionDescription,
        directives = LanguageOptionDirectivesConst,
        operations = LanguageOptionOperations
    }
) ->
    ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{schema_defined = true},
    ServiceDocument3 = set_description(ServiceDocument2, LanguageOptionDescription),
    ServiceDocument4 =
        case LanguageOptionDirectivesConst of
            none ->
                ServiceDocument3;
            {some, LanguageDirectivesConst} ->
                DirectivesConst = argo_graphql_directives_const:from_language(LanguageDirectivesConst),
                ServiceDocument3#argo_graphql_service_document{directives = DirectivesConst}
        end,
    ServiceDocument5 =
        case LanguageOptionOperations of
            none ->
                ServiceDocument4;
            {some, #argo_graphql_language_root_operation_types_definition{operations = LanguageOperations}} ->
                lists:foldl(
                    fun(LanguageOperation, ServiceDocument4Acc1) ->
                        case LanguageOperation of
                            #argo_graphql_language_root_operation_type_definition{
                                operation_type = OperationType,
                                named_type = #argo_graphql_language_named_type{name = NamedType}
                            } ->
                                add_root_operation_type_definition(ServiceDocument4Acc1, OperationType, NamedType)
                        end
                    end,
                    ServiceDocument4,
                    LanguageOperations
                )
        end,
    ServiceDocument5;
add_language_schema_definition(
    ServiceDocument1 = #argo_graphql_service_document{schema_defined = true},
    LanguageSchemaDefinition = #argo_graphql_language_schema_definition{}
) ->
    error_with_info(badarg, [ServiceDocument1, LanguageSchemaDefinition], #{2 => schema_already_defined}).

-spec add_language_schema_extension(ServiceDocument, LanguageSchemaExtension) -> ServiceDocument when
    ServiceDocument :: t(), LanguageSchemaExtension :: argo_graphql_language_schema_extension:t().
add_language_schema_extension(
    ServiceDocument1 = #argo_graphql_service_document{schema_defined = true}, #argo_graphql_language_schema_extension{
        directives = LanguageOptionDirectivesConst, operations = LanguageOptionOperations
    }
) ->
    ServiceDocument2 =
        case LanguageOptionDirectivesConst of
            none ->
                ServiceDocument1;
            {some, LanguageDirectivesConst} ->
                #argo_graphql_directives_const{directives = DirectivesConstList} = argo_graphql_directives_const:from_language(
                    LanguageDirectivesConst
                ),
                lists:foldl(
                    fun(DirectiveConst, ServiceDocument1_Acc1) ->
                        add_directive_const(ServiceDocument1_Acc1, DirectiveConst)
                    end,
                    ServiceDocument1,
                    DirectivesConstList
                )
        end,
    ServiceDocument3 =
        case LanguageOptionOperations of
            none ->
                ServiceDocument2;
            {some, #argo_graphql_language_root_operation_types_definition{operations = LanguageOperations}} ->
                lists:foldl(
                    fun(LanguageOperation, ServiceDocument2Acc1) ->
                        case LanguageOperation of
                            #argo_graphql_language_root_operation_type_definition{
                                operation_type = OperationType,
                                named_type = #argo_graphql_language_named_type{name = NamedType}
                            } ->
                                add_root_operation_type_definition(ServiceDocument2Acc1, OperationType, NamedType)
                        end
                    end,
                    ServiceDocument2,
                    LanguageOperations
                )
        end,
    ServiceDocument3;
add_language_schema_extension(
    ServiceDocument1 = #argo_graphql_service_document{schema_defined = false},
    LanguageSchemaExtension = #argo_graphql_language_schema_extension{}
) ->
    error_with_info(badarg, [ServiceDocument1, LanguageSchemaExtension], #{2 => schema_not_defined}).

-spec add_language_type_extension(ServiceDocument, LanguageTypeExtension) -> ServiceDocument when
    ServiceDocument :: t(), LanguageTypeExtension :: argo_graphql_language_type_extension:t().
add_language_type_extension(
    ServiceDocument1 = #argo_graphql_service_document{type_definitions = TypeDefinitions1},
    LanguageTypeExtension = #argo_graphql_language_type_extension{inner = Inner}
) ->
    {TypeDefinition1, LanguageOptionDirectivesConst} =
        case Inner of
            #argo_graphql_language_scalar_type_extension{name = TypeName, directives = LOptDirsConst} ->
                case maps:find(TypeName, TypeDefinitions1) of
                    {ok, TD1 = #argo_graphql_type_definition{kind = #argo_graphql_scalar_type_definition{}}} ->
                        {TD1, LOptDirsConst};
                    {ok, _} ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {invalid_type_extension, #{type => TypeName, extend => scalar}}
                        });
                    error ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {missing_type_definition, #{type => TypeName}}
                        })
                end;
            #argo_graphql_language_object_type_extension{
                name = TypeName,
                implements = LanguageOptionImplements,
                directives = LOptDirsConst,
                fields = LanguageOptionFields
            } ->
                case maps:find(TypeName, TypeDefinitions1) of
                    {ok,
                        TD1 = #argo_graphql_type_definition{
                            kind = Kind1 = #argo_graphql_object_type_definition{}
                        }} ->
                        Kind2 =
                            case LanguageOptionImplements of
                                none ->
                                    Kind1;
                                {some, #argo_graphql_language_implements_interfaces{interfaces = LanguageInterfaceList}} ->
                                    lists:foldl(
                                        fun(#argo_graphql_language_named_type{name = InterfaceName}, Kind1_Acc1) ->
                                            argo_graphql_object_type_definition:add_interface(Kind1_Acc1, InterfaceName)
                                        end,
                                        Kind1,
                                        LanguageInterfaceList
                                    )
                            end,
                        Kind3 =
                            case LanguageOptionFields of
                                none ->
                                    Kind2;
                                {some, #argo_graphql_language_fields_definition{fields = LanguageFieldDefinitionList}} ->
                                    lists:foldl(
                                        fun(LanguageFieldDefinition, Kind2_Acc1) ->
                                            FieldDefinition = argo_graphql_field_definition:from_language(
                                                LanguageFieldDefinition
                                            ),
                                            argo_graphql_object_type_definition:add_field_definition(
                                                Kind2_Acc1, FieldDefinition
                                            )
                                        end,
                                        Kind2,
                                        LanguageFieldDefinitionList
                                    )
                            end,
                        TD2 = TD1#argo_graphql_type_definition{kind = Kind3},
                        {TD2, LOptDirsConst};
                    {ok, _} ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {invalid_type_extension, #{type => TypeName, extend => type}}
                        });
                    error ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {missing_type_definition, #{type => TypeName}}
                        })
                end;
            #argo_graphql_language_interface_type_extension{
                name = TypeName,
                implements = LanguageOptionImplements,
                directives = LOptDirsConst,
                fields = LanguageOptionFields
            } ->
                case maps:find(TypeName, TypeDefinitions1) of
                    {ok,
                        TD1 = #argo_graphql_type_definition{
                            kind = Kind1 = #argo_graphql_interface_type_definition{}
                        }} ->
                        Kind2 =
                            case LanguageOptionImplements of
                                none ->
                                    Kind1;
                                {some, #argo_graphql_language_implements_interfaces{interfaces = LanguageInterfaceList}} ->
                                    lists:foldl(
                                        fun(#argo_graphql_language_named_type{name = InterfaceName}, Kind1_Acc1) ->
                                            argo_graphql_interface_type_definition:add_interface(
                                                Kind1_Acc1, InterfaceName
                                            )
                                        end,
                                        Kind1,
                                        LanguageInterfaceList
                                    )
                            end,
                        Kind3 =
                            case LanguageOptionFields of
                                none ->
                                    Kind2;
                                {some, #argo_graphql_language_fields_definition{fields = LanguageFieldDefinitionList}} ->
                                    lists:foldl(
                                        fun(LanguageFieldDefinition, Kind2_Acc1) ->
                                            FieldDefinition = argo_graphql_field_definition:from_language(
                                                LanguageFieldDefinition
                                            ),
                                            argo_graphql_interface_type_definition:add_field_definition(
                                                Kind2_Acc1, FieldDefinition
                                            )
                                        end,
                                        Kind2,
                                        LanguageFieldDefinitionList
                                    )
                            end,
                        TD2 = TD1#argo_graphql_type_definition{kind = Kind3},
                        {TD2, LOptDirsConst};
                    {ok, _} ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {invalid_type_extension, #{type => TypeName, extend => interface}}
                        });
                    error ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {missing_type_definition, #{type => TypeName}}
                        })
                end;
            #argo_graphql_language_union_type_extension{
                name = TypeName, directives = LOptDirsConst, types = LanguageOptionUnionMemberTypeList
            } ->
                case maps:find(TypeName, TypeDefinitions1) of
                    {ok, TD1 = #argo_graphql_type_definition{kind = Kind1 = #argo_graphql_union_type_definition{}}} ->
                        Kind2 =
                            case LanguageOptionUnionMemberTypeList of
                                none ->
                                    Kind1;
                                {some, []} ->
                                    Kind1;
                                {some, LanguageUnionMemberTypeList} ->
                                    lists:foldl(
                                        fun(#argo_graphql_language_named_type{name = UnionMemberType}, Kind1_Acc1) ->
                                            argo_graphql_union_type_definition:add_union_member_type(
                                                Kind1_Acc1, UnionMemberType
                                            )
                                        end,
                                        Kind1,
                                        LanguageUnionMemberTypeList
                                    )
                            end,
                        TD2 = TD1#argo_graphql_type_definition{kind = Kind2},
                        {TD2, LOptDirsConst};
                    {ok, _} ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {invalid_type_extension, #{type => TypeName, extend => union}}
                        });
                    error ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {missing_type_definition, #{type => TypeName}}
                        })
                end;
            #argo_graphql_language_enum_type_extension{
                name = TypeName,
                directives = LOptDirsConst,
                values = LanguageOptionEnumValuesDefinition
            } ->
                case maps:find(TypeName, TypeDefinitions1) of
                    {ok, TD1 = #argo_graphql_type_definition{kind = Kind1 = #argo_graphql_enum_type_definition{}}} ->
                        Kind2 =
                            case LanguageOptionEnumValuesDefinition of
                                none ->
                                    Kind1;
                                {some, #argo_graphql_language_enum_values_definition{values = []}} ->
                                    Kind1;
                                {some, #argo_graphql_language_enum_values_definition{
                                    values = LanguageEnumValueDefinitionList
                                }} ->
                                    lists:foldl(
                                        fun(LanguageEnumValueDefinition, Kind1_Acc1) ->
                                            EnumValueDefinition = argo_graphql_enum_value_definition:from_language(
                                                LanguageEnumValueDefinition
                                            ),
                                            argo_graphql_enum_type_definition:add_enum_value_definition(
                                                Kind1_Acc1, EnumValueDefinition
                                            )
                                        end,
                                        Kind1,
                                        LanguageEnumValueDefinitionList
                                    )
                            end,
                        TD2 = TD1#argo_graphql_type_definition{kind = Kind2},
                        {TD2, LOptDirsConst};
                    {ok, _} ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {invalid_type_extension, #{type => TypeName, extend => enum}}
                        });
                    error ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {missing_type_definition, #{type => TypeName}}
                        })
                end;
            #argo_graphql_language_input_object_type_extension{
                name = TypeName,
                directives = LOptDirsConst,
                fields = LanguageOptionInputFields
            } ->
                case maps:find(TypeName, TypeDefinitions1) of
                    {ok,
                        TD1 = #argo_graphql_type_definition{
                            kind = Kind1 = #argo_graphql_input_object_type_definition{}
                        }} ->
                        Kind2 =
                            case LanguageOptionInputFields of
                                none ->
                                    Kind1;
                                {some, #argo_graphql_language_input_fields_definition{
                                    inputs = LanguageInputValueDefinitionList
                                }} ->
                                    lists:foldl(
                                        fun(LanguageInputValueDefinition, Kind1_Acc1) ->
                                            InputValueDefinition = argo_graphql_input_value_definition:from_language(
                                                LanguageInputValueDefinition
                                            ),
                                            argo_graphql_input_object_type_definition:add_input_value_definition(
                                                Kind1_Acc1, InputValueDefinition
                                            )
                                        end,
                                        Kind1,
                                        LanguageInputValueDefinitionList
                                    )
                            end,
                        TD2 = TD1#argo_graphql_type_definition{kind = Kind2},
                        {TD2, LOptDirsConst};
                    {ok, _} ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {invalid_type_extension, #{type => TypeName, extend => input}}
                        });
                    error ->
                        error_with_info(badarg, [ServiceDocument1, LanguageTypeExtension], #{
                            2 => {missing_type_definition, #{type => TypeName}}
                        })
                end
        end,
    TypeDefinition2 =
        case LanguageOptionDirectivesConst of
            none ->
                TypeDefinition1;
            {some, LanguageDirectivesConst} ->
                #argo_graphql_directives_const{directives = DirectiveConstList} = argo_graphql_directives_const:from_language(
                    LanguageDirectivesConst
                ),
                lists:foldl(
                    fun(DirectiveConst, TypeDefinition1_Acc1) ->
                        argo_graphql_type_definition:add_directive_const(
                            TypeDefinition1_Acc1, DirectiveConst
                        )
                    end,
                    TypeDefinition1,
                    DirectiveConstList
                )
        end,
    TypeDefinitions2 = maps:put(TypeDefinition2#argo_graphql_type_definition.name, TypeDefinition2, TypeDefinitions1),
    ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{
        type_definitions = TypeDefinitions2
    },
    ServiceDocument2.

-spec add_root_operation_type_definition(ServiceDocument, OperationType, NamedType) -> ServiceDocument when
    ServiceDocument :: t(), OperationType :: operation_type(), NamedType :: argo_types:name().
add_root_operation_type_definition(ServiceDocument1 = #argo_graphql_service_document{}, OperationType, NamedType) when
    ?is_operation_type(OperationType) andalso is_binary(NamedType)
->
    case ServiceDocument1 of
        #argo_graphql_service_document{'query' = none} when OperationType =:= 'query' ->
            ServiceDocument1#argo_graphql_service_document{'query' = {some, NamedType}};
        #argo_graphql_service_document{'mutation' = none} when OperationType =:= 'mutation' ->
            ServiceDocument1#argo_graphql_service_document{'mutation' = {some, NamedType}};
        #argo_graphql_service_document{'subscription' = none} when OperationType =:= 'subscription' ->
            ServiceDocument1#argo_graphql_service_document{'subscription' = {some, NamedType}};
        _ ->
            error_with_info(badarg, [ServiceDocument1, OperationType, NamedType], #{
                2 => {duplicate_root_operation_type_definition, OperationType}
            })
    end.

-spec add_type_definition(ServiceDocument, TypeDefinition) -> ServiceDocument when
    ServiceDocument :: t(), TypeDefinition :: argo_graphql_type_definition:t().
add_type_definition(
    ServiceDocument1 = #argo_graphql_service_document{type_definitions = TypeDefinitions1},
    TypeDefinition = #argo_graphql_type_definition{name = TypeName}
) ->
    case maps:is_key(TypeName, TypeDefinitions1) of
        false ->
            TypeDefinitions2 = maps:put(TypeName, TypeDefinition, TypeDefinitions1),
            ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{type_definitions = TypeDefinitions2},
            ServiceDocument2;
        true ->
            error_with_info(badarg, [ServiceDocument1, TypeDefinition], #{2 => {duplicate_type_name, TypeName}})
    end.

-spec find_directive_definition(ServiceDocument, DirectiveName) -> {ok, DirectiveDefinition} | error when
    ServiceDocument :: t(),
    DirectiveName :: argo_types:name(),
    DirectiveDefinition :: argo_graphql_directive_definition:t().
find_directive_definition(
    #argo_graphql_service_document{directive_definitions = DirectiveDefinitions}, DirectiveName
) when is_binary(DirectiveName) ->
    case maps:find(DirectiveName, DirectiveDefinitions) of
        {ok, DirectiveDefinition} ->
            {ok, DirectiveDefinition};
        error ->
            argo_graphql_directive_definition:builtin(DirectiveName)
    end.

-spec find_type_definition(ServiceDocument, TypeName) -> {ok, TypeDefinition} | error when
    ServiceDocument :: t(), TypeName :: argo_types:name(), TypeDefinition :: argo_graphql_type_definition:t().
find_type_definition(#argo_graphql_service_document{type_definitions = TypeDefinitions}, TypeName) when
    is_binary(TypeName)
->
    case maps:find(TypeName, TypeDefinitions) of
        {ok, TypeDefinition} ->
            {ok, TypeDefinition};
        error ->
            argo_graphql_type_definition:builtin(TypeName)
    end.

-spec get_directive_definition(ServiceDocument, DirectiveName) -> DirectiveDefinition when
    ServiceDocument :: t(),
    DirectiveName :: argo_types:name(),
    DirectiveDefinition :: argo_graphql_directive_definition:t().
get_directive_definition(ServiceDocument = #argo_graphql_service_document{}, DirectiveName) when
    is_binary(DirectiveName)
->
    case find_directive_definition(ServiceDocument, DirectiveName) of
        {ok, DirectiveDefinition} ->
            DirectiveDefinition;
        error ->
            error_with_info(badarg, [ServiceDocument, DirectiveName], #{
                2 => {missing_directive_definition, #{directive => DirectiveName}}
            })
    end.

-spec get_input_type_definition(ServiceDocument, InputTypeName) -> InputTypeDefinition when
    ServiceDocument :: t(),
    InputTypeName :: argo_types:name(),
    InputTypeDefinition :: argo_graphql_type_definition:t().
get_input_type_definition(ServiceDocument = #argo_graphql_service_document{}, InputTypeName) when
    is_binary(InputTypeName)
->
    case get_type_definition(ServiceDocument, InputTypeName) of
        InputTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_scalar_type_definition{}} ->
            InputTypeDefinition;
        InputTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_enum_type_definition{}} ->
            InputTypeDefinition;
        InputTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_input_object_type_definition{}} ->
            InputTypeDefinition;
        _ ->
            error_with_info(badarg, [ServiceDocument, InputTypeName], #{
                2 => {invalid_input_type, InputTypeName}
            })
    end.

-spec get_input_type_graph(ServiceDocument) -> InputTypeGraph when
    ServiceDocument :: t(),
    InputTypeGraph :: argo_graphql_input_type_graph:t().
get_input_type_graph(ServiceDocument = #argo_graphql_service_document{type_definitions = TypeDefinitions}) ->
    V1 = maps:new(),
    G1 = argo_graphql_input_type_graph:new(),
    {_V2, G2 = #argo_graphql_input_type_graph{}} = maps:fold(
        fun(TypeName, TypeDefinition, {V1_Acc1, G1_Acc1}) ->
            case argo_graphql_type_definition:is_input_type(TypeDefinition) of
                false ->
                    {V1_Acc1, G1_Acc1};
                true ->
                    {V1_Acc2, G1_Acc2} = get_input_type_graph(ServiceDocument, TypeName, V1_Acc1, G1_Acc1),
                    {V1_Acc2, G1_Acc2}
            end
        end,
        {V1, G1},
        TypeDefinitions
    ),
    G2.

-spec get_input_type_graph(ServiceDocument, InputTypeName) -> InputTypeGraph when
    ServiceDocument :: t(),
    InputTypeName :: argo_types:name(),
    InputTypeGraph :: argo_graphql_input_type_graph:t().
get_input_type_graph(ServiceDocument = #argo_graphql_service_document{}, InputTypeName) when
    is_binary(InputTypeName)
->
    Visited1 = maps:new(),
    InputTypeGraph1 = argo_graphql_input_type_graph:new(),
    case get_input_type_graph(ServiceDocument, InputTypeName, Visited1, InputTypeGraph1) of
        {_Visited2, InputTypeGraph2 = #argo_graphql_input_type_graph{}} ->
            InputTypeGraph2
    end.

%% @private
-spec get_input_type_graph(ServiceDocument, InputTypeName, Visited, InputTypeGraph) -> {Visited, InputTypeGraph} when
    ServiceDocument :: t(),
    InputTypeName :: argo_types:name(),
    Visited :: #{InputTypeName => []},
    InputTypeGraph :: argo_graphql_input_type_graph:t().
get_input_type_graph(ServiceDocument = #argo_graphql_service_document{}, InputTypeName, Visited1, InputTypeGraph1) when
    is_binary(InputTypeName)
->
    case maps:is_key(InputTypeName, Visited1) of
        false ->
            Visited2 = Visited1#{InputTypeName => []},
            InputTypeGraph2 = argo_graphql_input_type_graph:add_input(InputTypeGraph1, InputTypeName),
            case get_input_type_definition(ServiceDocument, InputTypeName) of
                #argo_graphql_type_definition{kind = #argo_graphql_scalar_type_definition{}} ->
                    {Visited2, InputTypeGraph2};
                #argo_graphql_type_definition{kind = #argo_graphql_enum_type_definition{}} ->
                    {Visited2, InputTypeGraph2};
                #argo_graphql_type_definition{kind = #argo_graphql_input_object_type_definition{inputs = InputsMap}} ->
                    V2_1 = Visited2,
                    G2_1 = InputTypeGraph2,
                    {V2_2, G2_2} =
                        argo_index_map:foldl(
                            fun(
                                _Index,
                                _InputName,
                                InputValueDefinition = #argo_graphql_input_value_definition{type = InputType},
                                {V2_1_Acc1, G2_1_Acc1}
                            ) ->
                                case argo_graphql_input_value_definition:is_required(InputValueDefinition) of
                                    false ->
                                        {V2_1_Acc1, G2_1_Acc1};
                                    true ->
                                        RequiredInputTypeName = argo_graphql_type:get_type_name(InputType),
                                        {V2_1_Acc2, G2_1_Acc2} = get_input_type_graph(
                                            ServiceDocument, RequiredInputTypeName, V2_1_Acc1, G2_1_Acc1
                                        ),
                                        G2_1_Acc3 = argo_graphql_input_type_graph:add_dependency(
                                            G2_1_Acc2, InputTypeName, RequiredInputTypeName
                                        ),
                                        {V2_1_Acc2, G2_1_Acc3}
                                end
                            end,
                            {V2_1, G2_1},
                            InputsMap
                        ),
                    {V2_2, G2_2}
            end;
        true ->
            {Visited1, InputTypeGraph1}
    end.

-spec get_interface_type_definition(ServiceDocument, InterfaceName) -> InterfaceTypeDefinition when
    ServiceDocument :: t(),
    InterfaceName :: argo_types:name(),
    InterfaceTypeDefinition :: argo_graphql_type_definition:t().
get_interface_type_definition(ServiceDocument = #argo_graphql_service_document{}, InterfaceName) when
    is_binary(InterfaceName)
->
    case get_type_definition(ServiceDocument, InterfaceName) of
        InterfaceTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_interface_type_definition{}} ->
            InterfaceTypeDefinition;
        _ ->
            error_with_info(badarg, [ServiceDocument, InterfaceName], #{
                2 => {invalid_interface, InterfaceName}
            })
    end.

-spec get_root_operation_type_definition(ServiceDocument, RootOperationType) -> RootOperationTypeDefinition when
    ServiceDocument :: t(),
    RootOperationType :: operation_type(),
    RootOperationTypeDefinition :: argo_graphql_type_definition:t().
get_root_operation_type_definition(
    ServiceDocument = #argo_graphql_service_document{'query' = {some, RootOperationTypeName}},
    RootOperationType = 'query'
) ->
    case get_type_definition(ServiceDocument, RootOperationTypeName) of
        RootOperationTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}} ->
            RootOperationTypeDefinition;
        _ ->
            error_with_info(badarg, [ServiceDocument, RootOperationType], #{
                2 => {invalid_root_operation_type, RootOperationType}
            })
    end;
get_root_operation_type_definition(
    ServiceDocument = #argo_graphql_service_document{'mutation' = {some, RootOperationTypeName}},
    RootOperationType = 'mutation'
) ->
    case get_type_definition(ServiceDocument, RootOperationTypeName) of
        RootOperationTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}} ->
            RootOperationTypeDefinition;
        _ ->
            error_with_info(badarg, [ServiceDocument, RootOperationType], #{
                2 => {invalid_root_operation_type, RootOperationType}
            })
    end;
get_root_operation_type_definition(
    ServiceDocument = #argo_graphql_service_document{'subscription' = {some, RootOperationTypeName}},
    RootOperationType = 'subscription'
) ->
    case get_type_definition(ServiceDocument, RootOperationTypeName) of
        RootOperationTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}} ->
            RootOperationTypeDefinition;
        _ ->
            error_with_info(badarg, [ServiceDocument, RootOperationType], #{
                2 => {invalid_root_operation_type, RootOperationType}
            })
    end;
get_root_operation_type_definition(ServiceDocument = #argo_graphql_service_document{}, RootOperationType) when
    ?is_operation_type(RootOperationType)
->
    error_with_info(badarg, [ServiceDocument, RootOperationType], #{
        2 => {missing_root_operation_type, RootOperationType}
    }).

-spec get_shape(ServiceDocument, TypeName) -> Shape when
    ServiceDocument :: t(), TypeName :: argo_types:name(), Shape :: argo_graphql_type_definition:shape().
get_shape(ServiceDocument = #argo_graphql_service_document{}, TypeName) when is_binary(TypeName) ->
    TypeDefinition = get_type_definition(ServiceDocument, TypeName),
    argo_graphql_type_definition:get_shape(TypeDefinition, ServiceDocument).

-spec get_type_definition(ServiceDocument, TypeName) -> TypeDefinition when
    ServiceDocument :: t(), TypeName :: argo_types:name(), TypeDefinition :: argo_graphql_type_definition:t().
get_type_definition(ServiceDocument = #argo_graphql_service_document{}, TypeName) when is_binary(TypeName) ->
    case find_type_definition(ServiceDocument, TypeName) of
        {ok, TypeDefinition} ->
            TypeDefinition;
        error ->
            error_with_info(badarg, [ServiceDocument, TypeName], #{2 => {missing_type_definition, #{type => TypeName}}})
    end.

-spec get_union_member_type_definition(ServiceDocument, UnionMemberType) -> UnionMemberTypeDefinition when
    ServiceDocument :: t(),
    UnionMemberType :: argo_types:name(),
    UnionMemberTypeDefinition :: argo_graphql_type_definition:t().
get_union_member_type_definition(ServiceDocument = #argo_graphql_service_document{}, UnionMemberType) when
    is_binary(UnionMemberType)
->
    case get_type_definition(ServiceDocument, UnionMemberType) of
        UnionMemberTypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}} ->
            UnionMemberTypeDefinition;
        _ ->
            error_with_info(badarg, [ServiceDocument, UnionMemberType], #{
                2 => {invalid_union_member_type, UnionMemberType}
            })
    end.

-spec replace_type_definition(ServiceDocument, TypeDefinition) -> ServiceDocument when
    ServiceDocument :: t(), TypeDefinition :: argo_graphql_type_definition:t().
replace_type_definition(
    ServiceDocument1 = #argo_graphql_service_document{type_definitions = TypeDefinitions1},
    TypeDefinition = #argo_graphql_type_definition{name = TypeName}
) ->
    case get_type_definition(ServiceDocument1, TypeName) of
        TypeDefinition ->
            % Nothing changed, no need to update
            ServiceDocument1;
        _OldTypeDefinition ->
            TypeDefinitions2 = maps:put(TypeName, TypeDefinition, TypeDefinitions1),
            ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{type_definitions = TypeDefinitions2},
            ServiceDocument2
    end.

-spec resolve_type_definitions(ServiceDocument) -> ServiceDocument when ServiceDocument :: t().
resolve_type_definitions(ServiceDocument1 = #argo_graphql_service_document{type_definitions = TypeDefinitions}) ->
    ServiceDocument2 = maps:fold(fun resolve_type_definition/3, ServiceDocument1, TypeDefinitions),
    ServiceDocument2.

%% @private
-spec resolve_type_definition(TypeName, TypeDefinition, ServiceDocument) -> ServiceDocument when
    TypeName :: argo_types:name(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    ServiceDocument :: t().
resolve_type_definition(
    TypeName, _TypeDefinition1 = #argo_graphql_type_definition{}, ServiceDocument1 = #argo_graphql_service_document{}
) when is_binary(TypeName) ->
    ServiceDocument1.

-spec set_description(ServiceDocument, OptionDescription) -> ServiceDocument when
    ServiceDocument :: t(), OptionDescription :: none | {some, unicode:unicode_binary()}.
set_description(ServiceDocument1 = #argo_graphql_service_document{}, OptionDescription) when
    ?is_option_binary(OptionDescription)
->
    ServiceDocument2 = ServiceDocument1#argo_graphql_service_document{description = OptionDescription},
    ServiceDocument2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(
    Formatter1,
    ServiceDocument = #argo_graphql_service_document{
        directive_definitions = DirectiveDefinitions, type_definitions = TypeDefinitions
    }
) ->
    Formatter2 = format_schema_definition(Formatter1, ServiceDocument),
    Formatter3 = maps:fold(
        fun(_TypeName, TypeDefinition, Formatter2_Acc1) ->
            Formatter2_Acc2 = argo_graphql_formatter:write(Formatter2_Acc1, "~n", []),
            Formatter2_Acc3 = argo_graphql_type_definition:format(Formatter2_Acc2, TypeDefinition),
            Formatter2_Acc3
        end,
        Formatter2,
        argo_types:dynamic_cast(maps:iterator(TypeDefinitions, ordered))
    ),
    Formatter4 = maps:fold(
        fun(_TypeName, DirectiveDefinition, Formatter3_Acc1) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_directive_definition:format(Formatter3_Acc2, DirectiveDefinition),
            Formatter3_Acc3
        end,
        Formatter3,
        argo_types:dynamic_cast(maps:iterator(DirectiveDefinitions, ordered))
    ),
    Formatter4.

%% @private
-spec format_schema_definition(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format_schema_definition(Formatter1, #argo_graphql_service_document{schema_defined = false}) ->
    Formatter1;
format_schema_definition(Formatter1, #argo_graphql_service_document{
    schema_defined = true,
    description = OptionDescription,
    directives = DirectivesConst,
    'query' = OptionQuery,
    mutation = OptionMutation,
    subscription = OptionSubscription
}) ->
    Formatter2 =
        case OptionDescription of
            {some, Description} ->
                F1_1 = Formatter1,
                F1_2 = argo_graphql_formatter:write_description(F1_1, Description),
                F1_3 = argo_graphql_formatter:write_indent(F1_2),
                F1_3;
            none ->
                Formatter1
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "schema", []),
    Formatter4 = argo_graphql_directives_const:format(Formatter3, DirectivesConst),
    Formatter5 =
        case {OptionQuery, OptionMutation, OptionSubscription} of
            {none, none, none} ->
                argo_graphql_formatter:write(Formatter4, " {}", []);
            _ ->
                F4_1 = Formatter4,
                F4_2 = argo_graphql_formatter:write(F4_1, " {", []),
                F4_3 = argo_graphql_formatter:shift_right(F4_2),
                F4_4 =
                    case OptionQuery of
                        none ->
                            F4_3;
                        {some, QueryType} ->
                            F4_3_Acc1 = F4_3,
                            F4_3_Acc2 = argo_graphql_formatter:write(F4_3_Acc1, "~n", []),
                            F4_3_Acc3 = argo_graphql_formatter:write_indent(F4_3_Acc2),
                            F4_3_Acc4 = argo_graphql_formatter:write(F4_3_Acc3, "query: ~ts", [QueryType]),
                            F4_3_Acc4
                    end,
                F4_5 =
                    case OptionMutation of
                        none ->
                            F4_4;
                        {some, MutationType} ->
                            F4_4_Acc1 = F4_4,
                            F4_4_Acc2 = argo_graphql_formatter:write(F4_4_Acc1, "~n", []),
                            F4_4_Acc3 = argo_graphql_formatter:write_indent(F4_4_Acc2),
                            F4_4_Acc4 = argo_graphql_formatter:write(F4_4_Acc3, "mutation: ~ts", [MutationType]),
                            F4_4_Acc4
                    end,
                F4_6 =
                    case OptionSubscription of
                        none ->
                            F4_5;
                        {some, SubscriptionType} ->
                            F4_5_Acc1 = F4_5,
                            F4_5_Acc2 = argo_graphql_formatter:write(F4_5_Acc1, "~n", []),
                            F4_5_Acc3 = argo_graphql_formatter:write_indent(F4_5_Acc2),
                            F4_5_Acc4 = argo_graphql_formatter:write(F4_5_Acc3, "subscription: ~ts", [SubscriptionType]),
                            F4_5_Acc4
                    end,
                F4_7 = argo_graphql_formatter:write(F4_6, "~n", []),
                F4_8 = argo_graphql_formatter:shift_left(F4_7),
                F4_9 = argo_graphql_formatter:write_indent(F4_8),
                F4_10 = argo_graphql_formatter:write(F4_9, "}", []),
                F4_10
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter6.

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
format_error_description(_Key, {duplicate_directive_name, DirectiveName}) ->
    io_lib:format("duplicate DirectiveDefinition name: ~0tp", [DirectiveName]);
format_error_description(_Key, {duplicate_root_operation_type_definition, OperationType}) ->
    io_lib:format("duplicate OperationType name: ~0tp", [OperationType]);
format_error_description(_Key, {duplicate_type_name, TypeName}) ->
    io_lib:format("duplicate TypeDefinition name: ~0tp", [TypeName]);
format_error_description(_Key, executable_definition_not_supported) ->
    "ExecutableDefinition is not supported by ServiceDocument";
format_error_description(_Key, {invalid_input_type, InputTypeName}) ->
    io_lib:format("invalid InputTypeDefinition must be a scalar, enum, or input for named type: ~0tp", [
        InputTypeName
    ]);
format_error_description(_Key, {invalid_interface, InterfaceName}) ->
    io_lib:format("invalid ImplementsInterfaces must be an InterfaceTypeDefinition for named type: ~0tp", [
        InterfaceName
    ]);
format_error_description(_Key, {invalid_root_operation_type, RootOperationType}) ->
    io_lib:format("invalid RootOperationTypeDefinition must be an ObjectTypeDefinition for operation type: ~0tp", [
        RootOperationType
    ]);
format_error_description(_Key, {invalid_type_extension, #{type := TypeName, extend := ExtendType}}) ->
    io_lib:format("invalid TypeExtension for ~0tp on named type: ~0tp", [ExtendType, TypeName]);
format_error_description(_Key, {invalid_union_member_type, UnionMemberType}) ->
    io_lib:format("invalid UnionMemberType must be an ObjectTypeDefinition for named type: ~0tp", [UnionMemberType]);
format_error_description(_Key, {missing_directive_definition, #{directive := DirectiveName}}) ->
    io_lib:format("missing DirectiveDefinition for named directive: ~0tp", [DirectiveName]);
format_error_description(_Key, {missing_root_operation_type, RootOperationType}) ->
    io_lib:format("missing RootOperationTypeDefinition for operation type: ~0tp", [
        RootOperationType
    ]);
format_error_description(_Key, {missing_type_definition, #{type := TypeName}}) ->
    io_lib:format("missing TypeDefinition for named type: ~0tp", [TypeName]);
format_error_description(_Key, {parser_error, ParserError}) ->
    io_lib:format("parser error: ~0tp", [ParserError]);
format_error_description(_Key, {read_error, ReadError}) ->
    io_lib:format("read error: ~0tp", [ReadError]);
format_error_description(_Key, {scanner_error, #{error_info := ErrorInfo, end_loc := EndLoc}}) ->
    io_lib:format("scanner error at end location ~0tp: ~0tp", [EndLoc, ErrorInfo]);
format_error_description(_Key, schema_already_defined) ->
    "SchemaDefinition has already been defined (ServiceDocument only supports one SchemaDefinition)";
format_error_description(_Key, schema_not_defined) ->
    "SchemaExtension is not allowed if SchemaDefinition has not already been defined (ServiceDocument requires SchemaDefinition before SchemaExtension)";
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
