%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_type_definition).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    builtin/1,
    from_language/1,
    scalar_type_definition/2,
    object_type_definition/2,
    interface_type_definition/2,
    union_type_definition/2,
    enum_type_definition/2,
    input_object_type_definition/2
]).
%% Instance API
-export([
    add_directive_const/2,
    find_field_definition/3,
    get_field_definition/3,
    get_implementations/2,
    get_shape/2,
    is_ambiguous/1,
    is_input_type/1,
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
-type kind() ::
    argo_graphql_scalar_type_definition:t()
    | argo_graphql_object_type_definition:t()
    | argo_graphql_interface_type_definition:t()
    | argo_graphql_union_type_definition:t()
    | argo_graphql_enum_type_definition:t()
    | argo_graphql_input_object_type_definition:t().
-type shape() ::
    argo_graphql_interface_type_definition:shape()
    | argo_graphql_object_type_definition:shape()
    | argo_graphql_union_type_definition:shape()
    | argo_graphql_input_object_type_definition:shape().
-type t() :: #argo_graphql_type_definition{}.

-export_type([
    kind/0,
    shape/0,
    t/0
]).

%% Macros
-define(is_builtin_scalar(T),
    ((T) =:= <<"Boolean">> orelse (T) =:= <<"Float">> orelse (T) =:= <<"ID">> orelse (T) =:= <<"Int">> orelse
        (T) =:= <<"String">>)
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec builtin(TypeName) -> {ok, TypeDefinition} | error when TypeName :: argo_types:name(), TypeDefinition :: t().
builtin(TypeName = <<"ArgoCodecType">>) ->
    % See: https://msolomon.github.io/argo/versions/1.0/spec#sec-Directives
    EnumTypeDefinition1 = argo_graphql_enum_type_definition:new(),
    MakeEnumValueDefinition = fun(EnumValue, OptionDescription) ->
        EnumValueDefinition1 = argo_graphql_enum_value_definition:new(EnumValue),
        EnumValueDefinition2 = argo_graphql_enum_value_definition:set_description(
            EnumValueDefinition1, OptionDescription
        ),
        EnumValueDefinition2
    end,
    EnumTypeDefinition2 = argo_graphql_enum_type_definition:add_enum_value_definitions(EnumTypeDefinition1, [
        MakeEnumValueDefinition(
            <<"String">>, {some, <<"Serialize and deserialize a scalar as a GraphQL String (UTF-8).">>}
        ),
        MakeEnumValueDefinition(
            <<"Int">>, {some, <<"Serialize and deserialize a scalar as a GraphQL Int (32-bit signed integer).">>}
        ),
        MakeEnumValueDefinition(
            <<"Float">>,
            {some,
                <<"Serialize and deserialize a scalar as a GraphQL Float (IEEE 754 double-precision floating-point).">>}
        ),
        MakeEnumValueDefinition(<<"Boolean">>, {some, <<"Serialize and deserialize a scalar as a GraphQL Boolean.">>}),
        MakeEnumValueDefinition(
            <<"BYTES">>,
            {some,
                <<"Serialize and deserialize a scalar as Argo BYTES: a variable-length length-prefixed byte array.">>}
        ),
        MakeEnumValueDefinition(
            <<"FIXED">>, {some, <<"Serialize and deserialize a scalar as Argo FIXED: a fixed-length byte array.">>}
        ),
        MakeEnumValueDefinition(
            <<"DESC">>,
            {some,
                <<"Serialize and deserialize a scalar as Argo DESC: a flexible self-describing binary format (somewhat like JSON).">>}
        )
    ]),
    TypeDefinition1 = enum_type_definition(TypeName, EnumTypeDefinition2),
    TypeDefinition2 = set_description(
        TypeDefinition1,
        {some,
            <<"Specifies how to serialize and deserialize this scalar. Adding, changing, or removing this directive is typically a breaking change.">>}
    ),
    {ok, TypeDefinition2};
builtin(TypeName) when ?is_builtin_scalar(TypeName) ->
    % See: https://spec.graphql.org/draft/#sec-Scalars.Built-in-Scalars
    ScalarTypeDefinition = argo_graphql_scalar_type_definition:new(),
    TypeDefinition = scalar_type_definition(TypeName, ScalarTypeDefinition),
    {ok, TypeDefinition};
builtin(TypeName = <<"__Schema">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    DescriptionFieldType = argo_graphql_type:named_type(<<"String">>),
    DescriptionFieldDefinition = argo_graphql_field_definition:new(<<"description">>, DescriptionFieldType),
    TypesFieldType = argo_graphql_type:non_null_type(
        argo_graphql_non_null_type:new(
            argo_graphql_list_type:new(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Type">>)))
        )
    ),
    TypesFieldDefinition = argo_graphql_field_definition:new(<<"types">>, TypesFieldType),
    QueryTypeFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Type">>)),
    QueryTypeFieldDefinition = argo_graphql_field_definition:new(<<"queryType">>, QueryTypeFieldType),
    MutationTypeFieldType = argo_graphql_type:named_type(<<"__Type">>),
    MutationTypeFieldDefinition = argo_graphql_field_definition:new(<<"mutationType">>, MutationTypeFieldType),
    SubscriptionTypeFieldType = argo_graphql_type:named_type(<<"__Type">>),
    SubscriptionTypeFieldDefinition = argo_graphql_field_definition:new(
        <<"subscriptionType">>, SubscriptionTypeFieldType
    ),
    DirectivesFieldType = argo_graphql_type:non_null_type(
        argo_graphql_non_null_type:new(
            argo_graphql_list_type:new(
                argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Directive">>))
            )
        )
    ),
    DirectivesFieldDefinition = argo_graphql_field_definition:new(<<"directives">>, DirectivesFieldType),
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ObjectTypeDefinition2 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition1, DescriptionFieldDefinition
    ),
    ObjectTypeDefinition3 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition2, TypesFieldDefinition
    ),
    ObjectTypeDefinition4 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition3, QueryTypeFieldDefinition
    ),
    ObjectTypeDefinition5 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition4, MutationTypeFieldDefinition
    ),
    ObjectTypeDefinition6 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition5, SubscriptionTypeFieldDefinition
    ),
    ObjectTypeDefinition7 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition6, DirectivesFieldDefinition
    ),
    TypeDefinition = object_type_definition(TypeName, ObjectTypeDefinition7),
    {ok, TypeDefinition};
builtin(TypeName = <<"__Type">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    IncludeDeprecatedType = argo_graphql_type:named_type(<<"Boolean">>),
    IncludeDeprecatedDefaultValue = argo_graphql_value_const:boolean(false),
    IncludeDeprecatedInputValue1 = argo_graphql_input_value_definition:new(
        <<"includeDeprecated">>, IncludeDeprecatedType
    ),
    IncludeDeprecatedInputValue2 = argo_graphql_input_value_definition:set_default_value(
        IncludeDeprecatedInputValue1, {some, IncludeDeprecatedDefaultValue}
    ),
    KindFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__TypeKind">>)),
    KindFieldDefinition = argo_graphql_field_definition:new(<<"kind">>, KindFieldType),
    NameFieldType = argo_graphql_type:named_type(<<"String">>),
    NameFieldDefinition = argo_graphql_field_definition:new(<<"name">>, NameFieldType),
    DescriptionFieldType = argo_graphql_type:named_type(<<"String">>),
    DescriptionFieldDefinition = argo_graphql_field_definition:new(<<"description">>, DescriptionFieldType),
    FieldsFieldType = argo_graphql_type:list_type(
        argo_graphql_list_type:new(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Field">>)))
    ),
    FieldsFieldDefinition1 = argo_graphql_field_definition:new(<<"fields">>, FieldsFieldType),
    FieldsFieldDefinition2 = argo_graphql_field_definition:add_argument_definition(
        FieldsFieldDefinition1, IncludeDeprecatedInputValue2
    ),
    InterfacesFieldType = argo_graphql_type:list_type(
        argo_graphql_list_type:new(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Type">>)))
    ),
    InterfacesFieldDefinition = argo_graphql_field_definition:new(<<"interfaces">>, InterfacesFieldType),
    PossibleTypesFieldType = argo_graphql_type:list_type(
        argo_graphql_list_type:new(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Type">>)))
    ),
    PossibleTypesFieldDefinition = argo_graphql_field_definition:new(<<"possibleTypes">>, PossibleTypesFieldType),
    EnumValuesFieldType = argo_graphql_type:list_type(
        argo_graphql_list_type:new(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__EnumValue">>)))
    ),
    EnumValuesFieldDefinition1 = argo_graphql_field_definition:new(<<"enumValues">>, EnumValuesFieldType),
    EnumValuesFieldDefinition2 = argo_graphql_field_definition:add_argument_definition(
        EnumValuesFieldDefinition1, IncludeDeprecatedInputValue2
    ),
    InputFieldsFieldType = argo_graphql_type:list_type(
        argo_graphql_list_type:new(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__InputValue">>)))
    ),
    InputFieldsFieldDefinition1 = argo_graphql_field_definition:new(<<"inputFields">>, InputFieldsFieldType),
    InputFieldsFieldDefinition2 = argo_graphql_field_definition:add_argument_definition(
        InputFieldsFieldDefinition1, IncludeDeprecatedInputValue2
    ),
    OfTypeFieldType = argo_graphql_type:named_type(<<"__Type">>),
    OfTypeFieldDefinition = argo_graphql_field_definition:new(<<"ofType">>, OfTypeFieldType),
    SpecifiedByURLFieldType = argo_graphql_type:named_type(<<"String">>),
    SpecifiedByURLFieldDefinition = argo_graphql_field_definition:new(<<"specifiedByURL">>, SpecifiedByURLFieldType),
    IsOneOfFieldType = argo_graphql_type:named_type(<<"Boolean">>),
    IsOneOfFieldDefinition = argo_graphql_field_definition:new(<<"isOneOf">>, IsOneOfFieldType),
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ObjectTypeDefinition2 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition1, KindFieldDefinition
    ),
    ObjectTypeDefinition3 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition2, NameFieldDefinition
    ),
    ObjectTypeDefinition4 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition3, DescriptionFieldDefinition
    ),
    ObjectTypeDefinition5 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition4, FieldsFieldDefinition2
    ),
    ObjectTypeDefinition6 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition5, InterfacesFieldDefinition
    ),
    ObjectTypeDefinition7 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition6, PossibleTypesFieldDefinition
    ),
    ObjectTypeDefinition8 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition7, EnumValuesFieldDefinition2
    ),
    ObjectTypeDefinition9 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition8, InputFieldsFieldDefinition2
    ),
    ObjectTypeDefinition10 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition9, OfTypeFieldDefinition
    ),
    ObjectTypeDefinition11 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition10, SpecifiedByURLFieldDefinition
    ),
    ObjectTypeDefinition12 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition11, IsOneOfFieldDefinition
    ),
    TypeDefinition = object_type_definition(TypeName, ObjectTypeDefinition12),
    {ok, TypeDefinition};
builtin(TypeName = <<"__TypeKind">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    ScalarEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"SCALAR">>),
    ObjectEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"OBJECT">>),
    InterfaceEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"INTERFACE">>),
    UnionEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"UNION">>),
    EnumEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"ENUM">>),
    InputObjectEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"INPUT_OBJECT">>),
    ListEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"LIST">>),
    NonNullEnumValueDefinition = argo_graphql_enum_value_definition:new(<<"NON_NULL">>),
    EnumTypeDefinition1 = argo_graphql_enum_type_definition:new(),
    EnumTypeDefinition2 = argo_graphql_enum_type_definition:add_enum_value_definitions(EnumTypeDefinition1, [
        ScalarEnumValueDefinition,
        ObjectEnumValueDefinition,
        InterfaceEnumValueDefinition,
        UnionEnumValueDefinition,
        EnumEnumValueDefinition,
        InputObjectEnumValueDefinition,
        ListEnumValueDefinition,
        NonNullEnumValueDefinition
    ]),
    TypeDefinition = enum_type_definition(TypeName, EnumTypeDefinition2),
    {ok, TypeDefinition};
builtin(TypeName = <<"__Field">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    IncludeDeprecatedType = argo_graphql_type:named_type(<<"Boolean">>),
    IncludeDeprecatedDefaultValue = argo_graphql_value_const:boolean(false),
    IncludeDeprecatedInputValue1 = argo_graphql_input_value_definition:new(
        <<"includeDeprecated">>, IncludeDeprecatedType
    ),
    IncludeDeprecatedInputValue2 = argo_graphql_input_value_definition:set_default_value(
        IncludeDeprecatedInputValue1, {some, IncludeDeprecatedDefaultValue}
    ),
    NameFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"String">>)),
    NameFieldDefinition = argo_graphql_field_definition:new(<<"name">>, NameFieldType),
    DescriptionFieldType = argo_graphql_type:named_type(<<"String">>),
    DescriptionFieldDefinition = argo_graphql_field_definition:new(<<"description">>, DescriptionFieldType),
    ArgsFieldType = argo_graphql_type:non_null_type(
        argo_graphql_non_null_type:new(
            argo_graphql_list_type:new(
                argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__InputValue">>))
            )
        )
    ),
    ArgsFieldDefinition1 = argo_graphql_field_definition:new(<<"args">>, ArgsFieldType),
    ArgsFieldDefinition2 = argo_graphql_field_definition:add_argument_definition(
        ArgsFieldDefinition1, IncludeDeprecatedInputValue2
    ),
    TypeFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Type">>)),
    TypeFieldDefinition = argo_graphql_field_definition:new(<<"type">>, TypeFieldType),
    IsDeprecatedFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IsDeprecatedFieldDefinition = argo_graphql_field_definition:new(<<"isDeprecated">>, IsDeprecatedFieldType),
    DeprecationReasonFieldType = argo_graphql_type:named_type(<<"String">>),
    DeprecationReasonFieldDefinition = argo_graphql_field_definition:new(
        <<"deprecationReason">>, DeprecationReasonFieldType
    ),
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ObjectTypeDefinition2 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition1, NameFieldDefinition
    ),
    ObjectTypeDefinition3 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition2, DescriptionFieldDefinition
    ),
    ObjectTypeDefinition4 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition3, ArgsFieldDefinition2
    ),
    ObjectTypeDefinition5 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition4, TypeFieldDefinition
    ),
    ObjectTypeDefinition6 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition5, IsDeprecatedFieldDefinition
    ),
    ObjectTypeDefinition7 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition6, DeprecationReasonFieldDefinition
    ),
    TypeDefinition = object_type_definition(TypeName, ObjectTypeDefinition7),
    {ok, TypeDefinition};
builtin(TypeName = <<"__InputValue">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    NameFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"String">>)),
    NameFieldDefinition = argo_graphql_field_definition:new(<<"name">>, NameFieldType),
    DescriptionFieldType = argo_graphql_type:named_type(<<"String">>),
    DescriptionFieldDefinition = argo_graphql_field_definition:new(<<"description">>, DescriptionFieldType),
    TypeFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__Type">>)),
    TypeFieldDefinition = argo_graphql_field_definition:new(<<"type">>, TypeFieldType),
    DefaultValueFieldType = argo_graphql_type:named_type(<<"String">>),
    DefaultValueFieldDefinition = argo_graphql_field_definition:new(<<"defaultValue">>, DefaultValueFieldType),
    IsDeprecatedFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IsDeprecatedFieldDefinition = argo_graphql_field_definition:new(<<"isDeprecated">>, IsDeprecatedFieldType),
    DeprecationReasonFieldType = argo_graphql_type:named_type(<<"String">>),
    DeprecationReasonFieldDefinition = argo_graphql_field_definition:new(
        <<"deprecationReason">>, DeprecationReasonFieldType
    ),
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ObjectTypeDefinition2 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition1, NameFieldDefinition
    ),
    ObjectTypeDefinition3 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition2, DescriptionFieldDefinition
    ),
    ObjectTypeDefinition4 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition3, TypeFieldDefinition
    ),
    ObjectTypeDefinition5 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition4, DefaultValueFieldDefinition
    ),
    ObjectTypeDefinition6 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition5, IsDeprecatedFieldDefinition
    ),
    ObjectTypeDefinition7 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition6, DeprecationReasonFieldDefinition
    ),
    TypeDefinition = object_type_definition(TypeName, ObjectTypeDefinition7),
    {ok, TypeDefinition};
builtin(TypeName = <<"__EnumValue">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    NameFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"String">>)),
    NameFieldDefinition = argo_graphql_field_definition:new(<<"name">>, NameFieldType),
    DescriptionFieldType = argo_graphql_type:named_type(<<"String">>),
    DescriptionFieldDefinition = argo_graphql_field_definition:new(<<"description">>, DescriptionFieldType),
    IsDeprecatedFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IsDeprecatedFieldDefinition = argo_graphql_field_definition:new(<<"isDeprecated">>, IsDeprecatedFieldType),
    DeprecationReasonFieldType = argo_graphql_type:named_type(<<"String">>),
    DeprecationReasonFieldDefinition = argo_graphql_field_definition:new(
        <<"deprecationReason">>, DeprecationReasonFieldType
    ),
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ObjectTypeDefinition2 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition1, NameFieldDefinition
    ),
    ObjectTypeDefinition3 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition2, DescriptionFieldDefinition
    ),
    ObjectTypeDefinition4 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition3, IsDeprecatedFieldDefinition
    ),
    ObjectTypeDefinition5 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition4, DeprecationReasonFieldDefinition
    ),
    TypeDefinition = object_type_definition(TypeName, ObjectTypeDefinition5),
    {ok, TypeDefinition};
builtin(TypeName = <<"__Directive">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    IncludeDeprecatedType = argo_graphql_type:named_type(<<"Boolean">>),
    IncludeDeprecatedDefaultValue = argo_graphql_value_const:boolean(false),
    IncludeDeprecatedInputValue1 = argo_graphql_input_value_definition:new(
        <<"includeDeprecated">>, IncludeDeprecatedType
    ),
    IncludeDeprecatedInputValue2 = argo_graphql_input_value_definition:set_default_value(
        IncludeDeprecatedInputValue1, {some, IncludeDeprecatedDefaultValue}
    ),
    NameFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"String">>)),
    NameFieldDefinition = argo_graphql_field_definition:new(<<"name">>, NameFieldType),
    DescriptionFieldType = argo_graphql_type:named_type(<<"String">>),
    DescriptionFieldDefinition = argo_graphql_field_definition:new(<<"description">>, DescriptionFieldType),
    LocationsFieldType = argo_graphql_type:non_null_type(
        argo_graphql_non_null_type:new(
            argo_graphql_list_type:new(
                argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__DirectiveLocation">>))
            )
        )
    ),
    LocationsFieldDefinition = argo_graphql_field_definition:new(<<"locations">>, LocationsFieldType),
    ArgsFieldType = argo_graphql_type:non_null_type(
        argo_graphql_non_null_type:new(
            argo_graphql_list_type:new(
                argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"__InputValue">>))
            )
        )
    ),
    ArgsFieldDefinition1 = argo_graphql_field_definition:new(<<"args">>, ArgsFieldType),
    ArgsFieldDefinition2 = argo_graphql_field_definition:add_argument_definition(
        ArgsFieldDefinition1, IncludeDeprecatedInputValue2
    ),
    IsRepeatableFieldType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IsRepeatableFieldDefinition = argo_graphql_field_definition:new(<<"isRepeatable">>, IsRepeatableFieldType),
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    ObjectTypeDefinition2 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition1, NameFieldDefinition
    ),
    ObjectTypeDefinition3 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition2, DescriptionFieldDefinition
    ),
    ObjectTypeDefinition4 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition3, LocationsFieldDefinition
    ),
    ObjectTypeDefinition5 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition4, ArgsFieldDefinition2
    ),
    ObjectTypeDefinition6 = argo_graphql_object_type_definition:add_field_definition(
        ObjectTypeDefinition5, IsRepeatableFieldDefinition
    ),
    TypeDefinition = object_type_definition(TypeName, ObjectTypeDefinition6),
    {ok, TypeDefinition};
builtin(TypeName = <<"__DirectiveLocation">>) ->
    % See: https://spec.graphql.org/draft/#sec-Schema-Introspection
    EnumTypeDefinition1 = argo_graphql_enum_type_definition:new(),
    EnumTypeDefinition2 = argo_graphql_enum_type_definition:add_enum_value_definitions(EnumTypeDefinition1, [
        argo_graphql_enum_value_definition:new(<<"QUERY">>),
        argo_graphql_enum_value_definition:new(<<"MUTATION">>),
        argo_graphql_enum_value_definition:new(<<"SUBSCRIPTION">>),
        argo_graphql_enum_value_definition:new(<<"FIELD">>),
        argo_graphql_enum_value_definition:new(<<"FRAGMENT_DEFINITION">>),
        argo_graphql_enum_value_definition:new(<<"FRAGMENT_SPREAD">>),
        argo_graphql_enum_value_definition:new(<<"INLINE_FRAGMENT">>),
        argo_graphql_enum_value_definition:new(<<"VARIABLE_DEFINITION">>),
        argo_graphql_enum_value_definition:new(<<"SCHEMA">>),
        argo_graphql_enum_value_definition:new(<<"SCALAR">>),
        argo_graphql_enum_value_definition:new(<<"OBJECT">>),
        argo_graphql_enum_value_definition:new(<<"FIELD_DEFINITION">>),
        argo_graphql_enum_value_definition:new(<<"ARGUMENT_DEFINITION">>),
        argo_graphql_enum_value_definition:new(<<"INTERFACE">>),
        argo_graphql_enum_value_definition:new(<<"UNION">>),
        argo_graphql_enum_value_definition:new(<<"ENUM">>),
        argo_graphql_enum_value_definition:new(<<"ENUM_VALUE">>),
        argo_graphql_enum_value_definition:new(<<"INPUT_OBJECT">>),
        argo_graphql_enum_value_definition:new(<<"INPUT_FIELD_DEFINITION">>)
    ]),
    TypeDefinition = enum_type_definition(TypeName, EnumTypeDefinition2),
    {ok, TypeDefinition};
builtin(TypeName) when is_binary(TypeName) ->
    error.

-spec from_language(LanguageTypeDefinition) -> TypeDefinition when
    LanguageTypeDefinition :: argo_graphql_language_type_definition:t(), TypeDefinition :: t().
from_language(#argo_graphql_language_type_definition{inner = Inner}) ->
    {TypeDefinition1, LanguageOptionDescription, LanguageOptionDirectivesConst} =
        case Inner of
            #argo_graphql_language_scalar_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_scalar_type_definition:from_language(Inner),
                {scalar_type_definition(Name, Kind), A, B};
            #argo_graphql_language_object_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_object_type_definition:from_language(Inner),
                {object_type_definition(Name, Kind), A, B};
            #argo_graphql_language_interface_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_interface_type_definition:from_language(Inner),
                {interface_type_definition(Name, Kind), A, B};
            #argo_graphql_language_union_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_union_type_definition:from_language(Inner),
                {union_type_definition(Name, Kind), A, B};
            #argo_graphql_language_enum_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_enum_type_definition:from_language(Inner),
                {enum_type_definition(Name, Kind), A, B};
            #argo_graphql_language_input_object_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_input_object_type_definition:from_language(Inner),
                {input_object_type_definition(Name, Kind), A, B}
        end,
    TypeDefinition2 =
        case LanguageOptionDescription of
            none ->
                TypeDefinition1;
            {some, Description} ->
                set_description(TypeDefinition1, {some, Description})
        end,
    TypeDefinition3 =
        case LanguageOptionDirectivesConst of
            none ->
                TypeDefinition2;
            {some, LanguageDirectivesConst} ->
                DirectivesConst = argo_graphql_directives_const:from_language(LanguageDirectivesConst),
                TypeDefinition2#argo_graphql_type_definition{directives = DirectivesConst}
        end,
    TypeDefinition3.

-compile({inline, [scalar_type_definition/2]}).
-spec scalar_type_definition(Name, ScalarTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    ScalarTypeDefinition :: argo_graphql_scalar_type_definition:t(),
    TypeDefinition :: t().
scalar_type_definition(Name, ScalarTypeDefinition = #argo_graphql_scalar_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = ScalarTypeDefinition
    }.

-compile({inline, [object_type_definition/2]}).
-spec object_type_definition(Name, ObjectTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    ObjectTypeDefinition :: argo_graphql_object_type_definition:t(),
    TypeDefinition :: t().
object_type_definition(Name, ObjectTypeDefinition = #argo_graphql_object_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = ObjectTypeDefinition
    }.

-compile({inline, [interface_type_definition/2]}).
-spec interface_type_definition(Name, InterfaceTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    InterfaceTypeDefinition :: argo_graphql_interface_type_definition:t(),
    TypeDefinition :: t().
interface_type_definition(Name, InterfaceTypeDefinition = #argo_graphql_interface_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name,
        description = none,
        directives = argo_graphql_directives_const:new(),
        kind = InterfaceTypeDefinition
    }.

-compile({inline, [union_type_definition/2]}).
-spec union_type_definition(Name, UnionTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    UnionTypeDefinition :: argo_graphql_union_type_definition:t(),
    TypeDefinition :: t().
union_type_definition(Name, UnionTypeDefinition = #argo_graphql_union_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = UnionTypeDefinition
    }.

-compile({inline, [enum_type_definition/2]}).
-spec enum_type_definition(Name, EnumTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    EnumTypeDefinition :: argo_graphql_enum_type_definition:t(),
    TypeDefinition :: t().
enum_type_definition(Name, EnumTypeDefinition = #argo_graphql_enum_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = EnumTypeDefinition
    }.

-compile({inline, [input_object_type_definition/2]}).
-spec input_object_type_definition(Name, InputObjectTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    InputObjectTypeDefinition :: argo_graphql_input_object_type_definition:t(),
    TypeDefinition :: t().
input_object_type_definition(Name, InputObjectTypeDefinition = #argo_graphql_input_object_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name,
        description = none,
        directives = argo_graphql_directives_const:new(),
        kind = InputObjectTypeDefinition
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive_const(TypeDefinition, DirectiveConst) -> TypeDefinition when
    TypeDefinition :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    TypeDefinition1 = #argo_graphql_type_definition{directives = DirectivesConst1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConst2 = argo_graphql_directives_const:add_directive_const(DirectivesConst1, DirectiveConst),
    TypeDefinition2 = TypeDefinition1#argo_graphql_type_definition{directives = DirectivesConst2},
    TypeDefinition2.

-spec find_field_definition(TypeDefinition, FieldName, ServiceDocument) -> {ok, FieldDefinition} | error when
    TypeDefinition :: t(),
    FieldName :: argo_types:name(),
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldDefinition :: argo_graphql_field_definition:t().
find_field_definition(
    TypeDefinition = #argo_graphql_type_definition{kind = Kind},
    FieldName,
    ServiceDocument = #argo_graphql_service_document{}
) when
    is_binary(FieldName)
->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {no_field_definitions, #{type => scalar}}});
        #argo_graphql_object_type_definition{fields = Fields} ->
            case argo_index_map:find(FieldName, Fields) of
                {ok, FieldDefinition} ->
                    {ok, FieldDefinition};
                error ->
                    case argo_graphql_service_document:find_root_operation_type_definition(ServiceDocument, 'query') of
                        {ok, TypeDefinition} ->
                            argo_graphql_field_definition:builtin_query(FieldName);
                        _ ->
                            argo_graphql_field_definition:builtin(FieldName)
                    end
            end;
        #argo_graphql_interface_type_definition{fields = Fields} ->
            case argo_index_map:find(FieldName, Fields) of
                {ok, FieldDefinition} ->
                    {ok, FieldDefinition};
                error ->
                    argo_graphql_field_definition:builtin(FieldName)
            end;
        #argo_graphql_union_type_definition{types = UnionMemberTypes} ->
            UnionMemberTypeIterator = argo_index_set:iterator(UnionMemberTypes),
            find_field_definition_for_union(UnionMemberTypeIterator, FieldName, ServiceDocument);
        #argo_graphql_enum_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {no_field_definitions, #{type => enum}}});
        #argo_graphql_input_object_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {no_field_definitions, #{type => input_object}}})
    end.

%% @private
-spec find_field_definition_for_union(UnionMemberTypeIterator, FieldName, ServiceDocument) ->
    {ok, FieldDefinition} | error
when
    UnionMemberTypeIterator :: argo_index_set:iterator(argo_types:name()),
    FieldName :: argo_types:name(),
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldDefinition :: argo_graphql_field_definition:t().
find_field_definition_for_union(UnionMemberTypeIterator1, FieldName, ServiceDocument) ->
    case argo_index_set:next(UnionMemberTypeIterator1) of
        none ->
            argo_graphql_field_definition:builtin(FieldName);
        {_Index, UnionMemberType, UnionMemberTypeIterator2} ->
            UnionMemberTypeDefinition = argo_graphql_service_document:get_union_member_type_definition(
                ServiceDocument, UnionMemberType
            ),
            case find_field_definition(UnionMemberTypeDefinition, FieldName, ServiceDocument) of
                {ok, FieldDefinition} ->
                    {ok, FieldDefinition};
                error ->
                    find_field_definition_for_union(UnionMemberTypeIterator2, FieldName, ServiceDocument)
            end
    end.

-spec get_field_definition(TypeDefinition, FieldName, ServiceDocument) -> FieldDefinition when
    TypeDefinition :: t(),
    FieldName :: argo_types:name(),
    ServiceDocument :: argo_graphql_service_document:t(),
    FieldDefinition :: argo_graphql_field_definition:t().
get_field_definition(
    TypeDefinition = #argo_graphql_type_definition{}, FieldName, ServiceDocument = #argo_graphql_service_document{}
) when is_binary(FieldName) ->
    case find_field_definition(TypeDefinition, FieldName, ServiceDocument) of
        {ok, FieldDefinition} ->
            FieldDefinition;
        error ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {missing_field_definition, FieldName}})
    end.

-spec get_implementations(InterfaceTypeDefinition, ServiceDocument) -> [TypeName] when
    InterfaceTypeDefinition :: t(),
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeName :: argo_types:name().
get_implementations(
    _InterfaceTypeDefinition = #argo_graphql_type_definition{name = InterfaceName, kind = InterfaceTypeKind},
    _ServiceDocument = #argo_graphql_service_document{type_definitions = TypeDefinitionsMap}
) ->
    case InterfaceTypeKind of
        #argo_graphql_interface_type_definition{} ->
            Implementations = maps:fold(
                fun(TypeName, _TypeDefinition = #argo_graphql_type_definition{kind = TypeKind}, ImplementationsAcc) ->
                    case TypeKind of
                        #argo_graphql_object_type_definition{implements = Implements} ->
                            case argo_index_set:is_element(InterfaceName, Implements) of
                                false ->
                                    ImplementationsAcc;
                                true ->
                                    ImplementationsAcc#{TypeName => []}
                            end;
                        #argo_graphql_interface_type_definition{implements = Implements} ->
                            case argo_index_set:is_element(InterfaceName, Implements) of
                                false ->
                                    ImplementationsAcc;
                                true ->
                                    ImplementationsAcc#{TypeName => []}
                            end;
                        _ ->
                            ImplementationsAcc
                    end
                end,
                maps:new(),
                TypeDefinitionsMap
            ),
            lists:sort(maps:keys(Implementations));
        _ ->
            []
    end.

-spec get_shape(TypeDefinition, ServiceDocument) -> Shape when
    TypeDefinition :: t(), ServiceDocument :: argo_graphql_service_document:t(), Shape :: shape().
get_shape(
    TypeDefinition = #argo_graphql_type_definition{kind = Kind}, ServiceDocument = #argo_graphql_service_document{}
) ->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, ServiceDocument], #{1 => {shape_not_supported, #{type => scalar}}});
        #argo_graphql_object_type_definition{} ->
            argo_graphql_object_type_definition:get_shape(Kind, ServiceDocument);
        #argo_graphql_interface_type_definition{} ->
            argo_graphql_interface_type_definition:get_shape(Kind, ServiceDocument);
        #argo_graphql_union_type_definition{} ->
            argo_graphql_union_type_definition:get_shape(Kind, ServiceDocument);
        #argo_graphql_enum_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, ServiceDocument], #{1 => {shape_not_supported, #{type => enum}}});
        #argo_graphql_input_object_type_definition{} ->
            argo_graphql_input_object_type_definition:get_shape(Kind, ServiceDocument)
    end.

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_type_definition{kind = Kind}) ->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            false;
        #argo_graphql_object_type_definition{} ->
            argo_graphql_object_type_definition:is_ambiguous(Kind);
        #argo_graphql_interface_type_definition{} ->
            argo_graphql_interface_type_definition:is_ambiguous(Kind);
        #argo_graphql_union_type_definition{} ->
            false;
        #argo_graphql_enum_type_definition{} ->
            argo_graphql_enum_type_definition:is_ambiguous(Kind);
        #argo_graphql_input_object_type_definition{} ->
            argo_graphql_input_object_type_definition:is_ambiguous(Kind)
    end.

-spec is_input_type(Definition) -> boolean() when Definition :: t().
is_input_type(#argo_graphql_type_definition{kind = Kind}) ->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            true;
        #argo_graphql_object_type_definition{} ->
            false;
        #argo_graphql_interface_type_definition{} ->
            false;
        #argo_graphql_union_type_definition{} ->
            false;
        #argo_graphql_enum_type_definition{} ->
            true;
        #argo_graphql_input_object_type_definition{} ->
            true
    end.

-spec set_description(TypeDefinition, OptionDescription) -> TypeDefinition when
    TypeDefinition :: t(), OptionDescription :: none | {some, unicode:unicode_binary()}.
set_description(TypeDefinition1 = #argo_graphql_type_definition{}, OptionDescription) when
    ?is_option_binary(OptionDescription)
->
    TypeDefinition2 = TypeDefinition1#argo_graphql_type_definition{description = OptionDescription},
    TypeDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_type_definition{
    name = Name, description = OptionDescription, directives = DirectivesConst, kind = Kind
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
    Label =
        case Kind of
            #argo_graphql_scalar_type_definition{} ->
                "scalar";
            #argo_graphql_object_type_definition{} ->
                "type";
            #argo_graphql_interface_type_definition{} ->
                "interface";
            #argo_graphql_union_type_definition{} ->
                "union";
            #argo_graphql_enum_type_definition{} ->
                "enum";
            #argo_graphql_input_object_type_definition{} ->
                "input"
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~ts ~ts", [Label, Name]),
    Formatter4 = argo_graphql_directives_const:format(Formatter3, DirectivesConst),
    Formatter5 =
        case Kind of
            #argo_graphql_scalar_type_definition{} ->
                argo_graphql_scalar_type_definition:format(Formatter4, Kind);
            #argo_graphql_object_type_definition{} ->
                argo_graphql_object_type_definition:format(Formatter4, Kind);
            #argo_graphql_interface_type_definition{} ->
                argo_graphql_interface_type_definition:format(Formatter4, Kind);
            #argo_graphql_union_type_definition{} ->
                argo_graphql_union_type_definition:format(Formatter4, Kind);
            #argo_graphql_enum_type_definition{} ->
                argo_graphql_enum_type_definition:format(Formatter4, Kind);
            #argo_graphql_input_object_type_definition{} ->
                argo_graphql_input_object_type_definition:format(Formatter4, Kind)
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
format_error_description(_Key, {missing_field_definition, FieldName}) ->
    io_lib:format("missing FieldDefinition name: ~0tp", [FieldName]);
format_error_description(_Key, {no_field_definitions, #{type := Type}}) ->
    io_lib:format("FieldDefinition is not supported on ~0tp", [Type]);
format_error_description(_Key, {shape_not_supported, #{type := Type}}) ->
    io_lib:format("shape is not supported for TypeDefinition of kind: ~0tp", [Type]);
format_error_description(_Key, Value) ->
    Value.
