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
-module(argo_typer).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    derive_wire_type/3,
    graphql_type_to_wire_type/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Records
-record(selected_field_node, {
    by :: argo_graphql_selection_set:selection(),
    field :: argo_graphql_field:t()
}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec derive_wire_type(ServiceDocument, ExecutableDocument, OptionOperationName) -> {OptionOperationName, WireType} when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    OptionOperationName :: none | {some, OperationName},
    OperationName :: argo_types:name(),
    WireType :: argo_wire_type:t().
derive_wire_type(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument = #argo_graphql_executable_document{},
    OptionOperationName1
) when ?is_option_binary(OptionOperationName1) ->
    {OptionOperationName2, OperationDefinition} = argo_graphql_executable_document:get_operation_definition(
        ExecutableDocument, OptionOperationName1
    ),
    DataTypeDefinition = get_data_type_definition(ServiceDocument, OperationDefinition),
    DataWireType = collect_field_wire_types(
        ServiceDocument,
        ExecutableDocument,
        DataTypeDefinition,
        OperationDefinition#argo_graphql_operation_definition.selection_set
    ),
    DataFieldWireType = argo_field_wire_type:new(<<"data">>, DataWireType, false),
    ErrorsFieldWireType = argo_field_wire_type:new(
        <<"errors">>,
        argo_wire_type:nullable(
            argo_nullable_wire_type:new(argo_wire_type:array(argo_array_wire_type:new(argo_wire_type:error())))
        ),
        true
    ),
    RecordWireType1 = argo_record_wire_type:new(),
    RecordWireType2 = argo_record_wire_type:insert(RecordWireType1, DataFieldWireType),
    RecordWireType3 = argo_record_wire_type:insert(RecordWireType2, ErrorsFieldWireType),
    WireType = argo_wire_type:record(RecordWireType3),
    {OptionOperationName2, WireType}.

-spec graphql_type_to_wire_type(ServiceDocument, Type) -> WireType when
    ServiceDocument :: argo_graphql_service_document:t(),
    Type :: argo_graphql_type:t(),
    WireType :: argo_wire_type:t().
graphql_type_to_wire_type(ServiceDocument = #argo_graphql_service_document{}, Type = #argo_graphql_type{}) ->
    BaseType = argo_graphql_type:get_base_type(Type),
    WireType1 =
        case BaseType of
            #argo_graphql_type{inner = TypeName} when is_binary(TypeName) ->
                TypeDefinition = argo_graphql_service_document:get_type_definition(ServiceDocument, TypeName),
                OptionCodec = get_argo_codec_directive_value(TypeDefinition),
                OptionDeduplicate = get_argo_deduplicate_directive_value(TypeDefinition),
                case TypeDefinition#argo_graphql_type_definition.kind of
                    #argo_graphql_scalar_type_definition{} ->
                        case TypeName of
                            <<"Boolean">> ->
                                case OptionDeduplicate of
                                    {some, true} ->
                                        error_with_info(badarg, [ServiceDocument, Type], #{
                                            2 => {deduplicate_not_supported, boolean}
                                        });
                                    _ ->
                                        argo_wire_type:scalar(argo_scalar_wire_type:boolean())
                                end;
                            <<"Float">> ->
                                make_block(argo_scalar_wire_type:float64(), TypeName, OptionDeduplicate);
                            <<"ID">> ->
                                make_block(argo_scalar_wire_type:string(), TypeName, OptionDeduplicate);
                            <<"Int">> ->
                                make_block(argo_scalar_wire_type:varint(), TypeName, OptionDeduplicate);
                            <<"String">> ->
                                make_block(argo_scalar_wire_type:string(), TypeName, OptionDeduplicate);
                            _ ->
                                case OptionCodec of
                                    none ->
                                        error_with_info(badarg, [ServiceDocument, Type], #{
                                            2 => {custom_scalar_requires_argo_codec, TypeName}
                                        });
                                    {some, ScalarWireType} ->
                                        make_block(ScalarWireType, TypeName, OptionDeduplicate)
                                end
                        end;
                    #argo_graphql_object_type_definition{} ->
                        argo_wire_type:record(argo_record_wire_type:new());
                    #argo_graphql_interface_type_definition{} ->
                        argo_wire_type:record(argo_record_wire_type:new());
                    #argo_graphql_union_type_definition{} ->
                        argo_wire_type:record(argo_record_wire_type:new());
                    #argo_graphql_enum_type_definition{} ->
                        make_block(argo_scalar_wire_type:string(), TypeName, OptionDeduplicate);
                    #argo_graphql_input_object_type_definition{} ->
                        error_with_info(badarg, [ServiceDocument, Type], #{2 => {unsupported_graphql_type, TypeName}})
                end;
            #argo_graphql_type{inner = #argo_graphql_list_type{type = Of}} ->
                ArrayWireType = argo_array_wire_type:new(graphql_type_to_wire_type(ServiceDocument, Of)),
                argo_wire_type:array(ArrayWireType)
        end,
    WireType2 =
        case argo_graphql_type:is_nullable_type(Type) of
            false ->
                WireType1;
            true ->
                NullableWireType = argo_nullable_wire_type:new(WireType1),
                argo_wire_type:nullable(NullableWireType)
        end,
    WireType2.

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
format_error_description(_Key, {custom_scalar_requires_argo_codec, TypeName}) ->
    io_lib:format("custom scalar ~0tp requires directive @ArgoCodec(codec: ArgoCodecType!, fixedLength: Int)", [
        TypeName
    ]);
format_error_description(_Key, {deduplicate_not_supported, TypeName}) ->
    io_lib:format("directive @ArgoDeduplicate(deduplicate: true) is not supported for scalar type: ~0tp", [TypeName]);
format_error_description(_Key, {field_selection_type_shape_mismatch, #{field_alias := FieldAlias}}) ->
    io_lib:format("field selection type shape mismatch for alias: ~0tp", [FieldAlias]);
format_error_description(_Key, {unsupported_graphql_type, TypeName}) ->
    io_lib:format("unsupported GraphQL type: ~0tp", [TypeName]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec get_data_type_definition(ServiceDocument, OperationDefinition) -> DataTypeDefinition when
    ServiceDocument :: argo_graphql_service_document:t(),
    OperationDefinition :: argo_graphql_operation_definition:t(),
    DataTypeDefinition :: argo_graphql_type_definition:t().
get_data_type_definition(
    ServiceDocument = #argo_graphql_service_document{},
    _OperationDefinition = #argo_graphql_operation_definition{operation = Operation}
) ->
    case Operation of
        'query' ->
            case ServiceDocument of
                #argo_graphql_service_document{'query' = none} ->
                    argo_graphql_service_document:get_type_definition(ServiceDocument, <<"Query">>);
                #argo_graphql_service_document{'query' = {some, QueryType}} ->
                    argo_graphql_service_document:get_type_definition(ServiceDocument, QueryType)
            end;
        'mutation' ->
            case ServiceDocument of
                #argo_graphql_service_document{'mutation' = none} ->
                    argo_graphql_service_document:get_type_definition(ServiceDocument, <<"Mutation">>);
                #argo_graphql_service_document{'mutation' = {some, MutationType}} ->
                    argo_graphql_service_document:get_type_definition(ServiceDocument, MutationType)
            end;
        'subscription' ->
            case ServiceDocument of
                #argo_graphql_service_document{'subscription' = none} ->
                    argo_graphql_service_document:get_type_definition(ServiceDocument, <<"Subscription">>);
                #argo_graphql_service_document{'subscription' = {some, SubscriptionType}} ->
                    argo_graphql_service_document:get_type_definition(ServiceDocument, SubscriptionType)
            end
    end.

%% @private
-spec collect_fields_static(ServiceDocument, ExecutableDocument, SelectionSet, VisitedFragments) ->
    {GroupedFields, VisitedFragments}
when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    SelectionSet :: argo_graphql_selection_set:t(),
    VisitedFragments :: sets:set(FragmentName),
    FragmentName :: argo_types:name(),
    GroupedFields :: argo_index_map:t(ResponseKey, [SelectedFieldNode]),
    ResponseKey :: argo_types:name(),
    SelectedFieldNode :: #selected_field_node{}.
collect_fields_static(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument = #argo_graphql_executable_document{},
    #argo_graphql_selection_set{selections = Selections},
    VisitedFragments1
) ->
    collect_fields_static(ServiceDocument, ExecutableDocument, Selections, VisitedFragments1, argo_index_map:new()).

%% @private
-spec collect_fields_static(ServiceDocument, ExecutableDocument, SelectionList, VisitedFragments, GroupedFields) ->
    {GroupedFields, VisitedFragments}
when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    SelectionList :: [Selection],
    Selection :: argo_graphql_selection_set:selection(),
    VisitedFragments :: sets:set(FragmentName),
    FragmentName :: argo_types:name(),
    GroupedFields :: argo_index_map:t(ResponseKey, [SelectedFieldNode]),
    ResponseKey :: argo_types:name(),
    SelectedFieldNode :: #selected_field_node{}.
collect_fields_static(ServiceDocument, ExecutableDocument, [Selection | Selections], VisitedFragments1, GroupedFields1) ->
    case Selection of
        Field = #argo_graphql_field{} ->
            case always_skip_selection(Field#argo_graphql_field.directives) of
                false ->
                    ResponseKey =
                        case Field of
                            #argo_graphql_field{'alias' = {some, FieldAlias}} ->
                                FieldAlias;
                            #argo_graphql_field{name = FieldName} ->
                                FieldName
                        end,
                    SelectedFieldNode = #selected_field_node{by = Selection, field = Field},
                    GroupedFields2 =
                        case argo_index_map:find(ResponseKey, GroupedFields1) of
                            {ok, GroupForResponseKey1} ->
                                GroupForResponseKey2 = GroupForResponseKey1 ++ [SelectedFieldNode],
                                argo_index_map:put(ResponseKey, GroupForResponseKey2, GroupedFields1);
                            error ->
                                argo_index_map:put(ResponseKey, [SelectedFieldNode], GroupedFields1)
                        end,
                    collect_fields_static(
                        ServiceDocument, ExecutableDocument, Selections, VisitedFragments1, GroupedFields2
                    );
                true ->
                    collect_fields_static(
                        ServiceDocument, ExecutableDocument, Selections, VisitedFragments1, GroupedFields1
                    )
            end;
        FragmentSpread = #argo_graphql_fragment_spread{name = FragmentName} ->
            case
                always_skip_selection(FragmentSpread#argo_graphql_fragment_spread.directives) orelse
                    sets:is_element(FragmentName, VisitedFragments1)
            of
                false ->
                    VisitedFragments2 = sets:add_element(FragmentName, VisitedFragments1),
                    FragmentDefinition = argo_graphql_executable_document:get_fragment_definition(
                        ExecutableDocument, FragmentName
                    ),
                    {FragmentGroupedFieldSet, VisitedFragments3} = collect_fields_static(
                        ServiceDocument,
                        ExecutableDocument,
                        FragmentDefinition#argo_graphql_fragment_definition.selection_set,
                        VisitedFragments2
                    ),
                    GroupedFields2 = argo_index_map:foldl(
                        fun(_Index, ResponseKey, FragmentGroup, GroupedFields1_Acc1) ->
                            GroupForResponseKey1 =
                                case argo_index_map:find(ResponseKey, GroupedFields1_Acc1) of
                                    {ok, GRK1} ->
                                        GRK1;
                                    error ->
                                        []
                                end,
                            GroupForResponseKey2 = lists:foldl(
                                fun(#selected_field_node{field = Field}, GroupForResponseKey1_Acc1) ->
                                    SelectedFieldNode = #selected_field_node{by = Selection, field = Field},
                                    GroupForResponseKey1_Acc2 = GroupForResponseKey1_Acc1 ++ [SelectedFieldNode],
                                    GroupForResponseKey1_Acc2
                                end,
                                GroupForResponseKey1,
                                FragmentGroup
                            ),
                            GroupedFields1_Acc2 = argo_index_map:put(
                                ResponseKey, GroupForResponseKey2, GroupedFields1_Acc1
                            ),
                            GroupedFields1_Acc2
                        end,
                        GroupedFields1,
                        FragmentGroupedFieldSet
                    ),
                    collect_fields_static(
                        ServiceDocument, ExecutableDocument, Selections, VisitedFragments3, GroupedFields2
                    );
                true ->
                    collect_fields_static(
                        ServiceDocument, ExecutableDocument, Selections, VisitedFragments1, GroupedFields1
                    )
            end;
        InlineFragment = #argo_graphql_inline_fragment{} ->
            case always_skip_selection(InlineFragment#argo_graphql_inline_fragment.directives) of
                false ->
                    {FragmentGroupedFieldSet, VisitedFragments2} = collect_fields_static(
                        ServiceDocument,
                        ExecutableDocument,
                        InlineFragment#argo_graphql_inline_fragment.selection_set,
                        VisitedFragments1
                    ),
                    GroupedFields2 = argo_index_map:foldl(
                        fun(_Index, ResponseKey, FragmentGroup, GroupedFields1_Acc1) ->
                            GroupForResponseKey1 =
                                case argo_index_map:find(ResponseKey, GroupedFields1_Acc1) of
                                    {ok, GRK1} ->
                                        GRK1;
                                    error ->
                                        []
                                end,
                            GroupForResponseKey2 = lists:foldl(
                                fun(#selected_field_node{field = Field}, GroupForResponseKey1_Acc1) ->
                                    SelectedFieldNode = #selected_field_node{by = Selection, field = Field},
                                    GroupForResponseKey1_Acc2 = GroupForResponseKey1_Acc1 ++ [SelectedFieldNode],
                                    GroupForResponseKey1_Acc2
                                end,
                                GroupForResponseKey1,
                                FragmentGroup
                            ),
                            GroupedFields1_Acc2 = argo_index_map:put(
                                ResponseKey, GroupForResponseKey2, GroupedFields1_Acc1
                            ),
                            GroupedFields1_Acc2
                        end,
                        GroupedFields1,
                        FragmentGroupedFieldSet
                    ),
                    collect_fields_static(
                        ServiceDocument, ExecutableDocument, Selections, VisitedFragments2, GroupedFields2
                    );
                true ->
                    collect_fields_static(
                        ServiceDocument, ExecutableDocument, Selections, VisitedFragments1, GroupedFields1
                    )
            end
    end;
collect_fields_static(_ServiceDocument, _ExecutableDocument, [], VisitedFragments1, GroupedFields1) ->
    {GroupedFields1, VisitedFragments1}.

%% @private
-spec collect_field_wire_types(ServiceDocument, ExecutableDocument, SelectionTypeDefinition, SelectionSet) ->
    WireType
when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    SelectionTypeDefinition :: argo_graphql_type_definition:t(),
    SelectionSet :: argo_graphql_selection_set:t(),
    WireType :: argo_wire_type:t().
collect_field_wire_types(ServiceDocument, ExecutableDocument, SelectionTypeDefinition, SelectionSet) ->
    RecordWireType1 = argo_record_wire_type:new(),
    VisitedFragments1 = sets:new([{version, 2}]),
    {GroupedFields1, _VisitedFragments2} = collect_fields_static(
        ServiceDocument, ExecutableDocument, SelectionSet, VisitedFragments1
    ),
    RecordWireType2 = argo_index_map:foldl(
        fun(_Index, FieldAlias, Fields, RecordWireType1_Acc1) ->
            RecordWireType1_Acc2 =
                lists:foldl(
                    fun(Selected, RecordWireType1_Acc1_Acc1) ->
                        {Omittable1, OptionTypeCondition} =
                            case Selected#selected_field_node.by of
                                Field = #argo_graphql_field{} ->
                                    case maybe_omit_selection(Field#argo_graphql_field.directives) of
                                        false ->
                                            {false, none};
                                        true ->
                                            {true, none}
                                    end;
                                _FragmentSpread = #argo_graphql_fragment_spread{name = FragmentName} ->
                                    FragmentDefinition = argo_graphql_executable_document:get_fragment_definition(
                                        ExecutableDocument, FragmentName
                                    ),
                                    {false, {some, FragmentDefinition#argo_graphql_fragment_definition.type_condition}};
                                InlineFragment = #argo_graphql_inline_fragment{} ->
                                    {false, InlineFragment#argo_graphql_inline_fragment.type_condition}
                            end,
                        Omittable2 =
                            case OptionTypeCondition of
                                none ->
                                    Omittable1;
                                {some, TypeCondition} ->
                                    TypeCondition =/= SelectionTypeDefinition#argo_graphql_type_definition.name
                            end,
                        FieldName = Selected#selected_field_node.field#argo_graphql_field.name,
                        FieldDefinition = argo_graphql_type_definition:get_field_definition(
                            SelectionTypeDefinition, FieldName, ServiceDocument
                        ),
                        FieldSelectionSet = Selected#selected_field_node.field#argo_graphql_field.selection_set,
                        FieldType = FieldDefinition#argo_graphql_field_definition.type,
                        FieldWireType =
                            case length(FieldSelectionSet#argo_graphql_selection_set.selections) of
                                0 ->
                                    WireType = graphql_type_to_wire_type(ServiceDocument, FieldType),
                                    argo_field_wire_type:new(FieldAlias, WireType, Omittable2);
                                _ ->
                                    FieldTypeName = argo_graphql_type:get_type_name(FieldType),
                                    FieldTypeDefinition = argo_graphql_service_document:get_type_definition(
                                        ServiceDocument, FieldTypeName
                                    ),
                                    WireType = wrap_wire_type(
                                        FieldType,
                                        collect_field_wire_types(
                                            ServiceDocument, ExecutableDocument, FieldTypeDefinition, FieldSelectionSet
                                        )
                                    ),
                                    argo_field_wire_type:new(FieldAlias, WireType, Omittable2)
                            end,
                        RecordWireType1_Acc1_Acc2 =
                            case argo_record_wire_type:find(RecordWireType1_Acc1_Acc1, FieldAlias) of
                                {ok, FieldWireType} ->
                                    RecordWireType1_Acc1_Acc1;
                                {ok, _ExistingFieldWireType} ->
                                    error_with_info(
                                        badarg,
                                        [ServiceDocument, SelectionTypeDefinition, SelectionSet],
                                        {field_selection_type_shape_mismatch, #{field_alias => FieldAlias}}
                                    );
                                error ->
                                    argo_record_wire_type:insert(RecordWireType1_Acc1_Acc1, FieldWireType)
                            end,
                        RecordWireType1_Acc1_Acc2
                    end,
                    RecordWireType1_Acc1,
                    Fields
                ),
            RecordWireType1_Acc2
        end,
        RecordWireType1,
        GroupedFields1
    ),
    argo_wire_type:record(RecordWireType2).

%% @private
-spec always_skip_selection(Directives) -> boolean() when Directives :: argo_graphql_directives:t().
always_skip_selection(#argo_graphql_directives{directives = Directives}) ->
    lists:any(
        fun
            (#argo_graphql_directive{name = <<"skip">>, arguments = Arguments}) ->
                case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
                    {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {boolean, true}}}} ->
                        true;
                    _ ->
                        false
                end;
            (#argo_graphql_directive{name = <<"include">>, arguments = Arguments}) ->
                case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
                    {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {boolean, false}}}} ->
                        true;
                    _ ->
                        false
                end;
            (#argo_graphql_directive{}) ->
                false
        end,
        Directives
    ).

%% @private
-spec maybe_omit_selection(Directives) -> boolean() when Directives :: argo_graphql_directives:t().
maybe_omit_selection(#argo_graphql_directives{directives = Directives}) ->
    lists:any(
        fun
            (#argo_graphql_directive{name = <<"skip">>, arguments = Arguments}) ->
                case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
                    {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {variable, _}}}} ->
                        true;
                    _ ->
                        false
                end;
            (#argo_graphql_directive{name = <<"include">>, arguments = Arguments}) ->
                case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
                    {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {variable, _}}}} ->
                        true;
                    _ ->
                        false
                end;
            (#argo_graphql_directive{}) ->
                false
        end,
        Directives
    ).

%% @private
-spec wrap_wire_type(Type, WireType) -> WireType when Type :: argo_graphql_type:t(), WireType :: argo_wire_type:t().
wrap_wire_type(Type1 = #argo_graphql_type{}, WireType1 = #argo_wire_type{}) ->
    case Type1 of
        #argo_graphql_type{inner = NamedType} when is_binary(NamedType) ->
            NullableWireType = argo_nullable_wire_type:new(WireType1),
            WireType2 = argo_wire_type:nullable(NullableWireType),
            WireType2;
        #argo_graphql_type{inner = #argo_graphql_list_type{type = Type2}} ->
            WireType2 = wrap_wire_type(Type2, WireType1),
            ArrayWireType = argo_array_wire_type:new(WireType2),
            WireType3 = argo_wire_type:array(ArrayWireType),
            NullableWireType = argo_nullable_wire_type:new(WireType3),
            WireType4 = argo_wire_type:nullable(NullableWireType),
            WireType4;
        #argo_graphql_type{inner = #argo_graphql_non_null_type{type = NamedType}} when is_binary(NamedType) ->
            WireType1;
        #argo_graphql_type{inner = #argo_graphql_non_null_type{type = #argo_graphql_list_type{type = Type2}}} ->
            WireType2 = wrap_wire_type(Type2, WireType1),
            ArrayWireType = argo_array_wire_type:new(WireType2),
            WireType3 = argo_wire_type:array(ArrayWireType),
            WireType3
    end.

%% @private
-spec get_argo_codec_directive_value(TypeDefinition) -> none | {some, ScalarWireType} when
    TypeDefinition :: argo_graphql_type_definition:t(), ScalarWireType :: argo_scalar_wire_type:t().
get_argo_codec_directive_value(#argo_graphql_type_definition{
    kind = Kind, directives = DirectivesConst = #argo_graphql_directives_const{}
}) when
    is_record(Kind, argo_graphql_enum_type_definition) orelse is_record(Kind, argo_graphql_scalar_type_definition)
->
    case argo_graphql_directives_const:find_directive_const_non_repeatable(DirectivesConst, <<"ArgoCodec">>) of
        {ok, #argo_graphql_directive_const{arguments = ArgumentsConst}} ->
            case argo_graphql_arguments_const:get_argument_const(ArgumentsConst, <<"codec">>) of
                #argo_graphql_argument_const{value = #argo_graphql_value_const{inner = {enum, CodecEnumValue}}} when
                    is_binary(CodecEnumValue)
                ->
                    case CodecEnumValue of
                        <<"String">> ->
                            {some, argo_scalar_wire_type:string()};
                        <<"Int">> ->
                            {some, argo_scalar_wire_type:varint()};
                        <<"Float">> ->
                            {some, argo_scalar_wire_type:float64()};
                        <<"Boolean">> ->
                            {some, argo_scalar_wire_type:boolean()};
                        <<"BYTES">> ->
                            {some, argo_scalar_wire_type:bytes()};
                        <<"FIXED">> ->
                            case argo_graphql_arguments_const:get_argument_const(ArgumentsConst, <<"fixedLength">>) of
                                #argo_graphql_argument_const{
                                    value = #argo_graphql_value_const{inner = {int, FixedLength}}
                                } when is_integer(FixedLength) andalso FixedLength >= 0 ->
                                    {some, argo_scalar_wire_type:fixed(FixedLength)}
                            end
                    end
            end;
        error ->
            none
    end;
get_argo_codec_directive_value(#argo_graphql_type_definition{}) ->
    none.

%% @private
-spec get_argo_deduplicate_directive_value(TypeDefinition) -> none | {some, boolean()} when
    TypeDefinition :: argo_graphql_type_definition:t().
get_argo_deduplicate_directive_value(#argo_graphql_type_definition{
    kind = Kind, directives = DirectivesConst = #argo_graphql_directives_const{}
}) when
    is_record(Kind, argo_graphql_enum_type_definition) orelse is_record(Kind, argo_graphql_scalar_type_definition)
->
    case argo_graphql_directives_const:find_directive_const_non_repeatable(DirectivesConst, <<"ArgoDeduplicate">>) of
        {ok, #argo_graphql_directive_const{arguments = ArgumentsConst}} ->
            case argo_graphql_arguments_const:find_argument_const(ArgumentsConst, <<"deduplicate">>) of
                {ok, #argo_graphql_argument_const{value = #argo_graphql_value_const{inner = {boolean, Deduplicate}}}} when
                    is_boolean(Deduplicate)
                ->
                    {some, Deduplicate};
                error ->
                    {some, true}
            end;
        error ->
            none
    end;
get_argo_deduplicate_directive_value(#argo_graphql_type_definition{}) ->
    none.

%% @private
-compile({inline, [make_block/3]}).
-spec make_block(Of, Key, OptionDedupe) -> WireType when
    Of :: argo_scalar_wire_type:t(),
    Key :: argo_types:name(),
    OptionDedupe :: none | {some, boolean()},
    WireType :: argo_wire_type:t().
make_block(Of = #argo_scalar_wire_type{}, Key, OptionDedupe) when
    is_binary(Key) andalso ?is_option_boolean(OptionDedupe)
->
    Dedupe =
        case OptionDedupe of
            none ->
                argo_scalar_wire_type:deduplicate_by_default(Of);
            {some, B} when is_boolean(B) ->
                B
        end,
    BlockWireType = argo_block_wire_type:new(Of, Key, Dedupe),
    argo_wire_type:block(BlockWireType).
