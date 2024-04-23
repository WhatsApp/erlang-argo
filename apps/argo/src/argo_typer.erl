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
    derive_wire_type/4,
    graphql_type_to_wire_type/2,
    graphql_type_to_wire_type/3,
    path_to_wire_path/2,
    wire_path_to_path/2
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

%% Types
-type options() :: #{
    resolver => argo_typer_resolver:t()
}.

-export_type([
    options/0
]).

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
    OptionOperationName
) when ?is_option_binary(OptionOperationName) ->
    derive_wire_type(ServiceDocument, ExecutableDocument, OptionOperationName, #{}).

-spec derive_wire_type(ServiceDocument, ExecutableDocument, OptionOperationName, Options) ->
    {OptionOperationName, WireType}
when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    OptionOperationName :: none | {some, OperationName},
    Options :: options(),
    OperationName :: argo_types:name(),
    WireType :: argo_wire_type:t().
derive_wire_type(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument = #argo_graphql_executable_document{},
    OptionOperationName1,
    Options
) when ?is_option_binary(OptionOperationName1) andalso is_map(Options) ->
    % "data" Field
    {OptionOperationName2, OperationDefinition} = argo_graphql_executable_document:get_operation_definition(
        ExecutableDocument, OptionOperationName1
    ),
    DataTypeDefinition = get_data_type_definition(ServiceDocument, OperationDefinition, Options),
    DataWireType = collect_field_wire_types(
        ServiceDocument,
        ExecutableDocument,
        DataTypeDefinition,
        OperationDefinition#argo_graphql_operation_definition.selection_set,
        Options
    ),
    NullableDataWireType = argo_wire_type:nullable(argo_nullable_wire_type:new(DataWireType)),
    DataFieldWireType = argo_field_wire_type:new(<<"data">>, NullableDataWireType, false),
    % "errors" Field
    ErrorsWireType = argo_wire_type:array(argo_array_wire_type:new(argo_wire_type:error())),
    ErrorsFieldWireType = argo_field_wire_type:new(<<"errors">>, ErrorsWireType, true),
    % "extensions" Field
    ExtensionsWireType = argo_wire_type:extensions(),
    ExtensionsFieldWireType = argo_field_wire_type:new(<<"extensions">>, ExtensionsWireType, true),
    % Response Type
    RecordWireType1 = argo_record_wire_type:new(),
    RecordWireType2 = argo_record_wire_type:insert(RecordWireType1, DataFieldWireType),
    RecordWireType3 = argo_record_wire_type:insert(RecordWireType2, ErrorsFieldWireType),
    RecordWireType4 = argo_record_wire_type:insert(RecordWireType3, ExtensionsFieldWireType),
    WireType = argo_wire_type:record(RecordWireType4),
    {OptionOperationName2, WireType}.

-spec graphql_type_to_wire_type(ServiceDocument, Type) -> WireType when
    ServiceDocument :: argo_graphql_service_document:t(),
    Type :: argo_graphql_type:t(),
    WireType :: argo_wire_type:t().
graphql_type_to_wire_type(ServiceDocument = #argo_graphql_service_document{}, Type = #argo_graphql_type{}) ->
    graphql_type_to_wire_type(ServiceDocument, Type, #{}).

-spec graphql_type_to_wire_type(ServiceDocument, Type, Options) -> WireType when
    ServiceDocument :: argo_graphql_service_document:t(),
    Type :: argo_graphql_type:t(),
    Options :: options(),
    WireType :: argo_wire_type:t().
graphql_type_to_wire_type(ServiceDocument = #argo_graphql_service_document{}, Type = #argo_graphql_type{}, Options) when
    is_map(Options)
->
    BaseType = argo_graphql_type:get_base_type(Type),
    WireType1 =
        case BaseType of
            #argo_graphql_type{inner = TypeName} when is_binary(TypeName) ->
                TypeDefinition = get_type_definition(ServiceDocument, TypeName, Options),
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
                ArrayWireType = argo_array_wire_type:new(graphql_type_to_wire_type(ServiceDocument, Of, Options)),
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

-spec path_to_wire_path(WireType, Path) -> WirePath when
    WireType :: argo_wire_type:t(), Path :: argo_path_value:segment_list(), WirePath :: argo_wire_path:segment_list().
path_to_wire_path(_WireType = #argo_wire_type{}, []) ->
    [];
path_to_wire_path(#argo_wire_type{inner = #argo_array_wire_type{'of' = Of}}, [ArrayIndex | Path]) when
    ?is_usize(ArrayIndex) andalso is_list(Path)
->
    [ArrayIndex | path_to_wire_path(Of, Path)];
path_to_wire_path(#argo_wire_type{inner = #argo_nullable_wire_type{'of' = Of}}, Path) when is_list(Path) ->
    path_to_wire_path(Of, Path);
path_to_wire_path(WireType = #argo_wire_type{inner = RecordWireType = #argo_record_wire_type{}}, [FieldName | Path]) when
    is_binary(FieldName) andalso is_list(Path)
->
    case argo_record_wire_type:find_index_of(RecordWireType, FieldName) of
        {ok, FieldIndex, _FieldWireType = #argo_field_wire_type{name = FieldName, 'of' = FieldOf}} ->
            [FieldIndex | path_to_wire_path(FieldOf, Path)];
        error ->
            error_with_info(badarg, [WireType, [FieldName | Path]], #{2 => {missing_field_name, FieldName}})
    end;
path_to_wire_path(WireType = #argo_wire_type{}, Path = [Segment | SegmentList]) when is_list(SegmentList) ->
    case WireType#argo_wire_type.inner of
        #argo_array_wire_type{} ->
            error_with_info(badarg, [WireType, Path], #{2 => {invalid_list_index, Segment}});
        #argo_block_wire_type{} ->
            error_with_info(badarg, [WireType, Path], #{2 => {invalid_wire_type, block}});
        #argo_desc_wire_type{} ->
            error_with_info(badarg, [WireType, Path], #{2 => {invalid_wire_type, desc}});
        #argo_error_wire_type{} ->
            error_with_info(badarg, [WireType, Path], #{2 => {invalid_wire_type, error}});
        #argo_path_wire_type{} ->
            error_with_info(badarg, [WireType, Path], #{2 => {invalid_wire_type, path}});
        #argo_record_wire_type{} ->
            error_with_info(badarg, [WireType, Path], #{2 => {invalid_field_name, Segment}});
        #argo_scalar_wire_type{} ->
            error_with_info(badarg, [WireType, Path], #{2 => {invalid_wire_type, scalar}})
    end.

-spec wire_path_to_path(WireType, WirePath) -> Path when
    WireType :: argo_wire_type:t(), WirePath :: argo_wire_path:segment_list(), Path :: argo_path_value:segment_list().
wire_path_to_path(_WireType = #argo_wire_type{}, []) ->
    [];
wire_path_to_path(#argo_wire_type{inner = #argo_array_wire_type{'of' = Of}}, [ArrayIndex | WirePath]) when
    ?is_usize(ArrayIndex) andalso is_list(WirePath)
->
    [ArrayIndex | wire_path_to_path(Of, WirePath)];
wire_path_to_path(#argo_wire_type{inner = #argo_nullable_wire_type{'of' = Of}}, WirePath) when is_list(WirePath) ->
    wire_path_to_path(Of, WirePath);
wire_path_to_path(WireType = #argo_wire_type{inner = RecordWireType = #argo_record_wire_type{}}, [FieldIndex | WirePath]) when
    ?is_usize(FieldIndex) andalso is_list(WirePath)
->
    case argo_record_wire_type:find_index(RecordWireType, FieldIndex) of
        {ok, _FieldWireType = #argo_field_wire_type{name = FieldName, 'of' = FieldOf}} ->
            [FieldName | wire_path_to_path(FieldOf, WirePath)];
        error ->
            error_with_info(badarg, [WireType, [FieldIndex | WirePath]], #{2 => {missing_field_index, FieldIndex}})
    end;
wire_path_to_path(WireType = #argo_wire_type{}, WirePath = [Segment | SegmentList]) when is_list(SegmentList) ->
    case WireType#argo_wire_type.inner of
        #argo_array_wire_type{} ->
            error_with_info(badarg, [WireType, WirePath], #{2 => {invalid_list_index, Segment}});
        #argo_block_wire_type{} ->
            error_with_info(badarg, [WireType, WirePath], #{2 => {invalid_wire_type, block}});
        #argo_desc_wire_type{} ->
            error_with_info(badarg, [WireType, WirePath], #{2 => {invalid_wire_type, desc}});
        #argo_error_wire_type{} ->
            error_with_info(badarg, [WireType, WirePath], #{2 => {invalid_wire_type, error}});
        #argo_path_wire_type{} ->
            error_with_info(badarg, [WireType, WirePath], #{2 => {invalid_wire_type, path}});
        #argo_record_wire_type{} ->
            error_with_info(badarg, [WireType, WirePath], #{2 => {invalid_field_index, Segment}});
        #argo_scalar_wire_type{} ->
            error_with_info(badarg, [WireType, WirePath], #{2 => {invalid_wire_type, scalar}})
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
format_error_description(_Key, {custom_scalar_requires_argo_codec, TypeName}) ->
    io_lib:format("custom scalar ~0tp requires directive @ArgoCodec(codec: ArgoCodecType!, fixedLength: Int)", [
        TypeName
    ]);
format_error_description(_Key, {deduplicate_not_supported, TypeName}) ->
    io_lib:format("directive @ArgoDeduplicate(deduplicate: true) is not supported for scalar type: ~0tp", [TypeName]);
format_error_description(_Key, {field_selection_type_shape_mismatch, #{field_alias := FieldAlias, path := Path}}) ->
    io_lib:format("field selection type shape mismatch for alias: ~0tp (mismatch at ~ts)", [
        FieldAlias, argo_path_value:to_dotted_string(Path)
    ]);
format_error_description(_Key, {invalid_field_index, Arg}) ->
    io_lib:format("invalid field index (expected usize integer): ~0tp", [Arg]);
format_error_description(_Key, {invalid_field_name, Arg}) ->
    io_lib:format("invalid field name (expected unicode binary): ~0tp", [Arg]);
format_error_description(_Key, {invalid_list_index, Arg}) ->
    io_lib:format("invalid list index (expected usize integer): ~0tp", [Arg]);
format_error_description(_Key, {invalid_wire_type, ActualType}) ->
    io_lib:format("invalid wire type for wire path conversion: ~0tp", [ActualType]);
format_error_description(_Key, {missing_field_index, FieldIndex}) ->
    io_lib:format("field index missing: ~0tp", [FieldIndex]);
format_error_description(_Key, {missing_field_name, FieldName}) ->
    io_lib:format("field name missing: ~0tp", [FieldName]);
format_error_description(_Key, {unsupported_graphql_type, TypeName}) ->
    io_lib:format("unsupported GraphQL type: ~0tp", [TypeName]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec get_data_type_definition(ServiceDocument, OperationDefinition, Options) -> DataTypeDefinition when
    ServiceDocument :: argo_graphql_service_document:t(),
    OperationDefinition :: argo_graphql_operation_definition:t(),
    Options :: options(),
    DataTypeDefinition :: argo_graphql_type_definition:t().
get_data_type_definition(
    ServiceDocument = #argo_graphql_service_document{},
    _OperationDefinition = #argo_graphql_operation_definition{operation = Operation},
    Options
) when is_map(Options) ->
    case Operation of
        'query' ->
            case ServiceDocument of
                #argo_graphql_service_document{'query' = none} ->
                    get_type_definition(ServiceDocument, <<"Query">>, Options);
                #argo_graphql_service_document{'query' = {some, QueryType}} ->
                    get_type_definition(ServiceDocument, QueryType, Options)
            end;
        'mutation' ->
            case ServiceDocument of
                #argo_graphql_service_document{'mutation' = none} ->
                    get_type_definition(ServiceDocument, <<"Mutation">>, Options);
                #argo_graphql_service_document{'mutation' = {some, MutationType}} ->
                    get_type_definition(ServiceDocument, MutationType, Options)
            end;
        'subscription' ->
            case ServiceDocument of
                #argo_graphql_service_document{'subscription' = none} ->
                    get_type_definition(ServiceDocument, <<"Subscription">>, Options);
                #argo_graphql_service_document{'subscription' = {some, SubscriptionType}} ->
                    get_type_definition(ServiceDocument, SubscriptionType, Options)
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
-spec collect_field_wire_types(ServiceDocument, ExecutableDocument, SelectionTypeDefinition, SelectionSet, Options) ->
    WireType
when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    SelectionTypeDefinition :: argo_graphql_type_definition:t(),
    SelectionSet :: argo_graphql_selection_set:t(),
    Options :: options(),
    WireType :: argo_wire_type:t().
collect_field_wire_types(ServiceDocument, ExecutableDocument, SelectionTypeDefinition, SelectionSet, Options) ->
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
                                FragmentSpread = #argo_graphql_fragment_spread{name = FragmentName} ->
                                    FragmentSpreadOmittable = maybe_omit_selection(
                                        FragmentSpread#argo_graphql_fragment_spread.directives
                                    ),
                                    FragmentDefinition = argo_graphql_executable_document:get_fragment_definition(
                                        ExecutableDocument, FragmentName
                                    ),
                                    {FragmentSpreadOmittable,
                                        {some, FragmentDefinition#argo_graphql_fragment_definition.type_condition}};
                                InlineFragment = #argo_graphql_inline_fragment{} ->
                                    InlineFragmentOmittable = maybe_omit_selection(
                                        InlineFragment#argo_graphql_inline_fragment.directives
                                    ),
                                    {InlineFragmentOmittable,
                                        InlineFragment#argo_graphql_inline_fragment.type_condition}
                            end,
                        {Omittable2, TypeConditionDefinition} =
                            case OptionTypeCondition of
                                none ->
                                    {Omittable1, SelectionTypeDefinition};
                                {some, TypeCondition} ->
                                    case SelectionTypeDefinition#argo_graphql_type_definition.name of
                                        TypeCondition ->
                                            {Omittable1, SelectionTypeDefinition};
                                        _ ->
                                            {true, get_type_definition(ServiceDocument, TypeCondition, Options)}
                                    end
                            end,
                        Omittable3 =
                            Omittable2 orelse
                                maybe_omit_selection(Selected#selected_field_node.field#argo_graphql_field.directives),
                        FieldName = Selected#selected_field_node.field#argo_graphql_field.name,
                        FieldDefinition = get_field_definition(
                            TypeConditionDefinition, FieldName, ServiceDocument, Options
                        ),
                        FieldSelectionSet = Selected#selected_field_node.field#argo_graphql_field.selection_set,
                        FieldType = FieldDefinition#argo_graphql_field_definition.type,
                        FieldWireType =
                            case length(FieldSelectionSet#argo_graphql_selection_set.selections) of
                                0 ->
                                    WireType = graphql_type_to_wire_type(ServiceDocument, FieldType, Options),
                                    argo_field_wire_type:new(FieldAlias, WireType, Omittable3);
                                _ ->
                                    FieldTypeName = argo_graphql_type:get_type_name(FieldType),
                                    FieldTypeDefinition = get_type_definition(ServiceDocument, FieldTypeName, Options),
                                    WireType = wrap_wire_type(
                                        FieldType,
                                        collect_field_wire_types(
                                            ServiceDocument,
                                            ExecutableDocument,
                                            FieldTypeDefinition,
                                            FieldSelectionSet,
                                            Options
                                        )
                                    ),
                                    argo_field_wire_type:new(FieldAlias, WireType, Omittable3)
                            end,
                        RecordWireType1_Acc1_Acc2 =
                            case argo_record_wire_type:find(RecordWireType1_Acc1_Acc1, FieldAlias) of
                                {ok, FieldWireType} ->
                                    RecordWireType1_Acc1_Acc1;
                                {ok, ExistingFieldWireType} ->
                                    try
                                        merge_field_wire_type(
                                            argo_path_value:new(), ExistingFieldWireType, FieldWireType
                                        )
                                    of
                                        MergedFieldWireType = #argo_field_wire_type{} ->
                                            argo_record_wire_type:update(RecordWireType1_Acc1_Acc1, MergedFieldWireType)
                                    catch
                                        throw:{badshape, BadShapePath, _A, _B} ->
                                            error_with_info(
                                                badarg,
                                                [ServiceDocument, SelectionTypeDefinition, SelectionSet],
                                                #{
                                                    3 =>
                                                        {field_selection_type_shape_mismatch, #{
                                                            field_alias => FieldAlias, path => BadShapePath
                                                        }}
                                                }
                                            )
                                    end;
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
-spec merge_field_wire_type(Path, A, B) -> C when
    Path :: argo_path_value:t(),
    A :: argo_field_wire_type:t(),
    B :: argo_field_wire_type:t(),
    C :: argo_field_wire_type:t().
merge_field_wire_type(
    _Path,
    #argo_field_wire_type{name = Name, 'of' = Of, omittable = OmittableA},
    #argo_field_wire_type{name = Name, 'of' = Of, omittable = OmittableB}
) ->
    OmittableC = merge_field_wire_type_omittable(OmittableA, OmittableB),
    argo_field_wire_type:new(Name, Of, OmittableC);
merge_field_wire_type(
    Path1,
    #argo_field_wire_type{
        name = Name, 'of' = #argo_wire_type{inner = RecordA = #argo_record_wire_type{}}, omittable = OmittableA
    },
    #argo_field_wire_type{
        name = Name, 'of' = #argo_wire_type{inner = RecordB = #argo_record_wire_type{}}, omittable = OmittableB
    }
) ->
    Path2 = argo_path_value:push_field_name(Path1, Name),
    RecordC = merge_record_wire_type(Path2, RecordA, RecordB),
    Of = argo_wire_type:record(RecordC),
    OmittableC = merge_field_wire_type_omittable(OmittableA, OmittableB),
    argo_field_wire_type:new(Name, Of, OmittableC);
merge_field_wire_type(Path, A = #argo_field_wire_type{}, B = #argo_field_wire_type{}) ->
    throw({badshape, Path, A, B}).

%% @private
-spec merge_field_wire_type_omittable(OmittableA, OmittableB) -> OmittableC when
    OmittableA :: argo_field_wire_type:omittable(),
    OmittableB :: argo_field_wire_type:omittable(),
    OmittableC :: argo_field_wire_type:omittable().
merge_field_wire_type_omittable(OmittableA, OmittableB) when is_boolean(OmittableA) andalso is_boolean(OmittableB) ->
    OmittableA orelse OmittableB.

%% @private
-spec merge_record_wire_type(Path, A, B) -> C when
    Path :: argo_path_value:t(),
    A :: argo_record_wire_type:t(),
    B :: argo_record_wire_type:t(),
    C :: argo_record_wire_type:t().
merge_record_wire_type(
    _Path,
    Same = #argo_record_wire_type{},
    Same = #argo_record_wire_type{}
) ->
    Same;
merge_record_wire_type(
    Path,
    #argo_record_wire_type{fields = FieldsA},
    #argo_record_wire_type{fields = FieldsB}
) ->
    FieldsC = merge_record_wire_type_fields(Path, argo_index_map:iterator(FieldsA), FieldsB, argo_index_map:new()),
    RecordC = argo_record_wire_type:new(FieldsC),
    RecordC.

%% @private
-spec merge_record_wire_type_fields(Path, FieldsBIterator, FieldsC) -> FieldsC when
    Path :: argo_path_value:t(),
    FieldsBIterator :: argo_index_map:iterator(Name, FieldWireType),
    FieldsC :: argo_index_map:t(Name, FieldWireType),
    Name :: argo_types:name(),
    FieldWireType :: argo_field_wire_type:t().
merge_record_wire_type_fields(Path1, FieldsBIterator1, FieldsC1) ->
    case argo_index_map:next(FieldsBIterator1) of
        none ->
            FieldsC1;
        {_Index, Name, FieldWireTypeB = #argo_field_wire_type{name = Name, 'of' = Of}, FieldsBIterator2} ->
            case argo_index_map:find(Name, FieldsC1) of
                {ok, _FieldWireTypeC = #argo_field_wire_type{name = Name, 'of' = Of}} ->
                    merge_record_wire_type_fields(Path1, FieldsBIterator2, FieldsC1);
                error ->
                    OmittableC = true,
                    FieldWireTypeC = FieldWireTypeB#argo_field_wire_type{omittable = OmittableC},
                    FieldsC2 = argo_index_map:put(Name, FieldWireTypeC, FieldsC1),
                    merge_record_wire_type_fields(Path1, FieldsBIterator2, FieldsC2)
            end
    end.

%% @private
-spec merge_record_wire_type_fields(Path, FieldsAIterator, FieldsB, FieldsC) -> FieldsC when
    Path :: argo_path_value:t(),
    FieldsAIterator :: argo_index_map:iterator(Name, FieldWireType),
    FieldsB :: argo_index_map:t(Name, FieldWireType),
    FieldsC :: argo_index_map:t(Name, FieldWireType),
    Name :: argo_types:name(),
    FieldWireType :: argo_field_wire_type:t().
merge_record_wire_type_fields(Path1, FieldsAIterator1, FieldsB, FieldsC1) ->
    case argo_index_map:next(FieldsAIterator1) of
        none ->
            merge_record_wire_type_fields(Path1, argo_index_map:iterator(FieldsB), FieldsC1);
        {_Index, Name, FieldWireTypeA, FieldsAIterator2} ->
            case argo_index_map:find(Name, FieldsB) of
                {ok, FieldWireTypeA} ->
                    FieldsC2 = argo_index_map:put(Name, FieldWireTypeA, FieldsC1),
                    merge_record_wire_type_fields(Path1, FieldsAIterator2, FieldsB, FieldsC2);
                {ok, FieldWireTypeB} ->
                    Path2 = argo_path_value:push_field_name(Path1, Name),
                    FieldWireTypeC = merge_field_wire_type(Path2, FieldWireTypeA, FieldWireTypeB),
                    FieldsC2 = argo_index_map:put(Name, FieldWireTypeC, FieldsC1),
                    merge_record_wire_type_fields(Path1, FieldsAIterator2, FieldsB, FieldsC2);
                error ->
                    OmittableC = true,
                    FieldWireTypeC = FieldWireTypeA#argo_field_wire_type{omittable = OmittableC},
                    FieldsC2 = argo_index_map:put(Name, FieldWireTypeC, FieldsC1),
                    merge_record_wire_type_fields(Path1, FieldsAIterator2, FieldsB, FieldsC2)
            end
    end.

%% @private
-spec always_skip_selection(Directives) -> boolean() when Directives :: argo_graphql_directives:t().
always_skip_selection(#argo_graphql_directives{directives = Directives}) ->
    lists:any(fun always_skip_selection_filter/1, Directives).

%% @private
-spec always_skip_selection_filter(Directive) -> boolean() when Directive :: argo_graphql_directive:t().
always_skip_selection_filter(#argo_graphql_directive{name = <<"include">>, arguments = Arguments}) ->
    case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {boolean, false}}}} ->
            true;
        _ ->
            false
    end;
always_skip_selection_filter(#argo_graphql_directive{name = <<"skip">>, arguments = Arguments}) ->
    case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {boolean, true}}}} ->
            true;
        _ ->
            false
    end;
always_skip_selection_filter(#argo_graphql_directive{}) ->
    false.

%% @private
-spec maybe_omit_selection(Directives) -> boolean() when Directives :: argo_graphql_directives:t().
maybe_omit_selection(#argo_graphql_directives{directives = Directives}) ->
    lists:any(fun maybe_omit_selection_filter/1, Directives).

%% @private
-spec maybe_omit_selection_filter(Directive) -> boolean() when Directive :: argo_graphql_directive:t().
maybe_omit_selection_filter(#argo_graphql_directive{name = <<"defer">>, arguments = Arguments}) ->
    case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {boolean, true}}}} ->
            true;
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {variable, _}}}} ->
            true;
        error ->
            % `@defer` without `if` defaults to `true`
            true;
        _ ->
            false
    end;
maybe_omit_selection_filter(#argo_graphql_directive{name = <<"include">>, arguments = Arguments}) ->
    case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {variable, _}}}} ->
            true;
        _ ->
            false
    end;
maybe_omit_selection_filter(#argo_graphql_directive{name = <<"skip">>, arguments = Arguments}) ->
    case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {variable, _}}}} ->
            true;
        _ ->
            false
    end;
maybe_omit_selection_filter(#argo_graphql_directive{name = <<"stream">>, arguments = Arguments}) ->
    case argo_graphql_arguments:find_argument(Arguments, <<"if">>) of
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {boolean, true}}}} ->
            true;
        {ok, #argo_graphql_argument{value = #argo_graphql_value{inner = {variable, _}}}} ->
            true;
        error ->
            % `@stream` without `if` defaults to `true`
            true;
        _ ->
            false
    end;
maybe_omit_selection_filter(#argo_graphql_directive{}) ->
    false.

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
-spec get_field_definition(TypeDefinition, FieldName, ServiceDocument, Options) -> FieldDefinition when
    TypeDefinition :: argo_graphql_type_definition:t(),
    FieldName :: argo_types:name(),
    ServiceDocument :: argo_graphql_service_document:t(),
    Options :: options(),
    FieldDefinition :: argo_graphql_field_definition:t().
get_field_definition(
    TypeDefinition = #argo_graphql_type_definition{},
    FieldName,
    ServiceDocument = #argo_graphql_service_document{},
    Options
) when is_binary(FieldName) andalso is_map(Options) ->
    try argo_graphql_type_definition:get_field_definition(TypeDefinition, FieldName, ServiceDocument) of
        FieldDefinition = #argo_graphql_field_definition{} ->
            FieldDefinition
    catch
        error:badarg:Stacktrace ->
            case maps:find(resolver, Options) of
                {ok, Resolver} when is_atom(Resolver) ->
                    Result = argo_typer_resolver:find_field_definition(
                        Resolver, TypeDefinition, FieldName, ServiceDocument
                    ),
                    case Result of
                        {ok, FieldDefinition = #argo_graphql_field_definition{}} ->
                            FieldDefinition;
                        error ->
                            erlang:raise(error, badarg, Stacktrace)
                    end;
                error ->
                    erlang:raise(error, badarg, Stacktrace)
            end
    end.

%% @private
-spec get_type_definition(ServiceDocument, TypeName, Options) -> TypeDefinition when
    ServiceDocument :: argo_graphql_service_document:t(),
    TypeName :: argo_types:name(),
    Options :: options(),
    TypeDefinition :: argo_graphql_type_definition:t().
get_type_definition(ServiceDocument = #argo_graphql_service_document{}, TypeName, Options) when
    is_binary(TypeName) andalso is_map(Options)
->
    try argo_graphql_service_document:get_type_definition(ServiceDocument, TypeName) of
        TypeDefinition = #argo_graphql_type_definition{} ->
            TypeDefinition
    catch
        error:badarg:Stacktrace ->
            case maps:find(resolver, Options) of
                {ok, Resolver} when is_atom(Resolver) ->
                    Result = argo_typer_resolver:find_type_definition(Resolver, ServiceDocument, TypeName),
                    case Result of
                        {ok, TypeDefinition = #argo_graphql_type_definition{}} ->
                            TypeDefinition;
                        error ->
                            erlang:raise(error, badarg, Stacktrace)
                    end;
                error ->
                    erlang:raise(error, badarg, Stacktrace)
            end
    end.

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
