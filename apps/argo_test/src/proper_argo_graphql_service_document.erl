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
%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(proper_argo_graphql_service_document).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("proper/include/proper.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% Helper API
-export([
    complex/1,
    complexity/0,
    mostly/2,
    option/1,
    with_complexity/1
]).
%% Primitive API
-export([
    builtin_scalar_type_name/0,
    field_name/1,
    non_builtin_type_name/0,
    unique_type_name/1
]).
%% GraphQL ServiceDocument API
-export([
    field_definition/2,
    field_definition/3,
    list_type/1,
    object_type_definition/2,
    scalar_type_definition/2,
    service_document/0,
    type/1,
    type_definition/2,
    type_definition/3
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type type_definition_options() :: #{
    kinds => output | input | [scalar | object | interface | union | enum | input_object],
    must_be_new => boolean()
}.

-export_type([
    type_definition_options/0
]).

%% Macros
-define(COMPLEXITY, 'proper_argo_complexity').
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

%%%=============================================================================
%%% Helper API functions
%%%=============================================================================

-spec complex(RawType :: proper_types:raw_type()) -> proper_types:type().
complex(RawType) ->
    with_complexity(
        ?SIZED(Size, begin
            Complexity = complexity(),
            ComplexSize = max(1, Size bsr Complexity),
            ?LET(NewSize, mostly(range(1, min(ComplexSize, 4)), range(1, ComplexSize)), resize(NewSize, RawType))
        end)
    ).

-spec complexity() -> pos_integer().
complexity() ->
    case parameter(?COMPLEXITY, 0) of
        Complexity when is_integer(Complexity) andalso Complexity >= 0 ->
            max(Complexity, 1)
    end.

-spec mostly(U :: proper_types:type(), T :: proper_types:type()) -> proper_types:type().
mostly(U, T) ->
    frequency([
        {100, U},
        {1, T}
    ]).

-spec option(T :: proper_types:type()) -> proper_types:type().
option(T) ->
    oneof([none, {some, T}]).

-spec with_complexity(RawType :: proper_types:raw_type()) -> proper_types:type().
with_complexity(RawType) ->
    Complexity =
        case parameter(?COMPLEXITY, 0) of
            C when is_integer(C) andalso C >= 0 ->
                C + 1
        end,
    with_parameter(?COMPLEXITY, Complexity, RawType).

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

-spec field_name(ExcludeFieldNames) -> proper_types:type() when ExcludeFieldNames :: #{argo_types:name() => []}.
field_name(ExcludeFieldNames) when is_map(ExcludeFieldNames) ->
    ?SUCHTHAT(Name, ?LANG:name(), not maps:is_key(Name, ExcludeFieldNames)).

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

-spec unique_type_name(ServiceDocument) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t().
unique_type_name(#argo_graphql_service_document{type_definitions = TypeDefinitions}) ->
    ?SUCHTHAT(Name, non_builtin_type_name(), not maps:is_key(Name, TypeDefinitions)).

%%%=============================================================================
%%% GraphQL ServiceDocument API functions
%%%=============================================================================

%% @private
-spec add_argo_directives(TypeDefinition) -> proper_types:type() when TypeDefinition :: argo_graphql_type_definition:t().
add_argo_directives(TypeDefinition1 = #argo_graphql_type_definition{kind = Kind}) when is_record(Kind, argo_graphql_scalar_type_definition) orelse is_record(Kind, argo_graphql_enum_type_definition) ->
    ?LET(
        {ArgoCodecType1, OptionDeduplicate},
        oneof([
            {string, option(oneof([default, true, false]))},
            {int, none},
            {float, none},
            {boolean, none},
            {bytes, option(oneof([default, true, false]))},
            {{fixed, non_neg_integer()}, none}
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
                        <<"FIXED">>
                end,
            ArgoCodec1 = argo_graphql_directive_const:new(<<"ArgoCodec">>),
            ArgoCodec2 = argo_graphql_directive_const:add_argument_const(ArgoCodec1, argo_graphql_argument_const:new(<<"codec">>, argo_graphql_value_const:enum(ArgoCodecType2))),
            ArgoCodec3 =
                case ArgoCodecType1 of
                    {fixed, FixedLength} ->
                        argo_graphql_directive_const:add_argument_const(ArgoCodec2, argo_graphql_argument_const:new(<<"fixedLength">>, argo_graphql_value_const:int(FixedLength)));
                    _ ->
                        ArgoCodec2
                end,
            TypeDefinition2 = argo_graphql_type_definition:add_directive_const(TypeDefinition1, ArgoCodec3),
            TypeDefinition3 =
                case OptionDeduplicate of
                    none ->
                        TypeDefinition2;
                    {some, default} ->
                        argo_graphql_type_definition:add_directive_const(TypeDefinition2, argo_graphql_directive_const:new(<<"ArgoDeduplicate">>));
                    {some, Deduplicate} when is_boolean(Deduplicate) ->
                        ArgoDeduplicate1 = argo_graphql_directive_const:new(<<"ArgoDeduplicate">>),
                        ArgoDeduplicate2 = argo_graphql_directive_const:add_argument_const(ArgoDeduplicate1, argo_graphql_argument_const:new(<<"deduplicate">>, argo_graphql_value_const:boolean(Deduplicate))),
                        argo_graphql_type_definition:add_directive_const(TypeDefinition2, ArgoDeduplicate2)
                end,
            TypeDefinition3
        end
    ).

%% @private
-spec add_object_field_definitions(ServiceDocument, ObjectTypeDefinition) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), ObjectTypeDefinition :: argo_graphql_object_type_definition:t().
add_object_field_definitions(ServiceDocument1 = #argo_graphql_service_document{}, ObjectTypeDefinition1 = #argo_graphql_object_type_definition{fields = FieldsMap}) ->
    ?LET(
        {ServiceDocument2, FieldDefinitionList},
        field_definition_list(ServiceDocument1, #{FieldName => [] || FieldName <- argo_index_map:keys(FieldsMap)}),
        {ServiceDocument2, lists:foldl(fun(FieldDefinition, ObjectTypeDefinition1_Acc1) ->
            ObjectTypeDefinition1_Acc2 = argo_graphql_object_type_definition:add_field_definition(ObjectTypeDefinition1_Acc1, FieldDefinition),
            ObjectTypeDefinition1_Acc2
        end, ObjectTypeDefinition1, FieldDefinitionList)}
    ).

-spec field_definition(ServiceDocument, FieldName) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), FieldName :: argo_types:name().
field_definition(ServiceDocument1, FieldName) ->
    field_definition(ServiceDocument1, FieldName, #{}).

-spec field_definition(ServiceDocument, FieldName, Options) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), FieldName :: argo_types:name(), Options :: type_definition_options().
field_definition(ServiceDocument1, FieldName, Options) ->
    ?LET(
        {ServiceDocument2, TypeDefinition},
        type_definition(ServiceDocument1, Options),
        ?LET(
            FieldDefinition,
            #argo_graphql_field_definition{
                name = FieldName,
                type = type(TypeDefinition#argo_graphql_type_definition.name),
                arguments = argo_graphql_arguments_definition:new(),
                description = option(?LANG:block_string_value()),
                directives = argo_graphql_directives_const:new()
            },
            {ServiceDocument2, FieldDefinition}
        )
    ).

%% @private
-spec field_definition_list(ServiceDocument, ExcludeFieldNames) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), ExcludeFieldNames :: #{argo_types:name() => []}.
field_definition_list(ServiceDocument1 = #argo_graphql_service_document{}, ExcludeFieldNames) ->
    complex(
        ?LET(
            FieldNameList,
            field_name_list(ExcludeFieldNames),
            field_definition_list_from_field_name_list(ServiceDocument1, FieldNameList)
        )
    ).

%% @private
-spec field_definition_list_from_field_name_list(ServiceDocument, FieldNameList) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), FieldNameList :: [FieldName], FieldName :: argo_types:name().
field_definition_list_from_field_name_list(ServiceDocument = #argo_graphql_service_document{}, []) ->
    exactly({ServiceDocument, []});
field_definition_list_from_field_name_list(ServiceDocument1 = #argo_graphql_service_document{}, [FieldName | FieldNameList]) ->
    ?LAZY(
        ?LET(
            {ServiceDocument2, FieldDefinition},
            field_definition(ServiceDocument1, FieldName),
            ?LET({ServiceDocument3, FieldDefinitionList}, field_definition_list_from_field_name_list(ServiceDocument2, FieldNameList), {ServiceDocument3, [FieldDefinition | FieldDefinitionList]})
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
                ?SIZED(Size, proper_types:noshrink(field_name_list(Size, InitialFieldName, ExcludeFieldNames#{InitialFieldName => []}))),
                proper_types:shrink_list(List)
            ),
            field_name_list_is_valid(FieldNameList)
        )
    ).

%% @private
-spec field_name_list_is_valid(FieldNameList) -> boolean() when FieldNameList :: [FieldName], FieldName :: argo_types:name().
field_name_list_is_valid(FieldNameList = [_ | _]) ->
    length(FieldNameList) =:= sets:size(sets:from_list(FieldNameList, [{version, 2}]));
field_name_list_is_valid([]) ->
    false.

%% @private
-spec field_name_list(Size, PreviousFieldName, ExcludeFieldNames) -> proper_types:type() when Size :: non_neg_integer(), PreviousFieldName :: argo_types:name(), ExcludeFieldNames :: #{argo_types:name() => []}.
field_name_list(Size, PreviousFieldName, ExcludeFieldNames) ->
    ?LAZY(
        frequency([
            {1, [PreviousFieldName]},
            {Size,
                ?LET(
                    NextFieldName,
                    field_name(ExcludeFieldNames),
                    ?LET(FieldNameList, field_name_list(Size - 1, NextFieldName, ExcludeFieldNames#{NextFieldName => []}), [
                        PreviousFieldName | FieldNameList
                    ])
                )}
        ])
    ).

-spec list_type(TypeName) -> proper_types:type() when TypeName :: argo_types:name().
list_type(TypeName) when is_binary(TypeName) ->
    ?LAZY(?LET(Type, type(TypeName), exactly(argo_graphql_type:list_type(argo_graphql_list_type:new(Type))))).

-spec object_type_definition(ServiceDocument, TypeName) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
object_type_definition(ServiceDocument1, TypeName) ->
    ObjectTypeDefinition1 = argo_graphql_object_type_definition:new(),
    TypeDefinition1 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition1),
    ServiceDocument2 = argo_graphql_service_document:add_type_definition(ServiceDocument1, TypeDefinition1),
    ?LET({ServiceDocument3, ObjectTypeDefinition2}, ?LAZY(add_object_field_definitions(ServiceDocument2, ObjectTypeDefinition1)), begin
        TypeDefinition2 = argo_graphql_type_definition:object_type_definition(TypeName, ObjectTypeDefinition2),
        ServiceDocument4 = argo_graphql_service_document:add_type_definition(ServiceDocument3, TypeDefinition2),
        {ServiceDocument4, TypeDefinition2}
    end).

-spec scalar_type_definition(ServiceDocument, TypeName) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name().
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
        ServiceDocument,
        service_document_is_instance(#argo_graphql_service_document{
            description = option(?LANG:block_string_value()),
            directives = argo_graphql_directives_const:new(),
            schema_defined = true,
            'query' = option(non_builtin_type_name()),
            mutation = option(non_builtin_type_name()),
            subscription = option(non_builtin_type_name()),
            directive_definitions = maps:new(),
            type_definitions = maps:new()
        }),
        service_document(ServiceDocument, ['query', mutation, subscription])
    ).

%% @private
service_document(ServiceDocument1 = #argo_graphql_service_document{'query' = {some, TypeName}}, ['query' | Rest]) ->
    ?LET({ServiceDocument2, _ObjectTypeDefinition}, type_definition(ServiceDocument1, TypeName, #{kinds => [object], must_be_new => true}), service_document(ServiceDocument2, Rest));
service_document(ServiceDocument1 = #argo_graphql_service_document{mutation = {some, TypeName}}, [mutation | Rest]) ->
    ?LET({ServiceDocument2, _ObjectTypeDefinition}, type_definition(ServiceDocument1, TypeName, #{kinds => [object], must_be_new => true}), service_document(ServiceDocument2, Rest));
service_document(ServiceDocument1 = #argo_graphql_service_document{subscription = {some, TypeName}}, [subscription | Rest]) ->
    ?LET({ServiceDocument2, _ObjectTypeDefinition}, type_definition(ServiceDocument1, TypeName, #{kinds => [object], must_be_new => true}), service_document(ServiceDocument2, Rest));
service_document(ServiceDocument1 = #argo_graphql_service_document{}, [_ | Rest]) ->
    service_document(ServiceDocument1, Rest);
service_document(ServiceDocument1, []) ->
    ServiceDocument1.

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
        ?LET(#argo_graphql_type{inner = ListType}, list_type(TypeName), exactly(argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(ListType))))
    ]).

-spec type_definition(ServiceDocument, Options | TypeName) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), Options :: type_definition_options(), TypeName :: argo_types:name().
type_definition(ServiceDocument = #argo_graphql_service_document{type_definitions = TypeDefinitionsMap}, Options) when is_map(Options) ->
    NewTypeDefinitionGen = ?LAZY(?LET(TypeName, unique_type_name(ServiceDocument), type_definition(ServiceDocument, TypeName, Options))),
    MustBeNew = maps:get(must_be_new, Options, false),
    case MustBeNew of
        false ->
            Kinds = type_definition_option_kinds(Options),
            case type_definition_list_by_kinds(maps:to_list(TypeDefinitionsMap), Kinds, []) of
                [] when is_map_key(scalar, Kinds) ->
                    BuiltinTypeDefinitionGen = ?LET(TypeName, builtin_scalar_type_name(), type_definition(ServiceDocument, TypeName, Options)),
                    mostly(BuiltinTypeDefinitionGen, NewTypeDefinitionGen);
                [] ->
                    NewTypeDefinitionGen;
                TypeDefinitionList = [_ | _] ->
                    ExitingTypeDefinitionGen = ?LET(TypeDefinition, oneof(TypeDefinitionList), {ServiceDocument, TypeDefinition}),
                    mostly(ExitingTypeDefinitionGen, NewTypeDefinitionGen)
            end;
        true ->
            NewTypeDefinitionGen
    end;
type_definition(ServiceDocument, TypeName) when is_binary(TypeName) ->
    type_definition(ServiceDocument, TypeName, #{}).

-spec type_definition(ServiceDocument, TypeName, Options) -> proper_types:type() when ServiceDocument :: argo_graphql_service_document:t(), TypeName :: argo_types:name(), Options :: type_definition_options().
type_definition(ServiceDocument1, TypeName, Options) ->
    Kinds = type_definition_option_kinds(Options),
    MustBeNew = maps:get(must_be_new, Options, false),
    case argo_graphql_service_document:find_type_definition(ServiceDocument1, TypeName) of
        {ok, _} when MustBeNew =:= true ->
            error_with_info(badarg, [ServiceDocument1, TypeName, Options], #{2 => {must_be_new_violation, #{type_name => TypeName}}});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_scalar_type_definition{}}} when is_map_key(scalar, Kinds) ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}}} when is_map_key(object, Kinds) ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_interface_type_definition{}}} when is_map_key(interface, Kinds) ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_union_type_definition{}}} when is_map_key(union, Kinds) ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_enum_type_definition{}}} when is_map_key(enum, Kinds) ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, TypeDefinition = #argo_graphql_type_definition{kind = #argo_graphql_input_object_type_definition{}}} when is_map_key(input_object, Kinds) ->
            exactly({ServiceDocument1, TypeDefinition});
        {ok, _} ->
            error_with_info(badarg, [ServiceDocument1, TypeName, Options], #{2 => {kinds_violation, #{kinds => sets:to_list(Kinds), type_name => TypeName}}});
        error ->
            ?LET(
                Kind,
                oneof(sets:to_list(Kinds)),
                case Kind of
                    scalar ->
                        ?LAZY(scalar_type_definition(ServiceDocument1, TypeName));
                    object ->
                        ?LAZY(object_type_definition(ServiceDocument1, TypeName));
                    % interface ->
                    %     ?LAZY(interface_type_definition(ServiceDocument1, TypeName));
                    % union ->
                    %     ?LAZY(union_type_definition(ServiceDocument1, TypeName));
                    % enum ->
                    %     ?LAZY(enum_type_definition(ServiceDocument1, TypeName));
                    % input_object ->
                    %     ?LAZY(input_object_type_definition(ServiceDocument1, TypeName))
                    _ ->
                        ?LAZY(scalar_type_definition(ServiceDocument1, TypeName))
                end
            )
    end.

%% @private
type_definition_list_by_kinds([{_TypeName, TypeDefinition} | TypeDefinitions], Kinds, Acc) ->
    case TypeDefinition of
        #argo_graphql_type_definition{kind = #argo_graphql_scalar_type_definition{}} when is_map_key(scalar, Kinds) ->
            type_definition_list_by_kinds(TypeDefinitions, Kinds, [TypeDefinition | Acc]);
        #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}} when is_map_key(object, Kinds) ->
            type_definition_list_by_kinds(TypeDefinitions, Kinds, [TypeDefinition | Acc]);
        #argo_graphql_type_definition{kind = #argo_graphql_interface_type_definition{}} when is_map_key(interface, Kinds) ->
            type_definition_list_by_kinds(TypeDefinitions, Kinds, [TypeDefinition | Acc]);
        #argo_graphql_type_definition{kind = #argo_graphql_union_type_definition{}} when is_map_key(union, Kinds) ->
            type_definition_list_by_kinds(TypeDefinitions, Kinds, [TypeDefinition | Acc]);
        #argo_graphql_type_definition{kind = #argo_graphql_enum_type_definition{}} when is_map_key(enum, Kinds) ->
            type_definition_list_by_kinds(TypeDefinitions, Kinds, [TypeDefinition | Acc]);
        #argo_graphql_type_definition{kind = #argo_graphql_input_object_type_definition{}} when is_map_key(input_object, Kinds) ->
            type_definition_list_by_kinds(TypeDefinitions, Kinds, [TypeDefinition | Acc]);
        _ ->
            type_definition_list_by_kinds(TypeDefinitions, Kinds, Acc)
    end;
type_definition_list_by_kinds([], _Kinds, Acc) ->
    Acc.

%% @private
type_definition_option_kinds(Options) ->
    case maps:get(kinds, Options, output) of
        output ->
            sets:from_list([scalar, object, interface, union, enum], [{version, 2}]);
        input ->
            sets:from_list([scalar, enum, input_object], [{version, 2}]);
        KindsList when is_list(KindsList) andalso length(KindsList) > 0 ->
            sets:from_list(KindsList, [{version, 2}])
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
format_error_description(_Key, Value) ->
    Value.
