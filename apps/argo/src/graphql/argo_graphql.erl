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
-module(argo_graphql).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_debug_type).

-include_lib("argo/include/argo_graphql.hrl").

%% argo_debug_type callbacks
-export([
    display/3,
    format/1,
    format/2
]).

%% API
-export([
    xform/3
]).

%% Types
-type xform_action() :: cont | skip.
-type xform_result(TypeOut, AccOut) :: xform_action() | {xform_action(), AccOut} | {xform_action(), TypeOut, AccOut}.
-type xform_func(Type, Acc) :: xform_func(Type, Acc, Type, Acc).
-type xform_func(TypeIn, AccIn, TypeOut, AccOut) :: fun((TypeIn, AccIn) -> xform_result(TypeOut, AccOut)).

-export_type([
    xform_action/0,
    xform_result/2,
    xform_func/2,
    xform_func/4
]).

%% Macros
-define(DEFAULT_OPTIONS, #{}).
-define(is_record(X), (is_tuple((X)) andalso tuple_size((X)) > 0 andalso is_atom(element(1, (X))))).

%%%=============================================================================
%%% argo_debug_type callbacks
%%%=============================================================================

-spec display(IoDevice, Type, Options) -> ok when
    IoDevice :: io:device(), Type :: dynamic(), Options :: argo_graphql_printer:options().
display(IoDevice, Type, Options) when not is_list(IoDevice) andalso ?is_record(Type) andalso is_map(Options) ->
    Module = element(1, Type),
    Printer1 = argo_graphql_printer:new_io_device(IoDevice, Options),
    Printer2 = Module:format(Printer1, Type),
    case argo_graphql_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(Type) -> Output when Type :: dynamic(), Output :: unicode:unicode_binary().
format(Type) when ?is_record(Type) ->
    format(Type, ?DEFAULT_OPTIONS).

-spec format(Type, Options) -> Output when
    Type :: dynamic(), Options :: argo_graphql_printer:options(), Output :: unicode:unicode_binary().
format(Type, Options) when ?is_record(Type) andalso is_map(Options) ->
    Module = element(1, Type),
    Printer1 = argo_graphql_printer:new_string(Options),
    Printer2 = Module:format(Printer1, Type),
    case argo_graphql_printer:finalize(Printer2) of
        Output when is_list(Output) ->
            argo_types:unicode_binary(Output)
    end.

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec xform(TypeIn, AccIn, Fun) -> {TypeOut, AccOut} when
    TypeIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(TypeIn, AccIn, TypeOut, AccOut),
    TypeOut :: dynamic(),
    AccOut :: dynamic().
xform(T1, Acc1, Fun) when is_function(Fun, 2) ->
    case xform_normalize(T1, Acc1, Fun(T1, Acc1)) of
        {cont, T2, Acc2} ->
            case T2 of
                #argo_graphql_argument{value = Value1} ->
                    {Value2, Acc3} = xform(Value1, Acc2, Fun),
                    T3 = T2#argo_graphql_argument{value = Value2},
                    {T3, Acc3};
                #argo_graphql_argument_const{value = ValueConst1} ->
                    {ValueConst2, Acc3} = xform(ValueConst1, Acc2, Fun),
                    T3 = T2#argo_graphql_argument_const{value = ValueConst2},
                    {T3, Acc3};
                #argo_graphql_arguments{arguments = Arguments1} ->
                    {Arguments2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _ArgumentName, Argument1, {Arguments1_Acc1, Acc2_Acc1}) ->
                            {Argument2 = #argo_graphql_argument{name = ArgumentName}, Acc2_Acc2} = xform(
                                Argument1, Acc2_Acc1, Fun
                            ),
                            Arguments1_Acc2 = argo_index_map:put(ArgumentName, Argument2, Arguments1_Acc1),
                            {Arguments1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        Arguments1
                    ),
                    T3 = T2#argo_graphql_arguments{arguments = Arguments2},
                    {T3, Acc3};
                #argo_graphql_arguments_const{arguments = ArgumentsConst1} ->
                    {ArgumentsConst2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _ArgumentConstName, ArgumentConst1, {ArgumentsConst1_Acc1, Acc2_Acc1}) ->
                            {ArgumentConst2 = #argo_graphql_argument_const{name = ArgumentConstName}, Acc2_Acc2} = xform(
                                ArgumentConst1, Acc2_Acc1, Fun
                            ),
                            ArgumentsConst1_Acc2 = argo_index_map:put(
                                ArgumentConstName, ArgumentConst2, ArgumentsConst1_Acc1
                            ),
                            {ArgumentsConst1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        ArgumentsConst1
                    ),
                    T3 = T2#argo_graphql_arguments_const{arguments = ArgumentsConst2},
                    {T3, Acc3};
                #argo_graphql_arguments_definition{inputs = InputsMap1} ->
                    {InputsMap2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _InputName, InputValueDefinition1, {InputsMap1_Acc1, Acc2_Acc1}) ->
                            {InputValueDefinition2 = #argo_graphql_input_value_definition{name = InputName}, Acc2_Acc2} = xform(
                                InputValueDefinition1, Acc2_Acc1, Fun
                            ),
                            InputsMap1_Acc2 = argo_index_map:put(InputName, InputValueDefinition2, InputsMap1_Acc1),
                            {InputsMap1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        InputsMap1
                    ),
                    T3 = T2#argo_graphql_arguments_definition{inputs = InputsMap2},
                    {T3, Acc3};
                #argo_graphql_directive{arguments = Arguments1} ->
                    {Arguments2, Acc3} = xform(Arguments1, Acc2, Fun),
                    T3 = T2#argo_graphql_directive{arguments = Arguments2},
                    {T3, Acc3};
                #argo_graphql_directive_const{arguments = ArgumentsConst1} ->
                    {ArgumentsConst2, Acc3} = xform(ArgumentsConst1, Acc2, Fun),
                    T3 = T2#argo_graphql_directive_const{arguments = ArgumentsConst2},
                    {T3, Acc3};
                #argo_graphql_directives{directives = Directives1} ->
                    {Directives2, Acc3} = lists:foldl(
                        fun(Directive1, {Directives1_Acc1, Acc2_Acc1}) ->
                            {Directive2, Acc2_Acc2} = xform(Directive1, Acc2_Acc1, Fun),
                            Directives1_Acc2 = [Directive2 | Directives1_Acc1],
                            {Directives1_Acc2, Acc2_Acc2}
                        end,
                        {[], Acc2},
                        Directives1
                    ),
                    T3 = T2#argo_graphql_directives{directives = lists:reverse(Directives2)},
                    {T3, Acc3};
                #argo_graphql_directives_const{directives = DirectivesConst1} ->
                    {DirectivesConst2, Acc3} = lists:foldl(
                        fun(DirectiveConst1, {DirectivesConst1_Acc1, Acc2_Acc1}) ->
                            {DirectiveConst2, Acc2_Acc2} = xform(DirectiveConst1, Acc2_Acc1, Fun),
                            DirectivesConst1_Acc2 = [DirectiveConst2 | DirectivesConst1_Acc1],
                            {DirectivesConst1_Acc2, Acc2_Acc2}
                        end,
                        {[], Acc2},
                        DirectivesConst1
                    ),
                    T3 = T2#argo_graphql_directives_const{directives = lists:reverse(DirectivesConst2)},
                    {T3, Acc3};
                #argo_graphql_directive_definition{arguments = ArgumentsDefinition1} ->
                    {ArgumentsDefinition2, Acc3} = xform(ArgumentsDefinition1, Acc2, Fun),
                    T3 = T2#argo_graphql_directive_definition{arguments = ArgumentsDefinition2},
                    {T3, Acc3};
                #argo_graphql_enum_type_definition{values = EnumValueDefinitionMap1} ->
                    {EnumValueDefinitionMap2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _EnumValue, EnumValueDefinition1, {EnumValueDefinitionMap1_Acc1, Acc2_Acc1}) ->
                            {EnumValueDefinition2 = #argo_graphql_enum_value_definition{value = EnumValue}, Acc2_Acc2} = xform(
                                EnumValueDefinition1, Acc2_Acc1, Fun
                            ),
                            EnumValueDefinitionMap1_Acc2 = argo_index_map:put(
                                EnumValue, EnumValueDefinition2, EnumValueDefinitionMap1_Acc1
                            ),
                            {EnumValueDefinitionMap1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        EnumValueDefinitionMap1
                    ),
                    T3 = T2#argo_graphql_enum_type_definition{values = EnumValueDefinitionMap2},
                    {T3, Acc3};
                #argo_graphql_enum_value_definition{directives = DirectivesConst1} ->
                    {DirectivesConst2, Acc3} = xform(DirectivesConst1, Acc2, Fun),
                    T3 = T2#argo_graphql_enum_value_definition{directives = DirectivesConst2},
                    {T3, Acc3};
                #argo_graphql_executable_document{
                    operation_definitions = OperationDefinitions1, fragment_definitions = FragmentDefinitions1
                } ->
                    {OperationDefinitions2, Acc3} =
                        case OperationDefinitions1 of
                            none ->
                                {OperationDefinitions1, Acc2};
                            {single, OperationDefinition1} ->
                                Acc2_Acc1 = Acc2,
                                {OperationDefinition2, Acc2_Acc2} = xform(OperationDefinition1, Acc2_Acc1, Fun),
                                {{single, OperationDefinition2}, Acc2_Acc2};
                            {multiple, OperationDefinitionsMap1} ->
                                Acc2_Acc1 = Acc2,
                                {OperationDefinitionsMap2, Acc2_Acc2} = maps:fold(
                                    fun(
                                        _OperationName,
                                        OperationDefinition1,
                                        {OperationDefinitionsMap1_Acc1, Acc2_Acc1_Acc1}
                                    ) ->
                                        case xform(OperationDefinition1, Acc2_Acc1_Acc1, Fun) of
                                            {
                                                OperationDefinition2 = #argo_graphql_operation_definition{
                                                    name = {some, OperationName}
                                                },
                                                Acc2_Acc1_Acc2
                                            } ->
                                                OperationDefinitionsMap1_Acc2 = maps:put(
                                                    OperationName, OperationDefinition2, OperationDefinitionsMap1_Acc1
                                                ),
                                                {OperationDefinitionsMap1_Acc2, Acc2_Acc1_Acc2}
                                        end
                                    end,
                                    {maps:new(), Acc2_Acc1},
                                    OperationDefinitionsMap1
                                ),
                                {{multiple, OperationDefinitionsMap2}, Acc2_Acc2}
                        end,
                    {FragmentDefinitions2, Acc4} =
                        maps:fold(
                            fun(_FragmentName, FragmentDefinition1, {FragmentDefinitions1_Acc1, Acc3_Acc1}) ->
                                case xform(FragmentDefinition1, Acc3_Acc1, Fun) of
                                    {
                                        FragmentDefinition2 = #argo_graphql_fragment_definition{name = FragmentName},
                                        Acc3_Acc2
                                    } ->
                                        FragmentDefinitions1_Acc2 = maps:put(
                                            FragmentName, FragmentDefinition2, FragmentDefinitions1_Acc1
                                        ),
                                        {FragmentDefinitions1_Acc2, Acc3_Acc2}
                                end
                            end,
                            {maps:new(), Acc3},
                            FragmentDefinitions1
                        ),
                    T3 = T2#argo_graphql_executable_document{
                        operation_definitions = OperationDefinitions2, fragment_definitions = FragmentDefinitions2
                    },
                    {T3, Acc4};
                #argo_graphql_field{arguments = Arguments1, directives = Directives1, selection_set = SelectionSet1} ->
                    {Arguments2, Acc3} = xform(Arguments1, Acc2, Fun),
                    {Directives2, Acc4} = xform(Directives1, Acc3, Fun),
                    {SelectionSet2, Acc5} = xform(SelectionSet1, Acc4, Fun),
                    T3 = T2#argo_graphql_field{
                        arguments = Arguments2, directives = Directives2, selection_set = SelectionSet2
                    },
                    {T3, Acc5};
                #argo_graphql_field_definition{
                    type = Type1, arguments = ArgumentsDefinition1, directives = DirectivesConst1
                } ->
                    {Type2, Acc3} = xform(Type1, Acc2, Fun),
                    {ArgumentsDefinition2, Acc4} = xform(ArgumentsDefinition1, Acc3, Fun),
                    {DirectivesConst2, Acc5} = xform(DirectivesConst1, Acc4, Fun),
                    T3 = T2#argo_graphql_field_definition{
                        type = Type2, arguments = ArgumentsDefinition2, directives = DirectivesConst2
                    },
                    {T3, Acc5};
                #argo_graphql_fragment_definition{selection_set = SelectionSet1, directives = Directives1} ->
                    {SelectionSet2, Acc3} = xform(SelectionSet1, Acc2, Fun),
                    {Directives2, Acc4} = xform(Directives1, Acc3, Fun),
                    T3 = T2#argo_graphql_fragment_definition{selection_set = SelectionSet2, directives = Directives2},
                    {T3, Acc4};
                #argo_graphql_fragment_spread{directives = Directives1} ->
                    {Directives2, Acc3} = xform(Directives1, Acc2, Fun),
                    T3 = T2#argo_graphql_fragment_spread{directives = Directives2},
                    {T3, Acc3};
                #argo_graphql_inline_fragment{selection_set = SelectionSet1, directives = Directives1} ->
                    {SelectionSet2, Acc3} = xform(SelectionSet1, Acc2, Fun),
                    {Directives2, Acc4} = xform(Directives1, Acc3, Fun),
                    T3 = T2#argo_graphql_inline_fragment{selection_set = SelectionSet2, directives = Directives2},
                    {T3, Acc4};
                #argo_graphql_input_object_type_definition{inputs = InputsMap1} ->
                    {InputsMap2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _InputName, InputValueDefinition1, {InputsMap1_Acc1, Acc2_Acc1}) ->
                            {InputValueDefinition2 = #argo_graphql_input_value_definition{name = InputName}, Acc2_Acc2} = xform(
                                InputValueDefinition1, Acc2_Acc1, Fun
                            ),
                            InputsMap1_Acc2 = argo_index_map:put(InputName, InputValueDefinition2, InputsMap1_Acc1),
                            {InputsMap1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        InputsMap1
                    ),
                    T3 = T2#argo_graphql_input_object_type_definition{inputs = InputsMap2},
                    {T3, Acc3};
                #argo_graphql_input_value_definition{
                    type = Type1, directives = DirectivesConst1, default_value = OptionDefaultValue1
                } ->
                    {Type2, Acc3} = xform(Type1, Acc2, Fun),
                    {DirectivesConst2, Acc4} = xform(DirectivesConst1, Acc3, Fun),
                    {OptionDefaultValue2, Acc5} =
                        case OptionDefaultValue1 of
                            none ->
                                {OptionDefaultValue1, Acc4};
                            {some, DefaultValue1} ->
                                Acc4_Acc1 = Acc4,
                                {DefaultValue2, Acc4_Acc2} = xform(DefaultValue1, Acc4_Acc1, Fun),
                                {{some, DefaultValue2}, Acc4_Acc2}
                        end,
                    T3 = T2#argo_graphql_input_value_definition{
                        type = Type2, directives = DirectivesConst2, default_value = OptionDefaultValue2
                    },
                    {T3, Acc5};
                #argo_graphql_interface_type_definition{fields = FieldsMap1} ->
                    {FieldsMap2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _FieldName, FieldDefinition1, {FieldsMap1_Acc1, Acc2_Acc1}) ->
                            {FieldDefinition2 = #argo_graphql_field_definition{name = FieldName}, Acc2_Acc2} = xform(
                                FieldDefinition1, Acc2_Acc1, Fun
                            ),
                            FieldsMap1_Acc2 = argo_index_map:put(FieldName, FieldDefinition2, FieldsMap1_Acc1),
                            {FieldsMap1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        FieldsMap1
                    ),
                    T3 = T2#argo_graphql_interface_type_definition{fields = FieldsMap2},
                    {T3, Acc3};
                #argo_graphql_list_type{type = Type1} ->
                    {Type2, Acc3} = xform(Type1, Acc2, Fun),
                    T3 = T2#argo_graphql_list_type{type = Type2},
                    {T3, Acc3};
                #argo_graphql_non_null_type{type = NamedType} when is_binary(NamedType) ->
                    {T2, Acc2};
                #argo_graphql_non_null_type{type = ListType1 = #argo_graphql_list_type{}} ->
                    {ListType2, Acc3} = xform(ListType1, Acc2, Fun),
                    T3 = T2#argo_graphql_non_null_type{type = ListType2},
                    {T3, Acc3};
                #argo_graphql_object_type_definition{fields = FieldsMap1} ->
                    {FieldsMap2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _FieldName, FieldDefinition1, {FieldsMap1_Acc1, Acc2_Acc1}) ->
                            {FieldDefinition2 = #argo_graphql_field_definition{name = FieldName}, Acc2_Acc2} = xform(
                                FieldDefinition1, Acc2_Acc1, Fun
                            ),
                            FieldsMap1_Acc2 = argo_index_map:put(FieldName, FieldDefinition2, FieldsMap1_Acc1),
                            {FieldsMap1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        FieldsMap1
                    ),
                    T3 = T2#argo_graphql_object_type_definition{fields = FieldsMap2},
                    {T3, Acc3};
                #argo_graphql_operation_definition{
                    variables_definition = VariablesDefinition1, directives = Directives1, selection_set = SelectionSet1
                } ->
                    {VariablesDefinition2, Acc3} = xform(VariablesDefinition1, Acc2, Fun),
                    {Directives2, Acc4} = xform(Directives1, Acc3, Fun),
                    {SelectionSet2, Acc5} = xform(SelectionSet1, Acc4, Fun),
                    T3 = T2#argo_graphql_operation_definition{
                        variables_definition = VariablesDefinition2,
                        directives = Directives2,
                        selection_set = SelectionSet2
                    },
                    {T3, Acc5};
                #argo_graphql_scalar_type_definition{} ->
                    {T2, Acc2};
                #argo_graphql_selection_set{selections = Selections1} ->
                    {Selections2, Acc3} = lists:foldl(
                        fun(Selection1, {Selections1_Acc1, Acc2_Acc1}) ->
                            {Selection2, Acc2_Acc2} = xform(Selection1, Acc2_Acc1, Fun),
                            Selections1_Acc2 = [Selection2 | Selections1_Acc1],
                            {Selections1_Acc2, Acc2_Acc2}
                        end,
                        {[], Acc2},
                        Selections1
                    ),
                    T3 = T2#argo_graphql_selection_set{selections = lists:reverse(Selections2)},
                    {T3, Acc3};
                #argo_graphql_service_document{
                    directives = DirectivesConst1,
                    directive_definitions = DirectivesDefinitions1,
                    type_definitions = TypeDefinitions1
                } ->
                    {DirectivesConst2, Acc3} = xform(DirectivesConst1, Acc2, Fun),
                    {DirectivesDefinitions2, Acc4} = maps:fold(
                        fun(_DirectiveName, DirectiveDefinition1, {DirectivesDefinitions1_Acc1, Acc3_Acc1}) ->
                            {
                                DirectiveDefinition2 = #argo_graphql_directive_definition{name = DirectiveName},
                                Acc3_Acc2
                            } = xform(DirectiveDefinition1, Acc3_Acc1, Fun),
                            DirectivesDefinitions1_Acc2 = maps:put(
                                DirectiveName, DirectiveDefinition2, DirectivesDefinitions1_Acc1
                            ),
                            {DirectivesDefinitions1_Acc2, Acc3_Acc2}
                        end,
                        {maps:new(), Acc3},
                        DirectivesDefinitions1
                    ),
                    {TypeDefinitions2, Acc5} = maps:fold(
                        fun(_TypeName, TypeDefinition1, {TypeDefinitions1_Acc1, Acc4_Acc1}) ->
                            {TypeDefinition2 = #argo_graphql_type_definition{name = TypeName}, Acc4_Acc2} = xform(
                                TypeDefinition1, Acc4_Acc1, Fun
                            ),
                            TypeDefinitions1_Acc2 = maps:put(TypeName, TypeDefinition2, TypeDefinitions1_Acc1),
                            {TypeDefinitions1_Acc2, Acc4_Acc2}
                        end,
                        {maps:new(), Acc4},
                        TypeDefinitions1
                    ),
                    T3 = T2#argo_graphql_service_document{
                        directives = DirectivesConst2,
                        directive_definitions = DirectivesDefinitions2,
                        type_definitions = TypeDefinitions2
                    },
                    {T3, Acc5};
                #argo_graphql_type{inner = NamedType} when is_binary(NamedType) ->
                    {T2, Acc2};
                #argo_graphql_type{inner = ListType1 = #argo_graphql_list_type{}} ->
                    {ListType2, Acc3} = xform(ListType1, Acc2, Fun),
                    T3 = T2#argo_graphql_type{inner = ListType2},
                    {T3, Acc3};
                #argo_graphql_type{inner = NonNullType1 = #argo_graphql_non_null_type{}} ->
                    {NonNullType2, Acc3} = xform(NonNullType1, Acc2, Fun),
                    T3 = T2#argo_graphql_type{inner = NonNullType2},
                    {T3, Acc3};
                #argo_graphql_type_definition{directives = DirectivesConst1, kind = Kind1} ->
                    {DirectivesConst2, Acc3} = xform(DirectivesConst1, Acc2, Fun),
                    {Kind2, Acc4} = xform(Kind1, Acc3, Fun),
                    T3 = T2#argo_graphql_type_definition{directives = DirectivesConst2, kind = Kind2},
                    {T3, Acc4};
                #argo_graphql_union_type_definition{} ->
                    {T2, Acc2};
                #argo_graphql_value{} ->
                    {T2, Acc2};
                #argo_graphql_value_const{} ->
                    {T2, Acc2};
                #argo_graphql_variable_definition{
                    type = Type1, directives = DirectivesConst1, default_value = OptionDefaultValue1
                } ->
                    {Type2, Acc3} = xform(Type1, Acc2, Fun),
                    {DirectivesConst2, Acc4} = xform(DirectivesConst1, Acc3, Fun),
                    {OptionDefaultValue2, Acc5} =
                        case OptionDefaultValue1 of
                            none ->
                                {OptionDefaultValue1, Acc4};
                            {some, DefaultValue1} ->
                                Acc4_Acc1 = Acc4,
                                {DefaultValue2, Acc4_Acc2} = xform(DefaultValue1, Acc4_Acc1, Fun),
                                {{some, DefaultValue2}, Acc4_Acc2}
                        end,
                    T3 = T2#argo_graphql_variable_definition{
                        type = Type2, directives = DirectivesConst2, default_value = OptionDefaultValue2
                    },
                    {T3, Acc5};
                #argo_graphql_variables_definition{variables = VariablesMap1} ->
                    {VariablesMap2, Acc3} = argo_index_map:foldl(
                        fun(_Index, _VariableName, VariableDefinition1, {VariablesMap1_Acc1, Acc2_Acc1}) ->
                            {
                                VariableDefinition2 = #argo_graphql_variable_definition{variable = VariableName},
                                Acc2_Acc2
                            } = xform(VariableDefinition1, Acc2_Acc1, Fun),
                            VariablesMap1_Acc2 = argo_index_map:put(
                                VariableName, VariableDefinition2, VariablesMap1_Acc1
                            ),
                            {VariablesMap1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        VariablesMap1
                    ),
                    T3 = T2#argo_graphql_variables_definition{variables = VariablesMap2},
                    {T3, Acc3}
            end;
        {skip, T2, Acc2} ->
            {T2, Acc2}
    end.

%% @private
-spec xform_normalize(TypeIn, AccIn, Result) -> {Action, TypeOut, AccOut} when
    TypeIn :: dynamic(),
    AccIn :: dynamic(),
    Result :: xform_result(TypeOut, AccOut),
    Action :: xform_action(),
    TypeOut :: dynamic(),
    AccOut :: dynamic().
xform_normalize(TypeIn, AccIn, Action) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeIn, AccIn};
xform_normalize(TypeIn, _AccIn, {Action, AccOut}) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeIn, AccOut};
xform_normalize(_TypeIn, _AccIn, {Action, TypeOut, AccOut}) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeOut, AccOut}.
