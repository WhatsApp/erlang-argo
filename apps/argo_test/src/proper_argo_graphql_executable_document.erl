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
-module(proper_argo_graphql_executable_document).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% Context API
-export([
    context_build/2,
    context_field_names/2,
    context_find_field/2,
    context_frame_pop/1,
    context_frame_push/2,
    context_group_get/1,
    context_group_get/2,
    context_group_link/3,
    context_group_selector/1,
    context_group_unlink/2,
    context_size/1,
    context_stack_pop/1,
    context_stack_push/2,
    context_var_drop/3,
    context_var_get/2,
    context_var_list/2,
    context_var_update/3,
    context_weight/1
]).
%% GraphQL ExecutableDocument API
-export([
    executable_document/0,
    executable_document/1,
    field/3,
    field_alias_or_name/2,
    field_name/2,
    fragment_definition/2,
    fragment_definition/3,
    fragment_definition_list/2,
    fragment_name/1,
    fragment_spread/2,
    inline_fragment/2,
    operation_definition/3,
    optional_skip_or_include_directive_definition/1,
    selection_list/0,
    selections/2,
    type_condition/2,
    type_condition_list/2,
    value/2,
    variable_definition/2,
    variable_definition/3,
    variable_definition_list/2,
    variable_name/1
]).
%% Errors API
-export([
    format_error/2
]).

%% Records
-record(frame, {
    root :: var_fragment_definition() | var_operation_definition(),
    stack :: [var_field() | var_fragment_definition() | var_inline_fragment() | var_operation_definition()]
}).
-record(context, {
    service_document :: argo_graphql_service_document:t(),
    executable_document :: var_executable_document(),
    cached_field_names :: #{argo_types:name() => field_names()},
    clock :: clock(),
    parents :: #{var() => var()},
    groups_in :: #{var() => argo_index_set:t(var())},
    groups_out :: #{var() => var()},
    vars :: #{
        var_executable_document() => argo_graphql_executable_document:t(),
        var_field() => argo_graphql_field:t(),
        var_fragment_definition() => argo_graphql_fragment_definition:t(),
        var_inline_fragment() => argo_graphql_inline_fragment:t(),
        var_operation_definition() => argo_graphql_operation_definition:t()
    },
    frames :: [frame()]
}).
-record(var_executable_document, {clock :: clock()}).
-record(var_field, {clock :: clock()}).
-record(var_fragment_definition, {clock :: clock()}).
-record(var_inline_fragment, {clock :: clock()}).
-record(var_operation_definition, {clock :: clock()}).

%% Types
-type clock() :: non_neg_integer().
-type context() :: #context{}.
-type field_names() :: #{simple := [argo_types:name()], complex := [argo_types:name()]}.
-type frame() :: #frame{}.
-type group_selector() ::
    argo_graphql_field:t() | argo_graphql_fragment_definition:t() | argo_graphql_operation_definition:t().
-type var() ::
    var_executable_document()
    | var_field()
    | var_fragment_definition()
    | var_inline_fragment()
    | var_operation_definition().
-type var_executable_document() :: #var_executable_document{}.
-type var_field() :: #var_field{}.
-type var_fragment_definition() :: #var_fragment_definition{}.
-type var_inline_fragment() :: #var_inline_fragment{}.
-type var_operation_definition() :: #var_operation_definition{}.

-export_type([
    clock/0,
    context/0,
    field_names/0,
    frame/0,
    group_selector/0,
    var/0,
    var_executable_document/0,
    var_field/0,
    var_fragment_definition/0,
    var_inline_fragment/0,
    var_operation_definition/0
]).

%% Macros
-define(LANG, proper_argo_graphql_language).
% -define(is_empty_shape(X), ((X) =:= none orelse (is_map(X) andalso map_size(X) < 2))).
-define(is_operation_type(T), ((T) =:= 'query' orelse (T) =:= 'mutation' orelse (T) =:= 'subscription')).

%%%=============================================================================
%%% Context API functions
%%%=============================================================================

-spec context_build(ServiceDocument, ExecutableDocument) -> Context when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Context :: context().
context_build(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument = #argo_graphql_executable_document{}
) ->
    VarExecutableDocument = #var_executable_document{clock = 0},
    Context1 = #context{
        service_document = ServiceDocument,
        executable_document = VarExecutableDocument,
        cached_field_names = #{},
        clock = 1,
        parents = #{},
        groups_in = #{},
        groups_out = #{},
        vars = #{VarExecutableDocument => ExecutableDocument},
        frames = []
    },
    Context1.

-spec context_field_names(Context, TypeDefinition) -> {Context, FieldNames} when
    Context :: context(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    FieldNames :: field_names().
context_field_names(
    Context1 = #context{service_document = ServiceDocument, cached_field_names = FieldNamesMap1},
    TypeDefinition = #argo_graphql_type_definition{name = TypeName}
) ->
    case maps:find(TypeName, FieldNamesMap1) of
        {ok, FieldNames} ->
            {Context1, FieldNames};
        error ->
            Shape1 = argo_graphql_type_definition:get_shape(TypeDefinition, ServiceDocument),
            Shape2 = maps:without([type], Shape1),
            Shape3 = maps:put(
                <<"__typename">>, argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"String">>)), Shape2
            ),
            {SimpleMap, ComplexMap} = maps:fold(
                fun(FieldName, FieldType, {SimpleAcc, ComplexAcc}) ->
                    FieldTypeName = argo_graphql_type:get_type_name(FieldType),
                    FieldTypeDefinition = argo_graphql_service_document:get_type_definition(
                        ServiceDocument, FieldTypeName
                    ),
                    case FieldTypeDefinition of
                        #argo_graphql_type_definition{kind = #argo_graphql_scalar_type_definition{}} ->
                            {SimpleAcc#{FieldName => []}, ComplexAcc};
                        #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}} ->
                            {SimpleAcc, ComplexAcc#{FieldName => []}};
                        #argo_graphql_type_definition{kind = #argo_graphql_interface_type_definition{}} ->
                            {SimpleAcc, ComplexAcc#{FieldName => []}};
                        #argo_graphql_type_definition{kind = #argo_graphql_union_type_definition{}} ->
                            {SimpleAcc, ComplexAcc#{FieldName => []}};
                        #argo_graphql_type_definition{kind = #argo_graphql_enum_type_definition{}} ->
                            {SimpleAcc#{FieldName => []}, ComplexAcc}
                    end
                end,
                {maps:new(), maps:new()},
                Shape3
            ),
            FieldNames = #{
                simple => maps:keys(SimpleMap),
                complex => maps:keys(ComplexMap)
            },
            FieldNamesMap2 = maps:put(TypeName, FieldNames, FieldNamesMap1),
            Context2 = Context1#context{cached_field_names = FieldNamesMap2},
            {Context2, FieldNames}
    end.

-spec context_find_field(Context, FieldAliasOrName) -> {ok, Field} | error when
    Context :: context(),
    FieldAliasOrName :: argo_types:name(),
    Field :: argo_graphql_field:t().
context_find_field(Context = #context{executable_document = VarExecutableDocument}, FieldAliasOrName) when
    is_binary(FieldAliasOrName)
->
    ExecutableDocument = #argo_graphql_executable_document{} = context_var_get(Context, VarExecutableDocument),
    case context_group_selector(Context) of
        Field = #argo_graphql_field{} ->
            argo_graphql_field:find_field(Field, FieldAliasOrName, ExecutableDocument);
        FragmentDefinition = #argo_graphql_fragment_definition{} ->
            argo_graphql_fragment_definition:find_field(FragmentDefinition, FieldAliasOrName, ExecutableDocument);
        OperationDefinition = #argo_graphql_operation_definition{} ->
            argo_graphql_operation_definition:find_field(OperationDefinition, FieldAliasOrName, ExecutableDocument)
    end.

-spec context_frame_pop(Context) -> {Context, Root} when
    Context :: context(), Root :: argo_graphql_fragment_definition:t() | argo_graphql_operation_definition:t().
context_frame_pop(Context1 = #context{frames = [#frame{root = VarRoot, stack = [VarRoot]} | Frames1]}) ->
    Context2 = Context1#context{frames = Frames1},
    {Context3, Root} = context_var_drop(Context2, VarRoot, fun
        (
            Context2_Drop1 = #context{},
            ExecutableDocument1 = #argo_graphql_executable_document{},
            FragmentDefinition1 = #argo_graphql_fragment_definition{}
        ) ->
            ExecutableDocument2 = argo_graphql_executable_document:add_fragment_definition(
                ExecutableDocument1, FragmentDefinition1
            ),
            {Context2_Drop1, ExecutableDocument2, FragmentDefinition1};
        (
            Context2_Drop1 = #context{},
            ExecutableDocument1 = #argo_graphql_executable_document{},
            OperationDefinition1 = #argo_graphql_operation_definition{}
        ) ->
            ExecutableDocument2 = argo_graphql_executable_document:add_operation_definition(
                ExecutableDocument1, OperationDefinition1
            ),
            {Context2_Drop1, ExecutableDocument2, OperationDefinition1}
    end),
    {Context3, Root}.

-spec context_frame_push(Context, Root) -> {Context, VarRoot} when
    Context :: context(),
    Root :: argo_graphql_fragment_definition:t() | argo_graphql_operation_definition:t(),
    VarRoot :: var_fragment_definition() | var_operation_definition().
context_frame_push(
    Context1 = #context{
        executable_document = VarExecutableDocument, frames = Frames1 = [#frame{stack = [VarGroupParent | _]} | _]
    },
    FragmentDefinition = #argo_graphql_fragment_definition{}
) ->
    VarGroup = context_group_get(Context1, VarGroupParent),
    {Context2, VarFragmentDefinition = #var_fragment_definition{}} = context_var_keep(Context1, FragmentDefinition, #{
        parent => VarExecutableDocument, group => VarGroup
    }),
    Frame1 = #frame{
        root = VarFragmentDefinition,
        stack = [VarFragmentDefinition]
    },
    Frames2 = [Frame1 | Frames1],
    Context3 = Context2#context{frames = Frames2},
    {Context3, VarFragmentDefinition};
context_frame_push(
    Context1 = #context{executable_document = VarExecutableDocument, frames = Frames1},
    OperationDefinition = #argo_graphql_operation_definition{}
) ->
    {Context2, VarOperationDefinition = #var_operation_definition{}} = context_var_keep(
        Context1, OperationDefinition, #{
            parent => VarExecutableDocument, group => self
        }
    ),
    Frame1 = #frame{
        root = VarOperationDefinition,
        stack = [VarOperationDefinition]
    },
    Frames2 = [Frame1 | Frames1],
    Context3 = Context2#context{frames = Frames2},
    {Context3, VarOperationDefinition}.

-spec context_group_get(Context) -> VarGroup when Context :: context(), VarGroup :: var().
context_group_get(Context = #context{frames = [#frame{stack = [VarLeaf | _]} | _]}) ->
    context_group_get(Context, VarLeaf).

-spec context_group_get(Context, VarLeaf) -> VarGroup when Context :: context(), VarLeaf :: var(), VarGroup :: var().
context_group_get(_Context = #context{groups_out = GroupsOut}, VarLeaf) when is_map_key(VarLeaf, GroupsOut) ->
    VarGroup = maps:get(VarLeaf, GroupsOut),
    VarGroup.

-spec context_group_link(Context, VarLeaf, VarGroup) -> Context when
    Context :: context(), VarLeaf :: var(), VarGroup :: var().
context_group_link(Context1 = #context{groups_in = GroupsIn1, groups_out = GroupsOut1}, VarLeaf, VarGroup) when
    not is_map_key(VarLeaf, GroupsOut1)
->
    GroupsIn2 = maps:update_with(
        VarGroup,
        fun(Group) -> argo_index_set:add_element(VarLeaf, Group) end,
        argo_index_set:from_list([VarLeaf]),
        GroupsIn1
    ),
    GroupsOut2 = GroupsOut1#{VarLeaf => VarGroup},
    Context2 = Context1#context{groups_in = GroupsIn2, groups_out = GroupsOut2},
    Context2.

-spec context_group_selector(Context) -> GroupSelector when Context :: context(), GroupSelector :: group_selector().
context_group_selector(Context = #context{groups_in = GroupsIn}) ->
    VarGroup = context_group_get(Context),
    Group = maps:get(VarGroup, GroupsIn),
    {ok, VarFirst} = argo_index_set:first(Group),
    FirstSelector = context_var_get(Context, VarFirst),
    GroupSelector = argo_index_set:foldl(
        fun
            (_Index, VarSelector, GroupSelectorAcc) when VarSelector =:= VarFirst ->
                GroupSelectorAcc;
            (_Index, VarSelection, GroupSelectorAcc = #argo_graphql_field{}) ->
                case context_var_get(Context, VarSelection) of
                    Selection when
                        is_record(Selection, argo_graphql_field) orelse
                            is_record(Selection, argo_graphql_inline_fragment)
                    ->
                        argo_graphql_field:add_selection(GroupSelectorAcc, Selection);
                    #argo_graphql_fragment_definition{selection_set = SelectionSet} ->
                        InlineFragment = argo_graphql_inline_fragment:new(SelectionSet),
                        argo_graphql_field:add_selection(GroupSelectorAcc, InlineFragment)
                end;
            (_Index, VarSelection, GroupSelectorAcc = #argo_graphql_fragment_definition{}) ->
                case context_var_get(Context, VarSelection) of
                    Selection when
                        is_record(Selection, argo_graphql_field) orelse
                            is_record(Selection, argo_graphql_inline_fragment)
                    ->
                        argo_graphql_fragment_definition:add_selection(GroupSelectorAcc, Selection);
                    #argo_graphql_fragment_definition{selection_set = SelectionSet} ->
                        InlineFragment = argo_graphql_inline_fragment:new(SelectionSet),
                        argo_graphql_fragment_definition:add_selection(GroupSelectorAcc, InlineFragment)
                end;
            (_Index, VarSelection, GroupSelectorAcc = #argo_graphql_operation_definition{}) ->
                case context_var_get(Context, VarSelection) of
                    Selection when
                        is_record(Selection, argo_graphql_field) orelse
                            is_record(Selection, argo_graphql_inline_fragment)
                    ->
                        argo_graphql_operation_definition:add_selection(GroupSelectorAcc, Selection);
                    #argo_graphql_fragment_definition{selection_set = SelectionSet} ->
                        InlineFragment = argo_graphql_inline_fragment:new(SelectionSet),
                        argo_graphql_operation_definition:add_selection(GroupSelectorAcc, InlineFragment)
                end
        end,
        FirstSelector,
        Group
    ),
    GroupSelector.

-spec context_group_unlink(Context, VarLeaf) -> Context when Context :: context(), VarLeaf :: var().
context_group_unlink(Context1 = #context{groups_in = GroupsIn1, groups_out = GroupsOut1}, VarLeaf) when
    is_map_key(VarLeaf, GroupsOut1)
->
    {VarGroup, GroupsOut2} = maps:take(VarLeaf, GroupsOut1),
    GroupsIn2 = maps:update_with(
        VarGroup, fun(Group) -> argo_index_set:del_element(VarLeaf, Group) end, argo_index_set:new(), GroupsIn1
    ),
    GroupsIn3 =
        case argo_index_set:size(maps:get(VarGroup, GroupsIn2)) of
            0 ->
                maps:remove(VarGroup, GroupsIn2);
            _ ->
                GroupsIn2
        end,
    Context2 = Context1#context{groups_in = GroupsIn3, groups_out = GroupsOut2},
    Context2.

-spec context_size(context()) -> non_neg_integer().
context_size(#context{executable_document = ExecutableDocument, vars = Vars}) ->
    erlang:external_size({ExecutableDocument, Vars}).

-spec context_stack_pop(Context) -> {Context, Child} when
    Context :: context(), Child :: argo_graphql_field:t() | argo_graphql_inline_fragment:t().
context_stack_pop(
    Context1 = #context{frames = [Frame1 = #frame{root = VarRoot, stack = [VarChild, VarParent | Stack1]} | Frames1]}
) when VarRoot =/= VarChild ->
    Stack2 = [VarParent | Stack1],
    Frame2 = Frame1#frame{stack = Stack2},
    Frames2 = [Frame2 | Frames1],
    Context2 = Context1#context{frames = Frames2},
    {Context3, Child} = context_var_drop(Context2, VarChild, fun
        (Context2_Drop1 = #context{}, Field1 = #argo_graphql_field{}, Selection) when
            is_record(Selection, argo_graphql_field) orelse is_record(Selection, argo_graphql_inline_fragment)
        ->
            Field2 = argo_graphql_field:add_selection(Field1, Selection),
            {Context2_Drop1, Field2, Selection};
        (Context2_Drop1 = #context{}, FragmentDefinition1 = #argo_graphql_fragment_definition{}, Selection) when
            is_record(Selection, argo_graphql_field) orelse is_record(Selection, argo_graphql_inline_fragment)
        ->
            FragmentDefinition2 = argo_graphql_fragment_definition:add_selection(FragmentDefinition1, Selection),
            {Context2_Drop1, FragmentDefinition2, Selection};
        (Context2_Drop1 = #context{}, InlineFragment1 = #argo_graphql_inline_fragment{}, Selection) when
            is_record(Selection, argo_graphql_field) orelse is_record(Selection, argo_graphql_inline_fragment)
        ->
            InlineFragment2 = argo_graphql_inline_fragment:add_selection(InlineFragment1, Selection),
            {Context2_Drop1, InlineFragment2, Selection};
        (Context2_Drop1 = #context{}, OperationDefinition1 = #argo_graphql_operation_definition{}, Selection) when
            is_record(Selection, argo_graphql_field) orelse is_record(Selection, argo_graphql_inline_fragment)
        ->
            OperationDefinition2 = argo_graphql_operation_definition:add_selection(OperationDefinition1, Selection),
            {Context2_Drop1, OperationDefinition2, Selection}
    end),
    {Context3, Child}.

-spec context_stack_push(Context, Child) -> {Context, VarChild} when
    Context :: context(),
    Child :: argo_graphql_field:t() | argo_graphql_inline_fragment:t(),
    VarChild :: var_field() | var_inline_fragment().
context_stack_push(
    Context1 = #context{frames = [Frame1 = #frame{stack = [VarParent | Stack1]} | Frames1]},
    Field = #argo_graphql_field{}
) ->
    {Context2, VarField = #var_field{}} = context_var_keep(Context1, Field, #{parent => VarParent, group => self}),
    Stack2 = [VarField, VarParent | Stack1],
    Frame2 = Frame1#frame{stack = Stack2},
    Frames2 = [Frame2 | Frames1],
    Context3 = Context2#context{frames = Frames2},
    {Context3, VarField};
context_stack_push(
    Context1 = #context{frames = [Frame1 = #frame{stack = [VarParent | Stack1]} | Frames1]},
    InlineFragment = #argo_graphql_inline_fragment{}
) ->
    VarGroup = context_group_get(Context1, VarParent),
    {Context2, VarInlineFragment = #var_inline_fragment{}} = context_var_keep(Context1, InlineFragment, #{
        parent => VarParent, group => VarGroup
    }),
    Stack2 = [VarInlineFragment, VarParent | Stack1],
    Frame2 = Frame1#frame{stack = Stack2},
    Frames2 = [Frame2 | Frames1],
    Context3 = Context2#context{frames = Frames2},
    {Context3, VarInlineFragment}.

-spec context_var_drop(Context, VarChild, DropFun) -> {Context, Child} when
    Context :: context(),
    VarChild :: var(),
    DropFun :: fun((Context, Parent, Child) -> {Context, Parent, Child}),
    Parent :: dynamic(),
    Child :: dynamic().
context_var_drop(Context1 = #context{parents = Parents1, vars = Vars1}, VarChild, DropFun) when
    is_function(DropFun, 3)
->
    Child1 = maps:get(VarChild, Vars1),
    VarParent = maps:get(VarChild, Parents1),
    Parent1 = maps:get(VarParent, Vars1),
    {Context2 = #context{parents = Parents2, vars = Vars2}, Parent2, Child2} = DropFun(Context1, Parent1, Child1),
    Parents3 = maps:remove(VarChild, Parents2),
    Vars3 = maps:remove(VarChild, Vars2),
    Vars4 = maps:put(VarParent, Parent2, Vars3),
    Context3 = Context2#context{parents = Parents3, vars = Vars4},
    Context4 = context_group_unlink(Context3, VarChild),
    {Context4, Child2}.

-spec context_var_get(Context, Var) -> Child when
    Context :: context(),
    Var :: var(),
    Child ::
        argo_graphql_executable_document:t()
        | argo_graphql_field:t()
        | argo_graphql_fragment_definition:t()
        | argo_graphql_inline_fragment:t()
        | argo_graphql_operation_definition:t().
context_var_get(#context{vars = Vars}, Var) when is_map_key(Var, Vars) ->
    maps:get(Var, Vars).

%% @private
-spec context_var_keep(Context, Child, Options) -> {Context, VarChild} when
    Context :: context(),
    Child ::
        argo_graphql_field:t()
        | argo_graphql_fragment_definition:t()
        | argo_graphql_inline_fragment:t()
        | argo_graphql_operation_definition:t(),
    Options :: #{parent := VarParent, group := VarGroup | self},
    VarParent :: var(),
    VarGroup :: var(),
    VarChild :: var_field() | var_fragment_definition() | var_inline_fragment() | var_operation_definition().
context_var_keep(
    Context1 = #context{clock = Clock1}, Field = #argo_graphql_field{}, Options = #{parent := _, group := _}
) ->
    context_var_keep(Context1, #var_field{clock = Clock1}, Field, Options);
context_var_keep(
    Context1 = #context{clock = Clock1},
    FragmentDefinition = #argo_graphql_fragment_definition{},
    Options = #{parent := _, group := _}
) ->
    context_var_keep(Context1, #var_fragment_definition{clock = Clock1}, FragmentDefinition, Options);
context_var_keep(
    Context1 = #context{clock = Clock1},
    InlineFragment = #argo_graphql_inline_fragment{},
    Options = #{parent := _, group := _}
) ->
    context_var_keep(Context1, #var_inline_fragment{clock = Clock1}, InlineFragment, Options);
context_var_keep(
    Context1 = #context{clock = Clock1},
    OperationDefinition = #argo_graphql_operation_definition{},
    Options = #{parent := _, group := _}
) ->
    context_var_keep(Context1, #var_operation_definition{clock = Clock1}, OperationDefinition, Options).

%% @private
-spec context_var_keep(Context, VarChild, Child, Options) -> {Context, VarChild} when
    Context :: context(),
    VarChild :: var_field() | var_fragment_definition() | var_inline_fragment() | var_operation_definition(),
    Child ::
        argo_graphql_field:t()
        | argo_graphql_fragment_definition:t()
        | argo_graphql_inline_fragment:t()
        | argo_graphql_operation_definition:t(),
    Options :: #{parent := VarParent, group := VarGroup | self},
    VarParent :: var(),
    VarGroup :: var().
context_var_keep(
    Context1 = #context{clock = Clock1, parents = Parents1, vars = Vars1},
    VarChild,
    Child,
    Options = #{parent := VarParent, group := VarGroupOrSelf}
) when VarParent =/= VarChild ->
    case maps:find(VarChild, Vars1) of
        {ok, _ExistingChildObject} ->
            error_with_info(badarg, [Context1, VarChild, Child, Options], #{
                2 => {duplicate_context_var, #{var => VarChild}}
            });
        error ->
            Clock2 = Clock1 + 1,
            Parents2 = maps:put(VarChild, VarParent, Parents1),
            Vars2 = maps:put(VarChild, Child, Vars1),
            Context2 = Context1#context{clock = Clock2, parents = Parents2, vars = Vars2},
            Context3 =
                case VarGroupOrSelf of
                    self ->
                        context_group_link(Context2, VarChild, VarChild);
                    VarGroup ->
                        context_group_link(Context2, VarChild, VarGroup)
                end,
            {Context3, VarChild}
    end.

-spec context_var_list(Context, Label) -> VarList when Context :: context(), Label :: atom(), VarList :: [var()].
context_var_list(#context{vars = Vars}, Label) when is_atom(Label) ->
    maps:fold(
        fun(Var = {VarLabel, _}, _Object, Acc) ->
            case VarLabel =:= Label of
                false ->
                    Acc;
                true ->
                    [Var | Acc]
            end
        end,
        [],
        Vars
    ).

-spec context_var_update(Context, Var, UpdateFun) -> {Context, Var} when
    Context :: context(),
    Var :: var(),
    UpdateFun :: fun((Context, Object) -> {Context, Object}),
    Object :: dynamic().
context_var_update(Context1 = #context{}, Var, UpdateFun) when is_function(UpdateFun, 2) ->
    Object1 = context_var_get(Context1, Var),
    {Context2 = #context{vars = Vars1}, Object2} = UpdateFun(Context1, Object1),
    Vars2 = maps:put(Var, Object2, Vars1),
    Context3 = Context2#context{vars = Vars2},
    {Context3, Var}.

-spec context_weight(Context) -> Weight when Context :: context(), Weight :: non_neg_integer().
context_weight(Context = #context{}) ->
    max(1, context_size(Context) div 100).

%%%=============================================================================
%%% GraphQL ExecutableDocument API functions
%%%=============================================================================

-spec executable_document() -> proper_types:type().
executable_document() ->
    ?LET(
        ServiceDocument,
        proper_argo_graphql_service_document:service_document(),
        executable_document(ServiceDocument)
    ).

-spec executable_document(ServiceDocument) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t().
executable_document(ServiceDocument = #argo_graphql_service_document{}) ->
    ExecutableDocument1 = argo_graphql_executable_document:new(),
    ?LET(
        RootOperationType,
        proper_argo_graphql_service_document:root_operation_type(ServiceDocument),
        ?LET(
            {Context = #context{executable_document = VarExecutableDocument}, _OperationDefinition1},
            operation_definition(ServiceDocument, ExecutableDocument1, RootOperationType),
            context_var_get(Context, VarExecutableDocument)
        )
    ).

-spec field(Context, TypeDefinition | FieldDefinition, FieldNameOrAlias) -> proper_types:type() when
    Context :: context(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    FieldDefinition :: argo_graphql_field_definition:t(),
    FieldNameOrAlias :: argo_types:name().
field(
    Context1 = #context{service_document = ServiceDocument}, TypeDefinition = #argo_graphql_type_definition{}, FieldName
) when is_binary(FieldName) ->
    FieldDefinition = argo_graphql_type_definition:get_field_definition(TypeDefinition, FieldName, ServiceDocument),
    FieldAliasOrName = field_alias_or_name(Context1, FieldName),
    field(Context1, FieldDefinition, FieldAliasOrName);
field(
    Context1 = #context{service_document = ServiceDocument},
    _FieldDefinition = #argo_graphql_field_definition{
        name = FieldName, type = FieldType, arguments = ArgumentsDefinition
    },
    FieldAliasOrName
) when is_binary(FieldAliasOrName) ->
    FieldTypeName = argo_graphql_type:get_type_name(FieldType),
    FieldTypeDefinition = argo_graphql_service_document:get_type_definition(ServiceDocument, FieldTypeName),
    Field1 = argo_graphql_field:new(FieldName),
    Field2 =
        case FieldAliasOrName =:= FieldName of
            false ->
                argo_graphql_field:set_alias(Field1, {some, FieldAliasOrName});
            true ->
                Field1
        end,
    {Context2, VarField1} = context_stack_push(Context1, Field2),
    VarFieldGen1 = exactly({Context2, VarField1}),
    VarFieldGen2 = field_arguments(VarFieldGen1, ArgumentsDefinition),
    VarFieldGen3 = field_directives(VarFieldGen2),
    VarFieldGen4 = field_selections(VarFieldGen3, FieldTypeDefinition),
    ?LET(
        {Context3, VarField2},
        VarFieldGen4,
        begin
            VarField1 = VarField2,
            {Context4 = #context{}, Field3 = #argo_graphql_field{}} = context_stack_pop(Context3),
            exactly({Context4, Field3})
        end
    ).

%% @private
-spec field_arguments(VarFieldGen, ArgumentsDefinition) -> proper_types:type() when
    VarFieldGen :: proper_types:raw_type(), ArgumentsDefinition :: argo_graphql_arguments_definition:t().
field_arguments(VarFieldGen, ArgumentsDefinition = #argo_graphql_arguments_definition{}) ->
    ?LET(
        {Context1 = #context{}, VarField = #var_field{}},
        VarFieldGen,
        add_field_arguments(Context1, VarField, ArgumentsDefinition)
    ).

%% @private
-spec field_directives(VarFieldGen) -> proper_types:type() when VarFieldGen :: proper_types:raw_type().
field_directives(VarFieldGen) ->
    ?LET(
        {Context1 = #context{}, VarField = #var_field{}},
        VarFieldGen,
        add_field_directives(Context1, VarField)
    ).

%% @private
-spec field_selections(VarFieldGen, FieldTypeDefinition) -> proper_types:type() when
    VarFieldGen :: proper_types:raw_type(), FieldTypeDefinition :: argo_graphql_type_definition:t().
field_selections(VarFieldGen, FieldTypeDefinition = #argo_graphql_type_definition{kind = FieldTypeKind}) ->
    ?LET(
        {Context1 = #context{}, VarField = #var_field{}},
        VarFieldGen,
        case FieldTypeKind of
            _ when
                is_record(FieldTypeKind, argo_graphql_scalar_type_definition) orelse
                    is_record(FieldTypeKind, argo_graphql_enum_type_definition)
            ->
                exactly({Context1, VarField});
            _ when
                is_record(FieldTypeKind, argo_graphql_object_type_definition) orelse
                    is_record(FieldTypeKind, argo_graphql_interface_type_definition) orelse
                    is_record(FieldTypeKind, argo_graphql_union_type_definition)
            ->
                ?LET(
                    Context3 = #context{},
                    selections(Context1, FieldTypeDefinition),
                    exactly({Context3, VarField})
                );
            #argo_graphql_input_object_type_definition{} ->
                throw(oh_noes)
        end
    ).

-spec field_alias_or_name(Context, FieldName) -> FieldAliasOrName when
    Context :: context(),
    FieldName :: argo_types:name(),
    FieldAliasOrName :: argo_types:name().
field_alias_or_name(Context = #context{executable_document = VarExecutableDocument}, FieldName) when
    is_binary(FieldName)
->
    ExecutableDocument = #argo_graphql_executable_document{} = context_var_get(Context, VarExecutableDocument),
    FieldNameLength = byte_size(FieldName),
    Shape =
        case context_group_selector(Context) of
            Field = #argo_graphql_field{} ->
                argo_graphql_field:get_shape(Field, ExecutableDocument);
            FragmentDefinition = #argo_graphql_fragment_definition{} ->
                argo_graphql_fragment_definition:get_shape(FragmentDefinition, ExecutableDocument);
            OperationDefinition = #argo_graphql_operation_definition{} ->
                argo_graphql_operation_definition:get_shape(OperationDefinition, ExecutableDocument)
        end,
    Counter = maps:fold(
        fun
            (Key, _, Acc) when Key =:= FieldName ->
                max(0, Acc);
            (<<Prefix:FieldNameLength/bytes, "_alias_", Rest/bytes>>, _, Acc) when Prefix =:= FieldName ->
                max(binary_to_integer(Rest) + 1, Acc);
            (_, _, Acc) ->
                Acc
        end,
        -1,
        Shape
    ),
    case Counter of
        -1 ->
            FieldName;
        _ when is_integer(Counter) andalso Counter >= 0 ->
            <<FieldName/bytes, "_alias_", (integer_to_binary(Counter))/bytes>>
    end.

-spec field_name(Context, TypeDefinition) -> proper_types:type() when
    Context :: context(), TypeDefinition :: argo_graphql_type_definition:t().
field_name(Context1 = #context{}, TypeDefinition = #argo_graphql_type_definition{}) ->
    {Context2, #{simple := SimpleList, complex := ComplexList}} = context_field_names(Context1, TypeDefinition),
    SimpleWeight = 1 bsl complexity(),
    ComplexWeight = 1,
    SimpleWeightedList = [{SimpleWeight, FieldName} || FieldName <- SimpleList],
    ComplexWeightedList = [{ComplexWeight, FieldName} || FieldName <- ComplexList],
    WeightedList = SimpleWeightedList ++ ComplexWeightedList,
    ?LET(FieldName, ?SHRINK(frequency(WeightedList), [frequency(SimpleWeightedList)]), {Context2, FieldName}).

-spec fragment_definition(Context, TypeDefinition) -> proper_types:type() when
    Context :: context(), TypeDefinition :: argo_graphql_type_definition:t().
fragment_definition(Context1 = #context{}, TypeDefinition = #argo_graphql_type_definition{}) ->
    NewFragmentDefinitionGen = ?LAZY(
        ?LET(
            FragmentName,
            fragment_name(Context1),
            ?LET(
                TypeCondition,
                type_condition(Context1, TypeDefinition),
                fragment_definition(Context1, FragmentName, TypeCondition)
            )
        )
    ),
    case fragment_definition_list(Context1, TypeDefinition) of
        [] ->
            NewFragmentDefinitionGen;
        FragmentDefinitionList = [_ | _] ->
            ChanceOfExistingFragment = max(1 bsl complexity(), context_weight(Context1)),
            frequency([
                {ChanceOfExistingFragment,
                    ?LET(FragmentDefinition, oneof(FragmentDefinitionList), {Context1, FragmentDefinition})},
                {1, NewFragmentDefinitionGen}
            ])
    end.

-spec fragment_definition(Context, FragmentName, TypeCondition) -> proper_types:type() when
    Context :: context(), FragmentName :: argo_types:name(), TypeCondition :: argo_types:name().
fragment_definition(Context1 = #context{service_document = ServiceDocument}, FragmentName, TypeCondition) when
    is_binary(FragmentName) andalso is_binary(TypeCondition)
->
    TypeDefinition = argo_graphql_service_document:get_type_definition(ServiceDocument, TypeCondition),
    FragmentDefinition1 = argo_graphql_fragment_definition:new(
        FragmentName, TypeCondition, argo_graphql_selection_set:new()
    ),
    {Context2, VarFragmentDefinition1} = context_frame_push(Context1, FragmentDefinition1),
    VarFragmentDefinitionGen1 = exactly({Context2, VarFragmentDefinition1}),
    VarFragmentDefinitionGen2 = fragment_definition_directives(VarFragmentDefinitionGen1),
    VarFragmentDefinitionGen3 = fragment_definition_selections(VarFragmentDefinitionGen2, TypeDefinition),
    ?LET(
        {Context3, VarFragmentDefinition2},
        VarFragmentDefinitionGen3,
        begin
            VarFragmentDefinition1 = VarFragmentDefinition2,
            {Context4 = #context{}, FragmentDefinition2 = #argo_graphql_fragment_definition{}} = context_frame_pop(
                Context3
            ),
            exactly({Context4, FragmentDefinition2})
        end
    ).

%% @private
-spec fragment_definition_directives(VarFragmentDefinitionGen) -> proper_types:type() when
    VarFragmentDefinitionGen :: proper_types:raw_type().
fragment_definition_directives(VarFragmentDefinitionGen) ->
    ?LET(
        {Context1 = #context{}, VarFragmentDefinition = #var_fragment_definition{}},
        VarFragmentDefinitionGen,
        add_fragment_definition_directives(Context1, VarFragmentDefinition)
    ).

%% @private
-spec fragment_definition_selections(VarFragmentDefinitionGen, TypeDefinition) -> proper_types:type() when
    VarFragmentDefinitionGen :: proper_types:raw_type(), TypeDefinition :: argo_graphql_type_definition:t().
fragment_definition_selections(
    VarFragmentDefinitionGen, TypeDefinition = #argo_graphql_type_definition{kind = TypeKind}
) ->
    ?LET(
        {Context1 = #context{}, VarFragmentDefinition = #var_fragment_definition{}},
        VarFragmentDefinitionGen,
        case TypeKind of
            _ when
                is_record(TypeKind, argo_graphql_scalar_type_definition) orelse
                    is_record(TypeKind, argo_graphql_enum_type_definition)
            ->
                throw(oh_noes);
            _ when
                is_record(TypeKind, argo_graphql_object_type_definition) orelse
                    is_record(TypeKind, argo_graphql_interface_type_definition) orelse
                    is_record(TypeKind, argo_graphql_union_type_definition)
            ->
                ?LET(
                    Context2 = #context{},
                    selections(Context1, TypeDefinition),
                    exactly({Context2, VarFragmentDefinition})
                );
            #argo_graphql_input_object_type_definition{} ->
                throw(oh_noes)
        end
    ).

-spec fragment_definition_list(Context, TypeDefinition) -> FragmentDefinitionList when
    Context :: context(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    FragmentDefinitionList :: [FragmentDefinition],
    FragmentDefinition :: argo_graphql_fragment_definition:t().
fragment_definition_list(
    Context = #context{executable_document = VarExecutableDocument}, TypeDefinition = #argo_graphql_type_definition{}
) ->
    TypeConditionList = type_condition_list(Context, TypeDefinition),
    TypeConditionSet = #{TypeCondition => [] || TypeCondition <- TypeConditionList},
    ExecutableDocument =
        #argo_graphql_executable_document{fragment_definitions = FragmentDefinitionsMap} = context_var_get(
            Context, VarExecutableDocument
        ),
    fragment_definition_list(
        Context, ExecutableDocument, TypeConditionSet, maps:iterator(FragmentDefinitionsMap, ordered), []
    ).

%% @private
-spec fragment_definition_list(
    Context, ExecutableDocument, TypeConditionSet, FragmentDefinitionsIterator, FragmentDefinitionList
) -> FragmentDefinitionList when
    Context :: context(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    TypeConditionSet :: #{TypeCondition => []},
    TypeCondition :: argo_types:name(),
    FragmentDefinitionsIterator :: maps:iterator(FragmentName, FragmentDefinition),
    FragmentName :: argo_types:name(),
    FragmentDefinition :: argo_graphql_fragment_definition:t(),
    FragmentDefinitionList :: [FragmentDefinition].
fragment_definition_list(
    Context = #context{},
    ExecutableDocument = #argo_graphql_executable_document{},
    TypeConditionSet,
    FragmentDefinitionsIterator1,
    FragmentDefinitionList
) ->
    case maps:next(FragmentDefinitionsIterator1) of
        none ->
            lists:reverse(FragmentDefinitionList);
        {_FragmentName, FragmentDefinition = #argo_graphql_fragment_definition{type_condition = TypeCondition},
            FragmentDefinitionsIterator2} when is_map_key(TypeCondition, TypeConditionSet) ->
            % We still need to match fragment definitions against their "soft" shape.
            % argo_graphql_fragment_definition:get_shape()
            fragment_definition_list(Context, ExecutableDocument, TypeConditionSet, FragmentDefinitionsIterator2, [
                FragmentDefinition | FragmentDefinitionList
            ]);
        {_FragmentName, _FragmentDefinition = #argo_graphql_fragment_definition{}, FragmentDefinitionsIterator2} ->
            fragment_definition_list(
                Context, ExecutableDocument, TypeConditionSet, FragmentDefinitionsIterator2, FragmentDefinitionList
            )
    end.

-spec fragment_name(Context) -> proper_types:type() when Context :: context().
fragment_name(Context = #context{executable_document = VarExecutableDocument}) ->
    #argo_graphql_executable_document{fragment_definitions = FragmentDefinitionsMap} = context_var_get(
        Context, VarExecutableDocument
    ),
    Count = maps:size(FragmentDefinitionsMap) + length(context_var_list(Context, var_fragment_definition)),
    exactly(<<"fragment_", (integer_to_binary(Count))/bytes>>).

-spec fragment_spread(Context, FragmentName) -> proper_types:type() when
    Context :: context(), FragmentName :: argo_types:name().
fragment_spread(Context1 = #context{}, FragmentName) when is_binary(FragmentName) ->
    FragmentSpread1 = argo_graphql_fragment_spread:new(FragmentName),
    FragmentSpreadGen1 = exactly({Context1, FragmentSpread1}),
    FragmentSpreadGen2 = fragment_spread_directives(FragmentSpreadGen1),
    ?LET(
        {
            Context2 = #context{frames = [#frame{stack = [VarParent | _]} | _]},
            FragmentSpread2 = #argo_graphql_fragment_spread{}
        },
        FragmentSpreadGen2,
        begin
            {Context3, VarParent} = context_var_update(Context2, VarParent, fun(Context2_Update1, Parent1) ->
                Parent2 =
                    case Parent1 of
                        #argo_graphql_field{} ->
                            argo_graphql_field:add_selection(Parent1, FragmentSpread2);
                        #argo_graphql_fragment_definition{} ->
                            argo_graphql_fragment_definition:add_selection(Parent1, FragmentSpread2);
                        #argo_graphql_inline_fragment{} ->
                            argo_graphql_inline_fragment:add_selection(Parent1, FragmentSpread2);
                        #argo_graphql_operation_definition{} ->
                            argo_graphql_operation_definition:add_selection(Parent1, FragmentSpread2)
                    end,
                {Context2_Update1, Parent2}
            end),
            {Context3, FragmentSpread2}
        end
    ).

%% @private
-spec fragment_spread_directives(FragmentSpreadGen) -> proper_types:type() when
    FragmentSpreadGen :: proper_types:raw_type().
fragment_spread_directives(FragmentSpreadGen) ->
    ?LET(
        {Context1 = #context{}, FragmentSpread1 = #argo_graphql_fragment_spread{}},
        FragmentSpreadGen,
        add_fragment_spread_directives(Context1, FragmentSpread1)
    ).

-spec inline_fragment(Context, TypeDefinition) -> proper_types:type() when
    Context :: context(), TypeDefinition :: argo_graphql_type_definition:t().
inline_fragment(Context1 = #context{}, TypeDefinition = #argo_graphql_type_definition{}) ->
    InlineFragment1 = argo_graphql_inline_fragment:new(argo_graphql_selection_set:new()),
    {Context2, VarInlineFragment1} = context_stack_push(Context1, InlineFragment1),
    VarInlineFragmentGen1 = exactly({Context2, VarInlineFragment1}),
    VarInlineFragmentGen2 = inline_fragment_directives(VarInlineFragmentGen1),
    VarInlineFragmentGen3 = inline_fragment_type_condition(VarInlineFragmentGen2, TypeDefinition),
    VarInlineFragmentGen4 = inline_fragment_selections(VarInlineFragmentGen3, TypeDefinition),
    ?LET(
        {Context3, VarInlineFragment2},
        VarInlineFragmentGen4,
        begin
            VarInlineFragment1 = VarInlineFragment2,
            {Context4 = #context{}, InlineFragment2 = #argo_graphql_inline_fragment{}} = context_stack_pop(Context3),
            exactly({Context4, InlineFragment2})
        end
    ).

%% @private
-spec inline_fragment_directives(VarInlineFragmentGen) -> proper_types:type() when
    VarInlineFragmentGen :: proper_types:raw_type().
inline_fragment_directives(VarInlineFragmentGen) ->
    ?LET(
        {Context1 = #context{}, VarInlineFragment = #var_inline_fragment{}},
        VarInlineFragmentGen,
        add_inline_fragment_directives(Context1, VarInlineFragment)
    ).

%% @private
-spec inline_fragment_selections(VarInlineFragmentGen, DefaultTypeDefinition) -> proper_types:type() when
    VarInlineFragmentGen :: proper_types:raw_type(), DefaultTypeDefinition :: argo_graphql_type_definition:t().
inline_fragment_selections(
    VarInlineFragmentGen, DefaultTypeDefinition = #argo_graphql_type_definition{name = DefaultTypeName}
) ->
    ?LET(
        {Context1 = #context{service_document = ServiceDocument}, VarInlineFragment = #var_inline_fragment{}},
        VarInlineFragmentGen,
        begin
            #argo_graphql_inline_fragment{type_condition = OptionTypeCondition} = context_var_get(
                Context1, VarInlineFragment
            ),
            TypeDefinition =
                #argo_graphql_type_definition{kind = TypeKind} =
                case OptionTypeCondition of
                    {some, DefaultTypeName} ->
                        DefaultTypeDefinition;
                    {some, TypeCondition} ->
                        argo_graphql_service_document:get_type_definition(ServiceDocument, TypeCondition);
                    none ->
                        DefaultTypeDefinition
                end,
            case TypeKind of
                _ when
                    is_record(TypeKind, argo_graphql_scalar_type_definition) orelse
                        is_record(TypeKind, argo_graphql_enum_type_definition)
                ->
                    throw(oh_noes);
                _ when
                    is_record(TypeKind, argo_graphql_object_type_definition) orelse
                        is_record(TypeKind, argo_graphql_interface_type_definition) orelse
                        is_record(TypeKind, argo_graphql_union_type_definition)
                ->
                    ?LET(
                        Context2 = #context{},
                        selections(Context1, TypeDefinition),
                        exactly({Context2, VarInlineFragment})
                    );
                #argo_graphql_input_object_type_definition{} ->
                    throw(oh_noes)
            end
        end
    ).

%% @private
-spec inline_fragment_type_condition(VarInlineFragmentGen, TypeDefinition) -> proper_types:type() when
    VarInlineFragmentGen :: proper_types:raw_type(), TypeDefinition :: argo_graphql_type_definition:t().
inline_fragment_type_condition(VarInlineFragmentGen, TypeDefinition = #argo_graphql_type_definition{name = TypeName}) ->
    ?LET(
        {Context1 = #context{}, VarInlineFragment = #var_inline_fragment{}},
        VarInlineFragmentGen,
        ?LET(
            TypeCondition,
            type_condition(Context1, TypeDefinition),
            ?LET(
                OptionTypeCondition,
                case TypeCondition of
                    TypeName ->
                        option(exactly(TypeCondition));
                    TypeCondition ->
                        exactly({some, TypeCondition})
                end,
                begin
                    {Context2, VarInlineFragment} = context_var_update(Context1, VarInlineFragment, fun(
                        Context1_Update1, InlineFragment1 = #argo_graphql_inline_fragment{}
                    ) ->
                        InlineFragment2 = argo_graphql_inline_fragment:set_type_condition(
                            InlineFragment1, OptionTypeCondition
                        ),
                        {Context1_Update1, InlineFragment2}
                    end),
                    {Context2, VarInlineFragment}
                end
            )
        )
    ).

-spec operation_definition(ServiceDocument, ExecutableDocument, RootOperationType) -> proper_types:type() when
    ServiceDocument :: argo_graphql_service_document:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    RootOperationType :: argo_graphql_service_document:operation_type().
operation_definition(
    ServiceDocument = #argo_graphql_service_document{},
    ExecutableDocument1 = #argo_graphql_executable_document{operation_definitions = none},
    RootOperationType
) when ?is_operation_type(RootOperationType) ->
    RootOperationTypeDefinition =
        #argo_graphql_type_definition{kind = #argo_graphql_object_type_definition{}} = argo_graphql_service_document:get_root_operation_type_definition(
            ServiceDocument, RootOperationType
        ),
    ?LET(
        OperationDefinition1,
        ?LET(
            {
                OptionName
            },
            {
                option(?LANG:name())
            },
            #argo_graphql_operation_definition{
                operation = RootOperationType,
                name = OptionName,
                variables_definition = argo_graphql_variables_definition:new(),
                directives = argo_graphql_directives:new(),
                selection_set = argo_graphql_selection_set:new(),
                shorthand = false
            }
        ),
        begin
            Context1 = context_build(ServiceDocument, ExecutableDocument1),
            {Context2, _VarOperationDefinition} = context_frame_push(Context1, OperationDefinition1),
            ?LET(
                Context3 = #context{},
                selections(Context2, RootOperationTypeDefinition),
                begin
                    {Context4, OperationDefinition2 = #argo_graphql_operation_definition{}} = context_frame_pop(
                        Context3
                    ),
                    {Context4, OperationDefinition2}
                end
            )
        end
    ).

-spec optional_skip_or_include_directive_definition(Context) -> proper_types:type() when Context :: context().
optional_skip_or_include_directive_definition(Context1 = #context{service_document = ServiceDocument}) ->
    ChanceOfNoDirectives = max(1 bsl complexity(), context_weight(Context1)),
    ?LET(
        DirectiveName,
        frequency([
            {ChanceOfNoDirectives, none},
            {1, <<"include">>},
            {1, <<"skip">>}
        ]),
        case DirectiveName of
            none ->
                exactly({Context1, none});
            _ when is_binary(DirectiveName) ->
                DirectiveDefinition = argo_graphql_service_document:get_directive_definition(
                    ServiceDocument, DirectiveName
                ),
                exactly({Context1, {some, DirectiveDefinition}})
        end
    ).

-spec selection_list() -> proper_types:type().
selection_list() ->
    SelectionGen = oneof([field, fragment_spread, inline_fragment]),
    SingleItemListGen = [SelectionGen],
    MultipleItemListGen = non_empty(list(SelectionGen)),
    ListGen = frequency([
        {1 bsl complexity(), SingleItemListGen},
        {1, MultipleItemListGen}
    ]),
    ?LET(List, proper_types:noshrink(ListGen), non_empty(proper_types:shrink_list(List))).

-spec selections(Context, TypeDefinition) -> proper_types:type() when
    Context :: context(), TypeDefinition :: argo_graphql_type_definition:t().
selections(Context1 = #context{}, TypeDefinition = #argo_graphql_type_definition{}) ->
    complex(
        ?LET(
            SelectionList,
            selection_list(),
            selections(Context1, TypeDefinition, SelectionList)
        )
    ).

%% @private
-spec selections(Context, TypeDefinition, SelectionList) -> proper_types:type() when
    Context :: context(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    SelectionList :: [Selection],
    Selection :: field | fragment_spread | inline_fragment.
selections(Context1 = #context{}, _TypeDefinition = #argo_graphql_type_definition{}, []) ->
    exactly(Context1);
selections(Context1 = #context{}, TypeDefinition = #argo_graphql_type_definition{}, [
    Selection | SelectionList
]) ->
    SelectionGen =
        case Selection of
            field ->
                ?LET(
                    {Context2, FieldName},
                    field_name(Context1, TypeDefinition),
                    ?LET(
                        {Context3, _Field = #argo_graphql_field{}},
                        field(Context2, TypeDefinition, FieldName),
                        Context3
                    )
                );
            fragment_spread ->
                ?LET(
                    {Context2, _FragmentDefinition = #argo_graphql_fragment_definition{name = FragmentName}},
                    fragment_definition(Context1, TypeDefinition),
                    ?LET(
                        {Context3, _FragmentSpread = #argo_graphql_fragment_spread{}},
                        fragment_spread(Context2, FragmentName),
                        Context3
                    )
                );
            inline_fragment ->
                ?LET(
                    {Context2, _InlineFragment = #argo_graphql_inline_fragment{}},
                    inline_fragment(Context1, TypeDefinition),
                    Context2
                )
        end,
    ?LAZY(
        ?LET(
            NextContext = #context{},
            SelectionGen,
            selections(NextContext, TypeDefinition, SelectionList)
        )
    ).

-spec type_condition(Context, TypeDefinition) -> proper_types:type() when
    Context :: context(), TypeDefinition :: argo_graphql_type_definition:t().
type_condition(Context = #context{}, TypeDefinition = #argo_graphql_type_definition{}) ->
    oneof(type_condition_list(Context, TypeDefinition)).

-spec type_condition_list(Context, TypeDefinition) -> TypeConditionList when
    Context :: context(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    TypeConditionList :: [TypeCondition],
    TypeCondition :: argo_types:name().
type_condition_list(
    _Context = #context{service_document = ServiceDocument},
    TypeDefinition = #argo_graphql_type_definition{name = TypeName, kind = TypeKind}
) ->
    case TypeKind of
        #argo_graphql_object_type_definition{} ->
            ImplementsInterfaces = argo_graphql_object_type_definition:get_implements_interfaces(
                TypeKind, ServiceDocument
            ),
            TypeConditionList = ImplementsInterfaces -- [TypeName],
            [TypeName | TypeConditionList];
        #argo_graphql_interface_type_definition{} ->
            Implementations = argo_graphql_type_definition:get_implementations(TypeDefinition, ServiceDocument),
            ImplementsInterfaces = argo_graphql_interface_type_definition:get_implements_interfaces(
                TypeKind, ServiceDocument
            ),
            TypeConditionList = (Implementations ++ ImplementsInterfaces) -- [TypeName],
            [TypeName | TypeConditionList];
        #argo_graphql_union_type_definition{types = UnionMemberTypeSet} ->
            TypeConditionList = [_ | _] = argo_index_set:to_list(UnionMemberTypeSet),
            TypeConditionList
    end.

-spec value(Context, Type) -> proper_types:type() when
    Context :: context(), Type :: argo_graphql_type:t().
value(Context1 = #context{}, Type = #argo_graphql_type{}) ->
    value(Context1, Type, argo_graphql_input_type_graph:new()).

%% @private
-spec value(Context, Type, InputTypeGraph) -> proper_types:type() when
    Context :: context(),
    Type :: argo_graphql_type:t(),
    InputTypeGraph :: argo_graphql_input_type_graph:t().
value(
    Context1 = #context{frames = [#frame{root = VarRoot} | _]},
    Type = #argo_graphql_type{},
    InputTypeGraph1
) ->
    case context_var_get(Context1, VarRoot) of
        #argo_graphql_operation_definition{shorthand = false} ->
            VariableDefinitionGen =
                ?LAZY(
                    ?LET(
                        {Context2, #argo_graphql_variable_definition{variable = VariableName}},
                        complex(variable_definition(Context1, Type)),
                        {Context2, argo_graphql_value:variable(VariableName)}
                    )
                ),
            ChanceOfVariableDefinition = 1 bsl complexity(),
            frequency([
                {ChanceOfVariableDefinition, VariableDefinitionGen},
                {1, complex(value_without_variables(Context1, Type, InputTypeGraph1))}
            ]);
        _ ->
            value_without_variables(Context1, Type, InputTypeGraph1)
    end.

%% @private
-spec value_list_value(Context, Type, InputTypeGraph) -> proper_types:type() when
    Context :: context(),
    Type :: argo_graphql_type:t(),
    InputTypeGraph :: argo_graphql_input_type_graph:t().
value_list_value(Context1 = #context{}, Type = #argo_graphql_type{}, InputTypeGraph1) ->
    ?LET(Size, mostly_small_size(), value_list_value(Size, Context1, Type, InputTypeGraph1)).

%% @private
-spec value_list_value(Size, Context, Type, InputTypeGraph) -> proper_types:type() when
    Size :: non_neg_integer(),
    Context :: context(),
    Type :: argo_graphql_type:t(),
    InputTypeGraph :: argo_graphql_input_type_graph:t().
value_list_value(0, Context1 = #context{}, _Type = #argo_graphql_type{}, _InputTypeGraph1) ->
    exactly({Context1, []});
value_list_value(Size, Context1 = #context{}, Type = #argo_graphql_type{}, InputTypeGraph1) when
    is_integer(Size) andalso Size > 0
->
    ?LET(
        {Context2, Value},
        value(Context1, Type, InputTypeGraph1),
        ?LAZY(
            ?LET(
                {Context3, ListValue},
                value_list_value(Size - 1, Context2, Type, InputTypeGraph1),
                {Context3, [Value | ListValue]}
            )
        )
    ).

%% @private
-spec value_object_value_optional(Context, InputTypeGraph, ObjectValue, InputValueDefinitionList) ->
    proper_types:type()
when
    Context :: context(),
    InputTypeGraph :: argo_graphql_input_type_graph:t(),
    ObjectValue :: argo_graphql_value:object_value(),
    InputValueDefinitionList :: [InputValueDefinition],
    InputValueDefinition :: argo_graphql_input_value_definition:t().
value_object_value_optional(Context1 = #context{}, _InputTypeGraph, ObjectValue, []) ->
    exactly({Context1, ObjectValue});
value_object_value_optional(Context1 = #context{}, InputTypeGraph, ObjectValue1, InputValueDefinitionList) ->
    ?LET(
        List,
        list(oneof(InputValueDefinitionList)),
        value_object_value_required(Context1, InputTypeGraph, ObjectValue1, lists:uniq(List))
    ).

%% @private
-spec value_object_value_required(Context, InputTypeGraph, ObjectValue, InputValueDefinitionList) ->
    proper_types:type()
when
    Context :: context(),
    InputTypeGraph :: argo_graphql_input_type_graph:t(),
    ObjectValue :: argo_graphql_value:object_value(),
    InputValueDefinitionList :: [InputValueDefinition],
    InputValueDefinition :: argo_graphql_input_value_definition:t().
value_object_value_required(Context1 = #context{}, _InputTypeGraph, ObjectValue, []) ->
    exactly({Context1, ObjectValue});
value_object_value_required(Context1 = #context{}, InputTypeGraph, ObjectValue1, [
    InputValueDefinition | InputValueDefinitionList
]) ->
    #argo_graphql_input_value_definition{name = InputName, type = InputType} = InputValueDefinition,
    ?LAZY(
        ?LET(
            {Context2, Value},
            value(Context1, InputType, InputTypeGraph),
            begin
                ObjectValue2 = argo_index_map:put(InputName, Value, ObjectValue1),
                value_object_value_required(
                    Context2, InputTypeGraph, ObjectValue2, InputValueDefinitionList
                )
            end
        )
    ).

%% @private
-spec value_without_variables(Context, Type | TypeDefinition, InputTypeGraph) -> proper_types:type() when
    Context :: context(),
    Type :: argo_graphql_type:t(),
    TypeDefinition :: argo_graphql_type_definition:t(),
    InputTypeGraph :: argo_graphql_input_type_graph:t().
value_without_variables(
    Context1 = #context{service_document = ServiceDocument}, Type = #argo_graphql_type{}, InputTypeGraph1
) ->
    case Type of
        #argo_graphql_type{inner = NamedType} when is_binary(NamedType) ->
            InputTypeDefinition = argo_graphql_service_document:get_input_type_definition(ServiceDocument, NamedType),
            InputTypeGraph2 = argo_graphql_input_type_graph:add_input(InputTypeGraph1, NamedType),
            oneof([
                exactly({Context1, argo_graphql_value:null()}),
                ?LAZY(complex(value_without_variables(Context1, InputTypeDefinition, InputTypeGraph2)))
            ]);
        #argo_graphql_type{inner = #argo_graphql_list_type{type = NestedType}} ->
            oneof([
                exactly({Context1, argo_graphql_value:null()}),
                ?LAZY(
                    ?LET(
                        {Context2, ListValue},
                        complex(value_list_value(Context1, NestedType, InputTypeGraph1)),
                        {Context2, argo_graphql_value:list(ListValue)}
                    )
                )
            ]);
        #argo_graphql_type{inner = #argo_graphql_non_null_type{type = NamedType}} when is_binary(NamedType) ->
            InputTypeDefinition = argo_graphql_service_document:get_input_type_definition(ServiceDocument, NamedType),
            InputTypeGraph2 = argo_graphql_input_type_graph:add_input(InputTypeGraph1, NamedType),
            ?LAZY(complex(value_without_variables(Context1, InputTypeDefinition, InputTypeGraph2)));
        #argo_graphql_type{inner = #argo_graphql_non_null_type{type = #argo_graphql_list_type{type = NestedType}}} ->
            ?LAZY(
                ?LET(
                    {Context2, ListValue},
                    complex(value_list_value(Context1, NestedType, InputTypeGraph1)),
                    {Context2, argo_graphql_value:list(ListValue)}
                )
            )
    end;
value_without_variables(
    Context1 = #context{}, _TypeDefinition = #argo_graphql_type_definition{name = Name, kind = Kind}, InputTypeGraph1
) ->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            case Name of
                <<"Boolean">> ->
                    ?LET(V, boolean(), {Context1, argo_graphql_value:boolean(V)});
                <<"Float">> ->
                    ?LET(V, float(), {Context1, argo_graphql_value:float(V)});
                <<"Int">> ->
                    ?LET(V, integer(), {Context1, argo_graphql_value:int(V)});
                _ ->
                    ?LET(V, ?LANG:block_string_value(), {Context1, argo_graphql_value:string(V)})
            end;
        #argo_graphql_enum_type_definition{values = EnumValueDefinitionMap} ->
            ?LET(V, oneof(argo_index_map:keys(EnumValueDefinitionMap)), {Context1, argo_graphql_value:enum(V)});
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
                {Context2, ObjectValue2},
                value_object_value_required(Context1, InputTypeGraph2, ObjectValue1, ReqInputs),
                ?LET(
                    {Context3, ObjectValue3},
                    value_object_value_optional(Context2, InputTypeGraph2, ObjectValue2, OptInputs),
                    {Context3, argo_graphql_value:object(ObjectValue3)}
                )
            )
    end.

-spec variable_definition(Context, Type) -> proper_types:type() when
    Context :: context(), Type :: argo_graphql_type:t().
variable_definition(
    Context1 = #context{frames = [#frame{root = VarRoot} | _]},
    Type = #argo_graphql_type{}
) ->
    case context_var_get(Context1, VarRoot) of
        #argo_graphql_operation_definition{shorthand = false} ->
            NewVariableDefinitionGen = ?LAZY(
                ?LET(VariableName, variable_name(Context1), variable_definition(Context1, VariableName, Type))
            ),
            case variable_definition_list(Context1, Type) of
                [] ->
                    NewVariableDefinitionGen;
                VariableDefinitionList = [_ | _] ->
                    ChanceOfExistingVariable = max(1 bsl complexity(), context_weight(Context1)),
                    frequency([
                        {ChanceOfExistingVariable,
                            ?LET(VariableDefinition, oneof(VariableDefinitionList), {Context1, VariableDefinition})},
                        {1, NewVariableDefinitionGen}
                    ])
            end;
        _ ->
            error_with_info(badarg, [Context1, Type], #{
                1 => {requires_root_non_shorthand_operation, #{root => VarRoot}}
            })
    end.

-spec variable_definition(Context, VariableName, Type) -> proper_types:type() when
    Context :: context(), VariableName :: argo_types:name(), Type :: argo_graphql_type:t().
variable_definition(
    Context1 = #context{
        service_document = ServiceDocument,
        frames = [#frame{root = VarRoot} | _]
    },
    VariableName,
    Type = #argo_graphql_type{}
) when is_binary(VariableName) ->
    case context_var_get(Context1, VarRoot) of
        #argo_graphql_operation_definition{shorthand = false} ->
            VariableDefinition1 = argo_graphql_variable_definition:new(VariableName, Type),
            ?LET(
                OptionDefaultValue,
                option(proper_argo_graphql_service_document:value_const(ServiceDocument, Type)),
                begin
                    VariableDefinition2 = argo_graphql_variable_definition:set_default_value(
                        VariableDefinition1, OptionDefaultValue
                    ),
                    {Context2, VarRoot} = context_var_update(Context1, VarRoot, fun(
                        Context1_Update1, OperationDefinition1 = #argo_graphql_operation_definition{shorthand = false}
                    ) ->
                        OperationDefinition2 = argo_graphql_operation_definition:add_variable_definition(
                            OperationDefinition1, VariableDefinition2
                        ),
                        {Context1_Update1, OperationDefinition2}
                    end),
                    {Context2, VariableDefinition2}
                end
            );
        _ ->
            error_with_info(badarg, [Context1, VariableName, Type], #{
                1 => {requires_root_non_shorthand_operation, #{root => VarRoot}}
            })
    end.

-spec variable_definition_list(Context, Type) -> VariableDefinitionList when
    Context :: context(),
    Type :: argo_graphql_type:t(),
    VariableDefinitionList :: [VariableDefinition],
    VariableDefinition :: argo_graphql_variable_definition:t().
variable_definition_list(
    Context = #context{
        frames = [#frame{root = VarRoot} | _]
    },
    Type = #argo_graphql_type{}
) ->
    case context_var_get(Context, VarRoot) of
        #argo_graphql_operation_definition{
            shorthand = false,
            variables_definition = #argo_graphql_variables_definition{variables = VariableDefinitionMap}
        } ->
            VariableDefinitionIterator = argo_index_map:iterator(VariableDefinitionMap),
            variable_definition_list(Context, Type, VariableDefinitionIterator);
        _ ->
            error_with_info(badarg, [Context, Type], #{1 => {requires_root_non_shorthand_operation, #{root => VarRoot}}})
    end.

%% @private
-spec variable_definition_list(Context, Type, VariableDefinitionIterator) -> VariableDefinitionList when
    Context :: context(),
    Type :: argo_graphql_type:t(),
    VariableDefinitionIterator :: argo_index_map:iterator(VariableName, VariableDefinition),
    VariableName :: argo_types:name(),
    VariableDefinitionList :: [VariableDefinition],
    VariableDefinition :: argo_graphql_variable_definition:t().
variable_definition_list(Context = #context{}, Type = #argo_graphql_type{}, VariableDefinitionIterator1) ->
    case argo_index_map:next(VariableDefinitionIterator1) of
        none ->
            [];
        {_Index, _VariableName, VariableDefinition = #argo_graphql_variable_definition{type = Type},
            VariableDefinitionIterator2} ->
            [VariableDefinition | variable_definition_list(Context, Type, VariableDefinitionIterator2)];
        {_Index, _VariableName, _VariableDefinition, VariableDefinitionIterator2} ->
            variable_definition_list(Context, Type, VariableDefinitionIterator2)
    end.

-spec variable_name(Context) -> proper_types:type() when Context :: context().
variable_name(
    Context = #context{
        frames = [#frame{root = VarRoot} | _]
    }
) ->
    case context_var_get(Context, VarRoot) of
        #argo_graphql_operation_definition{
            shorthand = false,
            variables_definition = #argo_graphql_variables_definition{variables = VariableDefinitionMap}
        } ->
            case argo_index_map:keys(VariableDefinitionMap) of
                [] ->
                    exactly(<<"var_0">>);
                VariableNameList = [_ | _] ->
                    Suffix1 = lists:max([
                        erlang:binary_to_integer(hd(binary:split(VariableName, <<"var_">>, [trim_all])))
                     || VariableName <- VariableNameList
                    ]),
                    Suffix2 = Suffix1 + 1,
                    VariableName = <<"var_", (erlang:integer_to_binary(Suffix2))/bytes>>,
                    exactly(VariableName)
            end;
        _ ->
            error_with_info(badarg, [Context], #{1 => {requires_root_non_shorthand_operation, #{root => VarRoot}}})
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
format_error_description(_Key, {duplicate_context_var, #{var := Var}}) ->
    io_lib:format("duplicate context var ~0tp", [Var]);
format_error_description(_Key, {requires_root_non_shorthand_operation, #{root := VarRoot}}) ->
    io_lib:format("root must be a non-shorthand OperationDefinition (VarRoot was ~0tp)", [VarRoot]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec add_directive_arguments(Context, Directive, ArgumentsDefinition) -> proper_types:type() when
    Context :: context(),
    Directive :: argo_graphql_directive:t(),
    ArgumentsDefinition :: argo_graphql_arguments_definition:t().
add_directive_arguments(
    Context1 = #context{},
    Directive1 = #argo_graphql_directive{},
    _ArgumentsDefinition = #argo_graphql_arguments_definition{inputs = InputsMap}
) ->
    {ReqInputs, OptInputs} = argo_index_map:foldl(
        fun(_Index, _InputName, InputValueDefinition, {ReqAcc, OptAcc}) ->
            case argo_graphql_input_value_definition:is_required(InputValueDefinition) of
                false ->
                    {ReqAcc, [InputValueDefinition | OptAcc]};
                true ->
                    {[InputValueDefinition | ReqAcc], OptAcc}
            end
        end,
        {[], []},
        InputsMap
    ),
    ?LET(
        {Context2, Directive2},
        add_directive_arguments_required(Context1, Directive1, ReqInputs),
        add_directive_arguments_optional(Context2, Directive2, OptInputs)
    ).

%% @private
-spec add_directive_arguments_optional(Context, Directive, InputValueDefinitionList) -> proper_types:type() when
    Context :: context(),
    Directive :: argo_graphql_directive:t(),
    InputValueDefinitionList :: [InputValueDefinition],
    InputValueDefinition :: argo_graphql_input_value_definition:t().
add_directive_arguments_optional(Context1 = #context{}, Directive1 = #argo_graphql_directive{}, []) ->
    exactly({Context1, Directive1});
add_directive_arguments_optional(
    Context1 = #context{}, Directive1 = #argo_graphql_directive{}, InputValueDefinitionList = [_ | _]
) ->
    ?LET(
        List,
        list(oneof(InputValueDefinitionList)),
        add_directive_arguments_required(Context1, Directive1, lists:uniq(List))
    ).

%% @private
-spec add_directive_arguments_required(Context, Directive, InputValueDefinitionList) -> proper_types:type() when
    Context :: context(),
    Directive :: argo_graphql_directive:t(),
    InputValueDefinitionList :: [InputValueDefinition],
    InputValueDefinition :: argo_graphql_input_value_definition:t().
add_directive_arguments_required(Context1 = #context{}, Directive1 = #argo_graphql_directive{}, []) ->
    exactly({Context1, Directive1});
add_directive_arguments_required(Context1 = #context{}, Directive1 = #argo_graphql_directive{}, [
    InputValueDefinition | InputValueDefinitionList
]) ->
    #argo_graphql_input_value_definition{name = InputName, type = InputType} = InputValueDefinition,
    ?LET(
        {Context2, Value},
        value(Context1, InputType),
        begin
            Argument = argo_graphql_argument:new(InputName, Value),
            Directive2 = argo_graphql_directive:add_argument(Directive1, Argument),
            ?LAZY(add_directive_arguments_required(Context2, Directive2, InputValueDefinitionList))
        end
    ).

%% @private
-spec add_field_arguments(Context, VarField, ArgumentsDefinition) -> proper_types:type() when
    Context :: context(), VarField :: var_field(), ArgumentsDefinition :: argo_graphql_arguments_definition:t().
add_field_arguments(
    Context1 = #context{},
    VarField1 = #var_field{},
    _ArgumentsDefinition = #argo_graphql_arguments_definition{inputs = InputsMap}
) ->
    {ReqInputs, OptInputs} = argo_index_map:foldl(
        fun(_Index, _InputName, InputValueDefinition, {ReqAcc, OptAcc}) ->
            case argo_graphql_input_value_definition:is_required(InputValueDefinition) of
                false ->
                    {ReqAcc, [InputValueDefinition | OptAcc]};
                true ->
                    {[InputValueDefinition | ReqAcc], OptAcc}
            end
        end,
        {[], []},
        InputsMap
    ),
    ?LET(
        {Context2, VarField2},
        add_field_arguments_required(Context1, VarField1, ReqInputs),
        begin
            VarField1 = VarField2,
            add_field_arguments_optional(Context2, VarField2, OptInputs)
        end
    ).

%% @private
-spec add_field_arguments_optional(Context, VarField, InputValueDefinitionList) -> proper_types:type() when
    Context :: context(),
    VarField :: var_field(),
    InputValueDefinitionList :: [InputValueDefinition],
    InputValueDefinition :: argo_graphql_input_value_definition:t().
add_field_arguments_optional(Context1 = #context{}, VarField = #var_field{}, []) ->
    exactly({Context1, VarField});
add_field_arguments_optional(Context1 = #context{}, VarField = #var_field{}, InputValueDefinitionList = [_ | _]) ->
    ?LET(
        List, list(oneof(InputValueDefinitionList)), add_field_arguments_required(Context1, VarField, lists:uniq(List))
    ).

%% @private
-spec add_field_arguments_required(Context, VarField, InputValueDefinitionList) -> proper_types:type() when
    Context :: context(),
    VarField :: var_field(),
    InputValueDefinitionList :: [InputValueDefinition],
    InputValueDefinition :: argo_graphql_input_value_definition:t().
add_field_arguments_required(Context1 = #context{}, VarField = #var_field{}, []) ->
    exactly({Context1, VarField});
add_field_arguments_required(Context1 = #context{}, VarField = #var_field{}, [
    InputValueDefinition | InputValueDefinitionList
]) ->
    #argo_graphql_input_value_definition{name = InputName, type = InputType} = InputValueDefinition,
    ?LET(
        {Context2, Value},
        value(Context1, InputType),
        begin
            Argument = argo_graphql_argument:new(InputName, Value),
            {Context3, VarField} = context_var_update(Context2, VarField, fun(Context2_Update1, Field1) ->
                Field2 = argo_graphql_field:add_argument(Field1, Argument),
                {Context2_Update1, Field2}
            end),
            ?LAZY(add_field_arguments_required(Context3, VarField, InputValueDefinitionList))
        end
    ).

%% @private
-spec add_field_directive(Context, VarField, DirectiveDefinition) -> proper_types:type() when
    Context :: context(),
    VarField :: var_field(),
    DirectiveDefinition :: argo_graphql_directive_definition:t().
add_field_directive(
    Context1 = #context{},
    VarField = #var_field{},
    _DirectiveDefinition = #argo_graphql_directive_definition{name = DirectiveName, arguments = ArgumentsDefinition}
) ->
    Directive1 = argo_graphql_directive:new(DirectiveName),
    ?LET(
        {Context2, Directive2},
        add_directive_arguments(Context1, Directive1, ArgumentsDefinition),
        begin
            {Context3, VarField} = context_var_update(Context2, VarField, fun(Context2_Update1, Field1) ->
                Field2 = argo_graphql_field:add_directive(Field1, Directive2),
                {Context2_Update1, Field2}
            end),
            {Context3, VarField}
        end
    ).

%% @private
-spec add_field_directives(Context, VarField) -> proper_types:type() when
    Context :: context(),
    VarField :: var_field().
add_field_directives(Context1 = #context{}, VarField = #var_field{}) ->
    ?LET(
        {Context2 = #context{}, OptionDirectiveDefinition},
        optional_skip_or_include_directive_definition(Context1),
        case OptionDirectiveDefinition of
            none ->
                exactly({Context2, VarField});
            {some, DirectiveDefinition} ->
                add_field_directive(Context2, VarField, DirectiveDefinition)
        end
    ).

%% @private
-spec add_fragment_definition_directive(Context, VarFragmentDefinition, DirectiveDefinition) -> proper_types:type() when
    Context :: context(),
    VarFragmentDefinition :: var_fragment_definition(),
    DirectiveDefinition :: argo_graphql_directive_definition:t().
add_fragment_definition_directive(
    Context1 = #context{},
    VarFragmentDefinition = #var_fragment_definition{},
    _DirectiveDefinition = #argo_graphql_directive_definition{name = DirectiveName, arguments = ArgumentsDefinition}
) ->
    Directive1 = argo_graphql_directive:new(DirectiveName),
    ?LET(
        {Context2, Directive2},
        add_directive_arguments(Context1, Directive1, ArgumentsDefinition),
        begin
            {Context3, VarFragmentDefinition} = context_var_update(Context2, VarFragmentDefinition, fun(
                Context2_Update1, FragmentDefinition1 = #argo_graphql_fragment_definition{}
            ) ->
                FragmentDefinition2 = argo_graphql_fragment_definition:add_directive(FragmentDefinition1, Directive2),
                {Context2_Update1, FragmentDefinition2}
            end),
            {Context3, VarFragmentDefinition}
        end
    ).

%% @private
-spec add_fragment_definition_directives(Context, VarFragmentDefinition) -> proper_types:type() when
    Context :: context(),
    VarFragmentDefinition :: var_fragment_definition().
add_fragment_definition_directives(Context1 = #context{}, VarFragmentDefinition = #var_fragment_definition{}) ->
    ?LET(
        {Context2 = #context{}, OptionDirectiveDefinition},
        exactly({Context1, none}),
        case OptionDirectiveDefinition of
            none ->
                exactly({Context2, VarFragmentDefinition});
            {some, DirectiveDefinition} ->
                add_fragment_definition_directive(Context2, VarFragmentDefinition, DirectiveDefinition)
        end
    ).

%% @private
-spec add_fragment_spread_directive(Context, FragmentSpread, DirectiveDefinition) -> proper_types:type() when
    Context :: context(),
    FragmentSpread :: argo_graphql_fragment_spread:t(),
    DirectiveDefinition :: argo_graphql_directive_definition:t().
add_fragment_spread_directive(
    Context1 = #context{},
    FragmentSpread1 = #argo_graphql_fragment_spread{},
    _DirectiveDefinition = #argo_graphql_directive_definition{name = DirectiveName, arguments = ArgumentsDefinition}
) ->
    Directive1 = argo_graphql_directive:new(DirectiveName),
    ?LET(
        {Context2, Directive2},
        add_directive_arguments(Context1, Directive1, ArgumentsDefinition),
        begin
            FragmentSpread2 = argo_graphql_fragment_spread:add_directive(FragmentSpread1, Directive2),
            {Context2, FragmentSpread2}
        end
    ).

%% @private
-spec add_fragment_spread_directives(Context, FragmentSpread) -> proper_types:type() when
    Context :: context(),
    FragmentSpread :: argo_graphql_fragment_spread:t().
add_fragment_spread_directives(Context1 = #context{}, FragmentSpread1 = #argo_graphql_fragment_spread{}) ->
    ?LET(
        {Context2 = #context{}, OptionDirectiveDefinition},
        optional_skip_or_include_directive_definition(Context1),
        case OptionDirectiveDefinition of
            none ->
                exactly({Context2, FragmentSpread1});
            {some, DirectiveDefinition} ->
                add_fragment_spread_directive(Context2, FragmentSpread1, DirectiveDefinition)
        end
    ).

%% @private
-spec add_inline_fragment_directive(Context, VarInlineFragment, DirectiveDefinition) -> proper_types:type() when
    Context :: context(),
    VarInlineFragment :: var_inline_fragment(),
    DirectiveDefinition :: argo_graphql_directive_definition:t().
add_inline_fragment_directive(
    Context1 = #context{},
    VarInlineFragment = #var_inline_fragment{},
    _DirectiveDefinition = #argo_graphql_directive_definition{name = DirectiveName, arguments = ArgumentsDefinition}
) ->
    Directive1 = argo_graphql_directive:new(DirectiveName),
    ?LET(
        {Context2, Directive2},
        add_directive_arguments(Context1, Directive1, ArgumentsDefinition),
        begin
            {Context3, VarInlineFragment} = context_var_update(Context2, VarInlineFragment, fun(
                Context2_Update1, InlineFragment1 = #argo_graphql_inline_fragment{}
            ) ->
                InlineFragment2 = argo_graphql_inline_fragment:add_directive(InlineFragment1, Directive2),
                {Context2_Update1, InlineFragment2}
            end),
            {Context3, VarInlineFragment}
        end
    ).

%% @private
-spec add_inline_fragment_directives(Context, VarInlineFragment) -> proper_types:type() when
    Context :: context(),
    VarInlineFragment :: var_inline_fragment().
add_inline_fragment_directives(Context1 = #context{}, VarInlineFragment = #var_inline_fragment{}) ->
    ?LET(
        {Context2 = #context{}, OptionDirectiveDefinition},
        optional_skip_or_include_directive_definition(Context1),
        case OptionDirectiveDefinition of
            none ->
                exactly({Context2, VarInlineFragment});
            {some, DirectiveDefinition} ->
                add_inline_fragment_directive(Context2, VarInlineFragment, DirectiveDefinition)
        end
    ).
