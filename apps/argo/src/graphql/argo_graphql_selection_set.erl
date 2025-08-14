%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_selection_set).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
    add_field/2,
    add_fragment_spread/2,
    add_inline_fragment/2,
    add_selection/2,
    find_field/3,
    fold_fields/4,
    get_shape/2
]).
%% argo_graphql_display callbacks
-export([
    format/2,
    format_shorthand/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type fold_fields_action() :: 'cont' | 'halt'.
-type fold_fields_result(AccOut) :: fold_fields_action() | {fold_fields_action(), AccOut}.
-type fold_fields_func(Acc) :: fold_fields_func(Acc, Acc).
-type fold_fields_func(AccIn, AccOut) :: fun(
    (argo_graphql_field:t(), argo_graphql_executable_document:t(), AccIn) -> fold_fields_result(AccOut)
).
-type selection() ::
    argo_graphql_field:t()
    | argo_graphql_fragment_spread:t()
    | argo_graphql_inline_fragment:t().
-type shape() :: #{argo_types:name() => {argo_types:name(), shape()}}.
-type t() :: #argo_graphql_selection_set{}.

-export_type([
    fold_fields_action/0,
    fold_fields_result/1,
    fold_fields_func/1,
    fold_fields_func/2,
    selection/0,
    shape/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageSelectionSet) -> SelectionSet when
    LanguageSelectionSet :: argo_graphql_language_selection_set:t(), SelectionSet :: t().
from_language(#argo_graphql_language_selection_set{selections = LanguageSelectionList}) ->
    SelectionSet1 = new(),
    SelectionSet2 = lists:foldl(
        fun(LanguageSelection, SelectionSet1_Acc1) ->
            case LanguageSelection of
                #argo_graphql_language_selection{inner = LanguageField = #argo_graphql_language_field{}} ->
                    Field = argo_graphql_field:from_language(LanguageField),
                    add_field(SelectionSet1_Acc1, Field);
                #argo_graphql_language_selection{
                    inner = LanguageFragmentSpread = #argo_graphql_language_fragment_spread{}
                } ->
                    FragmentSpread = argo_graphql_fragment_spread:from_language(LanguageFragmentSpread),
                    add_fragment_spread(SelectionSet1_Acc1, FragmentSpread);
                #argo_graphql_language_selection{
                    inner = LanguageInlineFragment = #argo_graphql_language_inline_fragment{}
                } ->
                    InlineFragment = argo_graphql_inline_fragment:from_language(LanguageInlineFragment),
                    add_inline_fragment(SelectionSet1_Acc1, InlineFragment)
            end
        end,
        SelectionSet1,
        LanguageSelectionList
    ),
    SelectionSet2.

-compile({inline, [new/0]}).
-spec new() -> SelectionSet when SelectionSet :: t().
new() ->
    #argo_graphql_selection_set{selections = []}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-compile({inline, [add_field/2]}).
-spec add_field(SelectionSet, Field) -> SelectionSet when
    SelectionSet :: t(), Field :: argo_graphql_field:t().
add_field(
    SelectionSet1 = #argo_graphql_selection_set{selections = Selections1},
    Field = #argo_graphql_field{}
) ->
    Selections2 = Selections1 ++ [Field],
    SelectionSet2 = SelectionSet1#argo_graphql_selection_set{selections = Selections2},
    SelectionSet2.

-compile({inline, [add_fragment_spread/2]}).
-spec add_fragment_spread(SelectionSet, FragmentSpread) -> SelectionSet when
    SelectionSet :: t(), FragmentSpread :: argo_graphql_fragment_spread:t().
add_fragment_spread(
    SelectionSet1 = #argo_graphql_selection_set{selections = Selections1},
    FragmentSpread = #argo_graphql_fragment_spread{}
) ->
    Selections2 = Selections1 ++ [FragmentSpread],
    SelectionSet2 = SelectionSet1#argo_graphql_selection_set{selections = Selections2},
    SelectionSet2.

-compile({inline, [add_inline_fragment/2]}).
-spec add_inline_fragment(SelectionSet, InlineFragment) -> SelectionSet when
    SelectionSet :: t(), InlineFragment :: argo_graphql_inline_fragment:t().
add_inline_fragment(
    SelectionSet1 = #argo_graphql_selection_set{selections = Selections1},
    InlineFragment = #argo_graphql_inline_fragment{}
) ->
    Selections2 = Selections1 ++ [InlineFragment],
    SelectionSet2 = SelectionSet1#argo_graphql_selection_set{selections = Selections2},
    SelectionSet2.

-spec add_selection(SelectionSet, Selection) -> SelectionSet when
    SelectionSet :: t(), Selection :: selection().
add_selection(SelectionSet1 = #argo_graphql_selection_set{}, Field = #argo_graphql_field{}) ->
    add_field(SelectionSet1, Field);
add_selection(SelectionSet1 = #argo_graphql_selection_set{}, FragmentSpread = #argo_graphql_fragment_spread{}) ->
    add_fragment_spread(SelectionSet1, FragmentSpread);
add_selection(SelectionSet1 = #argo_graphql_selection_set{}, InlineFragment = #argo_graphql_inline_fragment{}) ->
    add_inline_fragment(SelectionSet1, InlineFragment).

-spec find_field(SelectionSet, FieldAliasOrName, ExecutableDocument) -> {ok, Field} | error when
    SelectionSet :: t(),
    FieldAliasOrName :: argo_types:name(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Field :: argo_graphql_field:t().
find_field(
    SelectionSet = #argo_graphql_selection_set{},
    FieldAliasOrName,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_binary(FieldAliasOrName) ->
    case fold_fields(SelectionSet, FieldAliasOrName, fun find_field_fold/3, ExecutableDocument) of
        FieldAliasOrName ->
            error;
        Field = #argo_graphql_field{} ->
            {ok, Field}
    end.

%% @private
-spec find_field_fold(Field, ExecutableDocument, FieldAliasOrName) -> {halt, Field} | cont when
    Field :: argo_graphql_field:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    FieldAliasOrName :: argo_types:name().
find_field_fold(
    Field = #argo_graphql_field{}, _ExecutableDocument = #argo_graphql_executable_document{}, FieldAliasOrName
) when is_binary(FieldAliasOrName) ->
    case Field of
        #argo_graphql_field{'alias' = {some, FieldAliasOrName}} ->
            {halt, Field};
        #argo_graphql_field{'alias' = none, name = FieldAliasOrName} ->
            {halt, Field};
        #argo_graphql_field{} ->
            cont
    end.

-spec fold_fields(SelectionSet, AccIn, Fun, ExecutableDocument) -> AccOut when
    SelectionSet :: t(),
    AccIn :: dynamic(),
    Fun :: fold_fields_func(AccIn, AccOut),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    AccOut :: dynamic().
fold_fields(
    SelectionSet = #argo_graphql_selection_set{}, AccIn, Fun, ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    case fold_fields_internal(SelectionSet, AccIn, Fun, ExecutableDocument) of
        {cont, AccOut} ->
            AccOut;
        {halt, AccOut} ->
            AccOut
    end.

%% @private
-spec fold_fields_internal(SelectionSet | FragmentDefinition | InlineFragment, AccIn, Fun, ExecutableDocument) ->
    {Action, AccOut}
when
    SelectionSet :: t(),
    FragmentDefinition :: argo_graphql_fragment_definition:t(),
    InlineFragment :: argo_graphql_inline_fragment:t(),
    AccIn :: dynamic(),
    Fun :: fold_fields_func(AccIn, AccOut),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Action :: fold_fields_action(),
    AccOut :: dynamic().
fold_fields_internal(
    #argo_graphql_selection_set{selections = SelectionList},
    AccIn,
    Fun,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    fold_fields_internal_selection_list(SelectionList, AccIn, Fun, ExecutableDocument);
fold_fields_internal(
    #argo_graphql_fragment_definition{selection_set = SelectionSet},
    AccIn,
    Fun,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    fold_fields_internal(SelectionSet, AccIn, Fun, ExecutableDocument);
fold_fields_internal(
    #argo_graphql_inline_fragment{selection_set = SelectionSet},
    AccIn,
    Fun,
    ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    fold_fields_internal(SelectionSet, AccIn, Fun, ExecutableDocument).

%% @private
-spec fold_fields_internal_normalize(AccIn, Result) -> {Action, AccOut} when
    AccIn :: dynamic(),
    Result :: fold_fields_result(AccOut),
    Action :: fold_fields_action(),
    AccOut :: dynamic().
fold_fields_internal_normalize(AccIn, Action) when Action =:= 'cont' orelse Action =:= 'halt' ->
    {Action, AccIn};
fold_fields_internal_normalize(_AccIn, {Action, AccOut}) when Action =:= 'cont' orelse Action =:= 'halt' ->
    {Action, AccOut}.

%% @private
-spec fold_fields_internal_selection_list(SelectionList, AccIn, Fun, ExecutableDocument) -> {Action, AccOut} when
    SelectionList :: [Selection],
    Selection :: selection(),
    AccIn :: dynamic(),
    Fun :: fold_fields_func(AccIn, AccOut),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Action :: fold_fields_action(),
    AccOut :: dynamic().
fold_fields_internal_selection_list(
    [Selection | SelectionList], AccIn, Fun, ExecutableDocument = #argo_graphql_executable_document{}
) when is_function(Fun, 3) ->
    case Selection of
        Field = #argo_graphql_field{} ->
            case fold_fields_internal_normalize(AccIn, Fun(Field, ExecutableDocument, AccIn)) of
                {cont, AccOut} ->
                    fold_fields_internal_selection_list(SelectionList, AccOut, Fun, ExecutableDocument);
                {halt, AccOut} ->
                    {halt, AccOut}
            end;
        #argo_graphql_fragment_spread{name = FragmentName} ->
            FragmentDefinition = argo_graphql_executable_document:get_fragment_definition(
                ExecutableDocument, FragmentName
            ),
            case fold_fields_internal(FragmentDefinition, AccIn, Fun, ExecutableDocument) of
                {cont, AccOut} ->
                    fold_fields_internal_selection_list(SelectionList, AccOut, Fun, ExecutableDocument);
                {halt, AccOut} ->
                    {halt, AccOut}
            end;
        InlineFragment = #argo_graphql_inline_fragment{} ->
            case fold_fields_internal(InlineFragment, AccIn, Fun, ExecutableDocument) of
                {cont, AccOut} ->
                    fold_fields_internal_selection_list(SelectionList, AccOut, Fun, ExecutableDocument);
                {halt, AccOut} ->
                    {halt, AccOut}
            end
    end;
fold_fields_internal_selection_list([], AccOut, Fun, _ExecutableDocument = #argo_graphql_executable_document{}) when
    is_function(Fun, 3)
->
    {cont, AccOut}.

-spec get_shape(SelectionSet, ExecutableDocument) -> Shape when
    SelectionSet :: t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Shape :: shape().
get_shape(SelectionSet = #argo_graphql_selection_set{}, ExecutableDocument = #argo_graphql_executable_document{}) ->
    fold_fields(SelectionSet, maps:new(), fun get_shape_fold/3, ExecutableDocument).

%% @private
-spec get_shape_fold(Field, ExecutableDocument, Shape) -> {cont, Shape} when
    Field :: argo_graphql_field:t(),
    ExecutableDocument :: argo_graphql_executable_document:t(),
    Shape :: shape().
get_shape_fold(
    Field = #argo_graphql_field{'alias' = OptionFieldAlias, name = FieldName},
    ExecutableDocument = #argo_graphql_executable_document{},
    Shape1
) when is_map(Shape1) ->
    FieldAliasOrName =
        case OptionFieldAlias of
            {some, FieldAlias} ->
                FieldAlias;
            none ->
                FieldName
        end,
    FieldShape = {FieldName, argo_graphql_field:get_shape(Field, ExecutableDocument)},
    case maps:find(FieldAliasOrName, Shape1) of
        {ok, FieldShape} ->
            {cont, Shape1};
        {ok, ExistingFieldShape} ->
            error_with_info(badarg, [Field, ExecutableDocument, Shape1], #{
                1 =>
                    {shape_mismatch, #{
                        field_name_or_alias => FieldAliasOrName,
                        field_shape => FieldShape,
                        existing_field_shape => ExistingFieldShape
                    }}
            });
        error ->
            Shape2 = Shape1#{FieldAliasOrName => FieldShape},
            {cont, Shape2}
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_selection_set{selections = []}) ->
    Formatter1;
format(Formatter1, SelectionSet = #argo_graphql_selection_set{selections = [_ | _]}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, " ", []),
    Formatter3 = format_shorthand(Formatter2, SelectionSet),
    Formatter3.

-spec format_shorthand(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format_shorthand(Formatter1, #argo_graphql_selection_set{selections = SelectionList}) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "{", []),
    Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
    Formatter4 = lists:foldl(
        fun(Selection, Formatter3_Acc1) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
            Formatter3_Acc4 =
                case Selection of
                    #argo_graphql_field{} ->
                        argo_graphql_field:format(Formatter3_Acc3, Selection);
                    #argo_graphql_fragment_spread{} ->
                        argo_graphql_fragment_spread:format(Formatter3_Acc3, Selection);
                    #argo_graphql_inline_fragment{} ->
                        argo_graphql_inline_fragment:format(Formatter3_Acc3, Selection)
                end,
            Formatter3_Acc4
        end,
        Formatter3,
        SelectionList
    ),
    Formatter5 = argo_graphql_formatter:shift_left(Formatter4),
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "}", []),
    Formatter8.

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
format_error_description(
    _Key,
    {shape_mismatch, #{
        field_name_or_alias := FieldAliasOrName,
        field_shape := FieldShape,
        existing_field_shape := ExistingFieldShape
    }}
) ->
    io_lib:format(
        "shape mismatch for SelectionSet on ~0tp (existing shape ~0tp does not match expected shape ~0tp)",
        [FieldAliasOrName, ExistingFieldShape, FieldShape]
    );
format_error_description(_Key, Value) ->
    Value.
