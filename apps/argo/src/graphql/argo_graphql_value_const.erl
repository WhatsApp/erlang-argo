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
-module(argo_graphql_value_const).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql.hrl").
-include_lib("argo/include/argo_index_map.hrl").

%% New API
-export([
    from_language/1,
    null/0,
    float/1,
    int/1,
    string/1,
    boolean/1,
    enum/1,
    list/1,
    object/1
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type list_value_const() :: [argo_graphql_value_const:t()].
-type object_value_const() :: argo_index_map:t(argo_types:name(), argo_graphql_value_const:t()).
-type inner() ::
    null
    | {float, float()}
    | {int, integer()}
    | {string, unicode:unicode_binary()}
    | {boolean, boolean()}
    | {enum, argo_types:name()}
    | {list, list_value_const()}
    | {object, object_value_const()}.
-type t() :: #argo_graphql_value_const{}.

-export_type([
    list_value_const/0,
    object_value_const/0,
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageValue) -> Value when LanguageValue :: argo_graphql_language_value_const:t(), Value :: t().
from_language(#argo_graphql_language_value_const{inner = Inner}) ->
    case Inner of
        null ->
            ?MODULE:null();
        {float, Float} ->
            ?MODULE:float(Float);
        {int, Int} ->
            ?MODULE:int(Int);
        {string, StringValue} ->
            ?MODULE:string(StringValue);
        {boolean, BooleanValue} ->
            ?MODULE:boolean(BooleanValue);
        {enum, #argo_graphql_language_enum_value{name = EnumValue}} ->
            ?MODULE:enum(EnumValue);
        {list, #argo_graphql_language_list_value_const{list = LanguageListValueConst}} ->
            ListValueConst = [
                from_language(LanguageListValueConstItem)
             || LanguageListValueConstItem <- LanguageListValueConst
            ],
            ?MODULE:list(ListValueConst);
        {object, #argo_graphql_language_object_value_const{fields = LanguageObjectValueConst}} ->
            ObjectValueConst1 = argo_index_map:new(),
            ObjectValueConst2 = lists:foldl(
                fun(
                    #argo_graphql_language_object_field_const{name = Name, value = LanguageValueConst},
                    ObjectValueConst1Acc1
                ) ->
                    Value = from_language(LanguageValueConst),
                    ObjectValueConst1Acc2 = argo_index_map:put(Name, Value, ObjectValueConst1Acc1),
                    ObjectValueConst1Acc2
                end,
                ObjectValueConst1,
                LanguageObjectValueConst
            ),
            ?MODULE:object(ObjectValueConst2)
    end.

-compile({inline, [null/0]}).
-spec null() -> ValueConst when ValueConst :: t().
null() ->
    #argo_graphql_value_const{inner = null}.

-compile({inline, [float/1]}).
-spec float(Float) -> ValueConst when Float :: float(), ValueConst :: t().
float(Float) when is_float(Float) ->
    #argo_graphql_value_const{inner = {float, Float}}.

-compile({inline, [int/1]}).
-spec int(Int) -> ValueConst when Int :: integer(), ValueConst :: t().
int(Int) when is_integer(Int) ->
    #argo_graphql_value_const{inner = {int, Int}}.

-compile({inline, [string/1]}).
-spec string(StringValue) -> ValueConst when StringValue :: unicode:unicode_binary(), ValueConst :: t().
string(StringValue) when is_binary(StringValue) ->
    #argo_graphql_value_const{inner = {string, StringValue}}.

-compile({inline, [boolean/1]}).
-spec boolean(Boolean) -> ValueConst when Boolean :: boolean(), ValueConst :: t().
boolean(Boolean) when is_boolean(Boolean) ->
    #argo_graphql_value_const{inner = {boolean, Boolean}}.

-compile({inline, [enum/1]}).
-spec enum(EnumValue) -> ValueConst when EnumValue :: argo_types:name(), ValueConst :: t().
enum(EnumValue) when is_binary(EnumValue) ->
    #argo_graphql_value_const{inner = {enum, EnumValue}}.

-compile({inline, [list/1]}).
-spec list(ListValueConst) -> ValueConst when ListValueConst :: list_value_const(), ValueConst :: t().
list(ListValueConst) when is_list(ListValueConst) ->
    #argo_graphql_value_const{inner = {list, ListValueConst}}.

-compile({inline, [object/1]}).
-spec object(ObjectValueConst) -> ValueConst when ObjectValueConst :: object_value_const(), ValueConst :: t().
object(ObjectValueConst = #argo_index_map{}) ->
    #argo_graphql_value_const{inner = {object, ObjectValueConst}}.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_value_const{inner = Inner}) ->
    case Inner of
        null ->
            argo_graphql_formatter:write(Formatter1, "null", []);
        {float, Float} ->
            argo_graphql_formatter:write(Formatter1, "~ts", [erlang:float_to_binary(Float, [short])]);
        {int, Int} ->
            argo_graphql_formatter:write(Formatter1, "~w", [Int]);
        {string, StringValue} ->
            argo_graphql_language_string_value:format(Formatter1, StringValue);
        {boolean, Boolean} ->
            argo_graphql_formatter:write(Formatter1, "~ts", [Boolean]);
        {enum, EnumValue} ->
            argo_graphql_formatter:write(Formatter1, "~ts", [EnumValue]);
        {list, ListValueConst} ->
            list_value_const_format(Formatter1, ListValueConst);
        {object, ObjectValueConst} ->
            object_value_const_format(Formatter1, ObjectValueConst)
    end.

%% @private
-spec list_value_const_format(Formatter1, Type :: [t()]) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
list_value_const_format(Formatter1, []) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "[]", []),
    Formatter2;
list_value_const_format(Formatter1, List) when is_list(List) ->
    Formatter2 = argo_graphql_formatter:write(Formatter1, "[", []),
    Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
    {_Index, Formatter4} = lists:foldl(
        fun(ValueConst, {Index, Formatter3_Acc1}) ->
            Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
            Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
            Formatter3_Acc4 = argo_graphql_value_const:format(Formatter3_Acc3, ValueConst),
            Formatter3_Acc5 =
                case Index =:= length(List) of
                    false ->
                        argo_graphql_formatter:write(Formatter3_Acc4, ",", []);
                    true ->
                        Formatter3_Acc4
                end,
            {Index + 1, Formatter3_Acc5}
        end,
        {1, Formatter3},
        List
    ),
    Formatter5 = argo_graphql_formatter:write(Formatter4, "~n", []),
    Formatter6 = argo_graphql_formatter:shift_left(Formatter5),
    Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "]", []),
    Formatter8.

%% @private
-spec object_value_const_format(Formatter1, Type :: argo_index_map:t(argo_types:name(), t())) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
object_value_const_format(Formatter1, Fields = #argo_index_map{}) ->
    case argo_index_map:size(Fields) of
        0 ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, "{}", []),
            Formatter2;
        Length ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, "{", []),
            Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
            Formatter4 = argo_index_map:foldl(
                fun(Index, Name, ValueConst, Formatter3_Acc1) ->
                    Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
                    Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
                    Formatter3_Acc4 = argo_graphql_formatter:write(Formatter3_Acc3, "~ts: ", [Name]),
                    Formatter3_Acc5 = argo_graphql_value_const:format(Formatter3_Acc4, ValueConst),
                    Formatter3_Acc6 =
                        case (Index + 1) =:= Length of
                            false ->
                                argo_graphql_formatter:write(Formatter3_Acc5, ",", []);
                            true ->
                                Formatter3_Acc5
                        end,
                    Formatter3_Acc6
                end,
                Formatter3,
                Fields
            ),
            Formatter5 = argo_graphql_formatter:write(Formatter4, "~n", []),
            Formatter6 = argo_graphql_formatter:shift_left(Formatter5),
            Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
            Formatter8 = argo_graphql_formatter:write(Formatter7, "}", []),
            Formatter8
    end.
