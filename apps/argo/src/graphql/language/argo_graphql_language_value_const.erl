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
-module(argo_graphql_language_value_const).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    null/1,
    float/2,
    int/2,
    string/2,
    boolean/2,
    enum/2,
    list/2,
    object/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type inner() ::
    null
    | {float, float()}
    | {int, integer()}
    | {string, argo_graphql_language_string_value:t()}
    | {boolean, boolean()}
    | {enum, argo_graphql_language_enum_value:t()}
    | {list, argo_graphql_language_list_value_const:t()}
    | {object, argo_graphql_language_object_value_const:t()}.
-type t() :: #argo_graphql_language_value_const{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [null/1]}).
-spec null(Location) -> ConstValue when Location :: erl_anno:location(), ConstValue :: t().
null(Location) ->
    #argo_graphql_language_value_const{location = Location, inner = null}.

-compile({inline, [float/2]}).
-spec float(Float, Location) -> ConstValue when Float :: float(), Location :: erl_anno:location(), ConstValue :: t().
float(Float, Location) when is_float(Float) ->
    #argo_graphql_language_value_const{location = Location, inner = {float, Float}}.

-compile({inline, [int/2]}).
-spec int(Int, Location) -> ConstValue when Int :: integer(), Location :: erl_anno:location(), ConstValue :: t().
int(Int, Location) when is_integer(Int) ->
    #argo_graphql_language_value_const{location = Location, inner = {int, Int}}.

-compile({inline, [string/2]}).
-spec string(String, Location) -> ConstValue when
    String :: string(), Location :: erl_anno:location(), ConstValue :: t().
string(String, Location) when is_binary(String) ->
    UnescapedString = argo_graphql_language_string_value:string_value_unescape(String),
    #argo_graphql_language_value_const{location = Location, inner = {string, UnescapedString}}.

-compile({inline, [boolean/2]}).
-spec boolean(Boolean, Location) -> ConstValue when
    Boolean :: boolean(), Location :: erl_anno:location(), ConstValue :: t().
boolean(Boolean, Location) when is_boolean(Boolean) ->
    #argo_graphql_language_value_const{location = Location, inner = {boolean, Boolean}}.

-compile({inline, [enum/2]}).
-spec enum(EnumValue, Location) -> ConstValue when
    EnumValue :: argo_graphql_language_enum_value:t(), Location :: erl_anno:location(), ConstValue :: t().
enum(EnumValue = #argo_graphql_language_enum_value{}, Location) ->
    #argo_graphql_language_value_const{location = Location, inner = {enum, EnumValue}}.

-compile({inline, [list/2]}).
-spec list(ListValueConst, Location) -> ConstValue when
    ListValueConst :: argo_graphql_language_list_value_const:t(), Location :: erl_anno:location(), ConstValue :: t().
list(ListValueConst = #argo_graphql_language_list_value_const{}, Location) ->
    #argo_graphql_language_value_const{location = Location, inner = {list, ListValueConst}}.

-compile({inline, [object/2]}).
-spec object(ObjectValueConst, Location) -> ConstValue when
    ObjectValueConst :: argo_graphql_language_object_value_const:t(),
    Location :: erl_anno:location(),
    ConstValue :: t().
object(ObjectValueConst = #argo_graphql_language_object_value_const{}, Location) ->
    #argo_graphql_language_value_const{location = Location, inner = {object, ObjectValueConst}}.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_value_const{inner = Inner}) ->
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
            argo_graphql_language_enum_value:format(Formatter1, EnumValue);
        {list, ListValueConst} ->
            argo_graphql_language_list_value_const:format(Formatter1, ListValueConst);
        {object, ObjectValueConst} ->
            argo_graphql_language_object_value_const:format(Formatter1, ObjectValueConst)
    end.
