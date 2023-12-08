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
-module(argo_graphql_language_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_graphql_language.hrl").

%% New API
-export([
    variable/2,
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
    {variable, argo_graphql_language_variable:t()}
    | null
    | {float, float()}
    | {int, integer()}
    | {string, argo_graphql_language_string_value:t()}
    | {boolean, boolean()}
    | {enum, argo_graphql_language_enum_value:t()}
    | {list, argo_graphql_language_list_value:t()}
    | {object, argo_graphql_language_object_value:t()}.
-type t() :: #argo_graphql_language_value{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-compile({inline, [variable/2]}).
-spec variable(Variable, Location) -> Value when
    Variable :: argo_graphql_language_variable:t(), Location :: erl_anno:location(), Value :: t().
variable(Variable = #argo_graphql_language_variable{}, Location) ->
    #argo_graphql_language_value{location = Location, inner = {variable, Variable}}.

-compile({inline, [null/1]}).
-spec null(Location) -> Value when Location :: erl_anno:location(), Value :: t().
null(Location) ->
    #argo_graphql_language_value{location = Location, inner = null}.

-compile({inline, [float/2]}).
-spec float(Float, Location) -> Value when Float :: float(), Location :: erl_anno:location(), Value :: t().
float(Float, Location) when is_float(Float) ->
    #argo_graphql_language_value{location = Location, inner = {float, Float}}.

-compile({inline, [int/2]}).
-spec int(Int, Location) -> Value when Int :: integer(), Location :: erl_anno:location(), Value :: t().
int(Int, Location) when is_integer(Int) ->
    #argo_graphql_language_value{location = Location, inner = {int, Int}}.

-compile({inline, [string/2]}).
-spec string(String, Location) -> Value when String :: string(), Location :: erl_anno:location(), Value :: t().
string(String, Location) when is_binary(String) ->
    UnescapedString = argo_graphql_language_string_value:string_value_unescape(String),
    #argo_graphql_language_value{location = Location, inner = {string, UnescapedString}}.

-compile({inline, [boolean/2]}).
-spec boolean(Boolean, Location) -> Value when Boolean :: boolean(), Location :: erl_anno:location(), Value :: t().
boolean(Boolean, Location) when is_boolean(Boolean) ->
    #argo_graphql_language_value{location = Location, inner = {boolean, Boolean}}.

-compile({inline, [enum/2]}).
-spec enum(EnumValue, Location) -> Value when
    EnumValue :: argo_graphql_language_enum_value:t(), Location :: erl_anno:location(), Value :: t().
enum(EnumValue = #argo_graphql_language_enum_value{}, Location) ->
    #argo_graphql_language_value{location = Location, inner = {enum, EnumValue}}.

-compile({inline, [list/2]}).
-spec list(ListValue, Location) -> Value when
    ListValue :: argo_graphql_language_list_value:t(), Location :: erl_anno:location(), Value :: t().
list(ListValue = #argo_graphql_language_list_value{}, Location) ->
    #argo_graphql_language_value{location = Location, inner = {list, ListValue}}.

-compile({inline, [object/2]}).
-spec object(ObjectValue, Location) -> Value when
    ObjectValue :: argo_graphql_language_object_value:t(), Location :: erl_anno:location(), Value :: t().
object(ObjectValue = #argo_graphql_language_object_value{}, Location) ->
    #argo_graphql_language_value{location = Location, inner = {object, ObjectValue}}.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_language_value{inner = Inner}) ->
    case Inner of
        {variable, Variable} ->
            argo_graphql_language_variable:format(Formatter1, Variable);
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
        {list, ListValue} ->
            argo_graphql_language_list_value:format(Formatter1, ListValue);
        {object, ObjectValue} ->
            argo_graphql_language_object_value:format(Formatter1, ObjectValue)
    end.
