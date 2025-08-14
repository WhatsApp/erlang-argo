%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_input_object_type_definition).
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
    add_input_value_definition/2,
    get_shape/1,
    get_shape/2,
    is_ambiguous/1
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
-type shape() :: #{
    argo_types:name() => argo_graphql_type:t(),
    type := input_object
}.
-type t() :: #argo_graphql_input_object_type_definition{}.

-export_type([
    shape/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_language(LanguageInputObjectTypeDefinition) -> InputObjectTypeDefinition when
    LanguageInputObjectTypeDefinition :: argo_graphql_language_input_object_type_definition:t(),
    InputObjectTypeDefinition :: t().
from_language(#argo_graphql_language_input_object_type_definition{fields = LanguageOptionFields}) ->
    InputObjectTypeDefinition1 = new(),
    InputObjectTypeDefinition2 =
        case LanguageOptionFields of
            none ->
                InputObjectTypeDefinition1;
            {some, #argo_graphql_language_input_fields_definition{inputs = []}} ->
                InputObjectTypeDefinition1;
            {some, #argo_graphql_language_input_fields_definition{
                inputs = LanguageInputFieldsDefinition = [#argo_graphql_language_input_value_definition{} | _]
            }} ->
                lists:foldl(
                    fun(LanguageInputValueDefinition, InputObjectTypeDefinition1Acc1) ->
                        InputValueDefinition = argo_graphql_input_value_definition:from_language(
                            LanguageInputValueDefinition
                        ),
                        InputObjectTypeDefinition1Acc2 = add_input_value_definition(
                            InputObjectTypeDefinition1Acc1, InputValueDefinition
                        ),
                        InputObjectTypeDefinition1Acc2
                    end,
                    InputObjectTypeDefinition1,
                    LanguageInputFieldsDefinition
                )
        end,
    InputObjectTypeDefinition2.

-spec new() -> InputObjectTypeDefinition when InputObjectTypeDefinition :: t().
new() ->
    #argo_graphql_input_object_type_definition{
        inputs = argo_index_map:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_input_value_definition(InputObjectTypeDefinition, InputValueDefinition) -> InputObjectTypeDefinition when
    InputObjectTypeDefinition :: t(), InputValueDefinition :: argo_graphql_input_value_definition:t().
add_input_value_definition(
    InputObjectTypeDefinition1 = #argo_graphql_input_object_type_definition{inputs = InputsMap1},
    InputValueDefinition = #argo_graphql_input_value_definition{name = InputName}
) ->
    case argo_index_map:is_key(InputName, InputsMap1) of
        false ->
            InputsMap2 = argo_index_map:put(InputName, InputValueDefinition, InputsMap1),
            InputObjectTypeDefinition2 = InputObjectTypeDefinition1#argo_graphql_input_object_type_definition{
                inputs = InputsMap2
            },
            InputObjectTypeDefinition2;
        true ->
            error_with_info(badarg, [InputObjectTypeDefinition1, InputValueDefinition], #{
                2 => {duplicate_input_name, InputName}
            })
    end.

-spec get_shape(InputObjectTypeDefinition) -> InputObjectShape when
    InputObjectTypeDefinition :: t(), InputObjectShape :: shape().
get_shape(_InputObjectTypeDefinition = #argo_graphql_input_object_type_definition{inputs = InputsMap}) ->
    Shape1 = #{type => input_object},
    Shape2 =
        argo_index_map:foldl(
            fun(_, InputName, #argo_graphql_input_value_definition{type = InputType}, Shape1_Acc1) ->
                maps:put(InputName, InputType, Shape1_Acc1)
            end,
            Shape1,
            InputsMap
        ),
    Shape2.

-spec get_shape(InputObjectTypeDefinition, ServiceDocument) -> InputObjectShape when
    InputObjectTypeDefinition :: t(), ServiceDocument :: argo_graphql_service_document:t(), InputObjectShape :: shape().
get_shape(
    InputObjectTypeDefinition = #argo_graphql_input_object_type_definition{},
    _ServiceDocument = #argo_graphql_service_document{}
) ->
    get_shape(InputObjectTypeDefinition).

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_input_object_type_definition{inputs = InputsMap}) ->
    argo_index_map:size(InputsMap) =:= 0.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_input_object_type_definition{inputs = InputsMap}) ->
    case argo_index_map:size(InputsMap) of
        0 ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, " {}", []),
            Formatter2;
        _ ->
            Formatter2 = argo_graphql_formatter:write(Formatter1, " {", []),
            Formatter3 = argo_graphql_formatter:shift_right(Formatter2),
            Formatter4 = argo_index_map:foldl(
                fun(_Index, _InputName, InputValueDefinition, Formatter3_Acc1) ->
                    Formatter3_Acc2 = argo_graphql_formatter:write(Formatter3_Acc1, "~n", []),
                    Formatter3_Acc3 = argo_graphql_formatter:write_indent(Formatter3_Acc2),
                    Formatter3_Acc4 = argo_graphql_input_value_definition:format(
                        Formatter3_Acc3, InputValueDefinition
                    ),
                    Formatter3_Acc4
                end,
                Formatter3,
                InputsMap
            ),
            Formatter5 = argo_graphql_formatter:write(Formatter4, "~n", []),
            Formatter6 = argo_graphql_formatter:shift_left(Formatter5),
            Formatter7 = argo_graphql_formatter:write_indent(Formatter6),
            Formatter8 = argo_graphql_formatter:write(Formatter7, "}", []),
            Formatter8
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
format_error_description(_Key, {duplicate_input_name, InputName}) ->
    io_lib:format("duplicate InputValueDefinition name: ~0tp", [InputName]);
format_error_description(_Key, Value) ->
    Value.
