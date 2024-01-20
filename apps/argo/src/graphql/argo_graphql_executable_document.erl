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
-module(argo_graphql_executable_document).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    from_file/1,
    from_language/1,
    from_string/1,
    new/0
]).
%% Instance API
-export([
    add_operation_definition/2,
    add_fragment_definition/2,
    get_fragment_definition/2,
    get_operation_definition/1,
    get_operation_definition/2
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
-type t() :: #argo_graphql_executable_document{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec from_file(Filename) -> ExecutableDocument when Filename :: file:filename_all(), ExecutableDocument :: t().
from_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Contents} ->
            from_string(Contents);
        {error, ReadError} ->
            error_with_info(badarg, [Filename], #{1 => {read_error, ReadError}})
    end.

-spec from_language(LanguageDocument) -> ExecutableDocument when
    LanguageDocument :: argo_graphql_language_document:t(), ExecutableDocument :: t().
from_language(LanguageDocument = #argo_graphql_language_document{definitions = LanguageDefinitionList}) ->
    ExecutableDocument1 = new(),
    ExecutableDocument2 = lists:foldl(
        fun(LanguageDefinition, ExecutableDocument1Acc1) ->
            case LanguageDefinition of
                #argo_graphql_language_definition{
                    inner = #argo_graphql_language_executable_definition{inner = LanguageExecutableDefinition}
                } ->
                    case LanguageExecutableDefinition of
                        LanguageOperationDefinition = #argo_graphql_language_operation_definition{} ->
                            OperationDefinition = argo_graphql_operation_definition:from_language(
                                LanguageOperationDefinition
                            ),
                            add_operation_definition(ExecutableDocument1Acc1, OperationDefinition);
                        LanguageFragmentDefinition = #argo_graphql_language_fragment_definition{} ->
                            FragmentDefinition = argo_graphql_fragment_definition:from_language(
                                LanguageFragmentDefinition
                            ),
                            add_fragment_definition(ExecutableDocument1Acc1, FragmentDefinition)
                    end;
                #argo_graphql_language_definition{inner = #argo_graphql_language_type_system_definition{}} ->
                    error_with_info(badarg, [LanguageDocument], #{1 => type_system_definition_not_supported});
                #argo_graphql_language_definition{inner = #argo_graphql_language_type_system_extension{}} ->
                    error_with_info(badarg, [LanguageDocument], #{1 => type_system_extension_not_supported})
            end
        end,
        ExecutableDocument1,
        LanguageDefinitionList
    ),
    ExecutableDocument2.

-spec from_string(Input) -> ExecutableDocument when Input :: unicode:chardata(), ExecutableDocument :: t().
from_string(Input) ->
    String = argo_types:unicode_string(Input),
    case argo_graphql_language_scanner:string(String) of
        {ok, Tokens, _EndLoc} ->
            case argo_graphql_language_document:from_tokens(Tokens) of
                {ok, LanguageDocument = #argo_graphql_language_document{}} ->
                    from_language(LanguageDocument);
                {error, ParserError} ->
                    error_with_info(badarg, [Input], #{1 => {parser_error, ParserError}})
            end;
        {error, ErrorInfo, EndLoc} ->
            error_with_info(badarg, [Input], #{1 => {scanner_error, #{error_info => ErrorInfo, end_loc => EndLoc}}})
    end.

-spec new() -> ExecutableDocument when ExecutableDocument :: t().
new() ->
    #argo_graphql_executable_document{
        operation_definitions = none,
        fragment_definitions = maps:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_fragment_definition(ExecutableDocument, FragmentDefinition) -> ExecutableDocument when
    ExecutableDocument :: t(), FragmentDefinition :: argo_graphql_fragment_definition:t().
add_fragment_definition(
    ExecutableDocument1 = #argo_graphql_executable_document{fragment_definitions = FragmentDefinitions1},
    FragmentDefinition = #argo_graphql_fragment_definition{name = FragmentName}
) ->
    case maps:is_key(FragmentName, FragmentDefinitions1) of
        false ->
            FragmentDefinitions2 = maps:put(FragmentName, FragmentDefinition, FragmentDefinitions1),
            ExecutableDocument2 = ExecutableDocument1#argo_graphql_executable_document{
                fragment_definitions = FragmentDefinitions2
            },
            ExecutableDocument2;
        true ->
            error_with_info(badarg, [ExecutableDocument1, FragmentDefinition], #{
                2 => {duplicate_fragment_name, FragmentName}
            })
    end.

-spec add_operation_definition(ExecutableDocument, OperationDefinition) -> ExecutableDocument when
    ExecutableDocument :: t(), OperationDefinition :: argo_graphql_operation_definition:t().
add_operation_definition(
    ExecutableDocument1 = #argo_graphql_executable_document{operation_definitions = OperationDefinitions1},
    OperationDefinition = #argo_graphql_operation_definition{name = {some, OperationName}}
) ->
    case OperationDefinitions1 of
        none ->
            OperationDefinitions2 = {multiple, #{OperationName => OperationDefinition}},
            ExecutableDocument2 = ExecutableDocument1#argo_graphql_executable_document{
                operation_definitions = OperationDefinitions2
            },
            ExecutableDocument2;
        {single, _} ->
            error_with_info(badarg, [ExecutableDocument1, OperationDefinition], #{
                2 => cannot_mix_named_and_unnamed_operations
            });
        {multiple, OperationsMap1} ->
            case maps:is_key(OperationName, OperationsMap1) of
                false ->
                    OperationsMap2 = maps:put(OperationName, OperationDefinition, OperationsMap1),
                    OperationDefinitions2 = {multiple, OperationsMap2},
                    ExecutableDocument2 = ExecutableDocument1#argo_graphql_executable_document{
                        operation_definitions = OperationDefinitions2
                    },
                    ExecutableDocument2;
                true ->
                    error_with_info(badarg, [ExecutableDocument1, OperationDefinition], #{
                        2 => {duplicate_operation_name, OperationName}
                    })
            end
    end;
add_operation_definition(
    ExecutableDocument1 = #argo_graphql_executable_document{operation_definitions = OperationDefinitions1},
    OperationDefinition = #argo_graphql_operation_definition{name = none}
) ->
    case OperationDefinitions1 of
        none ->
            OperationDefinitions2 = {single, OperationDefinition},
            ExecutableDocument2 = ExecutableDocument1#argo_graphql_executable_document{
                operation_definitions = OperationDefinitions2
            },
            ExecutableDocument2;
        {single, _} ->
            error_with_info(badarg, [ExecutableDocument1, OperationDefinition], #{
                2 => cannot_have_more_than_one_unnamed_operation
            });
        {multiple, _} ->
            error_with_info(badarg, [ExecutableDocument1, OperationDefinition], #{
                2 => cannot_mix_named_and_unnamed_operations
            })
    end.

-spec get_fragment_definition(ExecutableDocument, FragmentName) -> FragmentDefinition when
    ExecutableDocument :: t(),
    FragmentName :: argo_types:name(),
    FragmentDefinition :: argo_graphql_fragment_definition:t().
get_fragment_definition(
    #argo_graphql_executable_document{fragment_definitions = FragmentDefinitions}, FragmentName
) when is_binary(FragmentName) ->
    maps:get(FragmentName, FragmentDefinitions).

-spec get_operation_definition(ExecutableDocument) -> {OptionOperationName, OperationDefinition} when
    ExecutableDocument :: t(),
    OptionOperationName :: none | {some, argo_types:name()},
    OperationDefinition :: argo_graphql_operation_definition:t().
get_operation_definition(ExecutableDocument) ->
    get_operation_definition(ExecutableDocument, none).

-spec get_operation_definition(ExecutableDocument, OptionOperationName) ->
    {OptionOperationName, OperationDefinition}
when
    ExecutableDocument :: t(),
    OptionOperationName :: none | {some, argo_types:name()},
    OperationDefinition :: argo_graphql_operation_definition:t().
get_operation_definition(
    ExecutableDocument = #argo_graphql_executable_document{operation_definitions = OperationDefinitionsEnum},
    OptionOperationName
) when ?is_option_binary(OptionOperationName) ->
    case OperationDefinitionsEnum of
        {single, OperationDefinition} ->
            case OptionOperationName of
                none ->
                    {none, OperationDefinition};
                {some, OperationName} ->
                    error_with_info(badarg, [ExecutableDocument, OptionOperationName], #{
                        2 => {single_operation_must_not_have_name, OperationName}
                    })
            end;
        {multiple, OperationsMap} ->
            case maps:size(OperationsMap) of
                0 ->
                    error_with_info(badarg, [ExecutableDocument, OptionOperationName], #{1 => zero_operations_found});
                1 ->
                    case OptionOperationName of
                        none ->
                            [{OperationName, OperationDefinition}] = maps:to_list(OperationsMap),
                            {{some, OperationName}, OperationDefinition};
                        {some, OperationName} ->
                            case maps:find(OperationName, OperationsMap) of
                                {ok, OperationDefinition} ->
                                    {{some, OperationName}, OperationDefinition};
                                error ->
                                    error_with_info(badarg, [ExecutableDocument, OptionOperationName], #{
                                        2 => {operation_not_found, OperationName}
                                    })
                            end
                    end;
                _ ->
                    case OptionOperationName of
                        none ->
                            error_with_info(badarg, [ExecutableDocument, OptionOperationName], #{
                                2 => multiple_operations_must_have_name
                            });
                        {some, OperationName} ->
                            case maps:find(OperationName, OperationsMap) of
                                {ok, OperationDefinition} ->
                                    {{some, OperationName}, OperationDefinition};
                                error ->
                                    error_with_info(badarg, [ExecutableDocument, OptionOperationName], #{
                                        2 => {operation_not_found, OperationName}
                                    })
                            end
                    end
            end;
        none ->
            error_with_info(badarg, [ExecutableDocument, OptionOperationName], #{1 => zero_operations_found})
    end.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(
    Formatter1,
    #argo_graphql_executable_document{
        operation_definitions = OperationDefinitions, fragment_definitions = FragmentDefinitions
    }
) ->
    Formatter2 = format_operation_definitions(Formatter1, OperationDefinitions),
    Formatter3 = maps:fold(
        fun(_FragmentName, FragmentDefinition, Formatter2_Acc1) ->
            Formatter2_Acc2 = argo_graphql_fragment_definition:format(Formatter2_Acc1, FragmentDefinition),
            Formatter2_Acc3 = argo_graphql_formatter:write(Formatter2_Acc2, "~n", []),
            Formatter2_Acc3
        end,
        Formatter2,
        argo_types:dynamic_cast(maps:iterator(FragmentDefinitions, ordered))
    ),
    Formatter3.

%% @private
-spec format_operation_definitions(Formatter1, OperationDefinitions) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(),
    OperationDefinitions :: none | {single, OperationDefinition} | {multiple, #{OperationName => OperationDefinition}},
    OperationDefinition :: argo_graphql_operation_definition:t(),
    OperationName :: argo_types:name(),
    Formatter2 :: argo_graphql_formatter:t().
format_operation_definitions(Formatter1, none) ->
    Formatter1;
format_operation_definitions(Formatter1, {single, OperationDefinition}) ->
    Formatter2 = argo_graphql_operation_definition:format(Formatter1, OperationDefinition),
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~n", []),
    Formatter3;
format_operation_definitions(Formatter1, {multiple, OperationDefinitions}) ->
    Formatter2 = maps:fold(
        fun(_OperationName, OperationDefinition, Formatter1_Acc1) ->
            Formatter1_Acc2 = argo_graphql_operation_definition:format(Formatter1_Acc1, OperationDefinition),
            Formatter1_Acc3 = argo_graphql_formatter:write(Formatter1_Acc2, "~n", []),
            Formatter1_Acc3
        end,
        Formatter1,
        argo_types:dynamic_cast(maps:iterator(OperationDefinitions, ordered))
    ),
    Formatter2.

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
format_error_description(_Key, cannot_have_more_than_one_unnamed_operation) ->
    "cannot have more than one unnamed OperationDefinition in an ExecutableDocument";
format_error_description(_Key, cannot_mix_named_and_unnamed_operations) ->
    "cannot mix named and unnamed OperationDefinition in an ExecutableDocument";
format_error_description(_Key, {duplicate_fragment_name, FragmentName}) ->
    io_lib:format("duplicate FragmentDefinition name: ~0tp", [FragmentName]);
format_error_description(_Key, {duplicate_operation_name, OperationName}) ->
    io_lib:format("duplicate OperationDefinition name: ~0tp", [OperationName]);
format_error_description(_Key, {operation_not_found, OperationName}) ->
    io_lib:format("OperationName ~0tp is not found for ExecutableDocument", [OperationName]);
format_error_description(_Key, multiple_operations_must_have_name) ->
    "OperationName is required when multiple OperationDefinition are found inside ExecutableDocument";
format_error_description(_Key, {parser_error, ParserError}) ->
    io_lib:format("parser error: ~0tp", [ParserError]);
format_error_description(_Key, {read_error, ReadError}) ->
    io_lib:format("read error: ~0tp", [ReadError]);
format_error_description(_Key, {scanner_error, #{error_info := ErrorInfo, end_loc := EndLoc}}) ->
    io_lib:format("scanner error at end location ~0tp: ~0tp", [EndLoc, ErrorInfo]);
format_error_description(_Key, {single_operation_must_not_have_name, OperationName}) ->
    io_lib:format(
        "invalid OperationName ~0tp provided for ExecutableDocument with a single unnamed OperationDefinition", [
            OperationName
        ]
    );
format_error_description(_Key, type_system_definition_not_supported) ->
    "TypeSystemDefinition is not supported by ExecutableDocument";
format_error_description(_Key, type_system_extension_not_supported) ->
    "TypeSystemExtension is not supported by ExecutableDocument";
format_error_description(_Key, zero_operations_found) ->
    "zero OperationDefinition found inside ExecutableDocument";
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
