%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_directive_definition).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    builtin/1,
    from_language/1,
    new/2
]).
%% Instance API
-export([
    add_argument_definition/2,
    add_argument_definitions/2,
    add_directive_location/2,
    add_directive_locations/2,
    set_description/2
]).
%% argo_graphql_display callbacks
-export([
    format/2
]).

%% Types
-type directive_location() ::
    argo_graphql_language_executable_directive_location:name()
    | argo_graphql_language_type_system_directive_location:name().
-type t() :: #argo_graphql_directive_definition{}.

-export_type([
    directive_location/0,
    t/0
]).

%% Macros
-define(is_directive_location(T), (?is_executable_directive_location(T) orelse ?is_type_system_directive_location(T))).
-define(is_executable_directive_location(T),
    ((T) =:= 'QUERY' orelse (T) =:= 'MUTATION' orelse (T) =:= 'SUBSCRIPTION' orelse (T) =:= 'FIELD' orelse
        (T) =:= 'FRAGMENT_DEFINITION' orelse (T) =:= 'FRAGMENT_SPREAD' orelse (T) =:= 'INLINE_FRAGMENT' orelse
        (T) =:= 'VARIABLE_DEFINITION')
).
-define(is_type_system_directive_location(T),
    ((T) =:= 'SCHEMA' orelse
        (T) =:= 'SCALAR' orelse
        (T) =:= 'OBJECT' orelse
        (T) =:= 'FIELD_DEFINITION' orelse
        (T) =:= 'ARGUMENT_DEFINITION' orelse
        (T) =:= 'INTERFACE' orelse
        (T) =:= 'UNION' orelse
        (T) =:= 'ENUM' orelse
        (T) =:= 'ENUM_VALUE' orelse
        (T) =:= 'INPUT_OBJECT' orelse
        (T) =:= 'INPUT_FIELD_DEFINITION')
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec builtin(DirectiveName) -> {ok, DirectiveDefinition} | error when
    DirectiveName :: argo_types:name(), DirectiveDefinition :: t().
builtin(DirectiveName = <<"ArgoCodec">>) ->
    % See: https://msolomon.github.io/argo/versions/1.2/spec#sec-Directives
    DirectiveDefinition1 = new(DirectiveName, false),
    CodecArgumentType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"ArgoCodecType">>)),
    CodecArgumentDefinition1 = argo_graphql_input_value_definition:new(<<"codec">>, CodecArgumentType),
    CodecArgumentDefinition2 = argo_graphql_input_value_definition:set_description(
        CodecArgumentDefinition1, {some, <<"The codec to use to serialize and deserialize this scalar.">>}
    ),
    FixedLengthArgumentType = argo_graphql_type:named_type(<<"Int">>),
    FixedLengthArgumentDefinition1 = argo_graphql_input_value_definition:new(
        <<"fixedLength">>, FixedLengthArgumentType
    ),
    FixedLengthArgumentDefinition2 = argo_graphql_input_value_definition:set_description(
        FixedLengthArgumentDefinition1,
        {some,
            <<"For the FIXED codec only: the length of the encoded value in bytes. Required for FIXED, and invalid for all other codecs.">>}
    ),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [
        CodecArgumentDefinition2, FixedLengthArgumentDefinition2
    ]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, ['SCALAR', 'ENUM']),
    DirectiveDefinition4 = set_description(
        DirectiveDefinition3,
        {some,
            <<"Specifies how to serialize and deserialize this scalar. This is necessary for custom scalars to work with Argo serialization. Adding, changing, or removing this directive is typically a breaking change.">>}
    ),
    {ok, DirectiveDefinition4};
builtin(DirectiveName = <<"ArgoDeduplicate">>) ->
    % See: https://msolomon.github.io/argo/versions/1.2/spec#sec-Directives
    DirectiveDefinition1 = new(DirectiveName, false),
    DeduplicateArgumentType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    DeduplicateArgumentDefinition1 = argo_graphql_input_value_definition:new(
        <<"deduplicate">>, DeduplicateArgumentType
    ),
    DeduplicatDefaultValue = argo_graphql_value_const:boolean(true),
    DeduplicateArgumentDefinition2 = argo_graphql_input_value_definition:set_default_value(
        DeduplicateArgumentDefinition1, {some, DeduplicatDefaultValue}
    ),
    DeduplicateArgumentDefinition3 = argo_graphql_input_value_definition:set_description(
        DeduplicateArgumentDefinition2, {some, <<"Should values of this type be deduplicated?">>}
    ),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [DeduplicateArgumentDefinition3]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, ['SCALAR', 'ENUM']),
    DirectiveDefinition4 = set_description(
        DirectiveDefinition3,
        {some, <<"Deduplicate values of this type. Adding or removing this directive is typically a breaking change.">>}
    ),
    {ok, DirectiveDefinition4};
builtin(DirectiveName = <<"defer">>) ->
    % See: https://github.com/graphql/graphql-spec/pull/742
    DirectiveDefinition1 = new(DirectiveName, false),
    LabelArgumentType = argo_graphql_type:named_type(<<"String">>),
    LabelArgumentDefinition1 = argo_graphql_input_value_definition:new(<<"label">>, LabelArgumentType),
    IfArgumentType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IfArgumentDefaultValue = argo_graphql_value_const:boolean(true),
    IfArgumentDefinition1 = argo_graphql_input_value_definition:new(<<"if">>, IfArgumentType),
    IfArgumentDefinition2 = argo_graphql_input_value_definition:set_default_value(
        IfArgumentDefinition1, {some, IfArgumentDefaultValue}
    ),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [
        LabelArgumentDefinition1, IfArgumentDefinition2
    ]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, ['FRAGMENT_SPREAD', 'INLINE_FRAGMENT']),
    {ok, DirectiveDefinition3};
builtin(DirectiveName = <<"deprecated">>) ->
    % See: https://spec.graphql.org/draft/#sec--deprecated
    DirectiveDefinition1 = new(DirectiveName, false),
    ReasonArgumentType = argo_graphql_type:named_type(<<"String">>),
    ReasonArgumentDefinition1 = argo_graphql_input_value_definition:new(<<"reason">>, ReasonArgumentType),
    ReasonDefaultValue = argo_graphql_value_const:string(<<"No longer supported">>),
    ReasonArgumentDefinition2 = argo_graphql_input_value_definition:set_default_value(
        ReasonArgumentDefinition1, {some, ReasonDefaultValue}
    ),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [ReasonArgumentDefinition2]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, [
        'FIELD_DEFINITION', 'ARGUMENT_DEFINITION', 'INPUT_FIELD_DEFINITION', 'ENUM_VALUE'
    ]),
    {ok, DirectiveDefinition3};
builtin(DirectiveName = <<"include">>) ->
    % See: https://spec.graphql.org/draft/#sec--include
    DirectiveDefinition1 = new(DirectiveName, false),
    IfArgumentType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IfArgumentDefinition = argo_graphql_input_value_definition:new(<<"if">>, IfArgumentType),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [IfArgumentDefinition]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT']),
    {ok, DirectiveDefinition3};
builtin(DirectiveName = <<"oneOf">>) ->
    % See: https://github.com/graphql/graphql-spec/pull/825
    DirectiveDefinition1 = new(DirectiveName, false),
    DirectiveDefinition2 = add_directive_locations(DirectiveDefinition1, ['INPUT_OBJECT']),
    {ok, DirectiveDefinition2};
builtin(DirectiveName = <<"skip">>) ->
    % See: https://spec.graphql.org/draft/#sec--skip
    DirectiveDefinition1 = new(DirectiveName, false),
    IfArgumentType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IfArgumentDefinition = argo_graphql_input_value_definition:new(<<"if">>, IfArgumentType),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [IfArgumentDefinition]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT']),
    {ok, DirectiveDefinition3};
builtin(DirectiveName = <<"specifiedBy">>) ->
    % https://spec.graphql.org/draft/#sec--specifiedBy
    DirectiveDefinition1 = new(DirectiveName, false),
    UrlArgumentType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"String">>)),
    UrlArgumentDefinition = argo_graphql_input_value_definition:new(<<"url">>, UrlArgumentType),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [UrlArgumentDefinition]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, ['SCALAR']),
    {ok, DirectiveDefinition3};
builtin(DirectiveName = <<"stream">>) ->
    % See: https://github.com/graphql/graphql-spec/pull/742
    DirectiveDefinition1 = new(DirectiveName, false),
    LabelArgumentType = argo_graphql_type:named_type(<<"String">>),
    LabelArgumentDefinition1 = argo_graphql_input_value_definition:new(<<"label">>, LabelArgumentType),
    IfArgumentType = argo_graphql_type:non_null_type(argo_graphql_non_null_type:new(<<"Boolean">>)),
    IfArgumentDefaultValue = argo_graphql_value_const:boolean(true),
    IfArgumentDefinition1 = argo_graphql_input_value_definition:new(<<"if">>, IfArgumentType),
    IfArgumentDefinition2 = argo_graphql_input_value_definition:set_default_value(
        IfArgumentDefinition1, {some, IfArgumentDefaultValue}
    ),
    InitialCountArgumentType = argo_graphql_type:named_type(<<"Int">>),
    InitialCountArgumentDefaultValue = argo_graphql_value_const:int(0),
    InitialCountArgumentDefinition1 = argo_graphql_input_value_definition:new(
        <<"initialCount">>, InitialCountArgumentType
    ),
    InitialCountArgumentDefinition2 = argo_graphql_input_value_definition:set_default_value(
        InitialCountArgumentDefinition1, {some, InitialCountArgumentDefaultValue}
    ),
    DirectiveDefinition2 = add_argument_definitions(DirectiveDefinition1, [
        LabelArgumentDefinition1, IfArgumentDefinition2, InitialCountArgumentDefinition2
    ]),
    DirectiveDefinition3 = add_directive_locations(DirectiveDefinition2, ['FIELD']),
    {ok, DirectiveDefinition3};
builtin(DirectiveName) when is_binary(DirectiveName) ->
    error.

-spec from_language(LanguageDirectiveDefinition) -> DirectiveDefinition when
    LanguageDirectiveDefinition :: argo_graphql_language_directive_definition:t(), DirectiveDefinition :: t().
from_language(#argo_graphql_language_directive_definition{
    name = Name,
    locations = LanguageDirectiveLocations,
    description = LanguageOptionDescription,
    arguments = LanguageOptionArgumentsDefinition,
    repeatable = Repeatable
}) ->
    DirectiveDefinition1 = new(Name, Repeatable),
    DirectiveDefinition2 =
        case LanguageOptionDescription of
            none ->
                DirectiveDefinition1;
            {some, Description} ->
                set_description(DirectiveDefinition1, {some, Description})
        end,
    DirectiveDefinition3 =
        case LanguageOptionArgumentsDefinition of
            none ->
                DirectiveDefinition2;
            {some, LanguageArgumentsDefinition} ->
                ArgumentsDefinition = argo_graphql_arguments_definition:from_language(LanguageArgumentsDefinition),
                DirectiveDefinition2#argo_graphql_directive_definition{arguments = ArgumentsDefinition}
        end,
    DirectiveDefinition4 = lists:foldl(
        fun(LanguageDirectiveLocation, DirectiveDefinition3Acc1) ->
            DirectiveLocation =
                case LanguageDirectiveLocation of
                    #argo_graphql_language_directive_location{
                        inner = #argo_graphql_language_executable_directive_location{name = LocationName}
                    } ->
                        LocationName;
                    #argo_graphql_language_directive_location{
                        inner = #argo_graphql_language_type_system_directive_location{name = LocationName}
                    } ->
                        LocationName
                end,
            DirectiveDefinition3Acc2 = add_directive_location(DirectiveDefinition3Acc1, DirectiveLocation),
            DirectiveDefinition3Acc2
        end,
        DirectiveDefinition3,
        LanguageDirectiveLocations
    ),
    DirectiveDefinition4.

-compile({inline, [new/2]}).
-spec new(Name, Repeatable) -> DirectiveDefinition when
    Name :: argo_types:name(), Repeatable :: boolean(), DirectiveDefinition :: t().
new(Name, Repeatable) when is_binary(Name) andalso is_boolean(Repeatable) ->
    #argo_graphql_directive_definition{
        name = Name,
        locations = argo_index_set:new(),
        description = none,
        arguments = argo_graphql_arguments_definition:new(),
        repeatable = Repeatable
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_argument_definition(DirectiveDefinition, InputValueDefinition) -> DirectiveDefinition when
    DirectiveDefinition :: t(), InputValueDefinition :: argo_graphql_input_value_definition:t().
add_argument_definition(
    DirectiveDefinition1 = #argo_graphql_directive_definition{arguments = ArgumentsDefinition1},
    InputValueDefinition = #argo_graphql_input_value_definition{}
) ->
    ArgumentsDefinition2 = argo_graphql_arguments_definition:add_input_value_definition(
        ArgumentsDefinition1, InputValueDefinition
    ),
    DirectiveDefinition2 = DirectiveDefinition1#argo_graphql_directive_definition{arguments = ArgumentsDefinition2},
    DirectiveDefinition2.

-spec add_argument_definitions(DirectiveDefinition, InputValueDefinitionList) -> DirectiveDefinition when
    DirectiveDefinition :: t(),
    InputValueDefinitionList :: [InputValueDefinition],
    InputValueDefinition :: argo_graphql_input_value_definition:t().
add_argument_definitions(DirectiveDefinition1 = #argo_graphql_directive_definition{}, [
    InputValueDefinition = #argo_graphql_input_value_definition{} | InputValueDefinitions
]) ->
    DirectiveDefinition2 = add_argument_definition(DirectiveDefinition1, InputValueDefinition),
    add_argument_definitions(DirectiveDefinition2, InputValueDefinitions);
add_argument_definitions(DirectiveDefinition1 = #argo_graphql_directive_definition{}, []) ->
    DirectiveDefinition1.

-spec add_directive_location(DirectiveDefinition, DirectiveLocation) -> DirectiveDefinition when
    DirectiveDefinition :: t(), DirectiveLocation :: directive_location().
add_directive_location(
    DirectiveDefinition1 = #argo_graphql_directive_definition{locations = DirectiveLocations1}, DirectiveLocation
) when ?is_directive_location(DirectiveLocation) ->
    DirectiveLocations2 = argo_index_set:add_element(DirectiveLocation, DirectiveLocations1),
    DirectiveDefinition2 = DirectiveDefinition1#argo_graphql_directive_definition{locations = DirectiveLocations2},
    DirectiveDefinition2.

-spec add_directive_locations(DirectiveDefinition, DirectiveLocationList) -> DirectiveDefinition when
    DirectiveDefinition :: t(), DirectiveLocationList :: [DirectiveLocation], DirectiveLocation :: directive_location().
add_directive_locations(DirectiveDefinition1 = #argo_graphql_directive_definition{}, [
    DirectiveLocation | DirectiveLocations
]) when ?is_directive_location(DirectiveLocation) ->
    DirectiveDefinition2 = add_directive_location(DirectiveDefinition1, DirectiveLocation),
    add_directive_locations(DirectiveDefinition2, DirectiveLocations);
add_directive_locations(DirectiveDefinition1 = #argo_graphql_directive_definition{}, []) ->
    DirectiveDefinition1.

-spec set_description(DirectiveDefinition, OptionDescription) -> DirectiveDefinition when
    DirectiveDefinition :: t(), OptionDescription :: none | {some, unicode:unicode_binary()}.
set_description(DirectiveDefinition1 = #argo_graphql_directive_definition{}, OptionDescription) when
    ?is_option_binary(OptionDescription)
->
    DirectiveDefinition2 = DirectiveDefinition1#argo_graphql_directive_definition{description = OptionDescription},
    DirectiveDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_directive_definition{
    name = Name,
    locations = Locations,
    description = OptionDescription,
    arguments = ArgumentsDefinition,
    repeatable = Repeatable
}) ->
    Formatter2 =
        case OptionDescription of
            {some, Description} ->
                F1_1 = Formatter1,
                F1_2 = argo_graphql_formatter:write_description(F1_1, Description),
                F1_3 = argo_graphql_formatter:write_indent(F1_2),
                F1_3;
            none ->
                Formatter1
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "directive @~ts", [Name]),
    Formatter4 = argo_graphql_arguments_definition:format(Formatter3, ArgumentsDefinition),
    Formatter5 =
        case Repeatable of
            false ->
                Formatter4;
            true ->
                argo_graphql_formatter:write(Formatter4, " repeatable", [])
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, " on ", []),
    LocationsLength = argo_index_set:size(Locations),
    Formatter7 = argo_index_set:foldl(
        fun(Index, DirectiveLocation, Formatter6_Acc1) ->
            Formatter6_Acc2 = argo_graphql_formatter:write(Formatter6_Acc1, "~ts", [DirectiveLocation]),
            Formatter6_Acc3 =
                case (Index + 1) =:= LocationsLength of
                    false ->
                        argo_graphql_formatter:write(Formatter6_Acc2, " | ", []);
                    true ->
                        Formatter6_Acc2
                end,
            Formatter6_Acc3
        end,
        Formatter6,
        Locations
    ),
    Formatter8 = argo_graphql_formatter:write(Formatter7, "~n", []),
    Formatter8.
