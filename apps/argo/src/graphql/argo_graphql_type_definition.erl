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
-module(argo_graphql_type_definition).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_graphql_display).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    builtin/1,
    from_language/1,
    scalar_type_definition/2,
    object_type_definition/2,
    interface_type_definition/2,
    union_type_definition/2,
    enum_type_definition/2,
    input_object_type_definition/2
]).
%% Instance API
-export([
    add_directive_const/2,
    find_field_definition/2,
    get_field_definition/2,
    is_ambiguous/1,
    set_description/2
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
-type kind() ::
    argo_graphql_scalar_type_definition:t()
    | argo_graphql_object_type_definition:t()
    | argo_graphql_interface_type_definition:t()
    | argo_graphql_union_type_definition:t()
    | argo_graphql_enum_type_definition:t()
    | argo_graphql_input_object_type_definition:t().
-type t() :: #argo_graphql_type_definition{}.

-export_type([
    kind/0,
    t/0
]).

%% Macros
-define(is_builtin_scalar(T),
    ((T) =:= <<"Boolean">> orelse (T) =:= <<"Float">> orelse (T) =:= <<"ID">> orelse (T) =:= <<"Int">> orelse
        (T) =:= <<"String">>)
).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec builtin(TypeName) -> {ok, TypeDefinition} | error when TypeName :: argo_types:name(), TypeDefinition :: t().
builtin(TypeName = <<"ArgoCodecType">>) ->
    % See: https://msolomon.github.io/argo/versions/1.0/spec#sec-Directives
    EnumTypeDefinition1 = argo_graphql_enum_type_definition:new(),
    EnumTypeDefinition2 = argo_graphql_enum_type_definition:add_enum_value_definitions(EnumTypeDefinition1, [
        argo_graphql_enum_value_definition:new(<<"String">>),
        argo_graphql_enum_value_definition:new(<<"Int">>),
        argo_graphql_enum_value_definition:new(<<"Float">>),
        argo_graphql_enum_value_definition:new(<<"Boolean">>),
        argo_graphql_enum_value_definition:new(<<"BYTES">>),
        argo_graphql_enum_value_definition:new(<<"FIXED">>)
    ]),
    TypeDefinition = enum_type_definition(TypeName, EnumTypeDefinition2),
    {ok, TypeDefinition};
builtin(TypeName) when ?is_builtin_scalar(TypeName) ->
    % See: https://spec.graphql.org/draft/#sec-Scalars.Built-in-Scalars
    ScalarTypeDefinition = argo_graphql_scalar_type_definition:new(),
    TypeDefinition = scalar_type_definition(TypeName, ScalarTypeDefinition),
    {ok, TypeDefinition};
builtin(TypeName) when is_binary(TypeName) ->
    error.

-spec from_language(LanguageTypeDefinition) -> TypeDefinition when
    LanguageTypeDefinition :: argo_graphql_language_type_definition:t(), TypeDefinition :: t().
from_language(#argo_graphql_language_type_definition{inner = Inner}) ->
    {TypeDefinition1, LanguageOptionDescription, LanguageOptionDirectivesConst} =
        case Inner of
            #argo_graphql_language_scalar_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_scalar_type_definition:from_language(Inner),
                {scalar_type_definition(Name, Kind), A, B};
            #argo_graphql_language_object_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_object_type_definition:from_language(Inner),
                {object_type_definition(Name, Kind), A, B};
            #argo_graphql_language_interface_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_interface_type_definition:from_language(Inner),
                {interface_type_definition(Name, Kind), A, B};
            #argo_graphql_language_union_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_union_type_definition:from_language(Inner),
                {union_type_definition(Name, Kind), A, B};
            #argo_graphql_language_enum_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_enum_type_definition:from_language(Inner),
                {enum_type_definition(Name, Kind), A, B};
            #argo_graphql_language_input_object_type_definition{name = Name, description = A, directives = B} ->
                Kind = argo_graphql_input_object_type_definition:from_language(Inner),
                {input_object_type_definition(Name, Kind), A, B}
        end,
    TypeDefinition2 =
        case LanguageOptionDescription of
            none ->
                TypeDefinition1;
            {some, Description} ->
                set_description(TypeDefinition1, {some, Description})
        end,
    TypeDefinition3 =
        case LanguageOptionDirectivesConst of
            none ->
                TypeDefinition2;
            {some, LanguageDirectivesConst} ->
                DirectivesConst = argo_graphql_directives_const:from_language(LanguageDirectivesConst),
                TypeDefinition2#argo_graphql_type_definition{directives = DirectivesConst}
        end,
    TypeDefinition3.

-compile({inline, [scalar_type_definition/2]}).
-spec scalar_type_definition(Name, ScalarTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    ScalarTypeDefinition :: argo_graphql_scalar_type_definition:t(),
    TypeDefinition :: t().
scalar_type_definition(Name, ScalarTypeDefinition = #argo_graphql_scalar_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = ScalarTypeDefinition
    }.

-compile({inline, [object_type_definition/2]}).
-spec object_type_definition(Name, ObjectTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    ObjectTypeDefinition :: argo_graphql_object_type_definition:t(),
    TypeDefinition :: t().
object_type_definition(Name, ObjectTypeDefinition = #argo_graphql_object_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = ObjectTypeDefinition
    }.

-compile({inline, [interface_type_definition/2]}).
-spec interface_type_definition(Name, InterfaceTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    InterfaceTypeDefinition :: argo_graphql_interface_type_definition:t(),
    TypeDefinition :: t().
interface_type_definition(Name, InterfaceTypeDefinition = #argo_graphql_interface_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name,
        description = none,
        directives = argo_graphql_directives_const:new(),
        kind = InterfaceTypeDefinition
    }.

-compile({inline, [union_type_definition/2]}).
-spec union_type_definition(Name, UnionTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    UnionTypeDefinition :: argo_graphql_union_type_definition:t(),
    TypeDefinition :: t().
union_type_definition(Name, UnionTypeDefinition = #argo_graphql_union_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = UnionTypeDefinition
    }.

-compile({inline, [enum_type_definition/2]}).
-spec enum_type_definition(Name, EnumTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    EnumTypeDefinition :: argo_graphql_enum_type_definition:t(),
    TypeDefinition :: t().
enum_type_definition(Name, EnumTypeDefinition = #argo_graphql_enum_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name, description = none, directives = argo_graphql_directives_const:new(), kind = EnumTypeDefinition
    }.

-compile({inline, [input_object_type_definition/2]}).
-spec input_object_type_definition(Name, InputObjectTypeDefinition) -> TypeDefinition when
    Name :: argo_types:name(),
    InputObjectTypeDefinition :: argo_graphql_input_object_type_definition:t(),
    TypeDefinition :: t().
input_object_type_definition(Name, InputObjectTypeDefinition = #argo_graphql_input_object_type_definition{}) ->
    #argo_graphql_type_definition{
        name = Name,
        description = none,
        directives = argo_graphql_directives_const:new(),
        kind = InputObjectTypeDefinition
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_directive_const(TypeDefinition, DirectiveConst) -> TypeDefinition when
    TypeDefinition :: t(), DirectiveConst :: argo_graphql_directive_const:t().
add_directive_const(
    TypeDefinition1 = #argo_graphql_type_definition{directives = DirectivesConst1},
    DirectiveConst = #argo_graphql_directive_const{}
) ->
    DirectivesConst2 = argo_graphql_directives_const:add_directive_const(DirectivesConst1, DirectiveConst),
    TypeDefinition2 = TypeDefinition1#argo_graphql_type_definition{directives = DirectivesConst2},
    TypeDefinition2.

-spec find_field_definition(TypeDefinition, FieldName) -> {ok, FieldDefinition} | error when
    TypeDefinition :: t(), FieldName :: argo_types:name(), FieldDefinition :: argo_graphql_field_definition:t().
find_field_definition(TypeDefinition = #argo_graphql_type_definition{kind = Kind}, FieldName) when
    is_binary(FieldName)
->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {no_field_definitions, #{type => scalar}}});
        #argo_graphql_object_type_definition{fields = Fields} ->
            case argo_index_map:find(FieldName, Fields) of
                {ok, FieldDefinition} ->
                    {ok, FieldDefinition};
                error ->
                    argo_graphql_field_definition:builtin(FieldName)
            end;
        #argo_graphql_interface_type_definition{fields = Fields} ->
            case argo_index_map:find(FieldName, Fields) of
                {ok, FieldDefinition} ->
                    {ok, FieldDefinition};
                error ->
                    argo_graphql_field_definition:builtin(FieldName)
            end;
        #argo_graphql_union_type_definition{fields = Fields} ->
            case argo_index_map:find(FieldName, Fields) of
                {ok, FieldDefinition} ->
                    {ok, FieldDefinition};
                error ->
                    argo_graphql_field_definition:builtin(FieldName)
            end;
        #argo_graphql_enum_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {no_field_definitions, #{type => enum}}});
        #argo_graphql_input_object_type_definition{} ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {no_field_definitions, #{type => input_object}}})
    end.

-spec get_field_definition(TypeDefinition, FieldName) -> FieldDefinition when
    TypeDefinition :: t(), FieldName :: argo_types:name(), FieldDefinition :: argo_graphql_field_definition:t().
get_field_definition(TypeDefinition = #argo_graphql_type_definition{}, FieldName) when is_binary(FieldName) ->
    case find_field_definition(TypeDefinition, FieldName) of
        {ok, FieldDefinition} ->
            FieldDefinition;
        error ->
            error_with_info(badarg, [TypeDefinition, FieldName], #{2 => {missing_field_definition, FieldName}})
    end.

% @doc Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand) to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
-spec is_ambiguous(Definition) -> boolean() when Definition :: t().
is_ambiguous(#argo_graphql_type_definition{kind = Kind}) ->
    case Kind of
        #argo_graphql_scalar_type_definition{} ->
            false;
        #argo_graphql_object_type_definition{} ->
            argo_graphql_object_type_definition:is_ambiguous(Kind);
        #argo_graphql_interface_type_definition{} ->
            argo_graphql_interface_type_definition:is_ambiguous(Kind);
        #argo_graphql_union_type_definition{} ->
            false;
        #argo_graphql_enum_type_definition{} ->
            argo_graphql_enum_type_definition:is_ambiguous(Kind);
        #argo_graphql_input_object_type_definition{} ->
            argo_graphql_input_object_type_definition:is_ambiguous(Kind)
    end.

-spec set_description(TypeDefinition, OptionDescription) -> TypeDefinition when
    TypeDefinition :: t(), OptionDescription :: none | {some, unicode:unicode_binary()}.
set_description(TypeDefinition1 = #argo_graphql_type_definition{}, OptionDescription) when
    ?is_option_binary(OptionDescription)
->
    TypeDefinition2 = TypeDefinition1#argo_graphql_type_definition{description = OptionDescription},
    TypeDefinition2.

%%%=============================================================================
%%% argo_graphql_display callbacks
%%%=============================================================================

-spec format(Formatter1, Type :: t()) -> Formatter2 when
    Formatter1 :: argo_graphql_formatter:t(), Formatter2 :: argo_graphql_formatter:t().
format(Formatter1, #argo_graphql_type_definition{
    name = Name, description = OptionDescription, directives = DirectivesConst, kind = Kind
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
    Label =
        case Kind of
            #argo_graphql_scalar_type_definition{} ->
                "scalar";
            #argo_graphql_object_type_definition{} ->
                "type";
            #argo_graphql_interface_type_definition{} ->
                "interface";
            #argo_graphql_union_type_definition{} ->
                "union";
            #argo_graphql_enum_type_definition{} ->
                "enum";
            #argo_graphql_input_object_type_definition{} ->
                "input"
        end,
    Formatter3 = argo_graphql_formatter:write(Formatter2, "~ts ~ts", [Label, Name]),
    Formatter4 = argo_graphql_directives_const:format(Formatter3, DirectivesConst),
    Formatter5 =
        case Kind of
            #argo_graphql_scalar_type_definition{} ->
                argo_graphql_scalar_type_definition:format(Formatter4, Kind);
            #argo_graphql_object_type_definition{} ->
                argo_graphql_object_type_definition:format(Formatter4, Kind);
            #argo_graphql_interface_type_definition{} ->
                argo_graphql_interface_type_definition:format(Formatter4, Kind);
            #argo_graphql_union_type_definition{} ->
                argo_graphql_union_type_definition:format(Formatter4, Kind);
            #argo_graphql_enum_type_definition{} ->
                argo_graphql_enum_type_definition:format(Formatter4, Kind);
            #argo_graphql_input_object_type_definition{} ->
                argo_graphql_input_object_type_definition:format(Formatter4, Kind)
        end,
    Formatter6 = argo_graphql_formatter:write(Formatter5, "~n", []),
    Formatter6.

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
format_error_description(_Key, {missing_field_definition, FieldName}) ->
    io_lib:format("missing FieldDefinition name: ~0tp", [FieldName]);
format_error_description(_Key, {no_field_definitions, #{type := Type}}) ->
    io_lib:format("FieldDefinition is not supported on ~0tp", [Type]);
format_error_description(_Key, Value) ->
    Value.
