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
%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(proper_argo_graphql_language).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("proper/include/proper.hrl").
-include_lib("argo/include/argo_graphql_language.hrl").

%% Helper API
-export([
    complex/1,
    complexity/0,
    mostly/2,
    option/1,
    strip_whitespace/1,
    strip_whitespace_left/1,
    strip_whitespace_right/1,
    with_complexity/1
]).

%% Primitive API
-export([
    block_string_character/0,
    block_string_value/0,
    block_string_value/1,
    bytes/0,
    fixed/1,
    name/0,
    name/1,
    name_continue/0,
    name_start/0,
    non_empty_vector/1,
    non_empty_vector_upto/2,
    non_whitespace/1,
    string_utf8/0,
    string_character/0,
    string_character_escape/1,
    string_value/0,
    string_value/1,
    unicode_codepoint/1,
    unicode_codepoint_upto/1,
    usize/0,
    varint/0,
    vector_upto/2
]).

%% GraphQL Language API
-export([
    argument/0,
    argument_const/0,
    arguments/0,
    arguments_const/0,
    arguments_definition/0,
    definition/0,
    definition/1,
    definitions/0,
    directive/0,
    directive_const/0,
    directive_definition/0,
    directive_location/0,
    directive_locations/0,
    directives/0,
    directives_const/0,
    document/0,
    enum_type_definition/0,
    enum_type_extension/0,
    enum_value/0,
    enum_value_definition/0,
    enum_values_definition/0,
    executable_definition/0,
    executable_directive_location/0,
    field/0,
    field_definition/0,
    fields_definition/0,
    fragment_definition/0,
    fragment_name/0,
    fragment_spread/0,
    implements_interfaces/0,
    inline_fragment/0,
    input_fields_definition/0,
    input_object_type_definition/0,
    input_object_type_extension/0,
    input_value_definition/0,
    interface_type_definition/0,
    interface_type_extension/0,
    list_type/0,
    list_value/0,
    list_value_const/0,
    named_type/0,
    non_null_type/0,
    object_field/0,
    object_field_const/0,
    object_type_definition/0,
    object_type_extension/0,
    object_value/0,
    object_value_const/0,
    operation_definition/0,
    operation_type/0,
    root_operation_type_definition/0,
    root_operation_types_definition/0,
    scalar_type_definition/0,
    scalar_type_extension/0,
    schema_definition/0,
    schema_extension/0,
    selection/0,
    selection_set/0,
    type/0,
    type_condition/0,
    type_definition/0,
    type_extension/0,
    type_system_definition/0,
    type_system_directive_location/0,
    type_system_extension/0,
    union_member_types/0,
    union_type_definition/0,
    union_type_extension/0,
    value/0,
    value_const/0,
    variable/0,
    variable_definition/0,
    variables_definition/0
]).

%% Macros
-define(COMPLEXITY, 'proper_argo_complexity').
-define(CONTEXT(KeyValueListType),
    ?LET(
        KeyValueList,
        KeyValueListType,
        lists:foldl(
            fun({Key, OptionValue}, ContextAcc) ->
                case OptionValue of
                    none ->
                        ContextAcc;
                    {some, Value} ->
                        maps:put(Key, Value, ContextAcc);
                    Value ->
                        maps:put(Key, Value, ContextAcc)
                end
            end,
            maps:new(),
            KeyValueList
        )
    )
).
-define(is_whitespace(C), ((C) =:= $\s orelse (C) =:= $\t orelse (C) =:= $\r orelse (C) =:= $\n)).

%%%=============================================================================
%%% Helper API functions
%%%=============================================================================

-spec complex(RawType :: proper_types:raw_type()) -> proper_types:type().
complex(RawType) ->
    with_complexity(
        ?SIZED(Size, begin
            Complexity = complexity(),
            ComplexSize = max(1, Size bsr Complexity),
            ?LET(NewSize, mostly(range(1, min(ComplexSize, 4)), range(1, ComplexSize)), resize(NewSize, RawType))
        end)
    ).

-spec complexity() -> pos_integer().
complexity() ->
    case parameter(?COMPLEXITY, 0) of
        Complexity when is_integer(Complexity) andalso Complexity >= 0 ->
            max(Complexity, 1)
    end.

-spec mostly(U :: proper_types:type(), T :: proper_types:type()) -> proper_types:type().
mostly(U, T) ->
    frequency([
        {100, U},
        {1, T}
    ]).

-spec option(T :: proper_types:type()) -> proper_types:type().
option(T) ->
    oneof([none, {some, T}]).

-spec strip_whitespace(string()) -> string().
strip_whitespace(S) ->
    strip_whitespace_right(strip_whitespace_left(S)).

-spec strip_whitespace_left(string()) -> string().
strip_whitespace_left([W | S]) when ?is_whitespace(W) ->
    strip_whitespace_left(S);
strip_whitespace_left(S = [_ | _]) ->
    S;
strip_whitespace_left([]) ->
    [].

-spec strip_whitespace_right(string()) -> string().
strip_whitespace_right([W | S]) when ?is_whitespace(W) ->
    case strip_whitespace_right(S) of
        [] ->
            [];
        T ->
            [W | T]
    end;
strip_whitespace_right([C | S]) ->
    [C | strip_whitespace_right(S)];
strip_whitespace_right([]) ->
    [].

-spec with_complexity(RawType :: proper_types:raw_type()) -> proper_types:type().
with_complexity(RawType) ->
    Complexity =
        case parameter(?COMPLEXITY, 0) of
            C when is_integer(C) andalso C >= 0 ->
                C + 1
        end,
    with_parameter(?COMPLEXITY, Complexity, RawType).

%%%=============================================================================
%%% Primitive API functions
%%%=============================================================================

-spec block_string_character() -> proper_types:type().
block_string_character() ->
    unicode_codepoint_upto(4).

-spec block_string_value() -> proper_types:type().
block_string_value() ->
    ?LET(
        Characters,
        block_string_value_is_instance(list(block_string_character())),
        unicode:characters_to_binary(strip_whitespace(Characters))
    ).

-spec block_string_value(Length :: non_neg_integer()) -> proper_types:type().
block_string_value(0) ->
    exactly(<<>>);
block_string_value(1) ->
    ?LET(Character, non_whitespace(block_string_character()), unicode:characters_to_binary([Character]));
block_string_value(Length) when is_integer(Length) andalso Length >= 2 ->
    MiddleLength = Length - 2,
    FirstGen = non_whitespace(block_string_character()),
    MiddleGen = vector(MiddleLength, block_string_character()),
    LastGen = non_whitespace(block_string_character()),
    ?LET(
        Characters,
        block_string_value_is_instance(
            ?LET({First, Middle, Last}, {FirstGen, MiddleGen, LastGen}, [First] ++ Middle ++ [Last])
        ),
        unicode:characters_to_binary(Characters)
    ).

%% @private
-spec block_string_value_is_instance(RawType :: proper_types:raw_type()) -> proper_types:type().
block_string_value_is_instance(RawType) ->
    ?SUCHTHAT(Characters, RawType, block_string_value_is_valid(Characters)).

%% @private
-spec block_string_value_is_valid(Characters :: [char()]) -> boolean().
block_string_value_is_valid([$", $", $" | _]) ->
    false;
block_string_value_is_valid([_ | Rest]) ->
    block_string_value_is_valid(Rest);
block_string_value_is_valid([]) ->
    true.

-spec bytes() -> proper_types:type().
bytes() ->
    proper_types:binary().

-spec fixed(Length :: non_neg_integer()) -> proper_types:type().
fixed(Length) when is_integer(Length) andalso Length >= 0 ->
    proper_types:binary(Length).

-spec name() -> proper_types:type().
name() ->
    ?LET(
        NameStart,
        name_start(),
        ?LET(
            Characters,
            ?SIZED(
                Size,
                ?SUCHTHAT(
                    List,
                    ?LET(
                        NameContinue,
                        resize(Size, list(name_continue())),
                        proper_types:shrink_list([NameStart | NameContinue])
                    ),
                    List =/= [] andalso name_is_valid(List)
                )
            ),
            unicode:characters_to_binary(Characters)
        )
    ).

-spec name(Length :: pos_integer()) -> proper_types:type().
name(Length) when is_integer(Length) andalso Length >= 1 ->
    ?LET(
        {NameStart, NameContinue},
        {name_start(), vector(Length - 1, name_continue())},
        unicode:characters_to_binary([NameStart | NameContinue])
    ).

%% @private
-spec name_is_valid(Characters :: [char()]) -> boolean().
name_is_valid([C | _]) when
    (C >= $0 andalso C =< $9) orelse (C >= $A andalso C =< $Z) orelse (C =:= $_) orelse (C >= $a andalso C =< $z)
->
    true;
name_is_valid(_) ->
    false.

-compile({inline, [name_continue/0]}).
-spec name_continue() -> proper_types:type().
name_continue() ->
    oneof(lists:seq($0, $9) ++ lists:seq($A, $Z) ++ [$_] ++ lists:seq($a, $z)).

-compile({inline, [name_start/0]}).
-spec name_start() -> proper_types:type().
name_start() ->
    oneof(lists:seq($A, $Z) ++ [$_] ++ lists:seq($a, $z)).

-spec non_empty_vector(RawType :: proper_types:raw_type()) -> proper_types:type().
non_empty_vector(RawType) ->
    ?SIZED(Size, non_empty_vector_upto(max(1, Size), RawType)).

-spec non_empty_vector_upto(Length :: pos_integer() | 'inf', RawType :: proper_types:raw_type()) -> proper_types:type().
non_empty_vector_upto(Length, RawType) when (is_integer(Length) andalso Length >= 1) orelse Length =:= 'inf' ->
    ?LET(Size, integer(1, Length), vector(Size, RawType)).

-spec non_whitespace(RawType :: proper_types:raw_type()) -> proper_types:type().
non_whitespace(RawType) ->
    ?SUCHTHAT(Character, RawType, not ?is_whitespace(Character)).

-spec string_utf8() -> proper_types:type().
string_utf8() ->
    proper_unicode:utf8().

-spec string_character() -> proper_types:type().
string_character() ->
    ?LET(
        Character,
        unicode_codepoint_upto(4),
        string_character_escape(Character)
    ).

-spec string_character_escape(Character :: char()) -> proper_types:type().
string_character_escape(Character) ->
    case Character of
        $" ->
            exactly(<<$\\, $">>);
        $\\ ->
            exactly(<<$\\, $\\>>);
        $\b ->
            exactly(<<$\\, $b>>);
        $\f ->
            exactly(<<$\\, $f>>);
        $\n ->
            exactly(<<$\\, $n>>);
        $\r ->
            exactly(<<$\\, $r>>);
        $\t ->
            exactly(<<$\\, $t>>);
        $\/ ->
            exactly(<<$\\, $\/>>);
        _ when (Character band 16#80) =:= 16#00 andalso Character > 16#1F andalso Character =< 16#7F ->
            exactly(<<Character:8>>);
        _ ->
            ?LET(Legacy, boolean(), string_character_escaped_unicode(Character, Legacy))
    end.

%% @private
-spec string_character_escaped_unicode(HexDigitList :: <<_:8>> | <<_:16>> | <<_:24>> | <<_:32>> | <<_:40>> | <<_:48>>) ->
    proper_types:type().
string_character_escaped_unicode(<<A:8>>) ->
    ?LET(
        {Ax},
        {string_character_hex_digit(A)},
        <<Ax:8>>
    );
string_character_escaped_unicode(<<A:8, B:8>>) ->
    ?LET(
        {Ax, Bx},
        {string_character_hex_digit(A), string_character_hex_digit(B)},
        <<Ax:8, Bx:8>>
    );
string_character_escaped_unicode(<<A:8, B:8, C:8>>) ->
    ?LET(
        {Ax, Bx, Cx},
        {string_character_hex_digit(A), string_character_hex_digit(B), string_character_hex_digit(C)},
        <<Ax:8, Bx:8, Cx:8>>
    );
string_character_escaped_unicode(<<A:8, B:8, C:8, D:8>>) ->
    ?LET(
        {Ax, Bx, Cx, Dx},
        {
            string_character_hex_digit(A),
            string_character_hex_digit(B),
            string_character_hex_digit(C),
            string_character_hex_digit(D)
        },
        <<Ax:8, Bx:8, Cx:8, Dx:8>>
    );
string_character_escaped_unicode(<<A:8, B:8, C:8, D:8, E:8>>) ->
    ?LET(
        {Ax, Bx, Cx, Dx, Ex},
        {
            string_character_hex_digit(A),
            string_character_hex_digit(B),
            string_character_hex_digit(C),
            string_character_hex_digit(D),
            string_character_hex_digit(E)
        },
        <<Ax:8, Bx:8, Cx:8, Dx:8, Ex:8>>
    );
string_character_escaped_unicode(<<A:8, B:8, C:8, D:8, E:8, F:8>>) ->
    ?LET(
        {Ax, Bx, Cx, Dx, Ex, Fx},
        {
            string_character_hex_digit(A),
            string_character_hex_digit(B),
            string_character_hex_digit(C),
            string_character_hex_digit(D),
            string_character_hex_digit(E),
            string_character_hex_digit(F)
        },
        <<Ax:8, Bx:8, Cx:8, Dx:8, Ex:8, Fx:8>>
    ).

%% @private
-spec string_character_escaped_unicode(Character :: char(), Legacy :: boolean()) -> proper_types:type().
string_character_escaped_unicode(Character, Legacy) ->
    case Legacy of
        false ->
            ?LET(EscapedUnicode, string_character_escaped_unicode(integer_to_binary(Character, 16)), <<
                $\\, $u, ${, EscapedUnicode/bytes, $}
            >>);
        true ->
            case Character of
                _ when Character < 16#20 ->
                    ?LET(EscapedUnicode, string_character_escaped_unicode(binary:encode_hex(<<0:8, Character:8>>)), <<
                        $\\, $u, EscapedUnicode:4/bytes
                    >>);
                _ when Character =< 16#FFFF ->
                    ?LET(
                        EscapedUnicode,
                        string_character_escaped_unicode(
                            binary:encode_hex(<<(Character bsr 8):8, (Character band 16#FF):8>>)
                        ),
                        <<$\\, $u, EscapedUnicode:4/bytes>>
                    );
                _ ->
                    <<H1:8, H2:8, L1:8, L2:8>> = <<Character/utf16>>,
                    ?LET(
                        {FirstPart, SecondPart},
                        {
                            string_character_escaped_unicode(binary:encode_hex(<<H1:8, H2:8>>)),
                            string_character_escaped_unicode(binary:encode_hex(<<L1:8, L2:8>>))
                        },
                        <<$\\, $u, FirstPart:4/bytes, $\\, $u, SecondPart:4/bytes>>
                    )
            end
    end.

%% @private
-spec string_character_hex_digit(C :: char()) -> proper_types:type().
string_character_hex_digit(C) when C >= $0 andalso C =< $9 ->
    exactly(C);
string_character_hex_digit(C) ->
    ?LET(Case, oneof([lowercase, uppercase]), string_character_hex_digit(C, Case)).

%% @private
-spec string_character_hex_digit(C :: char(), Case :: lowercase | uppercase) -> char().
string_character_hex_digit(C, lowercase) when C >= $A andalso C =< $F ->
    (C - $A) + $a;
string_character_hex_digit(C, uppercase) when C >= $a andalso C =< $f ->
    (C - $a) + $A;
string_character_hex_digit(C, _) ->
    C.

-spec string_value() -> proper_types:type().
string_value() ->
    ?LET(Characters, list(string_character()), unicode:characters_to_binary(Characters)).

-spec string_value(Length :: non_neg_integer()) -> proper_types:type().
string_value(Length) when (is_integer(Length) andalso Length >= 0) ->
    ?LET(Characters, vector(Length, string_character()), unicode:characters_to_binary(Characters)).

-spec unicode_codepoint(1..4) -> proper_types:type().
unicode_codepoint(1) ->
    integer(0, 16#7F);
unicode_codepoint(2) ->
    integer(16#80, 16#7FF);
unicode_codepoint(3) ->
    union([integer(16#800, 16#D7FF), integer(16#E000, 16#FFFD)]);
unicode_codepoint(4) ->
    integer(16#10000, 16#10FFFF).

-spec unicode_codepoint_upto(1..4) -> proper_types:type().
unicode_codepoint_upto(N) when is_integer(N) andalso N >= 1 andalso N =< 4 ->
    union([unicode_codepoint(X) || X <- lists:seq(1, N)]).

-spec usize() -> proper_types:type().
usize() ->
    integer(16#0000000000000000, 16#FFFFFFFFFFFFFFFF).

-spec varint() -> proper_types:type().
varint() ->
    integer(-16#8000000000000000, 16#7FFFFFFFFFFFFFFF).

-spec vector_upto(Length :: non_neg_integer() | 'inf', RawType :: proper_types:raw_type()) -> proper_types:type().
vector_upto(Length, RawType) when (is_integer(Length) andalso Length >= 0) orelse Length =:= 'inf' ->
    ?LET(Size, integer(0, Length), vector(Size, RawType)).

%%%=============================================================================
%%% GraphQL Language API functions
%%%=============================================================================

-spec argument() -> proper_types:type().
argument() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {value, value()}
        ]),
        argo_graphql_language_argument:parse(Context, 0)
    ).

-spec argument_const() -> proper_types:type().
argument_const() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {value, value_const()}
        ]),
        argo_graphql_language_argument_const:parse(Context, 0)
    ).

-spec arguments() -> proper_types:type().
arguments() ->
    ?LET(
        Context,
        ?CONTEXT([
            {arguments, non_empty(list(argument()))}
        ]),
        argo_graphql_language_arguments:parse(Context, 0)
    ).

-spec arguments_const() -> proper_types:type().
arguments_const() ->
    ?LET(
        Context,
        ?CONTEXT([
            {arguments, non_empty(list(argument_const()))}
        ]),
        argo_graphql_language_arguments_const:parse(Context, 0)
    ).

-spec arguments_definition() -> proper_types:type().
arguments_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {inputs, complex(non_empty(list(input_value_definition())))}
        ]),
        argo_graphql_language_arguments_definition:parse(Context, 0)
    ).

-spec definition() -> proper_types:type().
definition() ->
    oneof([
        ?LET(
            ExecutableDefinition,
            executable_definition(),
            argo_graphql_language_definition:executable_definition(ExecutableDefinition, 0)
        ),
        ?LET(
            TypeSystemDefinition,
            type_system_definition(),
            argo_graphql_language_definition:type_system_definition(TypeSystemDefinition, 0)
        ),
        ?LET(
            TypeSystemExtension,
            type_system_extension(),
            argo_graphql_language_definition:type_system_extension(TypeSystemExtension, 0)
        )
    ]).

-spec definition(PreviousDefinition :: argo_graphql_language_definition:t()) -> proper_types:t().
definition(PreviousDefinition) ->
    case argo_graphql_language_definition:is_ambiguous(PreviousDefinition) of
        false ->
            definition();
        true ->
            ?SUCHTHAT(NextDefinition, definition(), not argo_graphql_language_definition:is_shorthand(NextDefinition))
    end.

-spec definitions() -> proper_types:type().
definitions() ->
    complex(
        ?LET(
            InitialDefinition,
            ?LAZY(definition()),
            ?SUCHTHAT(
                Definitions,
                ?LET(
                    List,
                    ?SIZED(Size, proper_types:noshrink(definitions(Size, InitialDefinition, 1))),
                    proper_types:shrink_list(List)
                ),
                Definitions =/= [] andalso definitions_is_valid(Definitions)
            )
        )
    ).

%% @private
-spec definitions(Size :: integer(), PreviousDefinition :: argo_graphql_language_definition:t(), Count :: integer()) ->
    proper_types:type().
definitions(Size, PreviousDefinition, Count) ->
    ?LAZY(
        frequency([
            {1, [PreviousDefinition]},
            {Size,
                ?LET(
                    NextDefinition,
                    definition(PreviousDefinition),
                    ?LET(Definitions, definitions(Size - 1, NextDefinition, Count + 1), [
                        PreviousDefinition | Definitions
                    ])
                )}
        ])
    ).

%% @private
-spec definitions_is_valid(Definitions :: [argo_graphql_language_definition:t()]) -> boolean().
definitions_is_valid([A, B | Rest]) ->
    case argo_graphql_language_definition:is_ambiguous(A) andalso argo_graphql_language_definition:is_shorthand(B) of
        true ->
            % Ambiguous definition cannot be followed by a Shorthand definition.
            false;
        _ ->
            definitions_is_valid([B | Rest])
    end;
definitions_is_valid([_]) ->
    true;
definitions_is_valid([]) ->
    true.

-spec directive() -> proper_types:type().
directive() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {arguments, option(arguments())}
        ]),
        argo_graphql_language_directive:parse(Context, 0)
    ).

-spec directive_const() -> proper_types:type().
directive_const() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {arguments, option(arguments_const())}
        ]),
        argo_graphql_language_directive_const:parse(Context, 0)
    ).

-spec directive_definition() -> proper_types:type().
directive_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {locations, directive_locations()},
            {description, option(block_string_value())},
            {arguments, option(complex(arguments_definition()))},
            {repeatable, option(boolean())}
        ]),
        argo_graphql_language_directive_definition:parse(Context, 0)
    ).

-spec directive_location() -> proper_types:type().
directive_location() ->
    oneof([
        ?LET(
            ExecutableDirectiveLocation,
            executable_directive_location(),
            argo_graphql_language_directive_location:executable_directive_location(ExecutableDirectiveLocation, 0)
        ),
        ?LET(
            TypeSystemDirectiveLocation,
            type_system_directive_location(),
            argo_graphql_language_directive_location:type_system_directive_location(TypeSystemDirectiveLocation, 0)
        )
    ]).

-spec directive_locations() -> proper_types:type().
directive_locations() ->
    non_empty(list(directive_location())).

-spec directives() -> proper_types:type().
directives() ->
    ?LET(
        Context,
        ?CONTEXT([
            {directives, non_empty(list(directive()))}
        ]),
        argo_graphql_language_directives:parse(Context, 0)
    ).

-spec directives_const() -> proper_types:type().
directives_const() ->
    ?LET(
        Context,
        ?CONTEXT([
            {directives, non_empty(list(directive_const()))}
        ]),
        argo_graphql_language_directives_const:parse(Context, 0)
    ).

-spec document() -> proper_types:type().
document() ->
    ?LET(
        Context,
        ?CONTEXT([
            {definitions, definitions()}
        ]),
        argo_graphql_language_document:parse(Context, 0)
    ).

-spec enum_type_definition() -> proper_types:type().
enum_type_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {description, option(block_string_value())},
            {directives, option(complex(directives_const()))},
            {values, option(complex(enum_values_definition()))}
        ]),
        argo_graphql_language_enum_type_definition:parse(Context, 0)
    ).

-spec enum_type_extension() -> proper_types:type().
enum_type_extension() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {name, name()},
                {directives, option(complex(directives_const()))},
                {values, option(complex(enum_values_definition()))}
            ]),
            is_map_key(directives, Context) orelse is_map_key(values, Context)
        ),
        argo_graphql_language_enum_type_extension:parse(Context, 0)
    ).

-spec enum_value() -> proper_types:type().
enum_value() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()}
        ]),
        argo_graphql_language_enum_value:parse(Context, 0)
    ).

-spec enum_value_definition() -> proper_types:type().
enum_value_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {value, enum_value()},
            {description, option(block_string_value())},
            {directives, option(complex(directives_const()))}
        ]),
        argo_graphql_language_enum_value_definition:parse(Context, 0)
    ).

-spec enum_values_definition() -> proper_types:type().
enum_values_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {values, complex(non_empty(list(enum_value_definition())))}
        ]),
        argo_graphql_language_enum_values_definition:parse(Context, 0)
    ).

-spec executable_definition() -> proper_types:type().
executable_definition() ->
    oneof([
        ?LET(
            OperationDefinition,
            operation_definition(),
            argo_graphql_language_executable_definition:operation_definition(OperationDefinition, 0)
        ),
        ?LET(
            FragmentDefinition,
            fragment_definition(),
            argo_graphql_language_executable_definition:fragment_definition(FragmentDefinition, 0)
        )
    ]).

-spec executable_directive_location() -> proper_types:type().
executable_directive_location() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, executable_directive_location_name()}
        ]),
        argo_graphql_language_executable_directive_location:parse(Context, 0)
    ).

%% @private
-spec executable_directive_location_name() -> proper_types:type().
executable_directive_location_name() ->
    oneof([
        'QUERY',
        'MUTATION',
        'SUBSCRIPTION',
        'FIELD',
        'FRAGMENT_DEFINITION',
        'FRAGMENT_SPREAD',
        'INLINE_FRAGMENT',
        'VARIABLE_DEFINITION'
    ]).

-spec field() -> proper_types:type().
field() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {'alias', option(name())},
            {arguments, option(arguments())},
            {directives, option(directives())},
            {selection_set, option(complex(selection_set()))}
        ]),
        argo_graphql_language_field:parse(Context, 0)
    ).

-spec field_definition() -> proper_types:type().
field_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {type, type()},
            {description, option(block_string_value())},
            {arguments, option(complex(arguments_definition()))},
            {directives, option(complex(directives_const()))}
        ]),
        argo_graphql_language_field_definition:parse(Context, 0)
    ).

-spec fields_definition() -> proper_types:type().
fields_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {fields, complex(list(field_definition()))}
        ]),
        argo_graphql_language_fields_definition:parse(Context, 0)
    ).

-spec fragment_definition() -> proper_types:type().
fragment_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, fragment_name()},
            {type_condition, type_condition()},
            {selection_set, selection_set()},
            {directives, option(directives())}
        ]),
        argo_graphql_language_fragment_definition:parse(Context, 0)
    ).

-spec fragment_name() -> proper_types:type().
fragment_name() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, ?SUCHTHAT(Name, name(), Name =/= <<"on">>)}
        ]),
        argo_graphql_language_fragment_name:parse(Context, 0)
    ).

-spec fragment_spread() -> proper_types:type().
fragment_spread() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, fragment_name()},
            {directives, option(directives())}
        ]),
        argo_graphql_language_fragment_spread:parse(Context, 0)
    ).

-spec implements_interfaces() -> proper_types:type().
implements_interfaces() ->
    ?LET(
        Context,
        ?CONTEXT([
            {interfaces, non_empty(list(named_type()))}
        ]),
        argo_graphql_language_implements_interfaces:parse(Context, 0)
    ).

-spec inline_fragment() -> proper_types:type().
inline_fragment() ->
    ?LET(
        Context,
        ?CONTEXT([
            {selection_set, selection_set()},
            {type_condition, option(type_condition())},
            {directives, option(directives())}
        ]),
        argo_graphql_language_inline_fragment:parse(Context, 0)
    ).

-spec input_fields_definition() -> proper_types:type().
input_fields_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {inputs, complex(list(input_value_definition()))}
        ]),
        argo_graphql_language_input_fields_definition:parse(Context, 0)
    ).

-spec input_object_type_definition() -> proper_types:type().
input_object_type_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {description, option(block_string_value())},
            {directives, option(directives_const())},
            {fields, option(input_fields_definition())}
        ]),
        argo_graphql_language_input_object_type_definition:parse(Context, 0)
    ).

-spec input_object_type_extension() -> proper_types:type().
input_object_type_extension() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {name, name()},
                {directives, option(complex(directives_const()))},
                {fields, option(complex(input_fields_definition()))}
            ]),
            is_map_key(directives, Context) orelse is_map_key(fields, Context)
        ),
        argo_graphql_language_input_object_type_extension:parse(Context, 0)
    ).

-spec input_value_definition() -> proper_types:type().
input_value_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {type, type()},
            {description, option(block_string_value())},
            {directives, option(complex(directives_const()))},
            {default_value, option(complex(value_const()))}
        ]),
        argo_graphql_language_input_value_definition:parse(Context, 0)
    ).

-spec interface_type_definition() -> proper_types:type().
interface_type_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {description, option(block_string_value())},
            {implements, option(implements_interfaces())},
            {directives, option(complex(directives_const()))},
            {fields, option(complex(fields_definition()))}
        ]),
        argo_graphql_language_interface_type_definition:parse(Context, 0)
    ).

-spec interface_type_extension() -> proper_types:type().
interface_type_extension() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {name, name()},
                {implements, option(implements_interfaces())},
                {directives, option(directives_const())},
                {fields, option(fields_definition())}
            ]),
            is_map_key(implements, Context) orelse is_map_key(directives, Context) orelse is_map_key(fields, Context)
        ),
        argo_graphql_language_interface_type_extension:parse(Context, 0)
    ).

-spec list_type() -> proper_types:type().
list_type() ->
    ?LET(
        Context,
        ?CONTEXT([
            {type, type()}
        ]),
        argo_graphql_language_list_type:parse(Context, 0)
    ).

-spec list_value() -> proper_types:type().
list_value() ->
    ?LET(
        Context,
        ?CONTEXT([
            {list, complex(list(?LAZY(value())))}
        ]),
        argo_graphql_language_list_value:parse(Context, 0)
    ).

-spec list_value_const() -> proper_types:type().
list_value_const() ->
    ?LET(
        Context,
        ?CONTEXT([
            {list, complex(list(?LAZY(value_const())))}
        ]),
        argo_graphql_language_list_value_const:parse(Context, 0)
    ).

-spec named_type() -> proper_types:type().
named_type() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()}
        ]),
        argo_graphql_language_named_type:parse(Context, 0)
    ).

-spec non_null_type() -> proper_types:type().
non_null_type() ->
    ?LET(
        Context,
        ?CONTEXT([
            {type, oneof([named_type(), list_type()])}
        ]),
        argo_graphql_language_non_null_type:parse(Context, 0)
    ).

-spec object_field() -> proper_types:type().
object_field() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {value, value()}
        ]),
        argo_graphql_language_object_field:parse(Context, 0)
    ).

-spec object_field_const() -> proper_types:type().
object_field_const() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {value, value_const()}
        ]),
        argo_graphql_language_object_field_const:parse(Context, 0)
    ).

-spec object_type_definition() -> proper_types:type().
object_type_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {description, option(block_string_value())},
            {implements, option(implements_interfaces())},
            {directives, option(directives_const())},
            {fields, option(fields_definition())}
        ]),
        argo_graphql_language_object_type_definition:parse(Context, 0)
    ).

-spec object_type_extension() -> proper_types:type().
object_type_extension() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {name, name()},
                {implements, option(implements_interfaces())},
                {directives, option(directives_const())},
                {fields, option(fields_definition())}
            ]),
            is_map_key(implements, Context) orelse is_map_key(directives, Context) orelse is_map_key(fields, Context)
        ),
        argo_graphql_language_object_type_extension:parse(Context, 0)
    ).

-spec object_value() -> proper_types:type().
object_value() ->
    ?LET(
        Context,
        ?CONTEXT([
            {fields, complex(list(?LAZY(object_field())))}
        ]),
        argo_graphql_language_object_value:parse(Context, 0)
    ).

-spec object_value_const() -> proper_types:type().
object_value_const() ->
    ?LET(
        Context,
        ?CONTEXT([
            {fields, complex(list(?LAZY(object_field_const())))}
        ]),
        argo_graphql_language_object_value_const:parse(Context, 0)
    ).

-spec operation_definition() -> proper_types:type().
operation_definition() ->
    ?LET(
        Context,
        oneof([
            ?CONTEXT([
                {operation, exactly('query')},
                {shorthand, exactly(true)},
                {selection_set, selection_set()}
            ]),
            ?CONTEXT([
                {operation, operation_type()},
                {selection_set, selection_set()},
                {name, option(name())},
                {variables_definition, option(variables_definition())},
                {directives, option(directives())}
            ])
        ]),
        argo_graphql_language_operation_definition:parse(Context, 0)
    ).

-spec operation_type() -> proper_types:type().
operation_type() ->
    oneof(['query', 'mutation', 'subscription']).

-spec root_operation_type_definition() -> proper_types:type().
root_operation_type_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {operation_type, operation_type()},
            {named_type, named_type()}
        ]),
        argo_graphql_language_root_operation_type_definition:parse(Context, 0)
    ).

-spec root_operation_types_definition() -> proper_types:type().
root_operation_types_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {operations, complex(list(root_operation_type_definition()))}
        ]),
        argo_graphql_language_root_operation_types_definition:parse(Context, 0)
    ).

-spec scalar_type_definition() -> proper_types:type().
scalar_type_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {description, option(block_string_value())},
            {directives, option(directives_const())}
        ]),
        argo_graphql_language_scalar_type_definition:parse(Context, 0)
    ).

-spec scalar_type_extension() -> proper_types:type().
scalar_type_extension() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {name, name()},
                {directives, option(directives_const())}
            ]),
            is_map_key(directives, Context)
        ),
        argo_graphql_language_scalar_type_extension:parse(Context, 0)
    ).

-spec schema_definition() -> proper_types:type().
schema_definition() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {description, option(block_string_value())},
                {directives, option(directives_const())},
                {operations, option(root_operation_types_definition())}
            ]),
            is_map_key(directives, Context) orelse is_map_key(operations, Context)
        ),
        argo_graphql_language_schema_definition:parse(Context, 0)
    ).

-spec schema_extension() -> proper_types:type().
schema_extension() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {directives, option(directives_const())},
                {operations, option(root_operation_types_definition())}
            ]),
            is_map_key(directives, Context) orelse is_map_key(operations, Context)
        ),
        argo_graphql_language_schema_extension:parse(Context, 0)
    ).

-spec selection() -> proper_types:type().
selection() ->
    ?LAZY(
        oneof([
            ?LET(Field, field(), argo_graphql_language_selection:field(Field, 0)),
            ?LET(FragmentSpread, fragment_spread(), argo_graphql_language_selection:fragment_spread(FragmentSpread, 0)),
            ?LET(InlineFragment, inline_fragment(), argo_graphql_language_selection:inline_fragment(InlineFragment, 0))
        ])
    ).

-spec selection_set() -> proper_types:type().
selection_set() ->
    ?LET(
        Context,
        ?CONTEXT([
            {selections, complex(non_empty(list(selection())))}
        ]),
        argo_graphql_language_selection_set:parse(Context, 0)
    ).

-spec type() -> proper_types:type().
type() ->
    oneof([
        ?LET(NamedType, named_type(), argo_graphql_language_type:named_type(NamedType, 0)),
        ?LAZY(?LET(NonNullType, non_null_type(), argo_graphql_language_type:non_null_type(NonNullType, 0))),
        ?LAZY(?LET(ListType, list_type(), argo_graphql_language_type:list_type(ListType, 0)))
    ]).

-spec type_condition() -> proper_types:type().
type_condition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {type, named_type()}
        ]),
        argo_graphql_language_type_condition:parse(Context, 0)
    ).

-spec type_definition() -> proper_types:type().
type_definition() ->
    oneof([
        ?LET(
            ScalarTypeDefinition,
            scalar_type_definition(),
            argo_graphql_language_type_definition:scalar_type_definition(ScalarTypeDefinition, 0)
        ),
        ?LET(
            ObjectTypeDefinition,
            object_type_definition(),
            argo_graphql_language_type_definition:object_type_definition(ObjectTypeDefinition, 0)
        ),
        ?LET(
            InterfaceTypeDefinition,
            interface_type_definition(),
            argo_graphql_language_type_definition:interface_type_definition(InterfaceTypeDefinition, 0)
        ),
        ?LET(
            UnionTypeDefinition,
            union_type_definition(),
            argo_graphql_language_type_definition:union_type_definition(UnionTypeDefinition, 0)
        ),
        ?LET(
            EnumTypeDefinition,
            enum_type_definition(),
            argo_graphql_language_type_definition:enum_type_definition(EnumTypeDefinition, 0)
        ),
        ?LET(
            InputObjectTypeDefinition,
            input_object_type_definition(),
            argo_graphql_language_type_definition:input_object_type_definition(InputObjectTypeDefinition, 0)
        )
    ]).

-spec type_extension() -> proper_types:type().
type_extension() ->
    oneof([
        ?LET(
            ScalarTypeExtension,
            scalar_type_extension(),
            argo_graphql_language_type_extension:scalar_type_extension(ScalarTypeExtension, 0)
        ),
        ?LET(
            ObjectTypeExtension,
            object_type_extension(),
            argo_graphql_language_type_extension:object_type_extension(ObjectTypeExtension, 0)
        ),
        ?LET(
            InterfaceTypeExtension,
            interface_type_extension(),
            argo_graphql_language_type_extension:interface_type_extension(InterfaceTypeExtension, 0)
        ),
        ?LET(
            UnionTypeExtension,
            union_type_extension(),
            argo_graphql_language_type_extension:union_type_extension(UnionTypeExtension, 0)
        ),
        ?LET(
            EnumTypeExtension,
            enum_type_extension(),
            argo_graphql_language_type_extension:enum_type_extension(EnumTypeExtension, 0)
        ),
        ?LET(
            InputObjectTypeExtension,
            input_object_type_extension(),
            argo_graphql_language_type_extension:input_object_type_extension(InputObjectTypeExtension, 0)
        )
    ]).

-spec type_system_definition() -> proper_types:type().
type_system_definition() ->
    oneof([
        ?LET(
            SchemaDefinition,
            schema_definition(),
            argo_graphql_language_type_system_definition:schema_definition(SchemaDefinition, 0)
        ),
        ?LET(
            TypeDefinition,
            type_definition(),
            argo_graphql_language_type_system_definition:type_definition(TypeDefinition, 0)
        ),
        ?LET(
            DirectiveDefinition,
            directive_definition(),
            argo_graphql_language_type_system_definition:directive_definition(DirectiveDefinition, 0)
        )
    ]).

-spec type_system_directive_location() -> proper_types:type().
type_system_directive_location() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, type_system_directive_location_name()}
        ]),
        argo_graphql_language_type_system_directive_location:parse(Context, 0)
    ).

%% @private
-spec type_system_directive_location_name() -> proper_types:type().
type_system_directive_location_name() ->
    oneof([
        'SCHEMA',
        'SCALAR',
        'OBJECT',
        'FIELD_DEFINITION',
        'ARGUMENT_DEFINITION',
        'INTERFACE',
        'UNION',
        'ENUM',
        'ENUM_VALUE',
        'INPUT_OBJECT',
        'INPUT_FIELD_DEFINITION'
    ]).

-spec type_system_extension() -> proper_types:type().
type_system_extension() ->
    oneof([
        ?LET(
            SchemaExtension,
            schema_extension(),
            argo_graphql_language_type_system_extension:schema_extension(SchemaExtension, 0)
        ),
        ?LET(
            TypeExtension,
            type_extension(),
            argo_graphql_language_type_system_extension:type_extension(TypeExtension, 0)
        )
    ]).

-spec union_member_types() -> proper_types:type().
union_member_types() ->
    non_empty(list(named_type())).

-spec union_type_definition() -> proper_types:type().
union_type_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()},
            {description, option(block_string_value())},
            {directives, option(directives_const())},
            {types, option(union_member_types())}
        ]),
        argo_graphql_language_union_type_definition:parse(Context, 0)
    ).

-spec union_type_extension() -> proper_types:type().
union_type_extension() ->
    ?LET(
        Context,
        ?SUCHTHAT(
            Context,
            ?CONTEXT([
                {name, name()},
                {directives, option(directives_const())},
                {types, option(union_member_types())}
            ]),
            is_map_key(directives, Context) orelse is_map_key(types, Context)
        ),
        argo_graphql_language_union_type_extension:parse(Context, 0)
    ).

-spec value() -> proper_types:type().
value() ->
    oneof([
        ?LET(Variable, variable(), argo_graphql_language_value:variable(Variable, 0)),
        argo_graphql_language_value:null(0),
        ?LET(Float, float(), argo_graphql_language_value:float(Float, 0)),
        ?LET(Int, int(), argo_graphql_language_value:int(Int, 0)),
        ?LET(String, string_value(), argo_graphql_language_value:string(String, 0)),
        ?LET(Boolean, boolean(), argo_graphql_language_value:boolean(Boolean, 0)),
        ?LET(EnumValue, enum_value(), argo_graphql_language_value:enum(EnumValue, 0)),
        ?LET(ListValue, list_value(), argo_graphql_language_value:list(ListValue, 0)),
        ?LET(ObjectValue, object_value(), argo_graphql_language_value:object(ObjectValue, 0))
    ]).

-spec value_const() -> proper_types:type().
value_const() ->
    oneof([
        argo_graphql_language_value_const:null(0),
        ?LET(Float, float(), argo_graphql_language_value_const:float(Float, 0)),
        ?LET(Int, int(), argo_graphql_language_value_const:int(Int, 0)),
        ?LET(String, string_value(), argo_graphql_language_value_const:string(String, 0)),
        ?LET(Boolean, boolean(), argo_graphql_language_value_const:boolean(Boolean, 0)),
        ?LET(EnumValue, enum_value(), argo_graphql_language_value_const:enum(EnumValue, 0)),
        ?LET(ListValueConst, list_value_const(), argo_graphql_language_value_const:list(ListValueConst, 0)),
        ?LET(ObjectValueConst, object_value_const(), argo_graphql_language_value_const:object(ObjectValueConst, 0))
    ]).

-spec variable() -> proper_types:type().
variable() ->
    ?LET(
        Context,
        ?CONTEXT([
            {name, name()}
        ]),
        argo_graphql_language_variable:parse(Context, 0)
    ).

-spec variable_definition() -> proper_types:type().
variable_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {variable, variable()},
            {type, type()},
            {default_value, option(value_const())},
            {directives, option(directives_const())}
        ]),
        argo_graphql_language_variable_definition:parse(Context, 0)
    ).

-spec variables_definition() -> proper_types:type().
variables_definition() ->
    ?LET(
        Context,
        ?CONTEXT([
            {variables, complex(non_empty(list(variable_definition())))}
        ]),
        argo_graphql_language_variables_definition:parse(Context, 0)
    ).
