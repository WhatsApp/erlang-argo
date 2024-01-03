%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc GraphQL Lexer
%%%
%%% See the spec reference
%%% https://spec.graphql.org/draft/#sec-Appendix-Grammar-Summary
%%%
%%% @end
%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format

Definitions.

% Ignored tokens
WhiteSpace          = [\x{0009}\x{000B}\x{000C}\x{0020}\x{00A0}]
_LineTerminator     = \x{000A}\x{000D}\x{2028}\x{2029}
LineTerminator      = [{_LineTerminator}]
Comment             = #[^{_LineTerminator}]*
Comma               = ,
Ignored             = {WhiteSpace}|{LineTerminator}|{Comment}|{Comma}

% Lexical tokens
Punctuator          = [&!$():=@\[\]{|}]|\.\.\.
Name                = [_A-Za-z][_0-9A-Za-z]*

% Int Value
Digit               = [0-9]
NonZeroDigit        = [1-9]
NegativeSign        = -
IntegerPart         = {NegativeSign}?(0|{NonZeroDigit}{Digit}*)
IntValue            = {IntegerPart}

% Float Value
FractionalPart      = \.{Digit}+
Sign                = [+\-]
ExponentIndicator   = [eE]
ExponentPart        = {ExponentIndicator}{Sign}?{Digit}+
FloatValue          = {IntegerPart}{FractionalPart}|{IntegerPart}{ExponentPart}|{IntegerPart}{FractionalPart}{ExponentPart}

% String Value
HexDigit            = [0-9A-Fa-f]
EscapedUnicode      = u{HexDigit}{HexDigit}{HexDigit}{HexDigit}
EscapedCharacter    = ["\\\/bfnrt]
StringCharacter     = ([^\"{_LineTerminator}]|\\{EscapedUnicode}|\\{EscapedCharacter})
StringValue         = "{StringCharacter}*"
MultiCharacter      = ([^"]|"[^"]|""[^"]|\\{EscapedUnicode}|\\{EscapedCharacter})
MultiStringValue    = """{MultiCharacter}*"""

Rules.

{Ignored}          : skip_token.
{Punctuator}       : {token, {list_to_atom(TokenChars), {TokenLine, TokenCol}}}.
{IntValue}         : {token, {int_value, {TokenLine, TokenCol}, list_to_integer(TokenChars)}}.
{FloatValue}       : {token, {float_value, {TokenLine, TokenCol}, list_to_float(TokenChars)}}.
{StringValue}      : {token, {string_value, {TokenLine, TokenCol}, TokenChars}}.
{MultiStringValue} : {token, {block_string_value, {TokenLine, TokenCol}, TokenChars}}.
{Name}             : {token, identifier(TokenChars, {TokenLine, TokenCol})}.

Erlang code.

%% Quell a warning in the leex header file for yyrev/2 which our current generated code
%% will never call.
-dialyzer({nowarn_function, yyrev/2}).

identifier("true", TokenLoc) -> {boolean_value, TokenLoc, true};
identifier("false", TokenLoc) -> {boolean_value, TokenLoc, false};
identifier("query", TokenLoc) -> {query, TokenLoc};
identifier("mutation", TokenLoc) -> {mutation, TokenLoc};
identifier("subscription", TokenLoc) -> {subscription, TokenLoc};
identifier("fragment", TokenLoc) -> {fragment, TokenLoc};
identifier("repeatable", TokenLoc) -> {repeatable, TokenLoc};
identifier("on", TokenLoc) -> {on, TokenLoc};
identifier("null", TokenLoc) -> {null, TokenLoc};
identifier("scalar", TokenLoc) -> {scalar, TokenLoc};
identifier("enum", TokenLoc) -> {enum, TokenLoc};
identifier("type", TokenLoc) -> {type, TokenLoc};
identifier("input", TokenLoc) -> {input, TokenLoc};
identifier("implements", TokenLoc) -> {implements, TokenLoc};
identifier("interface", TokenLoc) -> {interface, TokenLoc};
identifier("union", TokenLoc) -> {union, TokenLoc};
identifier("extend", TokenLoc) -> {extend, TokenLoc};
identifier("schema", TokenLoc) -> {schema, TokenLoc};
identifier("directive", TokenLoc) -> {directive, TokenLoc};
identifier(ID = "QUERY", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "MUTATION", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "SUBSCRIPTION", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "FIELD", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "FRAGMENT_DEFINITION", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "FRAGMENT_SPREAD", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "INLINE_FRAGMENT", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "VARIABLE_DEFINITION", TokenLoc) -> {executable_directive_location, TokenLoc, ID};
identifier(ID = "SCHEMA", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "SCALAR", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "OBJECT", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "FIELD_DEFINITION", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "ARGUMENT_DEFINITION", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "INTERFACE", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "UNION", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "ENUM", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "ENUM_VALUE", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "INPUT_OBJECT", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};
identifier(ID = "INPUT_FIELD_DEFINITION", TokenLoc) -> {type_system_directive_location, TokenLoc, ID};

identifier(ID, TokenLoc) -> {name, TokenLoc, ID}.
