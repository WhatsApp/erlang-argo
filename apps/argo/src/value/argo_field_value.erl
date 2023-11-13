%%% % @format
-module(argo_field_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% New API
-export([
    optional/2,
    required/2
]).

%% Instance API
-export([
    fetch/1,
    is_absent/1,
    is_labeled/1,
    is_optional/1,
    is_present/1,
    is_required/1,
    name/1,
    to_field_wire_type/1
]).

%% Types
-type inner() :: {optional, none | {some, argo_value:t()}} | {required, argo_value:t()}.
-type t() :: #argo_field_value{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec optional(FieldWireType, none | {some, Value}) -> FieldValue when
    FieldWireType :: argo_field_wire_type:t(), Value :: argo_value:t(), FieldValue :: t().
optional(FieldWireType = #argo_field_wire_type{}, none) ->
    #argo_field_value{wire_type = FieldWireType, inner = {optional, none}};
optional(FieldWireType = #argo_field_wire_type{}, {some, Value = #argo_value{}}) ->
    #argo_field_value{wire_type = FieldWireType, inner = {optional, {some, Value}}}.

-spec required(FieldWireType, Value) -> FieldValue when
    FieldWireType :: argo_field_wire_type:t(), Value :: argo_value:t(), FieldValue :: t().
required(FieldWireType = #argo_field_wire_type{}, Value = #argo_value{}) ->
    #argo_field_value{wire_type = FieldWireType, inner = {required, Value}}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec fetch(FieldValue) -> {ok, Value} | error when FieldValue :: t(), Value :: argo_value:t().
fetch(#argo_field_value{inner = {optional, none}}) ->
    error;
fetch(#argo_field_value{inner = {optional, {some, Value}}}) ->
    {ok, Value};
fetch(#argo_field_value{inner = {required, Value}}) ->
    {ok, Value}.

-spec is_absent(FieldValue) -> boolean() when FieldValue :: t().
is_absent(#argo_field_value{inner = {optional, none}}) -> true;
is_absent(#argo_field_value{}) -> false.

-compile({inline, [is_labeled/1]}).
-spec is_labeled(FieldValue) -> boolean() when FieldValue :: t().
is_labeled(#argo_field_value{wire_type = FieldWireType = #argo_field_wire_type{}}) ->
    argo_field_wire_type:is_labeled(FieldWireType).

-spec is_optional(FieldValue) -> boolean() when FieldValue :: t().
is_optional(#argo_field_value{inner = {optional, _}}) -> true;
is_optional(#argo_field_value{}) -> false.

-spec is_present(FieldValue) -> boolean() when FieldValue :: t().
is_present(#argo_field_value{inner = {optional, {some, _}}}) -> true;
is_present(#argo_field_value{inner = {required, _}}) -> true;
is_present(#argo_field_value{}) -> false.

-spec is_required(FieldValue) -> boolean() when FieldValue :: t().
is_required(#argo_field_value{inner = {required, _}}) -> true;
is_required(#argo_field_value{}) -> false.

-compile({inline, [name/1]}).
-spec name(FieldValue) -> Name when FieldValue :: t(), Name :: argo_types:name().
name(#argo_field_value{wire_type = #argo_field_wire_type{name = Name}}) ->
    Name.

-spec to_field_wire_type(FieldValue) -> FieldWireType when FieldValue :: t(), FieldWireType :: argo_field_wire_type:t().
to_field_wire_type(#argo_field_value{wire_type = FieldWireType = #argo_field_wire_type{}}) ->
    FieldWireType.
