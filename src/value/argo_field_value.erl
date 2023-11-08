%%% % @format
-module(argo_field_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_value.hrl").

%% New API
-export([
    optional/2,
    required/2
]).

%% Instance API
-export([
    fetch/1,
    is_absent/1,
    is_optional/1,
    is_present/1,
    is_required/1
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

-spec optional(Name, none | {some, Value}) -> FieldValue when
    Name :: argo_types:name(), Value :: argo_value:t(), FieldValue :: t().
optional(Name, none) when is_binary(Name) ->
    #argo_field_value{name = Name, inner = {optional, none}};
optional(Name, {some, Value = #argo_value{}}) when is_binary(Name) ->
    #argo_field_value{name = Name, inner = {optional, {some, Value}}}.

-spec required(Name, Value) -> FieldValue when Name :: argo_types:name(), Value :: argo_value:t(), FieldValue :: t().
required(Name, Value = #argo_value{}) when is_binary(Name) ->
    #argo_field_value{name = Name, inner = {required, Value}}.

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
