%%% % @format
-module(argo_nullable_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_value.hrl").

%% New API
-export([
    null/0,
    non_null/1,
    field_errors/1
]).

%% Instance API
-export([
    is_null/1,
    is_non_null/1,
    is_field_errors/1
]).

%% Types
-type inner() :: null | {non_null, argo_value:t()} | {field_errors, [argo_error_value:t()]}.
-type t() :: #argo_nullable_value{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec null() -> NullableValue when NullableValue :: t().
null() ->
    #argo_nullable_value{inner = null}.

-spec non_null(Value) -> NullableValue when Value :: argo_value:t(), NullableValue :: t().
non_null(Value = #argo_value{}) ->
    #argo_nullable_value{inner = {non_null, Value}}.

-spec field_errors(FieldErrors) -> NullableValue when FieldErrors :: [argo_error_value:t()], NullableValue :: t().
field_errors(FieldErrors) when is_list(FieldErrors) ->
    #argo_nullable_value{inner = {field_errors, FieldErrors}}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec is_null(NullableValue) -> boolean() when NullableValue :: t().
is_null(#argo_nullable_value{inner = null}) -> true;
is_null(#argo_nullable_value{}) -> false.

-spec is_non_null(NullableValue) -> boolean() when NullableValue :: t().
is_non_null(#argo_nullable_value{inner = {non_null, _}}) -> true;
is_non_null(#argo_nullable_value{}) -> false.

-spec is_field_errors(NullableValue) -> boolean() when NullableValue :: t().
is_field_errors(#argo_nullable_value{inner = {field_errors, _}}) -> true;
is_field_errors(#argo_nullable_value{}) -> false.
