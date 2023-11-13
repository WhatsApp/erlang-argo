%%% % @format
-module(argo_core).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").

%% New API
-export([
    absent/0,
    backreference/1,
    error/0,
    length/1,
    non_null/0,
    null/0
]).

%% Instance API
-export([
    is_absent/1,
    is_backreference/1,
    is_error/1,
    is_length/1,
    is_non_null/1,
    is_null/1
]).

%% Types
-type labeled_type() :: {backreference, argo_types:backreference()} | {length, argo_types:length()}.
-type nullable_type() :: null | non_null | error.
-type omittable_type() :: absent | non_null.

-export_type([
    labeled_type/0,
    nullable_type/0,
    omittable_type/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec absent() -> OmittableType when OmittableType :: omittable_type().
absent() ->
    absent.

-spec backreference(Backreference) -> LabeledType when
    Backreference :: argo_types:backreference(), LabeledType :: labeled_type().
backreference(Backreference) when ?is_u32(Backreference) ->
    {backreference, Backreference}.

-spec error() -> NullableType when NullableType :: nullable_type().
error() ->
    error.

-spec length(Length) -> LabeledType when Length :: argo_types:length(), LabeledType :: labeled_type().
length(Length) when ?is_u32(Length) ->
    {length, Length}.

-spec non_null() -> NullableType | OmittableType when
    NullableType :: nullable_type(), OmittableType :: omittable_type().
non_null() ->
    non_null.

-spec null() -> OmittableType when OmittableType :: omittable_type().
null() ->
    null.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec is_absent(OmittableType) -> boolean() when OmittableType :: omittable_type().
is_absent(absent) -> true;
is_absent(_) -> false.

-spec is_backreference(LabeledType) -> boolean() when LabeledType :: labeled_type().
is_backreference({backreference, _}) -> true;
is_backreference(_) -> false.

-spec is_error(NullableType) -> boolean() when NullableType :: nullable_type().
is_error(error) -> true;
is_error(_) -> false.

-spec is_length(LabeledType) -> boolean() when LabeledType :: labeled_type().
is_length({length, _}) -> true;
is_length(_) -> false.

-spec is_non_null(NullableType | OmittableType) -> boolean() when
    NullableType :: nullable_type(), OmittableType :: omittable_type().
is_non_null(non_null) -> true;
is_non_null(_) -> false.

-spec is_null(NullableType) -> boolean() when NullableType :: nullable_type().
is_null(non_null) -> true;
is_null(_) -> false.
