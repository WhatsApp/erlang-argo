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
-module(argo_core_reader).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_core.hrl").
-include_lib("argo/include/argo_label.hrl").

%% API
-export([
    new/1,
    length_bounds_check/2,
    peek_label/1,
    read_bytes/2,
    read_float64/1,
    read_label/1,
    read_labeled_type/1,
    read_length/1,
    read_nullable_type/2,
    read_omittable_type/2,
    read_string/3,
    read_varint/1
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_core_reader{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Core) -> CoreWriter when Core :: binary(), CoreWriter :: t().
new(Core) when is_binary(Core) ->
    #argo_core_reader{core = Core}.

-spec length_bounds_check(CoreReader, Length) -> ok when CoreReader :: t(), Length :: integer().
length_bounds_check(R = #argo_core_reader{core = Core}, Length) when is_integer(Length) ->
    ok = length_sanity_check(Length),
    case byte_size(Core) of
        CoreLength when CoreLength < Length ->
            error_with_info(badarg, [R, Length], #{2 => not_enough_data});
        _ ->
            ok
    end.

%% @private
-spec backreference_sanity_check(Backreference) -> ok when Backreference :: integer().
backreference_sanity_check(Backreference) when ?is_u32(Backreference) ->
    ok;
backreference_sanity_check(Backreference) when is_integer(Backreference) ->
    error_with_info(badarg, [Backreference], #{1 => {invalid_backreference, Backreference}}).

%% @private
-spec length_sanity_check(Length) -> ok when Length :: integer().
length_sanity_check(Length) when ?is_u32(Length) ->
    ok;
length_sanity_check(Length) when is_integer(Length) andalso Length < 0 ->
    error_with_info(badarg, [Length], #{1 => {invalid_length_negative, Length}});
length_sanity_check(Length) when is_integer(Length) ->
    error_with_info(badarg, [Length], #{1 => {invalid_length_too_large, Length}}).

-spec peek_label(CoreReader) -> {ok, Label} | error when CoreReader :: t(), Label :: argo_types:label().
peek_label(#argo_core_reader{core = Core}) ->
    argo_varint:peek_zigzag_i64(Core).

-spec read_bytes(CoreReader, Length) -> {CoreReader, Bytes} when
    CoreReader :: t(), Length :: argo_types:length(), Bytes :: binary().
read_bytes(R0 = #argo_core_reader{core = Core0}, Length) when ?is_usize(Length) ->
    ok = length_bounds_check(R0, Length),
    <<Bytes:Length/bytes, Core1/bytes>> = Core0,
    R1 = R0#argo_core_reader{core = Core1},
    {R1, Bytes}.

-spec read_float64(CoreReader) -> {CoreReader, Float64} when CoreReader :: t(), Float64 :: float().
read_float64(R0 = #argo_core_reader{core = Core0}) ->
    ok = length_bounds_check(R0, 8),
    <<Float64:1/float-little-unit:64, Core1/bytes>> = Core0,
    R1 = R0#argo_core_reader{core = Core1},
    {R1, Float64}.

-spec read_label(CoreReader) -> {CoreReader, Label} when CoreReader :: t(), Label :: argo_types:label().
read_label(R = #argo_core_reader{}) ->
    read_varint(R).

-spec read_labeled_type(CoreReader) -> {CoreReader, LabeledType} when
    CoreReader :: t(), LabeledType :: argo_core:labeled_type().
read_labeled_type(R0 = #argo_core_reader{}) ->
    {R1, LengthOrBackreference} = read_varint(R0),
    case LengthOrBackreference < ?ARGO_LABEL_MARKER_LOWEST_RESERVED_VALUE of
        true ->
            Backreference = -LengthOrBackreference + ?ARGO_LABEL_MARKER_OFFSET_FACTOR,
            ok = backreference_sanity_check(Backreference),
            {R1, {backreference, Backreference}};
        false ->
            Length = LengthOrBackreference,
            ok = length_sanity_check(Length),
            {R1, {length, Length}}
    end.

-spec read_length(CoreReader) -> {CoreReader, Length} when CoreReader :: t(), Length :: argo_types:length().
read_length(R0 = #argo_core_reader{}) ->
    {R1, Length} = read_varint(R0),
    ok = length_sanity_check(Length),
    {R1, Length}.

-spec read_nullable_type(CoreReader, IsLabeled) -> {CoreReader, NullableType} when
    CoreReader :: t(), IsLabeled :: boolean(), NullableType :: argo_core:nullable_type().
read_nullable_type(R0 = #argo_core_reader{}, IsLabeled) when is_boolean(IsLabeled) ->
    case IsLabeled of
        false ->
            case read_label(R0) of
                {R1, ?ARGO_LABEL_MARKER_NULL} ->
                    {R1, argo_core:null()};
                {R1, ?ARGO_LABEL_MARKER_NON_NULL} ->
                    {R1, argo_core:non_null()};
                {R1, ?ARGO_LABEL_MARKER_ERROR} ->
                    {R1, argo_core:error()};
                {_, FieldLabel} ->
                    error_with_info(badarg, [R0, IsLabeled], #{general => {invalid_nullable_label, FieldLabel}})
            end;
        true ->
            case peek_label(R0) of
                {ok, ?ARGO_LABEL_MARKER_NULL} ->
                    {R1, ?ARGO_LABEL_MARKER_NULL} = read_label(R0),
                    {R1, argo_core:null()};
                {ok, ?ARGO_LABEL_MARKER_ERROR} ->
                    {R1, ?ARGO_LABEL_MARKER_ERROR} = read_label(R0),
                    {R1, argo_core:error()};
                _ ->
                    {R0, argo_core:non_null()}
            end
    end.

-spec read_omittable_type(CoreReader, IsLabeled) -> {CoreReader, OmittableType} when
    CoreReader :: t(), IsLabeled :: boolean(), OmittableType :: argo_core:omittable_type().
read_omittable_type(R0 = #argo_core_reader{}, IsLabeled) when is_boolean(IsLabeled) ->
    case IsLabeled of
        false ->
            case read_label(R0) of
                {R1, ?ARGO_LABEL_MARKER_ABSENT} ->
                    {R1, argo_core:absent()};
                {R1, ?ARGO_LABEL_MARKER_NON_NULL} ->
                    {R1, argo_core:non_null()};
                {_, FieldLabel} ->
                    error_with_info(badarg, [R0, IsLabeled], #{general => {invalid_omittable_label, FieldLabel}})
            end;
        true ->
            case peek_label(R0) of
                {ok, ?ARGO_LABEL_MARKER_ABSENT} ->
                    {R1, ?ARGO_LABEL_MARKER_ABSENT} = read_label(R0),
                    {R1, argo_core:absent()};
                _ ->
                    {R0, argo_core:non_null()}
            end
    end.

-spec read_string(CoreReader, Length, NullTerminatedStrings) -> {CoreReader, String} when
    CoreReader :: t(),
    Length :: argo_types:length(),
    NullTerminatedStrings :: boolean(),
    String :: unicode:unicode_binary().
read_string(R0 = #argo_core_reader{}, Length, NullTerminatedStrings) when
    ?is_usize(Length) andalso is_boolean(NullTerminatedStrings)
->
    {R1, Bytes} = read_bytes(R0, Length),
    String =
        case unicode:characters_to_binary(Bytes, utf8, utf8) of
            Bytes ->
                Bytes;
            {incomplete, _Encoded, _Rest} ->
                error_with_info(badarg, [R0, Length, NullTerminatedStrings], #{general => from_utf8_error});
            {error, _Encoded, _Rest} ->
                error_with_info(badarg, [R0, Length, NullTerminatedStrings], #{general => from_utf8_error})
        end,
    R2 =
        case NullTerminatedStrings of
            true ->
                ok = length_bounds_check(R1, 1),
                case R1 of
                    #argo_core_reader{core = <<0:8, Core1/bytes>>} ->
                        R1#argo_core_reader{core = Core1};
                    #argo_core_reader{core = <<Actual:8, _/bytes>>} ->
                        error_with_info(badarg, [R0, Length, NullTerminatedStrings], #{
                            general => {expected_null_terminated_string, Actual}
                        })
                end;
            false ->
                R1
        end,
    {R2, String}.

-spec read_varint(CoreReader) -> {CoreReader, Varint} when CoreReader :: t(), Varint :: argo_types:i64().
read_varint(R0 = #argo_core_reader{core = Core0}) ->
    ok = length_bounds_check(R0, 1),
    {Core1, Varint} = argo_varint:read_zigzag_i64(Core0),
    R1 = R0#argo_core_reader{core = Core1},
    {R1, Varint}.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
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
format_error_description(_Key, {expected_null_terminated_string, Actual}) ->
    io_lib:format("expected NULL byte after STRING when using NullTerminatedStrings mode, but was ~w", [Actual]);
format_error_description(_Key, from_utf8_error) ->
    "failed to decode string: invalid UTF-8 string";
format_error_description(_Key, {invalid_backreference, Actual}) ->
    io_lib:format("invalid decoded backreference, expected non-negative value to be <= u32::MAX, but was ~w", [Actual]);
format_error_description(_Key, {invalid_length_negative, Actual}) ->
    io_lib:format("invalid decoded length, expected non-negative value, but was ~w", [Actual]);
format_error_description(_Key, {invalid_length_too_large, Actual}) ->
    io_lib:format("invalid decoded length, expected value to be <= u32::MAX, but was ~w", [Actual]);
format_error_description(_Key, {invalid_nullable_label, Actual}) ->
    io_lib:format("invalid decoded nullable label, expected ~w, ~w, or ~w, but was ~w", [
        ?ARGO_LABEL_MARKER_ERROR, ?ARGO_LABEL_MARKER_NULL, ?ARGO_LABEL_MARKER_NON_NULL, Actual
    ]);
format_error_description(_Key, {invalid_omittable_label, Actual}) ->
    io_lib:format("invalid decoded omittable label, expected ~w or ~w, but was ~w", [
        ?ARGO_LABEL_MARKER_ABSENT, ?ARGO_LABEL_MARKER_NON_NULL, Actual
    ]);
format_error_description(_Key, not_enough_data) ->
    "not enough data";
format_error_description(_Key, Value) ->
    Value.
