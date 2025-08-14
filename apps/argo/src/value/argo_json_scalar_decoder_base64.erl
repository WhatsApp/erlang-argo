%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_json_scalar_decoder_base64).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2024-01-19", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").

-behaviour(argo_json_scalar_decoder).

%% argo_json_scalar_decoder callbacks
-export([
    init/1,
    decode_block_scalar/4,
    decode_desc_scalar/3,
    decode_scalar/3
]).
%% Errors API
-export([
    format_error/2
]).

%% Records
-record(argo_json_scalar_decoder_base64, {
    mode :: mode()
}).

%% Types
-type mode() :: 'mixed' | 'standard' | 'urlsafe'.
-type options() :: #{mode => mode()}.
-type t() :: #argo_json_scalar_decoder_base64{}.

-export_type([
    mode/0,
    options/0,
    t/0
]).

%%%=============================================================================
%%% argo_json_scalar_decoder callbacks
%%%=============================================================================

-spec init(Options) -> JsonScalarDecoder when Options :: options(), JsonScalarDecoder :: t().
init(Options) when is_map(Options) ->
    Mode = maps:get(mode, Options, mixed),
    ok =
        case Mode of
            _ when Mode =:= mixed orelse Mode =:= standard orelse Mode =:= urlsafe ->
                ok;
            _ ->
                error_with_info(badarg, [Options], #{1 => {unsupported_mode_option, Mode}})
        end,
    #argo_json_scalar_decoder_base64{mode = Mode}.

-spec decode_block_scalar(JsonScalarDecoder, BlockKey, BlockScalarHint, JsonValue) ->
    {ok, JsonScalarDecoder, BlockScalar} | {error, ErrorReason}
when
    JsonScalarDecoder :: t(),
    BlockKey :: argo_types:name(),
    BlockScalarHint :: argo_json_scalar_decoder:scalar_hint(),
    JsonValue :: argo_json:json_value(),
    BlockScalar :: argo_scalar_value:inner(),
    ErrorReason :: argo_json_scalar_decoder:error_reason().
decode_block_scalar(JsonScalarDecoder = #argo_json_scalar_decoder_base64{}, BlockKey, BlockScalarHint, JsonValue) when
    is_binary(BlockKey)
->
    decode_scalar(JsonScalarDecoder, BlockScalarHint, JsonValue).

-spec decode_desc_scalar(JsonScalarDecoder, DescHint, JsonValue) ->
    {ok, JsonScalarDecoder, DescScalar} | {error, ErrorReason}
when
    JsonScalarDecoder :: t(),
    DescHint :: argo_json_scalar_decoder:desc_hint(),
    JsonValue :: argo_json:json_value(),
    DescScalar :: argo_desc_value:inner_scalar(),
    ErrorReason :: argo_json_scalar_decoder:error_reason().
decode_desc_scalar(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{}, DescHint, JsonValue) ->
    case DescHint of
        null when JsonValue =:= null ->
            {ok, JsonScalarDecoder1, null};
        boolean when is_boolean(JsonValue) ->
            {ok, JsonScalarDecoder1, {boolean, JsonValue}};
        string when is_binary(JsonValue) ->
            maybe_decode_desc_string(JsonScalarDecoder1, JsonValue);
        bytes when is_binary(JsonValue) ->
            case decode_bytes(JsonScalarDecoder1, JsonValue) of
                {ok, JsonScalarDecoder2, BytesValue} ->
                    {ok, JsonScalarDecoder2, {bytes, BytesValue}};
                DecodeBytesError = {error, _Reason} ->
                    DecodeBytesError
            end;
        int when ?is_i64(JsonValue) ->
            {ok, JsonScalarDecoder1, {int, JsonValue}};
        float when is_float(JsonValue) ->
            {ok, JsonScalarDecoder1, {float, JsonValue}};
        _ ->
            {error, type_mismatch}
    end.

-spec decode_scalar(JsonScalarDecoder, ScalarHint, JsonValue) ->
    {ok, JsonScalarDecoder, Scalar} | {error, ErrorReason}
when
    JsonScalarDecoder :: t(),
    ScalarHint :: argo_json_scalar_decoder:scalar_hint(),
    JsonValue :: argo_json:json_value(),
    Scalar :: argo_scalar_value:inner(),
    ErrorReason :: argo_json_scalar_decoder:error_reason().
decode_scalar(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{mode = Mode}, ScalarHint, JsonValue) ->
    case ScalarHint of
        string when is_binary(JsonValue) ->
            {ok, JsonScalarDecoder1, {string, JsonValue}};
        boolean when is_boolean(JsonValue) ->
            {ok, JsonScalarDecoder1, {boolean, JsonValue}};
        varint when ?is_i64(JsonValue) ->
            {ok, JsonScalarDecoder1, {varint, JsonValue}};
        float64 when is_float(JsonValue) ->
            {ok, JsonScalarDecoder1, {float64, JsonValue}};
        bytes when is_binary(JsonValue) ->
            case decode_bytes(JsonScalarDecoder1, JsonValue) of
                {ok, JsonScalarDecoder2, BytesValue} ->
                    {ok, JsonScalarDecoder2, {bytes, BytesValue}};
                DecodeBytesError = {error, _Reason} ->
                    DecodeBytesError
            end;
        {fixed, FixedLength} when ?is_usize(FixedLength) andalso is_binary(JsonValue) ->
            case decode_bytes(JsonScalarDecoder1, JsonValue) of
                {ok, JsonScalarDecoder2, BytesValue} ->
                    {ok, JsonScalarDecoder2, {fixed, BytesValue}};
                DecodeBytesError = {error, _Reason} ->
                    DecodeBytesError
            end;
        desc ->
            JsonValueDecoder1 = argo_json_value_decoder:new(?MODULE, #{mode => Mode}),
            {_JsonValueDecoder2, DescValue} = argo_json_value_decoder:decode_desc_wire_type(
                JsonValueDecoder1, JsonValue
            ),
            {ok, JsonScalarDecoder1, {desc, DescValue}};
        _ ->
            {error, type_mismatch}
    end.

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
format_error_description(_Key, {unsupported_mode_option, ModeOption}) ->
    io_lib:format("unsupported mode option (must be 'mixed', 'standard', or 'urlsafe'), but was ~0tp", [ModeOption]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_bytes(JsonScalarDecoder, JsonValue) -> {ok, JsonScalarDecoder, BytesValue} | {error, ErrorReason} when
    JsonScalarDecoder :: t(),
    JsonValue :: argo_json:json_value(),
    BytesValue :: binary(),
    ErrorReason :: argo_json_scalar_decoder:error_reason().
decode_bytes(JsonScalarDecoder = #argo_json_scalar_decoder_base64{mode = mixed}, JsonValue) when is_binary(JsonValue) ->
    try base64:decode(argo_types:dynamic_cast(JsonValue), #{padding => false, mode => urlsafe}) of
        BytesValue ->
            {ok, JsonScalarDecoder, BytesValue}
    catch
        _:_ ->
            try base64:decode(argo_types:dynamic_cast(JsonValue), #{padding => false, mode => standard}) of
                BytesValue ->
                    {ok, JsonScalarDecoder, BytesValue}
            catch
                _:_ ->
                    {error, invalid}
            end
    end;
decode_bytes(JsonScalarDecoder = #argo_json_scalar_decoder_base64{mode = Mode}, JsonValue) when
    Mode =:= standard orelse Mode =:= urlsafe andalso is_binary(JsonValue)
->
    try base64:decode(argo_types:dynamic_cast(JsonValue), #{padding => false, mode => Mode}) of
        BytesValue ->
            {ok, JsonScalarDecoder, BytesValue}
    catch
        _:_ ->
            {error, invalid}
    end.

%% @private
-spec maybe_decode_desc_string(JsonScalarDecoder, JsonValue) -> {ok, JsonScalarDecoder, DescScalar} when
    JsonScalarDecoder :: t(), JsonValue :: argo_json:json_value(), DescScalar :: argo_desc_value:inner_scalar().
maybe_decode_desc_string(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{mode = mixed}, JsonValue) when
    is_binary(JsonValue)
->
    case JsonValue of
        <<"$B64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {ok, JsonScalarDecoder2, BytesValue} ->
                    {ok, JsonScalarDecoder2, {bytes, BytesValue}};
                {error, _} ->
                    {ok, JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        <<"$U64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {ok, JsonScalarDecoder2, BytesValue} ->
                    {ok, JsonScalarDecoder2, {bytes, BytesValue}};
                {error, _} ->
                    {ok, JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        _ ->
            {ok, JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
    end;
maybe_decode_desc_string(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{mode = standard}, JsonValue) when
    is_binary(JsonValue)
->
    case JsonValue of
        <<"$B64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {ok, JsonScalarDecoder2, BytesValue} ->
                    {ok, JsonScalarDecoder2, {bytes, BytesValue}};
                {error, _} ->
                    {ok, JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        _ ->
            {ok, JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
    end;
maybe_decode_desc_string(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{mode = urlsafe}, JsonValue) when
    is_binary(JsonValue)
->
    case JsonValue of
        <<"$U64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {ok, JsonScalarDecoder2, BytesValue} ->
                    {ok, JsonScalarDecoder2, {bytes, BytesValue}};
                {error, _} ->
                    {ok, JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        _ ->
            {ok, JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
    end.
