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
%%% Created :  19 Jan 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_json_scalar_decoder_base64).
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
    {JsonScalarDecoder, BlockScalar} | error
when
    JsonScalarDecoder :: t(),
    BlockKey :: argo_types:name(),
    BlockScalarHint :: argo_json_scalar_decoder:scalar_hint(),
    JsonValue :: argo_json:json_value(),
    BlockScalar :: argo_scalar_value:inner().
decode_block_scalar(JsonScalarDecoder = #argo_json_scalar_decoder_base64{}, BlockKey, BlockScalarHint, JsonValue) when
    is_binary(BlockKey)
->
    decode_scalar(JsonScalarDecoder, BlockScalarHint, JsonValue).

-spec decode_desc_scalar(JsonScalarDecoder, DescHint, JsonValue) -> {JsonScalarDecoder, DescScalar} | error when
    JsonScalarDecoder :: t(),
    DescHint :: argo_json_scalar_decoder:desc_hint(),
    JsonValue :: argo_json:json_value(),
    DescScalar :: argo_desc_value:inner_scalar().
decode_desc_scalar(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{}, DescHint, JsonValue) ->
    case DescHint of
        null when JsonValue =:= null ->
            {JsonScalarDecoder1, DescHint};
        boolean when is_boolean(JsonValue) ->
            {JsonScalarDecoder1, {DescHint, JsonValue}};
        string when is_binary(JsonValue) ->
            maybe_decode_desc_string(JsonScalarDecoder1, JsonValue);
        bytes when is_binary(JsonValue) ->
            case decode_bytes(JsonScalarDecoder1, JsonValue) of
                {JsonScalarDecoder2, BytesValue} ->
                    {JsonScalarDecoder2, {bytes, BytesValue}};
                error ->
                    error
            end;
        int when ?is_i64(JsonValue) ->
            {JsonScalarDecoder1, {DescHint, JsonValue}};
        float when is_float(JsonValue) ->
            {JsonScalarDecoder1, {DescHint, JsonValue}}
    end.

-spec decode_scalar(JsonScalarDecoder, ScalarHint, JsonValue) -> {JsonScalarDecoder, Scalar} | error when
    JsonScalarDecoder :: t(),
    ScalarHint :: argo_json_scalar_decoder:scalar_hint(),
    JsonValue :: argo_json:json_value(),
    Scalar :: argo_scalar_value:inner().
decode_scalar(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{}, ScalarHint, JsonValue) ->
    case ScalarHint of
        string when is_binary(JsonValue) ->
            {JsonScalarDecoder1, {ScalarHint, JsonValue}};
        boolean when is_boolean(JsonValue) ->
            {JsonScalarDecoder1, {ScalarHint, JsonValue}};
        varint when ?is_i64(JsonValue) ->
            {JsonScalarDecoder1, {ScalarHint, JsonValue}};
        float64 when is_float(JsonValue) ->
            {JsonScalarDecoder1, {ScalarHint, JsonValue}};
        bytes when is_binary(JsonValue) ->
            case decode_bytes(JsonScalarDecoder1, JsonValue) of
                {JsonScalarDecoder2, BytesValue} ->
                    {JsonScalarDecoder2, {bytes, BytesValue}};
                error ->
                    error
            end;
        {fixed, FixedLength} when ?is_usize(FixedLength) andalso is_binary(JsonValue) ->
            case decode_bytes(JsonScalarDecoder1, JsonValue) of
                {JsonScalarDecoder2, BytesValue} ->
                    {JsonScalarDecoder2, {fixed, BytesValue}};
                error ->
                    error
            end
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
-spec decode_bytes(JsonScalarDecoder, JsonValue) -> {JsonScalarDecoder, BytesValue} | error when
    JsonScalarDecoder :: t(), JsonValue :: argo_json:json_value(), BytesValue :: binary().
decode_bytes(JsonScalarDecoder = #argo_json_scalar_decoder_base64{mode = mixed}, JsonValue) when is_binary(JsonValue) ->
    try base64:decode(argo_types:dynamic_cast(JsonValue), #{padding => false, mode => urlsafe}) of
        BytesValue ->
            {JsonScalarDecoder, BytesValue}
    catch
        _:_ ->
            try base64:decode(argo_types:dynamic_cast(JsonValue), #{padding => false, mode => standard}) of
                BytesValue ->
                    {JsonScalarDecoder, BytesValue}
            catch
                _:_ ->
                    error
            end
    end;
decode_bytes(JsonScalarDecoder = #argo_json_scalar_decoder_base64{mode = Mode}, JsonValue) when
    Mode =:= standard orelse Mode =:= urlsafe andalso is_binary(JsonValue)
->
    try base64:decode(argo_types:dynamic_cast(JsonValue), #{padding => false, mode => Mode}) of
        BytesValue ->
            {JsonScalarDecoder, BytesValue}
    catch
        _:_ ->
            error
    end.

%% @private
-spec maybe_decode_desc_string(JsonScalarDecoder, JsonValue) -> {JsonScalarDecoder, DescScalar} when
    JsonScalarDecoder :: t(), JsonValue :: argo_json:json_value(), DescScalar :: argo_desc_value:inner_scalar().
maybe_decode_desc_string(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{mode = mixed}, JsonValue) when
    is_binary(JsonValue)
->
    case JsonValue of
        <<"$B64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {JsonScalarDecoder2, BytesValue} ->
                    {JsonScalarDecoder2, {bytes, BytesValue}};
                error ->
                    {JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        <<"$U64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {JsonScalarDecoder2, BytesValue} ->
                    {JsonScalarDecoder2, {bytes, BytesValue}};
                error ->
                    {JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        _ ->
            {JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
    end;
maybe_decode_desc_string(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{mode = standard}, JsonValue) when
    is_binary(JsonValue)
->
    case JsonValue of
        <<"$B64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {JsonScalarDecoder2, BytesValue} ->
                    {JsonScalarDecoder2, {bytes, BytesValue}};
                error ->
                    {JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        _ ->
            {JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
    end;
maybe_decode_desc_string(JsonScalarDecoder1 = #argo_json_scalar_decoder_base64{mode = urlsafe}, JsonValue) when
    is_binary(JsonValue)
->
    case JsonValue of
        <<"$U64$", StringValue/bytes>> ->
            case decode_bytes(JsonScalarDecoder1, StringValue) of
                {JsonScalarDecoder2, BytesValue} ->
                    {JsonScalarDecoder2, {bytes, BytesValue}};
                error ->
                    {JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
            end;
        _ ->
            {JsonScalarDecoder1, {string, argo_types:unicode_binary(JsonValue)}}
    end.
