%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_json_scalar_encoder_base64).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2024-01-19", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_json_scalar_encoder).

%% argo_json_scalar_encoder callbacks
-export([
    init/1,
    encode_block_scalar/3,
    encode_desc_scalar/2,
    encode_scalar/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Records
-record(argo_json_scalar_encoder_base64, {
    mode :: mode()
}).

%% Types
-type mode() :: 'standard' | 'urlsafe'.
-type options() :: #{mode => mode()}.
-type t() :: #argo_json_scalar_encoder_base64{}.

-export_type([
    mode/0,
    options/0,
    t/0
]).

%%%=============================================================================
%%% argo_json_scalar_encoder callbacks
%%%=============================================================================

-spec init(Options) -> JsonScalarEncoder when Options :: options(), JsonScalarEncoder :: t().
init(Options) when is_map(Options) ->
    Mode = maps:get(mode, Options, urlsafe),
    ok =
        case Mode of
            _ when Mode =:= standard orelse Mode =:= urlsafe ->
                ok;
            _ ->
                error_with_info(badarg, [Options], #{1 => {unsupported_mode_option, Mode}})
        end,
    #argo_json_scalar_encoder_base64{mode = Mode}.

-spec encode_block_scalar(JsonScalarEncoder, BlockKey, BlockScalar) -> {JsonScalarEncoder, JsonValue} when
    JsonScalarEncoder :: t(),
    BlockKey :: argo_types:name(),
    BlockScalar :: argo_scalar_value:inner(),
    JsonValue :: argo_json:json_value().
encode_block_scalar(JsonScalarEncoder = #argo_json_scalar_encoder_base64{}, BlockKey, BlockScalar) when
    is_binary(BlockKey)
->
    encode_scalar(JsonScalarEncoder, BlockScalar).

-spec encode_desc_scalar(JsonScalarEncoder, DescScalar) -> {JsonScalarEncoder, JsonValue} when
    JsonScalarEncoder :: t(), DescScalar :: argo_desc_value:inner_scalar(), JsonValue :: argo_json:json_value().
encode_desc_scalar(JsonScalarEncoder = #argo_json_scalar_encoder_base64{}, DescScalar) ->
    case DescScalar of
        null ->
            {JsonScalarEncoder, argo_json:null()};
        {boolean, V} ->
            {JsonScalarEncoder, argo_json:boolean(V)};
        {string, V} ->
            {JsonScalarEncoder, argo_json:string(V)};
        {bytes, V} ->
            encode_bytes(JsonScalarEncoder, true, V);
        {int, V} ->
            {JsonScalarEncoder, argo_json:number(V)};
        {float, V} ->
            {JsonScalarEncoder, argo_json:number(V)}
    end.

-spec encode_scalar(JsonScalarEncoder, Scalar) -> {JsonScalarEncoder, JsonValue} when
    JsonScalarEncoder :: t(), Scalar :: argo_scalar_value:inner(), JsonValue :: argo_json:json_value().
encode_scalar(JsonScalarEncoder = #argo_json_scalar_encoder_base64{mode = Mode}, Scalar) ->
    case Scalar of
        {string, V} ->
            {JsonScalarEncoder, argo_json:string(V)};
        {boolean, V} ->
            {JsonScalarEncoder, argo_json:boolean(V)};
        {varint, V} ->
            {JsonScalarEncoder, argo_json:number(V)};
        {float64, V} ->
            {JsonScalarEncoder, argo_json:number(V)};
        {bytes, V} ->
            encode_bytes(JsonScalarEncoder, false, V);
        {fixed, V} ->
            encode_bytes(JsonScalarEncoder, false, V);
        {desc, V} ->
            JsonValueEncoder1 = argo_json_value_encoder:new(?MODULE, #{mode => Mode}),
            {_JsonValueEncoder2, JsonValue} = argo_json_value_encoder:encode_desc_value(JsonValueEncoder1, V),
            {JsonScalarEncoder, JsonValue}
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
    io_lib:format("unsupported mode option (must be 'standard' or 'urlsafe'), but was ~0tp", [ModeOption]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec encode_bytes(JsonScalarEncoder, SelfDescribing, BytesValue) -> {JsonScalarEncoder, JsonValue} when
    JsonScalarEncoder :: t(), SelfDescribing :: boolean(), BytesValue :: binary(), JsonValue :: argo_json:json_value().
encode_bytes(JsonScalarEncoder = #argo_json_scalar_encoder_base64{mode = standard}, SelfDescribing, BytesValue) when
    is_boolean(SelfDescribing) andalso is_binary(BytesValue)
->
    StringValue = base64:encode(BytesValue, #{padding => false, mode => standard}),
    case SelfDescribing of
        false ->
            {JsonScalarEncoder, StringValue};
        true ->
            {JsonScalarEncoder, <<"$B64$", StringValue/bytes>>}
    end;
encode_bytes(JsonScalarEncoder = #argo_json_scalar_encoder_base64{mode = urlsafe}, SelfDescribing, BytesValue) when
    is_boolean(SelfDescribing) andalso is_binary(BytesValue)
->
    StringValue = base64:encode(BytesValue, #{padding => false, mode => urlsafe}),
    case SelfDescribing of
        false ->
            {JsonScalarEncoder, StringValue};
        true ->
            {JsonScalarEncoder, <<"$U64$", StringValue/bytes>>}
    end.
