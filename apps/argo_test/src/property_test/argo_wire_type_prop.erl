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
-module(argo_wire_type_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Helpers
-export([
    report_not_equal/2
]).
%% Properties
-export([
    prop_roundtrip_encoder_and_decoder/1,
    prop_roundtrip_json_encoder_and_json_decoder/1
]).

%% Macros
-define(EQUALS(A, B), ?WHENFAIL(report_not_equal(A, B), A =:= B)).

%%%=============================================================================
%%% Helpers
%%%=============================================================================

-spec report_not_equal(A, B) -> ok when A :: term(), B :: term().
report_not_equal(A, B) ->
    io:format("Expected:~n~0tp~nActual:~n~0tp~n", [A, B]).

%%%=============================================================================
%%% Properties
%%%=============================================================================

-spec prop_roundtrip_encoder_and_decoder(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_encoder_and_decoder(_Config) ->
    ?FORALL(
        {WireType, Header},
        {proper_argo:wire_type(), proper_argo:header()},
        begin
            Encoded = argo_wire_type:to_writer(WireType, Header),
            {<<>>, Decoded} = argo_wire_type:from_reader(Encoded),
            ?EQUALS(WireType, Decoded)
        end
    ).

-spec prop_roundtrip_json_encoder_and_json_decoder(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_json_encoder_and_json_decoder(_Config) ->
    ?FORALL(
        WireType,
        proper_argo:wire_type(),
        begin
            JsonEncoded1 = json_encode(WireType),
            JsonDecoded1 = json_decode(JsonEncoded1),
            JsonEncoded2 = json_encode(JsonDecoded1),
            ExpectedWireType = WireType,
            ActualWireType = JsonDecoded1,
            ExpectedJson = JsonEncoded1,
            ActualJson = JsonEncoded2,
            ?WHENFAIL(
                begin
                    io:format(
                        "FAILURE: Expected (WireType JSON) does not match Actual (WireType JSON)~n"
                        "Expected (WireType):~n~0tp~n"
                        "Actual (WireType):~n~0tp~n"
                        "Expected (String):~n~ts~n"
                        "Actual (String):~n~ts~n"
                        "Expected (JSON):~n~ts~n"
                        "Actual (JSON):~n~ts~n",
                        [
                            ExpectedWireType,
                            ActualWireType,
                            argo:format_with_lines(ExpectedWireType),
                            argo:format_with_lines(ActualWireType),
                            json_encode_pretty(ExpectedWireType),
                            json_encode_pretty(ActualWireType)
                        ]
                    )
                end,
                ExpectedJson =:= ActualJson
            )
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal JSON functions
%%%-----------------------------------------------------------------------------

%% @private
-spec json_decode(JsonEncoded) -> WireType when
    JsonEncoded :: binary(), WireType :: argo_wire_type:t().
json_decode(JsonEncoded) when is_binary(JsonEncoded) ->
    json_decode(JsonEncoded, #{}).

%% @private
-spec json_decode(JsonEncoded, DecodeOptions) -> WireType when
    JsonEncoded :: binary(),
    DecodeOptions :: argo_json:decode_options(),
    WireType :: argo_wire_type:t().
json_decode(JsonEncoded, DecodeOptions) when
    is_binary(JsonEncoded) andalso is_map(DecodeOptions)
->
    argo_wire_type:from_json(argo_json:decode(JsonEncoded, DecodeOptions)).

%% @private
-spec json_encode(WireType) -> JsonEncoded when WireType :: argo_wire_type:t(), JsonEncoded :: binary().
json_encode(WireType = #argo_wire_type{}) ->
    argo_json:encode(argo_wire_type:to_json(WireType)).

%% @private
-spec json_encode_pretty(WireType) -> JsonEncodedPretty when
    WireType :: argo_wire_type:t(), JsonEncodedPretty :: unicode:unicode_binary().
json_encode_pretty(WireType = #argo_wire_type{}) ->
    argo_types:format_with_lines(argo_json:format(argo_wire_type:to_json(WireType))).
