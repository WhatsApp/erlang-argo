%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_value_prop).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-08-14"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Helpers
-export([
    report_not_equal/2
]).
%% Properties
-export([
    prop_roundtrip_encoder_and_decoder/1,
    prop_roundtrip_json_encoder_and_json_decoder/1,
    prop_roundtrip_term_encoder_and_term_decoder/1,
    prop_roundtrip_zlib_encoder_and_zlib_decoder/1,
    prop_to_wire_type/1
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
        {{WireType, Value}, Header},
        {?LET(WireType, proper_argo:wire_type(), {WireType, proper_argo:value(WireType)}), proper_argo:header()},
        begin
            Encoded = argo_value:to_writer(Value, Header),
            {<<>>, Decoded} = argo_value:from_reader(WireType, Encoded),
            ?EQUALS(Value, Decoded)
        end
    ).

-spec prop_roundtrip_json_encoder_and_json_decoder(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_json_encoder_and_json_decoder(_Config) ->
    ?FORALL(
        {WireType, Value},
        ?LET(WireType, proper_argo:wire_type(), {WireType, proper_argo:value_json_safe(WireType)}),
        begin
            % Header1 = argo_header:new(),
            % {Header2, _} = argo_header:set_self_describing_errors(Header1, false),
            JsonEncoded1 = json_encode(Value),
            JsonDecoded1 = json_decode(WireType, JsonEncoded1),
            JsonEncoded2 = json_encode(JsonDecoded1),
            % Encoded = argo_value:to_writer(Value, Header2),
            ExpectedValue = Value,
            ActualValue = JsonDecoded1,
            ExpectedJson = JsonEncoded1,
            ActualJson = JsonEncoded2,
            % PercentageSmaller = trunc(math:ceil((byte_size(Encoded) / byte_size(JsonEncoded1)) * 100)),
            % PercentageSmaller = byte_size(Encoded) / byte_size(JsonEncoded1),
            % SmallerOrLarger =
            %     case byte_size(Encoded) < byte_size(JsonEncoded1) of
            %         true ->
            %             smaller;
            %         false ->
            %             larger
            %     end,
            ?WHENFAIL(
                begin
                    io:format(
                        "FAILURE: Expected (JSON) does not match Actual (JSON)~n"
                        "Expected (Value):~n~0tp~n"
                        "Actual (Value):~n~0tp~n"
                        "Expected (String):~n~ts~n"
                        "Actual (String):~n~ts~n"
                        "Expected (JSON):~n~ts~n"
                        "Actual (JSON):~n~ts~n",
                        [
                            ExpectedValue,
                            ActualValue,
                            argo:format_with_lines(ExpectedValue),
                            argo:format_with_lines(ActualValue),
                            json_encode_pretty(ExpectedValue),
                            json_encode_pretty(ActualValue)
                        ]
                    )
                end,
                ExpectedJson =:= ActualJson
                % aggregate([SmallerOrLarger], ExpectedJson =:= ActualJson)
            )
        end
    ).

-spec prop_roundtrip_term_encoder_and_term_decoder(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_term_encoder_and_term_decoder(_Config) ->
    ?FORALL(
        {WireType, Value},
        ?LET(WireType, proper_argo:wire_type(), {WireType, proper_argo:value_term_safe(WireType)}),
        begin
            TermEncoded1 = term_encode(Value),
            TermDecoded1 = term_decode(WireType, TermEncoded1),
            TermEncoded2 = term_encode(TermDecoded1),
            ExpectedValue = Value,
            ActualValue = TermDecoded1,
            ExpectedTerm = TermEncoded1,
            ActualTerm = TermEncoded2,
            ?WHENFAIL(
                begin
                    io:format(
                        "FAILURE: Expected (Term) does not match Actual (Term)~n"
                        "Expected (Value):~n~0tp~n"
                        "Actual (Value):~n~0tp~n"
                        "Expected (String):~n~ts~n"
                        "Actual (String):~n~ts~n"
                        "Expected (Term):~n~ts~n"
                        "Actual (Term):~n~ts~n",
                        [
                            ExpectedValue,
                            ActualValue,
                            argo:format_with_lines(ExpectedValue),
                            argo:format_with_lines(ActualValue),
                            term_encode_pretty(ExpectedValue),
                            term_encode_pretty(ActualValue)
                        ]
                    )
                end,
                ExpectedTerm =:= ActualTerm
            )
        end
    ).

-spec prop_roundtrip_zlib_encoder_and_zlib_decoder(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_zlib_encoder_and_zlib_decoder(_Config) ->
    ?FORALL(
        {WireType, Value},
        ?LET(WireType, proper_argo:wire_type(), {WireType, proper_argo:value_term_safe(WireType)}),
        begin
            TermEncoded1 = zlib_encode(Value),
            TermDecoded1 = zlib_decode(WireType, TermEncoded1),
            TermEncoded2 = zlib_encode(TermDecoded1),
            ExpectedValue = Value,
            ActualValue = TermDecoded1,
            ExpectedTerm = TermEncoded1,
            ActualTerm = TermEncoded2,
            ?WHENFAIL(
                begin
                    io:format(
                        "FAILURE: Expected (zlib) does not match Actual (zlib)~n"
                        "Expected (Value):~n~0tp~n"
                        "Actual (Value):~n~0tp~n"
                        "Expected (String):~n~ts~n"
                        "Actual (String):~n~ts~n"
                        "Expected (zlib):~n~ts~n"
                        "Actual (zlib):~n~ts~n",
                        [
                            ExpectedValue,
                            ActualValue,
                            argo:format_with_lines(ExpectedValue),
                            argo:format_with_lines(ActualValue),
                            zlib_encode_pretty(ExpectedValue),
                            zlib_encode_pretty(ActualValue)
                        ]
                    )
                end,
                ExpectedTerm =:= ActualTerm
            )
        end
    ).

-spec prop_to_wire_type(ct_suite:ct_config()) -> proper:test().
prop_to_wire_type(_Config) ->
    ?FORALL(
        {WireType, Value},
        ?LET(WireType, proper_argo:wire_type(), {WireType, proper_argo:value(WireType)}),
        begin
            ToWireType = argo_value:to_wire_type(Value),
            ?EQUALS(WireType, ToWireType)
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal JSON functions
%%%-----------------------------------------------------------------------------

%% @private
-spec json_decode(WireType, JsonEncoded) -> Value when
    WireType :: argo_wire_type:t(), JsonEncoded :: binary(), Value :: argo_value:t().
json_decode(WireType = #argo_wire_type{}, JsonEncoded) when is_binary(JsonEncoded) ->
    json_decode(WireType, JsonEncoded, #{}).

%% @private
-spec json_decode(WireType, JsonEncoded, DecodeOptions) -> Value when
    WireType :: argo_wire_type:t(),
    JsonEncoded :: binary(),
    DecodeOptions :: argo_json:decode_options(),
    Value :: argo_value:t().
json_decode(WireType = #argo_wire_type{}, JsonEncoded, DecodeOptions) when
    is_binary(JsonEncoded) andalso is_map(DecodeOptions)
->
    argo_value:from_json(WireType, argo_json:decode(JsonEncoded, DecodeOptions)).

%% @private
-spec json_encode(Value) -> JsonEncoded when Value :: argo_value:t(), JsonEncoded :: binary().
json_encode(Value = #argo_value{}) ->
    argo_json:encode(argo_value:to_json(Value)).

%% @private
-spec json_encode_pretty(Value) -> JsonEncodedPretty when
    Value :: argo_value:t(), JsonEncodedPretty :: unicode:unicode_binary().
json_encode_pretty(Value = #argo_value{}) ->
    argo_types:format_with_lines(argo_json:format(argo_value:to_json(Value))).

%%%-----------------------------------------------------------------------------
%%% Internal Term functions
%%%-----------------------------------------------------------------------------

%% @private
-spec term_decode(WireType, TermEncoded) -> Value when
    WireType :: argo_wire_type:t(), TermEncoded :: argo_term:term_value(), Value :: argo_value:t().
term_decode(WireType = #argo_wire_type{}, TermEncoded) ->
    Value = argo_value:from_erlang(WireType, TermEncoded),
    Value.

%% @private
-spec term_encode(Value) -> TermEncoded when Value :: argo_value:t(), TermEncoded :: argo_term:term_value().
term_encode(Value = #argo_value{}) ->
    TermEncoded = argo_value:to_erlang(Value),
    TermEncoded.

%% @private
-spec term_encode_pretty(Value) -> TermEncodedPretty when
    Value :: argo_value:t(), TermEncodedPretty :: unicode:unicode_binary().
term_encode_pretty(Value = #argo_value{}) ->
    argo_types:format_with_lines(io_lib:format("~tp", [term_encode(Value)])).

%%%-----------------------------------------------------------------------------
%%% Internal zlib functions
%%%-----------------------------------------------------------------------------

%% @private
-spec zlib_decode(WireType, TermEncoded) -> Value when
    WireType :: argo_wire_type:t(), TermEncoded :: argo_term:term_value(), Value :: argo_value:t().
zlib_decode(WireType = #argo_wire_type{}, TermEncoded) ->
    Value = argo_value:from_erlang(WireType, TermEncoded, argo_test_zlib_scalar_decoder, #{level => default}),
    Value.

%% @private
-spec zlib_encode(Value) -> TermEncoded when Value :: argo_value:t(), TermEncoded :: argo_term:term_value().
zlib_encode(Value = #argo_value{}) ->
    TermEncoded = argo_value:to_erlang(Value, argo_test_zlib_scalar_encoder, #{}),
    TermEncoded.

%% @private
-spec zlib_encode_pretty(Value) -> TermEncodedPretty when
    Value :: argo_value:t(), TermEncodedPretty :: unicode:unicode_binary().
zlib_encode_pretty(Value = #argo_value{}) ->
    argo_types:format_with_lines(io_lib:format("~tp", [zlib_encode(Value)])).
