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
-module(argo_value_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).

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
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec json_decode(WireType, JsonEncoded) -> Value when
    WireType :: argo_wire_type:t(), JsonEncoded :: binary(), Value :: argo_value:t().
json_decode(WireType = #argo_wire_type{}, JsonEncoded) when is_binary(JsonEncoded) ->
    json_decode(WireType, JsonEncoded, []).

%% @private
-spec json_decode(WireType, JsonEncoded, DecodeOptions) -> Value when
    WireType :: argo_wire_type:t(),
    JsonEncoded :: binary(),
    DecodeOptions :: [jsone:decode_option()],
    Value :: argo_value:t().
json_decode(WireType = #argo_wire_type{}, JsonEncoded, DecodeOptions) when
    is_binary(JsonEncoded) andalso is_list(DecodeOptions)
->
    argo_value:from_json(
        WireType, argo_types:dynamic_cast(jsone:decode(JsonEncoded, [{object_format, tuple} | DecodeOptions]))
    ).

%% @private
-spec json_encode(Value) -> JsonEncoded when Value :: argo_value:t(), JsonEncoded :: binary().
json_encode(Value = #argo_value{}) ->
    json_encode(Value, []).

%% @private
-spec json_encode(Value, EncodeOptions) -> JsonEncoded when
    Value :: argo_value:t(), EncodeOptions :: [jsone:encode_option()], JsonEncoded :: binary().
json_encode(Value, EncodeOptions) when is_list(EncodeOptions) ->
    jsone:encode(argo_types:dynamic_cast(argo_value:to_json(Value)), [{float_format, [short]} | EncodeOptions]).

%% @private
-spec json_encode_pretty(Value) -> JsonEncoded when Value :: argo_value:t(), JsonEncoded :: unicode:unicode_binary().
json_encode_pretty(Value = #argo_value{}) ->
    argo_types:format_with_lines(json_encode(Value, [{indent, 2}])).
