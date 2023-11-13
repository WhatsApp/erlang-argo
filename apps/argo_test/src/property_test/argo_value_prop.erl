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

-include_lib("proper/include/proper.hrl").

-include_lib("argo/include/argo_value.hrl").

%% Helpers
-export([
    report_not_equal/2
]).
%% Properties
-export([
    prop_roundtrip_encoder_and_decoder/1,
    prop_to_wire_type/1
]).

%% Macros
-ifdef(WHENFAIL).
-undef(WHENFAIL).
-endif.
% eqWAlizer gets angry about `fun(() -> boolean())` not being a subtype of `fun(() -> proper:test())`
-define(WHENFAIL(Action, Prop), proper:whenfail(?DELAY(Action), dynamic_cast(?DELAY(Prop)))).
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
-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.
