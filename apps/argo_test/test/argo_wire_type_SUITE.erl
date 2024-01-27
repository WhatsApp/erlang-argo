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
-module(argo_wire_type_SUITE).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% ct callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    prop_roundtrip_encoder_and_decoder/0,
    prop_roundtrip_encoder_and_decoder/1,
    prop_roundtrip_json_encoder_and_json_decoder/0,
    prop_roundtrip_json_encoder_and_json_decoder/1
]).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, wire_type}
    ].

groups() ->
    [
        {wire_type, [parallel], [
            prop_roundtrip_encoder_and_decoder,
            prop_roundtrip_json_encoder_and_json_decoder
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

prop_roundtrip_encoder_and_decoder() ->
    [
        {doc, "Loads the `argo_wire_type' and runs property tests for encoder/decoder."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_encoder_and_decoder(Config) ->
    argo_proper:quickcheck(
        argo_wire_type_prop,
        prop_roundtrip_encoder_and_decoder,
        Config,
        [
            verbose,
            {max_shrinks, 100},
            {numtests, 1000}
        ]
    ).

prop_roundtrip_json_encoder_and_json_decoder() ->
    [
        {doc, "Property-based test for `argo_wire_type' JSON encoder and JSON decoder."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_json_encoder_and_json_decoder(Config) ->
    argo_proper:quickcheck(
        argo_wire_type_prop,
        prop_roundtrip_json_encoder_and_json_decoder,
        Config,
        [
            verbose,
            {max_shrinks, 10},
            {numtests, 100}
        ]
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
