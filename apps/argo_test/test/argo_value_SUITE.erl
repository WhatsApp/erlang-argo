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
-module(argo_value_SUITE).
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
    issue_18_non_null_desc_null/0,
    issue_18_non_null_desc_null/1,
    prop_roundtrip_encoder_and_decoder/0,
    prop_roundtrip_encoder_and_decoder/1,
    prop_roundtrip_json_encoder_and_json_decoder/0,
    prop_roundtrip_json_encoder_and_json_decoder/1,
    prop_roundtrip_term_encoder_and_term_decoder/0,
    prop_roundtrip_term_encoder_and_term_decoder/1,
    prop_to_wire_type/0,
    prop_to_wire_type/1
]).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, value}
    ].

groups() ->
    [
        {value, [parallel], [
            issue_18_non_null_desc_null,
            prop_roundtrip_encoder_and_decoder,
            prop_roundtrip_json_encoder_and_json_decoder,
            prop_roundtrip_term_encoder_and_term_decoder,
            prop_to_wire_type
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

issue_18_non_null_desc_null() ->
    [
        {doc, "Edge case where NON_NULL(DESC(NULL)) occurs. See https://github.com/msolomon/argo/issues/18"},
        {timetrap, {seconds, 60}}
    ].

issue_18_non_null_desc_null(_Config) ->
    InlineHeader = argo_header:new(#{inline_everything => true}),
    InlineSelfDescribingHeader = argo_header:new(#{inline_everything => true, self_describing => true}),
    SelfDescribingHeader = argo_header:new(#{self_describing => true}),

    ScalarWireType = argo_scalar_wire_type:desc(),
    BlockWireType = argo_block_wire_type:new(ScalarWireType, <<"JSON">>, false),
    NullableWireType = argo_nullable_wire_type:new(argo_wire_type:block(BlockWireType)),
    WireType = argo_wire_type:nullable(NullableWireType),

    DescValueNull = argo_desc_value:null(),
    ScalarValueNull = argo_scalar_value:desc(DescValueNull),
    BlockValueNull = argo_block_value:new(BlockWireType, ScalarValueNull),
    NullableValueNull = argo_nullable_value:non_null(NullableWireType, argo_value:block(BlockValueNull)),
    ValueNull = argo_value:nullable(NullableValueNull),

    ValueNullDefaultEncoded = argo_value:to_writer(ValueNull),
    ?assertEqual({<<>>, ValueNull}, argo_value:from_reader(WireType, ValueNullDefaultEncoded)),

    ValueNullInlineEncoded = argo_value:to_writer(ValueNull, InlineHeader),
    ?assertEqual({<<>>, ValueNull}, argo_value:from_reader(WireType, ValueNullInlineEncoded)),

    ValueNullInlineSelfDescribingEncoded = argo_value:to_writer(ValueNull, InlineSelfDescribingHeader),
    ?assertEqual({<<>>, ValueNull}, argo_value:from_reader(WireType, ValueNullInlineSelfDescribingEncoded)),

    ValueNullSelfDescribingEncoded = argo_value:to_writer(ValueNull, SelfDescribingHeader),
    ?assertEqual({<<>>, ValueNull}, argo_value:from_reader(WireType, ValueNullSelfDescribingEncoded)),

    DescValueNonNull = argo_desc_value:string(<<>>),
    ScalarValueNonNull = argo_scalar_value:desc(DescValueNonNull),
    BlockValueNonNull = argo_block_value:new(BlockWireType, ScalarValueNonNull),
    NullableValueNonNull = argo_nullable_value:non_null(NullableWireType, argo_value:block(BlockValueNonNull)),
    ValueNonNull = argo_value:nullable(NullableValueNonNull),

    ValueNonNullDefaultEncoded = argo_value:to_writer(ValueNonNull),
    ?assertEqual({<<>>, ValueNonNull}, argo_value:from_reader(WireType, ValueNonNullDefaultEncoded)),

    ValueNonNullInlineEncoded = argo_value:to_writer(ValueNonNull, InlineHeader),
    ?assertEqual({<<>>, ValueNonNull}, argo_value:from_reader(WireType, ValueNonNullInlineEncoded)),

    ValueNonNullInlineSelfDescribingEncoded = argo_value:to_writer(ValueNonNull, InlineSelfDescribingHeader),
    ?assertEqual({<<>>, ValueNonNull}, argo_value:from_reader(WireType, ValueNonNullInlineSelfDescribingEncoded)),

    ValueNonNullSelfDescribingEncoded = argo_value:to_writer(ValueNonNull, SelfDescribingHeader),
    ?assertEqual({<<>>, ValueNonNull}, argo_value:from_reader(WireType, ValueNonNullSelfDescribingEncoded)),

    ok.

prop_roundtrip_encoder_and_decoder() ->
    [
        {doc, "Property-based test for `argo_value' encoder and decoder."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_encoder_and_decoder(Config) ->
    argo_proper:quickcheck(
        argo_value_prop,
        prop_roundtrip_encoder_and_decoder,
        Config,
        [
            verbose,
            {max_shrinks, 100},
            {numtests, 100}
        ]
    ).

prop_roundtrip_json_encoder_and_json_decoder() ->
    [
        {doc, "Property-based test for `argo_value' JSON encoder and JSON decoder."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_json_encoder_and_json_decoder(Config) ->
    argo_proper:quickcheck(
        argo_value_prop,
        prop_roundtrip_json_encoder_and_json_decoder,
        Config,
        [
            verbose,
            {max_shrinks, 10},
            {numtests, 100}
        ]
    ).

prop_roundtrip_term_encoder_and_term_decoder() ->
    [
        {doc, "Property-based test for `argo_value' term encoder and term decoder."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_term_encoder_and_term_decoder(Config) ->
    argo_proper:quickcheck(
        argo_value_prop,
        prop_roundtrip_term_encoder_and_term_decoder,
        Config,
        [
            verbose,
            {max_shrinks, 10},
            {numtests, 100}
        ]
    ).

prop_to_wire_type() ->
    [
        {doc, "Property-based test for `argo_value:to_wire_type/1'."},
        {timetrap, {seconds, 600}}
    ].

prop_to_wire_type(Config) ->
    argo_proper:quickcheck(
        argo_value_prop,
        prop_to_wire_type,
        Config,
        [
            verbose,
            {max_shrinks, 100},
            {numtests, 100}
        ]
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
