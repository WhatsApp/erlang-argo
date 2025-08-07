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
%%% Created :  10 May 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_header_SUITE).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

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
    prop_roundtrip_http_argo_mode/0,
    prop_roundtrip_http_argo_mode/1,
    prop_roundtrip_u64/0,
    prop_roundtrip_u64/1,
    prop_roundtrip_uint/0,
    prop_roundtrip_uint/1
]).

%% Macros
-define(QUICKCHECK(OuterTest),
    argo_proper:quickcheck(OuterTest, [verbose, {max_shrinks, 100}, {numtests, 1000}])
).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, header}
    ].

groups() ->
    [
        {header, [parallel], [
            prop_roundtrip_http_argo_mode,
            prop_roundtrip_u64,
            prop_roundtrip_uint
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

prop_roundtrip_http_argo_mode() ->
    [
        {doc, "Property-based test for `argo_header' roundtrip with a HTTP Argo-Mode header."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_http_argo_mode(Config) ->
    ?QUICKCHECK(argo_header_prop:prop_roundtrip_http_argo_mode(Config)).

prop_roundtrip_u64() ->
    [
        {doc, "Property-based test for `argo_header' roundtrip with an unsigned 64-bit integer."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_u64(Config) ->
    ?QUICKCHECK(argo_header_prop:prop_roundtrip_u64(Config)).

prop_roundtrip_uint() ->
    [
        {doc, "Property-based test for `argo_header' roundtrip with an unsigned integer."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip_uint(Config) ->
    ?QUICKCHECK(argo_header_prop:prop_roundtrip_uint(Config)).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
