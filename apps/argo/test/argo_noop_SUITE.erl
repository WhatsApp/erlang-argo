%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc A test suite designed to load the `argo' app and do nothing
%%% else.
%%%
%%% @end
%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_noop_SUITE).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

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
    test_that_the_app_loads/0,
    test_that_the_app_loads/1
]).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, noop}
    ].

groups() ->
    [
        {noop, [parallel], [
            test_that_the_app_loads
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

test_that_the_app_loads() ->
    [
        {doc, "Loads the `argo' app and verifies that `argo_wire_type:desc()' works."},
        {timetrap, {seconds, 60}}
    ].

test_that_the_app_loads(_Config) ->
    ?assert(is_record(argo_wire_type:desc(), argo_wire_type)).
