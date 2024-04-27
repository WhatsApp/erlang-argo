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
-module(argo_typer_SUITE).
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
    prop_roundtrip/0,
    prop_roundtrip/1,
    test_issue_7_incorrect_type_for_fields_in_fragment/1,
    test_issue_8_field_omittable/1,
    test_issue_8_fragment_spread_omittable/1,
    test_issue_8_inline_fragment_omittable/1,
    test_issue_19_field_selection_merging/1,
    test_issue_19_field_selection_merging_invalid/1,
    test_argo_typer_resolver/1
]).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, properties},
        {group, static}
    ].

groups() ->
    [
        {properties, [parallel], [
            prop_roundtrip
        ]},
        {static, [parallel], [
            test_issue_7_incorrect_type_for_fields_in_fragment,
            test_issue_8_field_omittable,
            test_issue_8_fragment_spread_omittable,
            test_issue_8_inline_fragment_omittable,
            test_issue_19_field_selection_merging,
            test_issue_19_field_selection_merging_invalid,
            test_argo_typer_resolver
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(static, Config) ->
    DataDir = test_server:lookup_config(data_dir, Config),
    ServiceDocumentFileName = filename:join([DataDir, "service_document.graphql"]),
    ExecutableDocumentFileName = filename:join([DataDir, "executable_document.graphql"]),
    ServiceDocument = argo_graphql_service_document:from_file(ServiceDocumentFileName),
    ExecutableDocument = argo_graphql_executable_document:from_file(ExecutableDocumentFileName),
    [{service_document, ServiceDocument}, {executable_document, ExecutableDocument} | Config];
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

prop_roundtrip() ->
    [
        {doc, "Loads the `argo_typer' and runs property tests for type derivation."},
        {timetrap, {seconds, 600}}
    ].

prop_roundtrip(Config) ->
    argo_proper:quickcheck(
        argo_typer_prop,
        prop_roundtrip,
        Config,
        [
            verbose,
            {max_size, 15},
            {max_shrinks, 10},
            {numtests, 100}
            % {numworkers, erlang:system_info(schedulers_online)}
        ]
    ).

test_issue_7_incorrect_type_for_fields_in_fragment(Config) ->
    ServiceDocument = test_server:lookup_config(service_document, Config),
    ExecutableDocument = test_server:lookup_config(executable_document, Config),
    {_, WireType} = argo_typer:derive_wire_type(
        ServiceDocument, ExecutableDocument, {some, <<"IncorrectTypeForFieldsInFragment">>}
    ),
    Actual = erlang:iolist_to_binary(argo:format(WireType)),
    Expected =
        <<
            "{\n"
            "  data: {\n"
            "    hero: {\n"
            "      name?: STRING<String>\n"
            "    }?\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    case Actual =:= Expected of
        false ->
            ct:fail("Expected:~n~ts~nActual:~n~ts~n", [Expected, Actual]);
        true ->
            ok
    end.

test_issue_8_field_omittable(Config) ->
    ServiceDocument = test_server:lookup_config(service_document, Config),
    ExecutableDocument = test_server:lookup_config(executable_document, Config),
    {_, WireType} = argo_typer:derive_wire_type(ServiceDocument, ExecutableDocument, {some, <<"FieldOmittable">>}),
    Actual = erlang:iolist_to_binary(argo:format(WireType)),
    Expected =
        <<
            "{\n"
            "  data: {\n"
            "    root: {\n"
            "      includeAlways: STRING<String>\n"
            "      includeVariable?: STRING<String>\n"
            "      skipNever: STRING<String>\n"
            "      skipVariable?: STRING<String>\n"
            "    }?\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    case Actual =:= Expected of
        false ->
            ct:fail("Expected:~n~ts~nActual:~n~ts~n", [Expected, Actual]);
        true ->
            ok
    end.

test_issue_8_fragment_spread_omittable(Config) ->
    ServiceDocument = test_server:lookup_config(service_document, Config),
    ExecutableDocument = test_server:lookup_config(executable_document, Config),
    {_, WireType} = argo_typer:derive_wire_type(
        ServiceDocument, ExecutableDocument, {some, <<"FragmentSpreadOmittable">>}
    ),
    Actual = erlang:iolist_to_binary(argo:format(WireType)),
    Expected =
        <<
            "{\n"
            "  data: {\n"
            "    root: {\n"
            "      __typename: STRING<String>\n"
            "      includeAlways?: STRING<String>\n"
            "      skipNever?: STRING<String>\n"
            "      required: {\n"
            "        includeAlways: STRING<String>\n"
            "        skipNever: STRING<String>\n"
            "      }\n"
            "      includeFragmentAlways: {\n"
            "        includeAlways: STRING<String>\n"
            "        skipNever: STRING<String>\n"
            "      }\n"
            "      includeFragmentVariable: {\n"
            "        includeAlways?: STRING<String>\n"
            "        skipNever?: STRING<String>\n"
            "      }\n"
            "      skipFragmentNever: {\n"
            "        includeAlways: STRING<String>\n"
            "        skipNever: STRING<String>\n"
            "      }\n"
            "      skipFragmentVariable: {\n"
            "        includeAlways?: STRING<String>\n"
            "        skipNever?: STRING<String>\n"
            "      }\n"
            "    }?\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    case Actual =:= Expected of
        false ->
            ct:fail("Expected:~n~ts~nActual:~n~ts~n", [Expected, Actual]);
        true ->
            ok
    end.

test_issue_8_inline_fragment_omittable(Config) ->
    ServiceDocument = test_server:lookup_config(service_document, Config),
    ExecutableDocument = test_server:lookup_config(executable_document, Config),
    {_, WireType} = argo_typer:derive_wire_type(
        ServiceDocument, ExecutableDocument, {some, <<"InlineFragmentOmittable">>}
    ),
    Actual = erlang:iolist_to_binary(argo:format(WireType)),
    Expected =
        <<
            "{\n"
            "  data: {\n"
            "    root: {\n"
            "      __typename: STRING<String>\n"
            "      includeInlineAlways: STRING<String>\n"
            "      skipInlineNever: STRING<String>\n"
            "      includeInlineVariable?: STRING<String>\n"
            "      skipInlineVariable?: STRING<String>\n"
            "      typeConditionInlineMatch: STRING<String>\n"
            "      typeConditionInlineNoMatch?: STRING<String>\n"
            "    }?\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    case Actual =:= Expected of
        false ->
            ct:fail("Expected:~n~ts~nActual:~n~ts~n", [Expected, Actual]);
        true ->
            ok
    end.

test_issue_19_field_selection_merging(Config) ->
    ServiceDocument = test_server:lookup_config(service_document, Config),
    ExecutableDocument = test_server:lookup_config(executable_document, Config),
    {_, WireType} = argo_typer:derive_wire_type(
        ServiceDocument, ExecutableDocument, {some, <<"FieldSelectionMergingQuery">>}
    ),
    Actual = erlang:iolist_to_binary(argo:format(WireType)),
    Expected =
        <<
            "{\n"
            "  data: {\n"
            "    root: {\n"
            "      __typename: STRING<String>\n"
            "      required: {\n"
            "        __typename: STRING<String>\n"
            "        object?: STRING<String>\n"
            "        otherObject?: STRING<String>\n"
            "      }\n"
            "      properties?: {\n"
            "        x: VARINT{Int}\n"
            "        y?: STRING<String>\n"
            "        z?: STRING<String>\n"
            "      }\n"
            "    }?\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    case Actual =:= Expected of
        false ->
            ct:fail("Expected:~n~ts~nActual:~n~ts~n", [Expected, Actual]);
        true ->
            ok
    end.

test_issue_19_field_selection_merging_invalid(Config) ->
    SD = test_server:lookup_config(service_document, Config),
    ED = test_server:lookup_config(executable_document, Config),
    OpName = {some, <<"FieldSelectionMergingInvalidQuery">>},
    ?assertError(badarg, argo_typer:derive_wire_type(SD, ED, OpName)),
    ok.

test_argo_typer_resolver(Config) ->
    SD = test_server:lookup_config(service_document, Config),
    ED = test_server:lookup_config(executable_document, Config),
    OpName = {some, <<"SimpleQueryWithRelayResolver">>},
    ?assertError(badarg, argo_typer:derive_wire_type(SD, ED, OpName)),
    Options = #{resolver => argo_typer_relay_resolver},
    {_, WireType} = argo_typer:derive_wire_type(SD, ED, OpName, Options),
    Actual = erlang:iolist_to_binary(argo:format(WireType)),
    Expected =
        <<
            "{\n"
            "  data: {\n"
            "    hero: {\n"
            "      strong_id__: STRING<ID>?\n"
            "    }?\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    case Actual =:= Expected of
        false ->
            ct:fail("Expected:~n~ts~nActual:~n~ts~n", [Expected, Actual]);
        true ->
            ok
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
