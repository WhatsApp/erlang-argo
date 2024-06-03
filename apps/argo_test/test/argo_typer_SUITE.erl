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
    test_argo_typer_resolver/1,
    test_argo_1_2_json/1,
    test_simple_introspection_query/1
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
            test_argo_typer_resolver,
            test_argo_1_2_json,
            test_simple_introspection_query
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
    IntrospectionQueryFileName = filename:join([DataDir, "introspection_query.graphql"]),
    ServiceDocument = argo_graphql_service_document:from_file(ServiceDocumentFileName),
    ExecutableDocument = argo_graphql_executable_document:from_file(ExecutableDocumentFileName),
    IntrospectionQuery = argo_graphql_executable_document:from_file(IntrospectionQueryFileName),
    JsonResponses = filelib:fold_files(
        filename:join([DataDir, "responses"]),
        ".*\\.json",
        false,
        fun(File, Acc) ->
            {ok, JsonEncoded} = file:read_file(File),
            JsonValue = argo_types:dynamic_cast(jsone:decode(JsonEncoded, [{object_format, tuple}])),
            Key = argo_types:unicode_binary(filename:basename(File, ".json")),
            Acc#{Key => JsonValue}
        end,
        maps:new()
    ),
    [
        {service_document, ServiceDocument},
        {executable_document, ExecutableDocument},
        {introspection_query, IntrospectionQuery},
        {json_responses, JsonResponses}
        | Config
    ];
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
    ?assertEqual(Expected, Actual),
    ok.

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
    ?assertEqual(Expected, Actual),
    ok.

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
    ?assertEqual(Expected, Actual),
    ok.

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
    ?assertEqual(Expected, Actual),
    ok.

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
    ?assertEqual(Expected, Actual),
    ok.

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
    ?assertEqual(Expected, Actual),
    ok.

test_argo_1_2_json(Config) ->
    SD = test_server:lookup_config(service_document, Config),
    ED = test_server:lookup_config(executable_document, Config),
    Op = <<"Argo_1_2_JSON">>,
    #{Op := JsonValue} = test_server:lookup_config(json_responses, Config),
    {_, WireType} = argo_typer:derive_wire_type(SD, ED, {some, Op}),
    ActualWireType = erlang:iolist_to_binary(argo:format(WireType)),
    ExpectedWireType =
        <<
            "{\n"
            "  data: {\n"
            "    json: DESC{JSON}?\n"
            "    nullJson: DESC{JSON}?\n"
            "    requiredJson: DESC{JSON}\n"
            "    nullRequiredJson: DESC{JSON}\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    ?assertEqual(ExpectedWireType, ActualWireType),
    Value = argo_value:from_json(WireType, JsonValue),
    ActualValue = erlang:iolist_to_binary(argo:format(Value)),
    ExpectedValue =
        <<
            "{\n"
            "  data: NON_NULL({\n"
            "    json: NON_NULL(DESC({\n"
            "      <<\"name\">>: DESC(STRING(<<\"John Doe\">>))\n"
            "      <<\"age\">>: DESC(INT(30))\n"
            "      <<\"score\">>: DESC(FLOAT(-10.1))\n"
            "      <<\"eligible\">>: DESC(BOOLEAN(false))\n"
            "      <<\"null\">>: DESC(NULL)\n"
            "      <<\"entries\">>: DESC([\n"
            "        DESC({\n"
            "          <<\"name\">>: DESC(STRING(<<\"Entry 1\">>))\n"
            "          <<\"value\">>: DESC(INT(10))\n"
            "        }),\n"
            "        DESC({\n"
            "          <<\"name\">>: DESC(STRING(<<\"Entry 2\">>))\n"
            "          <<\"value\">>: DESC(INT(20))\n"
            "        }),\n"
            "      ])\n"
            "    }){JSON})\n"
            "    nullJson: NULL\n"
            "    requiredJson: DESC({\n"
            "      <<\"name\">>: DESC(STRING(<<\"John Doe\">>))\n"
            "      <<\"age\">>: DESC(INT(30))\n"
            "      <<\"score\">>: DESC(FLOAT(-10.1))\n"
            "      <<\"eligible\">>: DESC(BOOLEAN(false))\n"
            "      <<\"null\">>: DESC(NULL)\n"
            "      <<\"entries\">>: DESC([\n"
            "        DESC({\n"
            "          <<\"name\">>: DESC(STRING(<<\"Entry 1\">>))\n"
            "          <<\"value\">>: DESC(INT(10))\n"
            "        }),\n"
            "        DESC({\n"
            "          <<\"name\">>: DESC(STRING(<<\"Entry 2\">>))\n"
            "          <<\"value\">>: DESC(INT(20))\n"
            "        }),\n"
            "      ])\n"
            "    }){JSON}\n"
            "    nullRequiredJson: DESC(NULL){JSON}\n"
            "  })\n"
            "  errors?: ABSENT\n"
            "  extensions?: ABSENT\n"
            "}"
        >>,
    ?assertEqual(ExpectedValue, ActualValue),
    ArgoEncoded = argo_value:to_writer(Value),
    ?assertMatch({<<>>, Value}, argo_value:from_reader(WireType, ArgoEncoded)),
    ArgoSelfDescribingHeader = argo_header:new(#{self_describing => true}),
    ArgoSelfDescribingEncoded = argo_value:to_writer(Value, ArgoSelfDescribingHeader),
    ?assertMatch({<<>>, Value}, argo_value:from_reader(WireType, ArgoSelfDescribingEncoded)),
    ExpectedJson = jsone:encode(JsonValue),
    ArgoEncodedJson = argo_value:to_json(Value),
    ActualJson = jsone:encode(ArgoEncodedJson),
    ?assertEqual(ExpectedJson, ActualJson),
    ok.

test_simple_introspection_query(Config) ->
    SD = test_server:lookup_config(service_document, Config),
    ED = test_server:lookup_config(introspection_query, Config),
    ResponseKey = <<"simple_introspection">>,
    Op = <<"IntrospectionQuery">>,
    #{ResponseKey := JsonValue} = test_server:lookup_config(json_responses, Config),
    {_, WireType} = argo_typer:derive_wire_type(SD, ED, {some, Op}),
    ActualWireType = erlang:iolist_to_binary(argo:format(WireType)),
    ExpectedWireType =
        <<
            "{\n"
            "  data: {\n"
            "    __schema: {\n"
            "      queryType: {\n"
            "        name: STRING<String>?\n"
            "      }\n"
            "      mutationType: {\n"
            "        name: STRING<String>?\n"
            "      }?\n"
            "      subscriptionType: {\n"
            "        name: STRING<String>?\n"
            "      }?\n"
            "      types: {\n"
            "        kind: STRING<__TypeKind>\n"
            "        name: STRING<String>?\n"
            "        description: STRING<String>?\n"
            "        fields: {\n"
            "          name: STRING<String>\n"
            "          description: STRING<String>?\n"
            "          args: {\n"
            "            name: STRING<String>\n"
            "            description: STRING<String>?\n"
            "            type: {\n"
            "              kind: STRING<__TypeKind>\n"
            "              name: STRING<String>?\n"
            "              ofType: {\n"
            "                kind: STRING<__TypeKind>\n"
            "                name: STRING<String>?\n"
            "                ofType: {\n"
            "                  kind: STRING<__TypeKind>\n"
            "                  name: STRING<String>?\n"
            "                  ofType: {\n"
            "                    kind: STRING<__TypeKind>\n"
            "                    name: STRING<String>?\n"
            "                    ofType: {\n"
            "                      kind: STRING<__TypeKind>\n"
            "                      name: STRING<String>?\n"
            "                      ofType: {\n"
            "                        kind: STRING<__TypeKind>\n"
            "                        name: STRING<String>?\n"
            "                        ofType: {\n"
            "                          kind: STRING<__TypeKind>\n"
            "                          name: STRING<String>?\n"
            "                          ofType: {\n"
            "                            kind: STRING<__TypeKind>\n"
            "                            name: STRING<String>?\n"
            "                          }?\n"
            "                        }?\n"
            "                      }?\n"
            "                    }?\n"
            "                  }?\n"
            "                }?\n"
            "              }?\n"
            "            }\n"
            "            defaultValue: STRING<String>?\n"
            "          }[]\n"
            "          type: {\n"
            "            kind: STRING<__TypeKind>\n"
            "            name: STRING<String>?\n"
            "            ofType: {\n"
            "              kind: STRING<__TypeKind>\n"
            "              name: STRING<String>?\n"
            "              ofType: {\n"
            "                kind: STRING<__TypeKind>\n"
            "                name: STRING<String>?\n"
            "                ofType: {\n"
            "                  kind: STRING<__TypeKind>\n"
            "                  name: STRING<String>?\n"
            "                  ofType: {\n"
            "                    kind: STRING<__TypeKind>\n"
            "                    name: STRING<String>?\n"
            "                    ofType: {\n"
            "                      kind: STRING<__TypeKind>\n"
            "                      name: STRING<String>?\n"
            "                      ofType: {\n"
            "                        kind: STRING<__TypeKind>\n"
            "                        name: STRING<String>?\n"
            "                        ofType: {\n"
            "                          kind: STRING<__TypeKind>\n"
            "                          name: STRING<String>?\n"
            "                        }?\n"
            "                      }?\n"
            "                    }?\n"
            "                  }?\n"
            "                }?\n"
            "              }?\n"
            "            }?\n"
            "          }\n"
            "          isDeprecated: BOOLEAN\n"
            "          deprecationReason: STRING<String>?\n"
            "        }[]?\n"
            "        inputFields: {\n"
            "          name: STRING<String>\n"
            "          description: STRING<String>?\n"
            "          type: {\n"
            "            kind: STRING<__TypeKind>\n"
            "            name: STRING<String>?\n"
            "            ofType: {\n"
            "              kind: STRING<__TypeKind>\n"
            "              name: STRING<String>?\n"
            "              ofType: {\n"
            "                kind: STRING<__TypeKind>\n"
            "                name: STRING<String>?\n"
            "                ofType: {\n"
            "                  kind: STRING<__TypeKind>\n"
            "                  name: STRING<String>?\n"
            "                  ofType: {\n"
            "                    kind: STRING<__TypeKind>\n"
            "                    name: STRING<String>?\n"
            "                    ofType: {\n"
            "                      kind: STRING<__TypeKind>\n"
            "                      name: STRING<String>?\n"
            "                      ofType: {\n"
            "                        kind: STRING<__TypeKind>\n"
            "                        name: STRING<String>?\n"
            "                        ofType: {\n"
            "                          kind: STRING<__TypeKind>\n"
            "                          name: STRING<String>?\n"
            "                        }?\n"
            "                      }?\n"
            "                    }?\n"
            "                  }?\n"
            "                }?\n"
            "              }?\n"
            "            }?\n"
            "          }\n"
            "          defaultValue: STRING<String>?\n"
            "        }[]?\n"
            "        interfaces: {\n"
            "          kind: STRING<__TypeKind>\n"
            "          name: STRING<String>?\n"
            "          ofType: {\n"
            "            kind: STRING<__TypeKind>\n"
            "            name: STRING<String>?\n"
            "            ofType: {\n"
            "              kind: STRING<__TypeKind>\n"
            "              name: STRING<String>?\n"
            "              ofType: {\n"
            "                kind: STRING<__TypeKind>\n"
            "                name: STRING<String>?\n"
            "                ofType: {\n"
            "                  kind: STRING<__TypeKind>\n"
            "                  name: STRING<String>?\n"
            "                  ofType: {\n"
            "                    kind: STRING<__TypeKind>\n"
            "                    name: STRING<String>?\n"
            "                    ofType: {\n"
            "                      kind: STRING<__TypeKind>\n"
            "                      name: STRING<String>?\n"
            "                      ofType: {\n"
            "                        kind: STRING<__TypeKind>\n"
            "                        name: STRING<String>?\n"
            "                      }?\n"
            "                    }?\n"
            "                  }?\n"
            "                }?\n"
            "              }?\n"
            "            }?\n"
            "          }?\n"
            "        }[]?\n"
            "        enumValues: {\n"
            "          name: STRING<String>\n"
            "          description: STRING<String>?\n"
            "          isDeprecated: BOOLEAN\n"
            "          deprecationReason: STRING<String>?\n"
            "        }[]?\n"
            "        possibleTypes: {\n"
            "          kind: STRING<__TypeKind>\n"
            "          name: STRING<String>?\n"
            "          ofType: {\n"
            "            kind: STRING<__TypeKind>\n"
            "            name: STRING<String>?\n"
            "            ofType: {\n"
            "              kind: STRING<__TypeKind>\n"
            "              name: STRING<String>?\n"
            "              ofType: {\n"
            "                kind: STRING<__TypeKind>\n"
            "                name: STRING<String>?\n"
            "                ofType: {\n"
            "                  kind: STRING<__TypeKind>\n"
            "                  name: STRING<String>?\n"
            "                  ofType: {\n"
            "                    kind: STRING<__TypeKind>\n"
            "                    name: STRING<String>?\n"
            "                    ofType: {\n"
            "                      kind: STRING<__TypeKind>\n"
            "                      name: STRING<String>?\n"
            "                      ofType: {\n"
            "                        kind: STRING<__TypeKind>\n"
            "                        name: STRING<String>?\n"
            "                      }?\n"
            "                    }?\n"
            "                  }?\n"
            "                }?\n"
            "              }?\n"
            "            }?\n"
            "          }?\n"
            "        }[]?\n"
            "      }[]\n"
            "      directives: {\n"
            "        name: STRING<String>\n"
            "        description: STRING<String>?\n"
            "        locations: STRING<__DirectiveLocation>[]\n"
            "        args: {\n"
            "          name: STRING<String>\n"
            "          description: STRING<String>?\n"
            "          type: {\n"
            "            kind: STRING<__TypeKind>\n"
            "            name: STRING<String>?\n"
            "            ofType: {\n"
            "              kind: STRING<__TypeKind>\n"
            "              name: STRING<String>?\n"
            "              ofType: {\n"
            "                kind: STRING<__TypeKind>\n"
            "                name: STRING<String>?\n"
            "                ofType: {\n"
            "                  kind: STRING<__TypeKind>\n"
            "                  name: STRING<String>?\n"
            "                  ofType: {\n"
            "                    kind: STRING<__TypeKind>\n"
            "                    name: STRING<String>?\n"
            "                    ofType: {\n"
            "                      kind: STRING<__TypeKind>\n"
            "                      name: STRING<String>?\n"
            "                      ofType: {\n"
            "                        kind: STRING<__TypeKind>\n"
            "                        name: STRING<String>?\n"
            "                        ofType: {\n"
            "                          kind: STRING<__TypeKind>\n"
            "                          name: STRING<String>?\n"
            "                        }?\n"
            "                      }?\n"
            "                    }?\n"
            "                  }?\n"
            "                }?\n"
            "              }?\n"
            "            }?\n"
            "          }\n"
            "          defaultValue: STRING<String>?\n"
            "        }[]\n"
            "      }[]\n"
            "    }\n"
            "  }?\n"
            "  errors?: ERROR[]\n"
            "  extensions?: EXTENSIONS\n"
            "}"
        >>,
    ?assertEqual(ExpectedWireType, ActualWireType),
    Value = argo_value:from_json(WireType, JsonValue),
    ActualValue = erlang:iolist_to_binary(argo:format(Value)),
    ExpectedValue =
        <<
            "{\n"
            "  data: NON_NULL({\n"
            "    __schema: {\n"
            "      queryType: {\n"
            "        name: NON_NULL(STRING(<<\"Q\">>)<String>)\n"
            "      }\n"
            "      mutationType: NULL\n"
            "      subscriptionType: NULL\n"
            "      types: [\n"
            "        {\n"
            "          kind: STRING(<<\"OBJECT\">>)<__TypeKind>\n"
            "          name: NON_NULL(STRING(<<\"Q\">>)<String>)\n"
            "          description: NULL\n"
            "          fields: NON_NULL([\n"
            "            {\n"
            "              name: STRING(<<\"q\">>)<String>\n"
            "              description: NULL\n"
            "              args: []\n"
            "              type: {\n"
            "                kind: STRING(<<\"OBJECT\">>)<__TypeKind>\n"
            "                name: NON_NULL(STRING(<<\"Q\">>)<String>)\n"
            "                ofType: NULL\n"
            "              }\n"
            "              isDeprecated: BOOLEAN(false)\n"
            "              deprecationReason: NULL\n"
            "            },\n"
            "          ])\n"
            "          inputFields: NULL\n"
            "          interfaces: NON_NULL([])\n"
            "          enumValues: NULL\n"
            "          possibleTypes: NULL\n"
            "        },\n"
            "      ]\n"
            "      directives: []\n"
            "    }\n"
            "  })\n"
            "  errors?: ABSENT\n"
            "  extensions?: ABSENT\n"
            "}"
        >>,
    ?assertEqual(ExpectedValue, ActualValue),
    ArgoEncoded = argo_value:to_writer(Value),
    ?assertMatch({<<>>, Value}, argo_value:from_reader(WireType, ArgoEncoded)),
    ArgoSelfDescribingHeader = argo_header:new(#{self_describing => true}),
    ArgoSelfDescribingEncoded = argo_value:to_writer(Value, ArgoSelfDescribingHeader),
    ?assertMatch({<<>>, Value}, argo_value:from_reader(WireType, ArgoSelfDescribingEncoded)),
    ExpectedJson = jsone:encode(JsonValue),
    ArgoEncodedJson = argo_value:to_json(Value),
    ActualJson = jsone:encode(ArgoEncodedJson),
    ?assertEqual(ExpectedJson, ActualJson),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
