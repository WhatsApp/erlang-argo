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
%%% Created :  16 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_typer_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% Helpers
-export([
    report_not_equal/2
]).
%% Properties
-export([
    prop_roundtrip/1
]).

%% Macros
-define(EQUALS(A, B), ?WHENFAIL(report_not_equal(A, B), A =:= B)).
% -define(SHOULD_LOG_SIZE, true).

%%%=============================================================================
%%% Helpers
%%%=============================================================================

-spec report_not_equal(A, B) -> ok when A :: term(), B :: term().
report_not_equal(A, B) ->
    io:format("Expected:~n~0tp~nActual:~n~0tp~n", [A, B]).

%%%=============================================================================
%%% Properties
%%%=============================================================================

-spec prop_roundtrip(ct_suite:ct_config()) -> proper:test().
prop_roundtrip(_Config) ->
    ?FORALL(
        {
            #{
                service_document := ServiceDocument,
                executable_document := ExecutableDocument,
                operation_name := OptionOperationName,
                wire_type := WireType,
                value := Value
            },
            Header
        },
        maybe_log_size({proper_argo_typer:value(), proper_argo:header()}),
        begin
            Encoded = argo_value:to_writer(Value, Header),
            {<<>>, Decoded} = argo_value:from_reader(WireType, Encoded),
            ToWireType = argo_value:to_wire_type(Value),
            JsonEncoded = jsone:encode(argo_types:dynamic_cast(argo_value:to_json(Value))),
            PercentageSmaller = trunc(math:ceil((byte_size(Encoded) / byte_size(JsonEncoded)) * 100)),
            ?WHENFAIL(
                begin
                    io:format(
                        "FAILURE: roundtrip check failed~n"
                        "ServiceDocument (Record):~n~0tp~n"
                        "ExecutableDocument (Record):~n~0tp~n"
                        "OptionOperationName (Record):~n~0tp~n"
                        "WireType (Record):~n~0tp~n"
                        "Value (Record):~n~0tp~n"
                        "Header (Record):~n~0tp~n"
                        "ServiceDocument (String):~n~ts~n"
                        "ExecutableDocument (String):~n~ts~n"
                        "WireType (String):~n~ts~n"
                        "Value (String):~n~ts~n",
                        [
                            ServiceDocument,
                            ExecutableDocument,
                            OptionOperationName,
                            WireType,
                            Value,
                            Header,
                            argo_graphql:format_with_lines(ServiceDocument),
                            argo_graphql:format_with_lines(ExecutableDocument),
                            argo_wire_type:format(WireType),
                            argo_value:format(Value)
                        ]
                    )
                end,
                conjunction([
                    {encoder_and_decoder, ?EQUALS(Value, Decoded)},
                    {json_size_uncompressed, collect(PercentageSmaller, ?LAZY(true))},
                    {to_wire_type, ?EQUALS(WireType, ToWireType)}
                ])
            )
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-compile({inline, [maybe_log_size/1]}).
-spec maybe_log_size(RawType :: proper_types:raw_type()) -> proper_types:type().
-ifdef(SHOULD_LOG_SIZE).
maybe_log_size(RawType) ->
    ?SIZED(
        Size,
        begin
            io:format(user, "Size = ~p~n", [Size]),
            RawType
        end
    ).
-else.
maybe_log_size(RawType) ->
    proper_types:cook_outer(RawType).
-endif.
