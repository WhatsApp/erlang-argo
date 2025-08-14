%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_service_document_prop).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-08-14"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_graphql.hrl").

%% Helpers
-export([
    report_not_equal/2
]).
%% Properties
-export([
    prop_roundtrip_service_document/1
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

-spec prop_roundtrip_service_document(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_service_document(_Config) ->
    ?FORALL(
        ServiceDocument,
        maybe_log_size(proper_argo_graphql_service_document:service_document()),
        begin
            Expected = ServiceDocument,
            Formatted = argo_graphql:format(ServiceDocument),
            Actual = argo_graphql_service_document:from_string(Formatted),
            ?WHENFAIL(
                begin
                    io:format(
                        "FAILURE: Expected (ServiceDocument) does not match Actual (ServiceDocument)~n"
                        "Expected (ServiceDocument):~n~0tp~n"
                        "Actual (ServiceDocument):~n~0tp~n"
                        "Expected (String):~n~ts~n"
                        "Actual (String):~n~ts~n",
                        [
                            Expected,
                            Actual,
                            argo:format_with_lines(Expected),
                            argo:format_with_lines(Actual)
                        ]
                    )
                end,
                Expected =:= Actual
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
