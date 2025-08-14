%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_header_prop).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2024-05-10", modified => "2025-08-14"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_header.hrl").

%% Helpers
-export([
    report_not_equal/2
]).
%% Properties
-export([
    prop_roundtrip_http_argo_mode/1,
    prop_roundtrip_u64/1,
    prop_roundtrip_uint/1
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

-spec prop_roundtrip_http_argo_mode(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_http_argo_mode(_Config) ->
    ?FORALL(
        Header,
        proper_argo:header(),
        begin
            Encoded = argo_header:to_http_argo_mode(Header),
            Decoded = argo_header:from_http_argo_mode(Encoded),
            ?EQUALS(Header, Decoded)
        end
    ).

-spec prop_roundtrip_u64(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_u64(_Config) ->
    ?FORALL(
        Header,
        ?SUCHTHAT(
            #argo_header{user_flags = UserFlags},
            proper_argo:header(),
            (UserFlags =:= undefined orelse bit_size(UserFlags) =< (7 * 7))
        ),
        begin
            Encoded = argo_header:to_u64(Header),
            Decoded = argo_header:from_u64(Encoded),
            ?EQUALS(Header, Decoded)
        end
    ).

-spec prop_roundtrip_uint(ct_suite:ct_config()) -> proper:test().
prop_roundtrip_uint(_Config) ->
    ?FORALL(
        Header,
        proper_argo:header(),
        begin
            Encoded = argo_header:to_uint(Header),
            Decoded = argo_header:from_uint(Encoded),
            ?EQUALS(Header, Decoded)
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
