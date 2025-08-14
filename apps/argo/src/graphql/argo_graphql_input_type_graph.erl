%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_graphql_input_type_graph).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_graphql.hrl").

%% New API
-export([
    new/0
]).
%% Instance API
-export([
    add_dependency/3,
    add_input/2,
    is_valid_dependency/3,
    merge/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type dependency_type_name() :: argo_types:name().
-type input_type_deps() :: #{dependency_type_name() => []}.
-type input_type_name() :: argo_types:name().
-type inputs() :: #{
    input_type_name() => input_type_deps()
}.
-type t() :: #argo_graphql_input_type_graph{}.

-export_type([
    dependency_type_name/0,
    input_type_deps/0,
    input_type_name/0,
    inputs/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec new() -> InputObjectTypeDefinition when InputObjectTypeDefinition :: t().
new() ->
    #argo_graphql_input_type_graph{
        inputs = maps:new()
    }.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec add_dependency(InputTypeGraph, InputTypeName, DependencyTypeName) -> InputTypeGraph when
    InputTypeGraph :: t(), InputTypeName :: input_type_name(), DependencyTypeName :: dependency_type_name().
add_dependency(InputTypeGraph1 = #argo_graphql_input_type_graph{inputs = G1}, InputTypeName, DependencyTypeName) when
    is_binary(InputTypeName) andalso is_binary(DependencyTypeName)
->
    case is_valid_dependency(InputTypeGraph1, InputTypeName, DependencyTypeName) of
        false ->
            error_with_info(badarg, [InputTypeGraph1, InputTypeName, DependencyTypeName], #{
                3 =>
                    {invalid_dependency, #{
                        input_type_name => InputTypeName, dependency_type_name => DependencyTypeName
                    }}
            });
        true ->
            D1 = maps:get(InputTypeName, G1, maps:new()),
            D2 = D1#{DependencyTypeName => []},
            G2 = G1#{InputTypeName => D2},
            InputTypeGraph2 = InputTypeGraph1#argo_graphql_input_type_graph{inputs = G2},
            InputTypeGraph2
    end.

-spec add_input(InputTypeGraph, InputTypeName) -> InputTypeGraph when
    InputTypeGraph :: t(), InputTypeName :: input_type_name().
add_input(InputTypeGraph1 = #argo_graphql_input_type_graph{inputs = G1}, InputTypeName) when is_binary(InputTypeName) ->
    case maps:is_key(InputTypeName, G1) of
        false ->
            G2 = maps:put(InputTypeName, maps:new(), G1),
            InputTypeGraph2 = InputTypeGraph1#argo_graphql_input_type_graph{inputs = G2},
            InputTypeGraph2;
        true ->
            InputTypeGraph1
    end.

-spec merge(InputTypeGraph, InputTypeGraph) -> InputTypeGraph when InputTypeGraph :: t().
merge(InputTypeGraph1 = #argo_graphql_input_type_graph{}, #argo_graphql_input_type_graph{inputs = ExtendInputs}) ->
    maps:fold(
        fun(InputTypeName, Dependencies, InputTypeGraph2) ->
            maps:fold(
                fun(DependencyTypeName, [], InputTypeGraph3) ->
                    add_dependency(InputTypeGraph3, InputTypeName, DependencyTypeName)
                end,
                add_input(InputTypeGraph2, InputTypeName),
                Dependencies
            )
        end,
        InputTypeGraph1,
        ExtendInputs
    ).

-spec is_valid_dependency(InputTypeGraph, InputTypeName, DependencyTypeName) -> boolean() when
    InputTypeGraph :: t(), InputTypeName :: input_type_name(), DependencyTypeName :: dependency_type_name().
is_valid_dependency(#argo_graphql_input_type_graph{inputs = Graph}, InputTypeName, DependencyTypeName) when
    is_binary(InputTypeName) andalso is_binary(DependencyTypeName)
->
    not has_path(Graph, DependencyTypeName, InputTypeName).

%% @private
-spec has_path(G, U, V) -> HasPath when
    G :: inputs(),
    U :: argo_types:name(),
    V :: argo_types:name(),
    HasPath :: boolean().
has_path(G, U, V) ->
    case has_path(G, U, V, maps:new()) of
        {HasPath, _Visited} when is_boolean(HasPath) ->
            HasPath
    end.

%% @private
-spec has_path(G, U, V, Visited) -> {HasPath, Visited} when
    G :: inputs(),
    U :: argo_types:name(),
    V :: argo_types:name(),
    Visited :: #{argo_types:name() => []},
    HasPath :: boolean().
has_path(_G, V, V, Visited1) ->
    {true, Visited1};
has_path(G, U, V, Visited1) ->
    Visited2 = maps:put(U, [], Visited1),
    Iterator1 = maps:iterator(maps:get(U, G, maps:new())),
    has_path(G, U, V, Visited2, Iterator1).

%% @private
-spec has_path(G, U, V, Visited, Iterator) -> {HasPath, Visited} when
    G :: inputs(),
    U :: argo_types:name(),
    V :: argo_types:name(),
    Visited :: #{argo_types:name() => []},
    Iterator :: maps:iterator(argo_types:name(), []),
    HasPath :: boolean().
has_path(G, U, V, Visited1, Iterator1) ->
    case maps:next(Iterator1) of
        none ->
            {false, Visited1};
        {N, [], Iterator2} when is_map_key(N, Visited1) ->
            has_path(G, U, V, Visited1, Iterator2);
        {N, [], Iterator2} ->
            case has_path(G, N, V, Visited1) of
                {false, Visited2} ->
                    has_path(G, U, V, Visited2, Iterator2);
                {true, Visited2} ->
                    {true, Visited2}
            end
    end.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-compile({inline, [error_with_info/3]}).
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(
    _Key, {invalid_dependency, #{input_type_name := InputTypeName, dependency_type_name := DependencyTypeName}}
) ->
    io_lib:format("invalid dependency: ~0tp -> ~0tp", [InputTypeName, DependencyTypeName]);
format_error_description(_Key, Value) ->
    Value.
