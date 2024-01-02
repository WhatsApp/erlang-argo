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
-module(proper_argo_test).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("proper/include/proper.hrl").

%% Helper API
-export([
    complex/1,
    complexity/0,
    minsized/1,
    mostly/2,
    mostly_small_list/1,
    mostly_small_size/0,
    option/1,
    option_unlikely/1,
    with_complexity/1
]).

%% Macros
-define(COMPLEXITY, 'proper_argo_complexity').

%%%=============================================================================
%%% Helper API functions
%%%=============================================================================

-spec complex(RawType :: proper_types:raw_type()) -> proper_types:type().
complex(RawType) ->
    with_complexity(
        ?SIZED(Size, begin
            Complexity = complexity(),
            ComplexSize = max(1, Size bsr Complexity),
            ?LET(NewSize, mostly(range(1, min(ComplexSize, 4)), range(1, ComplexSize)), resize(NewSize, RawType))
        end)
    ).

-spec complexity() -> pos_integer().
complexity() ->
    case parameter(?COMPLEXITY, 0) of
        Complexity when is_integer(Complexity) andalso Complexity >= 0 ->
            max(Complexity, 1)
    end.

-spec minsized(RawType :: proper_types:raw_type()) -> proper_types:type().
minsized(RawType) ->
    ?LET(Raw, RawType, ?SIZED(Size, min(Raw, Size))).

-spec mostly(U :: proper_types:raw_type(), T :: proper_types:raw_type()) -> proper_types:type().
mostly(U, T) ->
    frequency([
        {100, U},
        {1, T}
    ]).

-spec mostly_small_list(RawType :: proper_types:raw_type()) -> proper_types:type().
mostly_small_list(RawType) ->
    mostly(
        frequency([
            {1 * complexity(), []},
            {1, [RawType]},
            {1, [RawType, RawType]}
        ]),
        list(RawType)
    ).

-spec mostly_small_size() -> proper_types:type().
mostly_small_size() ->
    mostly(
        frequency([
            {1 * complexity(), 0},
            {1, 1},
            {1, 2}
        ]),
        ?SIZED(Size, range(0, Size))
    ).

-spec option(T :: proper_types:type()) -> proper_types:type().
option(T) ->
    oneof([none, {some, T}]).

-spec option_unlikely(T :: proper_types:type()) -> proper_types:type().
option_unlikely(T) ->
    mostly(none, {some, T}).

-spec with_complexity(RawType :: proper_types:raw_type()) -> proper_types:type().
with_complexity(RawType) ->
    Complexity =
        case parameter(?COMPLEXITY, 0) of
            C when is_integer(C) andalso C >= 0 ->
                C + 1
        end,
    with_parameter(?COMPLEXITY, Complexity, RawType).
