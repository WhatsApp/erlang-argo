%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_limits).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2024-05-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Public API
-export([
    varbit_limit/0,
    varbit_limit/1
]).

%% Types
-type varbit_limit() :: pos_integer().

-export_type([
    varbit_limit/0
]).

%% Macros
-define(DEFAULT_VARBIT_LIMIT, (100 * 7)).
-define(is_pos_integer(X), (is_integer(X) andalso (X) > 0)).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec varbit_limit() -> varbit_limit().
varbit_limit() ->
    case application:get_env(argo, varbit_limit, ?DEFAULT_VARBIT_LIMIT) of
        VarbitLimit when ?is_pos_integer(VarbitLimit) ->
            VarbitLimit
    end.

-spec varbit_limit(varbit_limit()) -> ok.
varbit_limit(VarbitLimit) when ?is_pos_integer(VarbitLimit) ->
    application:set_env(argo, varbit_limit, VarbitLimit).
