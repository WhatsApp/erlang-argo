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
-module(argo_location_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_value.hrl").

%% API
-export([
    new/2
]).

%% Types
-type t() :: #argo_location_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Line, Column) -> LocationValue when
    Line :: argo_types:i64(), Column :: argo_types:i64(), LocationValue :: t().
new(Line, Column) when ?is_i64(Line) andalso ?is_i64(Column) ->
    #argo_location_value{line = Line, column = Column}.
