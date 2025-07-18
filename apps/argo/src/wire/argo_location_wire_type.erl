%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_location_wire_type).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-07-17", modified => "2025-07-17"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/0,
    expand_wire_type/1
]).

%% Types
-type t() :: #argo_location_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> LocationWireType when LocationWireType :: t().
new() ->
    #argo_location_wire_type{}.

-spec expand_wire_type(LocationWireType) -> WireType when LocationWireType :: t(), WireType :: argo_wire_type:t().
expand_wire_type(#argo_location_wire_type{}) ->
    % "line" Field
    LineWireType = argo_wire_type:block(argo_label:self_describing_blocks_varint()),
    LineFieldWireType = argo_field_wire_type:new(<<"line">>, LineWireType, false),
    % "column" Field
    ColumnWireType = argo_wire_type:block(argo_label:self_describing_blocks_varint()),
    ColumnFieldWireType = argo_field_wire_type:new(<<"column">>, ColumnWireType, false),
    % "Location" Record Wire Type
    RecordWireType1 = argo_record_wire_type:new(),
    RecordWireType2 = argo_record_wire_type:insert(RecordWireType1, LineFieldWireType),
    RecordWireType3 = argo_record_wire_type:insert(RecordWireType2, ColumnFieldWireType),
    WireType = argo_wire_type:record(RecordWireType3),
    WireType.
