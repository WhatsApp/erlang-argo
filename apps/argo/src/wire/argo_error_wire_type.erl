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
-module(argo_error_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/0,
    expand_wire_type/1
]).

%% Types
-type t() :: #argo_error_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> ErrorWireType when ErrorWireType :: t().
new() ->
    #argo_error_wire_type{}.

-spec expand_wire_type(ErrorWireType) -> WireType when ErrorWireType :: t(), WireType :: argo_wire_type:t().
expand_wire_type(#argo_error_wire_type{}) ->
    % "message" Field
    MessageWireType = argo_wire_type:block(argo_label:self_describing_blocks_string()),
    MessageFieldWireType = argo_field_wire_type:new(<<"message">>, MessageWireType, false),
    % "locations" Field
    LocationWireType = argo_location_wire_type:expand_wire_type(argo_location_wire_type:new()),
    LocationsWireType = argo_wire_type:array(argo_array_wire_type:new(LocationWireType)),
    LocationsFieldWireType = argo_field_wire_type:new(<<"locations">>, LocationsWireType, true),
    % "path" Field
    PathWireType = argo_wire_type:path(),
    PathFieldWireType = argo_field_wire_type:new(<<"path">>, PathWireType, true),
    % "extensions" Field
    ExtensionsWireType = argo_extensions_wire_type:expand_wire_type(argo_extensions_wire_type:new()),
    ExtensionsFieldWireType = argo_field_wire_type:new(<<"extensions">>, ExtensionsWireType, true),
    % "Error" Record Wire Type
    RecordWireType1 = argo_record_wire_type:new(),
    RecordWireType2 = argo_record_wire_type:insert(RecordWireType1, MessageFieldWireType),
    RecordWireType3 = argo_record_wire_type:insert(RecordWireType2, LocationsFieldWireType),
    RecordWireType4 = argo_record_wire_type:insert(RecordWireType3, PathFieldWireType),
    RecordWireType5 = argo_record_wire_type:insert(RecordWireType4, ExtensionsFieldWireType),
    WireType = argo_wire_type:record(RecordWireType5),
    WireType.
