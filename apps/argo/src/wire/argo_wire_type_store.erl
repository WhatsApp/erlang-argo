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
-module(argo_wire_type_store).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Codec API
-export([
    display/1,
    display/2,
    format/1,
    format_with_lines/1,
    from_json/1,
    from_reader/1,
    to_json/1,
    to_writer/1,
    to_writer/2
]).

%% New API
-export([
    new/0
]).

%% Instance API
-export([
    find/2,
    insert/3
]).

%% Types
-type t() :: #argo_wire_type_store{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% Codec API functions
%%%=============================================================================

-spec display(WireTypeStore) -> ok when WireTypeStore :: t().
display(WireTypeStore = #argo_wire_type_store{}) ->
    display(standard_io, WireTypeStore).

-spec display(IoDevice, WireTypeStore) -> ok when IoDevice :: io:device(), WireTypeStore :: t().
display(IoDevice, WireTypeStore = #argo_wire_type_store{}) when not is_list(IoDevice) ->
    Printer1 = argo_wire_type_printer:new_io_device(IoDevice),
    Printer2 = argo_wire_type_printer:print_wire_type_store(Printer1, WireTypeStore),
    case argo_wire_type_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(WireTypeStore) -> Output when WireTypeStore :: t(), Output :: iolist().
format(WireTypeStore = #argo_wire_type_store{}) ->
    Printer1 = argo_wire_type_printer:new_string(),
    Printer2 = argo_wire_type_printer:print_wire_type_store(Printer1, WireTypeStore),
    case argo_wire_type_printer:finalize(Printer2) of
        Output when is_list(Output) ->
            Output
    end.

-spec format_with_lines(WireTypeStore) -> unicode:unicode_binary() when WireTypeStore :: t().
format_with_lines(WireTypeStore = #argo_wire_type_store{}) ->
    argo_types:format_with_lines(format(WireTypeStore)).

-spec from_json(JsonValue) -> WireTypeStore when
    JsonValue :: argo_json:json_value(), WireTypeStore :: t().
from_json(JsonValue) ->
    JsonWireTypeDecoder1 = argo_json_wire_type_decoder:new(),
    {JsonWireTypeDecoder2, WireTypeStore} = argo_json_wire_type_decoder:decode_wire_type_store(
        JsonWireTypeDecoder1, JsonValue
    ),
    _ = JsonWireTypeDecoder2,
    WireTypeStore.

-spec from_reader(Reader) -> {Reader, WireTypeStore} when Reader :: binary(), WireTypeStore :: t().
from_reader(Reader1) when is_binary(Reader1) ->
    {Reader2, WireTypeDecoder1} = argo_wire_type_decoder:from_reader(Reader1),
    {WireTypeDecoder2, WireTypeStore} = argo_wire_type_decoder:decode_wire_type_store(WireTypeDecoder1),
    _ = WireTypeDecoder2,
    {Reader2, WireTypeStore}.

-spec to_json(WireTypeStore) -> JsonValue when WireTypeStore :: t(), JsonValue :: argo_json:json_value().
to_json(WireTypeStore = #argo_wire_type_store{}) ->
    JsonWireTypeEncoder1 = argo_json_wire_type_encoder:new(#{strict => false}),
    {JsonWireTypeEncoder2, JsonValue} = argo_json_wire_type_encoder:encode_wire_type_store(
        JsonWireTypeEncoder1, WireTypeStore
    ),
    _ = JsonWireTypeEncoder2,
    JsonValue.

-spec to_writer(WireTypeStore) -> Writer when WireTypeStore :: t(), Writer :: binary().
to_writer(WireTypeStore = #argo_wire_type_store{}) ->
    to_writer(WireTypeStore, argo_header:new()).

-spec to_writer(WireTypeStore, Header) -> Writer when
    WireTypeStore :: t(), Header :: argo_header:t(), Writer :: binary().
to_writer(WireTypeStore = #argo_wire_type_store{}, Header = #argo_header{}) ->
    WireTypeEncoder1 = argo_wire_type_encoder:new(Header),
    WireTypeEncoder2 = argo_wire_type_encoder:encode_wire_type_store(WireTypeEncoder1, WireTypeStore),
    argo_wire_type_encoder:to_writer(WireTypeEncoder2).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec new() -> WireTypeStore when WireTypeStore :: t().
new() ->
    #argo_wire_type_store{types = argo_index_map:new()}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec find(WireTypeStore, TypeName) -> {ok, WireType} | error when
    WireTypeStore :: t(), TypeName :: argo_types:name(), WireType :: argo_wire_type:t().
find(#argo_wire_type_store{types = Types}, TypeName) when is_binary(TypeName) ->
    argo_index_map:find(TypeName, Types).

-spec insert(WireTypeStore, TypeName, WireType) -> WireTypeStore when
    WireTypeStore :: t(), TypeName :: argo_types:name(), WireType :: argo_wire_type:t().
insert(WireTypeStore1 = #argo_wire_type_store{types = Types1}, TypeName, WireType = #argo_wire_type{}) when
    is_binary(TypeName)
->
    Types2 = argo_index_map:put(TypeName, WireType, Types1),
    WireTypeStore2 = WireTypeStore1#argo_wire_type_store{types = Types2},
    WireTypeStore2.
