%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_wire_type_store).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_debug_type).

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% argo_debug_type callbacks
-export([
    display/3,
    format/2
]).

%% Codec API
-export([
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
    find_entry/2,
    insert/2,
    insert/3
]).

%% Types
-type t() :: #argo_wire_type_store{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% argo_debug_type callbacks
%%%=============================================================================

-spec display(IoDevice, WireTypeStore, Options) -> ok when
    IoDevice :: io:device(), WireTypeStore :: t(), Options :: argo_wire_type_printer:options().
display(IoDevice, WireTypeStore = #argo_wire_type_store{}, Options) when
    not is_list(IoDevice) andalso is_map(Options)
->
    Printer1 = argo_wire_type_printer:new_io_device(IoDevice, Options),
    Printer2 = argo_wire_type_printer:print_wire_type_store(Printer1, WireTypeStore),
    case argo_wire_type_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(WireTypeStore, Options) -> Output when
    WireTypeStore :: t(), Options :: argo_wire_type_printer:options(), Output :: unicode:unicode_binary().
format(WireTypeStore = #argo_wire_type_store{}, Options) when is_map(Options) ->
    Printer1 = argo_wire_type_printer:new_string(Options),
    Printer2 = argo_wire_type_printer:print_wire_type_store(Printer1, WireTypeStore),
    case argo_wire_type_printer:finalize(Printer2) of
        Output when is_list(Output) ->
            argo_types:unicode_binary(Output)
    end.

%%%=============================================================================
%%% Codec API functions
%%%=============================================================================

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
find(WireTypeStore = #argo_wire_type_store{}, TypeName) when is_binary(TypeName) ->
    case find_entry(WireTypeStore, TypeName) of
        {ok, #argo_wire_type_store_entry{type = WireType}} ->
            {ok, WireType};
        error ->
            error
    end.

-spec find_entry(WireTypeStore, TypeName) -> {ok, WireTypeStoreEntry} | error when
    WireTypeStore :: t(), TypeName :: argo_types:name(), WireTypeStoreEntry :: argo_wire_type_store_entry:t().
find_entry(#argo_wire_type_store{types = Types}, TypeName) when is_binary(TypeName) ->
    argo_index_map:find(TypeName, Types).

-spec insert(WireTypeStore, WireTypeStoreEntry) -> WireTypeStore when
    WireTypeStore :: t(), WireTypeStoreEntry :: argo_wire_type_store_entry:t().
insert(
    WireTypeStore1 = #argo_wire_type_store{types = Types1},
    WireTypeStoreEntry = #argo_wire_type_store_entry{name = TypeName}
) when
    is_binary(TypeName)
->
    Types2 = argo_index_map:put(TypeName, WireTypeStoreEntry, Types1),
    WireTypeStore2 = WireTypeStore1#argo_wire_type_store{types = Types2},
    WireTypeStore2.

-spec insert(WireTypeStore, TypeName, WireType) -> WireTypeStore when
    WireTypeStore :: t(), TypeName :: argo_types:name(), WireType :: argo_wire_type:t().
insert(WireTypeStore = #argo_wire_type_store{}, TypeName, WireType = #argo_wire_type{}) when
    is_binary(TypeName)
->
    WireTypeStoreEntry = argo_wire_type_store_entry:new(TypeName, WireType),
    insert(WireTypeStore, WireTypeStoreEntry).
