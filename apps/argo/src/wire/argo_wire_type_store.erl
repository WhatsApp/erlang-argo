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
    from_reader/1,
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
    display(WireTypeStore, standard_io).

-spec display(WireTypeStore, IoDevice) -> ok when WireTypeStore :: t(), IoDevice :: io:device().
display(WireTypeStore = #argo_wire_type_store{}, IoDevice) when not is_list(IoDevice) ->
    Printer1 = argo_wire_type_printer:new_io_device(IoDevice),
    Printer2 = argo_wire_type_printer:print_wire_type_store(Printer1, WireTypeStore),
    argo_wire_type_printer:finalize(Printer2).

-spec format(WireTypeStore) -> Output when WireTypeStore :: t(), Output :: iolist().
format(WireTypeStore = #argo_wire_type_store{}) ->
    Printer1 = argo_wire_type_printer:new_string(),
    Printer2 = argo_wire_type_printer:print_wire_type_store(Printer1, WireTypeStore),
    argo_wire_type_printer:finalize(Printer2).

-spec from_reader(Reader) -> {Reader, WireTypeStore} when Reader :: binary(), WireTypeStore :: t().
from_reader(Reader1) when is_binary(Reader1) ->
    {Reader2, WireTypeDecoder1} = argo_wire_type_decoder:from_reader(Reader1),
    {WireTypeDecoder2, WireTypeStore} = argo_wire_type_decoder:decode_wire_type_store(WireTypeDecoder1),
    _ = WireTypeDecoder2,
    {Reader2, WireTypeStore}.

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
