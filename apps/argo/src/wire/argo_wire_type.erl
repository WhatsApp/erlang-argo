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
-module(argo_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_value.hrl").
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
    to_json/2,
    to_writer/1,
    to_writer/2
]).

%% New API
-export([
    array/1,
    block/1,
    desc/0,
    error/0,
    extensions/0,
    nullable/1,
    path/0,
    record/1,
    scalar/1
]).

%% Instance API
-export([
    fold_path_values/3,
    is_array/1,
    is_block/1,
    is_desc/1,
    is_error/1,
    is_extensions/1,
    is_labeled/1,
    is_nullable/1,
    is_path/1,
    is_record/1,
    is_scalar/1
]).

%% Types
-type inner() ::
    argo_array_wire_type:t()
    | argo_block_wire_type:t()
    | argo_desc_wire_type:t()
    | argo_error_wire_type:t()
    | argo_extensions_wire_type:t()
    | argo_nullable_wire_type:t()
    | argo_path_wire_type:t()
    | argo_record_wire_type:t()
    | argo_scalar_wire_type:t().

-type t() :: #argo_wire_type{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% Codec API functions
%%%=============================================================================

-spec display(WireType) -> ok when WireType :: t().
display(WireType = #argo_wire_type{}) ->
    display(standard_io, WireType).

-spec display(IoDevice, WireType) -> ok when IoDevice :: io:device(), WireType :: t().
display(IoDevice, WireType = #argo_wire_type{}) when not is_list(IoDevice) ->
    Printer1 = argo_wire_type_printer:new_io_device(IoDevice),
    Printer2 = argo_wire_type_printer:print_wire_type(Printer1, WireType),
    case argo_wire_type_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(WireType) -> Output when WireType :: t(), Output :: iolist().
format(WireType = #argo_wire_type{}) ->
    Printer1 = argo_wire_type_printer:new_string(),
    Printer2 = argo_wire_type_printer:print_wire_type(Printer1, WireType),
    case argo_wire_type_printer:finalize(Printer2) of
        Output when is_list(Output) ->
            Output
    end.

-spec format_with_lines(WireType) -> unicode:unicode_binary() when WireType :: t().
format_with_lines(WireType = #argo_wire_type{}) ->
    argo_types:format_with_lines(format(WireType)).

-spec from_json(JsonValue) -> WireType when
    JsonValue :: argo_json:json_value(), WireType :: t().
from_json(JsonValue) ->
    JsonWireTypeDecoder1 = argo_json_wire_type_decoder:new(),
    {JsonWireTypeDecoder2, WireType} = argo_json_wire_type_decoder:decode_wire_type(JsonWireTypeDecoder1, JsonValue),
    _ = JsonWireTypeDecoder2,
    WireType.

-spec from_reader(Reader) -> {Reader, WireType} when Reader :: binary(), WireType :: t().
from_reader(Reader1) when is_binary(Reader1) ->
    {Reader2, WireTypeDecoder1} = argo_wire_type_decoder:from_reader(Reader1),
    {WireTypeDecoder2, WireType} = argo_wire_type_decoder:decode_wire_type(WireTypeDecoder1),
    _ = WireTypeDecoder2,
    {Reader2, WireType}.

-spec to_json(WireType) -> JsonValue when WireType :: t(), JsonValue :: argo_json:json_value().
to_json(WireType = #argo_wire_type{}) ->
    to_json(WireType, #{strict => false}).

-spec to_json(WireType, Options) -> JsonValue when
    WireType :: t(), Options :: argo_json_wire_type_encoder:options(), JsonValue :: argo_json:json_value().
to_json(WireType = #argo_wire_type{}, Options) when is_map(Options) ->
    JsonWireTypeEncoder1 = argo_json_wire_type_encoder:new(Options),
    {JsonWireTypeEncoder2, JsonValue} = argo_json_wire_type_encoder:encode_wire_type(JsonWireTypeEncoder1, WireType),
    _ = JsonWireTypeEncoder2,
    JsonValue.

-spec to_writer(WireType) -> Writer when WireType :: t(), Writer :: binary().
to_writer(WireType = #argo_wire_type{}) ->
    to_writer(WireType, argo_header:new()).

-spec to_writer(WireType, Header) -> Writer when WireType :: t(), Header :: argo_header:t(), Writer :: binary().
to_writer(WireType = #argo_wire_type{}, Header = #argo_header{}) ->
    WireTypeEncoder1 = argo_wire_type_encoder:new(Header),
    WireTypeEncoder2 = argo_wire_type_encoder:encode_wire_type(WireTypeEncoder1, WireType),
    argo_wire_type_encoder:to_writer(WireTypeEncoder2).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec array(ArrayWireType) -> WireType when ArrayWireType :: argo_array_wire_type:t(), WireType :: t().
array(ArrayWireType = #argo_array_wire_type{}) ->
    #argo_wire_type{inner = ArrayWireType}.

-spec block(BlockWireType) -> WireType when BlockWireType :: argo_block_wire_type:t(), WireType :: t().
block(BlockWireType = #argo_block_wire_type{}) ->
    #argo_wire_type{inner = BlockWireType}.

-spec desc() -> WireType when WireType :: t().
desc() ->
    #argo_wire_type{inner = #argo_desc_wire_type{}}.

-spec error() -> WireType when WireType :: t().
error() ->
    #argo_wire_type{inner = #argo_error_wire_type{}}.

-spec extensions() -> WireType when WireType :: t().
extensions() ->
    #argo_wire_type{inner = #argo_extensions_wire_type{}}.

-spec nullable(NullableWireType) -> WireType when NullableWireType :: argo_nullable_wire_type:t(), WireType :: t().
nullable(NullableWireType = #argo_nullable_wire_type{}) ->
    #argo_wire_type{inner = NullableWireType}.

-spec path() -> WireType when WireType :: t().
path() ->
    #argo_wire_type{inner = #argo_path_wire_type{}}.

-spec record(RecordWireType) -> WireType when RecordWireType :: argo_record_wire_type:t(), WireType :: t().
record(RecordWireType = #argo_record_wire_type{}) ->
    #argo_wire_type{inner = RecordWireType}.

-spec scalar(ScalarWireType) -> WireType when ScalarWireType :: argo_scalar_wire_type:t(), WireType :: t().
scalar(ScalarWireType = #argo_scalar_wire_type{}) ->
    #argo_wire_type{inner = ScalarWireType}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec fold_path_values(Function, AccIn, WireType) -> AccOut when
    Function :: fun((PathValue, AccIn) -> AccOut),
    AccIn :: dynamic(),
    WireType :: argo_wire_type:t(),
    PathValue :: argo_path_value:t(),
    AccOut :: dynamic().
fold_path_values(Function, Init, WireType = #argo_wire_type{}) when is_function(Function, 2) ->
    fold_path_values(WireType, Function, Init, argo_path_value:new()).

-spec is_array(WireType) -> boolean() when WireType :: t().
is_array(#argo_wire_type{inner = #argo_array_wire_type{}}) -> true;
is_array(#argo_wire_type{}) -> false.

-spec is_block(WireType) -> boolean() when WireType :: t().
is_block(#argo_wire_type{inner = #argo_block_wire_type{}}) -> true;
is_block(#argo_wire_type{}) -> false.

-spec is_desc(WireType) -> boolean() when WireType :: t().
is_desc(#argo_wire_type{inner = #argo_desc_wire_type{}}) -> true;
is_desc(#argo_wire_type{}) -> false.

-spec is_error(WireType) -> boolean() when WireType :: t().
is_error(#argo_wire_type{inner = #argo_error_wire_type{}}) -> true;
is_error(#argo_wire_type{}) -> false.

-spec is_extensions(WireType) -> boolean() when WireType :: t().
is_extensions(#argo_wire_type{inner = #argo_extensions_wire_type{}}) -> true;
is_extensions(#argo_wire_type{}) -> false.

-spec is_labeled(WireType) -> boolean() when WireType :: t().
is_labeled(#argo_wire_type{inner = ScalarWireType = #argo_scalar_wire_type{}}) ->
    argo_scalar_wire_type:is_labeled(ScalarWireType);
is_labeled(#argo_wire_type{inner = BlockWireType = #argo_block_wire_type{}}) ->
    argo_scalar_wire_type:is_labeled(BlockWireType#argo_block_wire_type.'of');
is_labeled(#argo_wire_type{inner = #argo_nullable_wire_type{}}) ->
    true;
is_labeled(#argo_wire_type{inner = #argo_array_wire_type{}}) ->
    true;
is_labeled(#argo_wire_type{}) ->
    false.

-spec is_nullable(WireType) -> boolean() when WireType :: t().
is_nullable(#argo_wire_type{inner = #argo_nullable_wire_type{}}) -> true;
is_nullable(#argo_wire_type{}) -> false.

-spec is_path(WireType) -> boolean() when WireType :: t().
is_path(#argo_wire_type{inner = #argo_path_wire_type{}}) -> true;
is_path(#argo_wire_type{}) -> false.

-spec is_record(WireType) -> boolean() when WireType :: t().
is_record(#argo_wire_type{inner = #argo_record_wire_type{}}) -> true;
is_record(#argo_wire_type{}) -> false.

-spec is_scalar(WireType) -> boolean() when WireType :: t().
is_scalar(#argo_wire_type{inner = #argo_scalar_wire_type{}}) -> true;
is_scalar(#argo_wire_type{}) -> false.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec fold_path_values(WireType, Function, AccIn, PathValue) -> AccOut when
    Function :: fun((PathValue, AccIn) -> AccOut),
    AccIn :: dynamic(),
    WireType :: argo_wire_type:t(),
    PathValue :: argo_path_value:t(),
    AccOut :: dynamic().
fold_path_values(WireType = #argo_wire_type{}, Function, Acc1, PathValue1 = #argo_path_value{}) when
    is_function(Function, 2)
->
    case WireType#argo_wire_type.inner of
        #argo_array_wire_type{'of' = Of} ->
            PathValue2 = argo_path_value:push_list_index(PathValue1, 0),
            Acc2 = Function(PathValue2, Acc1),
            fold_path_values(Of, Function, Acc2, PathValue2);
        #argo_block_wire_type{} ->
            Acc1;
        #argo_desc_wire_type{} ->
            Acc1;
        #argo_error_wire_type{} ->
            Acc1;
        #argo_extensions_wire_type{} ->
            Acc1;
        #argo_nullable_wire_type{'of' = Of} ->
            fold_path_values(Of, Function, Acc1, PathValue1);
        #argo_path_wire_type{} ->
            Acc1;
        #argo_record_wire_type{fields = Fields} ->
            Acc2 = argo_index_map:foldl(
                fun(_Index, FieldName, #argo_field_wire_type{type = FieldType}, Acc1_Acc1) ->
                    PathValue2 = argo_path_value:push_field_name(PathValue1, FieldName),
                    Acc1_Acc2 = Function(PathValue2, Acc1_Acc1),
                    Acc1_Acc3 = fold_path_values(FieldType, Function, Acc1_Acc2, PathValue2),
                    Acc1_Acc3
                end,
                Acc1,
                Fields
            ),
            Acc2;
        #argo_scalar_wire_type{} ->
            Acc1
    end.
