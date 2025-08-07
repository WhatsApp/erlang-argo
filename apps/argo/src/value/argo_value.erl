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
-module(argo_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(argo_debug_type).

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% argo_debug_type callbacks
-export([
    display/3,
    format/2
]).

%% Codec API
-export([
    from_erlang/2,
    from_erlang/4,
    from_json/2,
    from_json/4,
    from_reader/2,
    from_term/4,
    to_erlang/1,
    to_erlang/3,
    to_json/1,
    to_json/3,
    to_term/3,
    to_writer/1,
    to_writer/2
]).

%% New API
-export([
    array/1,
    block/1,
    desc/1,
    error/1,
    extensions/1,
    nullable/1,
    path/1,
    record/1,
    scalar/1
]).

%% Instance API
-export([
    is_array/1,
    is_block/1,
    is_desc/1,
    is_error/1,
    is_extensions/1,
    is_labeled/1,
    is_null/1,
    is_nullable/1,
    is_path/1,
    is_record/1,
    is_scalar/1,
    to_wire_type/1,
    xform/3
]).

-type inner() ::
    argo_array_value:t()
    | argo_block_value:t()
    | argo_desc_value:t()
    | argo_error_value:t()
    | argo_extensions_value:t()
    | argo_nullable_value:t()
    | argo_path_value:t()
    | argo_record_value:t()
    | argo_scalar_value:t().

-type t() :: #argo_value{}.

-type xform_action() :: cont | skip.
-type xform_result(TypeOut, AccOut) :: xform_action() | {xform_action(), AccOut} | {xform_action(), TypeOut, AccOut}.
-type xform_func(Type, Acc) :: xform_func(Type, Acc, Type, Acc).
-type xform_func(TypeIn, AccIn, TypeOut, AccOut) :: fun((TypeIn, AccIn) -> xform_result(TypeOut, AccOut)).

-export_type([
    inner/0,
    t/0,
    xform_action/0,
    xform_result/2,
    xform_func/2,
    xform_func/4
]).

%%%=============================================================================
%%% argo_debug_type callbacks
%%%=============================================================================

-spec display(IoDevice, Value, Options) -> ok when
    IoDevice :: io:device(), Value :: t(), Options :: argo_value_printer:options().
display(IoDevice, Value = #argo_value{}, Options) when not is_list(IoDevice) andalso is_map(Options) ->
    Printer1 = argo_value_printer:new_io_device(IoDevice, Options),
    Printer2 = argo_value_printer:print_value(Printer1, Value),
    case argo_value_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(Value, Options) -> Output when
    Value :: t(), Options :: argo_value_printer:options(), Output :: unicode:unicode_binary().
format(Value = #argo_value{}, Options) when is_map(Options) ->
    Printer1 = argo_value_printer:new_string(Options),
    Printer2 = argo_value_printer:print_value(Printer1, Value),
    case argo_value_printer:finalize(Printer2) of
        Output when is_list(Output) ->
            argo_types:unicode_binary(Output)
    end.

%%%=============================================================================
%%% Codec API functions
%%%=============================================================================

-spec from_erlang(WireType, TermValue) -> Value when
    WireType :: argo_wire_type:t(), TermValue :: argo_term:term_value(), Value :: t().
from_erlang(WireType = #argo_wire_type{}, TermValue) ->
    Value = from_term(WireType, TermValue, argo_erlang_term_value_decoder, #{}),
    Value.

-spec from_erlang(WireType, TermValue, TermScalarDecoderModule, TermScalarDecoderOptions) -> Value when
    WireType :: argo_wire_type:t(),
    TermValue :: argo_term:term_value(),
    TermScalarDecoderModule :: module(),
    TermScalarDecoderOptions :: argo_erlang_term_scalar_decoder:options(),
    Value :: t().
from_erlang(WireType = #argo_wire_type{}, TermValue, TermScalarDecoderModule, TermScalarDecoderOptions) when
    is_atom(TermScalarDecoderModule)
->
    Value = from_term(WireType, TermValue, argo_erlang_term_value_decoder, #{
        scalar_decoder_module => TermScalarDecoderModule,
        scalar_decoder_options => TermScalarDecoderOptions
    }),
    Value.

-spec from_json(WireType, JsonValue) -> Value when
    WireType :: argo_wire_type:t(), JsonValue :: argo_json:json_value(), Value :: t().
from_json(WireType = #argo_wire_type{}, JsonValue) ->
    JsonValueDecoder1 = argo_json_value_decoder:new(),
    {JsonValueDecoder2, Value} = argo_json_value_decoder:decode_wire_type(JsonValueDecoder1, WireType, JsonValue),
    _ = JsonValueDecoder2,
    Value.

-spec from_json(WireType, JsonValue, JsonScalarDecoderModule, JsonScalarDecoderOptions) -> Value when
    WireType :: argo_wire_type:t(),
    JsonValue :: argo_json:json_value(),
    JsonScalarDecoderModule :: module(),
    JsonScalarDecoderOptions :: argo_json_scalar_decoder:options(),
    Value :: t().
from_json(WireType = #argo_wire_type{}, JsonValue, JsonScalarDecoderModule, JsonScalarDecoderOptions) when
    is_atom(JsonScalarDecoderModule)
->
    JsonValueDecoder1 = argo_json_value_decoder:new(JsonScalarDecoderModule, JsonScalarDecoderOptions),
    {JsonValueDecoder2, Value} = argo_json_value_decoder:decode_wire_type(JsonValueDecoder1, WireType, JsonValue),
    _ = JsonValueDecoder2,
    Value.

-spec from_reader(WireType, Reader) -> {Reader, Value} when
    WireType :: argo_wire_type:t(), Reader :: binary(), Value :: t().
from_reader(WireType = #argo_wire_type{}, Reader1) when is_binary(Reader1) ->
    {Reader2, ValueDecoder1} = argo_value_decoder:from_reader(Reader1),
    {ValueDecoder2, Value} = argo_value_decoder:decode_wire_type(ValueDecoder1, WireType),
    _ = ValueDecoder2,
    {Reader2, Value}.

-spec from_term(WireType, TermValue, TermValueDecoderModule, TermValueDecoderOptions) -> Value when
    WireType :: argo_wire_type:t(),
    TermValue :: argo_term:term_value(),
    TermValueDecoderModule :: module(),
    TermValueDecoderOptions :: argo_term_value_decoder:options(),
    Value :: t().
from_term(WireType = #argo_wire_type{}, TermValue, TermValueDecoderModule, TermValueDecoderOptions) when
    is_atom(TermValueDecoderModule)
->
    TermValueDecoder1 = argo_term_value_decoder:new(TermValueDecoderModule, TermValueDecoderOptions),
    {TermValueDecoder2, Value} = argo_term_value_decoder:decode_wire_type(TermValueDecoder1, WireType, TermValue),
    _ = TermValueDecoder2,
    Value.

-spec to_erlang(Value) -> TermValue when
    Value :: t(), TermValue :: argo_term:term_value().
to_erlang(Value = #argo_value{}) ->
    TermValue = to_term(Value, argo_erlang_term_value_encoder, #{}),
    TermValue.

-spec to_erlang(Value, TermScalarEncoderModule, TermScalarEncoderOptions) -> TermValue when
    Value :: t(),
    TermScalarEncoderModule :: module(),
    TermScalarEncoderOptions :: argo_erlang_term_scalar_encoder:options(),
    TermValue :: argo_term:term_value().
to_erlang(Value = #argo_value{}, TermScalarEncoderModule, TermScalarEncoderOptions) when
    is_atom(TermScalarEncoderModule)
->
    TermValue = to_term(Value, argo_erlang_term_value_encoder, #{
        scalar_encoder_module => TermScalarEncoderModule,
        scalar_encoder_options => TermScalarEncoderOptions
    }),
    TermValue.

-spec to_json(Value) -> JsonValue when Value :: t(), JsonValue :: argo_json:json_value().
to_json(Value = #argo_value{}) ->
    JsonValueEncoder1 = argo_json_value_encoder:new(),
    {JsonValueEncoder2, JsonValue} = argo_json_value_encoder:encode_value(JsonValueEncoder1, Value),
    _ = JsonValueEncoder2,
    JsonValue.

-spec to_json(Value, JsonScalarEncoderModule, JsonScalarEncoderOptions) -> JsonValue when
    Value :: t(),
    JsonScalarEncoderModule :: module(),
    JsonScalarEncoderOptions :: argo_json_scalar_encoder:options(),
    JsonValue :: argo_json:json_value().
to_json(Value = #argo_value{}, JsonScalarEncoderModule, JsonScalarEncoderOptions) when
    is_atom(JsonScalarEncoderModule)
->
    JsonValueEncoder1 = argo_json_value_encoder:new(JsonScalarEncoderModule, JsonScalarEncoderOptions),
    {JsonValueEncoder2, JsonValue} = argo_json_value_encoder:encode_value(JsonValueEncoder1, Value),
    _ = JsonValueEncoder2,
    JsonValue.

-spec to_term(Value, TermValueEncoderModule, TermValueEncoderOptions) -> TermValue when
    Value :: t(),
    TermValueEncoderModule :: module(),
    TermValueEncoderOptions :: argo_term_value_encoder:options(),
    TermValue :: argo_term:term_value().
to_term(Value = #argo_value{}, TermValueEncoderModule, TermValueEncoderOptions) when
    is_atom(TermValueEncoderModule)
->
    TermValueEncoder1 = argo_term_value_encoder:new(TermValueEncoderModule, TermValueEncoderOptions),
    {TermValueEncoder2, TermValue} = argo_term_value_encoder:encode_value(TermValueEncoder1, Value),
    _ = TermValueEncoder2,
    TermValue.

-spec to_writer(Value) -> Writer when Value :: t(), Writer :: binary().
to_writer(Value = #argo_value{}) ->
    to_writer(Value, argo_header:new()).

-spec to_writer(Value, Header) -> Writer when Value :: t(), Header :: argo_header:t(), Writer :: binary().
to_writer(Value = #argo_value{}, Header = #argo_header{}) ->
    ValueEncoder1 = argo_value_encoder:new(Header),
    ValueEncoder2 = argo_value_encoder:encode_value(ValueEncoder1, Value),
    argo_value_encoder:to_writer(ValueEncoder2).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec array(ArrayValue) -> Value when ArrayValue :: argo_array_value:t(), Value :: t().
array(ArrayValue = #argo_array_value{}) ->
    #argo_value{inner = ArrayValue}.

-spec block(BlockValue) -> Value when BlockValue :: argo_block_value:t(), Value :: t().
block(BlockValue = #argo_block_value{}) ->
    #argo_value{inner = BlockValue}.

-spec desc(DescValue) -> Value when DescValue :: argo_desc_value:t(), Value :: t().
desc(DescValue = #argo_desc_value{}) ->
    #argo_value{inner = DescValue}.

-spec error(ErrorValue) -> Value when ErrorValue :: argo_error_value:t(), Value :: t().
error(ErrorValue = #argo_error_value{}) ->
    #argo_value{inner = ErrorValue}.

-spec extensions(ExtensionsValue) -> Value when ExtensionsValue :: argo_extensions_value:t(), Value :: t().
extensions(ExtensionsValue = #argo_extensions_value{}) ->
    #argo_value{inner = ExtensionsValue}.

-spec nullable(NullableValue) -> Value when NullableValue :: argo_nullable_value:t(), Value :: t().
nullable(NullableValue = #argo_nullable_value{}) ->
    #argo_value{inner = NullableValue}.

-spec path(PathValue) -> Value when PathValue :: argo_path_value:t(), Value :: t().
path(PathValue = #argo_path_value{}) ->
    #argo_value{inner = PathValue}.

-spec record(RecordValue) -> Value when RecordValue :: argo_record_value:t(), Value :: t().
record(RecordValue = #argo_record_value{}) ->
    #argo_value{inner = RecordValue}.

-spec scalar(ScalarValue) -> Value when ScalarValue :: argo_scalar_value:t(), Value :: t().
scalar(ScalarValue = #argo_scalar_value{}) ->
    #argo_value{inner = ScalarValue}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec is_array(Value) -> boolean() when Value :: t().
is_array(#argo_value{inner = #argo_array_value{}}) -> true;
is_array(#argo_value{}) -> false.

-spec is_block(Value) -> boolean() when Value :: t().
is_block(#argo_value{inner = #argo_block_value{}}) -> true;
is_block(#argo_value{}) -> false.

-spec is_desc(Value) -> boolean() when Value :: t().
is_desc(#argo_value{inner = #argo_desc_value{}}) -> true;
is_desc(#argo_value{}) -> false.

-spec is_error(Value) -> boolean() when Value :: t().
is_error(#argo_value{inner = #argo_error_value{}}) -> true;
is_error(#argo_value{}) -> false.

-spec is_extensions(Value) -> boolean() when Value :: t().
is_extensions(#argo_value{inner = #argo_extensions_value{}}) -> true;
is_extensions(#argo_value{}) -> false.

-spec is_labeled(Value) -> boolean() when Value :: t().
is_labeled(#argo_value{inner = ScalarValue = #argo_scalar_value{}}) ->
    argo_scalar_value:is_labeled(ScalarValue);
is_labeled(#argo_value{inner = BlockValue = #argo_block_value{}}) ->
    argo_scalar_value:is_labeled(BlockValue#argo_block_value.value);
is_labeled(#argo_value{inner = #argo_nullable_value{}}) ->
    true;
is_labeled(#argo_value{inner = #argo_array_value{}}) ->
    true;
is_labeled(#argo_value{}) ->
    false.

-spec is_null(Value) -> boolean() when Value :: t().
is_null(#argo_value{inner = BlockValue = #argo_block_value{}}) ->
    argo_block_value:is_null(BlockValue);
is_null(#argo_value{inner = DescValue = #argo_desc_value{}}) ->
    argo_desc_value:is_null(DescValue);
is_null(#argo_value{inner = NullableValue = #argo_nullable_value{}}) ->
    argo_nullable_value:is_null(NullableValue);
is_null(#argo_value{inner = ScalarValue = #argo_scalar_value{}}) ->
    argo_scalar_value:is_null(ScalarValue);
is_null(#argo_value{}) ->
    false.

-spec is_nullable(Value) -> boolean() when Value :: t().
is_nullable(#argo_value{inner = #argo_nullable_value{}}) -> true;
is_nullable(#argo_value{}) -> false.

-spec is_path(Value) -> boolean() when Value :: t().
is_path(#argo_value{inner = #argo_path_value{}}) -> true;
is_path(#argo_value{}) -> false.

-spec is_record(Value) -> boolean() when Value :: t().
is_record(#argo_value{inner = #argo_record_value{}}) -> true;
is_record(#argo_value{}) -> false.

-spec is_scalar(Value) -> boolean() when Value :: t().
is_scalar(#argo_value{inner = #argo_scalar_value{}}) -> true;
is_scalar(#argo_value{}) -> false.

-spec to_wire_type(Value) -> WireType when Value :: t(), WireType :: argo_wire_type:t().
to_wire_type(#argo_value{inner = ArrayValue = #argo_array_value{}}) ->
    ArrayWireType = argo_array_value:to_array_wire_type(ArrayValue),
    argo_wire_type:array(ArrayWireType);
to_wire_type(#argo_value{inner = BlockValue = #argo_block_value{}}) ->
    BlockWireType = argo_block_value:to_block_wire_type(BlockValue),
    argo_wire_type:block(BlockWireType);
to_wire_type(#argo_value{inner = #argo_desc_value{}}) ->
    argo_wire_type:desc();
to_wire_type(#argo_value{inner = #argo_error_value{}}) ->
    argo_wire_type:error();
to_wire_type(#argo_value{inner = #argo_extensions_value{}}) ->
    argo_wire_type:extensions();
to_wire_type(#argo_value{inner = NullableValue = #argo_nullable_value{}}) ->
    NullableWireType = argo_nullable_value:to_nullable_wire_type(NullableValue),
    argo_wire_type:nullable(NullableWireType);
to_wire_type(#argo_value{inner = #argo_path_value{}}) ->
    argo_wire_type:path();
to_wire_type(#argo_value{inner = RecordValue = #argo_record_value{}}) ->
    RecordWireType = argo_record_value:to_record_wire_type(RecordValue),
    argo_wire_type:record(RecordWireType);
to_wire_type(#argo_value{inner = ScalarValue = #argo_scalar_value{}}) ->
    ScalarWireType = argo_scalar_value:to_scalar_wire_type(ScalarValue),
    argo_wire_type:scalar(ScalarWireType).

-spec xform(TypeIn, AccIn, Fun) -> {TypeOut, AccOut} when
    TypeIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(TypeIn, AccIn, TypeOut, AccOut),
    TypeOut :: dynamic(),
    AccOut :: dynamic().
xform(T1, Acc1, Fun) when is_function(Fun, 2) ->
    case xform_normalize(T1, Acc1, Fun(T1, Acc1)) of
        {cont, T2, Acc2} ->
            case T2 of
                #argo_array_value{items = Items1} ->
                    {Items2, Acc3} = lists:foldl(
                        fun(Item1, {Items1_Acc1, Acc2_Acc1}) ->
                            {Item2, Acc2_Acc2} = xform(Item1, Acc2_Acc1, Fun),
                            Items1_Acc2 = [Item2 | Items1_Acc1],
                            {Items1_Acc2, Acc2_Acc2}
                        end,
                        {[], Acc2},
                        Items1
                    ),
                    T3 = T2#argo_array_value{items = lists:reverse(Items2)},
                    {T3, Acc3};
                #argo_block_value{value = ScalarValue1} ->
                    {ScalarValue2, Acc3} = xform(ScalarValue1, Acc2, Fun),
                    T3 = T2#argo_block_value{value = ScalarValue2},
                    {T3, Acc3};
                #argo_desc_value{inner = {object, DescObject1}} ->
                    {DescObject2, Acc3} = argo_index_map:foldl(
                        fun(_Index, DescKey, DescValue1, {DescObject1_Acc1, Acc2_Acc1}) ->
                            {DescValue2, Acc2_Acc2} = xform(DescValue1, Acc2_Acc1, Fun),
                            DescObject1_Acc2 = argo_index_map:put(DescKey, DescValue2, DescObject1_Acc1),
                            {DescObject1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        DescObject1
                    ),
                    T3 = T2#argo_desc_value{inner = {object, DescObject2}},
                    {T3, Acc3};
                #argo_desc_value{inner = {list, DescList1}} ->
                    {DescList2, Acc3} = lists:foldl(
                        fun(DescValue1, {DescList1_Acc1, Acc2_Acc1}) ->
                            {DescValue2, Acc2_Acc2} = xform(DescValue1, Acc2_Acc1, Fun),
                            DescList1_Acc2 = [DescValue2 | DescList1_Acc1],
                            {DescList1_Acc2, Acc2_Acc2}
                        end,
                        {[], Acc2},
                        DescList1
                    ),
                    T3 = T2#argo_desc_value{inner = {list, DescList2}},
                    {T3, Acc3};
                #argo_desc_value{inner = null} ->
                    {T2, Acc2};
                #argo_desc_value{inner = {L, _}} when
                    L =:= 'boolean' orelse L =:= 'string' orelse L =:= 'bytes' orelse L =:= 'int' orelse L =:= 'float'
                ->
                    {T2, Acc2};
                #argo_error_value{locations = OptionLocations1, path = OptionPath1, extensions = OptionExtensions1} ->
                    {OptionLocations2, Acc3} =
                        case OptionLocations1 of
                            none ->
                                {OptionLocations1, Acc2};
                            {some, LocationList1} ->
                                A2_1 = Acc2,
                                {LocationList2, A2_2} = lists:foldl(
                                    fun(Location1, {LocationList1_Acc1, A2_1_Acc1}) ->
                                        {Location2, A2_1_Acc2} = xform(Location1, A2_1_Acc1, Fun),
                                        LocationList1_Acc2 = [Location2 | LocationList1_Acc1],
                                        {LocationList1_Acc2, A2_1_Acc2}
                                    end,
                                    {[], A2_1},
                                    LocationList1
                                ),
                                LocationList3 = lists:reverse(LocationList2),
                                {{some, LocationList3}, A2_2}
                        end,
                    {OptionPath2, Acc4} =
                        case OptionPath1 of
                            none ->
                                {OptionPath1, Acc3};
                            {some, Path1} ->
                                A3_1 = Acc3,
                                {Path2, A3_2} = xform(Path1, A3_1, Fun),
                                {{some, Path2}, A3_2}
                        end,
                    {OptionExtensions2, Acc5} =
                        case OptionExtensions1 of
                            none ->
                                {OptionExtensions1, Acc4};
                            {some, Extensions1} ->
                                A4_1 = Acc4,
                                {Extensions2, A4_2} = xform(Extensions1, A4_1, Fun),
                                {{some, Extensions2}, A4_2}
                        end,
                    T3 = T2#argo_error_value{
                        locations = OptionLocations2, path = OptionPath2, extensions = OptionExtensions2
                    },
                    {T3, Acc5};
                #argo_extensions_value{inner = Extensions1} ->
                    {Extensions2, Acc3} = argo_index_map:foldl(
                        fun(_Index, Key, DescValue1, {Extensions1_Acc1, Acc2_Acc1}) ->
                            {DescValue2, Acc2_Acc2} = xform(DescValue1, Acc2_Acc1, Fun),
                            Extensions1_Acc2 = argo_index_map:put(Key, DescValue2, Extensions1_Acc1),
                            {Extensions1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        Extensions1
                    ),
                    T3 = T2#argo_extensions_value{inner = Extensions2},
                    {T3, Acc3};
                #argo_nullable_value{inner = null} ->
                    {T2, Acc2};
                #argo_nullable_value{inner = {non_null, Value1}} ->
                    {Value2, Acc3} = xform(Value1, Acc2, Fun),
                    T3 = T2#argo_nullable_value{inner = {non_null, Value2}},
                    {T3, Acc3};
                #argo_nullable_value{inner = {field_errors, FieldErrorList1}} ->
                    {FieldErrorList2, Acc3} = lists:foldl(
                        fun(ErrorValue1, {FieldErrorList1_Acc1, Acc2_Acc1}) ->
                            {ErrorValue2, Acc2_Acc2} = xform(ErrorValue1, Acc2_Acc1, Fun),
                            FieldErrorList1_Acc2 = [ErrorValue2 | FieldErrorList1_Acc1],
                            {FieldErrorList1_Acc2, Acc2_Acc2}
                        end,
                        {[], Acc2},
                        FieldErrorList1
                    ),
                    FieldErrorList3 = lists:reverse(FieldErrorList2),
                    T3 = T2#argo_nullable_value{inner = {field_errors, FieldErrorList3}},
                    {T3, Acc3};
                #argo_path_value{} ->
                    {T2, Acc2};
                #argo_location_value{} ->
                    {T2, Acc2};
                #argo_record_value{fields = Fields1} ->
                    {Fields2, Acc3} = argo_index_map:foldl(
                        fun(_Index, FieldName, FieldValue1, {Fields1_Acc1, Acc2_Acc1}) ->
                            {FieldValue2, Acc2_Acc2} = xform(FieldValue1, Acc2_Acc1, Fun),
                            Fields1_Acc2 = argo_index_map:put(FieldName, FieldValue2, Fields1_Acc1),
                            {Fields1_Acc2, Acc2_Acc2}
                        end,
                        {argo_index_map:new(), Acc2},
                        Fields1
                    ),
                    T3 = T2#argo_record_value{fields = Fields2},
                    {T3, Acc3};
                #argo_field_value{inner = {optional, none}} ->
                    {T2, Acc2};
                #argo_field_value{inner = {optional, {some, Value1}}} ->
                    {Value2, Acc3} = xform(Value1, Acc2, Fun),
                    T3 = T2#argo_field_value{inner = {optional, {some, Value2}}},
                    {T3, Acc3};
                #argo_field_value{inner = {required, Value1}} ->
                    {Value2, Acc3} = xform(Value1, Acc2, Fun),
                    T3 = T2#argo_field_value{inner = {required, Value2}},
                    {T3, Acc3};
                #argo_scalar_value{} ->
                    {T2, Acc2};
                #argo_value{inner = Inner1} ->
                    {Inner2, Acc3} = xform(Inner1, Acc2, Fun),
                    T3 = T2#argo_value{inner = Inner2},
                    {T3, Acc3}
            end;
        {skip, T2, Acc2} ->
            {T2, Acc2}
    end.

%% @private
-spec xform_normalize(TypeIn, AccIn, Result) -> {Action, TypeOut, AccOut} when
    TypeIn :: dynamic(),
    AccIn :: dynamic(),
    Result :: xform_result(TypeOut, AccOut),
    Action :: xform_action(),
    TypeOut :: dynamic(),
    AccOut :: dynamic().
xform_normalize(TypeIn, AccIn, Action) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeIn, AccIn};
xform_normalize(TypeIn, _AccIn, {Action, AccOut}) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeIn, AccOut};
xform_normalize(_TypeIn, _AccIn, {Action, TypeOut, AccOut}) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeOut, AccOut}.
