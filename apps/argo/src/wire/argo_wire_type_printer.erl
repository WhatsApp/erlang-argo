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
-module(argo_wire_type_printer).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_message.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new_io_device/1,
    new_string/0,
    finalize/1,
    print_wire_type/2,
    print_wire_type_store/2
]).

%% Records
-record(argo_wire_type_printer, {
    depth = 0 :: non_neg_integer(),
    output = [] :: iolist() | io:device()
}).

%% Types
-type t() :: #argo_wire_type_printer{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new_io_device(IoDevice) -> Printer when IoDevice :: io:device(), Printer :: t().
new_io_device(IoDevice) when not is_list(IoDevice) ->
    #argo_wire_type_printer{depth = 0, output = IoDevice}.

-spec new_string() -> Printer when Printer :: t().
new_string() ->
    #argo_wire_type_printer{depth = 0, output = []}.

-spec finalize(Printer) -> ok | iolist() when Printer :: t().
finalize(#argo_wire_type_printer{output = Output}) when is_list(Output) ->
    Output;
finalize(Printer = #argo_wire_type_printer{}) ->
    _ = write(Printer, "~n", []),
    ok.

-spec print_wire_type(Printer, WireType) -> Printer when Printer :: t(), WireType :: argo_wire_type:t().
print_wire_type(Printer1 = #argo_wire_type_printer{}, WireType = #argo_wire_type{}) ->
    case WireType#argo_wire_type.inner of
        ScalarWireType = #argo_scalar_wire_type{} ->
            print_scalar_wire_type(Printer1, ScalarWireType);
        #argo_block_wire_type{'of' = Of, key = Key, dedupe = Dedupe} ->
            Printer2 = print_scalar_wire_type(Printer1, Of),
            case Dedupe of
                false ->
                    Printer3 = write(Printer2, "{~ts}", [Key]),
                    Printer3;
                true ->
                    Printer3 = write(Printer2, "<~ts>", [Key]),
                    Printer3
            end;
        #argo_nullable_wire_type{'of' = Of} ->
            Printer2 = print_wire_type(Printer1, Of),
            Printer3 = write(Printer2, "?", []),
            Printer3;
        #argo_array_wire_type{'of' = Of} ->
            Printer2 = print_wire_type(Printer1, Of),
            Printer3 = write(Printer2, "[]", []),
            Printer3;
        RecordWireType = #argo_record_wire_type{} ->
            print_record_wire_type(Printer1, RecordWireType);
        #argo_desc_wire_type{} ->
            Printer2 = write(Printer1, "DESC", []),
            Printer2;
        #argo_error_wire_type{} ->
            Printer2 = write(Printer1, "ERROR", []),
            Printer2;
        #argo_path_wire_type{} ->
            Printer2 = write(Printer1, "PATH", []),
            Printer2
    end.

-spec print_wire_type_store(Printer, WireTypeStore) -> Printer when
    Printer :: t(), WireTypeStore :: argo_wire_type_store:t().
print_wire_type_store(Printer1 = #argo_wire_type_printer{}, WireTypeStore = #argo_wire_type_store{}) ->
    Printer2 = write(Printer1, "{~n", []),
    Types = WireTypeStore#argo_wire_type_store.types,
    Printer3 = argo_index_map:foldl(
        fun(_Index, TypeName, WireType, PrinterAcc) ->
            print_wire_type_store_type(PrinterAcc, TypeName, WireType)
        end,
        Printer2,
        Types
    ),
    Printer4 = indent(Printer3),
    Printer5 = write(Printer4, "}", []),
    Printer5.

%% @private
-spec print_wire_type_store_type(Printer, TypeName, WireType) -> Printer when
    Printer :: t(), TypeName :: argo_types:name(), WireType :: argo_wire_type:t().
print_wire_type_store_type(Printer1 = #argo_wire_type_printer{}, TypeName, WireType = #argo_wire_type{}) ->
    Printer2 = Printer1#argo_wire_type_printer{depth = Printer1#argo_wire_type_printer.depth + 1},
    Printer3 = indent(Printer2),
    Printer4 = write(Printer3, "~ts: ", [TypeName]),
    Printer5 = print_wire_type(Printer4, WireType),
    Printer6 = write(Printer5, "~n", []),
    Printer7 = Printer6#argo_wire_type_printer{depth = Printer6#argo_wire_type_printer.depth - 1},
    Printer7.

%% @private
-spec print_field_wire_type(Printer, FieldWireType) -> Printer when
    Printer :: t(), FieldWireType :: argo_field_wire_type:t().
print_field_wire_type(Printer1 = #argo_wire_type_printer{}, FieldWireType = #argo_field_wire_type{}) ->
    Printer2 = Printer1#argo_wire_type_printer{depth = Printer1#argo_wire_type_printer.depth + 1},
    Printer3 = indent(Printer2),
    Printer4 =
        case FieldWireType#argo_field_wire_type.omittable of
            false ->
                write(Printer3, "~ts: ", [FieldWireType#argo_field_wire_type.name]);
            true ->
                write(Printer3, "~ts?: ", [FieldWireType#argo_field_wire_type.name])
        end,
    Printer5 = print_wire_type(Printer4, FieldWireType#argo_field_wire_type.type),
    Printer6 = write(Printer5, "~n", []),
    Printer7 = Printer6#argo_wire_type_printer{depth = Printer6#argo_wire_type_printer.depth - 1},
    Printer7.

%% @private
-spec print_record_wire_type(Printer, RecordWireType) -> Printer when
    Printer :: t(), RecordWireType :: argo_record_wire_type:t().
print_record_wire_type(Printer1 = #argo_wire_type_printer{}, RecordWireType = #argo_record_wire_type{}) ->
    Printer2 = write(Printer1, "{~n", []),
    Fields = RecordWireType#argo_record_wire_type.fields,
    Printer3 = argo_index_map:foldl(
        fun(_Index, _FieldName, FieldWireType, PrinterAcc) ->
            print_field_wire_type(PrinterAcc, FieldWireType)
        end,
        Printer2,
        Fields
    ),
    Printer4 = indent(Printer3),
    Printer5 = write(Printer4, "}", []),
    Printer5.

%% @private
-spec print_scalar_wire_type(Printer, ScalarWireType) -> Printer when
    Printer :: t(), ScalarWireType :: argo_scalar_wire_type:t().
print_scalar_wire_type(Printer1 = #argo_wire_type_printer{}, ScalarWireType = #argo_scalar_wire_type{}) ->
    case ScalarWireType#argo_scalar_wire_type.inner of
        string -> write(Printer1, "STRING", []);
        boolean -> write(Printer1, "BOOLEAN", []);
        varint -> write(Printer1, "VARINT", []);
        float64 -> write(Printer1, "FLOAT64", []);
        bytes -> write(Printer1, "BYTES", []);
        #argo_fixed_wire_type{length = Length} -> write(Printer1, "FIXED(~w)", [Length])
    end.

%% @private
-spec indent(Printer) -> Printer when Printer :: t().
indent(Printer1 = #argo_wire_type_printer{depth = Depth}) ->
    Printer2 = write(Printer1, "~ts", [binary:copy(<<"  ">>, Depth)]),
    Printer2.

%% @private
-spec write(Printer, Format, Data) -> Printer when Printer :: t(), Format :: io:format(), Data :: [term()].
write(Printer1 = #argo_wire_type_printer{output = Output1}, Format, Data) when is_list(Output1) ->
    Output2 = [Output1 | io_lib:format(Format, Data)],
    Printer2 = Printer1#argo_wire_type_printer{output = Output2},
    Printer2;
write(Printer1 = #argo_wire_type_printer{output = IoDevice}, Format, Data) ->
    ok = io:format(IoDevice, Format, Data),
    Printer1.
