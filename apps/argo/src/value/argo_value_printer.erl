%%% % @format
-module(argo_value_printer).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_index_map.hrl").
-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_message.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new_io_device/1,
    new_string/0,
    finalize/1,
    print_value/2
]).

%% Records
-record(argo_value_printer, {
    depth = 0 :: non_neg_integer(),
    output = [] :: iolist() | io:device()
}).

%% Types
-type t() :: #argo_value_printer{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new_io_device(IoDevice) -> Printer when IoDevice :: io:device(), Printer :: t().
new_io_device(IoDevice) when not is_list(IoDevice) ->
    #argo_value_printer{depth = 0, output = IoDevice}.

-spec new_string() -> Printer when Printer :: t().
new_string() ->
    #argo_value_printer{depth = 0, output = []}.

-spec finalize(Printer) -> ok | iolist() when Printer :: t().
finalize(#argo_value_printer{output = Output}) when is_list(Output) ->
    Output;
finalize(Printer = #argo_value_printer{}) ->
    _ = write(Printer, "~n", []),
    ok.

-spec print_value(Printer, Value) -> Printer when Printer :: t(), Value :: argo_value:t().
print_value(Printer1 = #argo_value_printer{}, Value = #argo_value{}) ->
    case Value#argo_value.inner of
        ScalarValue = #argo_scalar_value{} ->
            print_scalar_value(Printer1, ScalarValue);
        #argo_block_value{wire_type = #argo_block_wire_type{key = Key, dedupe = Dedupe}, value = ScalarValue} ->
            Printer2 = print_scalar_value(Printer1, ScalarValue),
            case Dedupe of
                false ->
                    Printer3 = write(Printer2, "{~ts}", [Key]),
                    Printer3;
                true ->
                    Printer3 = write(Printer2, "<~ts>", [Key]),
                    Printer3
            end;
        NullableValue = #argo_nullable_value{} ->
            print_nullable_value(Printer1, NullableValue);
        ArrayValue = #argo_array_value{} ->
            print_array_value(Printer1, ArrayValue);
        RecordValue = #argo_record_value{} ->
            print_record_value(Printer1, RecordValue);
        DescValue = #argo_desc_value{} ->
            print_desc_value(Printer1, DescValue);
        ErrorValue = #argo_error_value{} ->
            print_error_value(Printer1, ErrorValue);
        PathValue = #argo_path_value{} ->
            print_path_value(Printer1, PathValue)
    end.

%% @private
-spec print_array_value(Printer, ArrayValue) -> Printer when
    Printer :: t(), ArrayValue :: argo_array_value:t().
print_array_value(Printer1 = #argo_value_printer{}, ArrayValue = #argo_array_value{}) ->
    case ArrayValue#argo_array_value.items of
        [] ->
            Printer2 = write(Printer1, "[]", []),
            Printer2;
        Items = [_ | _] ->
            Printer2 = write(Printer1, "[~n", []),
            Printer3 = lists:foldl(
                fun(Value, PrinterAcc1) ->
                    PrinterAcc2 = indent(PrinterAcc1),
                    PrinterAcc3 = print_value(PrinterAcc2, Value),
                    PrinterAcc4 = write(PrinterAcc3, ",~n", []),
                    PrinterAcc4
                end,
                Printer2#argo_value_printer{depth = Printer2#argo_value_printer.depth + 1},
                Items
            ),
            Printer4 = Printer3#argo_value_printer{depth = Printer3#argo_value_printer.depth - 1},
            Printer5 = indent(Printer4),
            Printer6 = write(Printer5, "]", []),
            Printer6
    end.

%% @private
-spec print_desc_value(Printer, DescValue) -> Printer when
    Printer :: t(), DescValue :: argo_desc_value:t().
print_desc_value(Printer1 = #argo_value_printer{}, DescValue = #argo_desc_value{}) ->
    case DescValue#argo_desc_value.inner of
        null -> write(Printer1, "DESC(NULL)", []);
        {boolean, V} -> write(Printer1, "DESC(BOOLEAN(~0tp))", [V]);
        {object, V} -> print_desc_value_object(Printer1, V);
        {list, V} -> print_desc_value_list(Printer1, V);
        {string, V} -> write(Printer1, "DESC(STRING(~0tp))", [V]);
        {bytes, V} -> write(Printer1, "DESC(BYTES(~0tp))", [V]);
        {int, V} -> write(Printer1, "DESC(INT(~0tp))", [V]);
        {float, V} -> write(Printer1, "DESC(FLOAT(~0tp))", [V])
    end.

%% @private
-spec print_desc_value_list(Printer, List) -> Printer when
    Printer :: t(), List :: argo_desc_value:desc_list().
print_desc_value_list(Printer1 = #argo_value_printer{}, List) when is_list(List) ->
    case List of
        [] ->
            write(Printer1, "DESC([])", []);
        [_ | _] ->
            Printer2 = write(Printer1, "DESC([~n", []),
            Printer3 = lists:foldl(
                fun(DescValue, PrinterAcc1) ->
                    PrinterAcc2 = indent(PrinterAcc1),
                    PrinterAcc3 = print_desc_value(PrinterAcc2, DescValue),
                    PrinterAcc4 = write(PrinterAcc3, ",~n", []),
                    PrinterAcc4
                end,
                Printer2#argo_value_printer{depth = Printer2#argo_value_printer.depth + 1},
                List
            ),
            Printer4 = Printer3#argo_value_printer{depth = Printer3#argo_value_printer.depth - 1},
            Printer5 = indent(Printer4),
            Printer6 = write(Printer5, "])", []),
            Printer6
    end.

%% @private
-spec print_desc_value_object(Printer, Object) -> Printer when
    Printer :: t(), Object :: argo_desc_value:desc_object().
print_desc_value_object(Printer1 = #argo_value_printer{}, Object = #argo_index_map{}) ->
    case argo_index_map:size(Object) of
        0 ->
            write(Printer1, "DESC({})", []);
        _ ->
            Printer2 = write(Printer1, "DESC({~n", []),
            Printer3 = argo_index_map:foldl(
                fun(_Index, Key, DescValue, PrinterAcc1) ->
                    PrinterAcc2 = indent(PrinterAcc1),
                    PrinterAcc3 = write(PrinterAcc2, "~0tp: ", [Key]),
                    PrinterAcc4 = print_desc_value(PrinterAcc3, DescValue),
                    PrinterAcc5 = write(PrinterAcc4, "~n", []),
                    PrinterAcc5
                end,
                Printer2#argo_value_printer{depth = Printer2#argo_value_printer.depth + 1},
                Object
            ),
            Printer4 = Printer3#argo_value_printer{depth = Printer3#argo_value_printer.depth - 1},
            Printer5 = indent(Printer4),
            Printer6 = write(Printer5, "})", []),
            Printer6
    end.

%% @private
-spec print_error_value(Printer, ErrorValue) -> Printer when
    Printer :: t(), ErrorValue :: argo_error_value:t().
print_error_value(Printer1 = #argo_value_printer{}, ErrorValue = #argo_error_value{}) ->
    Printer2 = write(Printer1, "ERROR({~n", []),
    Printer3 = Printer2#argo_value_printer{depth = Printer2#argo_value_printer.depth + 1},
    Printer4 = indent(Printer3),
    Printer5 = write(Printer4, "message: ~0tp~n", [ErrorValue#argo_error_value.message]),
    Printer6 =
        case ErrorValue#argo_error_value.location of
            none ->
                Printer5;
            {some, []} ->
                P5_1 = Printer5,
                P5_2 = indent(P5_1),
                P5_3 = write(P5_2, "location: []~n", []),
                P5_3;
            {some, Location} ->
                P5_1 = Printer5,
                P5_2 = indent(P5_1),
                P5_3 = write(P5_2, "location: [~n", []),
                P5_4 = P5_3#argo_value_printer{depth = P5_3#argo_value_printer.depth + 1},
                P5_5 = lists:foldl(
                    fun(#argo_location_value{line = Line, column = Column}, P5_4_Acc1) ->
                        P5_4_Acc2 = indent(P5_4_Acc1),
                        P5_4_Acc3 = write(P5_4_Acc2, "LOCATION({line: ~0tp, column: ~0tp})~n", [Line, Column]),
                        P5_4_Acc3
                    end,
                    P5_4,
                    Location
                ),
                P5_6 = P5_5#argo_value_printer{depth = P5_5#argo_value_printer.depth - 1},
                P5_7 = indent(P5_6),
                P5_8 = write(P5_7, "]~n", []),
                P5_8
        end,
    Printer7 =
        case ErrorValue#argo_error_value.path of
            none ->
                Printer6;
            {some, PathValue} ->
                P6_1 = Printer6,
                P6_2 = indent(P6_1),
                P6_3 = write(P6_2, "path: ", []),
                P6_4 = print_path_value(P6_3, PathValue),
                P6_5 = write(P6_4, "~n", []),
                P6_5
        end,
    Printer8 =
        case ErrorValue#argo_error_value.extensions of
            none ->
                Printer7;
            {some, Extensions} ->
                P7_1 = Printer7,
                P7_2 = indent(P7_1),
                P7_3 = write(P7_2, "extensions: ", []),
                P7_4 = print_desc_value_object(P7_3, Extensions),
                P7_5 = write(P7_4, "~n", []),
                P7_5
        end,
    Printer9 = Printer8#argo_value_printer{depth = Printer8#argo_value_printer.depth - 1},
    Printer10 = indent(Printer9),
    Printer11 = write(Printer10, "})", []),
    Printer11.

%% @private
-spec print_field_value(Printer, FieldValue) -> Printer when
    Printer :: t(), FieldValue :: argo_field_value:t().
print_field_value(Printer1 = #argo_value_printer{}, FieldValue = #argo_field_value{}) ->
    Printer2 = Printer1#argo_value_printer{depth = Printer1#argo_value_printer.depth + 1},
    Printer3 = indent(Printer2),
    Printer4 =
        case argo_field_value:is_optional(FieldValue) of
            false ->
                write(Printer3, "~ts: ", [argo_field_value:name(FieldValue)]);
            true ->
                write(Printer3, "~ts?: ", [argo_field_value:name(FieldValue)])
        end,
    Printer5 =
        case FieldValue#argo_field_value.inner of
            {optional, none} ->
                write(Printer4, "ABSENT", []);
            {optional, {some, Value}} ->
                P4_1 = Printer4,
                P4_2 = print_value(P4_1, Value),
                P4_2;
            {required, Value} ->
                print_value(Printer4, Value)
        end,
    Printer6 = write(Printer5, "~n", []),
    Printer7 = Printer6#argo_value_printer{depth = Printer6#argo_value_printer.depth - 1},
    Printer7.

%% @private
-spec print_nullable_value(Printer, NullableValue) -> Printer when
    Printer :: t(), NullableValue :: argo_nullable_value:t().
print_nullable_value(Printer1 = #argo_value_printer{}, NullableValue = #argo_nullable_value{}) ->
    case NullableValue#argo_nullable_value.inner of
        null ->
            write(Printer1, "NULL", []);
        {non_null, Value} ->
            Printer2 = write(Printer1, "NON_NULL(", []),
            Printer3 = print_value(Printer2, Value),
            Printer4 = write(Printer3, ")", []),
            Printer4
    end.

%% @private
-spec print_path_value(Printer, PathValue) -> Printer when
    Printer :: t(), PathValue :: argo_path_value:t().
print_path_value(Printer1 = #argo_value_printer{}, PathValue = #argo_path_value{}) ->
    Printer2 = write(Printer1, "PATH(~0tp)", [argo_path_value:to_list(PathValue)]),
    Printer2.

%% @private
-spec print_record_value(Printer, RecordValue) -> Printer when
    Printer :: t(), RecordValue :: argo_record_value:t().
print_record_value(Printer1 = #argo_value_printer{}, RecordValue = #argo_record_value{}) ->
    Printer2 = write(Printer1, "{~n", []),
    Fields = RecordValue#argo_record_value.fields,
    Printer3 = argo_index_map:foldl(
        fun(_Index, _FieldName, FieldValue, PrinterAcc) ->
            print_field_value(PrinterAcc, FieldValue)
        end,
        Printer2,
        Fields
    ),
    Printer4 = indent(Printer3),
    Printer5 = write(Printer4, "}", []),
    Printer5.

%% @private
-spec print_scalar_value(Printer, ScalarValue) -> Printer when
    Printer :: t(), ScalarValue :: argo_scalar_value:t().
print_scalar_value(Printer1 = #argo_value_printer{}, ScalarValue = #argo_scalar_value{}) ->
    case ScalarValue#argo_scalar_value.inner of
        {string, Value} -> write(Printer1, "STRING(~0tp)", [Value]);
        {boolean, Value} -> write(Printer1, "BOOLEAN(~0tp)", [Value]);
        {varint, Value} -> write(Printer1, "VARINT(~0tp)", [Value]);
        {float64, Value} -> write(Printer1, "FLOAT64(~0tp)", [Value]);
        {bytes, Value} -> write(Printer1, "BYTES(~0tp)", [Value]);
        {fixed, Value} -> write(Printer1, "FIXED(~w, ~0tp)", [byte_size(Value), Value])
    end.

%% @private
-spec indent(Printer) -> Printer when Printer :: t().
indent(Printer1 = #argo_value_printer{depth = Depth}) ->
    Printer2 = write(Printer1, "~ts", [binary:copy(<<"  ">>, Depth)]),
    Printer2.

%% @private
-spec write(Printer, Format, Data) -> Printer when Printer :: t(), Format :: io:format(), Data :: [term()].
write(Printer1 = #argo_value_printer{output = Output1}, Format, Data) when is_list(Output1) ->
    Output2 = [Output1 | io_lib:format(Format, Data)],
    Printer2 = Printer1#argo_value_printer{output = Output2},
    Printer2;
write(Printer1 = #argo_value_printer{output = IoDevice}, Format, Data) ->
    ok = io:format(IoDevice, Format, Data),
    Printer1.
