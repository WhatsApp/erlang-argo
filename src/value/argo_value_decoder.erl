%%% % @format
-module(argo_value_decoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include("argo_header.hrl").
-include("argo_label.hrl").
-include("argo_message.hrl").
-include("argo_value.hrl").
-include("argo_wire_type.hrl").

%% API
-export([
    new/1,
    from_reader/1,
    decode_wire_type/2
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_value_decoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(MessageDecoder) -> ValueDecoder when MessageDecoder :: argo_message_decoder:t(), ValueDecoder :: t().
new(MessageDecoder = #argo_message_decoder{}) ->
    #argo_value_decoder{message = MessageDecoder}.

-spec from_reader(Reader) -> {Reader, ValueDecoder} when Reader :: binary(), ValueDecoder :: t().
from_reader(Reader1) ->
    {Reader2, MessageDecoder} = argo_message_decoder:from_reader(Reader1),
    ValueDecoder = new(MessageDecoder),
    {Reader2, ValueDecoder}.

-spec decode_wire_type(ValueDecoder, WireType) -> {ValueDecoder, Value} when
    ValueDecoder :: t(), WireType :: argo_wire_type:t(), Value :: argo_value:t().
decode_wire_type(ValueDecoder1 = #argo_value_decoder{}, WireType = #argo_wire_type{}) ->
    case WireType#argo_wire_type.inner of
        ScalarWireType = #argo_scalar_wire_type{} ->
            {ValueDecoder2, ScalarValue} = decode_scalar_wire_type(ValueDecoder1, ScalarWireType),
            Value = argo_value:scalar(ScalarValue),
            {ValueDecoder2, Value};
        BlockWireType = #argo_block_wire_type{} ->
            {ValueDecoder2, BlockValue} = decode_block_wire_type(ValueDecoder1, BlockWireType),
            Value = argo_value:block(BlockValue),
            {ValueDecoder2, Value};
        NullableWireType = #argo_nullable_wire_type{} ->
            {ValueDecoder2, NullableValue} = decode_nullable_wire_type(ValueDecoder1, NullableWireType),
            Value = argo_value:nullable(NullableValue),
            {ValueDecoder2, Value};
        ArrayWireType = #argo_array_wire_type{} ->
            {ValueDecoder2, ArrayValue} = decode_array_wire_type(ValueDecoder1, ArrayWireType),
            Value = argo_value:array(ArrayValue),
            {ValueDecoder2, Value};
        RecordWireType = #argo_record_wire_type{} ->
            {ValueDecoder2, RecordValue} = decode_record_wire_type(ValueDecoder1, RecordWireType),
            Value = argo_value:record(RecordValue),
            {ValueDecoder2, Value};
        #argo_desc_wire_type{} ->
            {ValueDecoder2, DescValue} = decode_desc_wire_type(ValueDecoder1),
            Value = argo_value:desc(DescValue),
            {ValueDecoder2, Value};
        #argo_error_wire_type{} ->
            {ValueDecoder2, ErrorValue} = decode_error_wire_type(ValueDecoder1),
            Value = argo_value:error(ErrorValue),
            {ValueDecoder2, Value};
        #argo_path_wire_type{} ->
            {ValueDecoder2, PathValue} = decode_path_wire_type(ValueDecoder1),
            Value = argo_value:path(PathValue),
            {ValueDecoder2, Value}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_scalar_wire_type(ValueDecoder, ScalarWireType) -> {ValueDecoder, ScalarValue} when
    ValueDecoder :: t(), ScalarWireType :: argo_scalar_wire_type:t(), ScalarValue :: argo_scalar_value:t().
decode_scalar_wire_type(ValueDecoder1 = #argo_value_decoder{}, ScalarWireType = #argo_scalar_wire_type{}) ->
    ValueDecoder2 =
        #argo_value_decoder{message = MessageDecoder1} = maybe_decode_self_describing_label_for_scalar(
            ValueDecoder1, ScalarWireType
        ),
    case ScalarWireType#argo_scalar_wire_type.inner of
        string ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_string(MessageDecoder1),
            ScalarValue = argo_scalar_value:string(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        boolean ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_boolean(MessageDecoder1),
            ScalarValue = argo_scalar_value:boolean(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        varint ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_varint(MessageDecoder1),
            ScalarValue = argo_scalar_value:varint(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        float64 ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_float64(MessageDecoder1),
            ScalarValue = argo_scalar_value:float64(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        bytes ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_bytes(MessageDecoder1),
            ScalarValue = argo_scalar_value:bytes(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue};
        #argo_fixed_wire_type{length = Length} ->
            {MessageDecoder2, Value} = argo_message_decoder:decode_block_fixed(MessageDecoder1, Length),
            ScalarValue = argo_scalar_value:fixed(Value),
            ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder3, ScalarValue}
    end.

%% @private
-spec decode_block_wire_type(ValueDecoder, BlockWireType) -> {ValueDecoder, BlockValue} when
    ValueDecoder :: t(), BlockWireType :: argo_block_wire_type:t(), BlockValue :: argo_block_value:t().
decode_block_wire_type(ValueDecoder1 = #argo_value_decoder{}, BlockWireType = #argo_block_wire_type{}) ->
    ValueDecoder2 =
        #argo_value_decoder{message = MessageDecoder1} = maybe_decode_self_describing_label_for_scalar(
            ValueDecoder1, BlockWireType#argo_block_wire_type.'of'
        ),
    {MessageDecoder2, ScalarValue} = argo_message_decoder:decode_block_scalar(MessageDecoder1, BlockWireType),
    BlockValue = argo_block_value:new(BlockWireType, ScalarValue),
    ValueDecoder3 = ValueDecoder2#argo_value_decoder{message = MessageDecoder2},
    {ValueDecoder3, BlockValue}.

%% @private
-spec decode_nullable_wire_type(ValueDecoder, NullableWireType) -> {ValueDecoder, NullableValue} when
    ValueDecoder :: t(), NullableWireType :: argo_nullable_wire_type:t(), NullableValue :: argo_nullable_value:t().
decode_nullable_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, NullableWireType = #argo_nullable_wire_type{}
) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
    case Label of
        ?ARGO_LABEL_MARKER_NULL ->
            NullableValue = argo_nullable_value:null(),
            {ValueDecoder2, NullableValue};
        ?ARGO_LABEL_MARKER_NON_NULL ->
            {ValueDecoder3, Value} = decode_wire_type(ValueDecoder2, NullableWireType#argo_nullable_wire_type.'of'),
            NullableValue = argo_nullable_value:non_null(Value),
            {ValueDecoder3, NullableValue};
        ?ARGO_LABEL_MARKER_ERROR ->
            NullableValue = argo_nullable_value:field_errors([]),
            {ValueDecoder2, NullableValue};
        _ ->
            error_with_info(badarg, [ValueDecoder1, NullableWireType], #{1 => {invalid_nullable_label, Label}})
    end.

%% @private
-spec decode_array_wire_type(ValueDecoder, ArrayWireType) -> {ValueDecoder, ArrayValue} when
    ValueDecoder :: t(), ArrayWireType :: argo_array_wire_type:t(), ArrayValue :: argo_array_value:t().
decode_array_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, ArrayWireType = #argo_array_wire_type{}
) ->
    MessageDecoder2 =
        case argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) of
            false ->
                MessageDecoder1;
            true ->
                {MD2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
                case Label of
                    ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST ->
                        MD2;
                    _ ->
                        error_with_info(badarg, [ValueDecoder1, ArrayWireType], #{1 => {invalid_array_label, Label}})
                end
        end,
    {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
    {ValueDecoder3, Items} = decode_array_wire_type_items(ValueDecoder2, ArrayWireType, Length, []),
    ArrayValue = argo_array_value:new(Items),
    {ValueDecoder3, ArrayValue}.

%% @private
-spec decode_array_wire_type_items(ValueDecoder, ArrayWireType, Length, Items) -> {ValueDecoder, Items} when
    ValueDecoder :: t(),
    ArrayWireType :: argo_array_wire_type:t(),
    Length :: non_neg_integer(),
    Items :: [argo_value:t()].
decode_array_wire_type_items(ValueDecoder1 = #argo_value_decoder{}, #argo_array_wire_type{}, 0, Items) ->
    {ValueDecoder1, lists:reverse(Items)};
decode_array_wire_type_items(
    ValueDecoder1 = #argo_value_decoder{}, ArrayWireType = #argo_array_wire_type{}, Length, Items
) when is_integer(Length) andalso Length > 0 ->
    {ValueDecoder2, Value} = decode_wire_type(ValueDecoder1, ArrayWireType#argo_array_wire_type.'of'),
    decode_array_wire_type_items(ValueDecoder2, ArrayWireType, Length - 1, [Value | Items]).

%% @private
-spec decode_record_wire_type(ValueDecoder, RecordWireType) -> {ValueDecoder, RecordValue} when
    ValueDecoder :: t(), RecordWireType :: argo_record_wire_type:t(), RecordValue :: argo_record_value:t().
decode_record_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, RecordWireType = #argo_record_wire_type{}
) ->
    case argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) of
        false ->
            argo_index_map:foldl(
                fun(_Index, _FieldName, FieldWireType, {ValueDecoderAcc1, RecordValueAcc1}) ->
                    {ValueDecoderAcc2, FieldValue} = decode_field_wire_type(ValueDecoderAcc1, FieldWireType),
                    RecordValueAcc2 = argo_record_value:insert(RecordValueAcc1, FieldValue),
                    {ValueDecoderAcc2, RecordValueAcc2}
                end,
                {ValueDecoder1, argo_record_value:new()},
                RecordWireType#argo_record_wire_type.fields
            );
        true ->
            decode_self_describing_record_wire_type(ValueDecoder1, RecordWireType)
    end.

%% @private
-spec decode_field_wire_type(ValueDecoder, FieldWireType) -> {ValueDecoder, FieldValue} when
    ValueDecoder :: t(), FieldWireType :: argo_field_wire_type:t(), FieldValue :: argo_field_value:t().
decode_field_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, FieldWireType = #argo_field_wire_type{name = Name}
) ->
    case FieldWireType#argo_field_wire_type.omittable of
        false ->
            {ValueDecoder2, Value} = decode_wire_type(ValueDecoder1, FieldWireType#argo_field_wire_type.'of'),
            FieldValue = argo_field_value:required(Name, Value),
            {ValueDecoder2, FieldValue};
        true ->
            RequiresNonNullLabel =
                argo_header:inline_everything(MessageDecoder1#argo_message_decoder.header) orelse
                    argo_wire_type:is_labeled(FieldWireType#argo_field_wire_type.'of'),
            try argo_message_decoder:read_core_label(MessageDecoder1) of
                {MessageDecoder2, Label} ->
                    case Label of
                        ?ARGO_LABEL_MARKER_ABSENT ->
                            FieldValue = argo_field_value:optional(Name, none),
                            ValueDecoder2 = #argo_value_decoder{message = MessageDecoder2},
                            {ValueDecoder2, FieldValue};
                        _ when RequiresNonNullLabel =:= false ->
                            {ValueDecoder2, Value} = decode_wire_type(
                                ValueDecoder1, FieldWireType#argo_field_wire_type.'of'
                            ),
                            FieldValue = argo_field_value:optional(Name, {some, Value}),
                            {ValueDecoder2, FieldValue};
                        ?ARGO_LABEL_MARKER_NON_NULL when RequiresNonNullLabel =:= true ->
                            ValueDecoder2 = #argo_value_decoder{message = MessageDecoder2},
                            {ValueDecoder3, Value} = decode_wire_type(
                                ValueDecoder2, FieldWireType#argo_field_wire_type.'of'
                            ),
                            FieldValue = argo_field_value:optional(Name, {some, Value}),
                            {ValueDecoder3, FieldValue};
                        _ ->
                            error_with_info(badarg, [ValueDecoder1, FieldWireType], #{1 => {invalid_field_label, Label}})
                    end
            catch
                error:badarg:Stacktrace ->
                    case RequiresNonNullLabel of
                        false ->
                            {ValueDecoder2, Value} = decode_wire_type(
                                ValueDecoder1, FieldWireType#argo_field_wire_type.'of'
                            ),
                            FieldValue = argo_field_value:optional(Name, {some, Value}),
                            {ValueDecoder2, FieldValue};
                        true ->
                            erlang:raise(error, badarg, Stacktrace)
                    end
            end
    end.

%% @private
-spec decode_desc_wire_type(ValueDecoder) -> {ValueDecoder, DescValue} when
    ValueDecoder :: t(), DescValue :: argo_desc_value:t().
decode_desc_wire_type(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_NULL ->
            DescValue = argo_desc_value:null(),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FALSE ->
            DescValue = argo_desc_value:boolean(false),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_TRUE ->
            DescValue = argo_desc_value:boolean(true),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_desc_wire_type_object(ValueDecoder2, Length, argo_index_map:new());
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_desc_wire_type_list(ValueDecoder2, Length, []);
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_string(MessageDecoder2),
            DescValue = argo_desc_value:string(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_bytes(MessageDecoder2),
            DescValue = argo_desc_value:bytes(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_varint(MessageDecoder2),
            DescValue = argo_desc_value:int(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT ->
            {MessageDecoder3, Value} = argo_message_decoder:decode_block_float64(MessageDecoder2),
            DescValue = argo_desc_value:float(Value),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            {ValueDecoder2, DescValue};
        _ ->
            error_with_info(badarg, [ValueDecoder1], #{1 => {invalid_self_describing_label, Label}})
    end.

%% @private
-spec decode_desc_wire_type_object(ValueDecoder, Length, Object) -> {ValueDecoder, Object} when
    ValueDecoder :: t(),
    Length :: non_neg_integer(),
    Object :: argo_index_map:t(unicode:unicode_binary(), argo_desc_value:t()).
decode_desc_wire_type_object(ValueDecoder1 = #argo_value_decoder{}, 0, Object1) ->
    DescValue = argo_desc_value:object(Object1),
    {ValueDecoder1, DescValue};
decode_desc_wire_type_object(ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, Length, Object1) when
    is_integer(Length) andalso Length > 0
->
    {MessageDecoder2, Key} = argo_message_decoder:decode_block_string(MessageDecoder1),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
    {ValueDecoder3, Val} = decode_desc_wire_type(ValueDecoder2),
    Object2 = argo_index_map:put(Key, Val, Object1),
    decode_desc_wire_type_object(ValueDecoder3, Length - 1, Object2).

%% @private
-spec decode_desc_wire_type_list(ValueDecoder, Length, List) -> {ValueDecoder, List} when
    ValueDecoder :: t(), Length :: non_neg_integer(), List :: [argo_desc_value:t()].
decode_desc_wire_type_list(ValueDecoder1 = #argo_value_decoder{}, 0, List1) ->
    DescValue = argo_desc_value:list(lists:reverse(List1)),
    {ValueDecoder1, DescValue};
decode_desc_wire_type_list(ValueDecoder1 = #argo_value_decoder{}, Length, List1) when
    is_integer(Length) andalso Length > 0
->
    {ValueDecoder2, Element} = decode_desc_wire_type(ValueDecoder1),
    List2 = [Element | List1],
    decode_desc_wire_type_list(ValueDecoder2, Length - 1, List2).

%% @private
-spec decode_error_wire_type(ValueDecoder) -> {ValueDecoder, ErrorValue} when
    ValueDecoder :: t(), ErrorValue :: argo_error_value:t().
decode_error_wire_type(ValueDecoder1 = #argo_value_decoder{}) ->
    error_with_info(badarg, [ValueDecoder1], #{general => unsupported}).

%% @private
-spec decode_path_wire_type(ValueDecoder) -> {ValueDecoder, PathValue} when
    ValueDecoder :: t(), PathValue :: argo_path_value:t().
decode_path_wire_type(ValueDecoder1 = #argo_value_decoder{}) ->
    error_with_info(badarg, [ValueDecoder1], #{general => unsupported}).

%% @private
-spec maybe_decode_self_describing_label_for_scalar(ValueDecoder, ScalarWireType) -> ValueDecoder when
    ValueDecoder :: t(), ScalarWireType :: argo_scalar_wire_type:t().
maybe_decode_self_describing_label_for_scalar(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, ScalarWireType = #argo_scalar_wire_type{}
) ->
    case argo_header:self_describing(MessageDecoder1#argo_message_decoder.header) of
        false ->
            ValueDecoder1;
        true ->
            decode_self_describing_label_for_scalar(ValueDecoder1, ScalarWireType)
    end.

%% @private
-spec decode_self_describing_label_for_scalar(ValueDecoder, ScalarWireType) -> ValueDecoder when
    ValueDecoder :: t(), ScalarWireType :: argo_scalar_wire_type:t().
decode_self_describing_label_for_scalar(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, ScalarWireType = #argo_scalar_wire_type{}
) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
    case ScalarWireType#argo_scalar_wire_type.inner of
        string when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING ->
            ValueDecoder2;
        boolean when
            (Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FALSE orelse
                Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_TRUE)
        ->
            % rollback read_core_label for boolean
            ValueDecoder1;
        varint when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_INT ->
            ValueDecoder2;
        float64 when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT ->
            ValueDecoder2;
        bytes when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES ->
            ValueDecoder2;
        #argo_fixed_wire_type{} when Label =:= ?ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES ->
            ValueDecoder2;
        string ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_string_label, Label}});
        boolean ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_boolean_label, Label}});
        varint ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_varint_label, Label}});
        float64 ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_float64_label, Label}});
        bytes ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_bytes_label, Label}});
        #argo_fixed_wire_type{} ->
            error_with_info(badarg, [ValueDecoder1, ScalarWireType], #{1 => {invalid_fixed_label, Label}})
    end.

%% @private
-spec decode_self_describing_record_wire_type(ValueDecoder, RecordWireType) -> ValueDecoder when
    ValueDecoder :: t(), RecordWireType :: argo_record_wire_type:t().
decode_self_describing_record_wire_type(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1}, RecordWireType = #argo_record_wire_type{}
) ->
    {MessageDecoder2, Label} = argo_message_decoder:read_core_label(MessageDecoder1),
    case Label of
        ?ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT ->
            {MessageDecoder3, Length} = argo_message_decoder:read_core_length(MessageDecoder2),
            ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder3},
            decode_self_describing_record_wire_type_fields(ValueDecoder2, RecordWireType, Length, maps:new());
        _ ->
            error_with_info(badarg, [ValueDecoder1, RecordWireType], #{1 => {invalid_record_label, Label}})
    end.

%% @private
-spec decode_self_describing_record_wire_type_fields(ValueDecoder, RecordWireType, Length, Map) ->
    {ValueDecoder, RecordValue}
when
    ValueDecoder :: t(),
    RecordWireType :: argo_record_wire_type:t(),
    Length :: non_neg_integer(),
    Map :: #{argo_types:name() => argo_field_value:t()},
    RecordValue :: argo_record_value:t().
decode_self_describing_record_wire_type_fields(
    ValueDecoder1 = #argo_value_decoder{}, RecordWireType = #argo_record_wire_type{}, 0, Map1
) ->
    RecordValue = argo_index_map:foldl(
        fun(_Index, FieldName, FieldWireType, RecordValueAcc) ->
            case maps:find(FieldName, Map1) of
                {ok, FieldValue} ->
                    argo_record_value:insert(RecordValueAcc, FieldValue);
                error ->
                    case FieldWireType#argo_field_wire_type.omittable of
                        false ->
                            error_with_info(badarg, [ValueDecoder1, RecordWireType, 0, Map1], #{
                                1 => {missing_required_field, FieldName}
                            });
                        true ->
                            FieldValue = argo_field_value:optional(FieldName, none),
                            argo_record_value:insert(RecordValueAcc, FieldValue)
                    end
            end
        end,
        argo_record_value:new(),
        RecordWireType#argo_record_wire_type.fields
    ),
    {ValueDecoder1, RecordValue};
decode_self_describing_record_wire_type_fields(
    ValueDecoder1 = #argo_value_decoder{message = MessageDecoder1},
    RecordWireType = #argo_record_wire_type{},
    Length,
    Map1
) when is_integer(Length) andalso Length > 0 ->
    {MessageDecoder2, FieldName} = argo_message_decoder:decode_block_string(MessageDecoder1),
    case maps:is_key(FieldName, Map1) of
        false ->
            case argo_index_map:find(FieldName, RecordWireType#argo_record_wire_type.fields) of
                {ok, FieldWireType = #argo_field_wire_type{}} ->
                    ValueDecoder2 = ValueDecoder1#argo_value_decoder{message = MessageDecoder2},
                    {ValueDecoder3, Value} = decode_wire_type(ValueDecoder2, FieldWireType#argo_field_wire_type.'of'),
                    case FieldWireType#argo_field_wire_type.omittable of
                        false ->
                            Map2 = maps:put(FieldName, argo_field_value:required(FieldName, Value), Map1),
                            decode_self_describing_record_wire_type_fields(
                                ValueDecoder3, RecordWireType, Length - 1, Map2
                            );
                        true ->
                            Map2 = maps:put(FieldName, argo_field_value:optional(FieldName, {some, Value}), Map1),
                            decode_self_describing_record_wire_type_fields(
                                ValueDecoder3, RecordWireType, Length - 1, Map2
                            )
                    end;
                error ->
                    error_with_info(badarg, [ValueDecoder1, RecordWireType, Length, Map1], #{
                        1 => {unknown_field, FieldName}
                    })
            end;
        true ->
            error_with_info(badarg, [ValueDecoder1, RecordWireType, Length, Map1], #{1 => {duplicate_field, FieldName}})
    end.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, missing_core) ->
    "missing core part of message";
format_error_description(_Key, Value) ->
    Value.
