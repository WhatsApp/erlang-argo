%%% % @format
-module(argo_block_encoder).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3,
        scalar_wire_type_mismatch/2
    ]}
).

-include_lib("argo/include/argo_block.hrl").
-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_core.hrl").
-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_label.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/2,
    to_writer/1,
    encode_boolean/3,
    encode_bytes/3,
    encode_fixed/3,
    encode_float64/3,
    encode_scalar/3,
    encode_string/3,
    encode_varint/3
]).

%% Errors API
-export([
    format_error/2
]).

%% Records
-record(argo_dedupe_block_encoder, {
    header :: argo_header:t(),
    seen :: argo_index_set:t(binary()),
    values :: argo_block_writer:t()
}).

-record(argo_inline_block_encoder, {
    header :: argo_header:t()
}).

-record(argo_normal_block_encoder, {
    header :: argo_header:t(),
    values :: argo_block_writer:t()
}).

%% Types
-type kind() :: #argo_dedupe_block_encoder{} | #argo_inline_block_encoder{} | #argo_normal_block_encoder{}.
-type t() :: #argo_block_encoder{}.

-export_type([
    kind/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header, BlockWireType) -> BlockEncoder when
    Header :: argo_header:t(), BlockWireType :: argo_block_wire_type:t(), BlockEncoder :: t().
new(Header = #argo_header{}, BlockWireType = #argo_block_wire_type{'of' = Of, dedupe = Dedupe}) ->
    InlineEverything = argo_header:inline_everything(Header),
    NoDeduplication = argo_header:no_deduplication(Header),
    Kind =
        case InlineEverything =:= true orelse argo_scalar_wire_type:is_boolean(Of) of
            true ->
                #argo_inline_block_encoder{header = Header};
            false ->
                case Dedupe =:= true andalso not NoDeduplication of
                    true ->
                        case argo_scalar_wire_type:supports_deduplication(Of) of
                            true ->
                                #argo_dedupe_block_encoder{
                                    header = Header, seen = argo_index_set:new(), values = argo_block_writer:new(<<>>)
                                };
                            false ->
                                error_with_info(badarg, [Header, BlockWireType], #{
                                    2 => {unsupported_dedupe_block_encoder_scalar_wire_type, Of}
                                })
                        end;
                    false ->
                        #argo_normal_block_encoder{header = Header, values = argo_block_writer:new(<<>>)}
                end
        end,
    BlockEncoder = #argo_block_encoder{block_wire_type = BlockWireType, kind = Kind},
    BlockEncoder.

-spec to_writer(BlockEncoder) -> Writer when BlockEncoder :: t(), Writer :: binary().
to_writer(#argo_block_encoder{kind = Kind}) ->
    case Kind of
        #argo_dedupe_block_encoder{values = Values} ->
            argo_block_writer:to_writer(Values);
        #argo_inline_block_encoder{} ->
            <<>>;
        #argo_normal_block_encoder{values = Values} ->
            argo_block_writer:to_writer(Values)
    end.

-spec encode_boolean(BlockEncoder, CoreWriter, Value) -> {BlockEncoder, CoreWriter} when
    BlockEncoder :: t(), CoreWriter :: argo_core_writer:t(), Value :: boolean().
encode_boolean(
    BlockEncoder1 = #argo_block_encoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreWriter1 = #argo_core_writer{},
    Value
) when is_boolean(Value) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_boolean(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_encoder{} ->
                    error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                        1 => {unsupported_dedupe_block_encoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_encoder{} ->
                    case Value of
                        false ->
                            CoreWriter2 = argo_core_writer:write_label(CoreWriter1, ?ARGO_LABEL_MARKER_FALSE),
                            {BlockEncoder1, CoreWriter2};
                        true ->
                            CoreWriter2 = argo_core_writer:write_label(CoreWriter1, ?ARGO_LABEL_MARKER_TRUE),
                            {BlockEncoder1, CoreWriter2}
                    end;
                #argo_normal_block_encoder{} ->
                    error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                        1 => {unsupported_normal_block_encoder_scalar_wire_type, Of}
                    })
            end;
        false ->
            error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                1 => scalar_wire_type_mismatch(BlockEncoder1, argo_scalar_wire_type:boolean())
            })
    end.

-spec encode_bytes(BlockEncoder, CoreWriter, Value) -> {BlockEncoder, CoreWriter} when
    BlockEncoder :: t(), CoreWriter :: argo_core_writer:t(), Value :: binary().
encode_bytes(
    BlockEncoder1 = #argo_block_encoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreWriter1 = #argo_core_writer{},
    Value
) when is_binary(Value) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_bytes(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_encoder{seen = Seen1, values = BlockWriter1} ->
                    case argo_index_set:find_index_of(Value, Seen1) of
                        {ok, Index} ->
                            CoreWriter2 = argo_core_writer:write_labeled_type(CoreWriter1, {backreference, Index}),
                            {BlockEncoder1, CoreWriter2};
                        error ->
                            Seen2 = argo_index_set:add_element(Value, Seen1),
                            CoreWriter2 = argo_core_writer:write_length(CoreWriter1, byte_size(Value)),
                            BlockWriter2 = argo_block_writer:write_bytes(BlockWriter1, Value),
                            Kind1 = Kind0#argo_dedupe_block_encoder{seen = Seen2, values = BlockWriter2},
                            BlockEncoder2 = BlockEncoder1#argo_block_encoder{kind = Kind1},
                            {BlockEncoder2, CoreWriter2}
                    end;
                #argo_inline_block_encoder{} ->
                    CoreWriter2 = argo_core_writer:write_length(CoreWriter1, byte_size(Value)),
                    CoreWriter3 = argo_core_writer:write_bytes(CoreWriter2, Value),
                    {BlockEncoder1, CoreWriter3};
                #argo_normal_block_encoder{values = BlockWriter1} ->
                    CoreWriter2 = argo_core_writer:write_length(CoreWriter1, byte_size(Value)),
                    BlockWriter2 = argo_block_writer:write_bytes(BlockWriter1, Value),
                    Kind1 = Kind0#argo_normal_block_encoder{values = BlockWriter2},
                    BlockEncoder2 = BlockEncoder1#argo_block_encoder{kind = Kind1},
                    {BlockEncoder2, CoreWriter2}
            end;
        false ->
            error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                1 => scalar_wire_type_mismatch(BlockEncoder1, argo_scalar_wire_type:bytes())
            })
    end.

-spec encode_fixed(BlockEncoder, CoreWriter, Value) -> {BlockEncoder, CoreWriter} when
    BlockEncoder :: t(), CoreWriter :: argo_core_writer:t(), Value :: binary().
encode_fixed(
    BlockEncoder1 = #argo_block_encoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreWriter1 = #argo_core_writer{},
    Value
) when is_binary(Value) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    Length = byte_size(Value),
    case argo_scalar_wire_type:is_fixed_length(Of, Length) of
        true ->
            case Kind0 of
                #argo_dedupe_block_encoder{} ->
                    error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                        1 => {unsupported_dedupe_block_encoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_encoder{} ->
                    CoreWriter2 = argo_core_writer:write_bytes(CoreWriter1, Value),
                    {BlockEncoder1, CoreWriter2};
                #argo_normal_block_encoder{values = BlockWriter1} ->
                    BlockWriter2 = argo_block_writer:write_bytes(BlockWriter1, Value),
                    Kind1 = Kind0#argo_normal_block_encoder{values = BlockWriter2},
                    BlockEncoder2 = BlockEncoder1#argo_block_encoder{kind = Kind1},
                    {BlockEncoder2, CoreWriter1}
            end;
        false ->
            error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                1 => scalar_wire_type_mismatch(BlockEncoder1, argo_scalar_wire_type:fixed(Length))
            })
    end.

-spec encode_float64(BlockEncoder, CoreWriter, Value) -> {BlockEncoder, CoreWriter} when
    BlockEncoder :: t(), CoreWriter :: argo_core_writer:t(), Value :: float().
encode_float64(
    BlockEncoder1 = #argo_block_encoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreWriter1 = #argo_core_writer{},
    Value
) when is_float(Value) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_float64(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_encoder{} ->
                    error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                        1 => {unsupported_dedupe_block_encoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_encoder{} ->
                    CoreWriter2 = argo_core_writer:write_float64(CoreWriter1, Value),
                    {BlockEncoder1, CoreWriter2};
                #argo_normal_block_encoder{values = BlockWriter1} ->
                    BlockWriter2 = argo_block_writer:write_float64(BlockWriter1, Value),
                    Kind1 = Kind0#argo_normal_block_encoder{values = BlockWriter2},
                    BlockEncoder2 = BlockEncoder1#argo_block_encoder{kind = Kind1},
                    {BlockEncoder2, CoreWriter1}
            end;
        false ->
            error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                1 => scalar_wire_type_mismatch(BlockEncoder1, argo_scalar_wire_type:float64())
            })
    end.

-spec encode_scalar(BlockEncoder, CoreWriter, ScalarValue) -> {BlockEncoder, CoreWriter} when
    BlockEncoder :: t(), CoreWriter :: argo_core_writer:t(), ScalarValue :: argo_scalar_value:t().
encode_scalar(
    BlockEncoder1 = #argo_block_encoder{}, CoreWriter1 = #argo_core_writer{}, ScalarValue = #argo_scalar_value{}
) ->
    case ScalarValue#argo_scalar_value.inner of
        {boolean, Value} -> encode_boolean(BlockEncoder1, CoreWriter1, Value);
        {bytes, Value} -> encode_bytes(BlockEncoder1, CoreWriter1, Value);
        {fixed, Value} -> encode_fixed(BlockEncoder1, CoreWriter1, Value);
        {float64, Value} -> encode_float64(BlockEncoder1, CoreWriter1, Value);
        {string, Value} -> encode_string(BlockEncoder1, CoreWriter1, Value);
        {varint, Value} -> encode_varint(BlockEncoder1, CoreWriter1, Value)
    end.

-spec encode_string(BlockEncoder, CoreWriter, Value) -> {BlockEncoder, CoreWriter} when
    BlockEncoder :: t(), CoreWriter :: argo_core_writer:t(), Value :: unicode:unicode_binary().
encode_string(
    BlockEncoder1 = #argo_block_encoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreWriter1 = #argo_core_writer{},
    Value
) when is_binary(Value) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_string(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_encoder{header = Header, seen = Seen1, values = BlockWriter1} ->
                    case argo_index_set:find_index_of(Value, Seen1) of
                        {ok, Index} ->
                            CoreWriter2 = argo_core_writer:write_labeled_type(CoreWriter1, {backreference, Index}),
                            {BlockEncoder1, CoreWriter2};
                        error ->
                            Seen2 = argo_index_set:add_element(Value, Seen1),
                            CoreWriter2 = argo_core_writer:write_length(CoreWriter1, byte_size(Value)),
                            BlockWriter2 = argo_block_writer:write_string(
                                BlockWriter1, Value, argo_header:null_terminated_strings(Header)
                            ),
                            Kind1 = Kind0#argo_dedupe_block_encoder{seen = Seen2, values = BlockWriter2},
                            BlockEncoder2 = BlockEncoder1#argo_block_encoder{kind = Kind1},
                            {BlockEncoder2, CoreWriter2}
                    end;
                #argo_inline_block_encoder{header = Header} ->
                    CoreWriter2 = argo_core_writer:write_length(CoreWriter1, byte_size(Value)),
                    CoreWriter3 = argo_core_writer:write_string(
                        CoreWriter2, Value, argo_header:null_terminated_strings(Header)
                    ),
                    {BlockEncoder1, CoreWriter3};
                #argo_normal_block_encoder{header = Header, values = BlockWriter1} ->
                    CoreWriter2 = argo_core_writer:write_length(CoreWriter1, byte_size(Value)),
                    BlockWriter2 = argo_block_writer:write_string(
                        BlockWriter1, Value, argo_header:null_terminated_strings(Header)
                    ),
                    Kind1 = Kind0#argo_normal_block_encoder{values = BlockWriter2},
                    BlockEncoder2 = BlockEncoder1#argo_block_encoder{kind = Kind1},
                    {BlockEncoder2, CoreWriter2}
            end;
        false ->
            error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                1 => scalar_wire_type_mismatch(BlockEncoder1, argo_scalar_wire_type:string())
            })
    end.

-spec encode_varint(BlockEncoder, CoreWriter, Value) -> {BlockEncoder, CoreWriter} when
    BlockEncoder :: t(), CoreWriter :: argo_core_writer:t(), Value :: argo_types:i64().
encode_varint(
    BlockEncoder1 = #argo_block_encoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreWriter1 = #argo_core_writer{},
    Value
) when ?is_i64(Value) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_varint(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_encoder{} ->
                    error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                        1 => {unsupported_dedupe_block_encoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_encoder{} ->
                    CoreWriter2 = argo_core_writer:write_varint(CoreWriter1, Value),
                    {BlockEncoder1, CoreWriter2};
                #argo_normal_block_encoder{values = BlockWriter1} ->
                    BlockWriter2 = argo_block_writer:write_varint(BlockWriter1, Value),
                    Kind1 = Kind0#argo_normal_block_encoder{values = BlockWriter2},
                    BlockEncoder2 = BlockEncoder1#argo_block_encoder{kind = Kind1},
                    {BlockEncoder2, CoreWriter1}
            end;
        false ->
            error_with_info(badarg, [BlockEncoder1, CoreWriter1, Value], #{
                1 => scalar_wire_type_mismatch(BlockEncoder1, argo_scalar_wire_type:varint())
            })
    end.

%% @private
-spec scalar_wire_type_mismatch(BlockEncoder, Expected) ->
    {scalar_wire_type_mismatch, #{expected := Expected, key := Key, actual := Actual}}
when
    BlockEncoder :: t(),
    Expected :: argo_scalar_wire_type:t(),
    Key :: argo_types:name(),
    Actual :: argo_scalar_wire_type:t().
scalar_wire_type_mismatch(
    #argo_block_encoder{block_wire_type = #argo_block_wire_type{'of' = Actual, key = Key}}, Expected
) ->
    {scalar_wire_type_mismatch, #{expected => Expected, key => Key, actual => Actual}}.

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
format_error_description(_Key, {scalar_wire_type_mismatch, #{key := Key, expected := Expected0, actual := Actual0}}) ->
    Expected = argo_scalar_wire_type:to_string(Expected0),
    Actual = argo_scalar_wire_type:to_string(Actual0),
    io_lib:format("scalar wire type mismatch for BlockEncoder ~0tp, expected ~ts, but was ~ts", [Key, Expected, Actual]);
format_error_description(_Key, {unsupported_dedupe_block_encoder_scalar_wire_type, Type0}) ->
    Type = argo_scalar_wire_type:to_string(Type0),
    io_lib:format("scalar wire type ~ts does not support deduplication", [Type]);
format_error_description(_Key, {unsupported_normal_block_encoder_scalar_wire_type, Type0}) ->
    Type = argo_scalar_wire_type:to_string(Type0),
    io_lib:format("scalar wire type ~ts does not support non-inline encoding", [Type]);
format_error_description(_Key, Value) ->
    Value.
