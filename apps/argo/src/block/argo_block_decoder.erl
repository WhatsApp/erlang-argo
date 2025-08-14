%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_block_decoder).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/3,
    decode_boolean/2,
    decode_bytes/2,
    decode_fixed/2,
    decode_float64/2,
    decode_scalar/3,
    decode_string/2,
    decode_varint/2
]).

%% Errors API
-export([
    format_error/2
]).

%% Records
-record(argo_dedupe_block_decoder, {
    header :: argo_header:t(),
    references :: array:array(binary()),
    values :: argo_block_reader:t()
}).

-record(argo_inline_block_decoder, {
    header :: argo_header:t()
}).

-record(argo_normal_block_decoder, {
    header :: argo_header:t(),
    values :: argo_block_reader:t()
}).

%% Types
-type kind() :: #argo_dedupe_block_decoder{} | #argo_inline_block_decoder{} | #argo_normal_block_decoder{}.
-type t() :: #argo_block_decoder{}.

-export_type([
    kind/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header, BlockWireType, none | {some, Block}) -> BlockDecoder when
    Header :: argo_header:t(), BlockWireType :: argo_block_wire_type:t(), Block :: binary(), BlockDecoder :: t().
new(Header = #argo_header{}, BlockWireType = #argo_block_wire_type{'of' = Of, dedupe = Dedupe}, OptionBlock) ->
    InlineEverything = argo_header:inline_everything(Header),
    NoDeduplication = argo_header:no_deduplication(Header),
    Kind =
        case InlineEverything =:= true orelse argo_scalar_wire_type:is_boolean(Of) of
            true ->
                case OptionBlock of
                    none ->
                        #argo_inline_block_decoder{header = Header};
                    {some, _} ->
                        error_with_info(badarg, [Header, BlockWireType, OptionBlock], #{
                            3 => unsupported_inline_block_decoder_must_have_empty_block
                        })
                end;
            false ->
                case Dedupe =:= true andalso not NoDeduplication of
                    true ->
                        case argo_scalar_wire_type:supports_deduplication(Of) of
                            true ->
                                case OptionBlock of
                                    {some, Block} when is_binary(Block) ->
                                        #argo_dedupe_block_decoder{
                                            header = Header,
                                            references = argo_types:dynamic_cast(array:new(0, fixed)),
                                            values = argo_block_reader:new(Block)
                                        };
                                    none ->
                                        error_with_info(badarg, [Header, BlockWireType, OptionBlock], #{
                                            3 => unsupported_dedupe_block_decoder_must_have_non_empty_block
                                        })
                                end;
                            false ->
                                error_with_info(badarg, [Header, BlockWireType, OptionBlock], #{
                                    2 => {unsupported_dedupe_block_decoder_scalar_wire_type, Of}
                                })
                        end;
                    false ->
                        case OptionBlock of
                            {some, Block} when is_binary(Block) ->
                                #argo_normal_block_decoder{header = Header, values = argo_block_reader:new(Block)};
                            none ->
                                error_with_info(badarg, [Header, BlockWireType, OptionBlock], #{
                                    3 => unsupported_normal_block_decoder_must_have_non_empty_block
                                })
                        end
                end
        end,
    BlockDecoder = #argo_block_decoder{block_wire_type = BlockWireType, kind = Kind},
    BlockDecoder.

-spec decode_boolean(BlockDecoder, CoreReader) -> {BlockDecoder, CoreReader, Value} when
    BlockDecoder :: t(), CoreReader :: argo_core_reader:t(), Value :: boolean().
decode_boolean(
    BlockDecoder1 = #argo_block_decoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreReader1 = #argo_core_reader{}
) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_boolean(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_decoder{} ->
                    error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                        1 => {unsupported_dedupe_block_decoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_decoder{} ->
                    {CoreReader2, Label} = argo_core_reader:read_label(CoreReader1),
                    Value =
                        case Label of
                            ?ARGO_LABEL_MARKER_FALSE ->
                                false;
                            ?ARGO_LABEL_MARKER_TRUE ->
                                true;
                            Actual ->
                                error_with_info(badarg, [BlockDecoder1, CoreReader1], #{2 => {invalid_boolean, Actual}})
                        end,
                    {BlockDecoder1, CoreReader2, Value};
                #argo_normal_block_decoder{} ->
                    error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                        1 => {unsupported_normal_block_decoder_scalar_wire_type, Of}
                    })
            end;
        false ->
            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                1 => scalar_wire_type_mismatch(BlockDecoder1, argo_scalar_wire_type:boolean())
            })
    end.

-spec decode_bytes(BlockDecoder, CoreReader) -> {BlockDecoder, CoreReader, Value} when
    BlockDecoder :: t(), CoreReader :: argo_core_reader:t(), Value :: binary().
decode_bytes(
    BlockDecoder1 = #argo_block_decoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreReader1 = #argo_core_reader{}
) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_bytes(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_decoder{references = References1, values = Values1} ->
                    ReferencesSize = array:size(References1),
                    {CoreReader2, LabeledType} = argo_core_reader:read_labeled_type(CoreReader1),
                    case LabeledType of
                        {backreference, Index} when Index < ReferencesSize ->
                            Value = array:get(Index, References1),
                            {BlockDecoder1, CoreReader2, Value};
                        {backreference, Index} ->
                            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{2 => {invalid_backreference, Index}});
                        {length, Length} ->
                            {Values2, Value} = argo_block_reader:read_bytes(Values1, Length),
                            References2 = array:resize(ReferencesSize + 1, References1),
                            References3 = array:set(ReferencesSize, Value, References2),
                            Kind1 = Kind0#argo_dedupe_block_decoder{references = References3, values = Values2},
                            BlockDecoder2 = BlockDecoder1#argo_block_decoder{kind = Kind1},
                            {BlockDecoder2, CoreReader2, Value}
                    end;
                #argo_inline_block_decoder{} ->
                    {CoreReader2, Length} = argo_core_reader:read_length(CoreReader1),
                    {CoreReader3, Value} = argo_core_reader:read_bytes(CoreReader2, Length),
                    {BlockDecoder1, CoreReader3, Value};
                #argo_normal_block_decoder{values = Values1} ->
                    {CoreReader2, Length} = argo_core_reader:read_length(CoreReader1),
                    {Values2, Value} = argo_block_reader:read_bytes(Values1, Length),
                    Kind1 = Kind0#argo_normal_block_decoder{values = Values2},
                    BlockDecoder2 = BlockDecoder1#argo_block_decoder{kind = Kind1},
                    {BlockDecoder2, CoreReader2, Value}
            end;
        false ->
            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                1 => scalar_wire_type_mismatch(BlockDecoder1, argo_scalar_wire_type:bytes())
            })
    end.

-spec decode_fixed(BlockDecoder, CoreReader) -> {BlockDecoder, CoreReader, Value} when
    BlockDecoder :: t(), CoreReader :: argo_core_reader:t(), Value :: binary().
decode_fixed(
    BlockDecoder1 = #argo_block_decoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreReader1 = #argo_core_reader{}
) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_fixed(Of) of
        true ->
            % eqwalizer:ignore argo_scalar_wire_type:is_fixed/1 already checked that this is safe
            Length = Of#argo_scalar_wire_type.inner#argo_fixed_wire_type.length,
            case Kind0 of
                #argo_dedupe_block_decoder{} ->
                    error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                        1 => {unsupported_dedupe_block_decoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_decoder{} ->
                    {CoreReader2, Value} = argo_core_reader:read_bytes(CoreReader1, Length),
                    {BlockDecoder1, CoreReader2, Value};
                #argo_normal_block_decoder{values = Values1} ->
                    {Values2, Value} = argo_block_reader:read_bytes(Values1, Length),
                    Kind1 = Kind0#argo_normal_block_decoder{values = Values2},
                    BlockDecoder2 = BlockDecoder1#argo_block_decoder{kind = Kind1},
                    {BlockDecoder2, CoreReader1, Value}
            end;
        false ->
            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                1 => scalar_wire_type_mismatch(BlockDecoder1, argo_scalar_wire_type:fixed(0))
            })
    end.

-spec decode_float64(BlockDecoder, CoreReader) -> {BlockDecoder, CoreReader, Value} when
    BlockDecoder :: t(), CoreReader :: argo_core_reader:t(), Value :: float().
decode_float64(
    BlockDecoder1 = #argo_block_decoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreReader1 = #argo_core_reader{}
) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_float64(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_decoder{} ->
                    error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                        1 => {unsupported_dedupe_block_decoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_decoder{} ->
                    {CoreReader2, Value} = argo_core_reader:read_float64(CoreReader1),
                    {BlockDecoder1, CoreReader2, Value};
                #argo_normal_block_decoder{values = Values1} ->
                    {Values2, Value} = argo_block_reader:read_float64(Values1),
                    Kind1 = Kind0#argo_normal_block_decoder{values = Values2},
                    BlockDecoder2 = BlockDecoder1#argo_block_decoder{kind = Kind1},
                    {BlockDecoder2, CoreReader1, Value}
            end;
        false ->
            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                1 => scalar_wire_type_mismatch(BlockDecoder1, argo_scalar_wire_type:float64())
            })
    end.

-spec decode_scalar(BlockDecoder, CoreReader, ScalarWireType) -> {BlockDecoder, CoreReader, ScalarValue} when
    BlockDecoder :: t(),
    CoreReader :: argo_core_reader:t(),
    ScalarWireType :: argo_scalar_wire_type:t(),
    ScalarValue :: argo_scalar_value:t().
decode_scalar(
    BlockDecoder1 = #argo_block_decoder{}, CoreReader1 = #argo_core_reader{}, ScalarWireType = #argo_scalar_wire_type{}
) ->
    case ScalarWireType#argo_scalar_wire_type.inner of
        boolean ->
            {BlockDecoder2, CoreReader2, Value} = decode_boolean(BlockDecoder1, CoreReader1),
            ScalarValue = argo_scalar_value:boolean(Value),
            {BlockDecoder2, CoreReader2, ScalarValue};
        bytes ->
            {BlockDecoder2, CoreReader2, Value} = decode_bytes(BlockDecoder1, CoreReader1),
            ScalarValue = argo_scalar_value:bytes(Value),
            {BlockDecoder2, CoreReader2, ScalarValue};
        #argo_fixed_wire_type{length = _Length} ->
            {BlockDecoder2, CoreReader2, Value} = decode_fixed(BlockDecoder1, CoreReader1),
            ScalarValue = argo_scalar_value:fixed(Value),
            {BlockDecoder2, CoreReader2, ScalarValue};
        float64 ->
            {BlockDecoder2, CoreReader2, Value} = decode_float64(BlockDecoder1, CoreReader1),
            ScalarValue = argo_scalar_value:float64(Value),
            {BlockDecoder2, CoreReader2, ScalarValue};
        string ->
            {BlockDecoder2, CoreReader2, Value} = decode_string(BlockDecoder1, CoreReader1),
            ScalarValue = argo_scalar_value:string(Value),
            {BlockDecoder2, CoreReader2, ScalarValue};
        varint ->
            {BlockDecoder2, CoreReader2, Value} = decode_varint(BlockDecoder1, CoreReader1),
            ScalarValue = argo_scalar_value:varint(Value),
            {BlockDecoder2, CoreReader2, ScalarValue}
    end.

-spec decode_string(BlockDecoder, CoreReader) -> {BlockDecoder, CoreReader, Value} when
    BlockDecoder :: t(), CoreReader :: argo_core_reader:t(), Value :: unicode:unicode_binary().
decode_string(
    BlockDecoder1 = #argo_block_decoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreReader1 = #argo_core_reader{}
) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_string(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_decoder{header = Header, references = References1, values = Values1} ->
                    ReferencesSize = array:size(References1),
                    {CoreReader2, LabeledType} = argo_core_reader:read_labeled_type(CoreReader1),
                    case LabeledType of
                        {backreference, Index} when Index < ReferencesSize ->
                            Value = array:get(Index, References1),
                            {BlockDecoder1, CoreReader2, Value};
                        {backreference, Index} ->
                            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{2 => {invalid_backreference, Index}});
                        {length, Length} ->
                            {Values2, Value} = argo_block_reader:read_string(
                                Values1, Length, argo_header:null_terminated_strings(Header)
                            ),
                            References2 = array:resize(ReferencesSize + 1, References1),
                            References3 = array:set(ReferencesSize, Value, References2),
                            Kind1 = Kind0#argo_dedupe_block_decoder{references = References3, values = Values2},
                            BlockDecoder2 = BlockDecoder1#argo_block_decoder{kind = Kind1},
                            {BlockDecoder2, CoreReader2, Value}
                    end;
                #argo_inline_block_decoder{header = Header} ->
                    {CoreReader2, Length} = argo_core_reader:read_length(CoreReader1),
                    {CoreReader3, Value} = argo_core_reader:read_string(
                        CoreReader2, Length, argo_header:null_terminated_strings(Header)
                    ),
                    {BlockDecoder1, CoreReader3, Value};
                #argo_normal_block_decoder{header = Header, values = Values1} ->
                    {CoreReader2, Length} = argo_core_reader:read_length(CoreReader1),
                    {Values2, Value} = argo_block_reader:read_string(
                        Values1, Length, argo_header:null_terminated_strings(Header)
                    ),
                    Kind1 = Kind0#argo_normal_block_decoder{values = Values2},
                    BlockDecoder2 = BlockDecoder1#argo_block_decoder{kind = Kind1},
                    {BlockDecoder2, CoreReader2, Value}
            end;
        false ->
            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                1 => scalar_wire_type_mismatch(BlockDecoder1, argo_scalar_wire_type:string())
            })
    end.

-spec decode_varint(BlockDecoder, CoreReader) -> {BlockDecoder, CoreReader, Value} when
    BlockDecoder :: t(), CoreReader :: argo_core_reader:t(), Value :: argo_types:i64().
decode_varint(
    BlockDecoder1 = #argo_block_decoder{block_wire_type = BlockWireType, kind = Kind0},
    CoreReader1 = #argo_core_reader{}
) ->
    Of = BlockWireType#argo_block_wire_type.'of',
    case argo_scalar_wire_type:is_varint(Of) of
        true ->
            case Kind0 of
                #argo_dedupe_block_decoder{} ->
                    error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                        1 => {unsupported_dedupe_block_decoder_scalar_wire_type, Of}
                    });
                #argo_inline_block_decoder{} ->
                    {CoreReader2, Value} = argo_core_reader:read_varint(CoreReader1),
                    {BlockDecoder1, CoreReader2, Value};
                #argo_normal_block_decoder{values = Values1} ->
                    {Values2, Value} = argo_block_reader:read_varint(Values1),
                    Kind1 = Kind0#argo_normal_block_decoder{values = Values2},
                    BlockDecoder2 = BlockDecoder1#argo_block_decoder{kind = Kind1},
                    {BlockDecoder2, CoreReader1, Value}
            end;
        false ->
            error_with_info(badarg, [BlockDecoder1, CoreReader1], #{
                1 => scalar_wire_type_mismatch(BlockDecoder1, argo_scalar_wire_type:varint())
            })
    end.

%% @private
-spec scalar_wire_type_mismatch(BlockDecoder, Expected) ->
    {scalar_wire_type_mismatch, #{expected := Expected, key := Key, actual := Actual}}
when
    BlockDecoder :: t(),
    Expected :: argo_scalar_wire_type:t(),
    Key :: argo_types:name(),
    Actual :: argo_scalar_wire_type:t().
scalar_wire_type_mismatch(
    #argo_block_decoder{block_wire_type = #argo_block_wire_type{'of' = Actual, key = Key}}, Expected
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
format_error_description(_Key, {invalid_backreference, Index}) ->
    io_lib:format("invalid backreference: ~w", [Index]);
format_error_description(_Key, {invalid_boolean, Actual}) ->
    io_lib:format("invalid Boolean label, expected 0 or 1, but was ~w", [Actual]);
format_error_description(_Key, {scalar_wire_type_mismatch, #{key := Key, expected := Expected0, actual := Actual0}}) ->
    Expected = argo_scalar_wire_type:to_string(Expected0),
    Actual = argo_scalar_wire_type:to_string(Actual0),
    io_lib:format("scalar wire type mismatch for BlockDecoder ~0tp, expected ~ts, but was ~ts", [Key, Expected, Actual]);
format_error_description(_Key, unsupported_dedupe_block_decoder_must_have_non_empty_block) ->
    "dedupe block decoder must have a non-empty block";
format_error_description(_Key, {unsupported_dedupe_block_decoder_scalar_wire_type, Type0}) ->
    Type = argo_scalar_wire_type:to_string(Type0),
    io_lib:format("scalar wire type ~ts does not support deduplication", [Type]);
format_error_description(_Key, unsupported_inline_block_decoder_must_have_empty_block) ->
    "inline block decoder must have an empty block";
format_error_description(_Key, unsupported_normal_block_decoder_must_have_non_empty_block) ->
    "normal block decoder must have a non-empty block";
format_error_description(_Key, {unsupported_normal_block_decoder_scalar_wire_type, Type0}) ->
    Type = argo_scalar_wire_type:to_string(Type0),
    io_lib:format("scalar wire type ~ts does not support non-inline decoding", [Type]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
