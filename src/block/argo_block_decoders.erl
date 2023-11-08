%%% % @format
-module(argo_block_decoders).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include("argo_block.hrl").
-include("argo_common.hrl").
-include("argo_core.hrl").
-include("argo_header.hrl").
-include("argo_label.hrl").
-include("argo_wire_type.hrl").

%% API
-export([
    new/1,
    decode_boolean/2,
    decode_bytes/2,
    decode_float64/2,
    decode_scalar_with_key/4,
    decode_string/2,
    decode_varint/2,
    pop_core/1,
    push_block/2
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type maybe_block_wire_type() :: argo_block_wire_type:t() | fun(() -> argo_block_wire_type:t()).
-type t() :: #argo_block_decoders{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header) -> BlockDecoders when Header :: argo_header:t(), BlockDecoders :: t().
new(Header = #argo_header{}) ->
    #argo_block_decoders{header = Header, core_popped = false, blocks = queue:new(), inner = argo_index_map:new()}.

-spec decode_boolean(BlockDecoders, CoreReader) -> {BlockDecoders, CoreReader, Value} when
    BlockDecoders :: t(), CoreReader :: t(), Value :: boolean().
decode_boolean(BlockDecoders1 = #argo_block_decoders{}, CoreReader1 = #argo_core_reader{}) ->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_BOOLEAN,
    {BlockDecoders2 = #argo_block_decoders{inner = Inner1}, BlockDecoder1} = get_or_else(
        BlockDecoders1, Key, fun argo_label:self_describing_blocks_boolean/0
    ),
    {BlockDecoder2, CoreReader2, Value} = argo_block_decoder:decode_boolean(BlockDecoder1, CoreReader1),
    Inner2 = argo_index_map:put(Key, BlockDecoder2, Inner1),
    BlockDecoders3 = BlockDecoders2#argo_block_decoders{inner = Inner2},
    {BlockDecoders3, CoreReader2, Value}.

-spec decode_bytes(BlockDecoders, CoreReader) -> {BlockDecoders, CoreReader, Value} when
    BlockDecoders :: t(), CoreReader :: t(), Value :: binary().
decode_bytes(BlockDecoders1 = #argo_block_decoders{}, CoreReader1 = #argo_core_reader{}) ->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_BYTES,
    {BlockDecoders2 = #argo_block_decoders{inner = Inner1}, BlockDecoder1} = get_or_else(
        BlockDecoders1, Key, fun argo_label:self_describing_blocks_bytes/0
    ),
    {BlockDecoder2, CoreReader2, Value} = argo_block_decoder:decode_bytes(BlockDecoder1, CoreReader1),
    Inner2 = argo_index_map:put(Key, BlockDecoder2, Inner1),
    BlockDecoders3 = BlockDecoders2#argo_block_decoders{inner = Inner2},
    {BlockDecoders3, CoreReader2, Value}.

-spec decode_float64(BlockDecoders, CoreReader) -> {BlockDecoders, CoreReader, Value} when
    BlockDecoders :: t(), CoreReader :: t(), Value :: float().
decode_float64(BlockDecoders1 = #argo_block_decoders{}, CoreReader1 = #argo_core_reader{}) ->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_FLOAT64,
    {BlockDecoders2 = #argo_block_decoders{inner = Inner1}, BlockDecoder1} = get_or_else(
        BlockDecoders1, Key, fun argo_label:self_describing_blocks_float64/0
    ),
    {BlockDecoder2, CoreReader2, Value} = argo_block_decoder:decode_float64(BlockDecoder1, CoreReader1),
    Inner2 = argo_index_map:put(Key, BlockDecoder2, Inner1),
    BlockDecoders3 = BlockDecoders2#argo_block_decoders{inner = Inner2},
    {BlockDecoders3, CoreReader2, Value}.

-spec decode_scalar_with_key(BlockDecoders, CoreReader, Key, MaybeBlockWireType) ->
    {BlockDecoders, CoreReader, ScalarValue}
when
    BlockDecoders :: t(),
    CoreReader :: t(),
    Key :: argo_types:name(),
    MaybeBlockWireType :: maybe_block_wire_type(),
    ScalarValue :: argo_scalar_value:t().
decode_scalar_with_key(
    BlockDecoders1 = #argo_block_decoders{}, CoreReader1 = #argo_core_reader{}, Key, MaybeBlockWireType
) when is_binary(Key) ->
    BlockWireType = resolve_block_wire_type(MaybeBlockWireType),
    {BlockDecoders2 = #argo_block_decoders{inner = Inner1}, BlockDecoder1} = get_or_else(
        BlockDecoders1, Key, BlockWireType
    ),
    {BlockDecoder2, CoreReader2, Value} = argo_block_decoder:decode_scalar(
        BlockDecoder1, CoreReader1, BlockWireType#argo_block_wire_type.'of'
    ),
    Inner2 = argo_index_map:put(Key, BlockDecoder2, Inner1),
    BlockDecoders3 = BlockDecoders2#argo_block_decoders{inner = Inner2},
    {BlockDecoders3, CoreReader2, Value}.

-spec decode_string(BlockDecoders, CoreReader) -> {BlockDecoders, CoreReader, Value} when
    BlockDecoders :: t(), CoreReader :: t(), Value :: unicode:unicode_binary().
decode_string(BlockDecoders1 = #argo_block_decoders{}, CoreReader1 = #argo_core_reader{}) ->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_STRING,
    {BlockDecoders2 = #argo_block_decoders{inner = Inner1}, BlockDecoder1} = get_or_else(
        BlockDecoders1, Key, fun argo_label:self_describing_blocks_string/0
    ),
    {BlockDecoder2, CoreReader2, Value} = argo_block_decoder:decode_string(BlockDecoder1, CoreReader1),
    Inner2 = argo_index_map:put(Key, BlockDecoder2, Inner1),
    BlockDecoders3 = BlockDecoders2#argo_block_decoders{inner = Inner2},
    {BlockDecoders3, CoreReader2, Value}.

-spec decode_varint(BlockDecoders, CoreReader) -> {BlockDecoders, CoreReader, Value} when
    BlockDecoders :: t(), CoreReader :: t(), Value :: argo_types:i64().
decode_varint(BlockDecoders1 = #argo_block_decoders{}, CoreReader1 = #argo_core_reader{}) ->
    Key = ?ARGO_LABEL_SELF_DESCRIBING_BLOCK_VARINT,
    {BlockDecoders2 = #argo_block_decoders{inner = Inner1}, BlockDecoder1} = get_or_else(
        BlockDecoders1, Key, fun argo_label:self_describing_blocks_varint/0
    ),
    {BlockDecoder2, CoreReader2, Value} = argo_block_decoder:decode_varint(BlockDecoder1, CoreReader1),
    Inner2 = argo_index_map:put(Key, BlockDecoder2, Inner1),
    BlockDecoders3 = BlockDecoders2#argo_block_decoders{inner = Inner2},
    {BlockDecoders3, CoreReader2, Value}.

-spec pop_core(BlockDecoders) -> {BlockDecoders, none | {some, CoreReader}} when
    BlockDecoders :: t(), CoreReader :: argo_core_reader:t().
pop_core(BlockDecoders1 = #argo_block_decoders{core_popped = true}) ->
    {BlockDecoders1, none};
pop_core(BlockDecoders1 = #argo_block_decoders{blocks = Blocks1}) ->
    case queue:out_r(Blocks1) of
        {{value, Core}, Blocks2} ->
            CoreReader = argo_core_reader:new(Core),
            BlockDecoders2 = BlockDecoders1#argo_block_decoders{core_popped = true, blocks = Blocks2},
            {BlockDecoders2, {some, CoreReader}};
        {empty, Blocks1} ->
            {BlockDecoders1, none}
    end.

-spec push_block(BlockDecoders, Block) -> BlockDecoders when BlockDecoders :: t(), Block :: binary().
push_block(BlockDecoders1 = #argo_block_decoders{blocks = Blocks1}, Block) when is_binary(Block) ->
    Blocks2 = queue:in(Block, Blocks1),
    BlockDecoders2 = BlockDecoders1#argo_block_decoders{blocks = Blocks2},
    BlockDecoders2.

%% @private
-spec get_or_else(BlockDecoders, Key, MaybeBlockWireType) -> {BlockDecoders, BlockDecoder} when
    BlockDecoders :: t(),
    Key :: argo_types:name(),
    MaybeBlockWireType :: maybe_block_wire_type(),
    BlockDecoder :: argo_block_decoder:t().
get_or_else(
    BlockDecoders1 = #argo_block_decoders{header = Header, blocks = Blocks1, inner = Inner1}, Key, MaybeBlockWireType
) ->
    case argo_index_map:find(Key, Inner1) of
        {ok, BlockDecoder1 = #argo_block_decoder{}} ->
            {BlockDecoders1, BlockDecoder1};
        error ->
            BlockWireType = #argo_block_wire_type{'of' = Of} = resolve_block_wire_type(MaybeBlockWireType),
            case argo_header:inline_everything(Header) orelse argo_scalar_wire_type:is_boolean(Of) of
                false ->
                    case queue:out(Blocks1) of
                        {{value, Block}, Blocks2} ->
                            BlockDecoder1 = argo_block_decoder:new(Header, BlockWireType, {some, Block}),
                            Inner2 = argo_index_map:put(Key, BlockDecoder1, Inner1),
                            BlockDecoders2 = BlockDecoders1#argo_block_decoders{blocks = Blocks2, inner = Inner2},
                            {BlockDecoders2, BlockDecoder1};
                        {empty, Blocks1} ->
                            error_with_info(badarg, [BlockDecoders1, Key, MaybeBlockWireType], #{
                                1 => {no_more_blocks_available, Key}
                            })
                    end;
                true ->
                    BlockDecoder1 = argo_block_decoder:new(Header, BlockWireType, none),
                    Inner2 = argo_index_map:put(Key, BlockDecoder1, Inner1),
                    BlockDecoders2 = BlockDecoders1#argo_block_decoders{inner = Inner2},
                    {BlockDecoders2, BlockDecoder1}
            end
    end.

%% @private
-spec resolve_block_wire_type(MaybeBlockWireType) -> BlockWireType when
    MaybeBlockWireType :: maybe_block_wire_type(),
    BlockWireType :: argo_block_wire_type:t().
resolve_block_wire_type(BlockWireType = #argo_block_wire_type{}) ->
    BlockWireType;
resolve_block_wire_type(Function) when is_function(Function, 0) ->
    BlockWireType = #argo_block_wire_type{} = Function(),
    BlockWireType.

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
format_error_description(_Key, {no_more_blocks_available, Key}) ->
    io_lib:format("no more blocks available to create BlockDecoder for key: ~0tp", [Key]);
format_error_description(_Key, Value) ->
    Value.
