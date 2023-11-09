%%% % @format
-module(argo_message_decoder).
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
-include("argo_message.hrl").
-include("argo_wire_type.hrl").

%% API
-export([
    new/3,
    from_reader/1,
    decode_block_boolean/1,
    decode_block_bytes/1,
    decode_block_fixed/2,
    decode_block_float64/1,
    decode_block_scalar/2,
    decode_block_string/1,
    decode_block_varint/1,
    read_core_bytes/2,
    read_core_float64/1,
    read_core_label/1,
    read_core_length/1,
    read_core_string/2,
    read_core_varint/1
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_message_decoder{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Header, Blocks, Core) -> MessageDecoder when
    Header :: argo_header:t(), Blocks :: argo_block_decoders:t(), Core :: argo_core_reader:t(), MessageDecoder :: t().
new(Header = #argo_header{}, Blocks = #argo_block_decoders{}, Core = #argo_core_reader{}) ->
    #argo_message_decoder{header = Header, blocks = Blocks, core = Core}.

-spec from_reader(Reader) -> {Reader, MessageDecoder} when Reader :: binary(), MessageDecoder :: t().
from_reader(Reader1) when is_binary(Reader1) ->
    {Reader2, Header} = argo_header:from_reader(Reader1),
    Blocks1 = argo_block_decoders:new(Header),
    {Reader3, Blocks2} =
        case argo_header:inline_everything(Header) of
            false ->
                from_reader_blocks(Reader2, Blocks1);
            true ->
                {<<>>, argo_block_decoders:push_block(Blocks1, Reader2)}
        end,
    case argo_block_decoders:pop_core(Blocks2) of
        {Blocks3, {some, Core}} ->
            {Reader3, new(Header, Blocks3, Core)};
        {Blocks2, none} ->
            error_with_info(badarg, [Reader1], #{1 => missing_core})
    end.

%% @private
-spec from_reader_blocks(ReaderOrCoreReader, Blocks) -> {Reader, Blocks} when
    ReaderOrCoreReader :: binary() | argo_core_reader:t(), Blocks :: argo_block_decoders:t(), Reader :: binary().
from_reader_blocks(Reader1 = #argo_core_reader{}, Blocks1) ->
    try argo_core_reader:read_length(Reader1) of
        {Reader2, Length} ->
            {Reader3, Block} = argo_core_reader:read_bytes(Reader2, Length),
            Blocks2 = argo_block_decoders:push_block(Blocks1, Block),
            from_reader_blocks(Reader3, Blocks2)
    catch
        error:badarg ->
            Reader2 = Reader1#argo_core_reader.core,
            {Reader2, Blocks1}
    end;
from_reader_blocks(Reader1, Blocks1) when is_binary(Reader1) ->
    Reader2 = argo_core_reader:new(Reader1),
    from_reader_blocks(Reader2, Blocks1).

-spec decode_block_boolean(MessageDecoder) -> {MessageDecoder, Value} when MessageDecoder :: t(), Value :: boolean().
decode_block_boolean(MessageDecoder1 = #argo_message_decoder{blocks = Blocks1, core = Core1}) ->
    {Blocks2, Core2, Value} = argo_block_decoders:decode_boolean(Blocks1, Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{blocks = Blocks2, core = Core2},
    {MessageDecoder2, Value}.

-spec decode_block_bytes(MessageDecoder) -> {MessageDecoder, Value} when MessageDecoder :: t(), Value :: binary().
decode_block_bytes(MessageDecoder1 = #argo_message_decoder{blocks = Blocks1, core = Core1}) ->
    {Blocks2, Core2, Value} = argo_block_decoders:decode_bytes(Blocks1, Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{blocks = Blocks2, core = Core2},
    {MessageDecoder2, Value}.

-spec decode_block_fixed(MessageDecoder, Length) -> {MessageDecoder, Value} when
    MessageDecoder :: t(), Length :: argo_types:length(), Value :: binary().
decode_block_fixed(MessageDecoder1 = #argo_message_decoder{blocks = Blocks1, core = Core1}, Length) when
    ?is_usize(Length)
->
    {Blocks2, Core2, Value} = argo_block_decoders:decode_fixed(Blocks1, Core1, Length),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{blocks = Blocks2, core = Core2},
    {MessageDecoder2, Value}.

-spec decode_block_float64(MessageDecoder) -> {MessageDecoder, Value} when MessageDecoder :: t(), Value :: float().
decode_block_float64(MessageDecoder1 = #argo_message_decoder{blocks = Blocks1, core = Core1}) ->
    {Blocks2, Core2, Value} = argo_block_decoders:decode_float64(Blocks1, Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{blocks = Blocks2, core = Core2},
    {MessageDecoder2, Value}.

-spec decode_block_scalar(MessageDecoder, BlockWireType) -> {MessageDecoder, Value} when
    MessageDecoder :: t(), BlockWireType :: argo_block_wire_type:t(), Value :: argo_scalar_value:t().
decode_block_scalar(
    MessageDecoder1 = #argo_message_decoder{blocks = Blocks1, core = Core1}, BlockWireType = #argo_block_wire_type{}
) ->
    {Blocks2, Core2, Value} = argo_block_decoders:decode_scalar_with_key(
        Blocks1, Core1, BlockWireType#argo_block_wire_type.key, BlockWireType
    ),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{blocks = Blocks2, core = Core2},
    {MessageDecoder2, Value}.

-spec decode_block_string(MessageDecoder) -> {MessageDecoder, Value} when
    MessageDecoder :: t(), Value :: unicode:unicode_binary().
decode_block_string(MessageDecoder1 = #argo_message_decoder{blocks = Blocks1, core = Core1}) ->
    {Blocks2, Core2, Value} = argo_block_decoders:decode_string(Blocks1, Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{blocks = Blocks2, core = Core2},
    {MessageDecoder2, Value}.

-spec decode_block_varint(MessageDecoder) -> {MessageDecoder, Value} when
    MessageDecoder :: t(), Value :: argo_types:i64().
decode_block_varint(MessageDecoder1 = #argo_message_decoder{blocks = Blocks1, core = Core1}) ->
    {Blocks2, Core2, Value} = argo_block_decoders:decode_varint(Blocks1, Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{blocks = Blocks2, core = Core2},
    {MessageDecoder2, Value}.

-spec read_core_bytes(MessageDecoder, Length) -> {MessageDecoder, Value} when
    MessageDecoder :: t(), Length :: argo_types:length(), Value :: binary().
read_core_bytes(MessageDecoder1 = #argo_message_decoder{core = Core1}, Length) when ?is_usize(Length) ->
    {Core2, Value} = argo_core_reader:read_bytes(Core1, Length),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{core = Core2},
    {MessageDecoder2, Value}.

-spec read_core_float64(MessageDecoder) -> {MessageDecoder, Value} when MessageDecoder :: t(), Value :: float().
read_core_float64(MessageDecoder1 = #argo_message_decoder{core = Core1}) ->
    {Core2, Value} = argo_core_reader:read_float64(Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{core = Core2},
    {MessageDecoder2, Value}.

-spec read_core_label(MessageDecoder) -> {MessageDecoder, Label} when MessageDecoder :: t(), Label :: argo_types:i64().
read_core_label(MessageDecoder1 = #argo_message_decoder{core = Core1}) ->
    {Core2, Label} = argo_core_reader:read_label(Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{core = Core2},
    {MessageDecoder2, Label}.

-spec read_core_length(MessageDecoder) -> {MessageDecoder, Length} when
    MessageDecoder :: t(), Length :: argo_types:length().
read_core_length(MessageDecoder1 = #argo_message_decoder{core = Core1}) ->
    {Core2, Length} = argo_core_reader:read_length(Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{core = Core2},
    {MessageDecoder2, Length}.

-spec read_core_string(MessageDecoder, Length) -> {MessageDecoder, Value} when
    MessageDecoder :: t(), Length :: argo_types:length(), Value :: unicode:unicode_binary().
read_core_string(MessageDecoder1 = #argo_message_decoder{header = Header, core = Core1}, Length) when
    ?is_usize(Length)
->
    {Core2, Value} = argo_core_reader:read_string(Core1, Length, argo_header:null_terminated_strings(Header)),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{core = Core2},
    {MessageDecoder2, Value}.

-spec read_core_varint(MessageDecoder) -> {MessageDecoder, Value} when MessageDecoder :: t(), Value :: argo_types:i64().
read_core_varint(MessageDecoder1 = #argo_message_decoder{core = Core1}) ->
    {Core2, Value} = argo_core_reader:read_varint(Core1),
    MessageDecoder2 = MessageDecoder1#argo_message_decoder{core = Core2},
    {MessageDecoder2, Value}.

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
