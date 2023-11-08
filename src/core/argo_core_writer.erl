%%% % @format
-module(argo_core_writer).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include("argo_common.hrl").
-include("argo_core.hrl").
-include("argo_label.hrl").

%% API
-export([
    new/1,
    to_writer/2,
    write_bytes/2,
    write_float64/2,
    write_label/2,
    write_labeled_type/2,
    write_length/2,
    write_omittable_type/3,
    write_string/3,
    write_varint/2
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_core_writer{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Core) -> CoreWriter when Core :: binary(), CoreWriter :: t().
new(Core) when is_binary(Core) ->
    #argo_core_writer{core = Core}.

-spec to_writer(CoreWriter, InlineEverything) -> Writer when
    CoreWriter :: t(), InlineEverything :: boolean(), Writer :: binary().
to_writer(#argo_core_writer{core = Core}, InlineEverything) when is_boolean(InlineEverything) ->
    case InlineEverything of
        false ->
            <<(argo_varint:write_zigzag_i64(byte_size(Core)))/bytes, Core/bytes>>;
        true ->
            Core
    end.

%% @private
-spec backreference_sanity_check(Backreference) -> ok when Backreference :: integer().
backreference_sanity_check(Backreference) when ?is_u32(Backreference) ->
    ok;
backreference_sanity_check(Backreference) when is_integer(Backreference) ->
    error_with_info(badarg, [Backreference], #{1 => {invalid_backreference, Backreference}}).

%% @private
-spec length_sanity_check(Length) -> ok when Length :: integer().
length_sanity_check(Length) when ?is_u32(Length) ->
    ok;
length_sanity_check(Length) when is_integer(Length) andalso Length < 0 ->
    error_with_info(badarg, [Length], #{1 => {invalid_length_negative, Length}});
length_sanity_check(Length) when is_integer(Length) ->
    error_with_info(badarg, [Length], #{1 => {invalid_length_too_large, Length}}).

-spec write_bytes(CoreWriter, Bytes) -> CoreWriter when CoreWriter :: t(), Bytes :: binary().
write_bytes(W0 = #argo_core_writer{core = Core0}, Bytes) when is_binary(Bytes) ->
    ok = length_sanity_check(byte_size(Bytes)),
    Core1 = <<Core0/bytes, Bytes/bytes>>,
    W1 = W0#argo_core_writer{core = Core1},
    W1.

-spec write_float64(CoreWriter, Float64) -> CoreWriter when CoreWriter :: t(), Float64 :: float().
write_float64(W0 = #argo_core_writer{core = Core0}, Float64) when is_float(Float64) ->
    Core1 = <<Core0/bytes, Float64:1/float-little-unit:64>>,
    W1 = W0#argo_core_writer{core = Core1},
    W1.

-spec write_label(CoreWriter, Label) -> CoreWriter when CoreWriter :: t(), Label :: argo_types:label().
write_label(W = #argo_core_writer{}, Label) ->
    write_varint(W, Label).

-spec write_labeled_type(CoreWriter, LabeledType) -> CoreWriter when
    CoreWriter :: t(), LabeledType :: argo_core:labeled_type().
write_labeled_type(W = #argo_core_writer{}, {backreference, Backreference}) when ?is_usize(Backreference) ->
    ok = backreference_sanity_check(Backreference),
    Label = ?ARGO_LABEL_MARKER_LOWEST_RESERVED_VALUE - Backreference,
    write_label(W, Label);
write_labeled_type(W = #argo_core_writer{}, {length, Length}) when ?is_usize(Length) ->
    write_length(W, Length).

-spec write_length(CoreWriter, Length) -> CoreWriter when CoreWriter :: t(), Length :: argo_types:length().
write_length(W0 = #argo_core_writer{}, Length) when ?is_usize(Length) ->
    ok = length_sanity_check(Length),
    write_varint(W0, Length).

-spec write_omittable_type(CoreWriter, OmittableType, IsLabeled) -> CoreWriter when
    CoreWriter :: t(), OmittableType :: argo_core:omittable_type(), IsLabeled :: boolean().
write_omittable_type(W = #argo_core_writer{}, absent, IsLabeled) when is_boolean(IsLabeled) ->
    write_label(W, ?ARGO_LABEL_MARKER_ABSENT);
write_omittable_type(W = #argo_core_writer{}, non_null, true) ->
    write_label(W, ?ARGO_LABEL_MARKER_NON_NULL);
write_omittable_type(W = #argo_core_writer{}, non_null, false) ->
    W.

-spec write_string(CoreWriter, String, NullTerminatedStrings) -> CoreWriter when
    CoreWriter :: t(), String :: unicode:unicode_binary(), NullTerminatedStrings :: boolean().
write_string(W0 = #argo_core_writer{core = Core0}, String, NullTerminatedStrings) when
    is_binary(String) andalso is_boolean(NullTerminatedStrings)
->
    ok = length_sanity_check(byte_size(String)),
    Core1 =
        case NullTerminatedStrings of
            true ->
                <<Core0/bytes, String/bytes, 0:8>>;
            false ->
                <<Core0/bytes, String/bytes>>
        end,
    W1 = W0#argo_core_writer{core = Core1},
    W1.

-spec write_varint(CoreWriter, Varint) -> CoreWriter when CoreWriter :: t(), Varint :: argo_types:i64().
write_varint(W0 = #argo_core_writer{core = Core0}, Varint) when ?is_i64(Varint) ->
    Core1 = <<Core0/bytes, (argo_varint:write_zigzag_i64(Varint))/bytes>>,
    W1 = W0#argo_core_writer{core = Core1},
    W1.

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
format_error_description(_Key, {invalid_backreference, Actual}) ->
    io_lib:format("invalid encoded backreference, expected non-negative value to be <= u32::MAX, but was ~w", [Actual]);
format_error_description(_Key, {invalid_length_negative, Actual}) ->
    io_lib:format("invalid encoded length, expected non-negative value, but was ~w", [Actual]);
format_error_description(_Key, {invalid_length_too_large, Actual}) ->
    io_lib:format("invalid encoded length, expected value to be <= u32::MAX, but was ~w", [Actual]);
format_error_description(_Key, Value) ->
    Value.
