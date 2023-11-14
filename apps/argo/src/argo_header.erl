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
-module(argo_header).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile(
    {inline, [
        error_with_info/3
    ]}
).

-include_lib("argo/include/argo_header.hrl").

%% Codec API
-export([
    from_reader/1,
    to_writer/1
]).

%% API
-export([
    new/0,
    inline_everything/1,
    set_inline_everything/2,
    self_describing/1,
    set_self_describing/2,
    out_of_band_field_errors/1,
    set_out_of_band_field_errors/2,
    self_describing_errors/1,
    set_self_describing_errors/2,
    null_terminated_strings/1,
    set_null_terminated_strings/2,
    no_deduplication/1,
    set_no_deduplication/2,
    has_user_flags/1,
    set_has_user_flags/2,
    user_flags/1,
    set_user_flags/2
]).

%% Errors API
-export([
    format_error/2
]).

%% Types
-type t() :: #argo_header{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% Codec API functions
%%%=============================================================================

-spec from_reader(Reader) -> {Reader, Header} when Reader :: binary(), Header :: t().
from_reader(
    Reader1 =
        <<0:1, HasUserFlags1:1, NoDeduplication:1, NullTerminatedStrings:1, SelfDescribingErrors:1,
            OutOfBandFieldErrors:1, SelfDescribing:1, InlineEverything:1, Reader2/bits>>
) ->
    HasUserFlags2 = decode_bit(HasUserFlags1),
    {Reader3, UserFlags} =
        case HasUserFlags2 of
            true ->
                try decode_varbit(Reader2, <<>>) of
                    {R3, UFlags} ->
                        {R3, UFlags}
                catch
                    throw:not_enough_data ->
                        error_with_info(badarg, [Reader1], #{1 => not_enough_data})
                end;
            false ->
                {Reader2, undefined}
        end,
    Header = #argo_header{
        inline_everything = decode_bit(InlineEverything),
        self_describing = decode_bit(SelfDescribing),
        out_of_band_field_errors = decode_bit(OutOfBandFieldErrors),
        self_describing_errors = decode_bit(SelfDescribingErrors),
        null_terminated_strings = decode_bit(NullTerminatedStrings),
        no_deduplication = decode_bit(NoDeduplication),
        has_user_flags = HasUserFlags2,
        user_flags = UserFlags
    },
    {Reader3, Header};
from_reader(Reader) when is_bitstring(Reader) ->
    error_with_info(badarg, [Reader], #{1 => not_enough_data}).

%% @private
-spec decode_bit(0 | 1) -> boolean().
decode_bit(0) -> false;
decode_bit(1) -> true.

%% @private
%% TODO: T169479375 limit decoding to X number of bits
-spec decode_varbit(Reader, Varbit) -> {Reader, Varbit} when Reader :: binary(), Varbit :: bitstring().
decode_varbit(<<Head:7/bits, 1:1, Rest/bits>>, Varbit) ->
    decode_varbit(Rest, <<Varbit/bits, Head:7/bits>>);
decode_varbit(<<Head:7/bits, 0:1, Rest/bits>>, Varbit) ->
    {Rest, <<Varbit/bits, Head:7/bits>>};
decode_varbit(<<>>, _Varbit) ->
    throw(not_enough_data).

-spec to_writer(Header) -> Writer when Header :: t(), Writer :: binary().
to_writer(H = #argo_header{}) ->
    <<
        0:1,
        (encode_bit(has_user_flags(H))):1/bits,
        (encode_bit(no_deduplication(H))):1/bits,
        (encode_bit(null_terminated_strings(H))):1/bits,
        (encode_bit(self_describing_errors(H))):1/bits,
        (encode_bit(out_of_band_field_errors(H))):1/bits,
        (encode_bit(self_describing(H))):1/bits,
        (encode_bit(inline_everything(H))):1/bits,
        (encode_varbit(user_flags(H)))/bits
    >>.

%% @private
-spec encode_bit(boolean()) -> bitstring().
encode_bit(false) -> <<0:1>>;
encode_bit(true) -> <<1:1>>.

%% @private
-spec encode_varbit(undefined | bitstring()) -> binary().
encode_varbit(undefined) ->
    <<>>;
encode_varbit(<<Chunk:7/bits>>) ->
    <<Chunk:7/bits, 0:1>>;
encode_varbit(<<Chunk:6/bits>>) ->
    <<Chunk:6/bits, 0:2>>;
encode_varbit(<<Chunk:5/bits>>) ->
    <<Chunk:5/bits, 0:3>>;
encode_varbit(<<Chunk:4/bits>>) ->
    <<Chunk:4/bits, 0:4>>;
encode_varbit(<<Chunk:3/bits>>) ->
    <<Chunk:3/bits, 0:5>>;
encode_varbit(<<Chunk:2/bits>>) ->
    <<Chunk:2/bits, 0:6>>;
encode_varbit(<<Chunk:1/bits>>) ->
    <<Chunk:1/bits, 0:7>>;
encode_varbit(<<>>) ->
    <<0:8>>;
encode_varbit(<<Chunk:7/bits, Rest/bits>>) ->
    <<Chunk:7/bits, 1:1, (encode_varbit(Rest))/bits>>.

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> t().
new() ->
    #argo_header{}.

-spec inline_everything(t()) -> boolean().
inline_everything(#argo_header{inline_everything = V}) ->
    V =:= true.

-spec set_inline_everything(Header, New) -> {Header, Old} when Header :: t(), New :: boolean(), Old :: boolean().
set_inline_everything(H0 = #argo_header{inline_everything = Old}, New) when is_boolean(New) ->
    H1 = H0#argo_header{inline_everything = New},
    {H1, Old =:= true}.

-spec self_describing(t()) -> boolean().
self_describing(#argo_header{self_describing = V}) ->
    V =:= true.

-spec set_self_describing(Header, New) -> {Header, Old} when Header :: t(), New :: boolean(), Old :: boolean().
set_self_describing(H0 = #argo_header{self_describing = Old}, New) when is_boolean(New) ->
    H1 = H0#argo_header{self_describing = New},
    {H1, Old =:= true}.

-spec out_of_band_field_errors(t()) -> boolean().
out_of_band_field_errors(#argo_header{out_of_band_field_errors = V}) ->
    V =:= true.

-spec set_out_of_band_field_errors(Header, New) -> {Header, Old} when Header :: t(), New :: boolean(), Old :: boolean().
set_out_of_band_field_errors(H0 = #argo_header{out_of_band_field_errors = Old}, New) when is_boolean(New) ->
    H1 = H0#argo_header{out_of_band_field_errors = New},
    {H1, Old =:= true}.

-spec self_describing_errors(t()) -> boolean().
self_describing_errors(#argo_header{self_describing_errors = V}) ->
    V =:= true.

-spec set_self_describing_errors(Header, New) -> {Header, Old} when Header :: t(), New :: boolean(), Old :: boolean().
set_self_describing_errors(H0 = #argo_header{self_describing_errors = Old}, New) when is_boolean(New) ->
    H1 = H0#argo_header{self_describing_errors = New},
    {H1, Old =:= true}.

-spec null_terminated_strings(t()) -> boolean().
null_terminated_strings(#argo_header{null_terminated_strings = V}) ->
    V =:= true.

-spec set_null_terminated_strings(Header, New) -> {Header, Old} when Header :: t(), New :: boolean(), Old :: boolean().
set_null_terminated_strings(H0 = #argo_header{null_terminated_strings = Old}, New) when is_boolean(New) ->
    H1 = H0#argo_header{null_terminated_strings = New},
    {H1, Old =:= true}.

-spec no_deduplication(t()) -> boolean().
no_deduplication(#argo_header{no_deduplication = V}) ->
    V =:= true.

-spec set_no_deduplication(Header, New) -> {Header, Old} when Header :: t(), New :: boolean(), Old :: boolean().
set_no_deduplication(H0 = #argo_header{no_deduplication = Old}, New) when is_boolean(New) ->
    H1 = H0#argo_header{no_deduplication = New},
    {H1, Old =:= true}.

-spec has_user_flags(t()) -> boolean().
has_user_flags(#argo_header{has_user_flags = V}) ->
    V =:= true.

-spec set_has_user_flags(Header, New) -> {Header, Old} when Header :: t(), New :: boolean(), Old :: boolean().
set_has_user_flags(H0 = #argo_header{has_user_flags = Old}, New) when is_boolean(New) ->
    UserFlags =
        case New of
            true ->
                <<>>;
            false ->
                undefined
        end,
    H1 = H0#argo_header{has_user_flags = New, user_flags = UserFlags},
    {H1, Old =:= true}.

-spec user_flags(t()) -> undefined | bitstring().
user_flags(#argo_header{has_user_flags = true, user_flags = UserFlags}) ->
    case UserFlags of
        undefined ->
            <<>>;
        _ when is_bitstring(UserFlags) ->
            UserFlags
    end;
user_flags(#argo_header{has_user_flags = false}) ->
    undefined.

-spec set_user_flags(Header, New) -> {Header, Old} when
    Header :: t(), New :: undefined | bitstring(), Old :: undefined | bitstring().
set_user_flags(H0 = #argo_header{}, New = undefined) ->
    Old = user_flags(H0),
    H1 = H0#argo_header{has_user_flags = false, user_flags = New},
    {H1, Old};
set_user_flags(H0 = #argo_header{}, New) when is_bitstring(New) ->
    Old = user_flags(H0),
    H1 = H0#argo_header{has_user_flags = true, user_flags = New},
    {H1, Old}.

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
format_error_description(_Key, not_enough_data) ->
    "not enough data";
format_error_description(_Key, Value) ->
    Value.
