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

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_header.hrl").

%% Codec API
-export([
    from_http_argo_mode/1,
    from_reader/1,
    from_u64/1,
    from_uint/1,
    to_http_argo_mode/1,
    to_u64/1,
    to_uint/1,
    to_writer/1
]).

%% API
-export([
    new/0,
    new/1,
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
-type options() :: #{
    inline_everything => boolean(),
    self_describing => boolean(),
    out_of_band_field_errors => boolean(),
    self_describing_errors => boolean(),
    null_terminated_strings => boolean(),
    no_deduplication => boolean(),
    user_flags => undefined | bitstring()
}.
-type t() :: #argo_header{}.

-export_type([
    options/0,
    t/0
]).

%% Macros
-define(LC(C),
    case C of
        $A -> $a;
        $B -> $b;
        $C -> $c;
        $D -> $d;
        $E -> $e;
        $F -> $f;
        $G -> $g;
        $H -> $h;
        $I -> $i;
        $J -> $j;
        $K -> $k;
        $L -> $l;
        $M -> $m;
        $N -> $n;
        $O -> $o;
        $P -> $p;
        $Q -> $q;
        $R -> $r;
        $S -> $s;
        $T -> $t;
        $U -> $u;
        $V -> $v;
        $W -> $w;
        $X -> $x;
        $Y -> $y;
        $Z -> $z;
        _ -> C
    end
).

%%%=============================================================================
%%% Codec API functions
%%%=============================================================================

-spec from_http_argo_mode(HttpArgoMode) -> Header when HttpArgoMode :: binary(), Header :: t().
from_http_argo_mode(HttpArgoMode) when is_binary(HttpArgoMode) ->
    try from_http_argo_mode(binary:split(HttpArgoMode, <<";">>, [global, trim_all]), from_uint(0)) of
        Header = #argo_header{} ->
            Header
    catch
        throw:Cause ->
            error_with_info(badarg, [HttpArgoMode], #{1 => Cause})
    end.

-spec from_reader(Reader) -> {Reader, Header} when Reader :: binary(), Header :: t().
from_reader(
    <<0:1, HasUserFlags1:1, NoDeduplication:1, NullTerminatedStrings:1, SelfDescribingErrors:1, OutOfBandFieldErrors:1,
        SelfDescribing:1, InlineEverything:1, Reader2/bits>>
) ->
    HasUserFlags2 = decode_bit(HasUserFlags1),
    {Reader3, UserFlags} =
        case HasUserFlags2 of
            true ->
                argo_varbit:read_varbit(Reader2);
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

-spec from_u64(U64) -> Header when U64 :: argo_types:u64(), Header :: t().
from_u64(U64) when ?is_u64(U64) ->
    from_uint(U64).

-spec from_uint(UInt) -> Header when UInt :: non_neg_integer(), Header :: t().
from_uint(UInt) when is_integer(UInt) andalso UInt >= 0 ->
    case from_reader(binary:encode_unsigned(UInt, big)) of
        {<<>>, Header} ->
            Header;
        {TrailingData, _BadHeader} ->
            error_with_info(badarg, [UInt], #{1 => {invalid_uint_trailing_data, TrailingData}})
    end.

-spec to_http_argo_mode(Header) -> HttpArgoMode when Header :: t(), HttpArgoMode :: binary().
to_http_argo_mode(H = #argo_header{}) ->
    erlang:iolist_to_binary(
        lists:join(<<";">>, [
            ArgoModeFlag
         || ArgoModeFlag <- [
                inline_everything(H) andalso <<"InlineEverything">>,
                self_describing(H) andalso <<"SelfDescribing">>,
                out_of_band_field_errors(H) andalso <<"OutOfBandFieldErrors">>,
                self_describing_errors(H) andalso <<"SelfDescribingErrors">>,
                null_terminated_strings(H) andalso <<"NullTerminatedStrings">>,
                no_deduplication(H) andalso <<"NoDeduplication">>,
                has_user_flags(H) andalso
                    <<"HasUserFlags",
                        (case user_flags(H) of
                            undefined -> <<>>;
                            UserFlags -> <<$:, (<<<<(UserFlag + $0):8>> || <<UserFlag:1>> <= UserFlags>>)/bytes>>
                        end)/bytes>>
            ],
            ArgoModeFlag =/= false
        ])
    ).

-spec to_u64(Header) -> U64 when Header :: t(), U64 :: argo_types:u64().
to_u64(Header = #argo_header{}) ->
    case to_writer(Header) of
        HeaderEncoded when bit_size(HeaderEncoded) =< 64 ->
            binary:decode_unsigned(HeaderEncoded, big);
        HeaderEncoded ->
            error_with_info(badarg, [Header], #{1 => {header_too_large_for_u64, HeaderEncoded}})
    end.

-spec to_uint(Header) -> UInt when Header :: t(), UInt :: non_neg_integer().
to_uint(Header = #argo_header{}) ->
    HeaderEncoded = to_writer(Header),
    binary:decode_unsigned(HeaderEncoded, big).

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
        (argo_varbit:write_varbit(user_flags(H)))/bits
    >>.

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> t().
new() ->
    #argo_header{}.

-spec new(options()) -> t().
new(Options) when is_map(Options) ->
    Config = [
        {inline_everything, fun set_inline_everything/2},
        {self_describing, fun set_self_describing/2},
        {out_of_band_field_errors, fun set_out_of_band_field_errors/2},
        {self_describing_errors, fun set_self_describing_errors/2},
        {null_terminated_strings, fun set_null_terminated_strings/2},
        {no_deduplication, fun set_no_deduplication/2},
        {user_flags, fun set_user_flags/2}
    ],
    new(Options, Config, new()).

%% @private
-spec new(Options, Config, Header) -> Header when
    Options :: options(), Config :: [{atom(), fun((Header, dynamic()) -> {Header, dynamic()})}], Header :: t().
new(_Options, [], Header) ->
    Header;
new(Options, [{Key, SetFun} | Config], Header1) when is_map(Options) ->
    case maps:find(Key, Options) of
        {ok, Value} ->
            {Header2, _} = SetFun(Header1, Value),
            new(Options, Config, Header2);
        error ->
            new(Options, Config, Header1)
    end.

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
-compile({inline, [error_with_info/3]}).
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
format_error_description(_Key, {header_too_large_for_u64, HeaderEncoded}) ->
    io_lib:format("encoded header is larger than 64-bits: ~0tp", [HeaderEncoded]);
format_error_description(_Key, {invalid_uint_trailing_data, TrailingData}) ->
    io_lib:format("decoded header for unsigned integer has non-empty trailing data: ~0tp", [TrailingData]);
format_error_description(_Key, not_enough_data) ->
    "not enough data";
format_error_description(_Key, {unknown_http_argo_mode, ArgoModeFlag}) ->
    io_lib:format("unknown HTTP header Argo-Mode flag: ~0tp", [ArgoModeFlag]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_bit(0 | 1) -> boolean().
decode_bit(0) -> false;
decode_bit(1) -> true.

%% @private
-spec encode_bit(boolean()) -> bitstring().
encode_bit(false) -> <<0:1>>;
encode_bit(true) -> <<1:1>>.

%% @private
-spec from_http_argo_mode(Flags, Header) -> Header when Flags :: [Flag], Flag :: binary(), Header :: t().
from_http_argo_mode([], Header1) ->
    Header1;
from_http_argo_mode([Flag | Flags], Header1) ->
    case from_http_argo_mode_flag_name(Flag, <<>>) of
        {<<"inlineeverything">>, <<>>} ->
            {Header2, _} = set_inline_everything(Header1, true),
            from_http_argo_mode(Flags, Header2);
        {<<"selfdescribing">>, <<>>} ->
            {Header2, _} = set_self_describing(Header1, true),
            from_http_argo_mode(Flags, Header2);
        {<<"outofbandfielderrors">>, <<>>} ->
            {Header2, _} = set_out_of_band_field_errors(Header1, true),
            from_http_argo_mode(Flags, Header2);
        {<<"selfdescribingerrors">>, <<>>} ->
            {Header2, _} = set_self_describing_errors(Header1, true),
            from_http_argo_mode(Flags, Header2);
        {<<"nullterminatedstrings">>, <<>>} ->
            {Header2, _} = set_null_terminated_strings(Header1, true),
            from_http_argo_mode(Flags, Header2);
        {<<"nodeduplication">>, <<>>} ->
            {Header2, _} = set_no_deduplication(Header1, true),
            from_http_argo_mode(Flags, Header2);
        {<<"hasuserflags">>, <<>>} ->
            from_http_argo_mode(Flags, Header1);
        {<<"hasuserflags">>, UserFlagsString} ->
            case from_http_argo_mode_user_flags(UserFlagsString, <<>>) of
                {ok, UserFlagsBits} ->
                    {Header2, _} = set_user_flags(Header1, UserFlagsBits),
                    from_http_argo_mode(Flags, Header2);
                error ->
                    throw({unknown_http_argo_mode, Flag})
            end;
        {_, _} ->
            throw({unknown_http_argo_mode, Flag})
    end.

%% @private
-spec from_http_argo_mode_flag_name(Rest, Name) -> {Name, Rest} when Rest :: binary(), Name :: binary().
from_http_argo_mode_flag_name(Rest, Name) when byte_size(Name) >= 21 ->
    % Largest flag is 21-bytes: "NullTerminatedStrings"
    % Anything else is either invalid or is the bitmap following "HasUserFlags"
    {Name, Rest};
from_http_argo_mode_flag_name(<<$:, Rest/bytes>>, Name) ->
    % If ":" is encountered, return with the name as-is.
    {Name, Rest};
from_http_argo_mode_flag_name(<<C:8, Rest/bytes>>, Name) when (C >= $A andalso C =< $Z) ->
    from_http_argo_mode_flag_name(Rest, <<Name/bytes, (?LC(C)):8>>);
from_http_argo_mode_flag_name(<<C:8, Rest/bytes>>, Name) when (C >= $a andalso C =< $z) ->
    from_http_argo_mode_flag_name(Rest, <<Name/bytes, C:8>>);
from_http_argo_mode_flag_name(Rest, Name) ->
    % Anything outside of [A-z] is invalid, return as-is.
    {Name, Rest}.

%% @private
-spec from_http_argo_mode_user_flags(UserFlagsString, UserFlagsBits) -> {ok, UserFlagsBits} | error when
    UserFlagsString :: binary(), UserFlagsBits :: bitstring().
from_http_argo_mode_user_flags(<<>>, UserFlagsBits) ->
    {ok, UserFlagsBits};
from_http_argo_mode_user_flags(<<C:8, UserFlagsString/bytes>>, UserFlagsBits) when C =:= $0 orelse C =:= $1 ->
    from_http_argo_mode_user_flags(UserFlagsString, <<UserFlagsBits/bits, (C - $0):1>>);
from_http_argo_mode_user_flags(_UserFlagsString, _UserFlagsBits) ->
    error.
