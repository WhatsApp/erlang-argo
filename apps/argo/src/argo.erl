%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-16", modified => "2025-07-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    display/1,
    display/2,
    display/3,
    display_with_lines/1,
    display_with_lines/2,
    display_with_lines/3,
    format/1,
    format/2,
    format_with_lines/1,
    format_with_lines/2
]).
%% Errors API
-export([
    format_error/2
]).

%% Macros
-define(DEFAULT_IO_DEVICE, standard_io).
-define(DEFAULT_OPTIONS, #{}).
-define(is_record(X), (is_tuple((X)) andalso tuple_size((X)) > 0 andalso is_atom(element(1, (X))))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec display(Type) -> ok when Type :: dynamic().
display(Type) when ?is_record(Type) ->
    display(?DEFAULT_IO_DEVICE, Type, ?DEFAULT_OPTIONS).

-spec display(IoDevice | Type, Type | Options) -> ok when
    IoDevice :: io:device(), Type :: dynamic(), Options :: dynamic().
display(IoDevice, Type) when not is_list(IoDevice) andalso ?is_record(Type) ->
    display(IoDevice, Type, ?DEFAULT_OPTIONS);
display(Type, Options) when ?is_record(Type) andalso is_map(Options) ->
    display(?DEFAULT_IO_DEVICE, Type, Options).

-spec display(IoDevice, Type, Options) -> ok when IoDevice :: io:device(), Type :: dynamic(), Options :: dynamic().
display(IoDevice, Type, Options) when not is_list(IoDevice) andalso ?is_record(Type) andalso is_map(Options) ->
    case element(1, Type) of
        argo_value ->
            argo_value:display(IoDevice, Type, Options);
        argo_wire_type ->
            argo_wire_type:display(IoDevice, Type, Options);
        argo_wire_type_store ->
            argo_wire_type_store:display(IoDevice, Type, Options);
        Module ->
            case erlang:atom_to_binary(Module, utf8) of
                <<"argo_graphql_", _/bytes>> ->
                    argo_graphql:display(IoDevice, Type, Options);
                _ ->
                    error_with_info(badarg, [IoDevice, Type, Options], #{2 => display_not_supported})
            end
    end.

-spec display_with_lines(Type) -> ok when Type :: dynamic().
display_with_lines(Type) when ?is_record(Type) ->
    display_with_lines(?DEFAULT_IO_DEVICE, Type, ?DEFAULT_OPTIONS).

-spec display_with_lines(IoDevice | Type, Type | Options) -> ok when
    IoDevice :: io:device(), Type :: dynamic(), Options :: dynamic().
display_with_lines(IoDevice, Type) when not is_list(IoDevice) andalso ?is_record(Type) ->
    display_with_lines(IoDevice, Type, ?DEFAULT_OPTIONS);
display_with_lines(Type, Options) when ?is_record(Type) andalso is_map(Options) ->
    display_with_lines(?DEFAULT_IO_DEVICE, Type, Options).

-spec display_with_lines(IoDevice, Type, Options) -> ok when
    IoDevice :: io:device(), Type :: dynamic(), Options :: dynamic().
display_with_lines(IoDevice, Type, Options) when
    not is_list(IoDevice) andalso ?is_record(Type) andalso is_map(Options)
->
    Lines = format_with_lines(Type, Options),
    Printer1 = argo_graphql_printer:new_io_device(IoDevice, Options),
    Printer2 = argo_graphql_printer:write(Printer1, "~ts", [Lines]),
    case argo_graphql_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(Type) -> Output when Type :: dynamic(), Output :: unicode:unicode_binary().
format(Type) when ?is_record(Type) ->
    format(Type, ?DEFAULT_OPTIONS).

-spec format(Type, Options) -> Output when Type :: dynamic(), Options :: dynamic(), Output :: unicode:unicode_binary().
format(Type, Options) when ?is_record(Type) andalso is_map(Options) ->
    case element(1, Type) of
        argo_value ->
            argo_value:format(Type, Options);
        argo_wire_type ->
            argo_wire_type:format(Type, Options);
        argo_wire_type_store ->
            argo_wire_type_store:format(Type, Options);
        Module ->
            case erlang:atom_to_binary(Module, utf8) of
                <<"argo_graphql_", _/bytes>> ->
                    argo_graphql:format(Type, Options);
                _ ->
                    error_with_info(badarg, [Type, Options], #{1 => format_not_supported})
            end
    end.

-spec format_with_lines(dynamic()) -> unicode:unicode_binary().
format_with_lines(Type) when ?is_record(Type) ->
    format_with_lines(Type, ?DEFAULT_OPTIONS).

-spec format_with_lines(Type, Options) -> Output when
    Type :: dynamic(), Options :: dynamic(), Output :: unicode:unicode_binary().
format_with_lines(Type, Options) when ?is_record(Type) andalso is_map(Options) ->
    argo_types:format_with_lines(format(Type, Options)).

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
format_error_description(_Key, display_not_supported) ->
    "display/3 is not supported for this Type";
format_error_description(_Key, format_not_supported) ->
    "format/2 is not supported for this Type";
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
