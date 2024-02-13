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
%%% Created :  16 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    display/1,
    display/2,
    display_with_lines/1,
    display_with_lines/2,
    format/1,
    format_with_lines/1
]).

%% Macros
-define(is_record(X), (is_tuple((X)) andalso tuple_size((X)) > 0 andalso is_atom(element(1, (X))))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec display(dynamic()) -> ok.
display(Type) when ?is_record(Type) ->
    display(standard_io, Type).

-spec display(io:device(), dynamic()) -> ok.
display(IoDevice, Type) when not is_list(IoDevice) andalso ?is_record(Type) ->
    Module = element(1, Type),
    case Module of
        argo_value ->
            argo_value:display(IoDevice, Type);
        argo_wire_type ->
            argo_wire_type:display(IoDevice, Type);
        argo_wire_type_store ->
            argo_wire_type_store:display(IoDevice, Type);
        _ ->
            case erlang:atom_to_binary(Module, utf8) of
                <<"argo_graphql_", _/bytes>> ->
                    argo_graphql:display(IoDevice, Type)
            end
    end.

-spec display_with_lines(dynamic()) -> ok.
display_with_lines(Type) when ?is_record(Type) ->
    display_with_lines(standard_io, Type).

-spec display_with_lines(io:device(), dynamic()) -> ok.
display_with_lines(IoDevice, Type) when not is_list(IoDevice) andalso ?is_record(Type) ->
    Lines = format_with_lines(Type),
    Printer1 = argo_graphql_printer:new_io_device(IoDevice),
    Printer2 = argo_graphql_printer:write(Printer1, "~ts", [Lines]),
    case argo_graphql_printer:finalize(Printer2) of
        ok ->
            ok
    end.

-spec format(dynamic()) -> unicode:unicode_binary().
format(Type) when ?is_record(Type) ->
    Module = element(1, Type),
    case Module of
        argo_value ->
            argo_types:unicode_binary(argo_value:format(Type));
        argo_wire_type ->
            argo_types:unicode_binary(argo_wire_type:format(Type));
        argo_wire_type_store ->
            argo_types:unicode_binary(argo_wire_type_store:format(Type));
        _ ->
            case erlang:atom_to_binary(Module, utf8) of
                <<"argo_graphql_", _/bytes>> ->
                    argo_graphql:format(Type)
            end
    end.

-spec format_with_lines(dynamic()) -> unicode:unicode_binary().
format_with_lines(Type) when ?is_record(Type) ->
    argo_types:format_with_lines(format(Type)).
