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
%%% Created :  17 Apr 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(argo_debug_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Behaviour
-callback display(IoDevice, Type, Options) -> ok when
    IoDevice :: io:device(), Type :: type(), Options :: options().
-callback format(Type) -> Output when
    Type :: type(), Output :: output().
-callback format(Type, Options) -> Output when
    Type :: type(), Options :: options(), Output :: output().

-optional_callbacks([
    format/1
]).

%% Types
-type options() :: dynamic().
-type output() :: unicode:unicode_binary().
-type type() :: dynamic().

-export_type([
    options/0,
    output/0,
    type/0
]).
