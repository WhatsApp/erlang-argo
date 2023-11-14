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
-module(argo_types).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Types
-type backreference() :: usize().
-type i8() :: -16#80..16#7F.
-type i16() :: -16#8000..16#7FFF.
-type i32() :: -16#80000000..16#7FFFFFFF.
-type i64() :: -16#8000000000000000..16#7FFFFFFFFFFFFFFF.
-type isize() :: i64().
-type label() :: i64().
-type length() :: usize().
-type name() :: unicode:unicode_binary().
-type u8() :: 16#00..16#FF.
-type u16() :: 16#0000..16#FFFF.
-type u32() :: 16#00000000..16#FFFFFFFF.
-type u64() :: 16#0000000000000000..16#FFFFFFFFFFFFFFFF.
-type usize() :: u64().
-type varint() :: i64().

-export_type([
    backreference/0,
    i8/0,
    i16/0,
    i32/0,
    i64/0,
    isize/0,
    label/0,
    length/0,
    name/0,
    u8/0,
    u16/0,
    u32/0,
    u64/0,
    usize/0,
    varint/0
]).
