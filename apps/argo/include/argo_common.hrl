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
%% @oncall whatsapp_clr
-ifndef(ARGO_COMMON_HRL).
-define(ARGO_COMMON_HRL, 1).

-define(is_i64(X), (is_integer((X)) andalso (X) >= -16#8000000000000000 andalso (X) =< 16#7FFFFFFFFFFFFFFF)).
-define(is_u32(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FFFFFFFF)).
-define(is_u64(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FFFFFFFFFFFFFFFF)).
-define(is_usize(X), ?is_u64(X)).

-endif.
