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
-ifndef(ARGO_HEADER_HRL).
-define(ARGO_HEADER_HRL, 1).

-record(argo_header, {
    inline_everything = false :: boolean(),
    self_describing = false :: boolean(),
    out_of_band_field_errors = true :: boolean(),
    self_describing_errors = true :: boolean(),
    null_terminated_strings = false :: boolean(),
    no_deduplication = false :: boolean(),
    has_user_flags = false :: boolean(),
    user_flags = undefined :: undefined | bitstring()
}).

-endif.
