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
-ifndef(ARGO_LABEL_HRL).
-define(ARGO_LABEL_HRL, 1).

-define(ARGO_LABEL_MARKER_TRUE, 1).
-define(ARGO_LABEL_MARKER_FALSE, 0).
-define(ARGO_LABEL_MARKER_NON_NULL, 0).
-define(ARGO_LABEL_MARKER_NULL, -1).
-define(ARGO_LABEL_MARKER_ABSENT, -2).
-define(ARGO_LABEL_MARKER_ERROR, -3).
% ?ARGO_LABEL_MARKER_ERROR
-define(ARGO_LABEL_MARKER_LOWEST_RESERVED_VALUE, -3).
% ?ARGO_LABEL_MARKER_LOWEST_RESERVED_VALUE - 1
-define(ARGO_LABEL_MARKER_OFFSET_FACTOR, -4).

% ?ARGO_LABEL_MARKER_NULL
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_NULL, -1).
% ?ARGO_LABEL_MARKER_FALSE
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_FALSE, 0).
% ?ARGO_LABEL_MARKER_TRUE
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_TRUE, 1).
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_OBJECT, 2).
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_LIST, 3).
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_STRING, 4).
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_BYTES, 5).
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_INT, 6).
-define(ARGO_LABEL_SELF_DESCRIBING_MARKER_FLOAT, 7).

-define(ARGO_LABEL_SELF_DESCRIBING_BLOCK_BOOLEAN, <<"Boolean">>).
-define(ARGO_LABEL_SELF_DESCRIBING_BLOCK_BYTES, <<"Bytes">>).
-define(ARGO_LABEL_SELF_DESCRIBING_BLOCK_FLOAT64, <<"Float">>).
-define(ARGO_LABEL_SELF_DESCRIBING_BLOCK_STRING, <<"String">>).
-define(ARGO_LABEL_SELF_DESCRIBING_BLOCK_VARINT, <<"Int">>).

% ?ARGO_LABEL_MARKER_FALSE
-define(ARGO_LABEL_WIRE_TYPE_MARKER_FALSE, 0).
% ?ARGO_LABEL_MARKER_TRUE
-define(ARGO_LABEL_WIRE_TYPE_MARKER_TRUE, 1).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_STRING, -1).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_BOOLEAN, -2).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_VARINT, -3).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_FLOAT64, -4).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_BYTES, -5).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_FIXED, -6).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_BLOCK, -7).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_NULLABLE, -8).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_ARRAY, -9).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_RECORD, -10).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_DESC, -11).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_ERROR, -12).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_PATH, -13).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_UNION, -14).
-define(ARGO_LABEL_WIRE_TYPE_MARKER_EXTENSIONS, -15).

-endif.
