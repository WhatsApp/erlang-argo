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
%% @oncall whatsapp_clr
-ifndef(PROPER_ARGO_TEST_HRL).
-define(PROPER_ARGO_TEST_HRL, 1).

-ifndef(PROPER_NO_TRANS).
-define(PROPER_NO_TRANS, true).
-endif.
-ifndef(PROPER_NO_IMPORT_PARSE).
-define(PROPER_NO_IMPORT_PARSE, true).
-endif.

-include_lib("proper/include/proper.hrl").

-ifdef(WHENFAIL).
-undef(WHENFAIL).
-endif.
% eqWAlizer gets angry about `fun(() -> boolean())` not being a subtype of `fun(() -> proper:test())`
-define(WHENFAIL(Action, Prop), proper:whenfail(?DELAY(Action), argo_types:dynamic_cast(?DELAY(Prop)))).

-ifndef(PROPER_NO_IMPORTS).

-import(proper_argo_test, [
    complex/1,
    complexity/0,
    minsized/1,
    mostly/2,
    mostly_small_list/1,
    mostly_small_size/0,
    option/1,
    option_unlikely/1,
    with_complexity/1
]).

-endif.

-endif.
