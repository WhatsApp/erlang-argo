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
-module(argo_field_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/3,
    is_labeled/1,
    is_omittable/1
]).

%% Types
-type omittable() :: boolean().
-type t() :: #argo_field_wire_type{}.

-export_type([
    omittable/0,
    t/0
]).

%% Macros
-define(is_omittable(X), (is_boolean(X))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Name, Of, Omittable) -> FieldWireType when
    Name :: argo_types:name(), Of :: argo_wire_type:t(), Omittable :: omittable(), FieldWireType :: t().
new(Name, Of = #argo_wire_type{}, Omittable) when is_binary(Name) andalso ?is_omittable(Omittable) ->
    #argo_field_wire_type{name = Name, 'of' = Of, omittable = Omittable}.

-compile({inline, [is_labeled/1]}).
-spec is_labeled(FieldWireType) -> boolean() when FieldWireType :: t().
is_labeled(#argo_field_wire_type{'of' = Of}) ->
    argo_wire_type:is_labeled(Of).

-spec is_omittable(FieldWireType) -> boolean() when FieldWireType :: t().
is_omittable(#argo_field_wire_type{omittable = Omittable}) ->
    Omittable =:= true.
