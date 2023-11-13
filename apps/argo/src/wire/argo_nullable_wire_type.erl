%%% % @format
-module(argo_nullable_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1,
    is_labeled/1
]).

%% Types
-type t() :: #argo_nullable_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Of) -> NullableWireType when Of :: argo_wire_type:t(), NullableWireType :: t().
new(#argo_wire_type{inner = NullableWireType = #argo_nullable_wire_type{}}) ->
    NullableWireType;
new(Of = #argo_wire_type{}) ->
    #argo_nullable_wire_type{'of' = Of}.

-compile({inline, [is_labeled/1]}).
-spec is_labeled(NullableWireType) -> boolean() when NullableWireType :: t().
is_labeled(#argo_nullable_wire_type{'of' = WireType}) ->
    argo_wire_type:is_labeled(WireType).
