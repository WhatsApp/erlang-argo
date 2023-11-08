%%% % @format
-module(argo_nullable_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_wire_type.hrl").

%% API
-export([
    new/1
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
new(Of = #argo_wire_type{}) ->
    #argo_nullable_wire_type{'of' = Of}.
