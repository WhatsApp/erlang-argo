%%% % @format
-module(argo_fixed_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/1
]).

%% Types
-type t() :: #argo_fixed_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Length) -> FixedWireType when Length :: argo_types:length(), FixedWireType :: t().
new(Length) when ?is_usize(Length) ->
    #argo_fixed_wire_type{length = Length}.
