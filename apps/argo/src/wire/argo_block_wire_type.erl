%%% % @format
-module(argo_block_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/3
]).

%% Types
-type t() :: #argo_block_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Of, Key, Dedupe) -> BlockWireType when
    Of :: argo_scalar_wire_type:t(), Key :: argo_types:name(), Dedupe :: boolean(), BlockWireType :: t().
new(Of = #argo_scalar_wire_type{}, Key, Dedupe) when is_binary(Key) andalso is_boolean(Dedupe) ->
    #argo_block_wire_type{'of' = Of, key = Key, dedupe = Dedupe}.
