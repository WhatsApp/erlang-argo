%%% % @format
-module(argo_block_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_value.hrl").
-include("argo_wire_type.hrl").

%% API
-export([
    new/2,
    new/4
]).

%% Types
-type t() :: #argo_block_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(BlockWireType, ScalarValue) -> BlockValue when
    BlockWireType :: argo_block_wire_type:t(), ScalarValue :: argo_scalar_value:t(), BlockValue :: t().
new(#argo_block_wire_type{'of' = Of, key = Key, dedupe = Dedupe}, Value = #argo_scalar_value{}) ->
    new(Of, Key, Dedupe, Value).

-spec new(Of, Key, Dedupe, ScalarValue) -> BlockValue when
    Of :: argo_scalar_wire_type:t(),
    Key :: unicode:unicode_binary(),
    Dedupe :: boolean(),
    ScalarValue :: argo_scalar_value:t(),
    BlockValue :: t().
new(Of = #argo_scalar_wire_type{}, Key, Dedupe, Value = #argo_scalar_value{}) when
    is_binary(Key) andalso is_boolean(Dedupe)
->
    #argo_block_value{'of' = Of, key = Key, dedupe = Dedupe, value = Value}.
