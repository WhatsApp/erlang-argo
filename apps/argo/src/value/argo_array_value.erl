%%% % @format
-module(argo_array_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% API
-export([
    new/2,
    to_array_wire_type/1
]).

%% Types
-type t() :: #argo_array_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(ArrayWireType, Items) -> ArrayValue when
    ArrayWireType :: argo_array_wire_type:t(), Items :: [argo_value:t()], ArrayValue :: t().
new(ArrayWireType = #argo_array_wire_type{}, Items) when is_list(Items) ->
    #argo_array_value{wire_type = ArrayWireType, items = Items}.

-spec to_array_wire_type(ArrayValue) -> ArrayWireType when ArrayValue :: t(), ArrayWireType :: argo_array_wire_type:t().
to_array_wire_type(#argo_array_value{wire_type = ArrayWireType = #argo_array_wire_type{}}) ->
    ArrayWireType.
