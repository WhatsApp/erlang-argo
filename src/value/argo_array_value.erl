%%% % @format
-module(argo_array_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_value.hrl").

%% API
-export([
    new/1
]).

%% Types
-type t() :: #argo_array_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Items) -> ArrayValue when Items :: [argo_value:t()], ArrayValue :: t().
new(Items) when is_list(Items) ->
    #argo_array_value{items = Items}.
