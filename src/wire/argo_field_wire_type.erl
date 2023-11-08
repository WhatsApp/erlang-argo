%%% % @format
-module(argo_field_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_wire_type.hrl").

%% API
-export([
    new/3
]).

%% Types
-type t() :: #argo_field_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Name, Of, Omittable) -> FieldWireType when
    Name :: argo_types:name(), Of :: argo_wire_type:t(), Omittable :: boolean(), FieldWireType :: t().
new(Name, Of = #argo_wire_type{}, Omittable) when is_binary(Name) andalso is_boolean(Omittable) ->
    #argo_field_wire_type{name = Name, 'of' = Of, omittable = Omittable}.
