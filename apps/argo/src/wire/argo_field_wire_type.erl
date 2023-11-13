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

-compile({inline, [is_labeled/1]}).
-spec is_labeled(FieldWireType) -> boolean() when FieldWireType :: t().
is_labeled(#argo_field_wire_type{'of' = Of}) ->
    argo_wire_type:is_labeled(Of).

-spec is_omittable(FieldWireType) -> boolean() when FieldWireType :: t().
is_omittable(#argo_field_wire_type{omittable = Omittable}) ->
    Omittable =:= true.
