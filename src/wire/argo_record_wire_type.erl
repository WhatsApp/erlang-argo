%%% % @format
-module(argo_record_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_index_map.hrl").
-include("argo_wire_type.hrl").

%% API
-export([
    new/0,
    new/1,
    insert/2
    % insert/4
]).

%% Types
-type t() :: #argo_record_wire_type{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> RecordWireType when RecordWireType :: t().
new() ->
    #argo_record_wire_type{fields = argo_index_map:new()}.

-spec new(Fields) -> RecordWireType when
    Fields :: argo_index_map:t(argo_types:name(), argo_field_wire_type:t()), RecordWireType :: t().
new(Fields = #argo_index_map{}) ->
    #argo_record_wire_type{fields = Fields}.

-spec insert(RecordWireType, FieldWireType) -> RecordWireType when
    RecordWireType :: t(), FieldWireType :: argo_field_wire_type:t().
insert(RecordWireType0 = #argo_record_wire_type{fields = Fields0}, FieldWireType = #argo_field_wire_type{name = Name}) ->
    Fields1 = argo_index_map:put(Name, FieldWireType, Fields0),
    RecordWireType1 = RecordWireType0#argo_record_wire_type{fields = Fields1},
    RecordWireType1.

% insert(RecordWireType = #argo_record_wire_type{}, Name, Of = #argo_wire_type{}, Omittable) when is_binary(Name) andalso is_boolean(Omittable) ->
%     FieldWireType = argo_field_wire_type:new(Name, Of, Omittable),
%     insert(RecordWireType, FieldWireType).
