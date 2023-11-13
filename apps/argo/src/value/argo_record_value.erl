%%% % @format
-module(argo_record_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_value.hrl").

%% API
-export([
    new/0,
    insert/2,
    present_fields_count/1,
    to_record_wire_type/1
]).

%% Types
-type t() :: #argo_record_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> RecordValue when RecordValue :: t().
new() ->
    #argo_record_value{fields = argo_index_map:new()}.

-spec insert(RecordValue, FieldValue) -> RecordValue when RecordValue :: t(), FieldValue :: argo_field_value:t().
insert(RecordValue0 = #argo_record_value{fields = Fields0}, FieldValue = #argo_field_value{}) ->
    Name = argo_field_value:name(FieldValue),
    Fields1 = argo_index_map:put(Name, FieldValue, Fields0),
    RecordValue1 = RecordValue0#argo_record_value{fields = Fields1},
    RecordValue1.

-spec present_fields_count(RecordValue) -> non_neg_integer() when RecordValue :: t().
present_fields_count(#argo_record_value{fields = Fields}) ->
    argo_index_map:foldl(
        fun(_Index, _Key, FieldValue, Count) ->
            case FieldValue of
                #argo_field_value{inner = {optional, none}} ->
                    Count;
                #argo_field_value{inner = {optional, {some, _}}} ->
                    Count + 1;
                #argo_field_value{inner = {required, _}} ->
                    Count + 1
            end
        end,
        0,
        Fields
    ).

-spec to_record_wire_type(RecordValue) -> RecordWireType when
    RecordValue :: t(), RecordWireType :: argo_record_wire_type:t().
to_record_wire_type(#argo_record_value{fields = Fields}) ->
    argo_index_map:foldl(
        fun(_Index, _FieldName, FieldValue, RecordWireTypeAcc) ->
            FieldWireType = argo_field_value:to_field_wire_type(FieldValue),
            argo_record_wire_type:insert(RecordWireTypeAcc, FieldWireType)
        end,
        argo_record_wire_type:new(),
        Fields
    ).
