%%% % @format
-module(argo_scalar_wire_type).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("argo/include/argo_common.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% New API
-export([
    boolean/0,
    bytes/0,
    fixed/1,
    float64/0,
    string/0,
    varint/0
]).

%% Instance API
-export([
    deduplicate_by_default/1,
    is_boolean/1,
    is_bytes/1,
    is_fixed/1,
    is_fixed_length/2,
    is_float64/1,
    is_labeled/1,
    is_string/1,
    is_varint/1,
    supports_deduplication/1,
    to_string/1
]).

%% Types
-type inner() :: boolean | bytes | argo_fixed_wire_type:t() | float64 | string | varint.
-type t() :: #argo_scalar_wire_type{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec boolean() -> ScalarWireType when ScalarWireType :: t().
boolean() -> #argo_scalar_wire_type{inner = boolean}.

-spec bytes() -> ScalarWireType when ScalarWireType :: t().
bytes() -> #argo_scalar_wire_type{inner = bytes}.

-spec fixed(LengthOrFixedWireType) -> ScalarWireType when
    LengthOrFixedWireType :: argo_types:length() | argo_fixed_wire_type:t(), ScalarWireType :: t().
fixed(Length) when ?is_usize(Length) -> fixed(argo_fixed_wire_type:new(Length));
fixed(FixedWireType = #argo_fixed_wire_type{}) -> #argo_scalar_wire_type{inner = FixedWireType}.

-spec float64() -> ScalarWireType when ScalarWireType :: t().
float64() -> #argo_scalar_wire_type{inner = float64}.

-spec string() -> ScalarWireType when ScalarWireType :: t().
string() -> #argo_scalar_wire_type{inner = string}.

-spec varint() -> ScalarWireType when ScalarWireType :: t().
varint() -> #argo_scalar_wire_type{inner = varint}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec deduplicate_by_default(ScalarWireType) -> boolean() when ScalarWireType :: t().
deduplicate_by_default(#argo_scalar_wire_type{inner = string}) -> true;
deduplicate_by_default(#argo_scalar_wire_type{inner = bytes}) -> true;
deduplicate_by_default(#argo_scalar_wire_type{}) -> false.

-spec is_boolean(ScalarWireType) -> boolean() when ScalarWireType :: t().
is_boolean(#argo_scalar_wire_type{inner = Inner}) -> Inner =:= boolean.

-spec is_bytes(ScalarWireType) -> boolean() when ScalarWireType :: t().
is_bytes(#argo_scalar_wire_type{inner = Inner}) -> Inner =:= bytes.

-spec is_fixed(ScalarWireType) -> boolean() when ScalarWireType :: t().
is_fixed(#argo_scalar_wire_type{inner = #argo_fixed_wire_type{}}) -> true;
is_fixed(#argo_scalar_wire_type{}) -> false.

-spec is_fixed_length(ScalarWireType, Length) -> boolean() when ScalarWireType :: t(), Length :: argo_types:length().
is_fixed_length(#argo_scalar_wire_type{inner = #argo_fixed_wire_type{length = Length}}, Length) -> true;
is_fixed_length(#argo_scalar_wire_type{}, _Length) -> false.

-spec is_float64(ScalarWireType) -> boolean() when ScalarWireType :: t().
is_float64(#argo_scalar_wire_type{inner = Inner}) -> Inner =:= float64.

-spec is_labeled(ScalarWireType) -> boolean() when ScalarWireType :: t().
is_labeled(#argo_scalar_wire_type{inner = string}) -> true;
is_labeled(#argo_scalar_wire_type{inner = boolean}) -> true;
is_labeled(#argo_scalar_wire_type{inner = bytes}) -> true;
is_labeled(#argo_scalar_wire_type{}) -> false.

-spec is_string(ScalarWireType) -> boolean() when ScalarWireType :: t().
is_string(#argo_scalar_wire_type{inner = Inner}) -> Inner =:= string.

-spec is_varint(ScalarWireType) -> boolean() when ScalarWireType :: t().
is_varint(#argo_scalar_wire_type{inner = Inner}) -> Inner =:= varint.

-spec supports_deduplication(ScalarWireType) -> boolean() when ScalarWireType :: t().
supports_deduplication(#argo_scalar_wire_type{inner = string}) -> true;
supports_deduplication(#argo_scalar_wire_type{inner = bytes}) -> true;
supports_deduplication(#argo_scalar_wire_type{}) -> false.

-spec to_string(ScalarWireType) -> iodata() when ScalarWireType :: t().
to_string(#argo_scalar_wire_type{inner = boolean}) ->
    "BOOLEAN";
to_string(#argo_scalar_wire_type{inner = bytes}) ->
    "BYTES";
to_string(#argo_scalar_wire_type{inner = #argo_fixed_wire_type{length = Length}}) ->
    io_lib:format("FIXED(~w)", [Length]);
to_string(#argo_scalar_wire_type{inner = float64}) ->
    "FLOAT64";
to_string(#argo_scalar_wire_type{inner = string}) ->
    "STRING";
to_string(#argo_scalar_wire_type{inner = varint}) ->
    "VARINT".
