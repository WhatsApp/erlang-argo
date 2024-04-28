%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(proper_argo).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("argo_test/include/proper_argo_test.hrl").
-include_lib("argo/include/argo_header.hrl").
-include_lib("argo/include/argo_value.hrl").
-include_lib("argo/include/argo_wire_type.hrl").

%% Primitive API
-export([
    bytes/0,
    fixed/1,
    maybe_json_safe_float/0,
    maybe_root_wire_type/0,
    maybe_root_wire_type/2,
    name/0,
    name/1,
    name_continue/0,
    name_start/0,
    string_utf8/0,
    usize/0,
    varint/0
]).

%% Header API
-export([
    header/0
]).

%% Value API
-export([
    array_value/0,
    array_value/1,
    block_value/0,
    block_value/1,
    desc_value/0,
    error_value/0,
    extensions_value/0,
    field_value/0,
    field_value/1,
    nullable_value/0,
    nullable_value/1,
    path_value/0,
    record_value/0,
    record_value/1,
    scalar_value/0,
    scalar_value/1,
    value/0,
    value/1,
    value_json_safe/0,
    value_json_safe/1,
    value_make_json_safe/1
]).

%% Wire Type API
-export([
    array_wire_type/0,
    block_wire_type/0,
    desc_wire_type/0,
    error_wire_type/0,
    extensions_wire_type/0,
    field_wire_type/0,
    field_wire_type/1,
    field_wire_type/2,
    nullable_wire_type/0,
    path_wire_type/0,
    record_wire_type/0,
    scalar_wire_type/0,
    wire_type/0
]).

%% Macros
-define(JSON_SAFE, 'proper_argo_json_safe').
-define(ROOT_SAFE, 'proper_argo_root_safe').
-define(ROOT_WIRE_TYPE, 'proper_argo_root_wire_type').

%%%=============================================================================
%%% Primitive API functions
%%%=============================================================================

-spec bytes() -> proper_types:type().
bytes() ->
    proper_types:binary().

-spec fixed(Length :: non_neg_integer()) -> proper_types:type().
fixed(Length) when is_integer(Length) andalso Length >= 0 ->
    proper_types:binary(Length).

-spec maybe_json_safe_float() -> proper_types:type().
maybe_json_safe_float() ->
    ?LET(Float, float(), begin
        case parameter(?JSON_SAFE, false) of
            false ->
                Float;
            true ->
                shrink_float_for_jsone(Float)
        end
    end).

-spec maybe_root_wire_type() -> proper_types:type().
maybe_root_wire_type() ->
    ?LAZY(parameter(?ROOT_WIRE_TYPE, none)).

-spec maybe_root_wire_type(WireType, RawType) -> proper_types:type() when
    RawType :: proper_types:raw_type(), WireType :: argo_wire_type:t().
maybe_root_wire_type(WireType = #argo_wire_type{}, RawType) ->
    ?LET(
        OptionRootWireType,
        maybe_root_wire_type(),
        case OptionRootWireType of
            none ->
                with_parameter(?ROOT_WIRE_TYPE, {some, WireType}, RawType);
            {some, _RootWireType} ->
                RawType
        end
    ).

-spec name() -> proper_types:type().
name() ->
    ?SIZED(Length, name(Length)).

-spec name(Length :: non_neg_integer()) -> proper_types:type().
name(0) ->
    exactly(<<>>);
name(1) ->
    ?LET(Character, name_start(), <<Character>>);
name(Length) when is_integer(Length) andalso Length > 1 ->
    ?LET(
        {NameStart, NameContinue},
        {name_start(), vector(Length - 1, name_continue())},
        unicode:characters_to_binary([NameStart | NameContinue])
    ).

-compile({inline, [name_continue/0]}).
-spec name_continue() -> proper_types:type().
name_continue() ->
    oneof(lists:seq($0, $9) ++ lists:seq($A, $Z) ++ [$_] ++ lists:seq($a, $z)).

-compile({inline, [name_start/0]}).
-spec name_start() -> proper_types:type().
name_start() ->
    oneof(lists:seq($A, $Z) ++ [$_] ++ lists:seq($a, $z)).

-spec string_utf8() -> proper_types:type().
string_utf8() ->
    proper_unicode:utf8().

-spec usize() -> proper_types:type().
usize() ->
    integer(16#0000000000000000, 16#FFFFFFFFFFFFFFFF).

-spec varint() -> proper_types:type().
varint() ->
    integer(-16#8000000000000000, 16#7FFFFFFFFFFFFFFF).

%%%=============================================================================
%%% Header API functions
%%%=============================================================================

-spec header() -> proper_types:type().
header() ->
    ?LET(
        {
            InlineEverything,
            SelfDescribing,
            OutOfBandFieldErrors,
            SelfDescribingErrors,
            NullTerminatedStrings,
            NoDeduplication,
            HasUserFlags,
            UserFlags
        },
        {
            boolean(),
            boolean(),
            exactly(true),
            boolean(),
            boolean(),
            boolean(),
            exactly(false),
            exactly(undefined)
        },
        #argo_header{
            inline_everything = InlineEverything,
            self_describing = SelfDescribing,
            out_of_band_field_errors = OutOfBandFieldErrors,
            self_describing_errors = SelfDescribingErrors,
            null_terminated_strings = NullTerminatedStrings,
            no_deduplication = NoDeduplication,
            has_user_flags = HasUserFlags,
            user_flags = UserFlags
        }
    ).

%%%=============================================================================
%%% Value API functions
%%%=============================================================================

-spec array_value() -> proper_types:type().
array_value() ->
    ?LET(ArrayWireType, array_wire_type(), array_value(ArrayWireType)).

-spec array_value(ArrayWireType :: argo_array_wire_type:t()) -> proper_types:type().
array_value(ArrayWireType = #argo_array_wire_type{}) ->
    with_complexity(
        ?SIZED(Size, begin
            Complexity = complexity(),
            Length = Size div (Complexity * 2),
            array_value(ArrayWireType, Length)
        end)
    ).

%% @private
-spec array_value(ArrayWireType :: argo_array_wire_type:t(), Length :: non_neg_integer()) -> proper_types:type().
array_value(ArrayWireType = #argo_array_wire_type{}, Length) when is_integer(Length) andalso Length >= 0 ->
    ?LET(
        Items,
        vector(Length, value(ArrayWireType#argo_array_wire_type.'of')),
        argo_array_value:new(ArrayWireType, Items)
    ).

-spec block_value() -> proper_types:type().
block_value() ->
    ?LET(BlockWireType, block_wire_type(), block_value(BlockWireType)).

-spec block_value(BlockWireType :: argo_block_wire_type:t()) -> proper_types:type().
block_value(BlockWireType = #argo_block_wire_type{}) ->
    ?LET(
        ScalarValue,
        scalar_value(BlockWireType#argo_block_wire_type.'of'),
        argo_block_value:new(BlockWireType, ScalarValue)
    ).

-spec desc_value() -> proper_types:type().
desc_value() ->
    oneof([
        argo_desc_value:null(),
        ?LET(V, boolean(), argo_desc_value:boolean(V)),
        ?LET(V, desc_value_object(), argo_desc_value:object(V)),
        ?LET(V, desc_value_list(), argo_desc_value:list(V)),
        ?LET(V, string_utf8(), argo_desc_value:string(V)),
        ?LET(V, bytes(), argo_desc_value:bytes(V)),
        ?LET(V, varint(), argo_desc_value:int(V)),
        ?LET(V, maybe_json_safe_float(), argo_desc_value:float(V))
    ]).

%% @private
-spec desc_value_list() -> proper_types:type().
desc_value_list() ->
    with_complexity(
        ?SIZED(Size, begin
            Complexity = complexity(),
            Length = Size div (Complexity * 2),
            desc_value_list(Length)
        end)
    ).

%% @private
desc_value_list(Length) ->
    vector(Length, ?LAZY(desc_value())).

%% @private
-spec desc_value_object() -> proper_types:type().
desc_value_object() ->
    with_complexity(
        ?SIZED(Size, begin
            Complexity = complexity(),
            Length = Size div (Complexity * 2),
            desc_value_object(Length)
        end)
    ).

%% @private
desc_value_object(Length) when is_integer(Length) andalso Length >= 0 ->
    ?LET(
        Keys,
        ?SUCHTHAT(
            Keys,
            vector(Length, string_utf8()),
            begin
                SetOfKeys = sets:from_list(Keys, [{version, 2}]),
                sets:size(SetOfKeys) =:= Length
            end
        ),
        ?LET(
            KeyValueList,
            [{Key, ?LAZY(desc_value())} || Key <- Keys],
            argo_index_map:from_list(KeyValueList)
        )
    ).

-spec error_value() -> proper_types:type().
error_value() ->
    ?LET(
        {Message, Location, Path, Extensions},
        {string_utf8(), option(error_value_location()), option(path_value()), option(extensions_value())},
        argo_error_value:new(Message, Location, Path, Extensions)
    ).

-spec extensions_value() -> proper_types:type().
extensions_value() ->
    ?LET(
        Extensions,
        desc_value_object(),
        argo_extensions_value:new(Extensions)
    ).

%% @private
-spec error_value_location() -> proper_types:type().
error_value_location() ->
    ?SIZED(Length, error_value_location(Length)).

%% @private
-spec error_value_location(Length :: non_neg_integer()) -> proper_types:type().
error_value_location(Length) when is_integer(Length) andalso Length >= 0 ->
    vector(Length, error_value_location_value()).

%% @private
-spec error_value_location_value() -> proper_types:type().
error_value_location_value() ->
    ?LET({Line, Column}, {varint(), varint()}, argo_location_value:new(Line, Column)).

-spec field_value() -> proper_types:type().
field_value() ->
    ?LET(FieldWireType, field_wire_type(), field_value(FieldWireType)).

-spec field_value(FieldWireType :: argo_field_wire_type:t()) -> proper_types:type().
field_value(FieldWireType = #argo_field_wire_type{'of' = Of, omittable = true}) ->
    ?LET(
        OptionValue,
        option(value(Of)),
        case OptionValue of
            none ->
                argo_field_value:optional(FieldWireType, OptionValue);
            {some, _} ->
                argo_field_value:optional(FieldWireType, OptionValue)
        end
    );
field_value(FieldWireType = #argo_field_wire_type{'of' = Of}) ->
    ?LET(Value, value(Of), argo_field_value:required(FieldWireType, Value)).

-spec nullable_value() -> proper_types:type().
nullable_value() ->
    ?LET(NullableWireType, nullable_wire_type(), nullable_value(NullableWireType)).

-spec nullable_value(NullableWireType :: argo_nullable_wire_type:t()) -> proper_types:type().
nullable_value(NullableWireType = #argo_nullable_wire_type{}) ->
    oneof([
        argo_nullable_value:null(NullableWireType),
        ?LET(
            Value,
            value(NullableWireType#argo_nullable_wire_type.'of'),
            case Value of
                #argo_value{inner = #argo_desc_value{inner = null}} ->
                    argo_nullable_value:null(NullableWireType);
                #argo_value{} ->
                    argo_nullable_value:non_null(NullableWireType, Value)
            end
        )
    ]).

-spec path_value() -> proper_types:type().
path_value() ->
    ?LET(
        OptionRootWireType,
        maybe_root_wire_type(),
        case OptionRootWireType of
            none ->
                exactly(argo_path_value:new());
            {some, RootWireType} ->
                PossibleSegmentLists = argo_wire_type:fold_path_values(
                    fun(PathValue = #argo_path_value{}, Acc) ->
                        [argo_path_value:to_list(PathValue) | Acc]
                    end,
                    [[]],
                    RootWireType
                ),
                ?LET(SegmentList, oneof(PossibleSegmentLists), path_value(SegmentList))
        end
    ).

%% @private
-spec path_value(SegmentListTemplate) -> proper_types:type() when SegmentListTemplate :: argo_path_value:segment_list().
path_value(SegmentListTemplate) when is_list(SegmentListTemplate) ->
    SegmentListGen = ?LAZY([
        case Segment of
            0 ->
                usize();
            _ when is_binary(Segment) ->
                exactly(Segment)
        end
     || Segment <- SegmentListTemplate
    ]),
    ?LET(SegmentList, SegmentListGen, argo_path_value:from_list(SegmentList)).

% -spec path_value() -> proper_types:type().
% path_value() ->
%     ?SIZED(Length, path_value(Length)).

% %% @private
% -spec path_value(Length :: non_neg_integer()) -> proper_types:type().
% path_value(Length) when is_integer(Length) andalso Length >= 0 ->
%     ?LET(SegmentsList, vector(Length, path_value_segment()), argo_path_value:from_list(SegmentsList)).

% %% @private
% -spec path_value_segment() -> proper_types:type().
% path_value_segment() ->
%     oneof([
%         name(),
%         usize()
%     ]).

-spec record_value() -> proper_types:type().
record_value() ->
    ?LET(RecordWireType, record_wire_type(), record_value(RecordWireType)).

-spec record_value(RecordWireType :: argo_record_wire_type:t()) -> proper_types:type().
record_value(RecordWireType = #argo_record_wire_type{}) ->
    ?LET(
        Fields,
        [
            {FieldName, field_value(FieldWireType)}
         || {FieldName, FieldWireType} <- argo_index_map:to_list(RecordWireType#argo_record_wire_type.fields)
        ],
        #argo_record_value{fields = argo_index_map:from_list(Fields)}
    ).

-spec scalar_value() -> proper_types:type().
scalar_value() ->
    ?LET(ScalarWireType, scalar_wire_type(), scalar_value(ScalarWireType)).

-spec scalar_value(ScalarWireType :: argo_scalar_wire_type:t()) -> proper_types:type().
scalar_value(#argo_scalar_wire_type{inner = boolean}) ->
    ?LET(Value, boolean(), argo_scalar_value:boolean(Value));
scalar_value(#argo_scalar_wire_type{inner = bytes}) ->
    ?LET(Value, bytes(), argo_scalar_value:bytes(Value));
scalar_value(#argo_scalar_wire_type{inner = desc}) ->
    ?LET(Value, desc_value(), argo_scalar_value:desc(Value));
scalar_value(#argo_scalar_wire_type{inner = #argo_fixed_wire_type{length = Length}}) ->
    ?LET(Value, fixed(Length), argo_scalar_value:fixed(Value));
scalar_value(#argo_scalar_wire_type{inner = float64}) ->
    ?LET(Value, maybe_json_safe_float(), argo_scalar_value:float64(Value));
scalar_value(#argo_scalar_wire_type{inner = string}) ->
    ?LET(Value, string_utf8(), argo_scalar_value:string(Value));
scalar_value(#argo_scalar_wire_type{inner = varint}) ->
    ?LET(Value, varint(), argo_scalar_value:varint(Value)).

-spec value() -> proper_types:type().
value() ->
    ?LET(WireType, wire_type(), value(WireType)).

-spec value(WireType :: argo_wire_type:t()) -> proper_types:type().
value(#argo_wire_type{inner = ArrayWireType = #argo_array_wire_type{}}) ->
    ?LAZY(?LET(Value, array_value(ArrayWireType), argo_value:array(Value)));
value(#argo_wire_type{inner = BlockWireType = #argo_block_wire_type{}}) ->
    ?LAZY(?LET(Value, block_value(BlockWireType), argo_value:block(Value)));
value(#argo_wire_type{inner = _DescWireType = #argo_desc_wire_type{}}) ->
    ?LAZY(?LET(Value, desc_value(), argo_value:desc(Value)));
value(#argo_wire_type{inner = _ErrorWireType = #argo_error_wire_type{}}) ->
    ?LAZY(?LET(Value, error_value(), argo_value:error(Value)));
value(#argo_wire_type{inner = _ExtensionsWireType = #argo_extensions_wire_type{}}) ->
    ?LAZY(?LET(Value, extensions_value(), argo_value:extensions(Value)));
value(#argo_wire_type{inner = NullableWireType = #argo_nullable_wire_type{}}) ->
    ?LAZY(?LET(Value, nullable_value(NullableWireType), argo_value:nullable(Value)));
value(#argo_wire_type{inner = _PathWireType = #argo_path_wire_type{}}) ->
    ?LAZY(?LET(Value, path_value(), argo_value:path(Value)));
value(WireType = #argo_wire_type{inner = RecordWireType = #argo_record_wire_type{}}) ->
    ?LAZY(maybe_root_wire_type(WireType, ?LET(Value, record_value(RecordWireType), argo_value:record(Value))));
value(#argo_wire_type{inner = ScalarWireType = #argo_scalar_wire_type{}}) ->
    ?LAZY(?LET(Value, scalar_value(ScalarWireType), argo_value:scalar(Value))).

-spec value_json_safe() -> proper_types:type().
value_json_safe() ->
    ?LET(WireType, wire_type(), value_json_safe(WireType)).

-spec value_json_safe(WireType :: argo_wire_type:t()) -> proper_types:type().
value_json_safe(WireType = #argo_wire_type{}) ->
    with_parameter(?JSON_SAFE, true, value(WireType)).

-spec value_make_json_safe(Value) -> Value when Value :: argo_value:t().
value_make_json_safe(Value1 = #argo_value{}) ->
    {Value2, ok} = argo_value:xform(Value1, ok, fun value_make_json_safe/2),
    Value2.

%% @private
-spec value_make_json_safe(dynamic(), ok) -> dynamic().
value_make_json_safe(ScalarValue1 = #argo_scalar_value{inner = {float64, Float64}}, Acc = ok) ->
    ShrunkFloat64 = shrink_float_for_jsone(Float64),
    ScalarValue2 = ScalarValue1#argo_scalar_value{inner = {float64, ShrunkFloat64}},
    {cont, ScalarValue2, Acc};
value_make_json_safe(DescValue1 = #argo_desc_value{inner = {float, Float}}, Acc = ok) ->
    ShrunkFloat = shrink_float_for_jsone(Float),
    DescValue2 = DescValue1#argo_desc_value{inner = {float, ShrunkFloat}},
    {cont, DescValue2, Acc};
value_make_json_safe(_, ok) ->
    cont.

% jsone doesn't handle accurate roundtrip for high precision floats
%% @private
-spec shrink_float_for_jsone(float()) -> float().
shrink_float_for_jsone(Float) when is_float(Float) ->
    list_to_float(float_to_list(Float, [{scientific, 5}])).

%%%=============================================================================
%%% Wire Type API functions
%%%=============================================================================

-spec array_wire_type() -> proper_types:type().
array_wire_type() ->
    ?LET(Of, ?LAZY(wire_type()), argo_array_wire_type:new(Of)).

-spec block_wire_type() -> proper_types:type().
block_wire_type() ->
    ?LET({Of, Key}, {scalar_wire_type(), name()}, begin
        case argo_scalar_wire_type:supports_deduplication(Of) of
            false ->
                argo_block_wire_type:new(Of, Key, false);
            true ->
                ?LET(Dedupe, boolean(), argo_block_wire_type:new(Of, Key, Dedupe))
        end
    end).

-spec desc_wire_type() -> proper_types:type().
desc_wire_type() ->
    exactly(#argo_desc_wire_type{}).

-spec error_wire_type() -> proper_types:type().
error_wire_type() ->
    exactly(#argo_error_wire_type{}).

-spec extensions_wire_type() -> proper_types:type().
extensions_wire_type() ->
    exactly(#argo_extensions_wire_type{}).

-spec field_wire_type() -> proper_types:type().
field_wire_type() ->
    ?LET({Name, Of, Omittable}, {name(), ?LAZY(wire_type()), boolean()}, argo_field_wire_type:new(Name, Of, Omittable)).

-spec field_wire_type(Name :: argo_types:name()) -> proper_types:type().
field_wire_type(Name) when is_binary(Name) ->
    ?LET({Of, Omittable}, {?LAZY(wire_type()), boolean()}, argo_field_wire_type:new(Name, Of, Omittable)).

-spec field_wire_type(Name :: argo_types:name(), Of :: argo_wire_type:t()) -> proper_types:type().
field_wire_type(Name, Of = #argo_wire_type{}) when is_binary(Name) ->
    ?LET(Omittable, boolean(), argo_field_wire_type:new(Name, Of, Omittable)).

-spec nullable_wire_type() -> proper_types:type().
nullable_wire_type() ->
    ?LET(Of, ?LAZY(wire_type()), argo_nullable_wire_type:new(unwrap_nullable_wire_type(Of))).

-spec path_wire_type() -> proper_types:type().
path_wire_type() ->
    exactly(#argo_path_wire_type{}).

-spec record_wire_type() -> proper_types:type().
record_wire_type() ->
    with_complexity(
        ?SIZED(Size, begin
            Complexity = complexity(),
            Length = max(Size div (Complexity * 2), 1),
            record_wire_type(Length)
        end)
    ).

%% @private
-spec record_wire_type(Length :: non_neg_integer()) -> proper_types:type().
record_wire_type(Length) when is_integer(Length) andalso Length >= 0 ->
    ?LET(
        FieldNames,
        ?SUCHTHAT(
            FieldNames,
            vector(Length, name()),
            begin
                SetOfFieldNames = sets:from_list(FieldNames, [{version, 2}]),
                sets:size(SetOfFieldNames) =:= Length
            end
        ),
        ?LET(
            FieldWireTypes,
            [field_wire_type(FieldName) || FieldName <- FieldNames],
            lists:foldl(
                fun(FieldWireType, RecordWireType) ->
                    argo_record_wire_type:insert(RecordWireType, FieldWireType)
                end,
                argo_record_wire_type:new(),
                FieldWireTypes
            )
        )
    ).

-spec scalar_wire_type() -> proper_types:type().
scalar_wire_type() ->
    oneof([
        exactly(argo_scalar_wire_type:boolean()),
        exactly(argo_scalar_wire_type:varint()),
        exactly(argo_scalar_wire_type:float64()),
        exactly(argo_scalar_wire_type:string()),
        exactly(argo_scalar_wire_type:bytes()),
        ?LET(Length, non_neg_integer(), argo_scalar_wire_type:fixed(Length)),
        exactly(argo_scalar_wire_type:desc())
    ]).

-spec wire_type() -> proper_types:type().
wire_type() ->
    InnerGen =
        ?LAZY(
            case parameter(?ROOT_SAFE, false) of
                false ->
                    with_parameter(?ROOT_SAFE, true, record_wire_type());
                true ->
                    oneof([
                        path_wire_type(),
                        scalar_wire_type(),
                        block_wire_type(),
                        desc_wire_type(),
                        error_wire_type(),
                        extensions_wire_type(),
                        nullable_wire_type(),
                        array_wire_type(),
                        record_wire_type()
                    ])
            end
        ),
    ?LET(
        InnerWireType,
        InnerGen,
        case InnerWireType of
            #argo_array_wire_type{} -> argo_wire_type:array(InnerWireType);
            #argo_block_wire_type{} -> argo_wire_type:block(InnerWireType);
            #argo_desc_wire_type{} -> argo_wire_type:desc();
            #argo_error_wire_type{} -> argo_wire_type:error();
            #argo_extensions_wire_type{} -> argo_wire_type:extensions();
            #argo_nullable_wire_type{} -> argo_wire_type:nullable(InnerWireType);
            #argo_path_wire_type{} -> argo_wire_type:path();
            #argo_record_wire_type{} -> argo_wire_type:record(InnerWireType);
            #argo_scalar_wire_type{} -> argo_wire_type:scalar(InnerWireType)
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec unwrap_nullable_wire_type(WireType) -> WireType when WireType :: argo_wire_type:t().
unwrap_nullable_wire_type(#argo_wire_type{inner = #argo_nullable_wire_type{'of' = Of}}) ->
    unwrap_nullable_wire_type(Of);
unwrap_nullable_wire_type(WireType = #argo_wire_type{}) ->
    WireType.
