%%% % @format
-module(argo_error_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_index_map.hrl").
-include("argo_value.hrl").

%% API
-export([
    new/4
]).

%% Types
-type t() :: #argo_error_value{}.

-export_type([
    t/0
]).

%% Macros
-define(is_option_none(X), ((X) =:= none)).
-define(is_option_some(X), (is_tuple((X)) andalso tuple_size((X)) =:= 2 andalso element(1, (X)) =:= some)).
-define(is_option_list(X), (?is_option_none(X) orelse (?is_option_some(X) andalso is_list(element(2, (X)))))).
-define(is_option_record(X, T), (?is_option_none(X) orelse (?is_option_some(X) andalso is_record(element(2, (X)), T)))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Message, none | {some, Location}, none | {some, Path}, none | {some, Extensions}) -> ErrorValue when
    Message :: argo_desc_value:desc_string(),
    Location :: [argo_location_value:t()],
    Path :: argo_path_value:t(),
    Extensions :: argo_desc_value:desc_object(),
    ErrorValue :: t().
new(Message, Location, Path, Extensions) when
    is_binary(Message) andalso ?is_option_list(Location) andalso ?is_option_record(Path, argo_path_value) andalso
        ?is_option_record(Extensions, argo_index_map)
->
    #argo_error_value{message = Message, location = Location, path = Path, extensions = Extensions}.
