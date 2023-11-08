%%% % @format
-module(argo_location_value).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include("argo_common.hrl").
-include("argo_value.hrl").

%% API
-export([
    new/2
]).

%% Types
-type t() :: #argo_location_value{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Line, Column) -> LocationValue when
    Line :: argo_types:i64(), Column :: argo_types:i64(), LocationValue :: t().
new(Line, Column) when ?is_i64(Line) andalso ?is_i64(Column) ->
    #argo_location_value{line = Line, column = Column}.
