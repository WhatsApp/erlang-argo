%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(argo_proper).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-11-10", modified => "2025-08-14"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% PropEr Helpers API
-export([
    pick/1,
    pick/2,
    pick/3,
    pick/4,
    present_result/4,
    quickcheck/2,
    sample/1,
    sample/2,
    sample/3,
    sample/4,
    sample/5,
    sample/6,
    sampleshrink/1,
    sampleshrink/2,
    sampleshrink/3,
    sampleshrink/4,
    sampleshrink/5
]).
%% Clean Execute API
-export([
    clean_execute/2,
    clean_execute/4,
    clean_execute_init/3,
    clean_sample_execute/5,
    clean_sampleshrink_execute/4
]).

%% Types
-type instance() :: dynamic().
-type print_function() :: fun((instance()) -> term()).
-type raw_seed() :: non_neg_integer() | seed().
-type size() :: non_neg_integer().
-type seed() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-export_type([
    instance/0,
    print_function/0,
    raw_seed/0,
    size/0,
    seed/0
]).

%% Macros
-define(DEFAULT_TIMEOUT, 5000).
-define(UNKNOWN, "unknown_property").
-define(is_size(X), (is_integer(X) andalso (X) >= 0)).
-define(is_seed(X),
    (is_tuple(X) andalso tuple_size(X) =:= 3 andalso ?is_size(element(1, (X))) andalso ?is_size(element(2, (X))) andalso
        ?is_size(element(3, (X))))
).
-define(is_raw_seed(X), (?is_size(X) orelse ?is_seed(X))).
-define(is_timeout(X), (?is_size(X) orelse (X) =:= infinity)).

%%%=============================================================================
%%% PropEr Helpers API functions
%%%=============================================================================

-spec pick(RawType) -> {ok, Instance} | error when RawType :: proper_types:raw_type(), Instance :: instance().
pick(RawType) ->
    pick(RawType, 10).

-spec pick(RawType, Size) -> {ok, Instance} | error when
    RawType :: proper_types:raw_type(), Size :: size(), Instance :: instance().
pick(RawType, Size) when ?is_size(Size) ->
    pick(RawType, Size, os:timestamp()).

-spec pick(RawType, Size, RawSeed) -> {ok, Instance} | error when
    RawType :: proper_types:raw_type(), Size :: size(), RawSeed :: raw_seed(), Instance :: instance().
pick(RawType, Size, RawSeed) when ?is_size(Size) andalso ?is_raw_seed(RawSeed) ->
    pick(RawType, Size, RawSeed, ?DEFAULT_TIMEOUT).

-spec pick(RawType, Size, RawSeed, Timeout) -> {ok, Instance} | error when
    RawType :: proper_types:raw_type(),
    Size :: size(),
    RawSeed :: raw_seed(),
    Timeout :: timeout(),
    Instance :: instance().
pick(RawType, Size, RawSeed, Timeout) when
    ?is_size(Size) andalso ?is_raw_seed(RawSeed) andalso ?is_size(Timeout) andalso ?is_timeout(Timeout)
->
    clean_execute(proper_gen, pick, [RawType, Size, cook_seed(RawSeed)], Timeout).

-spec present_result(module(), list(), tuple(), proplists:proplist()) -> boolean().
present_result(Module, Cmds, Triple, Config0) ->
    Config = [{property_test_tool, proper}] ++ Config0,
    ct_property_test:present_result(Module, Cmds, Triple, Config).

-spec quickcheck(OuterTest, Options) -> true | {fail, Reason} when
    OuterTest :: proper:outer_test(),
    Options :: dynamic(),
    Reason :: dynamic().
quickcheck(OuterTest, Options0) ->
    {Store, Options1} =
        case lists:keytake(store, 1, Options0) of
            {value, StoreTuple = {store, _}, Opts1} ->
                {StoreTuple, Opts1};
            false ->
                {false, Options0}
        end,
    Options = [long_result, {to_file, user}] ++ Options1,
    Result =
        try proper:quickcheck(OuterTest, Options) of
            true -> true;
            {error, Reason} -> #{failure => Reason};
            [CounterExample] -> #{counterexample => CounterExample};
            CounterExamples -> #{counterexamples => CounterExamples}
        catch
            Class:Reason:Stacktrace -> #{error => {Class, Reason, Stacktrace}}
        end,
    case Result of
        true ->
            true;
        _ ->
            case Store of
                {store, StoreFilename} ->
                    {ok, IoDevice} = file:open(StoreFilename, [write, {encoding, utf8}]),
                    io:format(IoDevice, "~p.~n", [Result]),
                    _ = file:close(IoDevice),
                    ok;
                false ->
                    ok
            end,
            case Result of
                #{error := {C, R, S}} ->
                    erlang:raise(C, R, S);
                Other ->
                    {fail, {property_name(OuterTest), Other}}
            end
    end.

-spec sample(RawType) -> ok | error when RawType :: proper_types:raw_type().
sample(RawType) ->
    sample(RawType, 10, 20).

-spec sample(RawType, PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(), PrintFunction :: print_function().
sample(RawType, PrintFunction) when is_function(PrintFunction, 1) ->
    sample(RawType, 10, 20, os:timestamp(), PrintFunction).

-spec sample(RawType, StartSize, EndSize) -> ok | error when
    RawType :: proper_types:raw_type(), StartSize :: size(), EndSize :: size().
sample(RawType, StartSize, EndSize) when ?is_size(StartSize) andalso ?is_size(EndSize) andalso StartSize =< EndSize ->
    sample(RawType, StartSize, EndSize, os:timestamp()).

-spec sample(RawType, StartSize, EndSize, RawSeed | PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(),
    StartSize :: size(),
    EndSize :: size(),
    RawSeed :: raw_seed(),
    PrintFunction :: print_function().
sample(RawType, StartSize, EndSize, RawSeed) when
    ?is_size(StartSize) andalso ?is_size(EndSize) andalso StartSize =< EndSize andalso ?is_raw_seed(RawSeed)
->
    sample(RawType, StartSize, EndSize, RawSeed, ?DEFAULT_TIMEOUT);
sample(RawType, StartSize, EndSize, PrintFunction) when
    ?is_size(StartSize) andalso ?is_size(EndSize) andalso StartSize =< EndSize andalso is_function(PrintFunction, 1)
->
    sample(RawType, StartSize, EndSize, os:timestamp(), ?DEFAULT_TIMEOUT, PrintFunction).

-spec sample(RawType, StartSize, EndSize, RawSeed, Timeout | PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(),
    StartSize :: size(),
    EndSize :: size(),
    RawSeed :: raw_seed(),
    Timeout :: timeout(),
    PrintFunction :: print_function().
sample(RawType, StartSize, EndSize, RawSeed, Timeout) when
    ?is_size(StartSize) andalso ?is_size(EndSize) andalso StartSize =< EndSize andalso ?is_raw_seed(RawSeed) andalso
        ?is_timeout(Timeout)
->
    sample(RawType, StartSize, EndSize, RawSeed, Timeout, fun sample_print/1);
sample(RawType, StartSize, EndSize, RawSeed, PrintFunction) when
    ?is_size(StartSize) andalso ?is_size(EndSize) andalso StartSize =< EndSize andalso ?is_raw_seed(RawSeed) andalso
        is_function(PrintFunction, 1)
->
    sample(RawType, StartSize, EndSize, RawSeed, ?DEFAULT_TIMEOUT, PrintFunction).

-spec sample(RawType, StartSize, EndSize, RawSeed, Timeout, PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(),
    StartSize :: size(),
    EndSize :: size(),
    RawSeed :: raw_seed(),
    Timeout :: timeout(),
    PrintFunction :: print_function().
sample(RawType, StartSize, EndSize, RawSeed, Timeout, PrintFunction) when
    ?is_size(StartSize) andalso ?is_size(EndSize) andalso StartSize =< EndSize andalso ?is_raw_seed(RawSeed) andalso
        ?is_timeout(Timeout) andalso is_function(PrintFunction, 1)
->
    clean_execute(
        ?MODULE,
        clean_sample_execute,
        [proper_types:cook_outer(RawType), StartSize, EndSize, cook_seed(RawSeed), PrintFunction],
        Timeout
    ).

-spec sampleshrink(RawType) -> ok | error when RawType :: proper_types:raw_type().
sampleshrink(RawType) ->
    sampleshrink(RawType, 10).

-spec sampleshrink(RawType, Size | PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(), Size :: size(), PrintFunction :: print_function().
sampleshrink(RawType, Size) when ?is_size(Size) ->
    sampleshrink(RawType, Size, os:timestamp());
sampleshrink(RawType, PrintFunction) when is_function(PrintFunction, 1) ->
    sampleshrink(RawType, 10, os:timestamp(), ?DEFAULT_TIMEOUT, PrintFunction).

-spec sampleshrink(RawType, Size, RawSeed | PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(), Size :: size(), RawSeed :: raw_seed(), PrintFunction :: print_function().
sampleshrink(RawType, Size, RawSeed) when ?is_size(Size) andalso ?is_raw_seed(RawSeed) ->
    sampleshrink(RawType, Size, RawSeed, ?DEFAULT_TIMEOUT);
sampleshrink(RawType, Size, PrintFunction) when ?is_size(Size) andalso is_function(PrintFunction, 1) ->
    sampleshrink(RawType, Size, os:timestamp(), ?DEFAULT_TIMEOUT, PrintFunction).

-spec sampleshrink(RawType, Size, RawSeed, Timeout | PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(),
    Size :: size(),
    RawSeed :: raw_seed(),
    Timeout :: timeout(),
    PrintFunction :: print_function().
sampleshrink(RawType, Size, RawSeed, Timeout) when
    ?is_size(Size) andalso ?is_raw_seed(RawSeed) andalso ?is_timeout(Timeout)
->
    sampleshrink(RawType, Size, RawSeed, Timeout, fun sample_print/1);
sampleshrink(RawType, Size, RawSeed, PrintFunction) when
    ?is_size(Size) andalso ?is_raw_seed(RawSeed) andalso is_function(PrintFunction, 1)
->
    sampleshrink(RawType, Size, RawSeed, ?DEFAULT_TIMEOUT, PrintFunction).

-spec sampleshrink(RawType, Size, RawSeed, Timeout, PrintFunction) -> ok | error when
    RawType :: proper_types:raw_type(),
    Size :: size(),
    RawSeed :: raw_seed(),
    Timeout :: timeout(),
    PrintFunction :: print_function().
sampleshrink(RawType, Size, RawSeed, Timeout, PrintFunction) when
    ?is_size(Size) andalso ?is_raw_seed(RawSeed) andalso
        ?is_timeout(Timeout) andalso is_function(PrintFunction, 1)
->
    clean_execute(
        ?MODULE,
        clean_sampleshrink_execute,
        [proper_types:cook_outer(RawType), Size, cook_seed(RawSeed), PrintFunction],
        Timeout
    ).

%%%=============================================================================
%%% Clean Execute API functions
%%%=============================================================================

-spec clean_execute(Function, Timeout) -> dynamic() when
    Function :: function(),
    Timeout :: timeout().
clean_execute(Function, Timeout) when is_function(Function, 0) andalso ?is_timeout(Timeout) ->
    clean_execute(erlang, apply, [Function, []], Timeout).

-spec clean_execute(Module, FunctionName, Arguments, Timeout) -> dynamic() when
    Module :: module(),
    FunctionName :: atom(),
    Arguments :: [dynamic()],
    Timeout :: timeout().
clean_execute(Module, FunctionName, Arguments, Timeout) when
    is_atom(Module) andalso is_atom(FunctionName) andalso is_list(Arguments) andalso ?is_timeout(Timeout)
->
    {Pid, Mon} = spawn_monitor(?MODULE, clean_execute_init, [Module, FunctionName, Arguments]),
    receive
        {'DOWN', Mon, process, Pid, normal} ->
            ok;
        {'DOWN', Mon, process, Pid, {return, Return}} ->
            Return;
        {'DOWN', Mon, process, Pid, Reason} ->
            {error, Reason}
    after Timeout ->
        true = erlang:exit(Pid, kill),
        receive
            {'DOWN', Mon, process, Pid, normal} ->
                ok;
            {'DOWN', Mon, process, Pid, {return, Return}} ->
                Return;
            {'DOWN', Mon, process, Pid, killed} ->
                {error, timeout};
            {'DOWN', Mon, process, Pid, Reason} ->
                {error, Reason}
        end
    end.

%% @private
-spec clean_execute_init(Module, FunctionName, Arguments) -> no_return() when
    Module :: module(),
    FunctionName :: atom(),
    Arguments :: [dynamic()].
clean_execute_init(Module, FunctionName, Arguments) when
    is_atom(Module) andalso is_atom(FunctionName) andalso is_list(Arguments)
->
    Return = erlang:apply(Module, FunctionName, Arguments),
    exit({return, Return}).

%% @private
-spec clean_sample_execute(Type, Size, EndSize, Seed, PrintFunction) -> ok | error when
    Type :: proper_types:type(), Size :: size(), EndSize :: size(), Seed :: seed(), PrintFunction :: print_function().
clean_sample_execute(Type, Size, EndSize, Seed, PrintFunction) when
    ?is_size(Size) andalso ?is_size(EndSize) andalso Size =< EndSize andalso ?is_seed(Seed) andalso
        is_function(PrintFunction, 1)
->
    case proper_gen:pick(Type, Size, Seed) of
        {ok, Instance} ->
            _ = PrintFunction(Instance),
            clean_sample_execute(Type, Size + 1, EndSize, Seed, PrintFunction);
        error ->
            error
    end;
clean_sample_execute(_Type, Size, EndSize, Seed, PrintFunction) when
    ?is_size(Size) andalso ?is_size(EndSize) andalso Size > EndSize andalso ?is_seed(Seed) andalso
        is_function(PrintFunction, 1)
->
    ok.

%% @private
-spec clean_sampleshrink_execute(Type, Size, Seed, PrintFunction) -> ok | error when
    Type :: proper_types:type(), Size :: size(), Seed :: seed(), PrintFunction :: print_function().
clean_sampleshrink_execute(Type, Size, Seed, PrintFunction) ->
    _ = proper:global_state_init_size_seed(Size, Seed),
    case proper_gen:safe_generate(Type) of
        {ok, ImmInstance} ->
            ok = shrink_and_print(ImmInstance, Type, PrintFunction, maps:new()),
            _ = proper:global_state_erase(),
            ok;
        {error, Reason} ->
            _ = proper:report_error(Reason, fun io:format/2),
            _ = proper:global_state_erase(),
            error
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec cook_seed(RawSeed) -> Seed when RawSeed :: raw_seed(), Seed :: seed().
cook_seed(Seed) when ?is_seed(Seed) ->
    Seed;
cook_seed(RawSeed) when ?is_size(RawSeed) ->
    {RawSeed, RawSeed, RawSeed}.

%% @private
-spec extract_name_from_fun(fun()) -> string().
extract_name_from_fun(Fun) ->
    FInfo = erlang:fun_info(Fun),
    case proplists:get_value(name, FInfo) of
        undefined ->
            ?UNKNOWN;
        FullName ->
            L = erlang:atom_to_list(FullName),
            case string:split(L, "/") of
                [L] -> L;
                [[$- | EncfunName], _] -> EncfunName;
                [EncfunName, _] -> EncfunName
            end
    end.

%% @private
-spec property_name(dynamic()) -> string().
property_name({forall, _Generator, PropFun}) ->
    extract_name_from_fun(PropFun);
property_name({setup, _SetupFun, Prop}) ->
    property_name(Prop);
property_name({exists, _Generator, PropFun, _Not}) ->
    extract_name_from_fun(PropFun);
property_name(_) ->
    ?UNKNOWN.

%% @private
-spec sample_print(term()) -> ok.
sample_print(X) ->
    _ = io:format("~tp~n", [X]),
    ok.

%% @private
-spec shrink_and_print(ImmInstance, Type, PrintFunction, Seen) -> ok when
    ImmInstance :: dynamic(),
    Type :: proper_types:type(),
    PrintFunction :: print_function(),
    Seen :: #{instance() => []}.
shrink_and_print(ImmInstance, Type, PrintFunction, Seen1) ->
    _ = PrintFunction(proper_gen:clean_instance(ImmInstance)),
    Seen2 = Seen1#{ImmInstance => []},
    shrink_and_print(ImmInstance, Type, PrintFunction, Seen2, init).

%% @private
-spec shrink_and_print(ImmInstance, Type, PrintFunction, Seen, State) -> ok when
    ImmInstance :: dynamic(),
    Type :: proper_types:type(),
    PrintFunction :: print_function(),
    Seen :: #{instance() => []},
    State :: proper_shrink:state().
shrink_and_print(ImmInstance, Type, PrintFunction, Seen1, State1) ->
    case proper_shrink:shrink(ImmInstance, Type, State1) of
        {[], done} ->
            % no more shrinkers
            ok;
        {[], State2} when State1 =/= State2 ->
            % try next shrinker
            shrink_and_print(ImmInstance, Type, PrintFunction, Seen1, State2);
        {[Shrunk | _], _State2} when is_map_key(Shrunk, Seen1) ->
            % avoid infinite loop
            ok;
        {[Shrunk | _], _State2} ->
            _ = PrintFunction(proper_gen:clean_instance(Shrunk)),
            Seen2 = Seen1#{Shrunk => []},
            shrink_and_print(Shrunk, Type, PrintFunction, Seen2, init)
    end.
