%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_concurrency).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    try_acquire_session_start/0,
    release_session_start/0,
    try_acquire_guild_start/0,
    release_guild_start/0,
    session_start_count/0,
    guild_start_count/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SESSION_START_IDX, 1).
-define(GUILD_START_IDX, 2).

-type state() :: #{
    counters := counters:counters_ref()
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason};
        ignore -> {error, ignore}
    end.

-spec try_acquire_session_start() -> ok | {error, at_capacity}.
try_acquire_session_start() ->
    try_acquire(?SESSION_START_IDX, fun gateway_rollout_config:max_concurrent_session_starts/0).

-spec release_session_start() -> ok.
release_session_start() ->
    release_counter(?SESSION_START_IDX).

-spec try_acquire_guild_start() -> ok | {error, at_capacity}.
try_acquire_guild_start() ->
    try_acquire(?GUILD_START_IDX, fun gateway_rollout_config:max_concurrent_guild_starts/0).

-spec try_acquire(pos_integer(), fun(() -> pos_integer())) -> ok | {error, at_capacity}.
try_acquire(Idx, LimitFun) ->
    try
        gen_server:call(?MODULE, {acquire, Idx, LimitFun()}, infinity)
    catch
        exit:_ -> ok
    end.

-spec do_acquire(pos_integer(), integer(), counters:counters_ref()) ->
    ok | {error, at_capacity}.
do_acquire(Idx, Limit, Ref) ->
    Count = counters:get(Ref, Idx),
    case Count >= Limit of
        true ->
            {error, at_capacity};
        false ->
            counters:add(Ref, Idx, 1),
            ok
    end.

-spec release_guild_start() -> ok.
release_guild_start() ->
    release_counter(?GUILD_START_IDX).

-spec release_counter(pos_integer()) -> ok.
release_counter(Idx) ->
    try
        Ref = persistent_term:get(gateway_concurrency_counters),
        case counters:get(Ref, Idx) of
            Value when Value > 0 ->
                counters:sub(Ref, Idx, 1);
            _ ->
                counters:put(Ref, Idx, 0)
        end,
        ok
    catch
        error:badarg -> ok
    end.

-spec session_start_count() -> non_neg_integer().
session_start_count() ->
    Ref = persistent_term:get(gateway_concurrency_counters),
    max(0, counters:get(Ref, ?SESSION_START_IDX)).

-spec guild_start_count() -> non_neg_integer().
guild_start_count() ->
    Ref = persistent_term:get(gateway_concurrency_counters),
    max(0, counters:get(Ref, ?GUILD_START_IDX)).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 50),
    Ref = counters:new(2, [write_concurrency]),
    persistent_term:put(gateway_concurrency_counters, Ref),
    {ok, #{counters => Ref}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({acquire, Idx, Limit}, _From, #{counters := Ref} = State) when
    is_integer(Idx), is_integer(Limit)
->
    {reply, do_acquire(Idx, Limit, Ref), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

concurrency_acquire_release_test() ->
    {ok, _Pid} = start_link(),
    ?assertEqual(0, session_start_count()),
    ?assertEqual(ok, try_acquire_session_start()),
    ?assertEqual(1, session_start_count()),
    ?assertEqual(ok, release_session_start()),
    ?assertEqual(0, session_start_count()),
    gen_server:stop(?MODULE).

release_without_acquire_does_not_underflow_test() ->
    {ok, _Pid} = start_link(),
    ?assertEqual(ok, release_session_start()),
    ?assertEqual(ok, release_guild_start()),
    ?assertEqual(0, session_start_count()),
    ?assertEqual(0, guild_start_count()),
    gen_server:stop(?MODULE).

acquire_is_bounded_at_limit_test() ->
    {ok, _Pid} = start_link(),
    Ref = persistent_term:get(gateway_concurrency_counters),
    ?assertEqual(ok, do_acquire(?SESSION_START_IDX, 1, Ref)),
    ?assertEqual(1, session_start_count()),
    ?assertEqual({error, at_capacity}, do_acquire(?SESSION_START_IDX, 1, Ref)),
    ?assertEqual(1, session_start_count()),
    gen_server:stop(?MODULE).

-endif.
