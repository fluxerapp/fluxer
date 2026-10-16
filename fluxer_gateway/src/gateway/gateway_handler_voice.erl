%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_voice).
-typing([eqwalizer]).

-export([
    handle_voice_state_update/3,
    process_queued_voice_updates/1,
    ensure_voice_queue_timer/1,
    enqueue_voice_update/2,
    cleanup_session/1
]).

-export_type([state/0, ws_result/0]).

-define(VOICE_UPDATE_RATE_LIMIT, 2).
-define(VOICE_RATE_LIMIT_WINDOW, 1000).
-define(VOICE_QUEUE_TABLE, voice_update_queue).
-define(VOICE_RATE_LIMIT_TABLE, voice_update_rate_limit).
-define(VOICE_QUEUE_PROCESS_INTERVAL, 500).
-define(MAX_VOICE_QUEUE_LENGTH, 64).

-type state() :: gateway_handler:state().
-type ws_result() :: gateway_handler:ws_result().

-spec handle_voice_state_update(pid(), map(), state()) -> ws_result().
handle_voice_state_update(Pid, Data, State) ->
    case should_queue_voice_update(Pid) of
        false ->
            log_voice_update(Pid, Data, direct),
            process_voice_update(Pid, Data, State);
        true ->
            log_voice_update(Pid, Data, queued),
            queue_voice_update(Pid, Data),
            {ok, ensure_voice_queue_timer(State)}
    end.

-spec log_voice_update(pid(), map(), direct | queued) -> ok.
log_voice_update(Pid, Data, Mode) ->
    GuildId = maps:get(<<"guild_id">>, Data, undefined),
    ChannelId = maps:get(<<"channel_id">>, Data, undefined),
    ConnectionId = maps:get(<<"connection_id">>, Data, undefined),
    case Mode of
        direct ->
            logger:info(
                "gateway_voice_state_update_dispatch: session_pid=~p guild_id=~p "
                "channel_id=~p connection_id=~p mode=direct",
                [Pid, GuildId, ChannelId, ConnectionId]
            );
        queued ->
            logger:warning(
                "gateway_voice_state_update_dispatch: session_pid=~p guild_id=~p "
                "channel_id=~p connection_id=~p mode=queued",
                [Pid, GuildId, ChannelId, ConnectionId]
            )
    end,
    ok.

-spec process_voice_update(pid(), map(), state()) -> ws_result().
process_voice_update(SessionPid, Data, State) ->
    try gen_server:call(SessionPid, {voice_state_update, Data}, 5000) of
        _ -> {ok, State}
    catch
        Class:Reason ->
            log_voice_update_call_failure(Class, Reason, SessionPid, Data),
            {ok, State}
    end.

-spec log_voice_update_call_failure(atom(), term(), pid(), map()) -> ok.
log_voice_update_call_failure(exit, {timeout, _}, SessionPid, Data) ->
    logger:warning("Gateway voice state update call timeout", #{
        session_pid => SessionPid, data => Data
    });
log_voice_update_call_failure(exit, {noproc, _}, SessionPid, Data) ->
    logger:warning("Gateway voice state update call noproc", #{
        session_pid => SessionPid, data => Data
    });
log_voice_update_call_failure(exit, Reason, SessionPid, _Data) ->
    logger:warning("Gateway voice state update call exit", #{
        session_pid => SessionPid, reason => Reason
    });
log_voice_update_call_failure(error, Reason, SessionPid, _Data) ->
    logger:warning("Gateway voice state update call error", #{
        session_pid => SessionPid, reason => Reason
    });
log_voice_update_call_failure(throw, Reason, SessionPid, _Data) ->
    logger:warning("Gateway voice state update call throw", #{
        session_pid => SessionPid, reason => Reason
    }).

-spec cleanup_session(pid() | undefined) -> ok.
cleanup_session(SessionPid) when is_pid(SessionPid) ->
    cleanup_session_table(?VOICE_QUEUE_TABLE, SessionPid),
    cleanup_session_table(?VOICE_RATE_LIMIT_TABLE, SessionPid),
    ok;
cleanup_session(_) ->
    ok.

-spec cleanup_session_table(atom(), pid()) -> ok.
cleanup_session_table(TableName, SessionPid) ->
    case ets:whereis(TableName) of
        undefined ->
            ok;
        _ ->
            ets:delete(TableName, SessionPid),
            ok
    end.

-spec should_queue_voice_update(pid()) -> boolean().
should_queue_voice_update(SessionPid) ->
    case gateway_handler_rate_limit:rate_limits_disabled() of
        true -> false;
        false -> should_queue_voice_update_limited(SessionPid)
    end.

-spec should_queue_voice_update_limited(pid()) -> boolean().
should_queue_voice_update_limited(SessionPid) ->
    ensure_voice_rate_limit_table(),
    Now = erlang:system_time(millisecond),
    case ets:lookup(?VOICE_RATE_LIMIT_TABLE, SessionPid) of
        [] ->
            ets:insert(?VOICE_RATE_LIMIT_TABLE, {SessionPid, [Now]}),
            false;
        [{SessionPid, Timestamps}] ->
            check_voice_rate(SessionPid, Timestamps, Now)
    end.

-spec check_voice_rate(pid(), [integer()], integer()) -> boolean().
check_voice_rate(SessionPid, Timestamps, Now) ->
    Filtered = [T || T <- Timestamps, (Now - T) < ?VOICE_RATE_LIMIT_WINDOW],
    case length(Filtered) >= ?VOICE_UPDATE_RATE_LIMIT of
        true ->
            logger:warning(
                "gateway_voice_state_update_rate_limited: session_pid=~p "
                "count=~p window_ms=~p",
                [SessionPid, length(Filtered), ?VOICE_RATE_LIMIT_WINDOW]
            ),
            true;
        false ->
            ets:insert(?VOICE_RATE_LIMIT_TABLE, {SessionPid, [Now | Filtered]}),
            false
    end.

-spec queue_voice_update(pid(), map()) -> ok.
queue_voice_update(SessionPid, Data) ->
    ensure_voice_queue_table(),
    Queue = get_voice_queue(SessionPid),
    NewQueue = enqueue_voice_update(Queue, Data),
    logger:warning(
        "gateway_voice_state_update_queue_insert: session_pid=~p queue_len=~p",
        [SessionPid, queue:len(NewQueue)]
    ),
    ets:insert(?VOICE_QUEUE_TABLE, {SessionPid, NewQueue}),
    ok.

-spec get_voice_queue(pid()) -> queue:queue(map()).
get_voice_queue(SessionPid) ->
    case ets:lookup(?VOICE_QUEUE_TABLE, SessionPid) of
        [] -> queue:new();
        [{SessionPid, Queue}] -> Queue
    end.

-spec enqueue_voice_update(queue:queue(map()), map()) -> queue:queue(map()).
enqueue_voice_update(Queue, Data) ->
    DedupedQueue = drop_queued_voice_updates_for_key(Queue, voice_update_debounce_key(Data)),
    TrimmedQueue = trim_voice_queue(DedupedQueue),
    typed_queue_in(Data, TrimmedQueue).

-spec typed_queue_in(map(), queue:queue(map())) -> queue:queue(map()).
typed_queue_in(Item, Queue) when is_map(Item) ->
    queue:in(Item, Queue).

-spec queued_keys(queue:queue(map())) -> #{term() => true}.
queued_keys(Queue) ->
    lists:foldl(
        fun add_queued_key/2,
        #{},
        queue:to_list(Queue)
    ).

-spec add_queued_key(map(), #{term() => true}) -> #{term() => true}.
add_queued_key(Q, Acc) ->
    case voice_update_debounce_key(Q) of
        undefined -> Acc;
        Key -> Acc#{Key => true}
    end.

-spec drop_queued_voice_updates_for_key(queue:queue(map()), term()) -> queue:queue(map()).
drop_queued_voice_updates_for_key(Queue, undefined) ->
    Queue;
drop_queued_voice_updates_for_key(Queue, Key) ->
    case maps:is_key(Key, queued_keys(Queue)) of
        false ->
            Queue;
        true ->
            filter_queued_voice_updates_by_key(Queue, Key)
    end.

-spec filter_queued_voice_updates_by_key(queue:queue(map()), term()) -> queue:queue(map()).
filter_queued_voice_updates_by_key(Queue, Key) ->
    queue:filter(fun(Q) -> voice_update_debounce_key(Q) =/= Key end, Queue).

-spec voice_update_debounce_key(map()) -> term().
voice_update_debounce_key(Data) when is_map(Data) ->
    Key = {
        maps:get(<<"guild_id">>, Data, undefined),
        maps:get(<<"connection_id">>, Data, undefined)
    },
    case Key of
        {undefined, undefined} -> undefined;
        _ -> Key
    end.

-spec trim_voice_queue(queue:queue(map())) -> queue:queue(map()).
trim_voice_queue(Queue) ->
    case queue:len(Queue) >= ?MAX_VOICE_QUEUE_LENGTH of
        false ->
            Queue;
        true ->
            drop_oldest_voice_item(Queue)
    end.

-spec drop_oldest_voice_item(queue:queue(map())) -> queue:queue(map()).
drop_oldest_voice_item(Queue) ->
    case queue:out(Queue) of
        {empty, _} -> Queue;
        {{value, _}, Rest} -> Rest
    end.

-spec ensure_voice_queue_timer(state()) -> state().
ensure_voice_queue_timer(#{voice_queue_timer := undefined} = State) ->
    Timer = erlang:send_after(?VOICE_QUEUE_PROCESS_INTERVAL, self(), {process_voice_queue}),
    State#{voice_queue_timer => Timer};
ensure_voice_queue_timer(State) ->
    State.

-spec process_queued_voice_updates(state()) -> state().
process_queued_voice_updates(#{session_pid := SessionPid} = State) when is_pid(SessionPid) ->
    ensure_voice_queue_table(),
    case ets:lookup(?VOICE_QUEUE_TABLE, SessionPid) of
        [] -> State;
        [{SessionPid, Queue}] -> process_queue_item(Queue, SessionPid, State)
    end;
process_queued_voice_updates(State) ->
    State.

-spec process_queue_item(queue:queue(), pid(), state()) -> state().
process_queue_item(Queue, SessionPid, State) ->
    case queue:out(Queue) of
        {empty, _} ->
            ets:delete(?VOICE_QUEUE_TABLE, SessionPid),
            State;
        {{value, Data}, NewQueue} ->
            dispatch_or_requeue(SessionPid, Data, NewQueue, State)
    end.

-spec dispatch_or_requeue(pid(), term(), queue:queue(), state()) -> state().
dispatch_or_requeue(SessionPid, Data, NewQueue, State) ->
    case should_queue_voice_update(SessionPid) of
        false ->
            {ok, _} = process_voice_update(SessionPid, ensure_voice_data_map(Data), State),
            finalize_queue(SessionPid, NewQueue, State);
        true ->
            ensure_voice_queue_timer(State)
    end.

-spec finalize_queue(pid(), queue:queue(), state()) -> state().
finalize_queue(SessionPid, NewQueue, State) ->
    case queue:is_empty(NewQueue) of
        true ->
            ets:delete(?VOICE_QUEUE_TABLE, SessionPid),
            State;
        false ->
            ets:insert(?VOICE_QUEUE_TABLE, {SessionPid, NewQueue}),
            ensure_voice_queue_timer(State)
    end.

-spec ensure_voice_queue_table() -> ok.
ensure_voice_queue_table() -> ensure_ets_table(?VOICE_QUEUE_TABLE).

-spec ensure_voice_rate_limit_table() -> ok.
ensure_voice_rate_limit_table() -> ensure_ets_table(?VOICE_RATE_LIMIT_TABLE).

-spec ensure_ets_table(atom()) -> ok.
ensure_ets_table(TableName) ->
    guild_ets_utils:ensure_table(
        TableName,
        [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]
    ).

-spec ensure_voice_data_map(term()) -> map().
ensure_voice_data_map(Data) when is_map(Data) -> Data;
ensure_voice_data_map(_) -> #{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_state() ->
    #{
        session_pid => undefined,
        voice_queue_timer => undefined,
        encoding => json,
        compress_ctx => undefined,
        version => 1,
        heartbeat_state => #{},
        socket_pid => undefined,
        peer_ip => undefined,
        rate_limit_state => #{events => [], op_events => #{}},
        request_guild_members_pid => undefined,
        request_guild_members_pending => undefined
    }.

cleanup_session_removes_voice_queue_and_rate_limit_entries_test() ->
    SessionPid = spawn(fun test_wait_until_stop/0),
    ensure_voice_queue_table(),
    ensure_voice_rate_limit_table(),
    ets:insert(?VOICE_QUEUE_TABLE, {SessionPid, queue:from_list([#{<<"guild_id">> => 1}])}),
    ets:insert(?VOICE_RATE_LIMIT_TABLE, {SessionPid, [erlang:system_time(millisecond)]}),
    cleanup_session(SessionPid),
    ?assertEqual([], ets:lookup(?VOICE_QUEUE_TABLE, SessionPid)),
    ?assertEqual([], ets:lookup(?VOICE_RATE_LIMIT_TABLE, SessionPid)),
    SessionPid ! stop.

handle_voice_state_update_dead_session_is_nonfatal_test() ->
    DeadPid = spawn(fun() -> ok end),
    Ref = monitor(process, DeadPid),
    receive
        {'DOWN', Ref, process, DeadPid, _Reason} -> ok
    after 200 ->
        ?assert(false)
    end,
    ?assertMatch({ok, #{}}, handle_voice_state_update(DeadPid, #{}, new_test_state())),
    cleanup_session(DeadPid).

should_queue_voice_update_disabled_by_env_test() ->
    SessionPid = spawn(fun test_wait_until_stop/0),
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:putenv("FLUXER_DISABLE_RATE_LIMITS", "true"),
    delete_test_table(?VOICE_RATE_LIMIT_TABLE),
    try
        ?assertEqual(false, should_queue_voice_update(SessionPid)),
        ?assertEqual(false, should_queue_voice_update(SessionPid)),
        ?assertEqual(false, should_queue_voice_update(SessionPid)),
        ?assertEqual(undefined, ets:whereis(?VOICE_RATE_LIMIT_TABLE))
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue),
        SessionPid ! stop,
        cleanup_session(SessionPid)
    end.

voice_tables_are_owned_by_ets_owner_when_running_test() ->
    delete_test_table(?VOICE_QUEUE_TABLE),
    delete_test_table(?VOICE_RATE_LIMIT_TABLE),
    {ok, Pid} = guild_ets_owner:start_link(),
    try
        ok = ensure_voice_queue_table(),
        ok = ensure_voice_rate_limit_table(),
        ?assertEqual(Pid, ets:info(?VOICE_QUEUE_TABLE, owner)),
        ?assertEqual(Pid, ets:info(?VOICE_RATE_LIMIT_TABLE, owner))
    after
        gen_server:stop(guild_ets_owner)
    end.

test_wait_until_stop() ->
    receive
        stop -> ok
    after 1000 ->
        ok
    end.

delete_test_table(TableName) ->
    try ets:delete(TableName) of
        _ -> ok
    catch
        _:_ -> ok
    end.

restore_env(Key, false) ->
    os:unsetenv(Key);
restore_env(Key, Value) ->
    os:putenv(Key, Value).

-endif.
