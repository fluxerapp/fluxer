%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_rate_limit).
-typing([eqwalizer]).

-export([
    check_rate_limit/2, check_shared_ip_rate/1, check_shared_user_rate/1, rate_limits_disabled/0
]).

-export_type([state/0]).

-define(GATEWAY_RATE_LIMIT_WINDOW_MS, 60000).
-define(GATEWAY_RATE_LIMIT_MAX_EVENTS, 600).
-define(PRESENCE_RATE_LIMIT_WINDOW_MS, 20000).
-define(PRESENCE_RATE_LIMIT_MAX_EVENTS, 5).
-define(FORUM_UNREADS_RATE_LIMIT_WINDOW_MS, 5000).
-define(FORUM_UNREADS_RATE_LIMIT_MAX_EVENTS, 5).
-define(VOICE_SIGNAL_RATE_LIMIT_WINDOW_MS, 10000).
-define(VOICE_SIGNAL_RATE_LIMIT_MAX_EVENTS, 240).

-define(SHARED_IP_RATE_TABLE, gateway_shared_ip_rate).
-define(SHARED_USER_RATE_TABLE, gateway_shared_user_rate).

-define(SHARED_IP_RATE_WINDOW_MS, 60000).
-define(SHARED_IP_RATE_MAX_EVENTS, 6000).
-define(SHARED_USER_RATE_WINDOW_MS, 60000).
-define(SHARED_USER_RATE_MAX_EVENTS, 600).
-define(SHARED_RATE_CLEANUP_INTERVAL_MS, ?SHARED_IP_RATE_WINDOW_MS * 2).

-type state() :: gateway_handler:state().

-spec check_rate_limit(state(), atom()) ->
    {ok, state()} | {rate_limited, state()} | {opcode_rate_limited, state()}.
check_rate_limit(State, Op) ->
    case rate_limits_disabled() of
        true -> {ok, State};
        false -> check_shared_then_connection(State, Op)
    end.

-spec check_shared_then_connection(state(), atom()) ->
    {ok, state()} | {rate_limited, state()} | {opcode_rate_limited, state()}.
check_shared_then_connection(State, Op) ->
    case check_shared_budgets(State) of
        rate_limited ->
            {rate_limited, State};
        ok ->
            check_rate_limit_limited(State, Op)
    end.

-spec check_shared_budgets(state()) -> ok | rate_limited.
check_shared_budgets(State) ->
    case check_shared_ip_rate(maps:get(peer_ip, State, undefined)) of
        {error, ip_rate_limited} ->
            rate_limited;
        ok ->
            check_shared_user_budget(maps:get(session_pid, State, undefined))
    end.

-spec check_shared_user_budget(term()) -> ok | rate_limited.
check_shared_user_budget(SessionPid) when is_pid(SessionPid) ->
    case check_shared_user_rate(SessionPid) of
        {error, user_rate_limited} -> rate_limited;
        ok -> ok
    end;
check_shared_user_budget(_) ->
    ok.

-spec check_rate_limit_limited(state(), atom()) ->
    {ok, state()} | {rate_limited, state()} | {opcode_rate_limited, state()}.
check_rate_limit_limited(#{rate_limit_state := RateLimitState} = State, Op) ->
    Now = erlang:system_time(millisecond),
    Events = maps:get(events, RateLimitState, []),
    case
        check_timestamp_window(
            Events,
            Now,
            ?GATEWAY_RATE_LIMIT_WINDOW_MS,
            ?GATEWAY_RATE_LIMIT_MAX_EVENTS
        )
    of
        rate_limited ->
            {rate_limited, State};
        {ok, NewEvents} ->
            NewRLS = RateLimitState#{events => NewEvents},
            check_opcode_rate_limit(State#{rate_limit_state => NewRLS}, Op, Now)
    end.

-spec check_opcode_rate_limit(state(), atom(), integer()) ->
    {ok, state()} | {opcode_rate_limited, state()}.
check_opcode_rate_limit(State, presence_update, Now) ->
    check_named_opcode_rate_limit(
        State,
        presence_update,
        Now,
        ?PRESENCE_RATE_LIMIT_WINDOW_MS,
        ?PRESENCE_RATE_LIMIT_MAX_EVENTS
    );
check_opcode_rate_limit(State, request_forum_unreads, Now) ->
    check_named_opcode_rate_limit(
        State,
        request_forum_unreads,
        Now,
        ?FORUM_UNREADS_RATE_LIMIT_WINDOW_MS,
        ?FORUM_UNREADS_RATE_LIMIT_MAX_EVENTS
    );
check_opcode_rate_limit(State, voice_signal, Now) ->
    check_named_opcode_rate_limit(
        State,
        voice_signal,
        Now,
        ?VOICE_SIGNAL_RATE_LIMIT_WINDOW_MS,
        ?VOICE_SIGNAL_RATE_LIMIT_MAX_EVENTS
    );
check_opcode_rate_limit(State, _Op, _Now) ->
    {ok, State}.

-spec check_named_opcode_rate_limit(state(), atom(), integer(), pos_integer(), pos_integer()) ->
    {ok, state()} | {opcode_rate_limited, state()}.
check_named_opcode_rate_limit(
    #{rate_limit_state := RateLimitState} = State,
    Op,
    Now,
    WindowMs,
    MaxEvents
) ->
    OpEvents = maps:get(op_events, RateLimitState, #{}),
    Events = maps:get(Op, OpEvents, []),
    case check_timestamp_window(Events, Now, WindowMs, MaxEvents) of
        rate_limited ->
            {opcode_rate_limited, State};
        {ok, NewEvents} ->
            NewOpEvents = OpEvents#{Op => NewEvents},
            {ok, State#{rate_limit_state => RateLimitState#{op_events => NewOpEvents}}}
    end.

-spec check_timestamp_window([integer()], integer(), pos_integer(), pos_integer()) ->
    {ok, [integer()]} | rate_limited.
check_timestamp_window(Events, Now, WindowMs, MaxEvents) ->
    EventsInWindow = [T || T <- Events, (Now - T) < WindowMs],
    case length(EventsInWindow) >= MaxEvents of
        true -> rate_limited;
        false -> {ok, [Now | EventsInWindow]}
    end.

-spec check_shared_ip_rate(term()) -> ok | {error, ip_rate_limited}.
check_shared_ip_rate(PeerIP) when is_binary(PeerIP), PeerIP =/= <<"unknown">> ->
    case rate_limits_disabled() of
        true ->
            ok;
        false ->
            ensure_window_table(?SHARED_IP_RATE_TABLE, ?SHARED_IP_RATE_WINDOW_MS),
            check_shared_ip_window(PeerIP)
    end;
check_shared_ip_rate(_) ->
    ok.

-spec check_shared_ip_window(binary()) -> ok | {error, ip_rate_limited}.
check_shared_ip_window(PeerIP) ->
    case
        check_shared_window(
            ?SHARED_IP_RATE_TABLE,
            PeerIP,
            ?SHARED_IP_RATE_WINDOW_MS,
            ?SHARED_IP_RATE_MAX_EVENTS,
            ip_rate_limited
        )
    of
        ok -> ok;
        {error, ip_rate_limited} -> {error, ip_rate_limited}
    end.

-spec check_shared_user_rate(term()) -> ok | {error, user_rate_limited}.
check_shared_user_rate(UserKey) when UserKey =/= undefined ->
    case rate_limits_disabled() of
        true ->
            ok;
        false ->
            ensure_window_table(?SHARED_USER_RATE_TABLE, ?SHARED_USER_RATE_WINDOW_MS),
            check_shared_user_window(UserKey)
    end;
check_shared_user_rate(_) ->
    ok.

-spec check_shared_user_window(term()) -> ok | {error, user_rate_limited}.
check_shared_user_window(UserKey) ->
    case
        check_shared_window(
            ?SHARED_USER_RATE_TABLE,
            UserKey,
            ?SHARED_USER_RATE_WINDOW_MS,
            ?SHARED_USER_RATE_MAX_EVENTS,
            user_rate_limited
        )
    of
        ok -> ok;
        {error, user_rate_limited} -> {error, user_rate_limited}
    end.

-spec check_shared_window(atom(), term(), pos_integer(), pos_integer(), atom()) ->
    ok | {error, atom()}.
check_shared_window(Table, Key, WindowMs, MaxEvents, LimitReason) ->
    Now = erlang:system_time(millisecond),
    Bucket = Now div WindowMs,
    BucketKey = {Key, Bucket},
    try ets:update_counter(Table, BucketKey, {2, 1}, {BucketKey, 0}) of
        Count when Count > MaxEvents -> {error, LimitReason};
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec ensure_window_table(atom(), pos_integer()) -> ok.
ensure_window_table(Table, WindowMs) ->
    case ets:whereis(Table) of
        undefined -> create_window_table(Table, WindowMs);
        _ -> ok
    end.

-spec create_window_table(atom(), pos_integer()) -> ok.
create_window_table(Table, WindowMs) ->
    case create_table(Table) of
        created -> schedule_window_cleanup(Table, WindowMs);
        exists -> ok
    end.

-spec schedule_window_cleanup(atom(), pos_integer()) -> ok.
schedule_window_cleanup(Table, WindowMs) ->
    case ets:whereis(Table) of
        undefined -> ok;
        Tid -> spawn_window_cleanup(Tid, WindowMs)
    end.

-spec spawn_window_cleanup(ets:table(), pos_integer()) -> ok.
spawn_window_cleanup(Tid, WindowMs) ->
    _ = spawn(fun() -> window_cleanup_loop(Tid, WindowMs) end),
    ok.

-spec window_cleanup_loop(ets:table(), pos_integer()) -> ok.
window_cleanup_loop(Table, WindowMs) ->
    ok = gateway_retry_timer:wait(?SHARED_RATE_CLEANUP_INTERVAL_MS),
    case prune_old_window_entries(Table, WindowMs) of
        ok -> window_cleanup_loop(Table, WindowMs);
        gone -> ok
    end.

-spec prune_old_window_entries(ets:table(), pos_integer()) -> ok | gone.
prune_old_window_entries(Table, WindowMs) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now div WindowMs - 1,
    try
        _ = ets:select_delete(Table, [
            {{{'$1', '$2'}, '_'}, [{'<', '$2', Cutoff}], [true]}
        ]),
        ok
    catch
        error:badarg -> gone
    end.

-spec create_table(atom()) -> created | exists.
create_table(Table) ->
    try
        _ = ets:new(Table, rate_table_options()),
        created
    catch
        error:badarg -> exists
    end.

-spec rate_table_options() -> list().
rate_table_options() ->
    [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}] ++
        guild_ets_utils:heir_options().

-spec rate_limits_disabled() -> boolean().
rate_limits_disabled() ->
    case os:getenv("FLUXER_DISABLE_RATE_LIMITS") of
        "1" -> true;
        "true" -> true;
        "TRUE" -> true;
        _ -> false
    end.
