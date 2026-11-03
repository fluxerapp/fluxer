%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_abuse_protection).
-typing([eqwalizer]).

-export([
    check_user_session_limit/1,
    increment_user_sessions/1,
    decrement_user_sessions/1,
    check_identify_rate/1
]).

-define(IDENTIFY_TABLE, gateway_identify_rate).
-define(SESSION_USER_COUNTS, session_user_counts).
-define(IDENTIFY_MAX_PER_IP, 300).
-define(IDENTIFY_WINDOW_SECS, 60).
-define(IDENTIFY_CLEANUP_INTERVAL_MS, ?IDENTIFY_WINDOW_SECS * 2 * 1000).
-define(MAX_SESSIONS_PER_USER, 100).

-spec check_user_session_limit(term()) -> ok | {error, too_many_sessions}.
check_user_session_limit(UserId) when is_integer(UserId), UserId > 0 ->
    case gateway_handler_rate_limit:rate_limits_disabled() of
        true ->
            ok;
        false ->
            ok = ensure_session_user_counts_table(),
            do_check_user_session_limit(UserId)
    end;
check_user_session_limit(_) ->
    ok.

-spec do_check_user_session_limit(integer()) -> ok | {error, too_many_sessions}.
do_check_user_session_limit(UserId) ->
    try ets:lookup(?SESSION_USER_COUNTS, UserId) of
        [{_, Count}] when Count >= ?MAX_SESSIONS_PER_USER ->
            {error, too_many_sessions};
        _ ->
            ok
    catch
        error:badarg ->
            ok
    end.

-spec increment_user_sessions(term()) -> ok.
increment_user_sessions(UserId) when is_integer(UserId), UserId > 0 ->
    ok = ensure_session_user_counts_table(),
    try
        _ = ets:update_counter(?SESSION_USER_COUNTS, UserId, {2, 1}, {UserId, 0}),
        ok
    catch
        error:badarg -> ok
    end;
increment_user_sessions(_) ->
    ok.

-spec decrement_user_sessions(term()) -> ok.
decrement_user_sessions(UserId) when is_integer(UserId), UserId > 0 ->
    try
        case ets:update_counter(?SESSION_USER_COUNTS, UserId, {2, -1, 0, 0}) of
            0 ->
                ets:delete(?SESSION_USER_COUNTS, UserId),
                ok;
            _ ->
                ok
        end
    catch
        error:badarg -> ok
    end;
decrement_user_sessions(_) ->
    ok.

-spec check_identify_rate(term()) -> ok | {error, identify_rate_limited}.
check_identify_rate(PeerIP) when is_binary(PeerIP) ->
    case gateway_handler_rate_limit:rate_limits_disabled() of
        true ->
            ok;
        false ->
            ok = ensure_identify_table(),
            do_check_identify_rate(PeerIP)
    end;
check_identify_rate(_) ->
    ok.

-spec do_check_identify_rate(binary()) -> ok | {error, identify_rate_limited}.
do_check_identify_rate(PeerIP) ->
    Now = erlang:system_time(second),
    Bucket = Now div ?IDENTIFY_WINDOW_SECS,
    Key = {PeerIP, Bucket},
    try ets:update_counter(?IDENTIFY_TABLE, Key, {2, 1}, {Key, 0}) of
        Count when Count > ?IDENTIFY_MAX_PER_IP ->
            {error, identify_rate_limited};
        _ ->
            ok
    catch
        error:badarg ->
            ok
    end.

-spec ensure_session_user_counts_table() -> ok.
ensure_session_user_counts_table() ->
    case ets:whereis(?SESSION_USER_COUNTS) of
        undefined -> create_session_user_counts_table();
        _ -> ok
    end.

-spec create_session_user_counts_table() -> ok.
create_session_user_counts_table() ->
    _ = create_rate_table(?SESSION_USER_COUNTS),
    ok.

-spec create_rate_table(atom()) -> created | exists.
create_rate_table(Table) ->
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

-spec ensure_identify_table() -> ok.
ensure_identify_table() ->
    case ets:whereis(?IDENTIFY_TABLE) of
        undefined -> create_identify_table();
        _ -> ok
    end.

-spec create_identify_table() -> ok.
create_identify_table() ->
    case create_rate_table(?IDENTIFY_TABLE) of
        created -> schedule_identify_cleanup();
        exists -> ok
    end.

-spec schedule_identify_cleanup() -> ok.
schedule_identify_cleanup() ->
    case ets:whereis(?IDENTIFY_TABLE) of
        undefined -> ok;
        Tid -> spawn_identify_cleanup(Tid)
    end.

-spec spawn_identify_cleanup(ets:table()) -> ok.
spawn_identify_cleanup(Tid) ->
    _ = spawn(fun() -> identify_cleanup_loop(Tid) end),
    ok.

-spec identify_cleanup_loop(ets:table()) -> ok.
identify_cleanup_loop(Table) ->
    ok = gateway_retry_timer:wait(?IDENTIFY_CLEANUP_INTERVAL_MS),
    case prune_old_identify_entries(Table) of
        ok -> identify_cleanup_loop(Table);
        gone -> ok
    end.

-spec prune_old_identify_entries(ets:table()) -> ok | gone.
prune_old_identify_entries(Table) ->
    Now = erlang:system_time(second),
    Cutoff = Now div ?IDENTIFY_WINDOW_SECS - 1,
    try
        _ = ets:select_delete(Table, [
            {{{'$1', '$2'}, '_'}, [{'<', '$2', Cutoff}], [true]}
        ]),
        ok
    catch
        error:badarg -> gone
    end.
