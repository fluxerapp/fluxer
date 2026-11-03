%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_connection_retry).
-typing([eqwalizer]).

-export([
    retry_or_fail/3,
    retry_timeout_without_penalty/3,
    handle_call_reconnect/3,
    is_guild_manager_nodedown_failure/1,
    is_transient_connect_exception/1
]).

-export_type([session_state/0, guild_id/0, channel_id/0, attempt/0, session_result/0]).

-define(MAX_RETRY_ATTEMPTS, 25).
-define(MAX_CALL_RETRY_ATTEMPTS, 15).
-define(GUILD_SLOW_RETRY_DELAY_MS, 60000).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type attempt() :: non_neg_integer().

-type session_result() :: {noreply, session_state()} | {stop, normal, session_state()}.

-spec retry_or_fail(guild_id(), attempt(), session_state()) -> session_result().
retry_or_fail(GuildId, Attempt, State) when Attempt < ?MAX_RETRY_ATTEMPTS ->
    Delay = backoff_utils:calculate_with_jitter(Attempt),
    erlang:send_after(Delay, self(), {guild_connect, GuildId, Attempt + 1}),
    {noreply, State};
retry_or_fail(GuildId, Attempt, State) ->
    UserId = maps:get(user_id, State),
    logger:warning(
        "guild_connect_exhausted: guild_id=~p user_id=~p"
        " total_attempts=~p slow_retry_ms=~p",
        [GuildId, UserId, Attempt, ?GUILD_SLOW_RETRY_DELAY_MS]
    ),
    finish_exhausted_guild_retry(GuildId, State).

-spec retry_timeout_without_penalty(guild_id(), attempt(), session_state()) -> session_result().
retry_timeout_without_penalty(GuildId, Attempt, State) when Attempt < ?MAX_RETRY_ATTEMPTS ->
    NextAttempt = Attempt + 1,
    DelayAttempt = min(NextAttempt, 10),
    Delay = backoff_utils:calculate_with_jitter(DelayAttempt),
    erlang:send_after(Delay, self(), {guild_connect, GuildId, NextAttempt}),
    {noreply, State};
retry_timeout_without_penalty(GuildId, Attempt, State) ->
    UserId = maps:get(user_id, State),
    logger:warning(
        "guild_connect_no_penalty_exhausted: guild_id=~p user_id=~p"
        " total_attempts=~p slow_retry_ms=~p",
        [GuildId, UserId, Attempt, ?GUILD_SLOW_RETRY_DELAY_MS]
    ),
    finish_exhausted_guild_retry(GuildId, State).

-spec finish_exhausted_guild_retry(guild_id(), session_state()) -> session_result().
finish_exhausted_guild_retry(GuildId, State) ->
    {noreply, MarkedState} = session_ready:mark_guild_unavailable(GuildId, State),
    case session_ready:check_readiness(MarkedState) of
        {noreply, ReadyState} ->
            erlang:send_after(?GUILD_SLOW_RETRY_DELAY_MS, self(), {guild_connect, GuildId, 0}),
            {noreply, ReadyState};
        {stop, normal, ReadyState} ->
            {stop, normal, ReadyState}
    end.

-spec handle_call_reconnect(channel_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
handle_call_reconnect(ChannelId, Attempt, State) ->
    Calls = maps:get(calls, State, #{}),
    case maps:get(ChannelId, Calls, undefined) of
        {_Pid, _Ref} -> {noreply, State};
        _ -> attempt_call_reconnect(ChannelId, Attempt, State)
    end.

-spec attempt_call_reconnect(
    channel_id(), attempt(), session_state()
) -> {noreply, session_state()}.
attempt_call_reconnect(ChannelId, Attempt, State) ->
    case call_manager:lookup(ChannelId) of
        {ok, CallPid} -> connect_to_call_process(CallPid, ChannelId, Attempt, State);
        _ -> retry_call_or_remove(ChannelId, Attempt, State)
    end.

-spec connect_to_call_process(
    pid(), channel_id(), attempt(), session_state()
) -> {noreply, session_state()}.
connect_to_call_process(CallPid, ChannelId, Attempt, State) ->
    Calls = maps:get(calls, State, #{}),
    MonitorRef = monitor(process, CallPid),
    NewCalls = Calls#{ChannelId => {CallPid, MonitorRef}},
    StateWithCall = State#{calls => NewCalls},
    case gateway_rpc_call_lookup:safe_gen_server_call(CallPid, {get_state}, 5000) of
        {ok, {ok, CallData}} when is_map(CallData) ->
            session_dispatch:handle_dispatch(call_create, CallData, StateWithCall);
        {ok, CallData} when is_map(CallData) ->
            session_dispatch:handle_dispatch(call_create, CallData, StateWithCall);
        _Error ->
            demonitor(MonitorRef, [flush]),
            retry_call_or_remove(ChannelId, Attempt, State)
    end.

-spec retry_call_or_remove(
    channel_id(), attempt(), session_state()
) -> {noreply, session_state()}.
retry_call_or_remove(ChannelId, Attempt, State) when
    Attempt < ?MAX_CALL_RETRY_ATTEMPTS
->
    Delay = backoff_utils:calculate_with_jitter(Attempt),
    Msg = {call_reconnect, ChannelId, Attempt + 1},
    erlang:send_after(Delay, self(), Msg),
    {noreply, State};
retry_call_or_remove(ChannelId, _Attempt, State) ->
    Calls = maps:get(calls, State, #{}),
    {noreply, State#{calls => maps:remove(ChannelId, Calls)}}.

-spec is_guild_manager_nodedown_failure(term()) -> boolean().
is_guild_manager_nodedown_failure({exit, {{nodedown, N}, {gen_server, call, _}}}) when
    is_atom(N)
->
    true;
is_guild_manager_nodedown_failure({exit, {{nodedown, N}, _}}) when is_atom(N) -> true;
is_guild_manager_nodedown_failure({exit, {nodedown, N}}) when is_atom(N) -> true;
is_guild_manager_nodedown_failure({exit, {noproc, {gen_server, call, _}}}) ->
    true;
is_guild_manager_nodedown_failure({exit, {noproc, _}}) ->
    true;
is_guild_manager_nodedown_failure({error, {nodedown, N}}) when is_atom(N) -> true;
is_guild_manager_nodedown_failure({nodedown, N}) when is_atom(N) -> true;
is_guild_manager_nodedown_failure({error, noproc}) ->
    true;
is_guild_manager_nodedown_failure(noproc) ->
    true;
is_guild_manager_nodedown_failure({exit, {{badarg, [{ets, _, _, _} | _]}, _}}) ->
    true;
is_guild_manager_nodedown_failure(_) ->
    false.

-spec is_transient_connect_exception(term()) -> boolean().
is_transient_connect_exception(badarg) -> true;
is_transient_connect_exception({badarg, _}) -> true;
is_transient_connect_exception(_) -> false.
