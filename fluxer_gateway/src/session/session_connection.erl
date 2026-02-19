%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(session_connection).

-export([
    handle_presence_connect/2,
    handle_guild_connect/3,
    handle_guild_connect_result/4,
    handle_guild_connect_timeout/3,
    handle_call_reconnect/3
]).

-define(GUILD_CONNECT_MAX_INFLIGHT, 8).
-define(MAX_RETRY_ATTEMPTS, 25).
-define(MAX_CALL_RETRY_ATTEMPTS, 15).
-define(GUILD_CONNECT_ASYNC_TIMEOUT_MS, 30000).
-define(GUILD_MANAGER_START_TIMEOUT_MS, 20000).
-define(GUILD_MANAGER_LOOKUP_FALLBACK_TIMEOUT_MS, 200).
-define(MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS, 30000).
-define(MAX_GUILD_UNAVAILABLE_BACKOFF_ATTEMPT, 5).
-define(GUILD_UNAVAILABLE_JITTER_DIVISOR, 5).
-define(GUILD_SLOW_RETRY_DELAY_MS, 60000).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type attempt() :: non_neg_integer().

-type guild_connect_result() ::
    {ok, pid(), map()}
    | {ok_unavailable, pid(), map()}
    | {ok_cached_unavailable, map()}
    | {error, term()}.

-spec handle_presence_connect(attempt(), session_state()) ->
    {noreply, session_state()}.
handle_presence_connect(Attempt, State) ->
    UserId = maps:get(user_id, State),
    UserData = maps:get(user_data, State),
    Guilds = maps:get(guilds, State),
    Status = maps:get(status, State),
    SessionId = maps:get(id, State),
    Afk = maps:get(afk, State),
    Mobile = maps:get(mobile, State),
    SocketPid = maps:get(socket_pid, State, undefined),
    FriendIds = presence_targets:friend_ids_from_state(State),
    GroupDmRecipients = presence_targets:group_dm_recipients_from_state(State),
    Request = #{
        user_id => UserId,
        user_data => UserData,
        guild_ids => maps:keys(Guilds),
        status => Status,
        friend_ids => FriendIds,
        group_dm_recipients => GroupDmRecipients,
        custom_status => maps:get(custom_status, State, null)
    },
    try presence_manager:start_or_lookup(Request) of
        {ok, Pid} ->
            try_presence_session_connect(
                Pid,
                SessionId,
                Status,
                Afk,
                Mobile,
                SocketPid,
                FriendIds,
                GroupDmRecipients,
                Attempt,
                State
            );
        _ ->
            schedule_presence_retry(Attempt, State)
    catch
        exit:{noproc, _} ->
            schedule_presence_retry(Attempt, State);
        exit:{normal, _} ->
            schedule_presence_retry(Attempt, State);
        _:_ ->
            schedule_presence_retry(Attempt, State)
    end.

-spec try_presence_session_connect(
    pid(),
    binary(),
    atom(),
    boolean(),
    boolean(),
    pid() | undefined,
    [integer()],
    map(),
    attempt(),
    session_state()
) ->
    {noreply, session_state()}.
try_presence_session_connect(
    Pid, SessionId, Status, Afk, Mobile, SocketPid, FriendIds, GroupDmRecipients, Attempt, State
) ->
    try
        case
            gen_server:call(
                Pid,
                {session_connect, #{
                    session_id => SessionId,
                    status => Status,
                    afk => Afk,
                    mobile => Mobile,
                    socket_pid => SocketPid
                }},
                10000
            )
        of
            {ok, Sessions} ->
                gen_server:cast(Pid, {sync_friends, FriendIds}),
                gen_server:cast(Pid, {sync_group_dm_recipients, GroupDmRecipients}),
                NewState = maps:merge(State, #{
                    presence_pid => Pid,
                    presence_mref => monitor(process, Pid),
                    collected_sessions => Sessions
                }),
                session_ready:check_readiness(NewState);
            _ ->
                schedule_presence_retry(Attempt, State)
        end
    catch
        exit:{noproc, _} ->
            schedule_presence_retry(Attempt, State);
        exit:{normal, _} ->
            schedule_presence_retry(Attempt, State);
        _:_ ->
            {noreply, State}
    end.

-spec schedule_presence_retry(attempt(), session_state()) -> {noreply, session_state()}.
schedule_presence_retry(Attempt, State) when Attempt < ?MAX_RETRY_ATTEMPTS ->
    erlang:send_after(backoff_utils:calculate(Attempt), self(), {presence_connect, Attempt + 1}),
    {noreply, State};
schedule_presence_retry(_Attempt, State) ->
    {noreply, State}.

-spec handle_guild_connect(guild_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
handle_guild_connect(GuildId, Attempt, State) ->
    Guilds = maps:get(guilds, State),
    SessionId = maps:get(id, State),
    UserId = maps:get(user_id, State),
    case maps:get(GuildId, Guilds, undefined) of
        {_Pid, _Ref} ->
            {noreply, State};
        cached_unavailable ->
            maybe_handle_cached_unavailability(GuildId, Attempt, SessionId, UserId, State);
        _ ->
            maybe_spawn_guild_connect(GuildId, Attempt, SessionId, UserId, State)
    end.

-spec maybe_handle_cached_unavailability(
    guild_id(), attempt(), binary(), integer(), session_state()
) ->
    {noreply, session_state()}.
maybe_handle_cached_unavailability(GuildId, Attempt, SessionId, UserId, State) ->
    UserData = maps:get(user_data, State, #{}),
    case guild_availability:is_guild_unavailable_for_user_from_cache(GuildId, UserData) of
        true ->
            case Attempt of
                0 ->
                    logger:info(
                        "guild_connect_cached_unavailable: guild_id=~p user_id=~p",
                        [GuildId, UserId]
                    );
                _ ->
                    ok
            end,
            mark_cached_guild_unavailable_and_retry(GuildId, Attempt, State);
        false ->
            Guilds = maps:get(guilds, State, #{}),
            ResetGuilds = maps:put(GuildId, undefined, Guilds),
            ResetState = maps:put(guilds, ResetGuilds, State),
            maybe_spawn_guild_connect(GuildId, 0, SessionId, UserId, ResetState)
    end.

-spec mark_cached_guild_unavailable(guild_id(), session_state()) ->
    {noreply, session_state()}.
mark_cached_guild_unavailable(GuildId, State) ->
    Guilds = maps:get(guilds, State, #{}),
    case maps:get(GuildId, Guilds, undefined) of
        cached_unavailable ->
            {noreply, State};
        _ ->
            UpdatedGuilds = maps:put(GuildId, cached_unavailable, Guilds),
            StateWithGuild = maps:put(guilds, UpdatedGuilds, State),
            {noreply, MarkedState} = session_ready:mark_guild_unavailable(GuildId, StateWithGuild),
            session_ready:check_readiness(MarkedState)
    end.

-spec mark_cached_guild_unavailable_and_retry(guild_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
mark_cached_guild_unavailable_and_retry(GuildId, Attempt, State) ->
    {noreply, MarkedState} = mark_cached_guild_unavailable(GuildId, State),
    schedule_cached_unavailable_retry(GuildId, Attempt, MarkedState).

-spec handle_guild_connect_result(guild_id(), attempt(), guild_connect_result(), session_state()) ->
    {noreply, session_state()}.
handle_guild_connect_result(GuildId, Attempt, Result, State) ->
    Inflight = maps:get(guild_connect_inflight, State, #{}),
    case maps:get(GuildId, Inflight, undefined) of
        Attempt ->
            NewInflight = maps:remove(GuildId, Inflight),
            State1 = maps:put(guild_connect_inflight, NewInflight, State),
            handle_guild_connect_result_internal(GuildId, Attempt, Result, State1);
        _ ->
            {noreply, State}
    end.

-spec handle_guild_connect_timeout(guild_id(), attempt(), session_state()) -> {noreply, session_state()}.
handle_guild_connect_timeout(GuildId, Attempt, State) ->
    Inflight0 = maps:get(guild_connect_inflight, State, #{}),
    case maps:get(GuildId, Inflight0, undefined) of
        Attempt ->
            UserId = maps:get(user_id, State),
            logger:warning(
                "guild_connect_timeout: guild_id=~p user_id=~p attempt=~p",
                [GuildId, UserId, Attempt]
            ),
            Inflight = maps:remove(GuildId, Inflight0),
            State1 = maps:put(guild_connect_inflight, Inflight, State),
            retry_or_fail(GuildId, Attempt, State1, fun(GId, St) ->
                session_ready:mark_guild_unavailable(GId, St)
            end);
        _ ->
            {noreply, State}
    end.

-spec handle_call_reconnect(channel_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
handle_call_reconnect(ChannelId, Attempt, State) ->
    Calls = maps:get(calls, State, #{}),
    SessionId = maps:get(id, State),
    case maps:get(ChannelId, Calls, undefined) of
        {_Pid, _Ref} ->
            {noreply, State};
        _ ->
            attempt_call_reconnect(ChannelId, Attempt, SessionId, State)
    end.

-spec maybe_spawn_guild_connect(guild_id(), attempt(), binary(), integer(), session_state()) ->
    {noreply, session_state()}.
maybe_spawn_guild_connect(GuildId, Attempt, SessionId, UserId, State) ->
    Inflight0 = maps:get(guild_connect_inflight, State, #{}),
    AlreadyInflight = maps:is_key(GuildId, Inflight0),
    TooManyInflight = map_size(Inflight0) >= ?GUILD_CONNECT_MAX_INFLIGHT,
    Bot = maps:get(bot, State, false),
    case {AlreadyInflight, TooManyInflight} of
        {true, _} ->
            {noreply, State};
        {false, true} ->
            erlang:send_after(50, self(), {guild_connect, GuildId, Attempt}),
            {noreply, State};
        {false, false} ->
            Inflight = maps:put(GuildId, Attempt, Inflight0),
            State1 = maps:put(guild_connect_inflight, Inflight, State),
            SessionPid = self(),
            InitialGuildId = maps:get(initial_guild_id, State, undefined),
            UserData = maps:get(user_data, State, #{}),
            spawn(fun() ->
                do_guild_connect(
                    SessionPid, GuildId, Attempt, SessionId, UserId, Bot, InitialGuildId, UserData
                )
            end),
            {noreply, State1}
    end.

-spec do_guild_connect(
    pid(), guild_id(), attempt(), binary(), integer(), boolean(), guild_id() | undefined, map()
) -> ok.
do_guild_connect(SessionPid, GuildId, Attempt, SessionId, UserId, Bot, InitialGuildId, UserData) ->
    Result =
        try
            case guild_manager:lookup(GuildId, ?GUILD_MANAGER_LOOKUP_FALLBACK_TIMEOUT_MS) of
                {ok, GuildPid} ->
                    start_guild_session_connect_async(
                        GuildPid,
                        SessionPid,
                        GuildId,
                        Attempt,
                        SessionId,
                        UserId,
                        Bot,
                        InitialGuildId,
                        UserData
                    );
                {error, not_found} ->
                    case guild_manager:ensure_started(GuildId, ?GUILD_MANAGER_START_TIMEOUT_MS) of
                        ok ->
                            {error, {guild_manager_failed, {error, loading}}};
                        {error, timeout} ->
                            {error, {guild_manager_failed, {error, timeout}}};
                        {error, EnsureReason} ->
                            {error, {guild_manager_failed, {error, EnsureReason}}}
                    end;
                Error ->
                    {error, {guild_manager_failed, Error}}
            end
        catch
            exit:{noproc, _} ->
                {error, {guild_died, noproc}};
            exit:{normal, _} ->
                {error, {guild_died, normal}};
            _:Reason ->
                {error, {exception, Reason}}
        end,
    case Result of
        pending ->
            ok;
        _ ->
            SessionPid ! {guild_connect_result, GuildId, Attempt, Result}
    end,
    ok.

-spec start_guild_session_connect_async(
    pid(), pid(), guild_id(), attempt(), binary(), integer(), boolean(), guild_id() | undefined, map()
) ->
    pending | {ok_cached_unavailable, map()}.
start_guild_session_connect_async(
    GuildPid, SessionPid, GuildId, Attempt, SessionId, UserId, Bot, InitialGuildId, UserData
) ->
    case maybe_build_unavailable_response_from_cache(GuildId, UserData) of
        {ok, UnavailableResponse} ->
            {ok_cached_unavailable, UnavailableResponse};
        not_unavailable ->
            ActiveGuilds = build_initial_active_guilds(InitialGuildId, GuildId),
            IsStaff = maps:get(<<"is_staff">>, UserData, false),
            Request = #{
                session_id => SessionId,
                user_id => UserId,
                session_pid => SessionPid,
                bot => Bot,
                is_staff => IsStaff,
                initial_guild_id => InitialGuildId,
                active_guilds => ActiveGuilds
            },
            gen_server:cast(GuildPid, {session_connect_async, #{
                guild_id => GuildId,
                attempt => Attempt,
                request => Request
            }}),
            _ = erlang:send_after(
                ?GUILD_CONNECT_ASYNC_TIMEOUT_MS,
                SessionPid,
                {guild_connect_timeout, GuildId, Attempt}
            ),
            pending
    end.

-spec maybe_build_unavailable_response_from_cache(guild_id(), map()) ->
    {ok, map()} | not_unavailable.
maybe_build_unavailable_response_from_cache(GuildId, UserData) ->
    case guild_availability:is_guild_unavailable_for_user_from_cache(GuildId, UserData) of
        true ->
            {ok, #{
                <<"id">> => integer_to_binary(GuildId),
                <<"unavailable">> => true
            }};
        false ->
            not_unavailable
    end.

-spec handle_guild_connect_result_internal(
    guild_id(), attempt(), guild_connect_result(), session_state()
) ->
    {noreply, session_state()}.
handle_guild_connect_result_internal(
    GuildId, _Attempt, {ok_unavailable, GuildPid, UnavailableResponse}, State
) ->
    finalize_guild_connection(GuildId, GuildPid, State, fun(St) ->
        session_ready:process_guild_state(UnavailableResponse, St)
    end);
handle_guild_connect_result_internal(
    GuildId, Attempt, {ok_cached_unavailable, _UnavailableResponse}, State
) ->
    mark_cached_guild_unavailable_and_retry(GuildId, Attempt, State);
handle_guild_connect_result_internal(GuildId, _Attempt, {ok, GuildPid, GuildState}, State) ->
    finalize_guild_connection(GuildId, GuildPid, State, fun(St) ->
        session_ready:process_guild_state(GuildState, St)
    end);
handle_guild_connect_result_internal(GuildId, Attempt, {error, {session_connect_failed, Reason}}, State) ->
    UserId = maps:get(user_id, State),
    logger:warning(
        "guild_session_connect_failed: guild_id=~p user_id=~p attempt=~p reason=~p",
        [GuildId, UserId, Attempt, Reason]
    ),
    retry_or_fail(GuildId, Attempt, State, fun(_GId, St) -> {noreply, St} end);
handle_guild_connect_result_internal(
    GuildId,
    Attempt,
    {error, {guild_manager_failed, {error, timeout}}},
    State
) ->
    UserId = maps:get(user_id, State),
    case Attempt of
        0 ->
            logger:debug(
                "guild_connect_deferred_timeout: guild_id=~p user_id=~p attempt=~p",
                [GuildId, UserId, Attempt]
            );
        _ when Attempt rem 5 =:= 0 ->
            logger:debug(
                "guild_connect_deferred_timeout: guild_id=~p user_id=~p attempt=~p",
                [GuildId, UserId, Attempt]
            );
        _ ->
            ok
    end,
    retry_timeout_without_penalty(GuildId, Attempt, State);
handle_guild_connect_result_internal(
    GuildId,
    Attempt,
    {error, {guild_manager_failed, {error, loading}}},
    State
) ->
    UserId = maps:get(user_id, State),
    case Attempt of
        0 ->
            logger:debug(
                "guild_connect_deferred_loading: guild_id=~p user_id=~p attempt=~p",
                [GuildId, UserId, Attempt]
            );
        _ when Attempt rem 5 =:= 0 ->
            logger:debug(
                "guild_connect_deferred_loading: guild_id=~p user_id=~p attempt=~p",
                [GuildId, UserId, Attempt]
            );
        _ ->
            ok
    end,
    retry_timeout_without_penalty(GuildId, Attempt, State);
handle_guild_connect_result_internal(GuildId, Attempt, {error, Reason}, State) ->
    UserId = maps:get(user_id, State),
    logger:warning(
        "guild_connect_failed: guild_id=~p user_id=~p attempt=~p reason=~p",
        [GuildId, UserId, Attempt, Reason]
    ),
    retry_or_fail(GuildId, Attempt, State, fun(GId, St) ->
        session_ready:mark_guild_unavailable(GId, St)
    end).

-spec retry_timeout_without_penalty(guild_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
retry_timeout_without_penalty(GuildId, Attempt, State) ->
    NextAttempt = min(Attempt + 1, 4),
    DelayMs = backoff_utils:calculate(NextAttempt),
    erlang:send_after(DelayMs, self(), {guild_connect, GuildId, NextAttempt}),
    {noreply, State}.

-spec finalize_guild_connection(guild_id(), pid(), session_state(), fun(
    (session_state()) -> {noreply, session_state()}
)) ->
    {noreply, session_state()}.
finalize_guild_connection(GuildId, GuildPid, State, ReadyFun) ->
    Guilds0 = maps:get(guilds, State),
    case maps:get(GuildId, Guilds0, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            {noreply, State};
        _ ->
            MonitorRef = monitor(process, GuildPid),
            Guilds = maps:put(GuildId, {GuildPid, MonitorRef}, Guilds0),
            State1 = maps:put(guilds, Guilds, State),
            ReadyFun(State1)
    end.

-spec retry_or_fail(guild_id(), attempt(), session_state(), fun(
    (guild_id(), session_state()) -> {noreply, session_state()}
)) ->
    {noreply, session_state()}.
retry_or_fail(GuildId, Attempt, State, _FailureFun) when Attempt < ?MAX_RETRY_ATTEMPTS ->
    BackoffMs = backoff_utils:calculate(Attempt),
    erlang:send_after(BackoffMs, self(), {guild_connect, GuildId, Attempt + 1}),
    {noreply, State};
retry_or_fail(GuildId, Attempt, State, FailureFun) ->
    UserId = maps:get(user_id, State),
    logger:warning(
        "guild_connect_exhausted: guild_id=~p user_id=~p total_attempts=~p slow_retry_ms=~p",
        [GuildId, UserId, Attempt, ?GUILD_SLOW_RETRY_DELAY_MS]
    ),
    {noreply, MarkedState} = FailureFun(GuildId, State),
    erlang:send_after(?GUILD_SLOW_RETRY_DELAY_MS, self(), {guild_connect, GuildId, 0}),
    {noreply, MarkedState}.

-spec schedule_cached_unavailable_retry(guild_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
schedule_cached_unavailable_retry(GuildId, Attempt, State) ->
    SessionId = maps:get(id, State, <<>>),
    DelayMs = cached_unavailable_retry_delay_ms(GuildId, SessionId, Attempt),
    NextAttempt = Attempt + 1,
    erlang:send_after(DelayMs, self(), {guild_connect, GuildId, NextAttempt}),
    {noreply, State}.

-spec cached_unavailable_retry_delay_ms(guild_id(), binary(), attempt()) -> non_neg_integer().
cached_unavailable_retry_delay_ms(GuildId, SessionId, Attempt) ->
    CappedAttempt = min(Attempt, ?MAX_GUILD_UNAVAILABLE_BACKOFF_ATTEMPT),
    BaseDelay = backoff_utils:calculate(CappedAttempt, ?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS),
    case BaseDelay >= ?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS of
        true ->
            ?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS;
        false ->
            MaxJitter = max(1, BaseDelay div ?GUILD_UNAVAILABLE_JITTER_DIVISOR),
            Jitter = erlang:phash2({GuildId, SessionId, Attempt}, MaxJitter + 1),
            min(?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS, BaseDelay + Jitter)
    end.

-spec attempt_call_reconnect(channel_id(), attempt(), binary(), session_state()) ->
    {noreply, session_state()}.
attempt_call_reconnect(ChannelId, Attempt, _SessionId, State) ->
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, CallPid} ->
            connect_to_call_process(CallPid, ChannelId, State);
        not_found ->
            retry_call_or_remove(ChannelId, Attempt, State);
        _Error ->
            retry_call_or_remove(ChannelId, Attempt, State)
    end.

-spec connect_to_call_process(pid(), channel_id(), session_state()) ->
    {noreply, session_state()}.
connect_to_call_process(CallPid, ChannelId, State) ->
    Calls = maps:get(calls, State, #{}),
    MonitorRef = monitor(process, CallPid),
    NewCalls = maps:put(ChannelId, {CallPid, MonitorRef}, Calls),
    StateWithCall = maps:put(calls, NewCalls, State),
    case gen_server:call(CallPid, {get_state}, 5000) of
        {ok, CallData} ->
            session_dispatch:handle_dispatch(call_create, CallData, StateWithCall);
        _Error ->
            demonitor(MonitorRef, [flush]),
            {noreply, State}
    end.

-spec retry_call_or_remove(channel_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
retry_call_or_remove(ChannelId, Attempt, State) when Attempt < ?MAX_CALL_RETRY_ATTEMPTS ->
    erlang:send_after(
        backoff_utils:calculate(Attempt), self(), {call_reconnect, ChannelId, Attempt + 1}
    ),
    {noreply, State};
retry_call_or_remove(ChannelId, _Attempt, State) ->
    Calls = maps:get(calls, State, #{}),
    NewCalls = maps:remove(ChannelId, Calls),
    {noreply, maps:put(calls, NewCalls, State)}.

-spec build_initial_active_guilds(guild_id() | undefined, guild_id()) -> sets:set(guild_id()).
build_initial_active_guilds(undefined, _GuildId) ->
    sets:new();
build_initial_active_guilds(GuildId, GuildId) ->
    sets:from_list([GuildId]);
build_initial_active_guilds(_, _) ->
    sets:new().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_initial_active_guilds_test() ->
    ?assertEqual(sets:new(), build_initial_active_guilds(undefined, 123)),
    ?assertEqual(sets:from_list([123]), build_initial_active_guilds(123, 123)),
    ?assertEqual(sets:new(), build_initial_active_guilds(456, 123)),
    ok.

mark_cached_guild_unavailable_test() ->
    GuildId = 2001,
    CacheState = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{
                <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]
            }
        }
    },
    _ = guild_availability:update_unavailability_cache_for_state(CacheState),
    State0 = #{
        id => <<"session-1">>,
        user_id => 55,
        user_data => #{<<"flags">> => <<"0">>},
        guilds => #{GuildId => undefined},
        collected_guild_states => [],
        ready => undefined
    },
    {noreply, State1} = mark_cached_guild_unavailable(GuildId, State0),
    Guilds = maps:get(guilds, State1),
    ?assertEqual(cached_unavailable, maps:get(GuildId, Guilds)),
    Collected = maps:get(collected_guild_states, State1, []),
    ?assertEqual(1, length(Collected)),
    ?assertMatch(
        #{<<"id">> := _, <<"unavailable">> := true},
        hd(Collected)
    ),
    {noreply, State2} = mark_cached_guild_unavailable(GuildId, State1),
    ?assertEqual(1, length(maps:get(collected_guild_states, State2, []))),
    CacheCleanupState = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{
                <<"features">> => []
            }
        }
    },
    _ = guild_availability:update_unavailability_cache_for_state(CacheCleanupState),
    ok.

do_guild_connect_skips_session_connect_when_cached_unavailable_test() ->
    GuildId = 2002,
    Attempt = 3,
    SessionId = <<"session-2">>,
    UserId = 77,
    CacheState = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{
                <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]
            }
        }
    },
    _ = guild_availability:update_unavailability_cache_for_state(CacheState),
    Parent = self(),
    TestRef = make_ref(),
    GuildPid = spawn(fun() -> guild_stub_loop(Parent, TestRef) end),
    ManagerPid = spawn(fun() -> manager_stub_loop(GuildId, GuildPid) end),
    ?assertEqual(undefined, whereis(guild_manager)),
    true = register(guild_manager, ManagerPid),
    try
        ok = do_guild_connect(
            Parent, GuildId, Attempt, SessionId, UserId, false, undefined, #{<<"flags">> => <<"0">>}
        ),
        case await_guild_connect_unavailable_result(GuildId, Attempt) of
            {ok, Response} ->
                ?assertEqual(integer_to_binary(GuildId), maps:get(<<"id">>, Response)),
                ?assertEqual(true, maps:get(<<"unavailable">>, Response));
            timeout ->
                ?assert(false, guild_connect_result_not_received)
        end,
        case saw_guild_stub_call(TestRef, 200) of
            true ->
                ?assert(false, should_not_call_session_connect_when_cache_unavailable);
            false ->
                ok
        end
    after
        case whereis(guild_manager) of
            ManagerPid ->
                unregister(guild_manager);
            _ ->
                ok
        end,
        ManagerPid ! stop,
        GuildPid ! stop,
        CacheCleanupState = #{
            id => GuildId,
            data => #{
                <<"guild">> => #{
                    <<"features">> => []
                }
            }
        },
        _ = guild_availability:update_unavailability_cache_for_state(CacheCleanupState),
        ok
    end.

do_guild_connect_uses_session_connect_async_cast_test() ->
    GuildId = 2004,
    Attempt = 0,
    SessionId = <<"session-async-1">>,
    UserId = 77,
    Parent = self(),
    TestRef = make_ref(),
    GuildPid = spawn(fun() -> guild_stub_loop(Parent, TestRef) end),
    ManagerPid = spawn(fun() -> manager_stub_loop(GuildId, GuildPid) end),
    ?assertEqual(undefined, whereis(guild_manager)),
    true = register(guild_manager, ManagerPid),
    SessionPid = spawn(fun() -> session_capture_loop() end),
    try
        ok = do_guild_connect(
            SessionPid,
            GuildId,
            Attempt,
            SessionId,
            UserId,
            false,
            undefined,
            #{<<"flags">> => <<"0">>, <<"is_staff">> => false}
        ),
        ?assertMatch({session_connect_async, _}, await_guild_stub_cast(TestRef, 1000))
    after
        SessionPid ! stop,
        case whereis(guild_manager) of
            ManagerPid -> unregister(guild_manager);
            _ -> ok
        end,
        ManagerPid ! stop,
        GuildPid ! stop
    end.

session_capture_loop() ->
    receive
        stop -> ok;
        _ -> session_capture_loop()
    end.

await_guild_stub_cast(TestRef, TimeoutMs) ->
    receive
        {guild_stub_cast, TestRef, Msg} ->
            Msg;
        _Other ->
            await_guild_stub_cast(TestRef, TimeoutMs)
    after TimeoutMs ->
        timeout
    end.

-spec await_guild_connect_unavailable_result(guild_id(), attempt()) ->
    {ok, map()} | timeout.
await_guild_connect_unavailable_result(GuildId, Attempt) ->
    receive
        {guild_connect_result, GuildId, Attempt, {ok_cached_unavailable, Response}} ->
            {ok, Response};
        _Other ->
            await_guild_connect_unavailable_result(GuildId, Attempt)
    after 1000 ->
        timeout
    end.

cached_unavailable_retry_delay_ms_cap_test() ->
    Delay = cached_unavailable_retry_delay_ms(123, <<"session-cap">>, 500),
    ?assertEqual(30000, Delay).

cached_unavailable_retry_delay_ms_uses_jitter_test() ->
    Delay = cached_unavailable_retry_delay_ms(123, <<"session-jitter">>, 0),
    ?assert(Delay >= 1000),
    ?assert(Delay =< 1200).

maybe_handle_cached_unavailability_retries_when_cache_available_again_test() ->
    GuildId = 2003,
    CacheState = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{
                <<"features">> => []
            }
        }
    },
    _ = guild_availability:update_unavailability_cache_for_state(CacheState),
    Inflight = maps:from_list([{N, N} || N <- lists:seq(3000, 3007)]),
    State0 = #{
        id => <<"session-3">>,
        user_id => 88,
        user_data => #{<<"flags">> => <<"0">>},
        guilds => #{GuildId => cached_unavailable},
        guild_connect_inflight => Inflight
    },
    {noreply, State1} = maybe_handle_cached_unavailability(
        GuildId, 42, <<"session-3">>, 88, State0
    ),
    Guilds = maps:get(guilds, State1),
    ?assertEqual(undefined, maps:get(GuildId, Guilds)),
    receive
        {guild_connect, GuildId, 0} -> ok
    after 300 ->
        ?assert(false, guild_connect_retry_not_scheduled_with_reset_attempt)
    end.

guild_connect_timeout_exhaustion_marks_unavailable_test() ->
    GuildId = 9001,
    Attempt = ?MAX_RETRY_ATTEMPTS,
    State0 = #{
        id => <<"session-timeout-1">>,
        user_id => 100,
        guilds => #{GuildId => undefined},
        guild_connect_inflight => #{GuildId => Attempt},
        collected_guild_states => [],
        ready => undefined
    },
    {noreply, State1} = handle_guild_connect_timeout(GuildId, Attempt, State0),
    Collected = maps:get(collected_guild_states, State1, []),
    ?assertEqual(1, length(Collected)),
    [UnavailableEntry] = Collected,
    ?assertEqual(integer_to_binary(GuildId), maps:get(<<"id">>, UnavailableEntry)),
    ?assertEqual(true, maps:get(<<"unavailable">>, UnavailableEntry)),
    Inflight = maps:get(guild_connect_inflight, State1, #{}),
    ?assertEqual(false, maps:is_key(GuildId, Inflight)).

-spec saw_guild_stub_call(reference(), non_neg_integer()) -> boolean.
saw_guild_stub_call(TestRef, TimeoutMs) ->
    receive
        {guild_stub_called, TestRef, _Request} ->
            true;
        _Other ->
            saw_guild_stub_call(TestRef, TimeoutMs)
    after TimeoutMs ->
        false
    end.

-spec manager_stub_loop(guild_id(), pid()) -> ok.
manager_stub_loop(GuildId, GuildPid) ->
    receive
        stop ->
            ok;
        {'$gen_call', From, {lookup, GuildId}} ->
            gen_server:reply(From, {ok, GuildPid}),
            manager_stub_loop(GuildId, GuildPid);
        {'$gen_call', From, {ensure_started, GuildId}} ->
            gen_server:reply(From, ok),
            manager_stub_loop(GuildId, GuildPid);
        {'$gen_call', From, {start_or_lookup, GuildId}} ->
            gen_server:reply(From, {ok, GuildPid}),
            manager_stub_loop(GuildId, GuildPid);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, {error, unsupported}),
            manager_stub_loop(GuildId, GuildPid);
        _Other ->
            manager_stub_loop(GuildId, GuildPid)
    end.

-spec guild_stub_loop(pid(), reference()) -> ok.
guild_stub_loop(Parent, TestRef) ->
    receive
        stop ->
            ok;
        {'$gen_cast', Msg} ->
            Parent ! {guild_stub_cast, TestRef, Msg},
            guild_stub_loop(Parent, TestRef);
        {'$gen_call', From, Request} ->
            Parent ! {guild_stub_called, TestRef, Request},
            gen_server:reply(From, {ok, #{}}),
            guild_stub_loop(Parent, TestRef);
        _Other ->
            guild_stub_loop(Parent, TestRef)
    end.

-endif.
