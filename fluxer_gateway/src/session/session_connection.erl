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
    handle_call_reconnect/3
]).

-define(GUILD_CONNECT_MAX_INFLIGHT, 8).

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

    Message =
        {start_or_lookup, #{
            user_id => UserId,
            user_data => UserData,
            guild_ids => maps:keys(Guilds),
            status => Status,
            friend_ids => FriendIds,
            group_dm_recipients => GroupDmRecipients,
            custom_status => maps:get(custom_status, State, null)
        }},

    case gen_server:call(presence_manager, Message, 5000) of
        {ok, Pid} ->
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
                        case Attempt < 25 of
                            true ->
                                erlang:send_after(
                                    backoff_utils:calculate(Attempt),
                                    self(),
                                    {presence_connect, Attempt + 1}
                                ),
                                {noreply, State};
                            false ->
                                {noreply, State}
                        end
                end
            catch
                exit:{noproc, _} when Attempt < 25 ->
                    erlang:send_after(
                        backoff_utils:calculate(Attempt), self(), {presence_connect, Attempt + 1}
                    ),
                    {noreply, State};
                exit:{normal, _} when Attempt < 25 ->
                    erlang:send_after(
                        backoff_utils:calculate(Attempt), self(), {presence_connect, Attempt + 1}
                    ),
                    {noreply, State};
                _:_ ->
                    {noreply, State}
            end;
        _ ->
            case Attempt < 25 of
                true ->
                    erlang:send_after(
                        backoff_utils:calculate(Attempt), self(), {presence_connect, Attempt + 1}
                    ),
                    {noreply, State};
                false ->
                    {noreply, State}
            end
    end.

handle_guild_connect(GuildId, Attempt, State) ->
    Guilds = maps:get(guilds, State),
    SessionId = maps:get(id, State),
    UserId = maps:get(user_id, State),

    case maps:get(GuildId, Guilds, undefined) of
        {_Pid, _Ref} ->
            {noreply, State};
        _ ->
            maybe_spawn_guild_connect(GuildId, Attempt, SessionId, UserId, State)
    end.

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

handle_call_reconnect(ChannelId, Attempt, State) ->
    Calls = maps:get(calls, State, #{}),
    SessionId = maps:get(id, State),

    case maps:get(ChannelId, Calls, undefined) of
        {_Pid, _Ref} ->
            {noreply, State};
        _ ->
            attempt_call_reconnect(ChannelId, Attempt, SessionId, State)
    end.

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
            spawn(fun() ->
                do_guild_connect(SessionPid, GuildId, Attempt, SessionId, UserId, Bot, InitialGuildId)
            end),
            {noreply, State1}
    end.

do_guild_connect(SessionPid, GuildId, Attempt, SessionId, UserId, Bot, InitialGuildId) ->
    Result =
        try
            case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 5000) of
                {ok, GuildPid} ->
                    ActiveGuilds = build_initial_active_guilds(InitialGuildId, GuildId),
                    Request = #{
                        session_id => SessionId,
                        user_id => UserId,
                        session_pid => SessionPid,
                        bot => Bot,
                        initial_guild_id => InitialGuildId,
                        active_guilds => ActiveGuilds
                    },
                    case gen_server:call(GuildPid, {session_connect, Request}, 10000) of
                        {ok, unavailable, UnavailableResponse} ->
                            {ok_unavailable, GuildPid, UnavailableResponse};
                        {ok, GuildState} ->
                            {ok, GuildPid, GuildState};
                        Error ->
                            {error, {session_connect_failed, Error}}
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
    SessionPid ! {guild_connect_result, GuildId, Attempt, Result},
    ok.

handle_guild_connect_result_internal(
    GuildId, _Attempt, {ok_unavailable, GuildPid, UnavailableResponse}, State
) ->
    finalize_guild_connection(GuildId, GuildPid, State, fun(St) ->
        session_ready:process_guild_state(UnavailableResponse, St)
    end);
handle_guild_connect_result_internal(GuildId, _Attempt, {ok, GuildPid, GuildState}, State) ->
    finalize_guild_connection(GuildId, GuildPid, State, fun(St) ->
        session_ready:process_guild_state(GuildState, St)
    end);
handle_guild_connect_result_internal(GuildId, Attempt, {error, {session_connect_failed, _}}, State) ->
    retry_or_fail(GuildId, Attempt, State, fun(_GId, St) -> {noreply, St} end);
handle_guild_connect_result_internal(GuildId, Attempt, {error, _Reason}, State) ->
    retry_or_fail(GuildId, Attempt, State, fun(GId, St) ->
        session_ready:mark_guild_unavailable(GId, St)
    end).

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

retry_or_fail(GuildId, Attempt, State, FailureFun) ->
    case Attempt < 25 of
        true ->
            BackoffMs = backoff_utils:calculate(Attempt),
            erlang:send_after(BackoffMs, self(), {guild_connect, GuildId, Attempt + 1}),
            {noreply, State};
        false ->
            logger:error(
                "[session_connection] Guild ~p connect failed after ~p attempts",
                [GuildId, Attempt]
            ),
            FailureFun(GuildId, State)
    end.

attempt_call_reconnect(ChannelId, Attempt, _SessionId, State) ->
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, CallPid} ->
            connect_to_call_process(CallPid, ChannelId, State);
        not_found ->
            handle_call_not_found(ChannelId, Attempt, State);
        _Error ->
            handle_call_lookup_error(ChannelId, Attempt, State)
    end.

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

handle_call_not_found(ChannelId, Attempt, State) ->
    retry_call_or_remove(ChannelId, Attempt, State).

handle_call_lookup_error(ChannelId, Attempt, State) ->
    retry_call_or_remove(ChannelId, Attempt, State).

retry_call_or_remove(ChannelId, Attempt, State) ->
    case Attempt < 15 of
        true ->
            erlang:send_after(
                backoff_utils:calculate(Attempt),
                self(),
                {call_reconnect, ChannelId, Attempt + 1}
            ),
            {noreply, State};
        false ->
            Calls = maps:get(calls, State, #{}),
            NewCalls = maps:remove(ChannelId, Calls),
            {noreply, maps:put(calls, NewCalls, State)}
    end.

build_initial_active_guilds(undefined, _GuildId) ->
    sets:new();
build_initial_active_guilds(GuildId, GuildId) ->
    sets:from_list([GuildId]);
build_initial_active_guilds(_, _) ->
    sets:new().
