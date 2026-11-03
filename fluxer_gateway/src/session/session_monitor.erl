%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_monitor).
-typing([eqwalizer]).

-export([
    handle_process_down/3,
    find_guild_by_ref/2,
    find_call_by_ref/2
]).

-export_type([session_state/0, guild_id/0, channel_id/0, guild_ref/0, call_ref/0]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type guild_ref() :: {pid(), reference()} | undefined | cached_unavailable | unavailable.
-type call_ref() :: {pid(), reference()} | undefined.

-define(RESUME_OFFLINE_GRACE_MS, 5000).

-spec handle_process_down(reference(), term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_process_down(Ref, Reason, State) ->
    SocketRef = maps:get(socket_mref, State, undefined),
    PresenceRef = maps:get(presence_mref, State, undefined),
    case Ref of
        SocketRef when Ref =:= SocketRef ->
            handle_socket_down(Reason, State);
        PresenceRef when Ref =:= PresenceRef ->
            handle_presence_down(State);
        _ ->
            handle_other_process_down(Ref, Reason, State)
    end.

-spec handle_other_process_down(reference(), term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_other_process_down(Ref, Reason, State) ->
    case session_connection_guild:handle_guild_connect_worker_down(Ref, Reason, State) of
        {guild_connect_worker, Result} -> Result;
        not_guild_connect_worker -> handle_monitored_resource_down(Ref, Reason, State)
    end.

-spec handle_monitored_resource_down(reference(), term(), session_state()) ->
    {noreply, session_state()}.
handle_monitored_resource_down(Ref, Reason, State) ->
    Guilds = maps:get(guilds, State),
    Calls = maps:get(calls, State, #{}),
    case find_guild_by_ref(Ref, Guilds) of
        {ok, GuildId} ->
            handle_guild_down(GuildId, Reason, State, Guilds);
        not_found ->
            handle_call_or_ignore(Ref, Reason, State, Calls)
    end.

-spec handle_call_or_ignore(reference(), term(), session_state(), #{channel_id() => call_ref()}) ->
    {noreply, session_state()}.
handle_call_or_ignore(Ref, Reason, State, Calls) ->
    case find_call_by_ref(Ref, Calls) of
        {ok, ChannelId} ->
            handle_call_down(ChannelId, Reason, State, Calls);
        not_found ->
            {noreply, State}
    end.

-spec handle_socket_down(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_socket_down({shutdown, client_closed}, State) ->
    {stop, normal, State#{socket_pid => undefined, socket_mref => undefined}};
handle_socket_down(_Reason, State) ->
    hold_session_for_resume(State).

-spec hold_session_for_resume(session_state()) -> {noreply, session_state()}.
hold_session_for_resume(State) ->
    ResumeToken = make_ref(),
    ResumeTimerRef = erlang:send_after(
        constants:resume_timeout(), self(), {resume_timeout, ResumeToken}
    ),
    OfflineToken = make_ref(),
    OfflineTimerRef = erlang:send_after(
        ?RESUME_OFFLINE_GRACE_MS, self(), {resume_offline_timeout, OfflineToken}
    ),
    {noreply, (remember_resume_status(State))#{
        socket_pid => undefined,
        socket_mref => undefined,
        resume_timer => {ResumeToken, ResumeTimerRef},
        offline_timer => {OfflineToken, OfflineTimerRef}
    }}.

-spec remember_resume_status(session_state()) -> session_state().
remember_resume_status(#{status := offline} = State) ->
    State;
remember_resume_status(#{status := Status} = State) ->
    State#{resume_status => Status}.

-spec handle_presence_down(session_state()) -> {noreply, session_state()}.
handle_presence_down(State) ->
    self() ! {presence_connect, 0},
    {noreply, State#{presence_pid => undefined}}.

-spec handle_guild_down(guild_id(), term(), session_state(), #{guild_id() => guild_ref()}) ->
    {noreply, session_state()}.
handle_guild_down(GuildId, {shutdown, handoff}, State, Guilds) ->
    NewGuilds = Guilds#{GuildId => undefined},
    self() ! {guild_connect, GuildId, 0},
    {noreply, State#{guilds => NewGuilds}};
handle_guild_down(GuildId, handoff, State, Guilds) ->
    NewGuilds = Guilds#{GuildId => undefined},
    self() ! {guild_connect, GuildId, 0},
    {noreply, State#{guilds => NewGuilds}};
handle_guild_down(GuildId, killed, State, _Guilds) ->
    gen_server:cast(self(), {guild_leave, GuildId}),
    {noreply, State};
handle_guild_down(GuildId, _Reason, State, Guilds) ->
    GuildDeleteData = #{
        <<"id">> => integer_to_binary(GuildId),
        <<"unavailable">> => true
    },
    {noreply, UpdatedState} = session_dispatch:handle_dispatch(
        guild_delete, GuildDeleteData, State
    ),
    NewGuilds = Guilds#{GuildId => undefined},
    DedupState = schedule_guild_reconnect(GuildId, 1000, UpdatedState),
    {noreply, DedupState#{guilds => NewGuilds}}.

-spec schedule_guild_reconnect(guild_id(), non_neg_integer(), session_state()) ->
    session_state().
schedule_guild_reconnect(GuildId, Delay, State) ->
    Timers0 = guild_reconnect_timers(State),
    cancel_existing_timer(maps:get(GuildId, Timers0, undefined)),
    TimerRef = erlang:send_after(Delay, self(), {guild_connect, GuildId, 0}),
    State#{guild_reconnect_timers => Timers0#{GuildId => TimerRef}}.

-spec guild_reconnect_timers(session_state()) -> #{guild_id() => reference()}.
guild_reconnect_timers(State) ->
    case maps:get(guild_reconnect_timers, State, #{}) of
        Timers when is_map(Timers) -> eqwalizer:dynamic_cast(Timers);
        _ -> #{}
    end.

-spec cancel_existing_timer(reference() | undefined) -> ok.
cancel_existing_timer(TimerRef) when is_reference(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef),
    ok;
cancel_existing_timer(_) ->
    ok.

-spec handle_call_down(channel_id(), term(), session_state(), #{channel_id() => call_ref()}) ->
    {noreply, session_state()}.
handle_call_down(ChannelId, {shutdown, handoff}, State, Calls) ->
    reconnect_call_after_handoff(ChannelId, State, Calls);
handle_call_down(ChannelId, handoff, State, Calls) ->
    reconnect_call_after_handoff(ChannelId, State, Calls);
handle_call_down(ChannelId, killed, State, Calls) ->
    NewCalls = maps:remove(ChannelId, Calls),
    {noreply, State#{calls => NewCalls}};
handle_call_down(ChannelId, _Reason, State, Calls) ->
    CallDeleteData = #{
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"unavailable">> => true
    },
    {noreply, UpdatedState} = session_dispatch:handle_dispatch(
        call_delete, CallDeleteData, State
    ),
    NewCalls = Calls#{ChannelId => undefined},
    DedupState = schedule_call_reconnect(ChannelId, 1000, UpdatedState),
    {noreply, DedupState#{calls => NewCalls}}.

-spec schedule_call_reconnect(channel_id(), non_neg_integer(), session_state()) ->
    session_state().
schedule_call_reconnect(ChannelId, Delay, State) ->
    Timers0 = call_reconnect_timers(State),
    cancel_existing_timer(maps:get(ChannelId, Timers0, undefined)),
    TimerRef = erlang:send_after(Delay, self(), {call_reconnect, ChannelId, 0}),
    State#{call_reconnect_timers => Timers0#{ChannelId => TimerRef}}.

-spec call_reconnect_timers(session_state()) -> #{channel_id() => reference()}.
call_reconnect_timers(State) ->
    case maps:get(call_reconnect_timers, State, #{}) of
        Timers when is_map(Timers) -> eqwalizer:dynamic_cast(Timers);
        _ -> #{}
    end.

-spec reconnect_call_after_handoff(channel_id(), session_state(), #{channel_id() => call_ref()}) ->
    {noreply, session_state()}.
reconnect_call_after_handoff(ChannelId, State, Calls) ->
    NewCalls = Calls#{ChannelId => undefined},
    erlang:send_after(0, self(), {call_reconnect, ChannelId, 0}),
    {noreply, State#{calls => NewCalls}}.

-spec find_guild_by_ref(reference(), #{guild_id() => guild_ref()}) ->
    {ok, guild_id()} | not_found.
find_guild_by_ref(Ref, Guilds) ->
    find_by_ref(Ref, Guilds).

-spec find_call_by_ref(reference(), #{channel_id() => call_ref()}) ->
    {ok, channel_id()} | not_found.
find_call_by_ref(Ref, Calls) ->
    find_by_ref(Ref, Calls).

-spec find_by_ref(reference(), #{integer() => term()}) ->
    {ok, integer()} | not_found.
find_by_ref(Ref, Map) ->
    maps:fold(
        fun
            (Id, {_Pid, R}, _) when R =:= Ref -> {ok, Id};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Map
    ).
