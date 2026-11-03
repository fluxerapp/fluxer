%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export_type([
    session_state/0, session_id/0, user_id/0, guild_id/0, channel_id/0, seq/0, status/0
]).

-type session_id() :: binary().
-type user_id() :: integer().
-type guild_id() :: integer().
-type channel_id() :: integer().
-type seq() :: non_neg_integer().
-type status() :: online | offline | idle | dnd | invisible.
-type guild_ref() :: {pid(), reference()} | undefined | cached_unavailable | unavailable.
-type call_ref() :: {pid(), reference()} | undefined.
-type replay_buffer() :: eqwalizer:dynamic(limited_deque:deque() | [map()]).
-type pending_presence_buffer() :: eqwalizer:dynamic([map()] | queue:queue(map())).
-type reaction_buffer() :: eqwalizer:dynamic([map()] | queue:queue(map())).

-define(HIBERNATE_AFTER_MS, 5000).

-type session_state() :: #{
    id => session_id(),
    user_id => user_id(),
    user_data => map(),
    custom_status => map() | null,
    version => non_neg_integer(),
    token_hash => binary(),
    auth_session_id_hash => binary(),
    buffer => replay_buffer(),
    buffer_bytes => non_neg_integer(),
    seq => seq(),
    ack_seq => seq(),
    replay_floor => seq(),
    properties => map(),
    status => status(),
    resume_status => status(),
    afk => boolean(),
    mobile => boolean(),
    presence_pid => pid() | undefined,
    presence_mref => reference() | undefined,
    socket_pid => pid() | undefined,
    socket_mref => reference() | undefined,
    resume_timer => {reference(), reference()} | undefined,
    offline_timer => {reference(), reference()} | undefined,
    guilds => #{guild_id() => guild_ref()},
    active_guilds => sets:set(guild_id()),
    guild_health => #{guild_id() => {pid(), boolean()}},
    calls => #{channel_id() => call_ref()},
    channels => #{channel_id() => map()},
    ready => map() | undefined,
    bot => boolean(),
    shard => gateway_sharding:shard() | undefined,
    e2ee_capable => boolean(),
    geoip_country_code => binary() | undefined,
    ignored_events => #{binary() => true},
    initial_guild_id => guild_id() | undefined,
    collected_guild_states => [map()],
    collected_sessions => [map()],
    collected_presences => [map()],
    guild_subscription_state => #{guild_id() => map()},
    relationships => #{user_id() => integer()},
    suppress_presence_updates => boolean(),
    pending_presences => pending_presence_buffer(),
    guild_connect_inflight => #{guild_id() => non_neg_integer()},
    guild_connect_workers => #{reference() => {guild_id(), non_neg_integer(), pid()}},
    guild_connect_timers => #{guild_id() => {reference(), reference()}},
    pending_guild_joins => #{guild_id() => true},
    voice_queue => queue:queue(map()),
    voice_queue_timer => reference() | undefined,
    debounce_reactions => boolean(),
    thread_channels_capable => boolean(),
    reaction_buffer => reaction_buffer(),
    reaction_buffer_timer => reference() | undefined,
    _ => _
}.

-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(SessionData) ->
    gen_server:start_link(?MODULE, SessionData, [{hibernate_after, ?HIBERNATE_AFTER_MS}]).

-spec init(map()) -> {ok, session_state()}.
init(SessionData) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    State0 = session_init:build_state(SessionData),
    ScheduleStartedAt = gateway_timings:start(),
    session_init:schedule_timers(State0),
    GwTimings = gateway_timings:record_function(
        schedule_initial_timers,
        <<"session_init:schedule_timers/1">>,
        ScheduleStartedAt,
        gateway_timings:from_state(State0)
    ),
    {ok, gateway_timings:put_state(GwTimings, State0)}.

-spec handle_call(term(), gen_server:from(), session_state()) ->
    {reply, term(), session_state()} | {stop, normal, term(), session_state()}.
handle_call({token_verify, Token}, _From, State) when is_binary(Token) ->
    session_lifecycle:handle_token_verify(Token, State);
handle_call({is_staff}, _From, State) ->
    session_lifecycle:handle_is_staff(State);
handle_call({heartbeat_ack, Seq}, _From, State) when is_integer(Seq), Seq >= 0 ->
    session_lifecycle:handle_heartbeat_ack(Seq, State);
handle_call({resume, Seq, SocketPid}, _From, State) when
    is_integer(Seq), Seq >= 0, is_pid(SocketPid)
->
    session_lifecycle:handle_resume(Seq, SocketPid, State);
handle_call({get_state}, _From, State) ->
    {reply, session_lifecycle:serialize_state(State), State};
handle_call(export_state, _From, State) ->
    {reply, {ok, session_lifecycle:serialize_transfer_state(State)}, State};
handle_call({terminate, SessionIdHashes}, _From, State) ->
    case binary_list(SessionIdHashes) of
        {ok, Hashes} -> session_lifecycle:handle_terminate_call(Hashes, State);
        error -> {reply, ok, State}
    end;
handle_call({voice_state_update, Data}, _From, State) when is_map(Data) ->
    session_voice:handle_voice_state_update(Data, State);
handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), session_state()) ->
    {noreply, session_state()}
    | {noreply, session_state(), hibernate}
    | {stop, normal, session_state()}.
handle_cast(Msg, State) ->
    hibernate_after_ready(State, handle_cast_message(Msg, State)).

-spec handle_cast_message(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_cast_message({presence_update, Update}, State) when is_map(Update) ->
    session_lifecycle:handle_presence_update_cast(Update, State);
handle_cast_message({dispatch, Event, {pre_encoded, EncodedData} = Data}, State) when
    is_atom(Event), is_binary(EncodedData)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast_message({dispatch, Event, {pre_encoded, EncodedData} = Data}, State) when
    is_binary(Event), is_binary(EncodedData)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast_message({dispatch, Event, Data}, State) when
    is_atom(Event), is_map(Data);
    is_atom(Event), is_list(Data)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast_message({dispatch, Event, Data}, State) when
    is_binary(Event), is_map(Data);
    is_binary(Event), is_list(Data)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast_message({voice_rejected, ErrorAtom}, State) when is_atom(ErrorAtom) ->
    ok = session_voice:report_rejection(ErrorAtom, State),
    {noreply, State};
handle_cast_message({voice_signal, Data}, State) when is_map(Data) ->
    ok = session_voice:handle_voice_signal(Data, State),
    {noreply, State};
handle_cast_message(Msg, State) ->
    handle_cast_guild_or_lifecycle(Msg, State).

-spec handle_cast_guild_or_lifecycle(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_cast_guild_or_lifecycle({guild_health, GuildId, GuildPid, Degraded}, State) when
    is_integer(GuildId), is_pid(GuildPid), is_boolean(Degraded)
->
    session_guild_health:handle_update(GuildId, GuildPid, Degraded, State);
handle_cast_guild_or_lifecycle(handoff_fence, State) ->
    session_lifecycle:handle_handoff_fence(State);
handle_cast_guild_or_lifecycle({reconnect_drain, SocketPid}, State) when
    is_pid(SocketPid); SocketPid =:= undefined
->
    session_lifecycle:handle_reconnect_drain(SocketPid, State);
handle_cast_guild_or_lifecycle({guild_join, GuildId}, State) when is_integer(GuildId) ->
    self() ! {guild_connect, GuildId, 0},
    {noreply,
        session_bot_guilds:track_join(
            GuildId, session_visits:handle_guild_join(GuildId, State)
        )};
handle_cast_guild_or_lifecycle({visit_start, GuildId, Operator}, State) when
    is_integer(GuildId), is_boolean(Operator)
->
    session_visits:handle_start(GuildId, Operator, State);
handle_cast_guild_or_lifecycle({visit_end, GuildId}, State) when is_integer(GuildId) ->
    session_visits:handle_end(GuildId, State);
handle_cast_guild_or_lifecycle({visit_rejected, GuildId, _Reason}, State) when
    is_integer(GuildId)
->
    session_visits:handle_rejected(GuildId, State);
handle_cast_guild_or_lifecycle({visit_promoted, GuildId, GuildPid, GuildState}, State) when
    is_integer(GuildId), is_pid(GuildPid), is_map(GuildState)
->
    session_visits:handle_promoted(GuildId, GuildPid, GuildState, State);
handle_cast_guild_or_lifecycle({store_guild_subscriptions, Data}, State) when is_map(Data) ->
    {noreply, session_guilds:store_guild_subscriptions(Data, State)};
handle_cast_guild_or_lifecycle({guild_leave, GuildId, forced_unavailable, true}, State) when
    is_integer(GuildId)
->
    session_guilds:handle_forced_unavailable_guild_leave(GuildId, true, State);
handle_cast_guild_or_lifecycle({guild_leave, GuildId, forced_unavailable}, State) when
    is_integer(GuildId)
->
    session_guilds:handle_forced_unavailable_guild_leave(GuildId, false, State);
handle_cast_guild_or_lifecycle({guild_leave, GuildId}, State) when is_integer(GuildId) ->
    session_guilds:handle_guild_leave(GuildId, State);
handle_cast_guild_or_lifecycle({terminate, SessionIdHashes}, State) ->
    case binary_list(SessionIdHashes) of
        {ok, Hashes} -> session_lifecycle:handle_terminate_cast(Hashes, State);
        error -> {noreply, State}
    end;
handle_cast_guild_or_lifecycle({terminate_force}, State) ->
    {stop, normal, State};
handle_cast_guild_or_lifecycle(reconnect_drain, State) ->
    session_lifecycle:handle_reconnect_drain(State);
handle_cast_guild_or_lifecycle(Msg, State) ->
    do_handle_cast_call(Msg, State).

-spec do_handle_cast_call(term(), session_state()) ->
    {noreply, session_state()}.
do_handle_cast_call({call_monitor, ChannelId, CallPid}, State) when
    is_integer(ChannelId), is_pid(CallPid)
->
    session_lifecycle:handle_call_monitor(ChannelId, CallPid, State);
do_handle_cast_call({call_unmonitor, ChannelId}, State) when
    is_integer(ChannelId)
->
    session_lifecycle:handle_call_unmonitor(ChannelId, State);
do_handle_cast_call({call_force_disconnect, ChannelId, ConnId}, State) when
    is_integer(ChannelId), is_binary(ConnId)
->
    {noreply, session_lifecycle:force_disconnect_dm_call(ChannelId, ConnId, State)};
do_handle_cast_call({call_force_disconnect, ChannelId, undefined}, State) when
    is_integer(ChannelId)
->
    {noreply, session_lifecycle:force_disconnect_dm_call(ChannelId, undefined, State)};
do_handle_cast_call(_, State) ->
    {noreply, State}.

-spec handle_info(term(), session_state()) ->
    {noreply, session_state()}
    | {noreply, session_state(), hibernate}
    | {stop, normal, session_state()}.
handle_info(Msg, State) ->
    hibernate_after_ready(State, handle_info_message(Msg, State)).

-spec handle_info_message(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_info_message({presence_connect, Attempt}, #{presence_pid := undefined} = State) when
    is_integer(Attempt), Attempt >= 0
->
    session_connection_presence:handle_presence_connect(Attempt, State);
handle_info_message({presence_connect, _Attempt}, State) ->
    {noreply, State};
handle_info_message({guild_connect, GuildId, Attempt}, State) when
    is_integer(GuildId), is_integer(Attempt), Attempt >= 0
->
    session_connection_guild:handle_guild_connect(GuildId, Attempt, State);
handle_info_message({guild_connect_result, _, _, _} = Msg, State) ->
    handle_info_guild_connect_result(Msg, State);
handle_info_message({guild_connect_timeout, GuildId, Attempt, Token}, State) when
    is_integer(GuildId), is_integer(Attempt), Attempt >= 0, is_reference(Token)
->
    session_connection_guild:handle_guild_connect_timeout(GuildId, Attempt, Token, State);
handle_info_message({call_reconnect, ChannelId, Attempt}, State) when
    is_integer(ChannelId), is_integer(Attempt), Attempt >= 0
->
    session_connection_retry:handle_call_reconnect(ChannelId, Attempt, State);
handle_info_message({gateway_timing_update, _Timings}, #{ready := undefined} = State) ->
    {noreply, State};
handle_info_message({gateway_timing_update, Timings}, State) ->
    {noreply, gateway_timings:merge_state(Timings, State)};
handle_info_message({thread_user_flip, _Version}, State) ->
    {noreply, notify_thread_user_flip(State)};
handle_info_message({dm_partner_mutual, GuildId, PartnerIds}, State) when
    is_integer(GuildId), is_list(PartnerIds)
->
    session_dm_partners:handle_mutual(GuildId, PartnerIds, State);
handle_info_message(Msg, State) ->
    handle_info_lifecycle(Msg, State).

-spec hibernate_after_ready(
    session_state(), {noreply, session_state()} | {stop, normal, session_state()}
) ->
    {noreply, session_state()}
    | {noreply, session_state(), hibernate}
    | {stop, normal, session_state()}.
hibernate_after_ready(#{ready := Ready}, {noreply, #{ready := undefined} = State}) when
    Ready =/= undefined
->
    {noreply, State, hibernate};
hibernate_after_ready(_OldState, Result) ->
    Result.

-spec notify_thread_user_flip(session_state()) -> session_state().
notify_thread_user_flip(#{id := SessionId} = State) when is_binary(SessionId) ->
    case session_thread_backstop:capable(State) of
        true -> cast_thread_user_flip(SessionId, State);
        false -> State
    end;
notify_thread_user_flip(State) ->
    State.

-spec cast_thread_user_flip(binary(), session_state()) -> session_state().
cast_thread_user_flip(SessionId, State) ->
    maps:foreach(
        fun
            (_GuildId, {GuildPid, _Ref}) when is_pid(GuildPid) ->
                gen_server:cast(GuildPid, {thread_user_flip, SessionId});
            (_GuildId, _Ref) ->
                ok
        end,
        maps:get(guilds, State, #{})
    ),
    State.

-spec handle_info_guild_connect_result(tuple(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_info_guild_connect_result(
    {guild_connect_result, GuildId, Attempt, Result}, State
) when is_integer(GuildId), is_integer(Attempt), Attempt >= 0 ->
    session_connection_guild:handle_guild_connect_result(
        GuildId, Attempt, eqwalizer:dynamic_cast(Result), State
    );
handle_info_guild_connect_result(_, State) ->
    {noreply, State}.

-spec handle_info_lifecycle(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_info_lifecycle(enable_presence_updates, State) ->
    Flushed = session_dispatch:flush_all_pending_presences(State),
    {noreply, Flushed#{suppress_presence_updates => false}};
handle_info_lifecycle(premature_readiness, #{ready := undefined} = State) ->
    {noreply, State};
handle_info_lifecycle(premature_readiness, State) ->
    session_ready:dispatch_ready_data(State);
handle_info_lifecycle(bot_initial_ready, #{ready := undefined} = State) ->
    {noreply, State};
handle_info_lifecycle(bot_initial_ready, State) ->
    session_ready:dispatch_ready_data(State);
handle_info_lifecycle({resume_timeout, _} = Msg, State) ->
    do_handle_resume_timeout(Msg, State);
handle_info_lifecycle(resume_timeout, State) ->
    do_handle_resume_timeout(resume_timeout, State);
handle_info_lifecycle({resume_offline_timeout, _} = Msg, State) ->
    session_lifecycle:handle_resume_offline_timeout(Msg, State);
handle_info_lifecycle(Msg, State) ->
    do_handle_info_misc(Msg, State).

-spec do_handle_resume_timeout(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
do_handle_resume_timeout({resume_timeout, Token}, #{socket_pid := undefined} = State) ->
    case maps:get(resume_timer, State, undefined) of
        {Token, _TimerRef} ->
            {stop, normal, State#{resume_timer => undefined}};
        _ ->
            {noreply, State}
    end;
do_handle_resume_timeout({resume_timeout, _Token}, State) ->
    {noreply, State};
do_handle_resume_timeout(resume_timeout, #{socket_pid := undefined} = State) ->
    case maps:get(resume_timer, State, undefined) of
        undefined -> {stop, normal, State};
        _ -> {noreply, State}
    end;
do_handle_resume_timeout(resume_timeout, State) ->
    {noreply, State}.

-spec do_handle_info_misc(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
do_handle_info_misc({process_voice_queue}, State) ->
    handle_voice_queue(State);
do_handle_info_misc(flush_reaction_buffer, State) ->
    {noreply, session_dispatch:flush_reaction_buffer(State)};
do_handle_info_misc({'DOWN', Ref, process, _Pid, Reason}, State) when
    is_reference(Ref)
->
    session_monitor:handle_process_down(Ref, Reason, State);
do_handle_info_misc(check_ack_lag, State) ->
    erlang:send_after(60000, self(), check_ack_lag),
    State1 = session_connection_guild:repair_stalled_guild_connects(State),
    {noreply, session_connection_presence:repair_presence_connection(State1)};
do_handle_info_misc({presence_rejoin_check}, State) ->
    handle_presence_rejoin_check(State);
do_handle_info_misc(_Info, State) ->
    {noreply, State}.

-spec handle_presence_rejoin_check(session_state()) -> {noreply, session_state()}.
handle_presence_rejoin_check(State) ->
    RepairedState = session_connection_presence:repair_presence_connection(State),
    {noreply, session_dispatch_presence:sync_presence_targets(RepairedState)}.

-spec terminate(term(), session_state()) -> ok.
terminate(Reason, State) ->
    session_lifecycle:terminate(Reason, State).

-spec code_change(term(), session_state(), term()) -> {ok, session_state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 10),
    erlang:garbage_collect(),
    {ok, State}.

-spec handle_voice_queue(session_state()) -> {noreply, session_state()}.
handle_voice_queue(State) ->
    NewState = session_voice:process_voice_queue(State),
    VoiceQueue = maps:get(voice_queue, NewState, queue:new()),
    case queue:is_empty(VoiceQueue) of
        false ->
            Timer = erlang:send_after(100, self(), {process_voice_queue}),
            {noreply, NewState#{voice_queue_timer => Timer}};
        true ->
            {noreply, NewState}
    end.

-spec binary_list(term()) -> {ok, [binary()]} | error.
binary_list(Value) when is_list(Value) ->
    binary_list(Value, []);
binary_list(_) ->
    error.

-spec binary_list([term()], [binary()]) -> {ok, [binary()]} | error.
binary_list([], Acc) ->
    {ok, lists:reverse(Acc)};
binary_list([Value | Rest], Acc) when is_binary(Value) ->
    binary_list(Rest, [Value | Acc]);
binary_list(_, _) ->
    error.
