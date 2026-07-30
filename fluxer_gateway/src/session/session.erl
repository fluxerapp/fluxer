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
    calls => #{channel_id() => call_ref()},
    channels => #{channel_id() => map()},
    ready => map() | undefined,
    bot => boolean(),
    shard => gateway_sharding:shard() | undefined,
    e2ee_capable => boolean(),
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
    voice_queue => queue:queue(map()),
    voice_queue_timer => reference() | undefined,
    debounce_reactions => boolean(),
    reaction_buffer => reaction_buffer(),
    reaction_buffer_timer => reference() | undefined,
    _ => _
}.

-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(SessionData) ->
    gen_server:start_link(?MODULE, SessionData, []).

-spec init(map()) -> {ok, session_state()}.
init(SessionData) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
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
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_cast({presence_update, Update}, State) when is_map(Update) ->
    session_lifecycle:handle_presence_update_cast(Update, State);
handle_cast({dispatch, Event, {pre_encoded, EncodedData} = Data}, State) when
    is_atom(Event), is_binary(EncodedData)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast({dispatch, Event, {pre_encoded, EncodedData} = Data}, State) when
    is_binary(Event), is_binary(EncodedData)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast({dispatch, Event, Data}, State) when
    is_atom(Event), is_map(Data)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast({dispatch, Event, Data}, State) when
    is_binary(Event), is_map(Data)
->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast({initial_global_presences, Presences}, State) ->
    handle_cast_presences(Presences, State);
handle_cast(Msg, State) ->
    handle_cast_guild_or_lifecycle(Msg, State).

-spec handle_cast_presences(term(), session_state()) ->
    {noreply, session_state()}.
handle_cast_presences(Presences, State) ->
    case map_list(Presences) of
        {ok, PresenceMaps} ->
            session_lifecycle:handle_initial_global_presences(
                PresenceMaps, State
            );
        error ->
            {noreply, State}
    end.

-spec handle_cast_guild_or_lifecycle(term(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_cast_guild_or_lifecycle(handoff_fence, State) ->
    session_lifecycle:handle_handoff_fence(State);
handle_cast_guild_or_lifecycle({reconnect_drain, SocketPid}, State) when
    is_pid(SocketPid); SocketPid =:= undefined
->
    session_lifecycle:handle_reconnect_drain(SocketPid, State);
handle_cast_guild_or_lifecycle({guild_join, GuildId}, State) when is_integer(GuildId) ->
    self() ! {guild_connect, GuildId, 0},
    {noreply, State};
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
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_info({presence_connect, Attempt}, #{presence_pid := undefined} = State) when
    is_integer(Attempt), Attempt >= 0
->
    session_connection:handle_presence_connect(Attempt, State);
handle_info({presence_connect, _Attempt}, State) ->
    {noreply, State};
handle_info({guild_connect, GuildId, Attempt}, State) when
    is_integer(GuildId), is_integer(Attempt), Attempt >= 0
->
    session_connection:handle_guild_connect(GuildId, Attempt, State);
handle_info({guild_connect_result, _, _, _} = Msg, State) ->
    handle_info_guild_connect_result(Msg, State);
handle_info({guild_connect_timeout, GuildId, Attempt}, State) when
    is_integer(GuildId), is_integer(Attempt), Attempt >= 0
->
    session_connection:handle_guild_connect_timeout(GuildId, Attempt, State);
handle_info({call_reconnect, ChannelId, Attempt}, State) when
    is_integer(ChannelId), is_integer(Attempt), Attempt >= 0
->
    session_connection:handle_call_reconnect(ChannelId, Attempt, State);
handle_info({gateway_timing_update, Timings}, State) ->
    {noreply, gateway_timings:merge_state(Timings, State)};
handle_info(Msg, State) ->
    handle_info_lifecycle(Msg, State).

-spec handle_info_guild_connect_result(tuple(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_info_guild_connect_result(
    {guild_connect_result, GuildId, Attempt, Result}, State
) when is_integer(GuildId), is_integer(Attempt), Attempt >= 0 ->
    session_connection:handle_guild_connect_result(
        GuildId, Attempt, Result, State
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
    {noreply, session_connection:repair_presence_connection(State1)};
do_handle_info_misc({presence_rejoin_check}, State) ->
    handle_presence_rejoin_check(State);
do_handle_info_misc(_Info, State) ->
    {noreply, State}.

-spec handle_presence_rejoin_check(session_state()) -> {noreply, session_state()}.
handle_presence_rejoin_check(State) ->
    RepairedState = session_connection:repair_presence_connection(State),
    {noreply, session_dispatch_presence:sync_presence_targets(RepairedState)}.

-spec terminate(term(), session_state()) -> ok.
terminate(Reason, State) ->
    session_lifecycle:terminate(Reason, State).

-spec code_change(term(), session_state(), term()) -> {ok, session_state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
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

-spec map_list(term()) -> {ok, [map()]} | error.
map_list(Value) when is_list(Value) ->
    map_list(Value, []);
map_list(_) ->
    error.

-spec map_list([term()], [map()]) -> {ok, [map()]} | error.
map_list([], Acc) ->
    {ok, lists:reverse(Acc)};
map_list([Value | Rest], Acc) when is_map(Value) ->
    map_list(Rest, [Value | Acc]);
map_list(_, _) ->
    error.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_base_state(GuildId) ->
    Buf = limited_deque:new(4096, 16777216),
    maps:merge(test_base_core(GuildId, Buf), test_base_extra()).

test_base_core(GuildId, Buffer) ->
    #{
        id => <<"test-session">>,
        user_id => 1,
        user_data => #{},
        custom_status => null,
        version => 1,
        token_hash => <<>>,
        auth_session_id_hash => <<>>,
        buffer => Buffer,
        seq => 0,
        ack_seq => 0,
        properties => #{},
        status => online,
        resume_status => online,
        afk => false,
        mobile => false,
        presence_pid => undefined,
        presence_mref => undefined,
        socket_pid => undefined,
        socket_mref => undefined,
        resume_timer => undefined,
        offline_timer => undefined,
        guilds => #{GuildId => {self(), make_ref()}},
        calls => #{},
        channels => #{},
        ready => undefined,
        bot => false
    }.

test_base_extra() ->
    #{
        ignored_events => #{},
        initial_guild_id => undefined,
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => [],
        relationships => #{},
        suppress_presence_updates => false,
        pending_presences => [],
        guild_connect_inflight => #{},
        voice_queue => queue:new(),
        voice_queue_timer => undefined,
        debounce_reactions => false,
        reaction_buffer => [],
        reaction_buffer_timer => undefined
    }.

handle_cast_forced_unavailable_guild_leave_schedules_retry_test() ->
    GuildId = 123,
    State0 = test_base_state(GuildId),
    {noreply, State1} = handle_cast({guild_leave, GuildId, forced_unavailable}, State0),
    ?assertEqual(cached_unavailable, maps:get(GuildId, maps:get(guilds, State1))),
    [LastEvent] = limited_deque:to_list(maps:get(buffer, State1)),
    LastData = assert_guild_delete_event_data(LastEvent),
    ?assertEqual(true, maps:get(<<"unavailable">>, LastData)),
    receive
        {guild_connect, GuildId, 0} -> ok
    after 100 -> ?assert(false, forced_unavailable_retry_not_scheduled)
    end.

handle_cast_forced_unavailable_hidden_guild_leave_includes_hidden_flag_test() ->
    GuildId = 124,
    State0 = test_base_state(GuildId),
    {noreply, State1} = handle_cast({guild_leave, GuildId, forced_unavailable, true}, State0),
    ?assertEqual(cached_unavailable, maps:get(GuildId, maps:get(guilds, State1))),
    [LastEvent] = limited_deque:to_list(maps:get(buffer, State1)),
    EventData = assert_guild_delete_event_data(LastEvent),
    ?assertEqual(true, maps:get(<<"unavailable">>, EventData)),
    ?assertEqual(true, maps:get(<<"unavailable_hidden">>, EventData)),
    receive
        {guild_connect, GuildId, 0} -> ok
    after 100 -> ?assert(false, forced_unavailable_hidden_retry_not_scheduled)
    end.

-spec assert_guild_delete_event_data(term()) -> map().
assert_guild_delete_event_data(Event) when is_map(Event) ->
    ?assertEqual(guild_delete, maps:get(event, Event)),
    ensure_map(maps:get(data, Event)).

ensure_map(V) when is_map(V) -> V.

handle_cast_reconnect_drain_signals_socket_test() ->
    SocketPid = spawn_msg_forwarder(self(), socket_message),
    State = #{socket_pid => SocketPid},
    {noreply, State} = handle_cast(reconnect_drain, State),
    receive
        {socket_message, session_reconnect} -> ok
    after 200 -> ?assert(false, reconnect_signal_not_received)
    end.

handle_cast_targeted_reconnect_drain_ignores_replaced_socket_test() ->
    OldSocketPid = spawn_msg_forwarder(self(), old_socket_message),
    NewSocketPid = spawn_msg_forwarder(self(), new_socket_message),
    State = #{socket_pid => NewSocketPid},
    {noreply, State} = handle_cast({reconnect_drain, OldSocketPid}, State),
    receive
        {old_socket_message, session_reconnect} -> ?assert(false, old_socket_signaled);
        {new_socket_message, session_reconnect} -> ?assert(false, new_socket_signaled)
    after 50 ->
        ok
    end.

handle_cast_targeted_reconnect_drain_signals_matching_socket_test() ->
    SocketPid = spawn_msg_forwarder(self(), socket_message),
    State = #{socket_pid => SocketPid},
    {noreply, State} = handle_cast({reconnect_drain, SocketPid}, State),
    receive
        {socket_message, session_reconnect} -> ok
    after 200 -> ?assert(false, targeted_reconnect_signal_not_received)
    end.

spawn_msg_forwarder(TargetPid, Tag) ->
    spawn(fun() -> msg_forwarder_loop(TargetPid, Tag) end).

msg_forwarder_loop(TargetPid, Tag) ->
    receive
        Msg -> TargetPid ! {Tag, Msg}
    after 30000 -> ok
    end.

handle_cast_reconnect_drain_with_missing_socket_is_noop_test() ->
    State = #{socket_pid => undefined},
    {noreply, State} = handle_cast(reconnect_drain, State),
    receive
        session_reconnect -> ?assert(false, unexpected_reconnect)
    after 50 -> ok
    end.

handle_cast_handoff_fence_signals_socket_and_stops_test() ->
    SocketPid = spawn_msg_forwarder(self(), socket_message),
    State0 = #{socket_pid => SocketPid},
    {stop, normal, State1} = handle_cast(handoff_fence, State0),
    ?assertEqual(true, maps:get(fenced, State1)),
    receive
        {socket_message, session_reconnect} -> ok
    after 200 -> ?assert(false, fence_signal_not_received)
    end.

handle_cast_handoff_fence_with_missing_socket_stops_test() ->
    State0 = #{socket_pid => undefined},
    {stop, normal, State1} = handle_cast(handoff_fence, State0),
    ?assertEqual(true, maps:get(fenced, State1)).

handle_info_ignores_stale_resume_timeout_test() ->
    CurrentToken = make_ref(),
    State = #{socket_pid => undefined, resume_timer => {CurrentToken, make_ref()}},
    {noreply, State} = handle_info({resume_timeout, make_ref()}, State).

handle_info_stops_on_current_resume_timeout_test() ->
    CurrentToken = make_ref(),
    State = #{socket_pid => undefined, resume_timer => {CurrentToken, make_ref()}},
    ?assertMatch({stop, normal, _}, handle_info({resume_timeout, CurrentToken}, State)).

handle_info_merges_gateway_timing_update_test() ->
    BaseTimings = gateway_timings:record_function(
        base,
        <<"base/0">>,
        gateway_timings:start() - 10,
        gateway_timings:new()
    ),
    UpdateTimings = gateway_timings:record_function(
        update,
        <<"update/0">>,
        gateway_timings:start() - 5,
        gateway_timings:new()
    ),
    {noreply, State1} = handle_info(
        {gateway_timing_update, UpdateTimings},
        #{gw_timings => BaseTimings}
    ),
    Timings = gateway_timings:finalize(maps:get(gw_timings, State1)),
    TraceNames = [maps:get(<<"name">>, Span) || Span <- maps:get(<<"trace">>, Timings)],
    ?assert(lists:member(<<"base/0">>, TraceNames)),
    ?assert(lists:member(<<"update/0">>, TraceNames)).

handle_presence_rejoin_check_reconnects_when_unattached_test() ->
    {noreply, State1} = handle_presence_rejoin_check(#{presence_pid => undefined}),
    ?assertEqual(undefined, maps:get(presence_pid, State1)),
    ?assert(received_presence_connect()).

handle_presence_rejoin_check_keeps_healthy_attachment_test() ->
    State0 = #{presence_pid => self(), presence_mref => undefined},
    ?assertEqual({noreply, State0}, handle_presence_rejoin_check(State0)),
    ?assertNot(received_presence_connect()).

check_ack_lag_repairs_unattached_presence_test() ->
    State0 = #{
        guilds => #{},
        guild_connect_inflight => #{},
        guild_connect_workers => #{},
        guild_connect_last_repair_at => 0,
        presence_pid => undefined
    },
    {noreply, State1} = do_handle_info_misc(check_ack_lag, State0),
    ?assertEqual(undefined, maps:get(presence_pid, State1)),
    ?assert(received_presence_connect()).

received_presence_connect() ->
    receive
        {presence_connect, 0} -> true
    after 200 -> false
    end.

-endif.
