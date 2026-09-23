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
    end_or_hold_session(enrolled_in_push_delivery(State), State);
handle_socket_down(_Reason, State) ->
    hold_session_for_resume(State).

-spec end_or_hold_session(boolean(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
end_or_hold_session(true, State) ->
    {stop, normal, State#{socket_pid => undefined, socket_mref => undefined}};
end_or_hold_session(false, State) ->
    hold_session_for_resume(State).

-spec enrolled_in_push_delivery(session_state()) -> boolean().
enrolled_in_push_delivery(State) ->
    case maps:get(user_id, State, undefined) of
        UserId when is_integer(UserId) -> push_delivery_config:is_enrolled(UserId);
        _ -> false
    end.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

find_by_ref_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),
    Map = #{
        123 => {self(), Ref1},
        456 => {self(), Ref2},
        789 => undefined
    },
    ?assertEqual({ok, 123}, find_by_ref(Ref1, Map)),
    ?assertEqual({ok, 456}, find_by_ref(Ref2, Map)),
    ?assertEqual(not_found, find_by_ref(Ref3, Map)),
    ok.

find_guild_by_ref_test() ->
    Ref = make_ref(),
    Guilds = #{100 => {self(), Ref}, 200 => undefined},
    ?assertEqual({ok, 100}, find_guild_by_ref(Ref, Guilds)),
    ?assertEqual(not_found, find_guild_by_ref(make_ref(), Guilds)),
    ok.

find_call_by_ref_test() ->
    Ref = make_ref(),
    Calls = #{300 => {self(), Ref}},
    ?assertEqual({ok, 300}, find_call_by_ref(Ref, Calls)),
    ?assertEqual(not_found, find_call_by_ref(make_ref(), Calls)),
    ok.

build_test_base_state() ->
    #{
        id => <<"session-monitor-test">>,
        user_id => 1,
        user_data => #{},
        custom_status => null,
        version => 1,
        token_hash => <<>>,
        auth_session_id_hash => <<>>,
        buffer => limited_deque:new(4096, 16777216),
        seq => 0,
        ack_seq => 0,
        properties => #{},
        status => online,
        resume_status => online,
        afk => false,
        mobile => false
    }.

build_test_connection_state() ->
    #{
        presence_pid => undefined,
        presence_mref => undefined,
        socket_pid => undefined,
        socket_mref => undefined,
        resume_timer => undefined,
        offline_timer => undefined
    }.

build_test_guild_state(GuildId, Guilds) ->
    #{
        guilds => Guilds,
        calls => #{},
        channels => #{},
        ready => undefined,
        bot => false,
        ignored_events => #{},
        initial_guild_id => GuildId,
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => [],
        relationships => #{},
        suppress_presence_updates => false,
        pending_presences => [],
        guild_connect_inflight => #{}
    }.

build_test_voice_state() ->
    #{
        voice_queue => queue:new(),
        voice_queue_timer => undefined,
        debounce_reactions => false,
        reaction_buffer => [],
        reaction_buffer_timer => undefined
    }.

-spec build_test_session_state(
    guild_id(), #{guild_id() => guild_ref()}
) -> session_state().
build_test_session_state(GuildId, Guilds) ->
    maps:merge(
        maps:merge(build_test_base_state(), build_test_connection_state()),
        maps:merge(build_test_guild_state(GuildId, Guilds), build_test_voice_state())
    ).

handle_socket_down_delays_session_offline_test() ->
    SocketRef = make_ref(),
    SocketPid = spawn_test_proc(),
    State0 = (build_test_session_state(50000, #{}))#{
        presence_pid => self(),
        socket_pid => SocketPid,
        socket_mref => SocketRef
    },
    {noreply, State1} = handle_process_down(SocketRef, normal, State0),
    ?assertEqual(online, maps:get(status, State1)),
    ?assertEqual(online, maps:get(resume_status, State1)),
    ?assertEqual(undefined, maps:get(socket_pid, State1)),
    ?assertEqual(undefined, maps:get(socket_mref, State1)),
    {_ResumeToken, ResumeTimerRef} = maps:get(resume_timer, State1),
    {OfflineToken, OfflineTimerRef} = maps:get(offline_timer, State1),
    erlang:cancel_timer(ResumeTimerRef),
    erlang:cancel_timer(OfflineTimerRef),
    {noreply, State2} = session_lifecycle:handle_resume_offline_timeout(
        {resume_offline_timeout, OfflineToken}, State1
    ),
    ?assertEqual(offline, maps:get(status, State2)),
    ?assertEqual(online, maps:get(resume_status, State2)),
    receive
        {'$gen_cast', {presence_update, PresenceUpdate}} ->
            ?assertEqual(<<"session-monitor-test">>, maps:get(session_id, PresenceUpdate)),
            ?assertEqual(offline, maps:get(status, PresenceUpdate)),
            ?assertEqual(false, maps:get(afk, PresenceUpdate)),
            ?assertEqual(false, maps:get(mobile, PresenceUpdate))
    after 200 ->
        ?assert(false)
    end,
    SocketPid ! stop.

spawn_test_proc() ->
    spawn(fun test_proc_loop/0).

test_proc_loop() ->
    receive
        stop -> ok
    after 30000 -> ok
    end.

run_guild_down_unavailable_test(GuildId, Reason) ->
    GuildRef = make_ref(),
    GuildPid = spawn_test_proc(),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, S1} = handle_guild_down(GuildId, Reason, State0, Guilds),
    assert_guild_undefined(GuildId, S1),
    assert_single_guild_delete_event(GuildId, S1),
    assert_receive_guild_connect(GuildId),
    GuildPid ! stop.

handle_guild_down_normal_marks_unavailable_and_schedules_reconnect_test() ->
    run_guild_down_unavailable_test(50001, normal).

handle_guild_down_shutdown_marks_unavailable_and_schedules_reconnect_test() ->
    run_guild_down_unavailable_test(50002, shutdown).

handle_guild_down_crash_marks_unavailable_and_schedules_reconnect_test() ->
    run_guild_down_unavailable_test(50003, {error, something_went_wrong}).

handle_guild_down_handoff_reconnects_without_unavailable_dispatch_test() ->
    GuildId = 50004,
    GuildRef = make_ref(),
    GuildPid = spawn_test_proc(),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, S1} = handle_guild_down(GuildId, {shutdown, handoff}, State0, Guilds),
    assert_guild_undefined(GuildId, S1),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, S1))),
    assert_receive_guild_connect(GuildId),
    GuildPid ! stop.

handle_guild_down_flapping_dedups_reconnect_timer_test() ->
    GuildId = 50010,
    GuildRef = make_ref(),
    GuildPid = spawn_test_proc(),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, S1} = handle_guild_down(GuildId, normal, State0, Guilds),
    Timers1 = guild_reconnect_timers(S1),
    FirstTimer = maps:get(GuildId, Timers1),
    {noreply, S2} = handle_guild_down(GuildId, normal, S1, maps:get(guilds, S1)),
    Timers2 = guild_reconnect_timers(S2),
    SecondTimer = maps:get(GuildId, Timers2),
    ?assertNotEqual(FirstTimer, SecondTimer),
    ?assertEqual(false, erlang:read_timer(FirstTimer)),
    erlang:cancel_timer(SecondTimer),
    GuildPid ! stop.

handle_call_down_flapping_dedups_reconnect_timer_test() ->
    ChannelId = 60010,
    CallRef = make_ref(),
    CallPid = spawn_test_proc(),
    Calls = #{ChannelId => {CallPid, CallRef}},
    State0 = (build_test_session_state(50011, #{}))#{calls => Calls},
    {noreply, S1} = handle_call_down(ChannelId, normal, State0, Calls),
    Timers1 = call_reconnect_timers(S1),
    FirstTimer = maps:get(ChannelId, Timers1),
    {noreply, S2} = handle_call_down(ChannelId, normal, S1, maps:get(calls, S1)),
    Timers2 = call_reconnect_timers(S2),
    SecondTimer = maps:get(ChannelId, Timers2),
    ?assertNotEqual(FirstTimer, SecondTimer),
    ?assertEqual(false, erlang:read_timer(FirstTimer)),
    erlang:cancel_timer(SecondTimer),
    CallPid ! stop.

handle_guild_down_killed_sends_permanent_guild_leave_test() ->
    GuildId = 50005,
    GuildRef = make_ref(),
    GuildPid = spawn_test_proc(),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, State1} = handle_guild_down(GuildId, killed, State0, Guilds),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))),
    UpdatedGuilds = maps:get(guilds, State1),
    ?assertEqual({GuildPid, GuildRef}, maps:get(GuildId, UpdatedGuilds)),
    assert_no_guild_connect(GuildId),
    receive
        {'$gen_cast', {guild_leave, GuildId}} -> ok
    after 200 ->
        ?assert(false)
    end,
    GuildPid ! stop.

handle_call_down_handoff_reconnects_without_unavailable_dispatch_test() ->
    ChannelId = 60004,
    CallRef = make_ref(),
    CallPid = spawn_test_proc(),
    Calls = #{ChannelId => {CallPid, CallRef}},
    State0 = (build_test_session_state(50007, #{}))#{calls => Calls},
    {noreply, State1} = handle_call_down(ChannelId, {shutdown, handoff}, State0, Calls),
    UpdatedCalls = maps:get(calls, State1),
    ?assertEqual(undefined, maps:get(ChannelId, UpdatedCalls)),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))),
    receive
        {call_reconnect, ChannelId, 0} -> ok
    after 200 ->
        ?assert(false)
    end,
    CallPid ! stop.

handle_process_down_guild_normal_exit_dispatches_unavailable_test() ->
    GuildId = 50006,
    GuildPid = spawn_test_proc(),
    GuildRef = monitor(process, GuildPid),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    GuildPid ! stop,
    Reason = receive_down(GuildRef, GuildPid),
    {noreply, State1} = handle_process_down(GuildRef, Reason, State0),
    assert_single_guild_delete_event(GuildId, State1),
    assert_receive_guild_connect(GuildId).

assert_guild_undefined(GuildId, State) ->
    UpdatedGuilds = maps:get(guilds, State),
    ?assertEqual(undefined, maps:get(GuildId, UpdatedGuilds)).

assert_single_guild_delete_event(GuildId, State) ->
    Buffer = limited_deque:to_list(maps:get(buffer, State)),
    ?assertEqual(1, length(Buffer)),
    [Event] = Buffer,
    case Event of
        EventRecord when is_map(EventRecord) ->
            ?assertEqual(guild_delete, maps:get(event, EventRecord)),
            EventData = maps:get(data, EventRecord),
            assert_guild_delete_data(GuildId, EventData);
        _ ->
            ?assert(false)
    end.

assert_guild_delete_data(GuildId, EventData) when is_map(EventData) ->
    ?assertEqual(integer_to_binary(GuildId), maps:get(<<"id">>, EventData)),
    ?assertEqual(true, maps:get(<<"unavailable">>, EventData));
assert_guild_delete_data(_GuildId, _EventData) ->
    ?assert(false).

assert_receive_guild_connect(GuildId) ->
    receive
        {guild_connect, GuildId, 0} -> ok
    after 2000 ->
        ?assert(false)
    end.

assert_no_guild_connect(GuildId) ->
    receive
        {guild_connect, GuildId, 0} -> ?assert(false)
    after 200 ->
        ok
    end.

receive_down(Ref, Pid) ->
    receive
        {'DOWN', Ref, process, Pid, Reason} -> Reason
    after 2000 ->
        error(timeout_waiting_for_down)
    end.

-endif.
