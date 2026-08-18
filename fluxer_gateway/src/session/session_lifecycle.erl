%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_lifecycle).
-typing([eqwalizer]).

-export([
    handle_terminate_call/2,
    handle_terminate_cast/2,
    handle_reconnect_drain/1,
    handle_reconnect_drain/2,
    handle_handoff_fence/1,
    handle_call_monitor/3,
    handle_call_unmonitor/2,
    terminate/2,
    force_disconnect_dm_call/3,
    serialize_state/1,
    serialize_transfer_state/1,
    handle_token_verify/2,
    handle_heartbeat_ack/2,
    handle_resume/3,
    handle_resume_offline_timeout/2,
    handle_presence_update_cast/2,
    handle_initial_global_presences/2
]).

-export_type([session_state/0, channel_id/0, user_id/0, session_id/0, seq/0, status/0]).

-type session_state() :: session:session_state().
-type channel_id() :: session:channel_id().
-type user_id() :: session:user_id().
-type session_id() :: session:session_id().
-type seq() :: session:seq().
-type status() :: session:status().
-type voice_state_entry() :: {binary(), map()}.

-spec handle_terminate_call([binary()], session_state()) ->
    {stop, normal, terminated, session_state()} | {reply, ignored, session_state()}.
handle_terminate_call(Hashes, #{auth_session_id_hash := AuthHash} = State) ->
    case any_hash_matches(AuthHash, Hashes) of
        true -> {stop, normal, terminated, State};
        false -> {reply, ignored, State}
    end.

-spec handle_terminate_cast([binary()], session_state()) ->
    {stop, normal, session_state()} | {noreply, session_state()}.
handle_terminate_cast(Hashes, #{auth_session_id_hash := AuthHash} = State) ->
    case any_hash_matches(AuthHash, Hashes) of
        true -> {stop, normal, State};
        false -> {noreply, State}
    end.

-spec any_hash_matches(binary(), [binary()]) -> boolean().
any_hash_matches(AuthHash, Hashes) ->
    lists:any(fun(Hash) -> base64url:decode(Hash) =:= AuthHash end, Hashes).

-spec terminate(term(), session_state()) -> ok.
terminate(_Reason, State) ->
    maybe_release_transferred_resources(State),
    try_cleanup_guild_monitors(State),
    try_cleanup_call_monitors(State),
    try_cleanup_socket_monitor(State),
    ok.

-spec maybe_release_transferred_resources(session_state()) -> ok.
maybe_release_transferred_resources(#{fenced := true}) ->
    ok;
maybe_release_transferred_resources(State) ->
    try_decrement_user_sessions(State),
    try_voice_disconnect(State),
    try_cleanup_presence(State),
    ok.

-spec try_decrement_user_sessions(session_state()) -> ok.
try_decrement_user_sessions(#{user_id := UserId}) when is_integer(UserId) ->
    session_abuse_protection:decrement_user_sessions(UserId);
try_decrement_user_sessions(_) ->
    ok.

-spec try_voice_disconnect(session_state()) -> ok.
try_voice_disconnect(#{user_id := Id} = State) when is_integer(Id) ->
    try session_voice:handle_voice_disconnect(State) of
        _ -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok;
        throw:_ -> ok
    end,
    ok;
try_voice_disconnect(_) ->
    ok.

-spec try_cleanup_presence(session_state()) -> ok.
try_cleanup_presence(#{presence_mref := Ref}) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    ok;
try_cleanup_presence(_) ->
    ok.

-spec try_cleanup_guild_monitors(session_state()) -> ok.
try_cleanup_guild_monitors(#{guilds := Guilds}) when is_map(Guilds) ->
    flush_monitor_map(Guilds);
try_cleanup_guild_monitors(_) ->
    ok.

-spec try_cleanup_call_monitors(session_state()) -> ok.
try_cleanup_call_monitors(#{calls := Calls}) when is_map(Calls) ->
    flush_monitor_map(Calls);
try_cleanup_call_monitors(_) ->
    ok.

-spec flush_monitor_map(map()) -> ok.
flush_monitor_map(Entries) ->
    maps:foreach(
        fun
            (_Key, {_Pid, Ref}) when is_reference(Ref) ->
                erlang:demonitor(Ref, [flush]);
            (_Key, _Other) ->
                ok
        end,
        Entries
    ),
    ok.

-spec try_cleanup_socket_monitor(session_state()) -> ok.
try_cleanup_socket_monitor(#{socket_mref := Ref}) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    ok;
try_cleanup_socket_monitor(_) ->
    ok.

-spec handle_reconnect_drain(session_state()) -> {noreply, session_state()}.
handle_reconnect_drain(#{socket_pid := Pid} = State) when is_pid(Pid) ->
    Pid ! session_reconnect,
    {noreply, State};
handle_reconnect_drain(State) ->
    {noreply, State}.

-spec handle_reconnect_drain(pid() | undefined, session_state()) -> {noreply, session_state()}.
handle_reconnect_drain(Pid, #{socket_pid := SocketPid} = State) when
    is_pid(Pid), Pid =:= SocketPid
->
    Pid ! session_reconnect,
    {noreply, State};
handle_reconnect_drain(_ExpectedSocketPid, State) ->
    {noreply, State}.

-spec handle_handoff_fence(session_state()) -> {stop, normal, session_state()}.
handle_handoff_fence(#{socket_pid := Pid} = State) when is_pid(Pid) ->
    Pid ! session_reconnect,
    {stop, normal, State#{fenced => true}};
handle_handoff_fence(State) ->
    {stop, normal, State#{fenced => true}}.

-spec handle_call_monitor(channel_id(), pid(), session_state()) -> {noreply, session_state()}.
handle_call_monitor(ChannelId, CallPid, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        undefined ->
            Ref = monitor(process, CallPid),
            {noreply, State#{calls => Calls#{ChannelId => {CallPid, Ref}}}};
        {OldPid, OldRef} when OldPid =/= CallPid ->
            demonitor(OldRef, [flush]),
            Ref = monitor(process, CallPid),
            {noreply, State#{calls => Calls#{ChannelId => {CallPid, Ref}}}};
        _ ->
            {noreply, State}
    end.

-spec handle_call_unmonitor(channel_id(), session_state()) -> {noreply, session_state()}.
handle_call_unmonitor(ChannelId, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {_Pid, Ref} ->
            demonitor(Ref, [flush]),
            {noreply, State#{calls => maps:remove(ChannelId, Calls)}};
        undefined ->
            {noreply, State}
    end.

-spec force_disconnect_dm_call(channel_id(), binary() | undefined, session_state()) ->
    session_state().
force_disconnect_dm_call(ChannelId, ConnectionId, State) ->
    #{user_id := UserId, id := SessionId} = State,
    EffConnId = resolve_dm_connection_id(ChannelId, ConnectionId, UserId, State),
    gen_server:cast(self(), {call_unmonitor, ChannelId}),
    execute_dm_disconnect(EffConnId, UserId, SessionId, State).

-spec execute_dm_disconnect(binary() | undefined, user_id(), session_id(), session_state()) ->
    session_state().
execute_dm_disconnect(undefined, _, _, State) ->
    State;
execute_dm_disconnect(ConnId, UserId, SessionId, State) ->
    Request = dm_disconnect_request(ConnId, UserId, SessionId, State),
    StateWithPid = State#{session_pid => self()},
    case dm_voice:voice_state_update(Request, StateWithPid) of
        {reply, #{success := true}, NewState} -> maps:remove(session_pid, NewState);
        _ -> fallback_dm_disconnect(UserId, StateWithPid)
    end.

-spec dm_disconnect_request(binary(), user_id(), session_id(), session_state()) -> map().
dm_disconnect_request(ConnId, UserId, SessionId, State) ->
    #{
        user_id => UserId,
        channel_id => null,
        session_id => SessionId,
        connection_id => ConnId,
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        viewer_stream_keys => [],
        is_mobile => false,
        latitude => null,
        longitude => null,
        e2ee_capable => maps:get(e2ee_capable, State, false),
        bot => maps:get(bot, State, false)
    }.

-spec fallback_dm_disconnect(user_id(), session_state()) -> session_state().
fallback_dm_disconnect(UserId, StateWithPid) ->
    {reply, #{success := true}, FbState} = dm_voice:disconnect_voice_user(UserId, StateWithPid),
    maps:remove(session_pid, FbState).

-spec resolve_dm_connection_id(channel_id(), binary() | undefined, user_id(), session_state()) ->
    binary() | undefined.
resolve_dm_connection_id(_, ConnectionId, _, _) when is_binary(ConnectionId) -> ConnectionId;
resolve_dm_connection_id(ChannelId, _, UserId, State) ->
    VoiceStates =
        case maps:get(dm_voice_states, State, #{}) of
            Value when is_map(Value) -> Value;
            _ -> #{}
        end,
    UserIdBin = integer_to_binary(UserId),
    ChannelIdBin = integer_to_binary(ChannelId),
    find_connection_id(UserIdBin, ChannelIdBin, voice_state_entries(VoiceStates)).

-spec voice_state_entries(map()) -> [voice_state_entry()].
voice_state_entries(VoiceStates) ->
    maps:fold(
        fun
            (ConnId, VS, Acc) when is_binary(ConnId), is_map(VS) ->
                [{ConnId, VS} | Acc];
            (_ConnId, _VS, Acc) ->
                Acc
        end,
        [],
        VoiceStates
    ).

-spec find_connection_id(binary(), binary(), [voice_state_entry()]) -> binary() | undefined.
find_connection_id(_, _, []) ->
    undefined;
find_connection_id(UBin, CBin, [{ConnId, VS} | Rest]) ->
    case {maps:get(<<"user_id">>, VS, undefined), maps:get(<<"channel_id">>, VS, undefined)} of
        {UBin, CBin} -> ConnId;
        _ -> find_connection_id(UBin, CBin, Rest)
    end.

-spec handle_token_verify(binary(), session_state()) ->
    {reply, boolean(), session_state()}.
handle_token_verify(Token, #{token_hash := TokenHash} = State) ->
    HashedInput = utils:hash_token(Token),
    {reply, token_hash_matches(HashedInput, TokenHash), State}.

-spec token_hash_matches(binary(), binary()) -> boolean().
token_hash_matches(HashedInput, TokenHash) when
    byte_size(HashedInput) =:= byte_size(TokenHash)
->
    crypto:hash_equals(HashedInput, TokenHash);
token_hash_matches(_HashedInput, _TokenHash) ->
    false.

-spec handle_heartbeat_ack(seq(), session_state()) ->
    {reply, boolean(), session_state()}.
handle_heartbeat_ack(Seq, #{ack_seq := AckSeq} = State) when Seq < AckSeq ->
    {reply, true, State};
handle_heartbeat_ack(Seq, #{buffer := Buffer} = State) ->
    NewBuffer = drop_acked_buffer(Seq, Buffer),
    NewBytes =
        case is_list(NewBuffer) of
            true -> session_init:replay_buffer_bytes(NewBuffer);
            false -> limited_deque:bytes(NewBuffer)
        end,
    NewState0 = State#{ack_seq => Seq, buffer => NewBuffer, buffer_bytes => NewBytes},
    NewState = session_connection_guild:repair_stalled_guild_connects(NewState0),
    {reply, true, NewState}.

-spec drop_acked_buffer(
    seq(), eqwalizer:dynamic(limited_deque:deque() | [map()])
) -> eqwalizer:dynamic(limited_deque:deque() | [map()]).
drop_acked_buffer(Seq, Buffer) when is_list(Buffer) ->
    events_after_seq(Seq, Buffer);
drop_acked_buffer(Seq, Buffer) ->
    limited_deque:drop_while_front(fun(E) -> buffer_event_acked(Seq, E) end, Buffer).

-spec buffer_event_acked(seq(), term()) -> boolean().
buffer_event_acked(Seq, Event) when is_map(Event) ->
    maps:get(seq, Event) =< Seq;
buffer_event_acked(_Seq, _Event) ->
    false.

-spec handle_resume(seq(), pid(), session_state()) ->
    {reply, invalid_seq | {ok, [map()], seq()}, session_state()}.
handle_resume(Seq, _SocketPid, #{seq := CurrentSeq} = State) when Seq > CurrentSeq ->
    {reply, invalid_seq, State};
handle_resume(Seq, _SocketPid, #{ack_seq := AckSeq} = State) when
    is_integer(AckSeq), Seq < AckSeq
->
    {reply, invalid_seq, State};
handle_resume(Seq, SocketPid, #{seq := CurrentSeq} = State) ->
    #{buffer := Buffer, id := SessionId, status := Status, afk := Afk, mobile := Mobile} =
        State,
    ResumeStatus = status_on_resume(State, Status),
    BufferList =
        case is_list(Buffer) of
            true -> Buffer;
            false -> limited_deque:to_list(Buffer)
        end,
    MissedEvents = events_after_seq(Seq, BufferList),
    NewState0 = cancel_offline_timer(cancel_resume_timer(State)),
    NewState1 = replace_socket(SocketPid, NewState0),
    NewState = NewState1#{status => ResumeStatus, resume_status => ResumeStatus},
    NewState2 = ensure_presence_attached_on_resume(
        NewState, SessionId, ResumeStatus, Afk, Mobile
    ),
    {reply, {ok, MissedEvents, CurrentSeq}, NewState2}.

-spec ensure_presence_attached_on_resume(
    session_state(), session_id(), status(), boolean(), boolean()
) -> session_state().
ensure_presence_attached_on_resume(State, SessionId, Status, Afk, Mobile) ->
    case session_connection_presence:presence_attachment_healthy(State) of
        true ->
            notify_presence_on_resume(State, SessionId, Status, Afk, Mobile),
            State;
        false ->
            session_connection_presence:force_presence_reconnect(State)
    end.

-spec status_on_resume(session_state(), status()) -> status().
status_on_resume(_State, Status) when Status =/= offline ->
    Status;
status_on_resume(State, offline) ->
    case maps:get(resume_status, State, online) of
        offline -> online;
        Status -> Status
    end.

-spec replace_socket(pid(), session_state()) -> session_state().
replace_socket(SocketPid, State) ->
    maybe_signal_replaced_socket(SocketPid, State),
    maybe_demonitor_socket(State),
    State#{socket_pid => SocketPid, socket_mref => monitor(process, SocketPid)}.

-spec maybe_signal_replaced_socket(pid(), session_state()) -> ok.
maybe_signal_replaced_socket(SocketPid, #{socket_pid := SocketPid}) ->
    ok;
maybe_signal_replaced_socket(_SocketPid, #{socket_pid := OldSocketPid}) when
    is_pid(OldSocketPid)
->
    OldSocketPid ! session_reconnect,
    ok;
maybe_signal_replaced_socket(_SocketPid, _State) ->
    ok.

-spec maybe_demonitor_socket(session_state()) -> ok.
maybe_demonitor_socket(#{socket_mref := Ref}) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    ok;
maybe_demonitor_socket(_State) ->
    ok.

-spec events_after_seq(seq(), [term()]) -> [map()].
events_after_seq(Seq, Events) ->
    [Event || Event <- Events, is_map(Event), maps:get(seq, Event) > Seq].

-spec cancel_resume_timer(session_state()) -> session_state().
cancel_resume_timer(State) ->
    case maps:get(resume_timer, State, undefined) of
        {_Token, TimerRef} ->
            _ = erlang:cancel_timer(TimerRef),
            State#{resume_timer => undefined};
        undefined ->
            State
    end.

-spec cancel_offline_timer(session_state()) -> session_state().
cancel_offline_timer(State) ->
    case maps:get(offline_timer, State, undefined) of
        {_Token, TimerRef} ->
            _ = erlang:cancel_timer(TimerRef),
            State#{offline_timer => undefined};
        undefined ->
            State
    end.

-spec handle_resume_offline_timeout(term(), session_state()) -> {noreply, session_state()}.
handle_resume_offline_timeout(
    {resume_offline_timeout, Token}, #{socket_pid := undefined} = State
) ->
    case maps:get(offline_timer, State, undefined) of
        {Token, _TimerRef} ->
            {noreply, OfflineState} = handle_presence_update_cast(
                #{status => offline}, State#{offline_timer => undefined}
            ),
            {noreply, OfflineState};
        _ ->
            {noreply, State}
    end;
handle_resume_offline_timeout({resume_offline_timeout, _Token}, State) ->
    {noreply, State};
handle_resume_offline_timeout(_Msg, State) ->
    {noreply, State}.

-spec notify_presence_on_resume(session_state(), session_id(), status(), boolean(), boolean()) ->
    ok.
notify_presence_on_resume(#{presence_pid := undefined}, _Sid, _St, _Afk, _Mob) ->
    ok;
notify_presence_on_resume(#{presence_pid := Pid} = State, SessionId, Status, Afk, Mobile) ->
    Request = #{
        session_id => SessionId,
        session_pid => self(),
        socket_pid => maps:get(socket_pid, State, undefined),
        status => Status,
        afk => Afk,
        mobile => Mobile
    },
    spawn(fun() -> notify_presence_on_resume_worker(Pid, Request) end),
    ok.

-spec notify_presence_on_resume_worker(pid(), map()) -> ok.
notify_presence_on_resume_worker(Pid, Request) ->
    try
        gen_server:call(Pid, {session_connect, Request}, 10000),
        ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec handle_presence_update_cast(map(), session_state()) -> {noreply, session_state()}.
handle_presence_update_cast(Update, State) ->
    #{id := SessionId, status := Status, afk := Afk, mobile := Mobile} = State,
    NewStatus = maps:get(status, Update, Status),
    NewAfk = maps:get(afk, Update, Afk),
    NewMobile = maps:get(mobile, Update, Mobile),
    NewState = maybe_update_resume_status(
        NewStatus, State#{status => NewStatus, afk => NewAfk, mobile => NewMobile}
    ),
    send_presence_update(State, SessionId, NewStatus, NewAfk, NewMobile, Update),
    {noreply, NewState}.

-spec maybe_update_resume_status(status(), session_state()) -> session_state().
maybe_update_resume_status(offline, State) ->
    State;
maybe_update_resume_status(Status, State) ->
    State#{resume_status => Status}.

-spec send_presence_update(
    session_state(), session_id(), status(), boolean(), boolean(), map()
) ->
    ok.
send_presence_update(#{presence_pid := undefined}, _Sid, _St, _Afk, _Mob, _Upd) ->
    ok;
send_presence_update(#{presence_pid := Pid}, SessionId, NewStatus, NewAfk, NewMobile, Update) ->
    BaseMsg = #{
        session_id => SessionId, status => NewStatus, afk => NewAfk, mobile => NewMobile
    },
    Msg =
        case maps:find(<<"custom_status">>, Update) of
            {ok, CS} -> BaseMsg#{<<"custom_status">> => CS};
            error -> BaseMsg
        end,
    gen_server:cast(Pid, {presence_update, Msg}),
    ok.

-spec handle_initial_global_presences([map()], session_state()) -> {noreply, session_state()}.
handle_initial_global_presences(Presences, State) ->
    NewState = lists:foldl(
        fun(Presence, AccState) ->
            {noreply, Updated} = session_dispatch:handle_dispatch(
                presence_update, Presence, AccState
            ),
            Updated
        end,
        State,
        Presences
    ),
    {noreply, NewState}.

-spec serialize_state(session_state()) -> map().
serialize_state(State) ->
    #{
        id => maps:get(id, State),
        session_id => maps:get(id, State),
        user_id => integer_to_binary(maps:get(user_id, State)),
        user_data => maps:get(user_data, State),
        version => maps:get(version, State),
        seq => maps:get(seq, State),
        ack_seq => maps:get(ack_seq, State),
        properties => maps:get(properties, State),
        status => maps:get(status, State),
        resume_status => maps:get(resume_status, State, maps:get(status, State)),
        afk => maps:get(afk, State),
        mobile => maps:get(mobile, State),
        buffer => maps:get(buffer, State),
        ready => maps:get(ready, State),
        bot => maps:get(bot, State, false),
        shard => maps:get(shard, State, undefined),
        e2ee_capable => maps:get(e2ee_capable, State, false),
        guilds => maps:get(guilds, State, #{}),
        active_guilds => maps:get(active_guilds, State, sets:new()),
        collected_guild_states => maps:get(collected_guild_states, State),
        collected_sessions => maps:get(collected_sessions, State),
        collected_presences => maps:get(collected_presences, State, []),
        guild_subscription_state => maps:get(guild_subscription_state, State, #{})
    }.

-spec serialize_transfer_state(session_state()) -> map().
serialize_transfer_state(State) ->
    maps:merge(serialize_transfer_identity(State), serialize_transfer_runtime(State)).

-spec serialize_transfer_identity(session_state()) -> map().
serialize_transfer_identity(State) ->
    #{
        id => maps:get(id, State),
        user_id => maps:get(user_id, State),
        user_data => maps:get(user_data, State),
        custom_status => maps:get(custom_status, State, null),
        version => maps:get(version, State),
        token_hash => maps:get(token_hash, State),
        auth_session_id_hash => maps:get(auth_session_id_hash, State),
        properties => maps:get(properties, State),
        status => maps:get(status, State),
        resume_status => maps:get(resume_status, State, maps:get(status, State)),
        afk => maps:get(afk, State),
        mobile => maps:get(mobile, State),
        socket_pid => undefined,
        guilds => session_init:normalize_guild_ids(maps:keys(maps:get(guilds, State, #{}))),
        ready => maps:get(ready, State),
        bot => maps:get(bot, State, false),
        shard => maps:get(shard, State, undefined),
        e2ee_capable => maps:get(e2ee_capable, State, false),
        ignored_events => maps:keys(maps:get(ignored_events, State, #{})),
        initial_guild_id => maps:get(initial_guild_id, State, undefined),
        active_guilds => maps:get(active_guilds, State, sets:new()),
        debounce_reactions => maps:get(debounce_reactions, State, false)
    }.

-spec serialize_transfer_runtime(session_state()) -> map().
serialize_transfer_runtime(State) ->
    #{
        channels => maps:get(channels, State, #{}),
        relationships => maps:get(relationships, State, #{}),
        seq => maps:get(seq, State, 0),
        ack_seq => maps:get(ack_seq, State, 0),
        buffer => maps:get(buffer, State, []),
        collected_guild_states => maps:get(collected_guild_states, State, []),
        collected_sessions => maps:get(collected_sessions, State, []),
        collected_presences => maps:get(collected_presences, State, []),
        guild_subscription_state => maps:get(guild_subscription_state, State, #{})
    }.
