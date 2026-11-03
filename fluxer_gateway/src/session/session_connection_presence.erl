%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_connection_presence).
-typing([eqwalizer]).

-export([
    handle_presence_connect/2,
    repair_presence_connection/1,
    presence_attachment_healthy/1,
    force_presence_reconnect/1
]).

-export_type([session_state/0, attempt/0]).

-define(MAX_RETRY_ATTEMPTS, 25).
-define(PRESENCE_CONNECT_TIMEOUT_MS, 10000).

-type session_state() :: session:session_state().
-type attempt() :: non_neg_integer().

-spec handle_presence_connect(attempt(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
handle_presence_connect(Attempt, State) ->
    Request = build_presence_request(State),
    LookupStartedAt = gateway_timings:start(),
    Result = safe_presence_lookup(Request),
    LookupRemote = presence_lookup_remote(Result),
    GwTimings = gateway_timings:record_function(
        presence_manager_lookup,
        <<"presence_manager:start_or_lookup/1">>,
        LookupStartedAt,
        #{remote => LookupRemote},
        gateway_timings:from_state(State)
    ),
    State1 = put_timings(GwTimings, State),
    case Result of
        {ok, Pid} -> do_session_connect(Pid, Attempt, State1);
        _Error -> schedule_presence_retry(Attempt, State1)
    end.

-spec put_timings(gateway_timings:recorder(), session_state()) -> session_state().
put_timings(_GwTimings, #{ready := undefined} = State) ->
    State;
put_timings(GwTimings, State) ->
    gateway_timings:put_state(GwTimings, State).

-spec presence_lookup_remote(term()) -> map() | undefined.
presence_lookup_remote({ok, PresencePid}) when is_pid(PresencePid) ->
    gateway_timings:remote_node(presence_manager, node(PresencePid));
presence_lookup_remote(_) ->
    undefined.

-spec safe_presence_lookup(map()) -> {ok, pid()} | {error, term()}.
safe_presence_lookup(Request) ->
    safe_call(fun() -> presence_manager:start_or_lookup(Request) end).

-spec build_presence_request(session_state()) -> map().
build_presence_request(State) when is_map(State) ->
    FriendIds = presence_targets:friend_ids_from_state(State),
    GroupDmRecipients = presence_targets:group_dm_recipients_from_state(State),
    #{
        user_id => maps:get(user_id, State),
        user_data => maps:get(user_data, State),
        guild_ids => session_visits:member_guild_ids(State),
        status => maps:get(status, State),
        friend_ids => FriendIds,
        group_dm_recipients => GroupDmRecipients,
        custom_status => maps:get(custom_status, State, null)
    }.

-spec do_session_connect(pid(), attempt(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
do_session_connect(Pid, Attempt, State) when is_map(State) ->
    SessionId = maps:get(id, State),
    Status = maps:get(status, State),
    Afk = maps:get(afk, State),
    Mobile = maps:get(mobile, State),
    SocketPid = maps:get(socket_pid, State, undefined),
    FriendIds = presence_targets:friend_ids_from_state(State),
    GroupDmRecipients = presence_targets:group_dm_recipients_from_state(State),
    ConnectReq = #{
        session_id => SessionId,
        status => Status,
        afk => Afk,
        mobile => Mobile,
        socket_pid => SocketPid
    },
    ConnectStartedAt = gateway_timings:start(),
    Result = safe_call(fun() ->
        gen_server:call(Pid, {session_connect, ConnectReq}, ?PRESENCE_CONNECT_TIMEOUT_MS)
    end),
    Remote = gateway_timings:remote_node(presence, node(Pid)),
    GwTimings = gateway_timings:record_function(
        presence_session_connect,
        <<"presence_session:handle_session_connect/3">>,
        ConnectStartedAt,
        #{remote => Remote},
        gateway_timings:from_state(State)
    ),
    State1 = put_timings(GwTimings, State),
    case Result of
        {ok, Sessions} ->
            gen_server:cast(Pid, {sync_friends, FriendIds}),
            gen_server:cast(Pid, {sync_group_dm_recipients, GroupDmRecipients}),
            maybe_demonitor_presence(State1),
            MRef = monitor(process, Pid),
            NewState = State1#{
                presence_pid => Pid,
                presence_mref => MRef,
                collected_sessions => Sessions
            },
            session_ready:check_readiness(NewState);
        _ ->
            schedule_presence_retry(Attempt, State1)
    end.

-spec safe_call(fun(() -> T)) -> T | {error, term()}.
safe_call(Fun) ->
    try
        Fun()
    catch
        exit:_Reason -> {error, exit};
        error:_Reason -> {error, error};
        throw:_Reason -> {error, throw}
    end.

-spec schedule_presence_retry(attempt(), session_state()) -> {noreply, session_state()}.
schedule_presence_retry(Attempt, State) when Attempt < ?MAX_RETRY_ATTEMPTS ->
    Delay = backoff_utils:calculate_with_jitter(Attempt),
    erlang:send_after(Delay, self(), {presence_connect, Attempt + 1}),
    {noreply, State};
schedule_presence_retry(_Attempt, State) ->
    {noreply, State}.

-spec repair_presence_connection(session_state()) -> session_state().
repair_presence_connection(State) ->
    case presence_attachment_healthy(State) of
        true -> State;
        false -> force_presence_reconnect(State)
    end.

-spec presence_attachment_healthy(session_state()) -> boolean().
presence_attachment_healthy(State) ->
    case maps:get(presence_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            presence_pid_alive(Pid) andalso presence_owner_matches(State, Pid);
        _ ->
            false
    end.

-spec presence_pid_alive(pid()) -> boolean().
presence_pid_alive(Pid) ->
    case node(Pid) =:= node() of
        true -> erlang:is_process_alive(Pid);
        false -> true
    end.

-spec presence_owner_matches(session_state(), pid()) -> boolean().
presence_owner_matches(State, Pid) ->
    case maps:get(user_id, State, undefined) of
        UserId when is_integer(UserId) -> owner_node_matches(UserId, Pid);
        _ -> true
    end.

-spec owner_node_matches(integer(), pid()) -> boolean().
owner_node_matches(UserId, Pid) ->
    case presence_owner_node(UserId) of
        {ok, OwnerNode} -> node(Pid) =:= OwnerNode;
        unavailable -> true
    end.

-spec presence_owner_node(integer()) -> {ok, node()} | unavailable.
presence_owner_node(UserId) ->
    try gateway_node_router:owner_node_result(UserId, presence) of
        {ok, OwnerNode} when is_atom(OwnerNode) -> {ok, OwnerNode};
        _ -> unavailable
    catch
        error:_Reason -> unavailable;
        exit:_Reason -> unavailable
    end.

-spec force_presence_reconnect(session_state()) -> session_state().
force_presence_reconnect(State) ->
    maybe_demonitor_presence(State),
    self() ! {presence_connect, 0},
    State#{presence_pid => undefined, presence_mref => undefined}.

-spec maybe_demonitor_presence(session_state()) -> ok.
maybe_demonitor_presence(State) ->
    case maps:get(presence_mref, State, undefined) of
        Ref when is_reference(Ref) ->
            erlang:demonitor(Ref, [flush]),
            ok;
        _ ->
            ok
    end.
