%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_connect_async).
-typing([eqwalizer]).

-export([
    enqueue_session_connect_async/5,
    finalize_session_connect_async/5,
    maybe_start_session_connect_workers/1,
    decrement_session_connect_inflight/1,
    ensure_session_connect_queue/1
]).

-type session_id() :: binary().
-type guild_id() :: integer().
-export_type([session_id/0]).

-define(SESSION_CONNECT_MAX_WORKERS, 8).
-define(SESSION_CONNECT_DEFAULT_MAX_QUEUE, 1024).
-define(CONNECT_SNAPSHOT_HEAVY_MEMBER_KEYS, [
    <<"members">>, members_normalized, <<"member_role_index">>, members_sorted_ids
]).

-spec ensure_session_connect_queue(term()) -> queue:queue().
ensure_session_connect_queue(Value) when is_list(Value) ->
    queue:from_list(Value);
ensure_session_connect_queue(Value) ->
    safe_queue_cast(Value).

-spec safe_queue_cast(term()) -> queue:queue().
safe_queue_cast({R, F} = _Value) when is_list(R), is_list(F) ->
    queue:from_list(F ++ lists:reverse(R));
safe_queue_cast(_) ->
    queue:new().

-spec enqueue_session_connect_async(integer(), non_neg_integer(), map(), map(), map()) -> map().
enqueue_session_connect_async(GuildId, Attempt, Request, Msg, State) ->
    SessionId = maps:get(session_id, Request, undefined),
    enqueue_session_connect_async_for_session(GuildId, Attempt, Request, Msg, SessionId, State).

-spec enqueue_session_connect_async_for_session(
    integer(), non_neg_integer(), map(), map(), term(), map()
) -> map().
enqueue_session_connect_async_for_session(
    _GuildId, _Attempt, _Request, _Msg, SessionId, State
) when
    not is_binary(SessionId)
->
    State;
enqueue_session_connect_async_for_session(GuildId, Attempt, Request, Msg, SessionId, State) ->
    Pending0 = maps:get(session_connect_pending, State, #{}),
    PrevAttempt = maps:get(SessionId, Pending0, undefined),
    case should_enqueue(SessionId, Attempt, PrevAttempt) of
        skip ->
            State;
        {enqueue, supersede} ->
            Queue0 = ensure_session_connect_queue(
                maps:get(session_connect_queue, State, queue:new())
            ),
            Filtered = drop_queued(SessionId, Queue0),
            do_enqueue(GuildId, Attempt, Request, Msg, SessionId, Pending0, Filtered, State);
        {enqueue, fresh} ->
            Queue0 = ensure_session_connect_queue(
                maps:get(session_connect_queue, State, queue:new())
            ),
            do_enqueue(GuildId, Attempt, Request, Msg, SessionId, Pending0, Queue0, State)
    end.

-spec finalize_session_connect_async(
    session_id() | undefined,
    non_neg_integer(),
    {ok, map()} | {ok_unavailable, map()} | {error, term()},
    map(),
    map()
) -> map().
finalize_session_connect_async(undefined, _Attempt, _Result0, _Computed, State) ->
    maybe_start_session_connect_workers(decrement_session_connect_inflight(State));
finalize_session_connect_async(SessionId, Attempt, Result0, Computed, State) ->
    State1 = decrement_session_connect_inflight(State),
    Pending0 = maps:get(session_connect_pending, State1, #{}),
    case maps:find(SessionId, Pending0) of
        {ok, Attempt} ->
            State2 = State1#{session_connect_pending => maps:remove(SessionId, Pending0)},
            finalize_matched(SessionId, Attempt, Result0, Computed, State2);
        _ ->
            maybe_start_session_connect_workers(State1)
    end.

-spec finalize_matched(session_id(), non_neg_integer(), term(), map(), map()) -> map().
finalize_matched(SessionId, Attempt, Result0, Computed, State) ->
    Request = maps:get(request, Computed, #{}),
    SessionPid = maps:get(session_pid, Request, undefined),
    case is_pid(SessionPid) of
        false ->
            maybe_start_session_connect_workers(State);
        true ->
            finalize_with_pid(SessionId, Attempt, Result0, Computed, Request, SessionPid, State)
    end.

-spec finalize_with_pid(session_id(), non_neg_integer(), term(), map(), map(), pid(), map()) ->
    map().
finalize_with_pid(SessionId, Attempt, Result0, Computed, Request, SessionPid, State) ->
    case resolve_guild_id(State, Computed) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            finalize_resolved(
                GuildId, SessionId, Attempt, Result0, Computed, Request, SessionPid, State
            );
        _ ->
            maybe_start_session_connect_workers(State)
    end.

-spec finalize_resolved(
    integer(), session_id(), non_neg_integer(), term(), map(), map(), pid(), map()
) -> map().
finalize_resolved(
    GuildId, SessionId, Attempt, {error, _} = Result0, _Computed, _Request, SessionPid, State
) ->
    State1 = discard_pending_session(SessionId, State),
    send_result(GuildId, Attempt, Result0, SessionPid),
    maybe_start_session_connect_workers(State1);
finalize_resolved(GuildId, SessionId, Attempt, Result0, Computed, Request, SessionPid, State) ->
    State1 = upsert_session(SessionId, SessionPid, Request, Computed, State),
    UserId = maps:get(user_id, Request, undefined),
    State2 = guild_sessions_connect:resection_connected_user(UserId, State, State1),
    send_result(GuildId, Attempt, Result0, SessionPid),
    maybe_start_session_connect_workers(State2).

-spec discard_pending_session(session_id() | undefined, map()) -> map().
discard_pending_session(SessionId, State) when is_binary(SessionId) ->
    Sessions0 = maps:get(sessions, State, #{}),
    case maps:find(SessionId, Sessions0) of
        {ok, #{pending_connect := true} = Entry} ->
            demonitor_pending_session(Entry),
            State#{sessions => maps:remove(SessionId, Sessions0)};
        _ ->
            State
    end;
discard_pending_session(_SessionId, State) ->
    State.

-spec demonitor_pending_session(map()) -> ok.
demonitor_pending_session(#{mref := MRef}) when is_reference(MRef) ->
    demonitor(MRef, [flush]),
    ok;
demonitor_pending_session(_) ->
    ok.

-spec maybe_start_session_connect_workers(map()) -> map().
maybe_start_session_connect_workers(State) ->
    Inflight0 = maps:get(session_connect_inflight, State, 0),
    Queue0 = ensure_session_connect_queue(
        maps:get(session_connect_queue, State, queue:new())
    ),
    case Inflight0 < ?SESSION_CONNECT_MAX_WORKERS of
        false ->
            State;
        true ->
            dequeue_and_start(Queue0, Inflight0, State)
    end.

-spec dequeue_and_start(queue:queue(), non_neg_integer(), map()) -> map().
dequeue_and_start(Queue0, Inflight0, State) ->
    case queue:out(Queue0) of
        {{value, Next}, Rest} when is_map(Next) ->
            State1 = State#{
                session_connect_queue => Rest,
                session_connect_inflight => Inflight0 + 1
            },
            maybe_start_session_connect_workers(start_worker(Next, State1));
        {{value, _Next}, Rest} ->
            maybe_start_session_connect_workers(
                State#{session_connect_queue => Rest}
            );
        {empty, _} ->
            State#{session_connect_queue => Queue0}
    end.

-spec decrement_session_connect_inflight(map()) -> map().
decrement_session_connect_inflight(State) ->
    Inflight0 = maps:get(session_connect_inflight, State, 0),
    State#{session_connect_inflight => erlang:max(0, Inflight0 - 1)}.

-spec should_enqueue(term(), non_neg_integer(), non_neg_integer() | undefined) ->
    skip | {enqueue, supersede} | {enqueue, fresh}.
should_enqueue(Sid, _Attempt, _Prev) when not is_binary(Sid) -> skip;
should_enqueue(_Sid, Attempt, Prev) when is_integer(Prev), Attempt =< Prev -> skip;
should_enqueue(_Sid, _Attempt, Prev) when is_integer(Prev) -> {enqueue, supersede};
should_enqueue(_Sid, _Attempt, undefined) -> {enqueue, fresh}.

-spec do_enqueue(
    integer(), non_neg_integer(), map(), map(), binary(), map(), queue:queue(), map()
) -> map().
do_enqueue(GuildId, Attempt, Request, Msg, SessionId, Pending0, Queue0, State) ->
    Item = #{
        guild_id => GuildId,
        attempt => Attempt,
        request => Request,
        reply_via_pid => maps:get(reply_via_pid, Msg, undefined)
    },
    State1 = State#{
        session_connect_pending => Pending0#{SessionId => Attempt},
        session_connect_queue => queue:in(Item, Queue0)
    },
    State2 = maybe_start_session_connect_workers(ensure_pending_session_entry(Request, State1)),
    trim_session_connect_queue(State2).

-spec trim_session_connect_queue(map()) -> map().
trim_session_connect_queue(State) ->
    Queue0 = ensure_session_connect_queue(
        maps:get(session_connect_queue, State, queue:new())
    ),
    MaxQueue = session_connect_max_queue(State),
    trim_session_connect_queue(queue:len(Queue0), MaxQueue, Queue0, State).

-spec trim_session_connect_queue(non_neg_integer(), non_neg_integer(), queue:queue(), map()) ->
    map().
trim_session_connect_queue(Queued, MaxQueue, Queue, State) when Queued =< MaxQueue ->
    State#{session_connect_queue => Queue};
trim_session_connect_queue(Queued, MaxQueue, Queue0, State) ->
    case queue:out(Queue0) of
        {{value, Dropped}, Queue1} ->
            State1 = drop_overflowed_session_connect(Dropped, Queued, MaxQueue, State),
            trim_session_connect_queue(Queued - 1, MaxQueue, Queue1, State1);
        {empty, _} ->
            State#{session_connect_queue => queue:new()}
    end.

-spec session_connect_max_queue(map()) -> non_neg_integer().
session_connect_max_queue(State) ->
    case maps:get(session_connect_max_queue, State, undefined) of
        MaxQueue when is_integer(MaxQueue), MaxQueue >= 0 ->
            MaxQueue;
        _ ->
            get_non_neg_int_or_default(
                session_connect_max_queue, ?SESSION_CONNECT_DEFAULT_MAX_QUEUE
            )
    end.

-spec get_non_neg_int_or_default(atom(), non_neg_integer()) -> non_neg_integer().
get_non_neg_int_or_default(Key, Default) ->
    case fluxer_gateway_env:get_optional(Key) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> Default
    end.

-spec drop_overflowed_session_connect(term(), non_neg_integer(), non_neg_integer(), map()) ->
    map().
drop_overflowed_session_connect(Item, Queued, MaxQueue, State) ->
    log_dropped_session_connect(Item, Queued, MaxQueue),
    notify_dropped_session_connect(Item),
    cleanup_dropped_session_connect(Item, State).

-spec log_dropped_session_connect(term(), non_neg_integer(), non_neg_integer()) -> ok.
log_dropped_session_connect(Item, Queued, MaxQueue) ->
    logger:warning(
        "guild_session_connect_queue_full: guild_id=~p session_id=~p"
        " attempt=~p queued=~p max_queue=~p",
        [
            queued_guild_id(Item),
            queued_session_id(Item),
            queued_attempt(Item),
            Queued,
            MaxQueue
        ]
    ),
    ok.

-spec notify_dropped_session_connect(term()) -> ok.
notify_dropped_session_connect(Item) ->
    case {queued_guild_id(Item), queued_attempt(Item), queued_session_pid(Item)} of
        {GuildId, Attempt, SessionPid} when
            is_integer(GuildId), is_integer(Attempt), is_pid(SessionPid)
        ->
            SessionPid ! {guild_connect_result, GuildId, Attempt, {error, overloaded}},
            ok;
        _ ->
            ok
    end.

-spec cleanup_dropped_session_connect(term(), map()) -> map().
cleanup_dropped_session_connect(Item, State) ->
    case queued_session_id(Item) of
        SessionId when is_binary(SessionId) ->
            State1 = remove_pending_session_connect(SessionId, State),
            discard_pending_session(SessionId, State1);
        _ ->
            State
    end.

-spec remove_pending_session_connect(binary(), map()) -> map().
remove_pending_session_connect(SessionId, State) ->
    Pending0 = maps:get(session_connect_pending, State, #{}),
    State#{session_connect_pending => maps:remove(SessionId, Pending0)}.

-spec ensure_pending_session_entry(map(), map()) -> map().
ensure_pending_session_entry(Request, State) ->
    Sid = maps:get(session_id, Request, undefined),
    Uid = maps:get(user_id, Request, undefined),
    Pid = maps:get(session_pid, Request, undefined),
    case {Sid, Uid, Pid} of
        {S, U, P} when is_binary(S), is_integer(U), is_pid(P) ->
            upsert_pending_session(S, U, P, Request, State);
        _ ->
            State
    end.

-spec upsert_pending_session(binary(), integer(), pid(), map(), map()) -> map().
upsert_pending_session(S, U, P, Request, State) ->
    Sessions0 = maps:get(sessions, State, #{}),
    case maps:find(S, Sessions0) of
        error ->
            Entry = #{
                session_id => S,
                user_id => U,
                pid => P,
                mref => monitor(process, P),
                active_guilds => maps:get(active_guilds, Request, sets:new()),
                bot => maps:get(bot, Request, false),
                is_staff => maps:get(is_staff, Request, false),
                pending_connect => true,
                viewable_channels => #{}
            },
            State#{sessions => Sessions0#{S => Entry}};
        {ok, Existing} ->
            State#{sessions => Sessions0#{S => Existing#{pending_connect => true}}}
    end.

-spec drop_queued(session_id(), queue:queue()) -> queue:queue().
drop_queued(SessionId, Queue) ->
    queue:filter(
        fun(Item) ->
            queued_session_id(Item) =/= SessionId
        end,
        Queue
    ).

-spec queued_session_id(term()) -> session_id() | undefined.
queued_session_id(#{request := Request}) when is_map(Request) ->
    case maps:get(session_id, Request, undefined) of
        SessionId when is_binary(SessionId) -> SessionId;
        _ -> undefined
    end;
queued_session_id(_) ->
    undefined.

-spec queued_guild_id(term()) -> guild_id() | undefined.
queued_guild_id(#{guild_id := GuildId}) when is_integer(GuildId) ->
    GuildId;
queued_guild_id(_) ->
    undefined.

-spec queued_attempt(term()) -> non_neg_integer() | undefined.
queued_attempt(#{attempt := Attempt}) when is_integer(Attempt), Attempt >= 0 ->
    Attempt;
queued_attempt(_) ->
    undefined.

-spec queued_session_pid(term()) -> pid() | undefined.
queued_session_pid(#{request := #{session_pid := SessionPid}}) when is_pid(SessionPid) ->
    SessionPid;
queued_session_pid(#{reply_via_pid := SessionPid}) when is_pid(SessionPid) ->
    SessionPid;
queued_session_pid(_) ->
    undefined.

-spec start_worker(map(), map()) -> map().
start_worker(Item, State) ->
    Self = self(),
    Snapshot = build_connect_snapshot(Item, State),
    {_Pid, Ref} = spawn_monitor(fun() -> compute_and_send_done(Item, Self, Snapshot) end),
    WorkerRefs = maps:get(session_connect_worker_refs, State, #{}),
    State#{session_connect_worker_refs => WorkerRefs#{Ref => true}}.

-spec build_connect_snapshot(map(), map()) -> map().
build_connect_snapshot(Item, State) ->
    Base = maps:with(
        [
            id,
            data,
            sessions,
            member_count,
            voice_server_pid,
            voice_states,
            member_list_engine,
            virtual_channel_access
        ],
        State
    ),
    maybe_trim_connect_snapshot(Item, Base, State).

-spec maybe_trim_connect_snapshot(map(), map(), map()) -> map().
maybe_trim_connect_snapshot(Item, Base, State) ->
    case has_members_ets(State) of
        true -> trim_connect_snapshot(Item, Base);
        false -> Base
    end.

-spec has_members_ets(map()) -> boolean().
has_members_ets(#{data := #{members_ets := Tab}}) -> is_reference(Tab);
has_members_ets(_) -> false.

-spec trim_connect_snapshot(map(), map()) -> map().
trim_connect_snapshot(Item, #{data := Data} = Base) when is_map(Data) ->
    Retained = retained_member_map(Item, Base, Data),
    Trimmed = maps:without(?CONNECT_SNAPSHOT_HEAVY_MEMBER_KEYS, Data),
    Base#{
        data => Trimmed#{
            <<"members">> => Retained,
            members_normalized => Retained,
            <<"member_role_index">> =>
                guild_data_index_members:build_member_role_index(Retained)
        }
    };
trim_connect_snapshot(_Item, Base) ->
    Base.

-spec retained_member_map(map(), map(), map()) -> #{integer() => map()}.
retained_member_map(Item, Base, Data) ->
    UserIds = [connect_user_id(Item) | voice_state_user_ids(Base)],
    lists:foldl(
        fun(UserId, Acc) -> retain_member(UserId, Data, Acc) end,
        #{},
        UserIds
    ).

-spec retain_member(term(), map(), #{integer() => map()}) -> #{integer() => map()}.
retain_member(UserId, Data, Acc) when is_integer(UserId) ->
    case guild_data_index_members:get_member_ets(UserId, Data) of
        Member when is_map(Member) -> Acc#{UserId => Member};
        _ -> Acc
    end;
retain_member(_UserId, _Data, Acc) ->
    Acc.

-spec connect_user_id(map()) -> integer() | undefined.
connect_user_id(Item) ->
    Request = maps:get(request, Item, #{}),
    case maps:get(user_id, Request, undefined) of
        UserId when is_integer(UserId) -> UserId;
        _ -> undefined
    end.

-spec voice_state_user_ids(map()) -> [integer()].
voice_state_user_ids(Base) ->
    maps:fold(
        fun(_Key, VoiceState, Acc) -> add_voice_state_user_id(VoiceState, Acc) end,
        [],
        voice_state_utils:voice_states(Base)
    ).

-spec add_voice_state_user_id(term(), [integer()]) -> [integer()].
add_voice_state_user_id(VoiceState, Acc) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        UserId when is_integer(UserId) -> [UserId | Acc];
        _ -> Acc
    end.

-spec compute_and_send_done(map(), pid(), map()) -> ok.
compute_and_send_done(Item, GuildPid, Snapshot) ->
    Request = maps:get(request, Item, #{}),
    {Result0, Computed0} = compute_connect_result(Snapshot, Item, Request),
    gen_server:cast(
        GuildPid,
        {session_connect_worker_done, maps:get(session_id, Request, undefined),
            maps:get(attempt, Item, 0), Result0, maps:merge(Item, Computed0)}
    ),
    ok.

-spec compute_connect_result(map(), map(), map()) ->
    {{ok, map()} | {ok_unavailable, map()} | {error, term()}, map()}.
compute_connect_result(Snapshot, Item, Request) ->
    case resolve_guild_id(Snapshot, Item) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            safe_compute_connect(GuildId, Request, Snapshot);
        _ ->
            {{error, missing_guild_id}, #{}}
    end.

-spec safe_compute_connect(integer(), map(), map()) ->
    {{ok, map()} | {ok_unavailable, map()} | {error, term()}, map()}.
safe_compute_connect(GuildId, Request, Snapshot) ->
    try compute_session_connect(GuildId, Request, Snapshot) of
        Tmp -> {computed_result(Tmp), Tmp}
    catch
        _:Reason ->
            {{error, {session_connect_async_failed, Reason}}, #{}}
    end.

-spec compute_session_connect(integer(), map(), map()) -> map().
compute_session_connect(GuildId, #{user_id := UserId} = Request, State) when
    is_integer(UserId)
->
    case guild_availability:is_guild_unavailable_for_user(UserId, State) of
        true ->
            #{
                unavailable_response => #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"unavailable">> => true
                }
            };
        false ->
            compute_available_connect(GuildId, UserId, Request, State)
    end;
compute_session_connect(_GuildId, _Request, _State) ->
    #{}.

-spec compute_available_connect(integer(), integer(), map(), map()) -> map().
compute_available_connect(GuildId, UserId, Request, State) ->
    case guild_data_members:find_member_by_user_id(UserId, State) of
        undefined ->
            #{not_member => true};
        _Member ->
            GS = guild_data:get_guild_state(UserId, State),
            #{
                guild_state => GS,
                initial_last_message_ids => guild_sessions:build_initial_last_message_ids(GS),
                initial_channel_versions => build_channel_versions(GS),
                viewable_channels => build_viewable_map(
                    guild_visibility:get_user_viewable_channels(UserId, State)
                ),
                user_roles => session_passive:get_user_roles_for_guild(UserId, State),
                should_mark_guild_synced =>
                    maps:get(initial_guild_id, Request, undefined) =:= GuildId
            }
    end.

-spec computed_result(map()) -> {ok, map()} | {ok_unavailable, map()} | {error, term()}.
computed_result(Computed) ->
    case maps:find(unavailable_response, Computed) of
        {ok, U} when is_map(U) -> {ok_unavailable, U};
        _ -> guild_state_or_not_member(Computed)
    end.

-spec guild_state_or_not_member(map()) -> {ok, map()} | {error, term()}.
guild_state_or_not_member(Computed) ->
    case maps:get(not_member, Computed, false) of
        true -> {error, not_member};
        _ -> extract_guild_state_result(Computed)
    end.

-spec extract_guild_state_result(map()) -> {ok, map()} | {error, term()}.
extract_guild_state_result(Computed) ->
    case maps:find(guild_state, Computed) of
        {ok, GS} when is_map(GS) -> {ok, GS};
        _ -> {error, invalid_guild_state}
    end.

-spec resolve_guild_id(map(), map()) -> integer() | undefined.
resolve_guild_id(State, Fallback) ->
    case snowflake_id:parse_optional(maps:get(id, State, undefined)) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            GuildId;
        _ ->
            snowflake_id:parse_optional(maps:get(guild_id, Fallback, undefined))
    end.

-spec upsert_session(session_id(), pid(), map(), map(), map()) -> map().
upsert_session(SessionId, SessionPid, Request, Computed, State) ->
    UserId = maps:get(user_id, Request, undefined),
    case is_integer(UserId) of
        false -> State;
        true -> upsert_session_valid(SessionId, SessionPid, UserId, Request, Computed, State)
    end.

-spec upsert_session_valid(session_id(), pid(), integer(), map(), map(), map()) -> map().
upsert_session_valid(SessionId, SessionPid, UserId, Request, Computed, State) ->
    Sessions0 = maps:get(sessions, State, #{}),
    Existing = maps:get(SessionId, Sessions0, undefined),
    {MRef, Existing1} = resolve_monitor(Existing, SessionPid),
    GuildId = require_guild_id(maps:get(id, State)),
    SessionData = build_session_data(SessionId, UserId, SessionPid, MRef, Request, Computed),
    store_passive_state(SessionId, GuildId, Computed),
    FinalSD = maybe_mark_synced(GuildId, Computed, SessionData),
    Sessions = merge_session(SessionId, FinalSD, Existing1, Sessions0),
    State1 = State#{sessions => Sessions},
    State2 = update_connected_tracking(UserId, Existing, State1),
    update_presence_subscription(UserId, Existing, State2).

-spec build_session_data(session_id(), integer(), pid(), reference(), map(), map()) -> map().
build_session_data(SessionId, UserId, SessionPid, MRef, Request, Computed) ->
    #{
        session_id => SessionId,
        user_id => UserId,
        pid => SessionPid,
        mref => MRef,
        active_guilds => maps:get(active_guilds, Request, sets:new()),
        user_roles => maps:get(user_roles, Computed, []),
        bot => maps:get(bot, Request, false),
        is_staff => maps:get(is_staff, Request, false),
        pending_connect => false,
        viewable_channels => maps:get(viewable_channels, Computed, #{})
    }.

-spec store_passive_state(session_id(), guild_id(), map()) -> ok.
store_passive_state(SessionId, GuildId, Computed) ->
    PassiveState = #{
        previous_passive_updates => maps:get(initial_last_message_ids, Computed, #{}),
        previous_passive_channel_versions => maps:get(initial_channel_versions, Computed, #{}),
        previous_passive_voice_states => #{}
    },
    passive_sync_registry:store(SessionId, GuildId, PassiveState).

-spec maybe_mark_synced(guild_id(), map(), map()) -> map().
maybe_mark_synced(GuildId, Computed, SessionData) ->
    case maps:get(should_mark_guild_synced, Computed, false) of
        true -> session_passive:mark_guild_synced(GuildId, SessionData);
        false -> SessionData
    end.

-spec merge_session(session_id(), map(), map() | undefined, map()) -> map().
merge_session(SessionId, FinalSD, undefined, Sessions0) ->
    Sessions0#{SessionId => FinalSD};
merge_session(SessionId, FinalSD, Existing, Sessions0) ->
    Sessions0#{SessionId => maps:merge(Existing, FinalSD)}.

-spec resolve_monitor(map() | undefined, pid()) -> {reference(), map() | undefined}.
resolve_monitor(undefined, SessionPid) ->
    {monitor(process, SessionPid), undefined};
resolve_monitor(Existing, SessionPid) ->
    case {maps:get(pid, Existing, undefined), maps:get(mref, Existing, undefined)} of
        {SessionPid, Ref} when is_reference(Ref) -> {Ref, Existing};
        {_OtherPid, Ref} when is_reference(Ref) ->
            demonitor(Ref, [flush]),
            {monitor(process, SessionPid), Existing};
        _ ->
            {monitor(process, SessionPid), Existing}
    end.

-spec update_connected_tracking(integer(), map() | undefined, map()) -> map().
update_connected_tracking(UserId, Existing, State) ->
    case connected_user_id_from_existing(Existing) of
        UserId ->
            State;
        undefined ->
            add_connected_user(UserId, State);
        ExistingUserId ->
            add_connected_user(UserId, remove_connected_user(ExistingUserId, State))
    end.

-spec update_presence_subscription(integer(), map() | undefined, map()) -> map().
update_presence_subscription(UserId, Existing, State) ->
    case connected_user_id_from_existing(Existing) of
        UserId ->
            State;
        undefined ->
            guild_sessions:subscribe_connected_user_presence(UserId, State);
        ExistingUserId ->
            State1 = guild_sessions:unsubscribe_from_user_presence(ExistingUserId, State),
            guild_sessions:subscribe_connected_user_presence(UserId, State1)
    end.

-spec connected_user_id_from_existing(map() | undefined) -> integer() | undefined.
connected_user_id_from_existing(undefined) ->
    undefined;
connected_user_id_from_existing(#{pending_connect := true}) ->
    undefined;
connected_user_id_from_existing(Existing) ->
    case maps:get(user_id, Existing, undefined) of
        UserId when is_integer(UserId), UserId > 0 ->
            UserId;
        _ ->
            undefined
    end.

-spec add_connected_user(integer(), map()) -> map().
add_connected_user(UserId, State) ->
    Counts = maps:get(user_session_counts, State, #{}),
    Connected = maps:get(connected_user_ids, State, sets:new()),
    State#{
        user_session_counts => Counts#{UserId => maps:get(UserId, Counts, 0) + 1},
        connected_user_ids => sets:add_element(UserId, Connected)
    }.

-spec remove_connected_user(integer(), map()) -> map().
remove_connected_user(UserId, State) ->
    Counts = maps:get(user_session_counts, State, #{}),
    Connected = maps:get(connected_user_ids, State, sets:new()),
    NewCount = max(0, maps:get(UserId, Counts, 0) - 1),
    {NewCounts, NewConnected} =
        case NewCount of
            0 -> {maps:remove(UserId, Counts), sets:del_element(UserId, Connected)};
            _ -> {Counts#{UserId => NewCount}, Connected}
        end,
    State#{user_session_counts => NewCounts, connected_user_ids => NewConnected}.

-spec build_channel_versions(map()) -> #{binary() => integer()}.
build_channel_versions(GuildState) ->
    lists:foldl(fun add_channel_version/2, #{}, channels(GuildState)).

-spec channels(map()) -> [term()].
channels(GuildState) ->
    case maps:get(<<"channels">>, GuildState, []) of
        Channels when is_list(Channels) -> Channels;
        _ -> []
    end.

-spec add_channel_version(term(), #{binary() => integer()}) -> #{binary() => integer()}.
add_channel_version(Channel, Acc) when is_map(Channel) ->
    case maps:find(<<"id">>, Channel) of
        {ok, Id} when is_binary(Id) ->
            Acc#{Id => require_integer(map_utils:get_integer(Channel, <<"version">>, 0))};
        _ ->
            Acc
    end;
add_channel_version(_Channel, Acc) ->
    Acc.

-spec require_guild_id(term()) -> guild_id().
require_guild_id(GuildId) when is_integer(GuildId) ->
    GuildId;
require_guild_id(_) ->
    error(badarg).

-spec require_integer(term()) -> integer().
require_integer(Value) when is_integer(Value) ->
    Value;
require_integer(_) ->
    error(badarg).

-spec build_viewable_map([integer()]) -> map().
build_viewable_map(Ids) ->
    maps:from_list([{Id, true} || Id <- Ids, is_integer(Id), Id > 0]).

-spec send_result(integer(), non_neg_integer(), term(), pid()) -> ok.
send_result(GuildId, Attempt, Result0, SessionPid) ->
    Reply =
        case Result0 of
            {ok, GS} -> {ok, self(), GS};
            {ok_unavailable, U} -> {ok_unavailable, self(), U};
            {error, R} -> {error, R}
        end,
    SessionPid ! {guild_connect_result, GuildId, Attempt, Reply},
    ok.
