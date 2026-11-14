%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_subscriptions).
-typing([eqwalizer]).

-export([
    update/3,
    handle_tick/2,
    handle_drain/1,
    clear_on_passive/1,
    access_gained/4,
    member_lists_changed/2,
    resync_after_load/1,
    presence_changed/2,
    member_changed/2,
    handle_list_flush/1,
    refresh_presence_users/1,
    export_handoff/1,
    restore_handoff/1,
    list_sync_counts/0,
    init_counters/0
]).

-define(MAX_MEMBER_LISTS, 10).
-define(MAX_LIST_MEMBERS, 1000).
-define(MAX_PRESENCE_USERS, 1000).
-define(SYNC_JITTER_MS, 5000).
-define(SYNC_RATE, 20).
-define(LIST_FLUSH_MS, 250).
-define(COUNTS_KEY, {?MODULE, list_sync}).

-type guild_state() :: map().

-export_type([guild_state/0]).

-spec update(binary(), map(), guild_state()) -> guild_state().
update(SessionId, Subscriptions, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        Session when is_map(Session) ->
            update_viewer(
                guild_thread_gate:session_viewer(Session),
                SessionId,
                Session,
                Subscriptions,
                State
            );
        _ ->
            State
    end.

-spec update_viewer(boolean(), binary(), map(), map(), guild_state()) -> guild_state().
update_viewer(true, SessionId, Session, Subscriptions, State) ->
    apply_update(SessionId, Session, Subscriptions, State);
update_viewer(false, _SessionId, _Session, _Subscriptions, State) ->
    State.

-spec apply_update(binary(), map(), map(), guild_state()) -> guild_state().
apply_update(SessionId, Session0, Subscriptions, State) ->
    {Session1, SyncNeeded} = apply_threads_flag(
        maps:get(threads, Subscriptions, undefined), Session0
    ),
    {Session2, NewLists} = apply_member_lists(
        maps:get(member_lists, Subscriptions, undefined), Session1, State
    ),
    Sessions = maps:get(sessions, State, #{}),
    State1 = refresh_presence_users(
        maps:get(thread_member_lists, Session0, []) =/=
            maps:get(thread_member_lists, Session2, []),
        State#{sessions => Sessions#{SessionId => Session2}}
    ),
    lists:foreach(fun(ThreadId) -> send_member_list(Session2, ThreadId, State1) end, NewLists),
    maybe_schedule_sync(SyncNeeded, SessionId, State1).

-spec apply_threads_flag(term(), map()) -> {map(), boolean()}.
apply_threads_flag(true, Session) ->
    {Session#{thread_subscribed => true}, not thread_subscribed(Session)};
apply_threads_flag(false, Session) ->
    {maps:remove(thread_subscribed, Session), false};
apply_threads_flag(_Other, Session) ->
    {Session, false}.

-spec thread_subscribed(map()) -> boolean().
thread_subscribed(#{thread_subscribed := true}) -> true;
thread_subscribed(_Session) -> false.

-spec apply_member_lists(term(), map(), guild_state()) -> {map(), [integer()]}.
apply_member_lists(Ids, Session, State) when is_list(Ids) ->
    Viewable = lists:sublist(
        [Id || Id <- lists:usort(Ids), is_integer(Id), viewable_thread(Session, Id, State)],
        ?MAX_MEMBER_LISTS
    ),
    Updated =
        case Viewable of
            [] -> maps:remove(thread_member_lists, Session);
            _ -> Session#{thread_member_lists => Viewable}
        end,
    {Updated, Viewable};
apply_member_lists(_Other, Session, _State) ->
    {Session, []}.

-spec viewable_thread(map(), integer(), guild_state()) -> boolean().
viewable_thread(Session, ThreadId, State) ->
    case guild_thread_gate:thread(ThreadId, State) of
        undefined -> false;
        Thread -> session_can_view(Session, Thread, State)
    end.

-spec session_can_view(map(), map(), guild_state()) -> boolean().
session_can_view(#{user_id := UserId}, Thread, State) when is_integer(UserId) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined -> false;
        Member -> guild_thread_view:can_view(UserId, Thread, Member, State)
    end;
session_can_view(_Session, _Thread, _State) ->
    false.

-spec schedule_sync(binary(), guild_state()) -> guild_state().
schedule_sync(SessionId, State) ->
    Pending = maps:get(thread_sync_pending, State, #{}),
    case maps:is_key(SessionId, Pending) of
        true ->
            State;
        false ->
            _ = erlang:send_after(
                rand:uniform(?SYNC_JITTER_MS) - 1, self(), {thread_list_sync_tick, SessionId}
            ),
            State#{thread_sync_pending => Pending#{SessionId => scheduled}}
    end.

-spec maybe_schedule_sync(boolean(), binary(), guild_state()) -> guild_state().
maybe_schedule_sync(true, SessionId, State) -> schedule_sync(SessionId, State);
maybe_schedule_sync(false, _SessionId, State) -> State.

-spec export_handoff(guild_state()) -> map().
export_handoff(State) ->
    maps:merge(
        case State of
            #{thread_sync_pending := Pending} -> #{thread_handoff_syncs => maps:keys(Pending)};
            _ -> #{}
        end,
        case State of
            #{thread_list_dirty := Dirty} -> #{thread_handoff_lists => maps:keys(Dirty)};
            _ -> #{}
        end
    ).

-spec restore_handoff(guild_state()) -> guild_state().
restore_handoff(State0) ->
    State = maps:without([thread_handoff_syncs, thread_handoff_lists], State0),
    State1 =
        case State0 of
            #{thread_handoff_syncs := SessionIds} when is_list(SessionIds) ->
                lists:foldl(
                    fun schedule_sync/2, State, [Sid || Sid <- SessionIds, is_binary(Sid)]
                );
            _ ->
                State
        end,
    case [Id || Id <- maps:get(thread_handoff_lists, State0, []), is_integer(Id)] of
        [] -> State1;
        ThreadIds -> mark_dirty(ThreadIds, State1#{thread_list_refresh => true})
    end.

-spec handle_tick(term(), guild_state()) -> guild_state().
handle_tick(SessionId, State) when is_binary(SessionId) ->
    Pending = maps:get(thread_sync_pending, State, #{}),
    case maps:get(SessionId, Pending, undefined) of
        queued ->
            drain(State);
        _ ->
            Queue = maps:get(thread_sync_queue, State, queue:new()),
            drain(State#{
                thread_sync_pending => Pending#{SessionId => queued},
                thread_sync_queue => queue:in(SessionId, Queue)
            })
    end;
handle_tick(_SessionId, State) ->
    State.

-spec handle_drain(guild_state()) -> guild_state().
handle_drain(State) ->
    drain(maps:remove(thread_sync_timer, State)).

-spec drain(guild_state()) -> guild_state().
drain(State) ->
    Queue = maps:get(thread_sync_queue, State, queue:new()),
    case queue:out(Queue) of
        {empty, _} ->
            maps:remove(thread_sync_queue, State);
        {{value, SessionId}, Rest} ->
            drain_session(SessionId, Rest, State)
    end.

-spec drain_session(binary(), queue:queue(), guild_state()) -> guild_state().
drain_session(SessionId, Rest, State) ->
    Now = erlang:monotonic_time(millisecond),
    case take_token(Now, maps:get(thread_sync_bucket, State, {?SYNC_RATE, Now})) of
        {ok, Bucket} ->
            send_full_sync(SessionId, State),
            Pending = maps:remove(SessionId, maps:get(thread_sync_pending, State, #{})),
            drain(
                put_pending(Pending, State#{
                    thread_sync_bucket => Bucket, thread_sync_queue => Rest
                })
            );
        {wait, {Tokens, _} = Bucket} ->
            arm_drain(ceil((1 - Tokens) * 1000 / ?SYNC_RATE), State#{
                thread_sync_bucket => Bucket
            })
    end.

-spec put_pending(map(), guild_state()) -> guild_state().
put_pending(Pending, State) when map_size(Pending) =:= 0 ->
    maps:remove(thread_sync_pending, State);
put_pending(Pending, State) ->
    State#{thread_sync_pending => Pending}.

-spec arm_drain(integer(), guild_state()) -> guild_state().
arm_drain(_DelayMs, #{thread_sync_timer := true} = State) ->
    State;
arm_drain(DelayMs, State) ->
    _ = erlang:send_after(max(1, DelayMs), self(), thread_list_sync_drain),
    State#{thread_sync_timer => true}.

-spec take_token(integer(), {number(), integer()}) ->
    {ok, {number(), integer()}} | {wait, {number(), integer()}}.
take_token(Now, {Tokens0, Last}) ->
    Tokens = min(?SYNC_RATE, Tokens0 + (Now - Last) * ?SYNC_RATE / 1000),
    case Tokens >= 1 of
        true -> {ok, {Tokens - 1, Now}};
        false -> {wait, {Tokens, Now}}
    end.

-spec send_full_sync(binary(), guild_state()) -> ok.
send_full_sync(SessionId, State) ->
    case maps:get(SessionId, maps:get(sessions, State, #{}), undefined) of
        #{thread_viewer := true, pid := Pid, user_id := UserId} = Session when
            is_pid(Pid)
        ->
            send_available_sync(Pid, UserId, Session, State);
        _ ->
            ok
    end.

-spec send_available_sync(pid(), integer(), map(), guild_state()) -> ok.
send_available_sync(Pid, UserId, Session, State) ->
    case guild_availability:is_guild_unavailable_for_user(UserId, State) of
        true ->
            ok;
        false ->
            send_list_sync(Pid, UserId, sync_scope(Session), State),
            count(full)
    end.

-spec sync_scope(map()) -> all | joined.
sync_scope(#{thread_subscribed := true}) -> all;
sync_scope(#{bot := true}) -> all;
sync_scope(_Session) -> joined.

-spec resync_after_load(guild_state()) -> guild_state().
resync_after_load(State) ->
    Tab = guild_thread_gate:store(State),
    State1 = refresh_presence_users(State),
    maps:fold(
        fun(SessionId, Session, Acc) ->
            maybe_schedule_sync(needs_resync(Session, Tab), SessionId, Acc)
        end,
        mark_dirty(maps:keys(maps:get(thread_list_sessions, State1, #{})), State1),
        maps:get(sessions, State, #{})
    ).

-spec needs_resync(term(), ets:table() | undefined) -> boolean().
needs_resync(_Session, undefined) ->
    false;
needs_resync(#{thread_viewer := true, pending_connect := true}, _Tab) ->
    false;
needs_resync(#{thread_viewer := true, user_id := UserId} = Session, Tab) ->
    sync_scope(Session) =:= all orelse
        guild_thread_store:user_thread_ids(Tab, eqwalizer:dynamic_cast(UserId)) =/= [];
needs_resync(_Session, _Tab) ->
    false.

-spec send_list_sync(pid(), integer(), [integer()] | all | joined, guild_state()) -> ok.
send_list_sync(Pid, UserId, Parents, #{id := GuildId} = State) ->
    Member = guild_permissions_check:find_member_by_user_id(UserId, State),
    Payload = guild_thread_view:list_sync(GuildId, UserId, Parents, Member, State),
    gateway_dispatch_relay:dispatch(Pid, thread_list_sync, Payload, GuildId).

-spec clear_on_passive(map()) -> map().
clear_on_passive(Session) ->
    maps:remove(thread_subscribed, Session).

-spec access_gained(map(), [integer()], guild_state(), integer()) -> ok.
access_gained(Session, AddedChannelIds, State, _GuildId) ->
    case guild_thread_gate:session_viewer(Session) andalso guild_thread_gate:store(State) of
        false ->
            ok;
        undefined ->
            ok;
        Tab ->
            Parents = [
                P
             || P <- AddedChannelIds, guild_thread_store:parent_thread_ids(Tab, P) =/= []
            ],
            send_access_sync(Parents, Session, State)
    end.

-spec send_access_sync([integer()], map(), guild_state()) -> ok.
send_access_sync([], _Session, _State) ->
    ok;
send_access_sync(Parents, #{pid := Pid, user_id := UserId}, State) when is_pid(Pid) ->
    count(access),
    send_list_sync(Pid, UserId, Parents, State);
send_access_sync(_Parents, _Session, _State) ->
    ok.

-spec member_lists_changed(integer(), guild_state()) -> guild_state().
member_lists_changed(ThreadId, #{thread_list_sessions := Index} = State) when
    is_map_key(ThreadId, Index)
->
    mark_dirty([ThreadId], State#{thread_list_refresh => true});
member_lists_changed(_ThreadId, State) ->
    State.

-spec presence_changed(integer(), guild_state()) -> guild_state().
presence_changed(UserId, #{thread_presence_users := Users} = State) ->
    mark_dirty(maps:get(UserId, Users, []), State);
presence_changed(_UserId, State) ->
    State.

-spec member_changed(integer(), guild_state()) -> guild_state().
member_changed(UserId, State) ->
    presence_changed(UserId, State).

-spec mark_dirty([integer()], guild_state()) -> guild_state().
mark_dirty([], State) ->
    State;
mark_dirty(ThreadIds, State) ->
    Dirty = maps:get(thread_list_dirty, State, #{}),
    arm_list_flush(State#{
        thread_list_dirty => maps:merge(Dirty, maps:from_keys(ThreadIds, true))
    }).

-spec arm_list_flush(guild_state()) -> guild_state().
arm_list_flush(#{thread_list_flush := true} = State) ->
    State;
arm_list_flush(State) ->
    _ = erlang:send_after(?LIST_FLUSH_MS, self(), thread_member_list_flush),
    State#{thread_list_flush => true}.

-spec handle_list_flush(guild_state()) -> guild_state().
handle_list_flush(State0) ->
    case guild_thread_load:loading(State0) of
        true -> maps:remove(thread_list_flush, State0);
        false -> flush_member_lists(State0)
    end.

-spec flush_member_lists(guild_state()) -> guild_state().
flush_member_lists(State0) ->
    Dirty = maps:keys(maps:get(thread_list_dirty, State0, #{})),
    State = refresh_presence_users(
        maps:is_key(thread_list_refresh, State0) orelse stale_list_sessions(Dirty, State0),
        maps:without([thread_list_dirty, thread_list_flush, thread_list_refresh], State0)
    ),
    ok = send_member_lists(Dirty, State),
    State.

-spec stale_list_sessions([integer()], guild_state()) -> boolean().
stale_list_sessions(ThreadIds, State) ->
    Index = maps:get(thread_list_sessions, State, #{}),
    Sessions = maps:get(sessions, State, #{}),
    lists:any(
        fun(ThreadId) -> stale_thread_sessions(ThreadId, Index, Sessions) end,
        ThreadIds
    ).

-spec stale_thread_sessions(integer(), #{integer() => [binary()]}, map()) -> boolean().
stale_thread_sessions(ThreadId, Index, Sessions) ->
    lists:any(
        fun(Sid) -> not lists_thread(ThreadId, maps:get(Sid, Sessions, undefined)) end,
        maps:get(ThreadId, Index, [])
    ).

-spec lists_thread(integer(), term()) -> boolean().
lists_thread(ThreadId, #{thread_member_lists := Lists}) ->
    lists:member(ThreadId, eqwalizer:dynamic_cast(Lists));
lists_thread(_ThreadId, _Session) ->
    false.

-spec refresh_presence_users(guild_state()) -> guild_state().
refresh_presence_users(State) ->
    refresh_presence_users(true, State).

-spec refresh_presence_users(boolean(), guild_state()) -> guild_state().
refresh_presence_users(false, State) ->
    State;
refresh_presence_users(true, State) ->
    ThreadIds = lists:usort(
        lists:append([
            Lists
         || #{thread_member_lists := Lists} <- maps:values(maps:get(sessions, State, #{}))
        ])
    ),
    case {ThreadIds, guild_thread_gate:store(State)} of
        {[_ | _], Tab} when Tab =/= undefined ->
            State#{
                thread_presence_users => presence_users(ThreadIds, Tab),
                thread_list_sessions => list_sessions(maps:get(sessions, State, #{}))
            };
        _ ->
            maps:without([thread_presence_users, thread_list_sessions], State)
    end.

-spec list_sessions(map()) -> #{integer() => [binary()]}.
list_sessions(Sessions) ->
    maps:fold(
        fun
            (SessionId, #{thread_member_lists := Lists}, Acc) ->
                index_session(SessionId, Lists, Acc);
            (_SessionId, _Session, Acc) ->
                Acc
        end,
        #{},
        Sessions
    ).

-spec index_session(binary(), [integer()], #{integer() => [binary()]}) ->
    #{integer() => [binary()]}.
index_session(SessionId, Lists, Index) ->
    lists:foldl(
        fun(ThreadId, Acc) -> Acc#{ThreadId => [SessionId | maps:get(ThreadId, Acc, [])]} end,
        Index,
        Lists
    ).

-spec presence_users([integer()], ets:table()) -> #{integer() => [integer()]}.
presence_users(ThreadIds, Tab) ->
    lists:foldl(
        fun(ThreadId, Acc) -> add_thread_presence(ThreadId, Tab, Acc) end, #{}, ThreadIds
    ).

-spec add_thread_presence(integer(), ets:table(), #{integer() => [integer()]}) ->
    #{integer() => [integer()]}.
add_thread_presence(ThreadId, Tab, Users) ->
    lists:foldl(
        fun(UserId, Acc) -> add_presence_user(ThreadId, UserId, Acc) end,
        Users,
        lists:sublist(guild_thread_store:member_ids(Tab, ThreadId), ?MAX_LIST_MEMBERS)
    ).

-spec add_presence_user(integer(), integer(), #{integer() => [integer()]}) ->
    #{integer() => [integer()]}.
add_presence_user(ThreadId, UserId, Users) ->
    case maps:find(UserId, Users) of
        {ok, Threads} ->
            Users#{UserId => [ThreadId | Threads]};
        error when map_size(Users) < ?MAX_PRESENCE_USERS ->
            Users#{UserId => [ThreadId]};
        error ->
            Users
    end.

-spec send_member_lists([integer()], guild_state()) -> ok.
send_member_lists([], _State) ->
    ok;
send_member_lists(ThreadIds, #{id := GuildId} = State) ->
    Index = maps:get(thread_list_sessions, State, #{}),
    Sessions = maps:get(sessions, State, #{}),
    Ctx = guild_member_list_connected:presence_context(State),
    lists:foreach(
        fun(ThreadId) ->
            Pids = list_pids(ThreadId, maps:get(ThreadId, Index, []), Sessions, State),
            send_thread_member_list(Pids, GuildId, ThreadId, Ctx, State)
        end,
        ThreadIds
    );
send_member_lists(_ThreadIds, _State) ->
    ok.

-spec send_thread_member_list([pid()], integer(), integer(), map(), guild_state()) -> ok.
send_thread_member_list([], _GuildId, _ThreadId, _Ctx, _State) ->
    ok;
send_thread_member_list(Pids, GuildId, ThreadId, Ctx, State) ->
    gateway_dispatch_relay:dispatch_many(
        Pids,
        thread_member_list_update,
        member_list_payload(GuildId, ThreadId, Ctx, State),
        GuildId
    ).

-spec list_pids(integer(), [binary()], map(), guild_state()) -> [pid()].
list_pids(ThreadId, SessionIds, Sessions, State) ->
    case guild_thread_gate:thread(ThreadId, State) of
        undefined ->
            [];
        Thread ->
            viewer_pids(ThreadId, Thread, SessionIds, Sessions, State)
    end.

-spec viewer_pids(integer(), map(), [binary()], map(), guild_state()) -> [pid()].
viewer_pids(ThreadId, Thread, SessionIds, Sessions, State) ->
    {Pids, _Memo} = lists:foldl(
        fun(Sid, {Acc, Memo}) ->
            list_pid(ThreadId, Thread, maps:get(Sid, Sessions, undefined), State, Acc, Memo)
        end,
        {[], #{}},
        SessionIds
    ),
    Pids.

-spec list_pid(integer(), map(), term(), guild_state(), [pid()], map()) -> {[pid()], map()}.
list_pid(
    ThreadId,
    Thread,
    #{pid := Pid, thread_viewer := true, thread_member_lists := Lists} = Session,
    State,
    Acc,
    Memo
) when is_pid(Pid) ->
    case lists:member(ThreadId, eqwalizer:dynamic_cast(Lists)) of
        true -> add_viewer_pid(Pid, Session, Thread, State, Acc, Memo);
        false -> {Acc, Memo}
    end;
list_pid(_ThreadId, _Thread, _Session, _State, Acc, Memo) ->
    {Acc, Memo}.

-spec add_viewer_pid(pid(), map(), map(), guild_state(), [pid()], map()) -> {[pid()], map()}.
add_viewer_pid(Pid, Session, Thread, State, Acc, Memo) ->
    case guild_thread_gate:thread_view_access(Session, Thread, State, Memo) of
        {true, Memo1} -> {[Pid | Acc], Memo1};
        {false, Memo1} -> {Acc, Memo1}
    end.

-spec send_member_list(map(), integer(), guild_state()) -> ok.
send_member_list(#{pid := Pid, thread_viewer := true}, ThreadId, #{id := GuildId} = State) when
    is_pid(Pid)
->
    Ctx = guild_member_list_connected:presence_context(State),
    gateway_dispatch_relay:dispatch(
        Pid,
        thread_member_list_update,
        member_list_payload(GuildId, ThreadId, Ctx, State),
        GuildId
    );
send_member_list(_Session, _ThreadId, _State) ->
    ok.

-spec member_list_payload(integer(), integer(), map(), guild_state()) -> map().
member_list_payload(GuildId, ThreadId, Ctx, State) ->
    Members =
        case guild_thread_gate:store(State) of
            undefined -> [];
            Tab -> lists:sublist(guild_thread_store:members(Tab, ThreadId), ?MAX_LIST_MEMBERS)
        end,
    #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"thread_id">> => integer_to_binary(ThreadId),
        <<"members">> => [list_member(UserId, M, Ctx, State) || {UserId, M} <- Members]
    }.

-spec list_member(integer(), map(), map(), guild_state()) -> map().
list_member(UserId, ThreadMember, Ctx, State) ->
    #{<<"presence">> := Presence} = guild_member_list_connected:add_presence_to_member(
        #{}, UserId, Ctx
    ),
    GuildMember =
        case guild_permissions_check:find_member_by_user_id(UserId, State) of
            undefined -> null;
            M -> M
        end,
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"join_timestamp">> => maps:get(<<"join_timestamp">>, ThreadMember, null),
        <<"flags">> => maps:get(<<"flags">>, ThreadMember, 0),
        <<"member">> => GuildMember,
        <<"presence">> => Presence
    }.

-spec count(full | access) -> ok.
count(Reason) ->
    guild_thread_store:add_counter(?COUNTS_KEY, reason_index(Reason), 1).

-spec init_counters() -> ok.
init_counters() ->
    guild_thread_store:ensure_counter(?COUNTS_KEY, 2).

-spec reason_index(full | access) -> 1 | 2.
reason_index(full) -> 1;
reason_index(access) -> 2.

-spec list_sync_counts() -> #{full | access => non_neg_integer()}.
list_sync_counts() ->
    case persistent_term:get(?COUNTS_KEY, undefined) of
        undefined -> #{};
        Counters -> #{R => counters:get(Counters, reason_index(R)) || R <- [full, access]}
    end.
