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
-define(SYNC_RETRY_MS, 100).
-define(LIST_FLUSH_MS, 250).
-define(COUNTS_KEY, {?MODULE, list_sync}).

-type guild_state() :: map().

-spec update(binary(), map(), guild_state()) -> guild_state().
update(SessionId, Subscriptions, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        Session when is_map(Session) ->
            case guild_thread_gate:session_viewer(Session) of
                true -> apply_update(SessionId, Session, Subscriptions, State);
                false -> State
            end;
        _ ->
            State
    end.

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
    case SyncNeeded of
        true -> schedule_sync(SessionId, State1);
        false -> State1
    end.

-spec apply_threads_flag(term(), map()) -> {map(), boolean()}.
apply_threads_flag(true, Session) ->
    {Session#{thread_subscribed => true}, maps:get(thread_subscribed, Session, false) =/= true};
apply_threads_flag(false, Session) ->
    {maps:remove(thread_subscribed, Session), false};
apply_threads_flag(_Other, Session) ->
    {Session, false}.

-spec apply_member_lists(term(), map(), guild_state()) -> {map(), [integer()]}.
apply_member_lists(Ids, Session, State) when is_list(Ids) ->
    Previous = maps:get(thread_member_lists, Session, []),
    Viewable = lists:sublist(
        [Id || Id <- lists:usort(Ids), viewable_thread(Session, Id, State)], ?MAX_MEMBER_LISTS
    ),
    Updated =
        case Viewable of
            [] -> maps:remove(thread_member_lists, Session);
            _ -> Session#{thread_member_lists => Viewable}
        end,
    {Updated, Viewable -- Previous};
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
    case guild_permissions:find_member_by_user_id(UserId, State) of
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
            end
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
            case guild_availability:is_guild_unavailable_for_user(UserId, State) of
                true ->
                    ok;
                false ->
                    send_list_sync(Pid, UserId, sync_scope(Session), State),
                    count(full)
            end;
        _ ->
            ok
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
            case needs_resync(Session, Tab) of
                true -> schedule_sync(SessionId, Acc);
                false -> Acc
            end
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
    sync_scope(Session) =:= all orelse guild_thread_store:user_thread_ids(Tab, UserId) =/= [];
needs_resync(_Session, _Tab) ->
    false.

-spec send_list_sync(pid(), integer(), [integer()] | all | joined, guild_state()) -> ok.
send_list_sync(Pid, UserId, Parents, #{id := GuildId} = State) ->
    Member = guild_permissions:find_member_by_user_id(UserId, State),
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
            case {Parents, Session} of
                {[], _} ->
                    ok;
                {_, #{pid := Pid, user_id := UserId}} when is_pid(Pid) ->
                    count(access),
                    send_list_sync(Pid, UserId, Parents, State);
                _ ->
                    ok
            end
    end.

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
        fun(ThreadId) ->
            lists:any(
                fun(Sid) -> not lists_thread(ThreadId, maps:get(Sid, Sessions, undefined)) end,
                maps:get(ThreadId, Index, [])
            )
        end,
        ThreadIds
    ).

-spec lists_thread(integer(), term()) -> boolean().
lists_thread(ThreadId, #{thread_member_lists := Lists}) ->
    lists:member(ThreadId, Lists);
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
            (SessionId, #{thread_member_lists := Lists}, Acc0) ->
                lists:foldl(
                    fun(ThreadId, Acc) ->
                        Acc#{ThreadId => [SessionId | maps:get(ThreadId, Acc, [])]}
                    end,
                    Acc0,
                    Lists
                );
            (_SessionId, _Session, Acc) ->
                Acc
        end,
        #{},
        Sessions
    ).

-spec presence_users([integer()], ets:table()) -> #{integer() => [integer()]}.
presence_users(ThreadIds, Tab) ->
    lists:foldl(
        fun(ThreadId, Acc0) ->
            lists:foldl(
                fun(UserId, Acc) ->
                    case maps:find(UserId, Acc) of
                        {ok, Threads} ->
                            Acc#{UserId => [ThreadId | Threads]};
                        error when map_size(Acc) < ?MAX_PRESENCE_USERS ->
                            Acc#{UserId => [ThreadId]};
                        error ->
                            Acc
                    end
                end,
                Acc0,
                lists:sublist(guild_thread_store:member_ids(Tab, ThreadId), ?MAX_LIST_MEMBERS)
            )
        end,
        #{},
        ThreadIds
    ).

-spec send_member_lists([integer()], guild_state()) -> ok.
send_member_lists([], _State) ->
    ok;
send_member_lists(ThreadIds, #{id := GuildId} = State) ->
    Index = maps:get(thread_list_sessions, State, #{}),
    Sessions = maps:get(sessions, State, #{}),
    Ctx = guild_member_list_connected:presence_context(State),
    lists:foreach(
        fun(ThreadId) ->
            case list_pids(ThreadId, maps:get(ThreadId, Index, []), Sessions, State) of
                [] ->
                    ok;
                Pids ->
                    gateway_dispatch_relay:dispatch_many(
                        Pids,
                        thread_member_list_update,
                        member_list_payload(GuildId, ThreadId, Ctx, State),
                        GuildId
                    )
            end
        end,
        ThreadIds
    );
send_member_lists(_ThreadIds, _State) ->
    ok.

-spec list_pids(integer(), [binary()], map(), guild_state()) -> [pid()].
list_pids(ThreadId, SessionIds, Sessions, State) ->
    case guild_thread_gate:thread(ThreadId, State) of
        undefined ->
            [];
        Thread ->
            {Pids, _Memo} = lists:foldl(
                fun(Sid, {Acc, Memo}) ->
                    list_pid(
                        ThreadId, Thread, maps:get(Sid, Sessions, undefined), State, Acc, Memo
                    )
                end,
                {[], #{}},
                SessionIds
            ),
            Pids
    end.

-spec list_pid(integer(), map(), term(), guild_state(), [pid()], map()) -> {[pid()], map()}.
list_pid(
    ThreadId,
    Thread,
    #{pid := Pid, thread_viewer := true, thread_member_lists := Lists} = Session,
    State,
    Acc,
    Memo
) when is_pid(Pid) ->
    case lists:member(ThreadId, Lists) of
        true ->
            case guild_thread_gate:thread_view_access(Session, Thread, State, Memo) of
                {true, Memo1} -> {[Pid | Acc], Memo1};
                {false, Memo1} -> {Acc, Memo1}
            end;
        false ->
            {Acc, Memo}
    end;
list_pid(_ThreadId, _Thread, _Session, _State, Acc, Memo) ->
    {Acc, Memo}.

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
        case guild_data_members:find_member_by_user_id(UserId, State) of
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

token_bucket_allows_twenty_per_second_test() ->
    Now = 1000,
    Bucket = lists:foldl(
        fun(_, B) ->
            {ok, Next} = take_token(Now, B),
            Next
        end,
        {?SYNC_RATE, Now},
        lists:seq(1, ?SYNC_RATE)
    ),
    ?assertMatch({wait, _}, take_token(Now, Bucket)),
    ?assertMatch({ok, _}, take_token(Now + 50, Bucket)).

queued_syncs_share_one_drain_timer_test_() ->
    {spawn, fun queued_syncs_share_one_drain_timer/0}.

queued_syncs_share_one_drain_timer() ->
    Ids = [integer_to_binary(N) || N <- lists:seq(1, 1000)],
    Now = erlang:monotonic_time(millisecond),
    State0 = #{id => 1, sessions => #{}, thread_sync_bucket => {0, Now + 60000}},
    State1 = lists:foldl(fun schedule_sync/2, State0, Ids ++ Ids),
    ?assertEqual(1000, map_size(maps:get(thread_sync_pending, State1))),
    State2 = lists:foldl(fun handle_tick/2, State1, Ids ++ Ids),
    ?assertEqual(1000, queue:len(maps:get(thread_sync_queue, State2))),
    ?assertEqual(true, maps:get(thread_sync_timer, State2)),
    State3 = handle_drain(State2#{
        thread_sync_bucket => {?SYNC_RATE, erlang:monotonic_time(millisecond)}
    }),
    Left = queue:len(maps:get(thread_sync_queue, State3)),
    ?assert(Left =< 1000 - ?SYNC_RATE andalso Left >= 1000 - ?SYNC_RATE - 2),
    ?assertEqual(true, maps:get(thread_sync_timer, State3)),
    ?assertEqual(1, count_drains(300, 0)).

presence_changes_coalesce_into_one_flush_test_() ->
    {spawn, fun presence_changes_coalesce_into_one_flush/0}.

presence_changes_coalesce_into_one_flush() ->
    Me = self(),
    Viewer = #{
        pid => Me, user_id => 1, thread_viewer => true, viewable_channels => #{5 => true}
    },
    Sessions = #{
        <<"a">> => Viewer#{thread_member_lists => [10]},
        <<"b">> => Viewer#{user_id => 2, thread_member_lists => [10, 11]},
        <<"c">> => #{pid => Me, thread_viewer => false},
        <<"d">> => Viewer#{thread_member_lists => [12]},
        <<"e">> => Viewer#{
            user_id => 3, viewable_channels => #{}, thread_member_lists => [10, 11]
        }
    },
    State0 = #{
        id => 1,
        data => store_data([10, 11, 12]),
        sessions => Sessions,
        connected_user_ids => sets:new(),
        thread_presence_users => #{7 => [10], 8 => [10, 11]},
        thread_list_sessions => list_sessions(Sessions)
    },
    ?assertEqual(State0, presence_changed(9, State0)),
    State1 = lists:foldl(fun presence_changed/2, State0, [7, 8, 7, 8, 9]),
    ?assertEqual(#{10 => true, 11 => true}, maps:get(thread_list_dirty, State1)),
    ?assertEqual([], list_updates()),
    receive
        thread_member_list_flush -> ok
    after 1000 -> error(no_flush)
    end,
    receive
        thread_member_list_flush -> error(second_flush)
    after 300 -> ok
    end,
    State2 = handle_list_flush(State1),
    ?assertNot(maps:is_key(thread_list_dirty, State2)),
    ?assertNot(maps:is_key(thread_list_flush, State2)),
    ?assertEqual([<<"10">>, <<"10">>, <<"11">>], lists:sort(list_updates())),
    ?assertEqual(State2, member_lists_changed(99, State2)),
    ?assertMatch(
        #{
            thread_list_dirty := #{12 := true},
            thread_list_refresh := true,
            thread_list_flush := true
        },
        member_lists_changed(12, State2)
    ).

flush_drops_presence_users_of_departed_list_sessions_test_() ->
    {spawn, fun flush_drops_presence_users_of_departed_list_sessions/0}.

flush_drops_presence_users_of_departed_list_sessions() ->
    Viewer = #{
        pid => self(),
        user_id => 1,
        thread_viewer => true,
        viewable_channels => #{5 => true},
        thread_member_lists => [10]
    },
    State0 = #{
        id => 1,
        data => store_data([10]),
        sessions => #{<<"b">> => maps:without([thread_member_lists], Viewer)},
        connected_user_ids => sets:new(),
        thread_presence_users => #{7 => [10]},
        thread_list_sessions => #{10 => [<<"a">>, <<"b">>]}
    },
    State1 = presence_changed(7, State0),
    receive
        thread_member_list_flush -> ok
    after 1000 -> error(no_flush)
    end,
    State2 = handle_list_flush(State1),
    ?assertEqual([], list_updates()),
    ?assertNot(maps:is_key(thread_presence_users, State2)),
    ?assertNot(maps:is_key(thread_list_sessions, State2)),
    ?assertEqual(State2, presence_changed(7, State2)).

flush_waits_for_the_thread_load_test_() ->
    {spawn, fun flush_waits_for_the_thread_load/0}.

flush_waits_for_the_thread_load() ->
    Sessions = #{
        <<"a">> => #{
            pid => self(),
            user_id => 1,
            thread_viewer => true,
            viewable_channels => #{5 => true},
            thread_member_lists => [10]
        }
    },
    State0 = #{
        id => 1,
        data => store_data([10]),
        sessions => Sessions,
        connected_user_ids => sets:new(),
        thread_list_dirty => #{10 => true},
        thread_list_flush => true,
        thread_load => #{status => loading, queue => [], queued => 0}
    },
    State1 = handle_list_flush(State0),
    ?assertEqual([], list_updates()),
    ?assertEqual(#{10 => true}, maps:get(thread_list_dirty, State1)),
    ?assertNot(maps:is_key(thread_list_flush, State1)),
    Loaded = State1#{thread_load => #{status => ready, queue => [], queued => 0}},
    State2 = resync_after_load(maps:remove(thread_list_dirty, Loaded)),
    ?assertMatch(#{thread_list_dirty := #{10 := true}, thread_list_flush := true}, State2),
    State3 = handle_list_flush(State2),
    ?assertEqual([<<"10">>], list_updates()),
    ?assertNot(maps:is_key(thread_list_dirty, State3)).

store_data(ThreadIds) ->
    Tab = guild_thread_store:new(),
    lists:foreach(
        fun(Id) ->
            guild_thread_store:put_thread(Tab, #{
                <<"id">> => integer_to_binary(Id), <<"parent_id">> => <<"5">>, <<"type">> => 11
            })
        end,
        ThreadIds
    ),
    #{thread_store => Tab, thread_gate => #{active => true}}.

list_updates() ->
    receive
        {'$gen_cast', {dispatch, thread_member_list_update, #{<<"thread_id">> := Id}}} ->
            [Id | list_updates()]
    after 0 -> []
    end.

count_drains(Timeout, Count) ->
    receive
        thread_list_sync_drain -> count_drains(Timeout, Count + 1)
    after Timeout -> Count
    end.

-endif.
