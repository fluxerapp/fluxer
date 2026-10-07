%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_connect_async_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

finalize_pending_session_tracks_connected_user_test() ->
    SessionId = <<"s1">>,
    UserId = 10,
    Attempt = 3,
    State0 = finalize_state(SessionId, UserId, Attempt, true, #{}, sets:new(), #{UserId => 1}),
    State1 = finalize_session(SessionId, UserId, Attempt, State0),
    ?assertEqual(#{UserId => 1}, maps:get(user_session_counts, State1)),
    ?assertEqual(true, sets:is_element(UserId, maps:get(connected_user_ids, State1))),
    ?assertEqual(2, maps:get(UserId, maps:get(presence_subscriptions, State1))),
    flush_connect_result().

finalize_records_the_thread_viewer_flag_test() ->
    Key = channel_threads_config,
    Previous = persistent_term:get(Key, undefined),
    persistent_term:put(Key, (channel_threads_config:default_config())#{
        enabled => true, included_users => #{<<"10">> => true}
    }),
    try
        SessionId = <<"s-thread">>,
        Attempt = 2,
        State0 = finalize_state(SessionId, 10, Attempt, true, #{}, sets:new(), #{}),
        State1 = State0#{data => #{thread_gate => #{active => true, version => 1}}},
        Computed0 = finalize_computed(SessionId, 10),
        Request = (maps:get(request, Computed0))#{thread_channels_capable => true},
        Computed = Computed0#{request => Request, thread_viewer => true},
        State2 = guild_connect_async:finalize_session_connect_async(
            SessionId, Attempt, {ok, #{}}, Computed, State1
        ),
        Session = maps:get(SessionId, maps:get(sessions, State2)),
        ?assertEqual(true, maps:get(thread_viewer, Session)),
        ?assertEqual(true, maps:get(thread_capable, Session)),
        receive
            {thread_flip_resend, SessionId} -> ?assert(false)
        after 0 -> ok
        end,
        flush_connect_result()
    after
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end
    end.

finalize_connected_session_does_not_double_count_test() ->
    SessionId = <<"s1">>,
    UserId = 10,
    Attempt = 4,
    State0 = finalize_state(
        SessionId,
        UserId,
        Attempt,
        false,
        #{UserId => 1},
        sets:from_list([UserId]),
        #{UserId => 1}
    ),
    State1 = finalize_session(SessionId, UserId, Attempt, State0),
    ?assertEqual(#{UserId => 1}, maps:get(user_session_counts, State1)),
    ?assertEqual(true, sets:is_element(UserId, maps:get(connected_user_ids, State1))),
    ?assertEqual(1, maps:get(UserId, maps:get(presence_subscriptions, State1))),
    flush_connect_result().

finalize_not_member_discards_pending_session_test() ->
    SessionId = <<"s-nm">>,
    UserId = 777,
    Attempt = 1,
    Pending = #{
        session_id => SessionId,
        user_id => UserId,
        pid => self(),
        mref => make_ref(),
        pending_connect => true,
        active_guilds => sets:new()
    },
    State0 = #{
        id => 42,
        sessions => #{SessionId => Pending},
        session_connect_pending => #{SessionId => Attempt},
        session_connect_inflight => 1,
        session_connect_queue => queue:new(),
        user_session_counts => #{},
        connected_user_ids => sets:new(),
        presence_subscriptions => #{}
    },
    State1 = guild_connect_async:finalize_session_connect_async(
        SessionId,
        Attempt,
        {error, not_member},
        finalize_computed(SessionId, UserId),
        State0
    ),
    ?assertEqual(#{}, maps:get(sessions, State1)),
    ?assertEqual(0, maps:get(UserId, maps:get(user_session_counts, State1), 0)),
    ?assertNot(sets:is_element(UserId, maps:get(connected_user_ids, State1))),
    receive
        {guild_connect_result, 42, Attempt, {error, not_member}} -> ok
    after 0 ->
        ?assert(false, not_member_result_not_relayed)
    end.

enqueue_session_connect_async_drops_oldest_waiter_when_queue_full_test() ->
    OldSessionId = <<"s-old">>,
    NewSessionId = <<"s-new">>,
    GuildId = 42,
    OldAttempt = 1,
    NewAttempt = 2,
    State0 = saturated_connect_state(GuildId, OldSessionId, OldAttempt, 1),
    Request = connect_request(NewSessionId, 20),
    State1 = guild_connect_async:enqueue_session_connect_async(
        GuildId, NewAttempt, Request, #{}, State0
    ),
    Pending = maps:get(session_connect_pending, State1),
    ?assertEqual(false, maps:is_key(OldSessionId, Pending)),
    ?assertEqual(NewAttempt, maps:get(NewSessionId, Pending)),
    ?assertEqual(false, maps:is_key(OldSessionId, maps:get(sessions, State1))),
    ?assertEqual(true, maps:is_key(NewSessionId, maps:get(sessions, State1))),
    QueueItems = queue:to_list(maps:get(session_connect_queue, State1)),
    ?assertEqual([NewSessionId], queued_session_ids(QueueItems)),
    receive
        {guild_connect_result, GuildId, OldAttempt, {error, overloaded}} -> ok
    after 0 ->
        ?assert(false, overflow_drop_was_not_reported)
    end.

enqueue_session_connect_async_allows_immediate_start_when_wait_queue_disabled_test() ->
    SessionId = <<"s-start">>,
    GuildId = 42,
    Attempt = 1,
    State0 = #{
        id => GuildId,
        sessions => #{},
        session_connect_queue => queue:new(),
        session_connect_pending => #{},
        session_connect_inflight => 0,
        session_connect_max_queue => 0,
        data => #{},
        member_count => 0,
        voice_states => #{},
        member_list_engine => undefined,
        virtual_channel_access => #{}
    },
    State1 = guild_connect_async:enqueue_session_connect_async(
        GuildId, Attempt, connect_request(SessionId, 30), #{}, State0
    ),
    ?assertEqual(1, maps:get(session_connect_inflight, State1)),
    ?assertEqual([], queue:to_list(maps:get(session_connect_queue, State1))),
    ?assertEqual(Attempt, maps:get(SessionId, maps:get(session_connect_pending, State1))),
    WorkerRefs = maps:get(session_connect_worker_refs, State1, #{}),
    cleanup_worker_refs(WorkerRefs).

finalize_batch_upserts_every_result_and_frees_one_worker_test() ->
    UserId = 10,
    Sessions = [<<"s1">>, <<"s2">>],
    State0 = (finalize_state(<<"s1">>, UserId, 1, true, #{}, sets:new(), #{UserId => 1}))#{
        sessions => maps:from_list([{S, pending_entry(S, UserId)} || S <- Sessions]),
        session_connect_pending => maps:from_list([{S, 1} || S <- Sessions]),
        session_connect_inflight => 2
    },
    State1 = guild_connect_async:finalize_session_connect_batch(
        [{S, 1, {ok, #{}}, finalize_computed(S, UserId)} || S <- Sessions], State0
    ),
    ?assertEqual(1, maps:get(session_connect_inflight, State1)),
    ?assertEqual(#{}, maps:get(session_connect_pending, State1)),
    [
        ?assertEqual(false, maps:get(pending_connect, maps:get(S, maps:get(sessions, State1))))
     || S <- Sessions
    ],
    ?assertEqual(#{UserId => 2}, maps:get(user_session_counts, State1)),
    ?assertEqual(3, maps:get(UserId, maps:get(presence_subscriptions, State1))),
    ?assertEqual(2, length(flush_connect_results())).

finalize_batch_skips_stale_attempts_test() ->
    UserId = 11,
    State0 = finalize_state(<<"s1">>, UserId, 2, true, #{}, sets:new(), #{UserId => 1}),
    State1 = guild_connect_async:finalize_session_connect_batch(
        [{<<"s1">>, 1, {ok, #{}}, finalize_computed(<<"s1">>, UserId)}], State0
    ),
    ?assertEqual(#{<<"s1">> => 2}, maps:get(session_connect_pending, State1)),
    ?assertEqual(
        true, maps:get(pending_connect, maps:get(<<"s1">>, maps:get(sessions, State1)))
    ),
    ?assertEqual([], flush_connect_results()).

full_guild_reconnect_fits_the_connect_queue_test() ->
    GuildId = 42,
    State0 = (saturated_connect_state(GuildId, <<"s-0">>, 0, 1))#{
        session_connect_queue => queue:new(),
        session_connect_pending => #{},
        sessions => #{}
    },
    State1 = lists:foldl(
        fun(N, Acc) ->
            SessionId = integer_to_binary(N),
            guild_connect_async:enqueue_session_connect_async(
                GuildId,
                0,
                connect_request(SessionId, N),
                #{},
                maps:remove(session_connect_max_queue, Acc)
            )
        end,
        State0,
        lists:seq(1, 3000)
    ),
    ?assertEqual(3000, queue:len(maps:get(session_connect_queue, State1))),
    ?assertEqual(3000, map_size(maps:get(session_connect_pending, State1))),
    receive
        {guild_connect_result, GuildId, _, {error, overloaded}} ->
            ?assert(false, connect_dropped)
    after 0 ->
        ok
    end.

queued_connects_spread_over_batched_workers_test() ->
    ok = flush_batches(),
    GuildId = 42,
    Requests = [connect_request(integer_to_binary(N), N) || N <- lists:seq(1, 20)],
    State0 = #{
        id => GuildId,
        sessions => #{},
        session_connect_queue => queue:from_list([
            #{guild_id => GuildId, attempt => 0, request => R, reply_via_pid => undefined}
         || R <- Requests
        ]),
        session_connect_pending => #{},
        session_connect_inflight => 0,
        data => #{},
        member_count => 0,
        voice_states => #{},
        member_list_engine => undefined,
        virtual_channel_access => #{}
    },
    State1 = guild_connect_async:maybe_start_session_connect_workers(State0),
    WorkerRefs = maps:get(session_connect_worker_refs, State1),
    ?assertEqual(8, maps:get(session_connect_inflight, State1)),
    ?assertEqual(8, map_size(WorkerRefs)),
    ?assertEqual(0, queue:len(maps:get(session_connect_queue, State1))),
    Batches = [receive_batch() || _ <- lists:seq(1, 8)],
    ?assertEqual(
        lists:sort([maps:get(session_id, R) || R <- Requests]),
        lists:sort([SessionId || Batch <- Batches, {SessionId, 0, _, _} <- Batch])
    ),
    cleanup_worker_refs(WorkerRefs).

pending_entry(SessionId, UserId) ->
    #{
        session_id => SessionId,
        user_id => UserId,
        pid => self(),
        mref => make_ref(),
        pending_connect => true,
        active_guilds => sets:new()
    }.

receive_batch() ->
    receive
        {'$gen_cast', {session_connect_worker_batch_done, Results}} -> Results
    after 5000 ->
        ?assert(false, connect_batch_not_received)
    end.

flush_batches() ->
    receive
        {'$gen_cast', {session_connect_worker_batch_done, _Results}} -> flush_batches()
    after 100 ->
        ok
    end.

flush_connect_results() ->
    receive
        {guild_connect_result, _GuildId, _Attempt, _Reply} = Msg ->
            [Msg | flush_connect_results()]
    after 0 ->
        []
    end.

finalize_state(SessionId, UserId, Attempt, PendingConnect, Counts, Connected, PresenceSubs) ->
    Existing = #{
        session_id => SessionId,
        user_id => UserId,
        pid => self(),
        mref => make_ref(),
        pending_connect => PendingConnect,
        active_guilds => sets:new()
    },
    #{
        id => 42,
        sessions => #{SessionId => Existing},
        session_connect_pending => #{SessionId => Attempt},
        session_connect_inflight => 1,
        session_connect_queue => queue:new(),
        user_session_counts => Counts,
        connected_user_ids => Connected,
        presence_subscriptions => PresenceSubs,
        member_presence => #{}
    }.

saturated_connect_state(GuildId, SessionId, Attempt, MaxQueue) ->
    PendingSession = #{
        session_id => SessionId,
        user_id => 10,
        pid => self(),
        mref => make_ref(),
        pending_connect => true,
        active_guilds => sets:new()
    },
    #{
        id => GuildId,
        sessions => #{SessionId => PendingSession},
        session_connect_pending => #{SessionId => Attempt},
        session_connect_queue => queue:from_list([
            #{
                guild_id => GuildId,
                attempt => Attempt,
                request => connect_request(SessionId, 10),
                reply_via_pid => undefined
            }
        ]),
        session_connect_inflight => 8,
        session_connect_max_queue => MaxQueue,
        user_session_counts => #{},
        connected_user_ids => sets:new(),
        presence_subscriptions => #{},
        member_presence => #{}
    }.

connect_request(SessionId, UserId) ->
    #{
        session_id => SessionId,
        user_id => UserId,
        session_pid => self(),
        bot => false,
        is_staff => false,
        active_guilds => sets:new()
    }.

queued_session_ids(Items) ->
    [maps:get(session_id, maps:get(request, Item, #{}), undefined) || Item <- Items].

cleanup_worker_refs(WorkerRefs) ->
    maps:foreach(
        fun(Ref, _Value) ->
            receive
                {'DOWN', Ref, process, _Pid, _Reason} -> ok
            after 1000 ->
                ?assert(false, async_connect_worker_did_not_exit)
            end
        end,
        WorkerRefs
    ).

finalize_computed(SessionId, UserId) ->
    #{
        request => #{session_id => SessionId, user_id => UserId, session_pid => self()},
        user_roles => [],
        viewable_channels => #{},
        should_mark_guild_synced => false,
        initial_last_message_ids => #{},
        initial_channel_versions => #{}
    }.

finalize_session(SessionId, UserId, Attempt, State) ->
    guild_connect_async:finalize_session_connect_async(
        SessionId,
        Attempt,
        {ok, #{}},
        finalize_computed(SessionId, UserId),
        State
    ).

flush_connect_result() ->
    receive
        {guild_connect_result, _GuildId, _Attempt, _Reply} -> ok
    after 0 ->
        ok
    end.
