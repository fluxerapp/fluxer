%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions_connect_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

build_initial_last_message_ids_empty_channels_test() ->
    ?assertEqual(
        #{}, guild_sessions_connect:build_initial_last_message_ids(#{<<"channels">> => []})
    ).

build_initial_last_message_ids_with_channels_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => <<"600">>}
        ]
    },
    ?assertEqual(
        #{<<"100">> => <<"500">>, <<"101">> => <<"600">>},
        guild_sessions_connect:build_initial_last_message_ids(GuildState)
    ).

build_initial_last_message_ids_normalizes_integer_ids_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => 100, <<"last_message_id">> => 500},
            #{<<"id">> => 101, <<"last_message_id">> => <<"600">>}
        ]
    },
    ?assertEqual(
        #{<<"100">> => <<"500">>, <<"101">> => <<"600">>},
        guild_sessions_connect:build_initial_last_message_ids(GuildState)
    ).

build_initial_last_message_ids_filters_null_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => null},
            #{<<"id">> => <<"102">>}
        ]
    },
    ?assertEqual(
        #{<<"100">> => <<"500">>},
        guild_sessions_connect:build_initial_last_message_ids(GuildState)
    ).

build_initial_channel_versions_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"version">> => 5},
            #{<<"id">> => <<"101">>}
        ]
    },
    ?assertEqual(
        #{<<"100">> => 5, <<"101">> => 0},
        guild_sessions_connect:build_initial_channel_versions(GuildState)
    ).

build_initial_channel_versions_normalizes_integer_ids_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => 100, <<"version">> => 5},
            #{<<"id">> => 101}
        ]
    },
    ?assertEqual(
        #{<<"100">> => 5, <<"101">> => 0},
        guild_sessions_connect:build_initial_channel_versions(GuildState)
    ).

remove_session_cleans_connect_pending_test() ->
    SessionId = <<"s1">>,
    State = remove_session_state(SessionId),
    NewState = guild_sessions_connect:remove_session(SessionId, State),
    #{} = Pending = maps:get(session_connect_pending, NewState, #{}),
    ?assertEqual(false, maps:is_key(SessionId, Pending)),
    ?assertEqual(true, maps:is_key(<<"s2">>, Pending)).

remove_session_clears_ref_index_test() ->
    SessionId = <<"s1">>,
    State0 = remove_session_state(SessionId),
    Session = maps:get(SessionId, maps:get(sessions, State0)),
    Ref = maps:get(mref, Session),
    State = State0#{guild_session_refs => #{Ref => SessionId}},
    NewState = guild_sessions_connect:remove_session(SessionId, State),
    Refs = map_utils:ensure_map(maps:get(guild_session_refs, NewState, #{})),
    ?assertEqual(false, maps:is_key(Ref, Refs)).

cleanup_connect_admission_queue_format_test() ->
    S1 = <<"s1">>,
    S2 = <<"s2">>,
    S3 = <<"s3">>,
    Queue = queue:from_list([
        #{request => #{session_id => S1}, attempt => 0},
        #{request => #{session_id => S2}, attempt => 1},
        #{request => #{session_id => S1}, attempt => 2},
        #{request => #{session_id => S3}, attempt => 3}
    ]),
    State1 = guild_sessions_connect_cleanup:cleanup_connect_admission_for_session(
        S1, #{
            sessions => #{},
            session_connect_pending => #{S1 => 2},
            session_connect_queue => Queue
        }
    ),
    ResultQueue = queue:to_list(maps:get(session_connect_queue, State1)),
    SessionIds = [
        maps:get(session_id, maps:get(request, Item, #{}), undefined)
     || Item <- ResultQueue
    ],
    ?assertEqual(2, length(ResultQueue)),
    ?assertEqual(false, lists:member(S1, SessionIds)),
    ?assertEqual(true, lists:member(S2, SessionIds)),
    ?assertEqual(true, lists:member(S3, SessionIds)).

auto_stop_tests_test() ->
    ?assertEqual(true, guild_sessions_connect_cleanup:should_auto_stop_on_empty(#{})),
    ?assertEqual(
        false,
        guild_sessions_connect_cleanup:should_auto_stop_on_empty(#{
            disable_auto_stop_on_empty => true
        })
    ),
    State = guild_sessions_connect_cleanup:maybe_mark_auto_stop_pending(#{}),
    Pending = maps:get(auto_stop_pending, State),
    ?assert(is_reference(maps:get(token, Pending))),
    ?assert(is_reference(maps:get(timer_ref, Pending))),
    ?assert(is_integer(maps:get(started_at, Pending))),
    Cleared = guild_sessions_connect_cleanup:clear_auto_stop_pending(State),
    ?assertEqual(false, maps:is_key(auto_stop_pending, Cleared)).

normalize_connect_queue_test() ->
    List = [#{a => 1}, #{a => 2}],
    Queue = guild_sessions_connect_cleanup:normalize_connect_queue(List),
    ?assert(queue:is_queue(Queue)),
    ?assertEqual(queue:from_list(List), Queue),
    Q = queue:from_list([1, 2, 3]),
    ?assertEqual(Q, guild_sessions_connect_cleanup:normalize_connect_queue(Q)),
    ?assertEqual(undefined, guild_sessions_connect_cleanup:normalize_connect_queue(undefined)).

session_down_clears_passive_sync_entry_test() ->
    ok = passive_sync_registry:init(),
    SessionId = <<"passive-down">>,
    ok = passive_sync_registry:store(SessionId, 42, stored_passive_state()),
    {Ref, Session, State} = down_state(SessionId),
    {noreply, _} = guild_sessions_connect:handle_session_down(Ref, {SessionId, Session}, State),
    ?assertEqual([], ets:lookup(passive_sync_registry, {SessionId, 42})).

remove_session_clears_passive_sync_entry_test() ->
    ok = passive_sync_registry:init(),
    SessionId = <<"passive-remove">>,
    ok = passive_sync_registry:store(SessionId, 42, stored_passive_state()),
    _ = guild_sessions_connect:remove_session(SessionId, remove_session_state(SessionId)),
    ?assertEqual([], ets:lookup(passive_sync_registry, {SessionId, 42})).

passive_sync_registry_stays_bounded_across_session_cycles_test() ->
    ok = passive_sync_registry:init(),
    GuildRows = fun() -> length(ets:match(passive_sync_registry, {{'_', 42}, '_'})) end,
    Before = GuildRows(),
    lists:foreach(
        fun(N) ->
            SessionId = <<"cycle-", (integer_to_binary(N))/binary>>,
            ok = passive_sync_registry:store(SessionId, 42, stored_passive_state()),
            {Ref, Session, State} = down_state(SessionId),
            {noreply, _} = guild_sessions_connect:handle_session_down(
                Ref, {SessionId, Session}, State
            )
        end,
        lists:seq(1, 500)
    ),
    ?assertEqual(Before, GuildRows()).

guild_terminate_clears_passive_sync_entries_test() ->
    ok = passive_sync_registry:init(),
    GuildId = 4242,
    OtherGuildId = 4343,
    lists:foreach(
        fun({SessionId, GId}) ->
            ok = passive_sync_registry:store(SessionId, GId, stored_passive_state())
        end,
        [{<<"t1">>, GuildId}, {<<"t2">>, GuildId}, {<<"t1">>, OtherGuildId}]
    ),
    ok = guild:terminate(shutdown, #{
        id => GuildId,
        sessions => #{<<"t1">> => #{}, <<"t2">> => #{}}
    }),
    ?assertEqual([], ets:lookup(passive_sync_registry, {<<"t1">>, GuildId})),
    ?assertEqual([], ets:lookup(passive_sync_registry, {<<"t2">>, GuildId})),
    ?assertEqual(
        stored_passive_state(), passive_sync_registry:lookup(<<"t1">>, OtherGuildId)
    ),
    ok = passive_sync_registry:delete(<<"t1">>, OtherGuildId).

stored_passive_state() ->
    #{
        previous_passive_updates => #{<<"1">> => <<"2">>},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    }.

down_state(SessionId) ->
    State0 = remove_session_state(SessionId),
    Session = maps:get(SessionId, maps:get(sessions, State0)),
    Ref = maps:get(mref, Session),
    {Ref, Session, State0#{guild_session_refs => #{Ref => SessionId}}}.

remove_session_state(SessionId) ->
    SessionData = #{
        session_id => SessionId,
        user_id => 1,
        pid => self(),
        mref => make_ref(),
        active_guilds => sets:new(),
        user_roles => [],
        bot => false
    },
    #{
        id => 42,
        sessions => #{SessionId => SessionData},
        presence_subscriptions => #{1 => 1},
        member_list_subscriptions => guild_member_list_subs:new(),
        member_subscriptions => guild_subscriptions:init_state(),
        connected_user_ids => sets:new(),
        user_session_counts => #{},
        session_connect_pending => #{SessionId => 3, <<"s2">> => 1},
        session_connect_queue => [
            #{request => #{session_id => SessionId}, attempt => 3},
            #{request => #{session_id => <<"s2">>}, attempt => 1}
        ]
    }.
