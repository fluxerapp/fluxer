%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_handoff_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

remonitor_transferred_sessions_replaces_stale_refs_test() ->
    Pid = spawn(fun session_loop/0),
    OldRef = make_ref(),
    State0 = #{
        sessions => #{
            <<"s1">> => #{pid => Pid, mref => OldRef, user_id => 10},
            <<"missing_pid">> => #{user_id => 11},
            <<"bad">> => bad_session
        }
    },
    State1 = guild_handoff:remonitor_transferred_sessions(State0),
    Sessions = maps:get(sessions, State1),
    ?assertEqual(false, maps:is_key(<<"missing_pid">>, Sessions)),
    ?assertEqual(false, maps:is_key(<<"bad">>, Sessions)),
    Session = maps:get(<<"s1">>, Sessions),
    NewRef = maps:get(mref, Session),
    ?assert(is_reference(NewRef)),
    ?assertNotEqual(OldRef, NewRef),
    Pid ! stop,
    receive
        {'DOWN', NewRef, process, Pid, _Reason} -> ok
    after 1000 ->
        ?assert(false)
    end.

restore_transferred_session_state_rebuilds_connected_counts_test() ->
    UserId = 10,
    Sessions = #{
        <<"s1">> => #{pid => self(), user_id => UserId},
        <<"s2">> => #{pid => self(), user_id => UserId, pending_connect => false},
        <<"pending">> => #{pid => self(), user_id => 11, pending_connect => true},
        <<"bad">> => #{user_id => 12},
        <<"reconnecting">> => #{
            pid => self(),
            user_id => 13,
            pending_connect => true,
            owns_connected_tracking => true
        }
    },
    State0 = #{
        sessions => Sessions,
        connected_user_ids => sets:new(),
        user_session_counts => #{},
        presence_subscriptions => #{UserId => 1}
    },
    State1 = guild_handoff:restore_transferred_session_state(State0),
    ?assertEqual(#{UserId => 2, 13 => 1}, maps:get(user_session_counts, State1)),
    Connected = maps:get(connected_user_ids, State1),
    ?assertEqual(true, sets:is_element(UserId, Connected)),
    ?assertEqual(false, sets:is_element(11, Connected)),
    ?assertEqual(true, sets:is_element(13, Connected)),
    ?assertEqual(3, maps:get(UserId, maps:get(presence_subscriptions, State1))).

export_handoff_state_contains_all_required_keys_test() ->
    State = #{
        id => 12345,
        data => #{<<"guild">> => #{<<"id">> => <<"12345">>}},
        sessions => #{
            <<"s1">> => #{pid => self(), user_id => 10, mref => make_ref()}
        },
        voice_states => #{<<"conn1">> => #{<<"user_id">> => 10}},
        virtual_channel_access => #{},
        virtual_channel_access_pending => #{},
        virtual_channel_access_preserve => #{},
        virtual_channel_access_move_pending => #{}
    },
    Exported = guild_handoff:export_handoff_state(State),
    ?assertEqual(ok, guild_handoff:validate_handoff_state(Exported)),
    ?assertEqual(12345, maps:get(id, Exported)),
    ?assert(is_map(maps:get(data, Exported))),
    ?assert(is_map(maps:get(sessions, Exported))),
    ?assert(is_map(maps:get(voice_states, Exported))),
    Session = maps:get(<<"s1">>, maps:get(sessions, Exported)),
    ?assertEqual(false, maps:is_key(mref, Session)).

export_handoff_state_omits_pending_member_list_batch_test() ->
    State = #{
        id => 12345,
        data => #{<<"guild">> => #{<<"id">> => <<"12345">>}},
        sessions => #{},
        voice_states => #{},
        pending_member_list_sync_batch => #{
            timer_ref => make_ref(),
            pending_list_ids => #{<<"500">> => true}
        }
    },
    Exported = guild_handoff:export_handoff_state(State),
    ?assertNot(maps:is_key(pending_member_list_sync_batch, Exported)).

validate_handoff_state_rejects_missing_keys_test() ->
    ?assertMatch({error, _}, guild_handoff:validate_handoff_state(#{})),
    ?assertMatch({error, _}, guild_handoff:validate_handoff_state(#{id => 1})),
    ?assertMatch({error, [not_a_map]}, guild_handoff:validate_handoff_state(not_a_map)).

validate_handoff_state_rejects_bad_types_test() ->
    BadState = #{id => <<"not_int">>, data => #{}, sessions => #{}, voice_states => #{}},
    ?assertMatch({error, [id]}, guild_handoff:validate_handoff_state(BadState)).

export_import_roundtrip_preserves_data_test() ->
    State = #{
        id => 54321,
        data => #{<<"guild">> => #{<<"id">> => <<"54321">>}, <<"channels">> => []},
        sessions => #{},
        voice_states => #{<<"c1">> => #{<<"user_id">> => 1, <<"channel_id">> => 100}},
        virtual_channel_access => #{10 => sets:from_list([20])},
        virtual_channel_access_pending => #{},
        virtual_channel_access_preserve => #{},
        virtual_channel_access_move_pending => #{}
    },
    Exported = guild_handoff:export_handoff_state(State),
    ?assertEqual(ok, guild_handoff:validate_handoff_state(Exported)),
    ExportedVoice = maps:get(voice_states, Exported),
    ?assertEqual(1, maps:size(ExportedVoice)),
    ?assert(maps:is_key(<<"c1">>, ExportedVoice)).

session_loop() ->
    receive
        stop -> ok
    after infinity ->
        ok
    end.

export_handoff_state_replaces_the_thread_store_with_rows_test() ->
    Tab = guild_thread_store:new(),
    ok = guild_thread_store:put_thread(Tab, #{<<"id">> => 10, <<"parent_id">> => 1}),
    try
        State = #{
            id => 12345,
            data => #{thread_store => Tab, thread_gate => #{active => true, version => 1}},
            sessions => #{}
        },
        Data = maps:get(data, guild_handoff:export_handoff_state(State)),
        ?assertNot(maps:is_key(thread_store, Data)),
        ?assertEqual(#{active => true, version => 1}, maps:get(thread_gate, Data)),
        ?assertEqual(guild_thread_store:export(Tab), maps:get(thread_rows, Data)),
        ?assert(is_integer(maps:get(thread_rows_at, Data)))
    after
        guild_thread_store:destroy(Tab)
    end.

init_from_export_queues_a_flip_when_the_gate_disagrees_with_config_test() ->
    Tab = guild_thread_store:new(),
    ok = guild_thread_store:put_thread(Tab, #{<<"id">> => 10, <<"parent_id">> => 1}),
    Key = channel_threads_config,
    Previous = persistent_term:get(Key, undefined),
    try
        Exported = guild_handoff:export_handoff_state(#{
            id => 12345,
            data => #{thread_store => Tab, thread_gate => #{active => true, version => 4}},
            sessions => #{}
        }),
        persistent_term:put(Key, (channel_threads_config:default_config())#{
            enabled => true, config_version => 5
        }),
        _ = guild_init:init_base_state(Exported),
        ?assertEqual([5], drain_flips([])),
        persistent_term:put(Key, (channel_threads_config:default_config())#{
            enabled => true, config_version => 5, enabled_guilds => #{<<"12345">> => true}
        }),
        _ = guild_init:init_base_state(Exported),
        ?assertEqual([], drain_flips([])),
        persistent_term:erase(Key),
        _ = guild_init:init_base_state(Exported),
        ?assertEqual([], drain_flips([]))
    after
        guild_thread_store:destroy(Tab),
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end
    end.

drain_flips(Acc) ->
    receive
        {thread_gate_flip, Version} -> drain_flips([Version | Acc])
    after 0 -> lists:reverse(Acc)
    end.
