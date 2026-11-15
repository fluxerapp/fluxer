%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_connections_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHANNEL_ID, 1539791936896897100).
-define(USER_A, 1539791936896897001).
-define(USER_B, 1539791936896897002).
-define(USER_C, 1539791936896897003).

call_connections_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun two_devices_of_one_user_are_both_in_the_call/1,
        fun one_device_leaving_keeps_the_other/1,
        fun a_device_going_down_keeps_the_other/1,
        fun disconnect_with_connection_id_removes_only_that_device/1,
        fun disconnect_without_connection_id_removes_every_device/1,
        fun devices_with_different_self_deaf_do_not_flap/1,
        fun update_for_an_unknown_connection_is_rejected/1,
        fun max_participants_counts_users/1,
        fun ringing_skips_a_user_who_is_in_the_call/1,
        fun handoff_keeps_every_connection/1,
        fun handoff_from_a_user_keyed_export_keeps_every_user/1,
        fun voice_states_for_channel_lists_both_connections/1,
        fun an_update_keeps_the_voice_server_routing/1,
        fun call_create_after_ready_hides_voice_server_routing/1
    ]}.

handoff_import_test_() ->
    {foreach, fun() -> process_registry:init() end, fun(_) -> ok end, [
        fun imports_a_v1_export/1,
        fun imports_a_v2_hotpatch_export/1
    ]}.

setup() ->
    process_registry:init(),
    {ok, Manager} = call_manager:start_link(),
    unlink(Manager),
    {ok, Pid} = gen_server:call(
        call_manager,
        {create, ?CHANNEL_ID, #{
            channel_id => ?CHANNEL_ID,
            message_id => 1,
            region => undefined,
            ringing => [],
            recipients => [?USER_A, ?USER_B, ?USER_C]
        }}
    ),
    #{manager => Manager, call => Pid}.

teardown(#{manager := Manager, call := Pid}) ->
    exit(Pid, kill),
    exit(Manager, kill),
    wait_dead(Pid),
    wait_dead(Manager).

two_devices_of_one_user_are_both_in_the_call(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {_, _} = join(Pid, ?USER_A, <<"a2">>, #{}),
    ?_assertEqual(
        [<<"a1">>, <<"a2">>],
        connection_ids(call_update_voice_states(Pid))
    ).

one_device_leaving_keeps_the_other(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {_, _} = join(Pid, ?USER_A, <<"a2">>, #{}),
    ok = gen_server:call(Pid, {leave, session_id(<<"a2">>)}),
    ?_assertEqual([<<"a1">>], connection_ids(get_state_voice_states(Pid))).

a_device_going_down_keeps_the_other(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {Session2, _} = join(Pid, ?USER_A, <<"a2">>, #{}),
    wait_dead(Session2),
    ?_assertEqual([<<"a1">>], connection_ids(get_state_voice_states(Pid))).

disconnect_with_connection_id_removes_only_that_device(#{call := Pid}) ->
    {Session1, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {Session2, _} = join(Pid, ?USER_A, <<"a2">>, #{}),
    Reply = gen_server:call(
        Pid, {disconnect_user_if_in_channel, ?USER_A, ?CHANNEL_ID, <<"a2">>}
    ),
    [
        ?_assertEqual(#{success => true}, Reply),
        ?_assertEqual([<<"a1">>], connection_ids(get_state_voice_states(Pid))),
        ?_assertEqual([{?CHANNEL_ID, <<"a2">>}], force_disconnects(Session2)),
        ?_assertEqual([], force_disconnects(Session1)),
        ?_assertEqual(
            [session_id(<<"a1">>)], maps:keys(maps:get(sessions, sys:get_state(Pid)))
        )
    ].

disconnect_without_connection_id_removes_every_device(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_B, <<"b1">>, #{}),
    {Session1, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {Session2, _} = join(Pid, ?USER_A, <<"a2">>, #{}),
    Reply = gen_server:call(
        Pid, {disconnect_user_if_in_channel, ?USER_A, ?CHANNEL_ID, undefined}
    ),
    [
        ?_assertEqual(#{success => true}, Reply),
        ?_assertEqual([<<"b1">>], connection_ids(get_state_voice_states(Pid))),
        ?_assertEqual([{?CHANNEL_ID, <<"a1">>}], force_disconnects(Session1)),
        ?_assertEqual([{?CHANNEL_ID, <<"a2">>}], force_disconnects(Session2))
    ].

devices_with_different_self_deaf_do_not_flap(#{call := Pid}) ->
    {_, Deaf} = join(Pid, ?USER_A, <<"a1">>, #{<<"self_deaf">> => true}),
    {_, Hearing} = join(Pid, ?USER_A, <<"a2">>, #{<<"self_deaf">> => false}),
    Updates = lists:append(lists:duplicate(5, [Deaf, Hearing])),
    Replies = [gen_server:call(Pid, {update_voice_state, ?USER_A, VS}) || VS <- Updates],
    Event = call_update_voice_states(Pid),
    [
        ?_assertEqual(lists:duplicate(10, ok), Replies),
        ?_assertEqual(
            [{<<"a1">>, true}, {<<"a2">>, false}],
            [{maps:get(<<"connection_id">>, VS), maps:get(<<"self_deaf">>, VS)} || VS <- Event]
        )
    ].

update_for_an_unknown_connection_is_rejected(#{call := Pid}) ->
    {_, VS} = join(Pid, ?USER_A, <<"a1">>, #{}),
    [
        ?_assertEqual(
            {error, not_in_call},
            gen_server:call(
                Pid, {update_voice_state, ?USER_A, VS#{<<"connection_id">> => <<"zz">>}}
            )
        ),
        ?_assertEqual(
            {error, not_in_call},
            gen_server:call(Pid, {update_voice_state, ?USER_B, VS})
        )
    ].

max_participants_counts_users(#{call := Pid}) ->
    P2p = #{<<"p2p">> => true},
    ok = join_reply(Pid, ?USER_A, <<"a1">>, P2p, 2),
    ok = join_reply(Pid, ?USER_A, <<"a2">>, P2p, 2),
    ok = join_reply(Pid, ?USER_B, <<"b1">>, P2p, 2),
    [
        ?_assertEqual({error, voice_channel_full}, join_reply(Pid, ?USER_C, <<"c1">>, P2p, 2)),
        ?_assertEqual(
            [<<"a1">>, <<"a2">>, <<"b1">>], connection_ids(get_state_voice_states(Pid))
        )
    ].

ringing_skips_a_user_who_is_in_the_call(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    ok = gen_server:call(Pid, {ring_recipients, [?USER_A, ?USER_B]}),
    ?_assertEqual([?USER_B], maps:get(ringing, sys:get_state(Pid))).

handoff_keeps_every_connection(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {_, _} = join(Pid, ?USER_A, <<"a2">>, #{}),
    {ok, Export} = gen_server:call(Pid, export_handoff_state),
    Restored = call_handoff:restore_state(Export),
    [
        ?_assertEqual([<<"a1">>, <<"a2">>], restored_connection_ids(Restored)),
        ?_assertEqual([?USER_A], maps:keys(maps:get(voice_states, Export)))
    ].

handoff_from_a_user_keyed_export_keeps_every_user(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {_, _} = join(Pid, ?USER_B, <<"b1">>, #{}),
    {ok, Export} = gen_server:call(Pid, export_handoff_state),
    Restored = call_handoff:restore_state(maps:remove(connection_voice_states, Export)),
    ?_assertEqual([<<"a1">>, <<"b1">>], restored_connection_ids(Restored)).

voice_states_for_channel_lists_both_connections(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{}),
    {_, _} = join(Pid, ?USER_A, <<"a2">>, #{}),
    #{<<"voice_states">> := VoiceStates} = gateway_rpc_voice:execute_method(
        <<"voice.get_voice_states_for_channel">>,
        #{<<"channel_id">> => integer_to_binary(?CHANNEL_ID)}
    ),
    ?_assertEqual(
        [{<<"a1">>, integer_to_binary(?USER_A)}, {<<"a2">>, integer_to_binary(?USER_A)}],
        lists:sort([
            {maps:get(<<"connection_id">>, VS), maps:get(<<"user_id">>, VS)}
         || VS <- VoiceStates
        ])
    ).

an_update_keeps_the_voice_server_routing(#{call := Pid}) ->
    Routing = #{<<"region_id">> => <<"ams">>, <<"server_id">> => <<"ams-1">>},
    {_, VS} = join(Pid, ?USER_A, <<"a1">>, Routing),
    Update = maps:without(maps:keys(Routing), VS#{<<"self_mute">> => true}),
    ok = gen_server:call(Pid, {update_voice_state, ?USER_A, Update}),
    [Stored] = get_state_voice_states(Pid),
    ?_assertEqual(
        {true, <<"ams">>, <<"ams-1">>},
        {
            maps:get(<<"self_mute">>, Stored),
            maps:get(<<"region_id">>, Stored),
            maps:get(<<"server_id">>, Stored)
        }
    ).

call_create_after_ready_hides_voice_server_routing(#{call := Pid}) ->
    {_, _} = join(Pid, ?USER_A, <<"a1">>, #{<<"server_id">> => <<"ams-1">>}),
    {ok, CallData} = gen_server:call(Pid, {get_state}),
    #{voice_states := [Sent]} = Event = call_state:broadcast_call_event(CallData),
    [
        ?_assertEqual(call_state:build_call_event(sys:get_state(Pid)), Event),
        ?_assertNot(maps:is_key(<<"server_id">>, Sent)),
        ?_assertEqual(
            [<<"ams-1">>],
            [maps:get(<<"server_id">>, VS) || VS <- maps:get(voice_states, CallData)]
        )
    ].

imports_a_v1_export(_) ->
    Session = fake_session(),
    Export = #{
        channel_id => ?CHANNEL_ID,
        message_id => 1,
        region => undefined,
        ringing => [],
        pending_ringing => [],
        recipients => [?USER_A, ?USER_B],
        voice_states => #{
            ?USER_A => voice_state(?USER_A, <<"a2">>, #{}),
            ?USER_B => voice_state(?USER_B, <<"b1">>, #{})
        },
        sessions => #{
            session_id(<<"a2">>) => #{user_id => ?USER_A, session_pid => Session},
            session_id(<<"b1">>) => #{user_id => ?USER_B, session_pid => Session}
        },
        pending_connections => #{},
        initiator_ready => true,
        created_at => 1,
        participants_history => [?USER_A, ?USER_B],
        last_call_event => undefined
    },
    Restored = call_handoff:restore_state(Export),
    ?_assertEqual([<<"a2">>, <<"b1">>], restored_connection_ids(Restored)).

imports_a_v2_hotpatch_export(_) ->
    Session = fake_session(),
    Connections = #{
        <<"a1">> => voice_state(?USER_A, <<"a1">>, #{<<"self_deaf">> => true}),
        <<"a2">> => voice_state(?USER_A, <<"a2">>, #{}),
        <<"b1">> => voice_state(?USER_B, <<"b1">>, #{})
    },
    Export = #{
        channel_id => ?CHANNEL_ID,
        message_id => 1,
        voice_states => call_state:voice_states_by_user(Connections),
        connection_voice_states => Connections,
        sessions => maps:from_list([
            {session_id(C), #{user_id => U, session_pid => Session}}
         || {C, U} <- [{<<"a1">>, ?USER_A}, {<<"a2">>, ?USER_A}, {<<"b1">>, ?USER_B}]
        ]),
        pending_connections => #{},
        participants_history => [?USER_A, ?USER_B]
    },
    Restored = call_handoff:restore_state(Export),
    [
        ?_assertEqual([<<"a1">>, <<"a2">>, <<"b1">>], restored_connection_ids(Restored)),
        ?_assertEqual(Connections, maps:get(voice_states, Restored))
    ].

join(Pid, UserId, ConnectionId, Extra) ->
    Session = fake_session(),
    VoiceState = voice_state(UserId, ConnectionId, Extra),
    ok = gen_server:call(
        Pid, {join, UserId, VoiceState, session_id(ConnectionId), Session, ConnectionId}
    ),
    {Session, VoiceState}.

join_reply(Pid, UserId, ConnectionId, Extra, MaxParticipants) ->
    gen_server:call(
        Pid,
        {join, UserId, voice_state(UserId, ConnectionId, Extra), session_id(ConnectionId),
            fake_session(), ConnectionId, MaxParticipants}
    ).

voice_state(UserId, ConnectionId, Extra) ->
    maps:merge(
        #{
            <<"guild_id">> => null,
            <<"user_id">> => integer_to_binary(UserId),
            <<"channel_id">> => integer_to_binary(?CHANNEL_ID),
            <<"connection_id">> => ConnectionId,
            <<"session_id">> => session_id(ConnectionId),
            <<"self_mute">> => false,
            <<"self_deaf">> => false,
            <<"p2p">> => false
        },
        Extra
    ).

session_id(ConnectionId) ->
    <<"session-", ConnectionId/binary>>.

fake_session() ->
    spawn(fun() -> fake_session_loop([]) end).

fake_session_loop(Received) ->
    receive
        {'$gen_cast', {call_force_disconnect, ChannelId, ConnectionId}} ->
            fake_session_loop([{ChannelId, ConnectionId} | Received]);
        {force_disconnects, From} ->
            From ! {force_disconnects, lists:reverse(Received)},
            fake_session_loop(Received);
        _ ->
            fake_session_loop(Received)
    end.

force_disconnects(Session) ->
    Session ! {force_disconnects, self()},
    receive
        {force_disconnects, Received} -> Received
    after 1000 -> timeout
    end.

get_state_voice_states(Pid) ->
    {ok, #{voice_states := VoiceStates}} = gen_server:call(Pid, {get_state}),
    VoiceStates.

call_update_voice_states(Pid) ->
    #{voice_states := VoiceStates} = call_state:build_call_event(sys:get_state(Pid)),
    VoiceStates.

restored_connection_ids(State) ->
    lists:sort(maps:keys(maps:get(voice_states, State))).

connection_ids(VoiceStates) ->
    lists:sort([maps:get(<<"connection_id">>, VS) || VS <- VoiceStates]).

wait_dead(Pid) ->
    Ref = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    end.
