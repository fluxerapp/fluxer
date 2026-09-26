%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dispatch_integration_tests).
-typing([eqwalizer]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

base_state(Opts) ->
    maps:merge(
        #{
            seq => 0,
            user_id => 1,
            buffer => limited_deque:new(4096, 16777216),
            buffer_bytes => 0,
            socket_pid => undefined,
            channels => #{},
            relationships => #{},
            suppress_presence_updates => false,
            pending_presences => [],
            presence_pid => undefined,
            ignored_events => #{},
            debounce_reactions => false,
            reaction_buffer => [],
            reaction_buffer_timer => undefined
        },
        Opts
    ).

one_to_one_dm_presence_with_attached_guild_id_not_buffered_test() ->
    State0 = base_state(#{
        guilds => #{123 => connected},
        channels => #{
            100 => #{
                <<"id">> => <<"100">>,
                <<"type">> => 1,
                <<"recipients">> => [#{<<"id">> => <<"2">>}]
            }
        }
    }),
    Presence = #{
        <<"guild_id">> => <<"123">>,
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"status">> => <<"idle">>
    },
    {noreply, State1} = session_dispatch:handle_dispatch(presence_update, Presence, State0),
    ?assertEqual([], maps:get(pending_presences, State1, [])),
    ?assertEqual(1, limited_deque:size(maps:get(buffer, State1))).

one_to_one_dm_presence_with_unattached_guild_id_buffered_test() ->
    State0 = base_state(#{
        guilds => #{456 => connected},
        channels => #{
            100 => #{
                <<"id">> => <<"100">>,
                <<"type">> => 1,
                <<"recipients">> => [#{<<"id">> => <<"2">>}]
            }
        }
    }),
    Presence = #{
        <<"guild_id">> => <<"123">>,
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"status">> => <<"idle">>
    },
    {noreply, State1} = session_dispatch:handle_dispatch(presence_update, Presence, State0),
    ?assertEqual(1, pending_presence_count(State1)),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))).

presence_update_without_guild_id_buffered_for_non_relationship_test() ->
    State0 = base_state(#{}),
    Presence = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"status">> => <<"online">>},
    {noreply, State1} = session_dispatch:handle_dispatch(presence_update, Presence, State0),
    ?assertEqual(1, pending_presence_count(State1)),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))).

presence_update_without_guild_id_not_buffered_for_relationship_test() ->
    State0 = base_state(#{relationships => #{2 => 1}}),
    Presence = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"status">> => <<"online">>},
    {noreply, State1} = session_dispatch:handle_dispatch(presence_update, Presence, State0),
    ?assertEqual([], maps:get(pending_presences, State1, [])),
    ?assertEqual(1, limited_deque:size(maps:get(buffer, State1))).

presence_update_buffered_for_outgoing_request_relationship_test() ->
    State0 = base_state(#{relationships => #{2 => 4}}),
    Presence = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"status">> => <<"online">>},
    {noreply, State1} = session_dispatch:handle_dispatch(presence_update, Presence, State0),
    ?assertEqual(1, pending_presence_count(State1)),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))).

presence_update_buffered_for_incoming_request_test() ->
    State0 = base_state(#{relationships => #{2 => 3}}),
    Presence = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"status">> => <<"online">>},
    {noreply, State1} = session_dispatch:handle_dispatch(presence_update, Presence, State0),
    ?assertEqual(1, pending_presence_count(State1)),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))).

relationship_mutations_keep_presence_buffer_valid_test() ->
    State0 = base_state(#{}),
    Friend = #{
        <<"id">> => <<"2">>, <<"type">> => 1, <<"user">> => #{<<"id">> => <<"2">>}
    },
    {noreply, State1} = session_dispatch:handle_dispatch(
        relationship_add, Friend, State0
    ),
    Online = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"status">> => <<"online">>},
    {noreply, State2} = session_dispatch:handle_dispatch(presence_update, Online, State1),
    ?assertEqual(0, pending_presence_count(State2)),
    Blocked = Friend#{<<"type">> => 2},
    {noreply, State3} = session_dispatch:handle_dispatch(
        relationship_update, Blocked, State2
    ),
    Offline = Online#{<<"status">> => <<"offline">>},
    {noreply, State4} = session_dispatch:handle_dispatch(presence_update, Offline, State3),
    ?assertEqual(1, pending_presence_count(State4)),
    {noreply, State5} = session_dispatch:handle_dispatch(
        relationship_add, Friend, State4
    ),
    ?assertEqual(0, pending_presence_count(State5)),
    {noreply, State6} = session_dispatch:handle_dispatch(
        relationship_remove, #{<<"id">> => <<"2">>}, State5
    ),
    {noreply, State7} = session_dispatch:handle_dispatch(presence_update, Offline, State6),
    ?assertEqual(1, pending_presence_count(State7)).

presence_update_skips_sync_presence_targets_test() ->
    State0 = base_state(#{relationships => #{2 => 1}}),
    Presence = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"status">> => <<"online">>},
    {noreply, State1} = session_dispatch:handle_dispatch(presence_update, Presence, State0),
    ?assertEqual(1, limited_deque:size(maps:get(buffer, State1))).

pre_encoded_not_buffered_test() ->
    State0 = base_state(#{}),
    Data = {pre_encoded, <<"[{\"test\":true}]">>},
    {noreply, State1} = session_dispatch:handle_dispatch(
        guild_member_list_update, Data, State0
    ),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))),
    ?assertEqual(1, maps:get(seq, State1)).

pre_encoded_voice_state_is_buffered_for_replay_test() ->
    State0 = base_state(#{}),
    Data = {pre_encoded, <<"{\"user_id\":\"1\",\"channel_id\":\"2\"}">>},
    {noreply, State1} = session_dispatch:handle_dispatch(voice_state_update, Data, State0),
    ?assertEqual(1, limited_deque:size(maps:get(buffer, State1))),
    [Entry] = limited_deque:to_list(maps:get(buffer, State1)),
    ?assertEqual(voice_state_update, maps:get(event, Entry)),
    ?assertEqual(Data, maps:get(data, Entry)),
    ?assertEqual(1, maps:get(seq, Entry)).

pre_encoded_message_create_is_buffered_for_replay_test() ->
    State0 = base_state(#{}),
    Data = {pre_encoded, <<"{\"id\":\"9\",\"content\":\"hi\"}">>},
    {noreply, State1} = session_dispatch:handle_dispatch(message_create, Data, State0),
    ?assertEqual(1, limited_deque:size(maps:get(buffer, State1))),
    [Entry] = limited_deque:to_list(maps:get(buffer, State1)),
    ?assertEqual(message_create, maps:get(event, Entry)),
    ?assertEqual(Data, maps:get(data, Entry)),
    ?assertEqual(1, maps:get(seq, Entry)).

pre_encoded_guild_sync_stays_out_of_replay_test() ->
    State0 = base_state(#{}),
    Data = {pre_encoded, <<"{\"id\":\"123\"}">>},
    {noreply, State1} = session_dispatch:handle_dispatch(guild_sync, Data, State0),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))),
    ?assertEqual(1, maps:get(seq, State1)).

pre_encoded_eviction_raises_replay_floor_test() ->
    State0 = base_state(#{buffer => limited_deque:new(2, 0)}),
    Data = {pre_encoded, <<"{\"content\":\"hi\"}">>},
    State3 = lists:foldl(
        fun(_N, S) ->
            {noreply, Next} = session_dispatch:handle_dispatch(message_create, Data, S),
            Next
        end,
        State0,
        lists:seq(1, 3)
    ),
    ?assertEqual(1, maps:get(replay_floor, State3)),
    ?assertEqual(
        [2, 3],
        [maps:get(seq, E) || E <- limited_deque:to_list(maps:get(buffer, State3))]
    ).

pre_encoded_member_list_stays_out_of_replay_test() ->
    State0 = base_state(#{}),
    Data = {pre_encoded, <<"[{\"test\":true}]">>},
    {noreply, State1} = session_dispatch:handle_dispatch(
        guild_member_list_update, Data, State0
    ),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))).

pre_encoded_increments_seq_test() ->
    State0 = base_state(#{seq => 10}),
    Data = {pre_encoded, <<"[{\"test\":true}]">>},
    {noreply, State1} = session_dispatch:handle_dispatch(message_create, Data, State0),
    ?assertEqual(11, maps:get(seq, State1)).

pre_encoded_multiple_events_seq_test() ->
    State0 = base_state(#{}),
    {noreply, S1} = session_dispatch:handle_dispatch(
        message_create, {pre_encoded, <<"{\"a\":1}">>}, State0
    ),
    {noreply, S2} = session_dispatch:handle_dispatch(
        message_create, {pre_encoded, <<"{\"b\":2}">>}, S1
    ),
    {noreply, S3} = session_dispatch:handle_dispatch(
        message_create, {pre_encoded, <<"{\"c\":3}">>}, S2
    ),
    ?assertEqual(3, maps:get(seq, S3)),
    ?assertEqual(3, limited_deque:size(maps:get(buffer, S3))),
    ?assertEqual(
        [1, 2, 3],
        [maps:get(seq, E) || E <- limited_deque:to_list(maps:get(buffer, S3))]
    ).

pre_encoded_sends_to_socket_test() ->
    State0 = base_state(#{socket_pid => self()}),
    {noreply, _} = dispatch_pre_encoded(message_create, <<"{\"content\":\"hello\"}">>, State0),
    receive
        {dispatch, message_create, {pre_encoded, _}, 1} -> ok
    after 100 -> ?assert(false, dispatch_not_received)
    end.

pre_encoded_data_matches_original_test() ->
    State0 = base_state(#{socket_pid => self()}),
    Json = <<"{\"content\":\"hello world\"}">>,
    {noreply, _} = session_dispatch:handle_dispatch(
        message_create, {pre_encoded, Json}, State0
    ),
    receive
        {dispatch, message_create, {pre_encoded, R}, _} -> ?assertEqual(Json, R)
    after 100 -> ?assert(false, dispatch_not_received)
    end.

pre_encoded_channel_create_updates_channels_test() ->
    State0 = base_state(#{}),
    ChannelData = #{<<"id">> => <<"12345">>, <<"type">> => 1, <<"recipients">> => []},
    Encoded = {pre_encoded, iolist_to_binary(json:encode(ChannelData))},
    {noreply, State1} = session_dispatch:handle_dispatch(channel_create, Encoded, State0),
    ?assert(maps:is_key(12345, maps:get(channels, State1, #{}))).

pre_encoded_channel_delete_updates_channels_test() ->
    State0 = base_state(#{channels => #{12345 => #{<<"id">> => <<"12345">>, <<"type">> => 1}}}),
    Encoded = {pre_encoded, iolist_to_binary(json:encode(#{<<"id">> => <<"12345">>}))},
    {noreply, State1} = session_dispatch:handle_dispatch(channel_delete, Encoded, State0),
    ?assertEqual(false, maps:is_key(12345, maps:get(channels, State1, #{}))).

pre_encoded_message_create_does_not_alter_channels_test() ->
    State0 = base_state(#{channels => #{}}),
    MsgData = #{
        <<"id">> => <<"99999">>, <<"channel_id">> => <<"12345">>, <<"content">> => <<"hi">>
    },
    Encoded = {pre_encoded, iolist_to_binary(json:encode(MsgData))},
    {noreply, State1} = session_dispatch:handle_dispatch(message_create, Encoded, State0),
    ?assertEqual(#{}, maps:get(channels, State1, #{})).

pre_encoded_ignored_event_skipped_test() ->
    State0 = base_state(#{ignored_events => #{<<"MESSAGE_CREATE">> => true}}),
    {noreply, State1} = dispatch_pre_encoded(
        message_create, <<"{\"content\":\"hello\"}">>, State0
    ),
    ?assertEqual(0, maps:get(seq, State1)).

pre_encoded_roundtrip_integrity_test() ->
    OriginalData = #{
        <<"guild_id">> => <<"123">>,
        <<"members">> => [
            #{<<"id">> => <<"1">>, <<"nick">> => <<"Alice">>},
            #{<<"id">> => <<"2">>, <<"nick">> => <<"Bob">>}
        ],
        <<"ops">> => [#{<<"op">> => <<"SYNC">>, <<"range">> => [0, 99]}]
    },
    ?assertEqual(OriginalData, json:decode(iolist_to_binary(json:encode(OriginalData)))).

guild_counts_update_reaches_nonzero_shard_test() ->
    drain_mailbox(),
    State0 = base_state(#{socket_pid => self(), shard => {1, 2}}),
    Data = #{counts => [], nonce => <<"n">>},
    {noreply, State1} = session_dispatch:handle_dispatch(guild_counts_update, Data, State0),
    ?assertEqual(1, maps:get(seq, State1)),
    receive
        {dispatch, guild_counts_update, _Payload, 1} -> ok
    after 100 -> ?assert(false, dispatch_not_received)
    end.

rate_limited_reaches_nonzero_shard_test() ->
    drain_mailbox(),
    State0 = base_state(#{socket_pid => self(), shard => {1, 2}}),
    Data = #{opcode => 8, retry_after => 12.5, meta => #{guild_id => <<"1">>}},
    {noreply, State1} = session_dispatch:handle_dispatch(rate_limited, Data, State0),
    ?assertEqual(1, maps:get(seq, State1)),
    receive
        {dispatch, rate_limited, _Payload, 1} -> ok
    after 100 -> ?assert(false, dispatch_not_received)
    end.

channel_member_counts_update_reaches_nonzero_shard_test() ->
    drain_mailbox(),
    State0 = base_state(#{socket_pid => self(), shard => {1, 2}}),
    Data = #{counts => [], nonce => <<"n">>},
    {noreply, State1} = session_dispatch:handle_dispatch(
        channel_member_counts_update, Data, State0
    ),
    ?assertEqual(1, maps:get(seq, State1)),
    receive
        {dispatch, channel_member_counts_update, _Payload, 1} -> ok
    after 100 -> ?assert(false, dispatch_not_received)
    end.

guildless_presence_still_skipped_for_nonzero_shard_test() ->
    drain_mailbox(),
    State0 = base_state(#{socket_pid => self(), shard => {1, 2}}),
    {noreply, State1} = session_dispatch:handle_dispatch(
        presence_update, #{<<"user_id">> => <<"5">>}, State0
    ),
    ?assertEqual(0, maps:get(seq, State1)),
    receive
        {dispatch, _Event, _Data, _Seq} -> ?assert(false, unexpected_dispatch)
    after 100 -> ok
    end.

drain_mailbox() ->
    receive
        _Message -> drain_mailbox()
    after 0 -> ok
    end.

dispatch_pre_encoded(Event, Json, State) ->
    session_dispatch:handle_dispatch(Event, {pre_encoded, Json}, State).

pending_presence_count(State) ->
    Pending = maps:get(pending_presences, State, []),
    case is_list(Pending) of
        true -> length(Pending);
        false -> queue:len(Pending)
    end.

-endif.
