%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_passive_sync_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

compute_delta_empty_previous_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Previous = #{},
    Delta = guild_passive_sync:compute_delta(Current, Previous),
    ?assertEqual(Current, Delta).

compute_delta_no_changes_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = guild_passive_sync:compute_delta(Current, Previous),
    ?assertEqual(#{}, Delta).

compute_delta_partial_changes_test() ->
    Current = #{<<"1">> => <<"101">>, <<"2">> => <<"200">>, <<"3">> => <<"300">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = guild_passive_sync:compute_delta(Current, Previous),
    ?assertEqual(#{<<"1">> => <<"101">>, <<"3">> => <<"300">>}, Delta).

compute_delta_only_new_channels_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>, <<"3">> => <<"300">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = guild_passive_sync:compute_delta(Current, Previous),
    ?assertEqual(#{<<"3">> => <<"300">>}, Delta).

compute_delta_ignores_removed_channels_test() ->
    Current = #{<<"1">> => <<"100">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = guild_passive_sync:compute_delta(Current, Previous),
    ?assertEqual(#{}, Delta).

compute_channel_diffs_detects_created_updated_deleted_test() ->
    Current = #{<<"1">> => 2, <<"2">> => 1},
    Previous = #{<<"1">> => 1, <<"3">> => 9},
    {Created, Updated, Deleted} = guild_passive_sync:compute_channel_diffs(Current, Previous),
    ?assertEqual([<<"2">>], lists:sort(Created)),
    ?assertEqual([<<"1">>], lists:sort(Updated)),
    ?assertEqual([<<"3">>], lists:sort(Deleted)).

send_passive_updates_uses_permission_capable_state_test() ->
    passive_sync_registry:init(),
    GuildId = 1427764661718740994,
    UserId = 1130650140672000000,
    SessionId = <<"passive-session">>,
    ChannelId = 1497639278555484216,
    MessageId = 1500000000000000000,
    State = #{
        id => GuildId,
        member_count => 300,
        sessions => #{
            SessionId => #{
                session_id => SessionId,
                user_id => UserId,
                pid => self(),
                active_guilds => sets:new(),
                bot => false
            }
        },
        data => #{
            <<"guild">> => #{<<"id">> => GuildId, <<"owner_id">> => UserId},
            <<"channels">> => [#{<<"id">> => ChannelId, <<"last_message_id">> => MessageId}],
            <<"members">> => [#{<<"user">> => #{<<"id">> => UserId}, <<"roles">> => []}],
            <<"roles">> => []
        },
        voice_states => #{}
    },
    guild_passive_sync:send_passive_updates_to_sessions(State),
    RegState = passive_sync_registry:lookup(SessionId, GuildId),
    ?assertEqual(
        #{integer_to_binary(ChannelId) => integer_to_binary(MessageId)},
        maps:get(previous_passive_updates, RegState)
    ).

send_passive_updates_uses_current_channel_index_and_message_only_payload_test() ->
    flush_dispatches(),
    passive_sync_registry:init(),
    GuildId = 1427764661718740994,
    UserId = 1130650140672000000,
    SessionId = <<"passive-session-current">>,
    ChannelId = 1497639278555484216,
    OldMessageId = 1500000000000000000,
    NewMessageId = 1500000000000000001,
    Data0 = guild_data_index:normalize_map(#{
        <<"guild">> => #{<<"id">> => GuildId, <<"owner_id">> => UserId},
        <<"channels">> => [#{<<"id">> => ChannelId, <<"last_message_id">> => OldMessageId}],
        <<"members">> => [#{<<"user">> => #{<<"id">> => UserId}, <<"roles">> => []}],
        <<"roles">> => []
    }),
    ChannelIndex0 = maps:get(<<"channel_index">>, Data0),
    Channel0 = maps:get(ChannelId, ChannelIndex0),
    Data = Data0#{
        <<"channel_index">> => ChannelIndex0#{
            ChannelId => Channel0#{<<"last_message_id">> => NewMessageId}
        },
        channels_stale => true
    },
    passive_sync_registry:store(SessionId, GuildId, #{
        previous_passive_updates => #{
            integer_to_binary(ChannelId) => integer_to_binary(OldMessageId)
        },
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    }),
    State = #{
        id => GuildId,
        member_count => 300,
        sessions => #{
            SessionId => #{
                session_id => SessionId,
                user_id => UserId,
                pid => self(),
                active_guilds => sets:new(),
                bot => false
            }
        },
        data => Data,
        voice_states => #{}
    },
    guild_passive_sync:send_passive_updates_to_sessions(State),
    Payload = receive_passive_payload(),
    ?assertEqual(
        #{integer_to_binary(ChannelId) => integer_to_binary(NewMessageId)},
        maps:get(<<"channels">>, Payload)
    ),
    ?assertNot(maps:is_key(<<"created_channels">>, Payload)),
    ?assertNot(maps:is_key(<<"updated_channels">>, Payload)),
    ?assertNot(maps:is_key(<<"deleted_channel_ids">>, Payload)).

receive_passive_payload() ->
    receive
        {'$gen_cast', {dispatch, passive_updates, Payload}} when is_map(Payload) ->
            Payload
    after 1000 ->
        ?assert(false, passive_payload_not_received)
    end.

flush_dispatches() ->
    receive
        {'$gen_cast', {dispatch, passive_updates, _Payload}} ->
            flush_dispatches()
    after 0 ->
        ok
    end.

compute_voice_state_updates_reports_changes_test() ->
    PrevVoiceState = #{
        <<"connection_id">> => <<"conn1">>,
        <<"channel_id">> => <<"100">>,
        <<"user_id">> => <<"200">>,
        <<"version">> => 1
    },
    CurrentVoiceState = PrevVoiceState#{<<"version">> => 2},
    Current = #{<<"conn1">> => CurrentVoiceState},
    Previous = #{<<"conn1">> => PrevVoiceState},
    Updates = guild_passive_sync_voice:compute_voice_state_updates(Current, Previous, 999),
    ?assertEqual(1, length(Updates)),
    Update = hd(Updates),
    ?assertEqual(<<"conn1">>, maps:get(<<"connection_id">>, Update)),
    ?assertEqual(integer_to_binary(999), maps:get(<<"guild_id">>, Update)).

compute_voice_state_updates_reports_removal_test() ->
    RemovedVoiceState = #{
        <<"connection_id">> => <<"conn2">>,
        <<"channel_id">> => <<"200">>,
        <<"user_id">> => <<"300">>,
        <<"version">> => 3
    },
    Current = #{},
    Previous = #{<<"conn2">> => RemovedVoiceState},
    Updates = guild_passive_sync_voice:compute_voice_state_updates(Current, Previous, 101),
    ?assertEqual(1, length(Updates)),
    Update = hd(Updates),
    ?assertEqual(null, maps:get(<<"channel_id">>, Update)),
    ?assertEqual(integer_to_binary(101), maps:get(<<"guild_id">>, Update)).

compute_voice_state_updates_sanitizes_internal_fields_test() ->
    PrevVoiceState = #{
        <<"connection_id">> => <<"conn1">>,
        <<"channel_id">> => <<"100">>,
        <<"user_id">> => <<"200">>,
        <<"version">> => 1
    },
    CurrentVoiceState = PrevVoiceState#{
        <<"version">> => 2,
        <<"latitude">> => <<"1.0">>,
        <<"longitude">> => <<"2.0">>,
        <<"region_id">> => <<"us-east">>,
        <<"server_id">> => <<"voice-1">>
    },
    Current = #{<<"conn1">> => CurrentVoiceState},
    Previous = #{<<"conn1">> => PrevVoiceState},
    Updates = guild_passive_sync_voice:compute_voice_state_updates(Current, Previous, 999),
    ?assertEqual(1, length(Updates)),
    Update = hd(Updates),
    ?assertNot(maps:is_key(<<"latitude">>, Update)),
    ?assertNot(maps:is_key(<<"longitude">>, Update)),
    ?assertNot(maps:is_key(<<"region_id">>, Update)),
    ?assertNot(maps:is_key(<<"server_id">>, Update)),
    ?assertEqual(2, maps:get(<<"version">>, Update)),
    ?assertEqual(false, maps:get(<<"suppress">>, Update)),
    ?assertEqual(null, maps:get(<<"member">>, Update)).
