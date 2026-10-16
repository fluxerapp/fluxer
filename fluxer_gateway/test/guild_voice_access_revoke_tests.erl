%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_access_revoke_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD, 42).
-define(USER, 10).
-define(CHANNEL, 500).
-define(ROLE, 999).

overwrite_deny_view_connect_disconnects_connected_user_test() ->
    State = guild_state:update_state(channel_update, deny_overwrite_update(), base_state(true)),
    Messages = collect_sync_messages(),
    ?assert(disconnected(Messages)),
    ?assertNot(granted_permissions(Messages)),
    ?assert(guild_virtual_channel_access:has_virtual_access(?USER, ?CHANNEL, State)),
    ?assertNot(guild_virtual_channel_access:has_voice_access(?USER, ?CHANNEL, State)).

overwrite_deny_connect_only_disconnects_connected_user_test() ->
    Update = channel_update_with_user_deny(constants:connect_permission()),
    _ = guild_state:update_state(channel_update, Update, base_state(true)),
    ?assert(disconnected(collect_sync_messages())).

overwrite_deny_view_connect_disconnects_user_without_session_test() ->
    _ = guild_state:update_state(channel_update, deny_overwrite_update(), base_state(false)),
    ?assert(disconnected(collect_sync_messages())).

role_removal_disconnects_connected_user_test() ->
    Update = #{<<"user">> => #{<<"id">> => integer_to_binary(?USER)}, <<"roles">> => []},
    _ = guild_state:update_state(guild_member_update, Update, base_state(true)),
    Messages = collect_sync_messages(),
    ?assert(disconnected(Messages)),
    ?assertNot(granted_permissions(Messages)).

moderator_granted_access_survives_revoke_test() ->
    State0 = guild_virtual_channel_access:add_virtual_access(?USER, ?CHANNEL, base_state(true)),
    State = guild_state:update_state(channel_update, deny_overwrite_update(), State0),
    Messages = collect_sync_messages(),
    ?assertNot(disconnected(Messages)),
    ?assert(guild_virtual_channel_access:has_voice_access(?USER, ?CHANNEL, State)).

view_only_access_grants_no_publish_rights_test() ->
    ViewConnect = constants:view_channel_permission() bor constants:connect_permission(),
    State0 = with_role_permissions(ViewConnect, base_state(true)),
    State1 = guild_virtual_channel_access:add_view_only_access(?USER, ?CHANNEL, State0),
    State = with_overwrites([user_deny(ViewConnect)], State1),
    ?assertEqual(
        #{can_speak => false, can_stream => false, can_video => false},
        voice_utils:compute_voice_permissions(?USER, ?CHANNEL, State)
    ).

explicit_access_replaces_view_only_mark_test() ->
    State0 = guild_virtual_channel_access:add_view_only_access(?USER, ?CHANNEL, #{}),
    ?assert(guild_virtual_channel_access:is_view_only(?USER, ?CHANNEL, State0)),
    State1 = guild_virtual_channel_access:add_virtual_access(?USER, ?CHANNEL, State0),
    ?assertNot(guild_virtual_channel_access:is_view_only(?USER, ?CHANNEL, State1)),
    ?assert(guild_virtual_channel_access:has_voice_access(?USER, ?CHANNEL, State1)).

removing_access_clears_view_only_mark_test() ->
    State0 = guild_virtual_channel_access:add_view_only_access(?USER, ?CHANNEL, #{}),
    State1 = guild_virtual_channel_access:add_view_only_access(?USER, ?CHANNEL + 1, State0),
    State2 = guild_virtual_channel_access:remove_virtual_access(?USER, ?CHANNEL, State1),
    ?assertNot(guild_virtual_channel_access:is_view_only(?USER, ?CHANNEL, State2)),
    ?assert(guild_virtual_channel_access:is_view_only(?USER, ?CHANNEL + 1, State2)),
    State3 = guild_virtual_channel_access:remove_virtual_access(?USER, ?CHANNEL + 1, State2),
    ?assertNot(guild_virtual_channel_access:is_view_only(?USER, ?CHANNEL + 1, State3)).

channel_delete_disconnects_everyone_in_channel_test() ->
    Self = self(),
    VoicePid = spawn(fun() ->
        receive
            Message -> Self ! {voice_server_got, Message}
        end
    end),
    State0 = (base_state(true))#{voice_server_pid => VoicePid},
    _ = guild_state:update_state(
        channel_delete,
        #{<<"id">> => integer_to_binary(?CHANNEL), <<"type">> => 2},
        State0
    ),
    receive
        {voice_server_got, {'$gen_cast', {disconnect_all_voice_users_in_channel, Request}}} ->
            ?assertEqual(#{channel_id => ?CHANNEL}, Request)
    after 500 ->
        exit(VoicePid, kill),
        ?assert(false)
    end.

voice_server_disconnects_all_users_in_channel_test() ->
    Self = self(),
    ForceFun = fun(GId, ChId, UId, ConnId) ->
        Self ! {force_disconnect, GId, ChId, UId, ConnId},
        {ok, #{success => true}}
    end,
    GuildPid = spawn(fun() -> guild_state_reply_loop(ForceFun) end),
    State = #{
        guild_id => ?GUILD,
        guild_pid => GuildPid,
        voice_states => #{
            <<"conn-a">> => voice_state(<<"conn-a">>, 5, ?CHANNEL),
            <<"conn-b">> => voice_state(<<"conn-b">>, 6, ?CHANNEL),
            <<"conn-c">> => voice_state(<<"conn-c">>, 7, ?CHANNEL + 1)
        },
        pending_voice_connections => #{},
        recently_disconnected_voice_states => #{},
        e2ee_room_keys => #{}
    },
    try
        {noreply, NewState} = guild_voice_server:handle_cast(
            {disconnect_all_voice_users_in_channel, #{channel_id => ?CHANNEL}}, State
        ),
        ?assertEqual([<<"conn-c">>], maps:keys(maps:get(voice_states, NewState))),
        Disconnected = lists:sort([receive_force_disconnect(), receive_force_disconnect()]),
        ?assertEqual(
            [{?GUILD, ?CHANNEL, 5, <<"conn-a">>}, {?GUILD, ?CHANNEL, 6, <<"conn-b">>}],
            Disconnected
        )
    after
        exit(GuildPid, kill)
    end.

receive_force_disconnect() ->
    receive
        {force_disconnect, GId, ChId, UId, ConnId} -> {GId, ChId, UId, ConnId}
    after 500 -> erlang:error(missing_force_disconnect)
    end.

guild_state_reply_loop(ForceFun) ->
    GuildState = #{
        id => ?GUILD,
        data => #{<<"guild">> => #{<<"owner_id">> => <<"1">>}},
        sessions => #{},
        test_force_disconnect_fun => ForceFun
    },
    receive
        {'$gen_call', From, {get_voice_guild_state}} ->
            gen_server:reply(From, GuildState),
            guild_state_reply_loop(ForceFun);
        _ ->
            guild_state_reply_loop(ForceFun)
    end.

voice_state(ConnId, UserId, ChannelId) ->
    #{
        <<"guild_id">> => integer_to_binary(?GUILD),
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnId
    }.

base_state(WithSession) ->
    ok = drain(),
    Self = self(),
    SyncFun = fun(GId, ChId, UId, ConnId, Perms) ->
        Self ! {synced, GId, ChId, UId, ConnId, Perms}
    end,
    Sessions =
        case WithSession of
            true ->
                #{
                    <<"sess">> => #{
                        session_id => <<"sess">>,
                        user_id => ?USER,
                        pid => spawn(fun sink/0),
                        viewable_channels => #{?CHANNEL => true}
                    }
                };
            false ->
                #{}
        end,
    RolePerms =
        constants:view_channel_permission() bor constants:connect_permission() bor
            constants:speak_permission(),
    #{
        id => ?GUILD,
        sessions => Sessions,
        voice_states => #{
            <<"conn">> => (voice_state(<<"conn">>, ?USER, ?CHANNEL))#{<<"deaf">> => false}
        },
        member_list_subscriptions => guild_member_list_subs:new(),
        test_permission_sync_fun => SyncFun,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => roles(RolePerms),
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(?USER)},
                    <<"roles">> => [integer_to_binary(?ROLE)]
                }
            ],
            <<"channels">> => [voice_channel([])]
        }
    }.

voice_channel(Overwrites) ->
    #{
        <<"id">> => integer_to_binary(?CHANNEL),
        <<"type">> => 2,
        <<"permission_overwrites">> => Overwrites
    }.

with_overwrites(Overwrites, State) ->
    Data = maps:get(data, State),
    State#{data => Data#{<<"channels">> => [voice_channel(Overwrites)]}}.

with_role_permissions(Permissions, State) ->
    Data = maps:get(data, State),
    State#{data => Data#{<<"roles">> => roles(Permissions)}}.

roles(Permissions) ->
    [
        #{
            <<"id">> => integer_to_binary(?ROLE),
            <<"permissions">> => integer_to_binary(Permissions)
        },
        #{<<"id">> => integer_to_binary(?GUILD), <<"permissions">> => <<"0">>}
    ].

user_deny(Deny) ->
    #{
        <<"id">> => integer_to_binary(?USER),
        <<"type">> => 1,
        <<"allow">> => <<"0">>,
        <<"deny">> => integer_to_binary(Deny)
    }.

channel_update_with_user_deny(Deny) ->
    voice_channel([user_deny(Deny)]).

deny_overwrite_update() ->
    channel_update_with_user_deny(
        constants:view_channel_permission() bor constants:connect_permission()
    ).

sink() ->
    receive
        stop -> ok;
        _ -> sink()
    end.

drain() ->
    receive
        _ -> drain()
    after 0 -> ok
    end.

collect_sync_messages() ->
    collect_sync_messages([]).

collect_sync_messages(Acc) ->
    receive
        {synced, _, _, _, _, _} = Message -> collect_sync_messages([Message | Acc])
    after 300 -> lists:reverse(Acc)
    end.

disconnected(Messages) ->
    lists:any(
        fun({synced, _, _, _, _, Perms}) -> maps:get(disconnected, Perms, false) =:= true end,
        Messages
    ).

granted_permissions(Messages) ->
    lists:any(
        fun({synced, _, _, _, _, Perms}) -> maps:is_key(can_speak, Perms) end,
        Messages
    ).
