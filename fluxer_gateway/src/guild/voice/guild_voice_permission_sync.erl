%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_permission_sync).
-typing([eqwalizer]).

-export([
    sync_user_voice_permissions/2,
    sync_all_voice_permissions_for_channel/2,
    sync_users_with_role/2,
    maybe_sync_permissions_on_role_update/2,
    maybe_sync_permissions_on_member_update/2
]).

-export_type([
    guild_state/0,
    voice_state/0,
    user_id/0,
    channel_id/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec sync_user_voice_permissions(user_id(), guild_state()) -> ok.
sync_user_voice_permissions(UserId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    case state_guild_id(State) of
        undefined ->
            ok;
        GuildId ->
            sync_matching_user_voice_states(GuildId, UserId, VoiceStates, State)
    end.

-spec sync_matching_user_voice_states(integer(), user_id(), map(), guild_state()) -> ok.
sync_matching_user_voice_states(GuildId, UserId, VoiceStates, State) ->
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            maybe_sync_user_voice_state(GuildId, UserId, VoiceState, State)
        end,
        VoiceStates
    ),
    ok.

-spec maybe_sync_user_voice_state(integer(), user_id(), voice_state(), guild_state()) -> ok.
maybe_sync_user_voice_state(GuildId, UserId, VoiceState, State) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        UserId -> sync_voice_state_permissions(GuildId, UserId, VoiceState, State);
        _ -> ok
    end.

-spec sync_all_voice_permissions_for_channel(channel_id(), guild_state()) -> ok.
sync_all_voice_permissions_for_channel(ChannelId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    case state_guild_id(State) of
        undefined ->
            ok;
        GuildId ->
            do_sync_channel_permissions(GuildId, ChannelId, VoiceStates, State)
    end.

-spec do_sync_channel_permissions(integer(), channel_id(), map(), guild_state()) -> ok.
do_sync_channel_permissions(GuildId, ChannelId, VoiceStates, State) ->
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            maybe_sync_channel_voice_state(GuildId, ChannelId, VoiceState, State)
        end,
        VoiceStates
    ),
    ok.

-spec maybe_sync_channel_voice_state(integer(), channel_id(), voice_state(), guild_state()) ->
    ok.
maybe_sync_channel_voice_state(GuildId, ChannelId, VoiceState, State) ->
    case voice_state_utils:voice_state_channel_id(VoiceState) of
        ChannelId -> sync_channel_voice_state_permissions(GuildId, VoiceState, State);
        _ -> ok
    end.

-spec sync_channel_voice_state_permissions(integer(), voice_state(), guild_state()) -> ok.
sync_channel_voice_state_permissions(GuildId, VoiceState, State) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        undefined -> ok;
        UserId -> sync_voice_state_permissions(GuildId, UserId, VoiceState, State)
    end.

-spec maybe_sync_permissions_on_role_update(map(), guild_state()) -> ok.
maybe_sync_permissions_on_role_update(RoleUpdate, State) ->
    RoleId = snowflake_id:parse_optional(maps:get(<<"id">>, RoleUpdate, undefined)),
    case RoleId of
        undefined -> ok;
        RoleIdInt -> sync_users_with_role(RoleIdInt, State)
    end.

-spec maybe_sync_permissions_on_member_update(map(), guild_state()) -> ok.
maybe_sync_permissions_on_member_update(MemberUpdate, State) ->
    UserId = get_member_user_id(MemberUpdate),
    case UserId of
        undefined ->
            ok;
        _ ->
            sync_user_voice_permissions(UserId, State)
    end.

-spec sync_voice_state_permissions(integer(), user_id(), voice_state(), guild_state()) -> ok.
sync_voice_state_permissions(GuildId, UserId, VoiceState, State) ->
    ChannelId = voice_state_utils:voice_state_channel_id(VoiceState),
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case {ChannelId, ConnectionId} of
        {undefined, _} ->
            ok;
        {_, undefined} ->
            ok;
        {ChId, ConnId} when is_integer(ChId), is_binary(ConnId) ->
            sync_validated_voice_state(GuildId, UserId, ChId, ConnId, VoiceState, State)
    end.

-spec sync_validated_voice_state(
    integer(), user_id(), channel_id(), binary(), voice_state(), guild_state()
) -> ok.
sync_validated_voice_state(GuildId, UserId, ChId, ConnId, VoiceState, State) ->
    case user_has_base_voice_access(UserId, ChId, State) of
        true ->
            VoicePermissions0 = voice_utils:compute_voice_permissions(UserId, ChId, State),
            VoicePermissions = VoicePermissions0#{
                deaf => maps:get(<<"deaf">>, VoiceState, false)
            },
            ok = maybe_clear_self_stream(ChId, VoiceState, VoicePermissions, State),
            dispatch_permission_update(
                GuildId, ChId, UserId, ConnId, VoicePermissions, State
            );
        false ->
            dispatch_force_disconnect(GuildId, ChId, UserId, ConnId, State)
    end.

-spec maybe_clear_self_stream(
    channel_id(), voice_state(), voice_utils:voice_permissions(), guild_state()
) -> ok.
maybe_clear_self_stream(ChId, VoiceState, #{can_stream := false}, State) ->
    SelfStream = maps:get(<<"self_stream">>, VoiceState, false),
    SelfVideo = maps:get(<<"self_video">>, VoiceState, false),
    case SelfStream =:= true orelse SelfVideo =:= true of
        false ->
            ok;
        true ->
            Cleared = VoiceState#{
                <<"self_stream">> => false,
                <<"self_video">> => false
            },
            guild_voice_broadcast:broadcast_voice_state_update(
                Cleared, State, integer_to_binary(ChId)
            )
    end;
maybe_clear_self_stream(_ChId, _VoiceState, _VoicePermissions, _State) ->
    ok.

-spec user_has_base_voice_access(user_id(), channel_id(), guild_state()) -> boolean().
user_has_base_voice_access(UserId, ChannelId, State) ->
    case guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) of
        true ->
            true;
        false ->
            Permissions = guild_permissions:get_member_permissions(UserId, ChannelId, State),
            ViewPerm = constants:view_channel_permission(),
            ConnectPerm = constants:connect_permission(),
            HasView = permission_bits:has(Permissions, ViewPerm),
            HasConnect = permission_bits:has(Permissions, ConnectPerm),
            HasView andalso HasConnect
    end.

-spec dispatch_force_disconnect(
    integer(), channel_id(), user_id(), binary(), guild_state()
) -> ok.
dispatch_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State) ->
    case maps:get(test_permission_sync_fun, State, undefined) of
        Fun when is_function(Fun, 5) ->
            _ = Fun(GuildId, ChannelId, UserId, ConnectionId, #{disconnected => true}),
            ok;
        _ ->
            spawn_force_disconnect(GuildId, ChannelId, UserId, ConnectionId),
            ok
    end.

-spec spawn_force_disconnect(integer(), channel_id(), user_id(), binary()) -> pid().
spawn_force_disconnect(GuildId, ChannelId, UserId, ConnectionId) ->
    spawn(fun() ->
        guild_voice_disconnect:force_disconnect_participant(
            GuildId, ChannelId, UserId, ConnectionId
        )
    end).

-spec dispatch_permission_update(
    integer(), channel_id(), user_id(), binary(), voice_utils:voice_permissions(), guild_state()
) ->
    ok.
dispatch_permission_update(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions, State) ->
    case maps:get(test_permission_sync_fun, State, undefined) of
        Fun when is_function(Fun, 5) ->
            _ = Fun(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions),
            ok;
        _ ->
            spawn_permission_update(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions),
            ok
    end.

-spec spawn_permission_update(
    integer(), channel_id(), user_id(), binary(), voice_utils:voice_permissions()
) -> pid().
spawn_permission_update(GuildId, ChannelId, UserId, ConnId, VoicePerms) ->
    spawn(fun() ->
        enforce_voice_permissions_in_livekit(
            GuildId, ChannelId, UserId, ConnId, VoicePerms
        )
    end).

-spec enforce_voice_permissions_in_livekit(
    integer(), channel_id(), user_id(), binary(), voice_utils:voice_permissions()
) -> ok.
enforce_voice_permissions_in_livekit(
    GuildId, ChannelId, UserId, ConnectionId, VoicePermissions
) ->
    Req = voice_utils:build_update_participant_permissions_rpc_request(
        GuildId, ChannelId, UserId, ConnectionId, VoicePermissions
    ),
    case rpc_client:call(Req) of
        {ok, _Data} ->
            ok;
        {error, _Reason} ->
            ok
    end.

-spec sync_users_with_role(integer(), guild_state()) -> ok.
sync_users_with_role(RoleId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    case state_guild_id(State) of
        undefined ->
            ok;
        GuildId ->
            do_sync_users_with_role(GuildId, RoleId, VoiceStates, State)
    end.

-spec do_sync_users_with_role(integer(), integer(), map(), guild_state()) -> ok.
do_sync_users_with_role(GuildId, RoleId, VoiceStates, State) ->
    RoleUsers = user_ids_with_role(RoleId, State),
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            maybe_sync_role_voice_state(GuildId, RoleUsers, VoiceState, State)
        end,
        VoiceStates
    ),
    ok.

-spec maybe_sync_role_voice_state(integer(), sets:set(user_id()), voice_state(), guild_state()) ->
    ok.
maybe_sync_role_voice_state(GuildId, RoleUsers, VoiceState, State) ->
    UserId = voice_state_utils:voice_state_user_id(VoiceState),
    case UserId of
        undefined ->
            ok;
        _ ->
            sync_voice_state_permissions_if_role_matches(
                GuildId, RoleUsers, UserId, VoiceState, State
            )
    end.

-spec sync_voice_state_permissions_if_role_matches(
    integer(), sets:set(user_id()), user_id(), voice_state(), guild_state()
) -> ok.
sync_voice_state_permissions_if_role_matches(GuildId, RoleUsers, UserId, VoiceState, State) ->
    case sets:is_element(UserId, RoleUsers) of
        true -> sync_voice_state_permissions(GuildId, UserId, VoiceState, State);
        false -> ok
    end.

-spec user_ids_with_role(integer(), guild_state()) -> sets:set(user_id()).
user_ids_with_role(RoleId, State) ->
    Data = maps:get(data, State, #{}),
    RoleIndex = guild_data_index:member_role_index(Data),
    sets:from_list(maps:keys(maps:get(RoleId, RoleIndex, #{}))).

-spec get_member_user_id(map()) -> user_id() | undefined.
get_member_user_id(MemberUpdate) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, MemberUpdate, #{})),
    guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(<<"id">>, User, undefined)
    ).

-spec state_guild_id(guild_state()) -> integer() | undefined.
state_guild_id(State) ->
    guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(id, State, undefined)
    ).

-ifdef(TEST).

build_sync_test_state(TestFun) ->
    UserId = 10,
    ChannelId = 500,
    GuildId = 42,
    RoleId = 999,
    VoiceState = #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => <<"test-conn">>,
        <<"deaf">> => true
    },
    Permissions =
        constants:view_channel_permission() bor
            constants:connect_permission() bor
            constants:speak_permission() bor
            constants:stream_permission(),
    #{
        id => GuildId,
        voice_states => #{<<"conn">> => VoiceState},
        test_permission_sync_fun => TestFun,
        data => build_sync_test_data(GuildId, RoleId, UserId, ChannelId, Permissions)
    }.

build_sync_test_data(GuildId, RoleId, UserId, ChannelId, Permissions) ->
    RoleIdBin = integer_to_binary(RoleId),
    PermsBin = integer_to_binary(Permissions),
    GuildIdBin = integer_to_binary(GuildId),
    UserIdBin = integer_to_binary(UserId),
    ChIdBin = integer_to_binary(ChannelId),
    #{
        <<"guild">> => #{<<"owner_id">> => <<"1">>},
        <<"roles">> => [
            #{<<"id">> => RoleIdBin, <<"permissions">> => PermsBin},
            #{<<"id">> => GuildIdBin, <<"permissions">> => <<"0">>}
        ],
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => UserIdBin}, <<"roles">> => [RoleIdBin]}
        ],
        <<"channels">> => [
            #{<<"id">> => ChIdBin, <<"permission_overwrites">> => []}
        ]
    }.

sync_user_voice_permissions_syncs_connected_user_test() ->
    ok = drain_mailbox(),
    TestFun = make_sync_test_fun(),
    State = build_sync_test_state(TestFun),
    ok = sync_user_voice_permissions(10, State),
    receive
        {synced, 42, 500, 10, <<"test-conn">>, Perms} ->
            ?assertEqual(true, maps:get(can_speak, Perms)),
            ?assertEqual(true, maps:get(can_stream, Perms)),
            ?assertEqual(true, maps:get(deaf, Perms))
    after 100 ->
        ?assert(false)
    end.

sync_user_voice_permissions_no_voice_state_test() ->
    State = #{
        id => 42,
        voice_states => #{}
    },
    ok = sync_user_voice_permissions(10, State).

maybe_sync_permissions_on_member_update_no_role_change_test() ->
    State = #{id => 42, voice_states => #{}},
    MemberUpdate = #{
        <<"user">> => #{<<"id">> => <<"10">>},
        <<"roles">> => [<<"1">>]
    },
    ?assertEqual(ok, maybe_sync_permissions_on_member_update(MemberUpdate, State)).

maybe_sync_permissions_on_role_update_uses_role_index_test() ->
    TestFun = make_sync_test_fun(),
    GuildId = 42,
    RoleId = 999,
    OtherRoleId = 1000,
    UserId = 10,
    OtherUserId = 20,
    ChannelId = 500,
    Permissions =
        constants:view_channel_permission() bor
            constants:connect_permission() bor
            constants:speak_permission(),
    State = #{
        id => GuildId,
        voice_states => #{
            <<"conn1">> => #{
                <<"user_id">> => integer_to_binary(UserId),
                <<"channel_id">> => integer_to_binary(ChannelId),
                <<"connection_id">> => <<"conn1">>
            },
            <<"conn2">> => #{
                <<"user_id">> => integer_to_binary(OtherUserId),
                <<"channel_id">> => integer_to_binary(ChannelId),
                <<"connection_id">> => <<"conn2">>
            }
        },
        test_permission_sync_fun => TestFun,
        data => role_sync_test_data(
            GuildId, RoleId, OtherRoleId, UserId, OtherUserId, ChannelId, Permissions
        )
    },
    ok = maybe_sync_permissions_on_role_update(
        #{<<"id">> => integer_to_binary(RoleId)}, State
    ),
    receive
        {synced, GuildId, ChannelId, UserId, <<"conn1">>, _Perms} -> ok
    after 200 ->
        ?assert(false)
    end,
    receive
        {synced, GuildId, ChannelId, OtherUserId, <<"conn2">>, _OtherPerms} ->
            ?assert(false)
    after 50 ->
        ok
    end.

sync_disconnects_when_connect_permission_lost_test() ->
    ok = drain_mailbox(),
    TestFun = make_sync_test_fun(),
    UserId = 10,
    ChannelId = 500,
    GuildId = 42,
    RoleId = 999,
    ViewOnly = constants:view_channel_permission(),
    State = build_sync_test_state_with_perms(
        TestFun, GuildId, RoleId, UserId, ChannelId, ViewOnly
    ),
    ok = sync_user_voice_permissions(UserId, State),
    receive
        {synced, GuildId, ChannelId, UserId, <<"test-conn">>, Perms} ->
            ?assertEqual(true, maps:get(disconnected, Perms, false))
    after 200 ->
        ?assert(false)
    end.

sync_clears_self_stream_when_stream_permission_lost_test() ->
    ok = drain_mailbox(),
    TestFun = make_sync_test_fun(),
    UserId = 10,
    ChannelId = 500,
    GuildId = 42,
    RoleId = 999,
    WithoutStream =
        constants:view_channel_permission() bor
            constants:connect_permission() bor
            constants:speak_permission(),
    State = build_streaming_sync_test_state(
        TestFun, GuildId, RoleId, UserId, ChannelId, WithoutStream
    ),
    ok = sync_user_voice_permissions(UserId, State),
    Broadcast = receive_voice_state_dispatch(),
    ?assertEqual(false, maps:get(<<"self_stream">>, Broadcast)),
    ?assertEqual(false, maps:get(<<"self_video">>, Broadcast)),
    ?assertEqual(<<"test-conn">>, maps:get(<<"connection_id">>, Broadcast)).

sync_keeps_self_stream_when_stream_permission_held_test() ->
    ok = drain_mailbox(),
    TestFun = make_sync_test_fun(),
    UserId = 10,
    ChannelId = 500,
    GuildId = 42,
    RoleId = 999,
    WithStream =
        constants:view_channel_permission() bor
            constants:connect_permission() bor
            constants:speak_permission() bor
            constants:stream_permission(),
    State = build_streaming_sync_test_state(
        TestFun, GuildId, RoleId, UserId, ChannelId, WithStream
    ),
    ok = sync_user_voice_permissions(UserId, State),
    ?assertEqual(undefined, receive_optional_voice_state_dispatch()).

build_streaming_sync_test_state(TestFun, GuildId, RoleId, UserId, ChannelId, Permissions) ->
    VoiceState = #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => <<"test-conn">>,
        <<"self_stream">> => true,
        <<"self_video">> => true
    },
    #{
        id => GuildId,
        voice_states => #{<<"conn">> => VoiceState},
        sessions => #{
            <<"s1">> => #{
                pid => self(),
                user_id => UserId,
                viewable_channels => #{ChannelId => true}
            }
        },
        test_permission_sync_fun => TestFun,
        data => build_sync_test_data(GuildId, RoleId, UserId, ChannelId, Permissions)
    }.

receive_voice_state_dispatch() ->
    case receive_optional_voice_state_dispatch() of
        undefined -> error(voice_state_update_not_received);
        Payload -> Payload
    end.

receive_optional_voice_state_dispatch() ->
    receive
        {'$gen_cast', {dispatch, voice_state_update, {pre_encoded, Bin}}} -> json:decode(Bin);
        {'$gen_cast', {dispatch, voice_state_update, Payload}} -> Payload
    after 200 ->
        undefined
    end.

make_sync_test_fun() ->
    Self = self(),
    fun(GId, ChId, UId, ConnId, Perms) ->
        Self ! {synced, GId, ChId, UId, ConnId, Perms}
    end.

build_sync_test_state_with_perms(TestFun, GuildId, RoleId, UserId, ChannelId, Permissions) ->
    VoiceState = #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => <<"test-conn">>,
        <<"deaf">> => false
    },
    #{
        id => GuildId,
        voice_states => #{<<"conn">> => VoiceState},
        test_permission_sync_fun => TestFun,
        data => build_sync_test_data(GuildId, RoleId, UserId, ChannelId, Permissions)
    }.

role_sync_test_data(
    GuildId, RoleId, OtherRoleId, UserId, OtherUserId, ChannelId, Permissions
) ->
    guild_data_index:normalize_data(#{
        <<"guild">> => #{<<"owner_id">> => <<"1">>},
        <<"roles">> => [
            #{
                <<"id">> => integer_to_binary(RoleId),
                <<"permissions">> => integer_to_binary(Permissions)
            },
            #{
                <<"id">> => integer_to_binary(OtherRoleId),
                <<"permissions">> => integer_to_binary(Permissions)
            },
            #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}
        ],
        <<"members">> => [
            #{
                <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                <<"roles">> => [integer_to_binary(RoleId)]
            },
            #{
                <<"user">> => #{<<"id">> => integer_to_binary(OtherUserId)},
                <<"roles">> => [integer_to_binary(OtherRoleId)]
            }
        ],
        <<"channels">> => [
            #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
        ]
    }).

drain_mailbox() ->
    receive
        _ -> drain_mailbox()
    after 0 ->
        ok
    end.

-endif.
