%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild_voice_permission_sync).

-export([
    sync_user_voice_permissions/2,
    sync_all_voice_permissions_for_channel/2,
    maybe_sync_permissions_on_role_update/2,
    maybe_sync_permissions_on_member_update/2
]).

-type guild_state() :: map().
-type voice_state() :: map().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec sync_user_voice_permissions(integer(), guild_state()) -> ok.
sync_user_voice_permissions(UserId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    GuildId = map_utils:get_integer(State, id, 0),

    UserVoiceStates = maps:filter(
        fun(_ConnId, VoiceState) ->
            voice_state_utils:voice_state_user_id(VoiceState) =:= UserId
        end,
        VoiceStates
    ),

    maps:foreach(
        fun(_ConnId, VoiceState) ->
            sync_voice_state_permissions(GuildId, UserId, VoiceState, State)
        end,
        UserVoiceStates
    ),
    ok.

-spec sync_all_voice_permissions_for_channel(integer(), guild_state()) -> ok.
sync_all_voice_permissions_for_channel(ChannelId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    GuildId = map_utils:get_integer(State, id, 0),

    ChannelVoiceStates = maps:filter(
        fun(_ConnId, VoiceState) ->
            voice_state_utils:voice_state_channel_id(VoiceState) =:= ChannelId
        end,
        VoiceStates
    ),

    maps:foreach(
        fun(_ConnId, VoiceState) ->
            UserId = voice_state_utils:voice_state_user_id(VoiceState),
            case UserId of
                undefined -> ok;
                _ -> sync_voice_state_permissions(GuildId, UserId, VoiceState, State)
            end
        end,
        ChannelVoiceStates
    ),
    ok.

-spec maybe_sync_permissions_on_role_update(map(), guild_state()) -> ok.
maybe_sync_permissions_on_role_update(RoleUpdate, State) ->
    RoleId = maps:get(<<"id">>, RoleUpdate, undefined),
    case RoleId of
        undefined ->
            ok;
        _ ->
            OldPermissions = maps:get(<<"old_permissions">>, RoleUpdate, 0),
            NewPermissions = maps:get(<<"permissions">>, RoleUpdate, 0),

            AdminPerm = constants:administrator_permission(),
            SpeakPerm = constants:speak_permission(),
            StreamPerm = constants:stream_permission(),
            VoicePerms = AdminPerm bor SpeakPerm bor StreamPerm,

            OldVoicePerms = OldPermissions band VoicePerms,
            NewVoicePerms = NewPermissions band VoicePerms,

            case OldVoicePerms =/= NewVoicePerms of
                true ->
                    sync_users_with_role(RoleId, State);
                false ->
                    ok
            end
    end.

-spec maybe_sync_permissions_on_member_update(map(), guild_state()) -> ok.
maybe_sync_permissions_on_member_update(MemberUpdate, State) ->
    UserId = get_member_user_id(MemberUpdate),
    case UserId of
        undefined ->
            ok;
        _ ->
            OldRoles = maps:get(<<"old_roles">>, MemberUpdate, []),
            NewRoles = maps:get(<<"roles">>, MemberUpdate, []),

            case OldRoles =/= NewRoles of
                true ->
                    sync_user_voice_permissions(UserId, State);
                false ->
                    ok
            end
    end.

-spec sync_voice_state_permissions(integer(), integer(), voice_state(), guild_state()) -> ok.
sync_voice_state_permissions(GuildId, UserId, VoiceState, State) ->
    ChannelId = voice_state_utils:voice_state_channel_id(VoiceState),
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),

    case {ChannelId, ConnectionId} of
        {undefined, _} ->
            ok;
        {_, undefined} ->
            ok;
        {ChId, ConnId} when is_integer(ChId), is_binary(ConnId) ->
            VoicePermissions = voice_utils:compute_voice_permissions(UserId, ChId, State),
            dispatch_permission_update(GuildId, ChId, UserId, ConnId, VoicePermissions, State)
    end.

-spec dispatch_permission_update(integer(), integer(), integer(), binary(), map(), guild_state()) ->
    ok.
dispatch_permission_update(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions, State) ->
    case maps:get(test_permission_sync_fun, State, undefined) of
        Fun when is_function(Fun, 5) ->
            Fun(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions);
        _ ->
            spawn(fun() ->
                enforce_voice_permissions_in_livekit(
                    GuildId, ChannelId, UserId, ConnectionId, VoicePermissions
                )
            end)
    end.

-spec enforce_voice_permissions_in_livekit(
    integer(), integer(), integer(), binary(), map()
) -> ok.
enforce_voice_permissions_in_livekit(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions) ->
    Req = voice_utils:build_update_participant_permissions_rpc_request(
        GuildId, ChannelId, UserId, ConnectionId, VoicePermissions
    ),
    case rpc_client:call(Req) of
        {ok, _Data} ->
            logger:debug(
                "[guild_voice_permission_sync] Synced voice permissions ~p",
                [
                    [
                        {guildId, GuildId},
                        {channelId, ChannelId},
                        {userId, UserId},
                        {connectionId, ConnectionId},
                        {permissions, VoicePermissions}
                    ]
                ]
            ),
            ok;
        {error, Reason} ->
            logger:warning(
                "[guild_voice_permission_sync] Failed to sync voice permissions ~p",
                [
                    [
                        {guildId, GuildId},
                        {channelId, ChannelId},
                        {userId, UserId},
                        {connectionId, ConnectionId},
                        {permissions, VoicePermissions},
                        {error, Reason}
                    ]
                ]
            ),
            ok
    end.

-spec sync_users_with_role(binary() | integer(), guild_state()) -> ok.
sync_users_with_role(RoleId, State) ->
    RoleIdBin = ensure_binary(RoleId),
    VoiceStates = voice_state_utils:voice_states(State),
    GuildId = map_utils:get_integer(State, id, 0),

    maps:foreach(
        fun(_ConnId, VoiceState) ->
            UserId = voice_state_utils:voice_state_user_id(VoiceState),
            case UserId of
                undefined ->
                    ok;
                _ ->
                    case user_has_role(UserId, RoleIdBin, State) of
                        true ->
                            sync_voice_state_permissions(GuildId, UserId, VoiceState, State);
                        false ->
                            ok
                    end
            end
        end,
        VoiceStates
    ),
    ok.

-spec user_has_role(integer(), binary(), guild_state()) -> boolean().
user_has_role(UserId, RoleIdBin, State) ->
    case guild_voice_member:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            Roles = maps:get(<<"roles">>, Member, []),
            lists:member(RoleIdBin, Roles)
    end.

-spec get_member_user_id(map()) -> integer() | undefined.
get_member_user_id(MemberUpdate) ->
    User = maps:get(<<"user">>, MemberUpdate, #{}),
    map_utils:get_integer(User, <<"id">>, undefined).

-spec ensure_binary(binary() | integer()) -> binary().
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_integer(Value) -> integer_to_binary(Value).

-ifdef(TEST).

sync_user_voice_permissions_syncs_connected_user_test() ->
    Self = self(),
    TestFun = fun(GuildId, ChannelId, UserId, ConnectionId, Permissions) ->
        Self ! {synced, GuildId, ChannelId, UserId, ConnectionId, Permissions}
    end,
    UserId = 10,
    ChannelId = 500,
    GuildId = 42,
    RoleId = 999,

    VoiceState = #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => <<"test-conn">>
    },

    Permissions =
        constants:view_channel_permission() bor
            constants:connect_permission() bor
            constants:speak_permission() bor
            constants:stream_permission(),

    State = #{
        id => GuildId,
        voice_states => #{<<"conn">> => VoiceState},
        test_permission_sync_fun => TestFun,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(RoleId),
                    <<"permissions">> => integer_to_binary(Permissions)
                },
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => <<"0">>
                }
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => []
                }
            ]
        }
    },
    ok = sync_user_voice_permissions(UserId, State),
    receive
        {synced, GuildId, ChannelId, UserId, <<"test-conn">>, Perms} ->
            ?assertEqual(true, maps:get(can_speak, Perms)),
            ?assertEqual(true, maps:get(can_stream, Perms))
    after 100 ->
        ?assert(false)
    end.

sync_user_voice_permissions_no_voice_state_test() ->
    State = #{
        id => 42,
        voice_states => #{}
    },
    ok = sync_user_voice_permissions(10, State).

-endif.
