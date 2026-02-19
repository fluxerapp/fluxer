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

-module(gateway_rpc_guild).

-export([execute_method/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MAX_BATCH_SIZE, 100).
-define(BATCH_TIMEOUT_MS, 5000).
-define(GUILD_LOOKUP_TIMEOUT, 2000).
-define(GUILD_CALL_TIMEOUT, 3000).

-spec execute_method(binary(), map()) -> term().
execute_method(<<"guild.dispatch">>, #{
    <<"guild_id">> := GuildIdBin, <<"event">> := Event, <<"data">> := Data
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_guild(GuildId, fun(Pid) ->
        EventAtom = constants:dispatch_event_atom(Event),
        IsAlive = erlang:is_process_alive(Pid),
        logger:debug("rpc guild.dispatch: guild_id=~p event=~p pid=~p alive=~p",
            [GuildId, EventAtom, Pid, IsAlive]),
        gen_server:cast(Pid, {dispatch, #{event => EventAtom, data => Data}}),
        true
    end);
execute_method(<<"guild.get_counts">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_guild(GuildId, fun(Pid) ->
        case gen_server:call(Pid, {get_counts}, ?GUILD_CALL_TIMEOUT) of
            #{member_count := MemberCount, presence_count := PresenceCount} ->
                #{<<"member_count">> => MemberCount, <<"presence_count">> => PresenceCount};
            _ ->
                throw({error, <<"guild_counts_error">>})
        end
    end);
execute_method(<<"guild.get_data">>, #{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    {ok, UserId} = validation:validate_optional_snowflake(UserIdBin),
    with_guild(
        GuildId,
        fun(Pid) ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_guild_data, Request}, ?GUILD_CALL_TIMEOUT) of
                #{guild_data := null, error_reason := <<"forbidden">>} ->
                    throw({error, <<"forbidden">>});
                #{guild_data := null} ->
                    throw({error, <<"forbidden">>});
                #{guild_data := GuildData} ->
                    GuildData;
                _ ->
                    throw({error, <<"guild_data_error">>})
            end
        end,
        <<"guild_not_found">>
    );
execute_method(<<"guild.get_member">>, #{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case get_member_cached_or_rpc(GuildId, UserId) of
        {ok, MemberData} when is_map(MemberData) ->
            #{<<"success">> => true, <<"member_data">> => MemberData};
        {ok, undefined} ->
            #{<<"success">> => false};
        error ->
            throw({error, <<"guild_member_error">>})
    end;
execute_method(<<"guild.has_member">>, #{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case get_has_member_cached_or_rpc(GuildId, UserId) of
        {ok, HasMember} ->
            #{<<"has_member">> => HasMember};
        error ->
            throw({error, <<"membership_check_error">>})
    end;
execute_method(<<"guild.list_members">>, #{
    <<"guild_id">> := GuildIdBin, <<"limit">> := Limit, <<"offset">> := Offset
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{limit => Limit, offset => Offset},
        case gen_server:call(Pid, {list_guild_members, Request}, ?GUILD_CALL_TIMEOUT) of
            #{members := Members, total := Total} ->
                #{<<"members">> => Members, <<"total">> => Total};
            _ ->
                throw({error, <<"guild_members_error">>})
        end
    end);
execute_method(<<"guild.list_members_cursor">>, Request) ->
    GuildIdBin = maps:get(<<"guild_id">>, Request, undefined),
    LimitRaw = maps:get(<<"limit">>, Request, 1),
    AfterParam = maps:get(<<"after">>, Request, undefined),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    Limit = clamp_limit(LimitRaw),
    AfterId = parse_optional_snowflake(AfterParam, <<"after">>),
    with_guild(GuildId, fun(Pid) ->
        CursorRequest = #{<<"limit">> => Limit, <<"after">> => AfterId},
        case
            gen_server:call(Pid, {list_guild_members_cursor, CursorRequest}, ?GUILD_CALL_TIMEOUT)
        of
            #{members := Members, total := Total} ->
                #{<<"members">> => Members, <<"total">> => Total};
            _ ->
                throw({error, <<"guild_members_error">>})
        end
    end);
execute_method(<<"guild.start">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case guild_manager:start_or_lookup(GuildId, ?GUILD_LOOKUP_TIMEOUT) of
        {ok, _Pid} -> true;
        {error, Reason} ->
            throw({error, <<"guild_start_error:", (error_term_to_binary(Reason))/binary>>})
    end;
execute_method(<<"guild.stop">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {stop_guild, GuildId}, ?GUILD_CALL_TIMEOUT) of
        ok -> true;
        _ -> throw({error, <<"guild_stop_error">>})
    end;
execute_method(<<"guild.reload">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {reload_guild, GuildId}, ?GUILD_CALL_TIMEOUT) of
        ok ->
            true;
        {error, not_found} ->
            case guild_manager:start_or_lookup(GuildId, 20000) of
                {ok, _Pid} -> true;
                _ -> throw({error, <<"guild_reload_error">>})
            end;
        _ ->
            throw({error, <<"guild_reload_error">>})
    end;
execute_method(<<"guild.reload_all">>, #{<<"guild_ids">> := GuildIdsBin}) ->
    GuildIds = validation:snowflake_list_or_throw(<<"guild_ids">>, GuildIdsBin),
    case gen_server:call(guild_manager, {reload_all_guilds, GuildIds}, 15000) of
        #{count := Count} -> #{<<"count">> => Count};
        _ -> throw({error, <<"guilds_reload_error">>})
    end;
execute_method(<<"guild.shutdown">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {shutdown_guild, GuildId}, ?GUILD_CALL_TIMEOUT) of
        ok ->
            true;
        {error, timeout} ->
            case gen_server:call(guild_manager, {stop_guild, GuildId}, ?GUILD_CALL_TIMEOUT) of
                ok -> true;
                _ -> throw({error, <<"guild_shutdown_error">>})
            end;
        _ ->
            throw({error, <<"guild_shutdown_error">>})
    end;
execute_method(<<"guild.get_user_permissions">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin, <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ChannelId = parse_channel_id(ChannelIdBin),
    case get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) of
        {ok, Permissions} ->
            #{<<"permissions">> => integer_to_binary(Permissions)};
        error ->
            throw({error, <<"permissions_error">>})
    end;
execute_method(<<"guild.get_user_permissions_batch">>, #{
    <<"guild_ids">> := GuildIdsBin, <<"user_id">> := UserIdBin, <<"channel_id">> := ChannelIdBin
}) ->
    GuildIds = validation:snowflake_list_or_throw(<<"guild_ids">>, GuildIdsBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ChannelId = parse_channel_id(ChannelIdBin),
    UniqueGuildIds = lists:usort(GuildIds),
    PermissionsResults = process_batch(
        UniqueGuildIds,
        fun(GuildId) -> fetch_user_permissions_entry(GuildId, UserId, ChannelId) end,
        ?BATCH_TIMEOUT_MS
    ),
    PermissionsList = [Result || Result <- PermissionsResults, is_map(Result)],
    #{<<"permissions">> => PermissionsList};
execute_method(<<"guild.check_permission">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"permission">> := PermissionBin,
    <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    Permission = validation:snowflake_or_throw(<<"permission">>, PermissionBin),
    ChannelId = parse_channel_id(ChannelIdBin),
    case get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) of
        {ok, Permissions} ->
            HasPermission = (Permissions band Permission) =:= Permission,
            #{<<"has_permission">> => HasPermission};
        error ->
            throw({error, <<"permission_check_error">>})
    end;
execute_method(<<"guild.can_manage_roles">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"target_user_id">> := TargetUserIdBin,
    <<"role_id">> := RoleIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    TargetUserId = validation:snowflake_or_throw(<<"target_user_id">>, TargetUserIdBin),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RoleIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{user_id => UserId, target_user_id => TargetUserId, role_id => RoleId},
        case gen_server:call(Pid, {can_manage_roles, Request}, ?GUILD_CALL_TIMEOUT) of
            #{can_manage := CanManage} -> #{<<"can_manage">> => CanManage};
            _ -> throw({error, <<"role_management_check_error">>})
        end
    end);
execute_method(<<"guild.can_manage_role">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin, <<"role_id">> := RoleIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RoleIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{user_id => UserId, role_id => RoleId},
        case gen_server:call(Pid, {can_manage_role, Request}, ?GUILD_CALL_TIMEOUT) of
            #{can_manage := CanManage} -> #{<<"can_manage">> => CanManage};
            _ -> throw({error, <<"role_management_check_error">>})
        end
    end);
execute_method(<<"guild.get_assignable_roles">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{user_id => UserId},
        case gen_server:call(Pid, {get_assignable_roles, Request}, ?GUILD_CALL_TIMEOUT) of
            #{role_ids := RoleIds} ->
                #{<<"role_ids">> => [integer_to_binary(RoleId) || RoleId <- RoleIds]};
            _ ->
                throw({error, <<"assignable_roles_error">>})
        end
    end);
execute_method(<<"guild.get_user_max_role_position">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{user_id => UserId},
        case gen_server:call(Pid, {get_user_max_role_position, Request}, ?GUILD_CALL_TIMEOUT) of
            #{position := Position} -> #{<<"position">> => Position};
            _ -> throw({error, <<"max_role_position_error">>})
        end
    end);
execute_method(<<"guild.get_members_with_role">>, #{
    <<"guild_id">> := GuildIdBin, <<"role_id">> := RoleIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RoleIdBin),
    case get_members_with_role_cached_or_rpc(GuildId, RoleId) of
        {ok, UserIds} ->
            #{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]};
        error ->
            throw({error, <<"members_with_role_error">>})
    end;
execute_method(<<"guild.check_target_member">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"target_user_id">> := TargetUserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    TargetUserId = validation:snowflake_or_throw(<<"target_user_id">>, TargetUserIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{user_id => UserId, target_user_id => TargetUserId},
        case gen_server:call(Pid, {check_target_member, Request}, ?GUILD_CALL_TIMEOUT) of
            #{can_manage := CanManage} -> #{<<"can_manage">> => CanManage};
            _ -> throw({error, <<"target_member_check_error">>})
        end
    end);
execute_method(<<"guild.get_viewable_channels">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case get_viewable_channels_cached_or_rpc(GuildId, UserId) of
        {ok, ChannelIds} ->
            #{<<"channel_ids">> => [integer_to_binary(ChannelId) || ChannelId <- ChannelIds]};
        error ->
            throw({error, <<"viewable_channels_error">>})
    end;
execute_method(<<"guild.get_users_to_mention_by_roles">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"channel_id">> := ChannelIdBin,
    <<"role_ids">> := RoleIds,
    <<"author_id">> := AuthorIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AuthorIdBin),
    RoleIdsList = validation:snowflake_list_or_throw(<<"role_ids">>, RoleIds),
    with_guild(GuildId, fun(Pid) ->
        Request = #{channel_id => ChannelId, role_ids => RoleIdsList, author_id => AuthorId},
        case gen_server:call(Pid, {get_users_to_mention_by_roles, Request}, ?GUILD_CALL_TIMEOUT) of
            #{user_ids := UserIds} ->
                #{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]};
            _ ->
                throw({error, <<"users_error">>})
        end
    end);
execute_method(<<"guild.get_users_to_mention_by_user_ids">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"channel_id">> := ChannelIdBin,
    <<"user_ids">> := UserIds,
    <<"author_id">> := AuthorIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AuthorIdBin),
    UserIdsList = validation:snowflake_list_or_throw(<<"user_ids">>, UserIds),
    with_guild(GuildId, fun(Pid) ->
        Request = #{channel_id => ChannelId, user_ids => UserIdsList, author_id => AuthorId},
        case
            gen_server:call(Pid, {get_users_to_mention_by_user_ids, Request}, ?GUILD_CALL_TIMEOUT)
        of
            #{user_ids := ResultUserIds} ->
                #{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- ResultUserIds]};
            _ ->
                throw({error, <<"users_error">>})
        end
    end);
execute_method(<<"guild.get_all_users_to_mention">>, #{
    <<"guild_id">> := GuildIdBin, <<"channel_id">> := ChannelIdBin, <<"author_id">> := AuthorIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AuthorIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{channel_id => ChannelId, author_id => AuthorId},
        case gen_server:call(Pid, {get_all_users_to_mention, Request}, ?GUILD_CALL_TIMEOUT) of
            #{user_ids := UserIds} ->
                #{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]};
            _ ->
                throw({error, <<"users_error">>})
        end
    end);
execute_method(<<"guild.resolve_all_mentions">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"channel_id">> := ChannelIdBin,
    <<"author_id">> := AuthorIdBin,
    <<"mention_everyone">> := MentionEveryone,
    <<"mention_here">> := MentionHere,
    <<"role_ids">> := RoleIds,
    <<"user_ids">> := UserIds
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AuthorIdBin),
    RoleIdsList = validation:snowflake_list_or_throw(<<"role_ids">>, RoleIds),
    UserIdsList = validation:snowflake_list_or_throw(<<"user_ids">>, UserIds),
    with_guild(GuildId, fun(Pid) ->
        Request = #{
            channel_id => ChannelId,
            author_id => AuthorId,
            mention_everyone => MentionEveryone,
            mention_here => MentionHere,
            role_ids => RoleIdsList,
            user_ids => UserIdsList
        },
        case gen_server:call(Pid, {resolve_all_mentions, Request}, ?GUILD_CALL_TIMEOUT) of
            #{user_ids := ResultUserIds} ->
                #{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- ResultUserIds]};
            _ ->
                throw({error, <<"resolve_mentions_error">>})
        end
    end);
execute_method(<<"guild.get_vanity_url_channel">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_guild(GuildId, fun(Pid) ->
        case gen_server:call(Pid, {get_vanity_url_channel}, ?GUILD_CALL_TIMEOUT) of
            #{channel_id := ChannelId} when ChannelId =/= null ->
                #{<<"channel_id">> => integer_to_binary(ChannelId)};
            #{channel_id := null} ->
                #{<<"channel_id">> => null};
            _ ->
                throw({error, <<"vanity_url_channel_error">>})
        end
    end);
execute_method(<<"guild.get_first_viewable_text_channel">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_guild(GuildId, fun(Pid) ->
        case gen_server:call(Pid, {get_first_viewable_text_channel}, ?GUILD_CALL_TIMEOUT) of
            #{channel_id := ChannelId} when ChannelId =/= null ->
                #{<<"channel_id">> => integer_to_binary(ChannelId)};
            #{channel_id := null} ->
                #{<<"channel_id">> => null};
            _ ->
                throw({error, <<"first_viewable_text_channel_error">>})
        end
    end);
execute_method(<<"guild.update_member_voice">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin, <<"mute">> := Mute, <<"deaf">> := Deaf
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        Request = #{user_id => UserId, mute => Mute, deaf => Deaf},
        case gen_server:call(VoicePid, {update_member_voice, Request}, ?GUILD_CALL_TIMEOUT) of
            #{success := true} -> #{<<"success">> => true};
            #{error := Error} -> throw({error, normalize_voice_rpc_error(Error)})
        end
    end);
execute_method(
    <<"guild.disconnect_voice_user">>,
    #{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin} = Params
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ConnectionId = maps:get(<<"connection_id">>, Params, null),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        Request = #{user_id => UserId, connection_id => ConnectionId},
        case gen_server:call(VoicePid, {disconnect_voice_user, Request}, ?GUILD_CALL_TIMEOUT) of
            #{success := true} -> #{<<"success">> => true};
            #{error := Error} -> throw({error, normalize_voice_rpc_error(Error)})
        end
    end);
execute_method(
    <<"guild.disconnect_voice_user_if_in_channel">>,
    #{
        <<"guild_id">> := GuildIdBin,
        <<"user_id">> := UserIdBin,
        <<"expected_channel_id">> := ExpectedChannelIdBin
    } = Params
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ExpectedChannelId = validation:snowflake_or_throw(
        <<"expected_channel_id">>, ExpectedChannelIdBin
    ),
    ConnectionId = maps:get(<<"connection_id">>, Params, undefined),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        Request = build_disconnect_request(UserId, ExpectedChannelId, ConnectionId),
        case
            gen_server:call(
                VoicePid, {disconnect_voice_user_if_in_channel, Request}, ?GUILD_CALL_TIMEOUT
            )
        of
            #{success := true, ignored := true} -> #{<<"success">> => true, <<"ignored">> => true};
            #{success := true} -> #{<<"success">> => true};
            #{error := Error} -> throw({error, normalize_voice_rpc_error(Error)})
        end
    end);
execute_method(<<"guild.disconnect_all_voice_users_in_channel">>, #{
    <<"guild_id">> := GuildIdBin, <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        Request = #{channel_id => ChannelId},
        case
            gen_server:call(
                VoicePid, {disconnect_all_voice_users_in_channel, Request}, ?GUILD_CALL_TIMEOUT
            )
        of
            #{success := true, disconnected_count := Count} ->
                #{<<"success">> => true, <<"disconnected_count">> => Count};
            #{error := Error} ->
                throw({error, normalize_voice_rpc_error(Error)})
        end
    end);
execute_method(<<"guild.confirm_voice_connection_from_livekit">>, Params) ->
    GuildIdBin = maps:get(<<"guild_id">>, Params),
    ConnectionId = maps:get(<<"connection_id">>, Params),
    TokenNonce = maps:get(<<"token_nonce">>, Params, undefined),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        Request = #{connection_id => ConnectionId, token_nonce => TokenNonce},
        case
            gen_server:call(
                VoicePid, {confirm_voice_connection_from_livekit, Request}, ?GUILD_CALL_TIMEOUT
            )
        of
            #{success := true} -> #{<<"success">> => true};
            #{success := false, error := Error} ->
                #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(Error)};
            {error, _Category, ErrorAtom} ->
                #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(ErrorAtom)};
            #{error := Error} -> throw({error, normalize_voice_rpc_error(Error)})
        end
    end);
execute_method(<<"guild.get_voice_states_for_channel">>, Params) ->
    GuildIdBin = maps:get(<<"guild_id">>, Params),
    ChannelIdBin = maps:get(<<"channel_id">>, Params),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        case gen_server:call(VoicePid, {get_voice_states_for_channel, ChannelIdBin}, 10000) of
            #{voice_states := VoiceStates} ->
                #{<<"voice_states">> => VoiceStates};
            _ ->
                throw({error, <<"voice_states_error">>})
        end
    end);
execute_method(<<"guild.get_pending_joins_for_channel">>, Params) ->
    GuildIdBin = maps:get(<<"guild_id">>, Params),
    ChannelIdBin = maps:get(<<"channel_id">>, Params),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        case gen_server:call(VoicePid, {get_pending_joins_for_channel, ChannelIdBin}, 10000) of
            #{pending_joins := PendingJoins} ->
                #{<<"pending_joins">> => PendingJoins};
            _ ->
                throw({error, <<"pending_joins_error">>})
        end
    end);
execute_method(<<"guild.move_member">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"moderator_id">> := ModeratorIdBin,
    <<"channel_id">> := ChannelIdBin
} = Params) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ModeratorId = validation:snowflake_or_throw(<<"moderator_id">>, ModeratorIdBin),
    {ok, ChannelId} = validation:validate_optional_snowflake(ChannelIdBin),
    ConnectionId = maps:get(<<"connection_id">>, Params, null),
    logger:debug(
        "Processing guild.move_member RPC",
        #{
            guild_id => GuildId,
            user_id => UserId,
            moderator_id => ModeratorId,
            channel_id => ChannelId,
            connection_id => ConnectionId
        }
    ),
    with_voice_server(GuildId, fun(VoicePid, GuildPid) ->
        Request = #{
            user_id => UserId,
            moderator_id => ModeratorId,
            channel_id => ChannelId,
            connection_id => ConnectionId
        },
        handle_move_member_result(
            gen_server:call(VoicePid, {move_member, Request}, ?GUILD_CALL_TIMEOUT),
            GuildId,
            ChannelId,
            GuildPid
        )
    end);
execute_method(<<"guild.get_voice_state">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    with_voice_server(GuildId, fun(VoicePid, _GuildPid) ->
        Request = #{user_id => UserId},
        case gen_server:call(VoicePid, {get_voice_state, Request}, ?GUILD_CALL_TIMEOUT) of
            #{voice_state := null} -> #{<<"voice_state">> => null};
            #{voice_state := VoiceState} -> #{<<"voice_state">> => VoiceState};
            _ -> throw({error, <<"voice_state_error">>})
        end
    end);
execute_method(<<"guild.switch_voice_region">>, #{
    <<"guild_id">> := GuildIdBin, <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    with_voice_server(GuildId, fun(VoicePid, GuildPid) ->
        Request = #{channel_id => ChannelId},
        case gen_server:call(VoicePid, {switch_voice_region, Request}, ?GUILD_CALL_TIMEOUT) of
            #{success := true} ->
                spawn(fun() -> guild_voice:switch_voice_region(GuildId, ChannelId, GuildPid) end),
                #{<<"success">> => true};
            #{error := Error} ->
                throw({error, normalize_voice_rpc_error(Error)})
        end
    end);
execute_method(<<"guild.get_category_channel_count">>, #{
    <<"guild_id">> := GuildIdBin, <<"category_id">> := CategoryIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    CategoryId = validation:snowflake_or_throw(<<"category_id">>, CategoryIdBin),
    with_guild(GuildId, fun(Pid) ->
        Request = #{category_id => CategoryId},
        case gen_server:call(Pid, {get_category_channel_count, Request}, ?GUILD_CALL_TIMEOUT) of
            #{count := Count} -> #{<<"count">> => Count};
            _ -> throw({error, <<"category_channel_count_error">>})
        end
    end);
execute_method(<<"guild.get_channel_count">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    with_guild(GuildId, fun(Pid) ->
        case gen_server:call(Pid, {get_channel_count}, ?GUILD_CALL_TIMEOUT) of
            #{count := Count} -> #{<<"count">> => Count};
            _ -> throw({error, <<"channel_count_error">>})
        end
    end);
execute_method(<<"guild.get_online_counts_batch">>, #{<<"guild_ids">> := GuildIdsBin}) ->
    GuildIds = validation:snowflake_list_or_throw(<<"guild_ids">>, GuildIdsBin),
    UniqueGuildIds = lists:usort(GuildIds),
    validate_batch_size(length(UniqueGuildIds)),
    Results = process_batch(
        UniqueGuildIds,
        fun(GuildId) -> fetch_online_count_entry(GuildId) end,
        ?BATCH_TIMEOUT_MS
    ),
    OnlineCounts = [Result || Result <- Results, is_map(Result)],
    #{<<"online_counts">> => OnlineCounts};
execute_method(<<"guild.batch_voice_state_update">>, #{<<"updates">> := UpdatesBin}) ->
    BatchSize = length(UpdatesBin),
    StartTime = erlang:monotonic_time(millisecond),
    validate_batch_size(BatchSize),
    Updates = lists:map(fun parse_voice_update/1, UpdatesBin),
    Results = process_batch(Updates, fun process_voice_update/1, ?BATCH_TIMEOUT_MS),
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    gateway_metrics_collector:record_rpc_latency(Duration),
    #{<<"results">> => Results}.

-spec fetch_online_count_entry(integer()) -> map() | undefined.
fetch_online_count_entry(GuildId) ->
    case guild_counts_cache:get(GuildId) of
        {ok, MemberCount, OnlineCount} ->
            #{
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"member_count">> => MemberCount,
                <<"online_count">> => OnlineCount
            };
        miss ->
            fetch_online_count_entry_from_process(GuildId)
    end.

-spec fetch_online_count_entry_from_process(integer()) -> map() | undefined.
fetch_online_count_entry_from_process(GuildId) ->
    case get_guild_pid(GuildId) of
        {ok, Pid} ->
            case gen_server:call(Pid, {get_counts}, ?GUILD_CALL_TIMEOUT) of
                #{member_count := MemberCount, presence_count := PresenceCount} ->
                    #{
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"member_count">> => MemberCount,
                        <<"online_count">> => PresenceCount
                    };
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec with_guild(integer(), fun((pid()) -> T)) -> T when T :: term().
with_guild(GuildId, Fun) ->
    with_guild(GuildId, Fun, <<"guild_not_found">>).

-spec with_guild(integer(), fun((pid()) -> T), binary()) -> T when T :: term().
with_guild(GuildId, Fun, NotFoundError) ->
    case get_guild_pid(GuildId) of
        {ok, Pid} -> Fun(Pid);
        _ -> throw({error, NotFoundError})
    end.

-spec with_voice_server(integer(), fun((pid(), pid()) -> T)) -> T when T :: term().
with_voice_server(GuildId, Fun) ->
    case get_guild_pid(GuildId) of
        {ok, GuildPid} ->
            VoicePid = resolve_voice_pid(GuildId, GuildPid),
            Fun(VoicePid, GuildPid);
        _ ->
            throw({error, <<"guild_not_found">>})
    end.

-spec resolve_voice_pid(integer(), pid()) -> pid().
resolve_voice_pid(GuildId, FallbackGuildPid) ->
    case guild_voice_server:lookup(GuildId) of
        {ok, VoicePid} -> VoicePid;
        {error, not_found} -> FallbackGuildPid
    end.

-spec get_guild_pid(integer()) -> {ok, pid()} | error.
get_guild_pid(GuildId) ->
    case lookup_guild_pid_from_cache(GuildId) of
        {ok, Pid} ->
            {ok, Pid};
        not_found ->
            lookup_guild_pid_from_manager(GuildId)
    end.

-spec lookup_guild_pid_from_cache(integer()) -> {ok, pid()} | not_found.
lookup_guild_pid_from_cache(GuildId) ->
    case catch ets:lookup(guild_pid_cache, GuildId) of
        [{GuildId, Pid}] when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    _ = ets:delete(guild_pid_cache, GuildId),
                    not_found
            end;
        _ ->
            not_found
    end.

-spec lookup_guild_pid_from_manager(integer()) -> {ok, pid()} | error.
lookup_guild_pid_from_manager(GuildId) ->
    case guild_manager:start_or_lookup(GuildId, ?GUILD_LOOKUP_TIMEOUT) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            error
    end.

-spec fetch_user_permissions_entry(integer(), integer(), integer() | undefined) -> map() | undefined.
fetch_user_permissions_entry(GuildId, UserId, ChannelId) ->
    case get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) of
        {ok, Permissions} ->
            #{
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"permissions">> => integer_to_binary(Permissions)
            };
        error ->
            undefined
    end.

-spec get_permissions_cached_or_rpc(integer(), integer(), integer() | undefined) ->
    {ok, integer()} | error.
get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) ->
    case guild_permission_cache:get_permissions(GuildId, UserId, ChannelId) of
        {ok, Permissions} ->
            {ok, Permissions};
        {error, not_found} ->
            get_permissions_via_rpc(GuildId, UserId, ChannelId)
    end.

-spec get_permissions_via_rpc(integer(), integer(), integer() | undefined) -> {ok, integer()} | error.
get_permissions_via_rpc(GuildId, UserId, ChannelId) ->
    case get_guild_pid(GuildId) of
        {ok, Pid} ->
            Request = #{user_id => UserId, channel_id => ChannelId},
            case gen_server:call(Pid, {get_user_permissions, Request}, ?GUILD_CALL_TIMEOUT) of
                #{permissions := Permissions} -> {ok, Permissions};
                _ -> error
            end;
        error ->
            error
    end.

-spec get_members_with_role_cached_or_rpc(integer(), integer()) -> {ok, [integer()]} | error.
get_members_with_role_cached_or_rpc(GuildId, RoleId) ->
    case guild_permission_cache:get_snapshot(GuildId) of
        {ok, Snapshot} ->
            {ok, get_members_with_role_from_snapshot(RoleId, Snapshot)};
        {error, not_found} ->
            get_members_with_role_via_rpc(GuildId, RoleId)
    end.

-spec get_members_with_role_from_snapshot(integer(), map()) -> [integer()].
get_members_with_role_from_snapshot(RoleId, Snapshot) ->
    Data = maps:get(data, Snapshot, #{}),
    MemberRoleIndex = guild_data_index:member_role_index(Data),
    lists:sort(maps:keys(maps:get(RoleId, MemberRoleIndex, #{}))).

-spec get_members_with_role_via_rpc(integer(), integer()) -> {ok, [integer()]} | error.
get_members_with_role_via_rpc(GuildId, RoleId) ->
    case get_guild_pid(GuildId) of
        {ok, Pid} ->
            Request = #{role_id => RoleId},
            case gen_server:call(Pid, {get_members_with_role, Request}, ?GUILD_CALL_TIMEOUT) of
                #{user_ids := UserIds} ->
                    {ok, UserIds};
                _ ->
                    error
            end;
        error ->
            error
    end.

-spec get_viewable_channels_cached_or_rpc(integer(), integer()) -> {ok, [integer()]} | error.
get_viewable_channels_cached_or_rpc(GuildId, UserId) ->
    case guild_permission_cache:get_snapshot(GuildId) of
        {ok, Snapshot} ->
            {ok, guild_visibility:get_user_viewable_channels(UserId, Snapshot)};
        {error, not_found} ->
            get_viewable_channels_via_rpc(GuildId, UserId)
    end.

-spec get_viewable_channels_via_rpc(integer(), integer()) -> {ok, [integer()]} | error.
get_viewable_channels_via_rpc(GuildId, UserId) ->
    case get_guild_pid(GuildId) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_viewable_channels, Request}, ?GUILD_CALL_TIMEOUT) of
                #{channel_ids := ChannelIds} ->
                    {ok, ChannelIds};
                _ ->
                    error
            end;
        error ->
            error
    end.

-spec get_has_member_cached_or_rpc(integer(), integer()) -> {ok, boolean()} | error.
get_has_member_cached_or_rpc(GuildId, UserId) ->
    case guild_permission_cache:has_member(GuildId, UserId) of
        {ok, HasMember} ->
            {ok, HasMember};
        {error, not_found} ->
            get_has_member_via_rpc(GuildId, UserId)
    end.

-spec get_has_member_via_rpc(integer(), integer()) -> {ok, boolean()} | error.
get_has_member_via_rpc(GuildId, UserId) ->
    case get_guild_pid(GuildId) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {has_member, Request}, ?GUILD_CALL_TIMEOUT) of
                #{has_member := HasMember} when is_boolean(HasMember) ->
                    {ok, HasMember};
                _ ->
                    error
            end;
        error ->
            error
    end.

-spec get_member_cached_or_rpc(integer(), integer()) -> {ok, map() | undefined} | error.
get_member_cached_or_rpc(GuildId, UserId) ->
    case guild_permission_cache:get_member(GuildId, UserId) of
        {ok, MemberOrUndefined} ->
            {ok, MemberOrUndefined};
        {error, not_found} ->
            get_member_via_rpc(GuildId, UserId)
    end.

-spec get_member_via_rpc(integer(), integer()) -> {ok, map() | undefined} | error.
get_member_via_rpc(GuildId, UserId) ->
    case get_guild_pid(GuildId) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_guild_member, Request}, ?GUILD_CALL_TIMEOUT) of
                #{success := true, member_data := MemberData} ->
                    {ok, MemberData};
                #{success := false} ->
                    {ok, undefined};
                _ ->
                    error
            end;
        error ->
            error
    end.

-spec parse_channel_id(binary()) -> integer() | undefined.
parse_channel_id(<<"0">>) -> undefined;
parse_channel_id(ChannelIdBin) -> validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin).

-spec parse_optional_snowflake(term(), binary()) -> integer() | undefined.
parse_optional_snowflake(undefined, _) -> undefined;
parse_optional_snowflake(Value, Field) -> validation:snowflake_or_throw(Field, Value).

-spec build_disconnect_request(integer(), integer(), term()) -> map().
build_disconnect_request(UserId, ExpectedChannelId, undefined) ->
    #{user_id => UserId, expected_channel_id => ExpectedChannelId};
build_disconnect_request(UserId, ExpectedChannelId, ConnId) ->
    #{user_id => UserId, expected_channel_id => ExpectedChannelId, connection_id => ConnId}.

-spec handle_move_member_result(map(), integer(), integer() | null, pid()) -> map().
handle_move_member_result(
    #{success := true, needs_token := true, session_data := SessionData, connections_to_move := _},
    GuildId,
    ChannelId,
    Pid
) when ChannelId =/= null ->
    spawn(fun() ->
        guild_voice:send_voice_server_updates_for_move(GuildId, ChannelId, SessionData, Pid)
    end),
    #{<<"success">> => true};
handle_move_member_result(#{success := true, user_id := DisconnectedUserId}, _, null, Pid) ->
    spawn(fun() -> guild_voice:cleanup_virtual_access_on_disconnect(DisconnectedUserId, Pid) end),
    #{<<"success">> => true};
handle_move_member_result(#{success := true}, _, _, _) ->
    #{<<"success">> => true};
handle_move_member_result({error, _Category, ErrorAtom}, _, _, _) ->
    #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(ErrorAtom)};
handle_move_member_result(#{error := Error}, _, _, _) ->
    #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(Error)};
handle_move_member_result(_, _, _, _) ->
    #{<<"success">> => false, <<"error">> => <<"move_member_error">>}.

-spec normalize_voice_rpc_error(term()) -> binary().
normalize_voice_rpc_error(voice_user_not_in_voice) -> <<"user_not_in_voice">>;
normalize_voice_rpc_error(voice_channel_not_found) -> <<"channel_not_found">>;
normalize_voice_rpc_error(voice_channel_not_voice) -> <<"channel_not_voice">>;
normalize_voice_rpc_error(voice_moderator_missing_connect) -> <<"moderator_missing_connect">>;
normalize_voice_rpc_error(voice_permission_denied) -> <<"target_missing_connect">>;
normalize_voice_rpc_error(voice_connection_not_found) -> <<"connection_not_found">>;
normalize_voice_rpc_error(voice_missing_connection_id) -> <<"connection_not_found">>;
normalize_voice_rpc_error(Error) when is_binary(Error) ->
    normalize_voice_rpc_error_binary(Error);
normalize_voice_rpc_error(Error) when is_atom(Error) ->
    error_term_to_binary(Error);
normalize_voice_rpc_error(_Error) ->
    <<"move_member_error">>.

-spec normalize_voice_rpc_error_binary(binary()) -> binary().
normalize_voice_rpc_error_binary(<<"voice_user_not_in_voice">>) -> <<"user_not_in_voice">>;
normalize_voice_rpc_error_binary(<<"voice_channel_not_found">>) -> <<"channel_not_found">>;
normalize_voice_rpc_error_binary(<<"voice_channel_not_voice">>) -> <<"channel_not_voice">>;
normalize_voice_rpc_error_binary(<<"voice_moderator_missing_connect">>) ->
    <<"moderator_missing_connect">>;
normalize_voice_rpc_error_binary(<<"voice_permission_denied">>) -> <<"target_missing_connect">>;
normalize_voice_rpc_error_binary(<<"voice_connection_not_found">>) -> <<"connection_not_found">>;
normalize_voice_rpc_error_binary(<<"voice_missing_connection_id">>) -> <<"connection_not_found">>;
normalize_voice_rpc_error_binary(Error) -> Error.

-spec error_term_to_binary(term()) -> binary().
error_term_to_binary(Value) when is_binary(Value) ->
    Value;
error_term_to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
error_term_to_binary(Value) when is_list(Value) ->
    try
        unicode:characters_to_binary(Value)
    catch
        _:_ ->
            <<"invalid_error">>
    end;
error_term_to_binary(Value) ->
    try
        unicode:characters_to_binary(io_lib:format("~p", [Value]))
    catch
        _:_ ->
            <<"invalid_error">>
    end.

-spec parse_voice_update(map()) -> {integer(), integer(), boolean(), boolean(), term()}.
parse_voice_update(
    #{
        <<"guild_id">> := GuildIdBin,
        <<"user_id">> := UserIdBin,
        <<"mute">> := Mute,
        <<"deaf">> := Deaf
    } = Update
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ConnectionId = maps:get(<<"connection_id">>, Update, null),
    {GuildId, UserId, Mute, Deaf, ConnectionId}.

-spec process_voice_update({integer(), integer(), boolean(), boolean(), term()}) -> map().
process_voice_update({GuildId, UserId, Mute, Deaf, ConnectionId}) ->
    case guild_manager:start_or_lookup(GuildId, ?GUILD_LOOKUP_TIMEOUT) of
        {ok, GuildPid} ->
            VoicePid = resolve_voice_pid(GuildId, GuildPid),
            Request = #{
                user_id => UserId, mute => Mute, deaf => Deaf, connection_id => ConnectionId
            },
            case gen_server:call(VoicePid, {update_member_voice, Request}, ?GUILD_CALL_TIMEOUT) of
                #{success := true} ->
                    #{
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"user_id">> => integer_to_binary(UserId),
                        <<"success">> => true
                    };
                #{error := Error} ->
                    #{
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"user_id">> => integer_to_binary(UserId),
                        <<"success">> => false,
                        <<"error">> => Error
                    }
            end;
        _ ->
            #{
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"user_id">> => integer_to_binary(UserId),
                <<"success">> => false,
                <<"error">> => <<"guild_not_found">>
            }
    end.

-spec validate_batch_size(non_neg_integer()) -> ok.
validate_batch_size(Size) when Size > ?MAX_BATCH_SIZE ->
    throw(
        {error, <<"Batch size exceeds maximum of ", (integer_to_binary(?MAX_BATCH_SIZE))/binary>>}
    );
validate_batch_size(_) ->
    ok.

-spec process_batch([T], fun((T) -> R), pos_integer()) -> [R] when T :: term(), R :: term().
process_batch(Items, HandlerFun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Workers = [
        spawn_monitor(fun() ->
            try
                Parent ! {Ref, {ok, HandlerFun(Item)}}
            catch
                _:_ -> Parent ! {Ref, error}
            end
        end)
     || Item <- Items
    ],
    collect_results(Ref, length(Workers), Timeout, []).

-spec collect_results(reference(), non_neg_integer(), non_neg_integer(), [T]) -> [T] when
    T :: term().
collect_results(_, 0, _, Acc) ->
    lists:reverse(Acc);
collect_results(Ref, Remaining, Timeout, Acc) when Timeout > 0 ->
    receive
        {Ref, {ok, Result}} ->
            collect_results(Ref, Remaining - 1, Timeout, [Result | Acc]);
        {Ref, error} ->
            collect_results(Ref, Remaining - 1, Timeout, Acc);
        {'DOWN', _, process, _, _} ->
            collect_results(Ref, Remaining, Timeout, Acc)
    after Timeout ->
        lists:reverse(Acc)
    end;
collect_results(_, _, _, Acc) ->
    lists:reverse(Acc).

-spec clamp_limit(term()) -> pos_integer().
clamp_limit(Value) ->
    case type_conv:to_integer(Value) of
        undefined -> 1;
        N when N < 1 -> 1;
        N when N > 1000 -> 1000;
        N -> N
    end.

-ifdef(TEST).

clamp_limit_test() ->
    ?assertEqual(1, clamp_limit(undefined)),
    ?assertEqual(1, clamp_limit(0)),
    ?assertEqual(1, clamp_limit(-5)),
    ?assertEqual(50, clamp_limit(50)),
    ?assertEqual(1000, clamp_limit(2000)).

parse_channel_id_test() ->
    ?assertEqual(undefined, parse_channel_id(<<"0">>)).

validate_batch_size_test() ->
    ?assertEqual(ok, validate_batch_size(50)),
    ?assertEqual(ok, validate_batch_size(100)),
    ?assertThrow({error, _}, validate_batch_size(101)).

get_permissions_cached_or_rpc_prefers_cache_test() ->
    GuildId = 12345,
    UserId = 500,
    ViewPermission = constants:view_channel_permission(),
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [
            #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPermission)}
        ],
        <<"members">> => #{
            UserId => #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
        },
        <<"channels">> => [#{<<"id">> => <<"700">>, <<"permission_overwrites">> => []}]
    },
    ok = guild_permission_cache:put_data(GuildId, Data),
    try
        ?assertMatch({ok, _}, get_permissions_cached_or_rpc(GuildId, UserId, 700))
    after
        ok = guild_permission_cache:delete(GuildId)
    end.

get_members_with_role_cached_or_rpc_prefers_cache_test() ->
    GuildId = 12346,
    UserId = 501,
    RoleId = 9999,
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [
            #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
            #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}
        ],
        <<"members">> => #{
            UserId => #{
                <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                <<"roles">> => [integer_to_binary(RoleId)]
            }
        },
        <<"channels">> => []
    },
    ok = guild_permission_cache:put_data(GuildId, Data),
    try
        ?assertEqual({ok, [UserId]}, get_members_with_role_cached_or_rpc(GuildId, RoleId))
    after
        ok = guild_permission_cache:delete(GuildId)
    end.

get_viewable_channels_cached_or_rpc_prefers_cache_test() ->
    GuildId = 12347,
    UserId = 7001,
    ChannelId = 321,
    ViewPerm = constants:view_channel_permission(),
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [
            #{
                <<"id">> => integer_to_binary(GuildId),
                <<"permissions">> => integer_to_binary(ViewPerm)
            }
        ],
        <<"members">> => #{
            UserId => #{
                <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                <<"roles">> => []
            }
        },
        <<"channels">> => [#{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}]
    },
    ok = guild_permission_cache:put_data(GuildId, Data),
    try
        ?assertEqual({ok, [ChannelId]}, get_viewable_channels_cached_or_rpc(GuildId, UserId))
    after
        ok = guild_permission_cache:delete(GuildId)
    end.

get_has_member_cached_or_rpc_prefers_cache_test() ->
    GuildId = 12348,
    UserId = 502,
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [],
        <<"members">> => #{
            UserId => #{
                <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                <<"roles">> => []
            }
        },
        <<"channels">> => []
    },
    ok = guild_permission_cache:put_data(GuildId, Data),
    try
        ?assertEqual({ok, true}, get_has_member_cached_or_rpc(GuildId, UserId)),
        ?assertEqual({ok, false}, get_has_member_cached_or_rpc(GuildId, 99999))
    after
        ok = guild_permission_cache:delete(GuildId)
    end.

get_member_cached_or_rpc_prefers_cache_test() ->
    GuildId = 12349,
    UserId = 503,
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [],
        <<"members">> => #{
            UserId => #{
                <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                <<"roles">> => [],
                <<"nick">> => <<"CacheNick">>
            }
        },
        <<"channels">> => []
    },
    ok = guild_permission_cache:put_data(GuildId, Data),
    try
        {ok, MemberData} = get_member_cached_or_rpc(GuildId, UserId),
        ?assertEqual(<<"CacheNick">>, maps:get(<<"nick">>, MemberData)),
        ?assertEqual({ok, undefined}, get_member_cached_or_rpc(GuildId, 99999))
    after
        ok = guild_permission_cache:delete(GuildId)
    end.

-endif.
