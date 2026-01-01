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

execute_method(<<"guild.dispatch">>, #{
    <<"guild_id">> := GuildIdBin, <<"event">> := Event, <<"data">> := Data
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            EventAtom = constants:dispatch_event_atom(Event),
            case gen_server:call(Pid, {dispatch, #{event => EventAtom, data => Data}}, 10000) of
                ok -> true;
                _ -> throw({error, <<"Dispatch failed">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_counts">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {get_counts}, 10000) of
                #{member_count := MemberCount, presence_count := PresenceCount} ->
                    #{
                        <<"member_count">> => MemberCount,
                        <<"presence_count">> => PresenceCount
                    };
                _ ->
                    throw({error, <<"Failed to get counts">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_data">>, #{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case validation:validate_optional_snowflake(UserIdBin) of
        {ok, UserId} ->
            case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
                {ok, Pid} ->
                    Request = #{user_id => UserId},
                    case gen_server:call(Pid, {get_guild_data, Request}, 10000) of
                        #{guild_data := null, error_reason := <<"forbidden">>} ->
                            throw({error, <<"forbidden">>});
                        #{guild_data := null} ->
                            throw({error, <<"Guild data not available for user">>});
                        #{guild_data := GuildData} ->
                            GuildData;
                        _ ->
                            throw({error, <<"Failed to get guild data">>})
                    end;
                _ ->
                    throw({error, <<"guild_not_found">>})
            end
    end;
execute_method(<<"guild.get_member">>, #{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_guild_member, Request}, 10000) of
                #{success := true, member_data := MemberData} ->
                    #{
                        <<"success">> => true,
                        <<"member_data">> => MemberData
                    };
                #{success := false} ->
                    #{<<"success">> => false};
                _ ->
                    throw({error, <<"Failed to get guild member">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.has_member">>, #{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {has_member, Request}, 10000) of
                #{has_member := HasMember} when is_boolean(HasMember) ->
                    #{<<"has_member">> => HasMember};
                _ ->
                    throw({error, <<"Failed to determine membership">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.list_members">>, #{
    <<"guild_id">> := GuildIdBin, <<"limit">> := Limit, <<"offset">> := Offset
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{limit => Limit, offset => Offset},
            case gen_server:call(Pid, {list_guild_members, Request}, 10000) of
                #{members := Members, total := Total} ->
                    #{
                        <<"members">> => Members,
                        <<"total">> => Total
                    };
                _ ->
                    throw({error, <<"Failed to list guild members">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.start">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, _Pid} ->
            true;
        _ ->
            throw({error, <<"Failed to start guild">>})
    end;
execute_method(<<"guild.stop">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {stop_guild, GuildId}, 10000) of
        ok ->
            true;
        _ ->
            throw({error, <<"Failed to stop guild">>})
    end;
execute_method(<<"guild.reload">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {reload_guild, GuildId}, 10000) of
        ok ->
            true;
        {error, not_found} ->
            case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 20000) of
                {ok, _Pid} -> true;
                _ -> throw({error, <<"Failed to reload guild">>})
            end;
        _ ->
            throw({error, <<"Failed to reload guild">>})
    end;
execute_method(<<"guild.reload_all">>, #{<<"guild_ids">> := GuildIdsBin}) ->
    GuildIds = validation:snowflake_list_or_throw(<<"guild_ids">>, GuildIdsBin),
    case gen_server:call(guild_manager, {reload_all_guilds, GuildIds}, 60000) of
        #{count := Count} ->
            #{<<"count">> => Count};
        _ ->
            throw({error, <<"Failed to reload guilds">>})
    end;
execute_method(<<"guild.shutdown">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {shutdown_guild, GuildId}, 10000) of
        ok ->
            true;
        {error, timeout} ->
            case gen_server:call(guild_manager, {stop_guild, GuildId}, 10000) of
                ok -> true;
                _ -> throw({error, <<"Failed to shutdown guild">>})
            end;
        _ ->
            throw({error, <<"Failed to shutdown guild">>})
    end;
execute_method(<<"guild.get_user_permissions">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin, <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ChannelId =
        case ChannelIdBin of
            <<"0">> ->
                undefined;
            _ ->
                validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin)
        end,
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId, channel_id => ChannelId},
            case gen_server:call(Pid, {get_user_permissions, Request}, 10000) of
                #{permissions := Permissions} ->
                    #{<<"permissions">> => integer_to_binary(Permissions)};
                _ ->
                    throw({error, <<"Failed to get permissions">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.check_permission">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"permission">> := PermissionBin,
    <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    Permission = validation:snowflake_or_throw(<<"permission">>, PermissionBin),
    ChannelId =
        case ChannelIdBin of
            <<"0">> ->
                undefined;
            _ ->
                validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin)
        end,
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{
                user_id => UserId,
                permission => Permission,
                channel_id => ChannelId
            },
            case gen_server:call(Pid, {check_permission, Request}, 10000) of
                #{has_permission := HasPermission} ->
                    #{<<"has_permission">> => HasPermission};
                _ ->
                    throw({error, <<"Failed to check permission">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
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
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{
                user_id => UserId,
                target_user_id => TargetUserId,
                role_id => RoleId
            },
            case gen_server:call(Pid, {can_manage_roles, Request}, 10000) of
                #{can_manage := CanManage} ->
                    #{<<"can_manage">> => CanManage};
                _ ->
                    throw({error, <<"Failed to check role management">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.can_manage_role">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"role_id">> := RoleIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RoleIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId, role_id => RoleId},
            case gen_server:call(Pid, {can_manage_role, Request}, 10000) of
                #{can_manage := CanManage} ->
                    #{<<"can_manage">> => CanManage};
                _ ->
                    throw({error, <<"Failed to check role management">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_assignable_roles">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_assignable_roles, Request}, 10000) of
                #{role_ids := RoleIds} ->
                    #{
                        <<"role_ids">> => [
                            integer_to_binary(RoleId)
                         || RoleId <- RoleIds
                        ]
                    };
                _ ->
                    throw({error, <<"Failed to get assignable roles">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_user_max_role_position">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_user_max_role_position, Request}, 10000) of
                #{position := Position} ->
                    #{<<"position">> => Position};
                _ ->
                    throw({error, <<"Failed to get max role position">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_members_with_role">>, #{
    <<"guild_id">> := GuildIdBin, <<"role_id">> := RoleIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RoleIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{role_id => RoleId},
            case gen_server:call(Pid, {get_members_with_role, Request}, 10000) of
                #{user_ids := UserIds} ->
                    #{
                        <<"user_ids">> => [
                            integer_to_binary(UserId)
                         || UserId <- UserIds
                        ]
                    };
                _ ->
                    throw({error, <<"Failed to get members with role">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.check_target_member">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"target_user_id">> := TargetUserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    TargetUserId = validation:snowflake_or_throw(<<"target_user_id">>, TargetUserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId, target_user_id => TargetUserId},
            case gen_server:call(Pid, {check_target_member, Request}, 10000) of
                #{can_manage := CanManage} ->
                    #{<<"can_manage">> => CanManage};
                _ ->
                    throw({error, <<"Failed to check target member">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_viewable_channels">>, #{
    <<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_viewable_channels, Request}, 10000) of
                #{channel_ids := ChannelIds} ->
                    #{
                        <<"channel_ids">> => [
                            integer_to_binary(ChannelId)
                         || ChannelId <- ChannelIds
                        ]
                    };
                _ ->
                    throw({error, <<"Failed to get viewable channels">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
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
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{
                channel_id => ChannelId,
                role_ids => RoleIdsList,
                author_id => AuthorId
            },
            case gen_server:call(Pid, {get_users_to_mention_by_roles, Request}, 10000) of
                #{user_ids := UserIds} ->
                    #{
                        <<"user_ids">> => [
                            integer_to_binary(UserId)
                         || UserId <- UserIds
                        ]
                    };
                _ ->
                    throw({error, <<"Failed to get users">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
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
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{
                channel_id => ChannelId,
                user_ids => UserIdsList,
                author_id => AuthorId
            },
            case gen_server:call(Pid, {get_users_to_mention_by_user_ids, Request}, 10000) of
                #{user_ids := ResultUserIds} ->
                    #{
                        <<"user_ids">> => [
                            integer_to_binary(UserId)
                         || UserId <- ResultUserIds
                        ]
                    };
                _ ->
                    throw({error, <<"Failed to get users">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_all_users_to_mention">>, #{
    <<"guild_id">> := GuildIdBin, <<"channel_id">> := ChannelIdBin, <<"author_id">> := AuthorIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AuthorIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{
                channel_id => ChannelId,
                author_id => AuthorId
            },
            case gen_server:call(Pid, {get_all_users_to_mention, Request}, 10000) of
                #{user_ids := UserIds} ->
                    #{
                        <<"user_ids">> => [
                            integer_to_binary(UserId)
                         || UserId <- UserIds
                        ]
                    };
                _ ->
                    throw({error, <<"Failed to get users">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
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
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{
                channel_id => ChannelId,
                author_id => AuthorId,
                mention_everyone => MentionEveryone,
                mention_here => MentionHere,
                role_ids => RoleIdsList,
                user_ids => UserIdsList
            },
            case gen_server:call(Pid, {resolve_all_mentions, Request}, 10000) of
                #{user_ids := ResultUserIds} ->
                    #{
                        <<"user_ids">> => [
                            integer_to_binary(UserId)
                         || UserId <- ResultUserIds
                        ]
                    };
                _ ->
                    throw({error, <<"Failed to resolve mentions">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_vanity_url_channel">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {get_vanity_url_channel}, 10000) of
                #{channel_id := ChannelId} when ChannelId =/= null ->
                    #{<<"channel_id">> => integer_to_binary(ChannelId)};
                #{channel_id := null} ->
                    #{<<"channel_id">> => null};
                _ ->
                    throw({error, <<"Failed to get vanity URL channel">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_first_viewable_text_channel">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {get_first_viewable_text_channel}, 10000) of
                #{channel_id := ChannelId} when ChannelId =/= null ->
                    #{<<"channel_id">> => integer_to_binary(ChannelId)};
                #{channel_id := null} ->
                    #{<<"channel_id">> => null};
                _ ->
                    throw({error, <<"Failed to get first viewable text channel">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(
    <<"guild.update_member_voice">>,
    #{
        <<"guild_id">> := GuildIdBin,
        <<"user_id">> := UserIdBin,
        <<"mute">> := Mute,
        <<"deaf">> := Deaf
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId, mute => Mute, deaf => Deaf},
            case gen_server:call(Pid, {update_member_voice, Request}, 10000) of
                #{success := true} ->
                    #{<<"success">> => true};
                #{error := Error} ->
                    throw({error, Error})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(
    <<"guild.disconnect_voice_user">>,
    #{
        <<"guild_id">> := GuildIdBin,
        <<"user_id">> := UserIdBin
    } = Params
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ConnectionId = maps:get(<<"connection_id">>, Params, null),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId, connection_id => ConnectionId},
            case gen_server:call(Pid, {disconnect_voice_user, Request}, 10000) of
                #{success := true} ->
                    #{<<"success">> => true};
                #{error := Error} ->
                    throw({error, Error})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
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
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request =
                case ConnectionId of
                    undefined ->
                        #{
                            user_id => UserId,
                            expected_channel_id => ExpectedChannelId
                        };
                    ConnId ->
                        #{
                            user_id => UserId,
                            expected_channel_id => ExpectedChannelId,
                            connection_id => ConnId
                        }
                end,
            case gen_server:call(Pid, {disconnect_voice_user_if_in_channel, Request}, 10000) of
                #{success := true, ignored := true} ->
                    #{<<"success">> => true, <<"ignored">> => true};
                #{success := true} ->
                    #{<<"success">> => true};
                #{error := Error} ->
                    throw({error, Error})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.disconnect_all_voice_users_in_channel">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{channel_id => ChannelId},
            case gen_server:call(Pid, {disconnect_all_voice_users_in_channel, Request}, 10000) of
                #{success := true, disconnected_count := Count} ->
                    #{<<"success">> => true, <<"disconnected_count">> => Count};
                #{error := Error} ->
                    throw({error, Error})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.confirm_voice_connection_from_livekit">>, Params) ->
    GuildIdBin = maps:get(<<"guild_id">>, Params),
    ConnectionId = maps:get(<<"connection_id">>, Params),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{connection_id => ConnectionId},
            case gen_server:call(Pid, {confirm_voice_connection_from_livekit, Request}, 10000) of
                #{success := true} ->
                    #{<<"success">> => true};
                #{success := false, error := Error} ->
                    #{<<"success">> => false, <<"error">> => Error};
                #{error := Error} ->
                    throw({error, Error})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.move_member">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin,
    <<"moderator_id">> := ModeratorIdBin,
    <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ModeratorId = validation:snowflake_or_throw(<<"moderator_id">>, ModeratorIdBin),
    case validation:validate_optional_snowflake(ChannelIdBin) of
        {ok, ChannelId} ->
            case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
                {ok, Pid} ->
                    Request = #{
                        user_id => UserId,
                        moderator_id => ModeratorId,
                        channel_id => ChannelId
                    },
                    case gen_server:call(Pid, {move_member, Request}, 10000) of
                        #{
                            success := true,
                            needs_token := true,
                            session_data := SessionData,
                            connections_to_move := ConnectionsToMove
                        } when
                            ChannelId =/= null
                        ->
                            spawn(fun() ->
                                guild_voice:handle_virtual_channel_access_for_move(
                                    UserId, ChannelId, ConnectionsToMove, Pid
                                ),
                                guild_voice:send_voice_server_updates_for_move(
                                    GuildId, ChannelId, SessionData, Pid
                                )
                            end),
                            #{<<"success">> => true};
                        #{success := true, user_id := DisconnectedUserId} when
                            ChannelId =:= null
                        ->
                            spawn(fun() ->
                                guild_voice:cleanup_virtual_access_on_disconnect(
                                    DisconnectedUserId, Pid
                                )
                            end),
                            #{<<"success">> => true};
                        #{success := true} ->
                            #{<<"success">> => true};
                        #{error := Error} ->
                            throw({error, Error})
                    end;
                _ ->
                    throw({error, <<"Guild not found">>})
            end
    end;
execute_method(<<"guild.get_voice_state">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"user_id">> := UserIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{user_id => UserId},
            case gen_server:call(Pid, {get_voice_state, Request}, 10000) of
                #{voice_state := null} ->
                    #{<<"voice_state">> => null};
                #{voice_state := VoiceState} ->
                    #{<<"voice_state">> => VoiceState};
                _ ->
                    throw({error, <<"Failed to get voice state">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.switch_voice_region">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"channel_id">> := ChannelIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{channel_id => ChannelId},
            case gen_server:call(Pid, {switch_voice_region, Request}, 10000) of
                #{success := true} ->
                    spawn(fun() ->
                        guild_voice:switch_voice_region(GuildId, ChannelId, Pid)
                    end),
                    #{<<"success">> => true};
                #{error := Error} ->
                    throw({error, Error})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_category_channel_count">>, #{
    <<"guild_id">> := GuildIdBin,
    <<"category_id">> := CategoryIdBin
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    CategoryId = validation:snowflake_or_throw(<<"category_id">>, CategoryIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            Request = #{category_id => CategoryId},
            case gen_server:call(Pid, {get_category_channel_count, Request}, 10000) of
                #{count := Count} ->
                    #{<<"count">> => Count};
                _ ->
                    throw({error, <<"Failed to get category channel count">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end;
execute_method(<<"guild.get_channel_count">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(guild_manager, {start_or_lookup, GuildId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {get_channel_count}, 10000) of
                #{count := Count} ->
                    #{<<"count">> => Count};
                _ ->
                    throw({error, <<"Failed to get channel count">>})
            end;
        _ ->
            throw({error, <<"Guild not found">>})
    end.
