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

-module(guild_virtual_channel_access).

-export([
    add_virtual_access/3,
    remove_virtual_access/3,
    has_virtual_access/3,
    get_virtual_channels_for_user/2,
    get_users_with_virtual_access/2,
    dispatch_channel_visibility_change/4
]).

-import(guild_permissions, [find_channel_by_id/2]).

add_virtual_access(UserId, ChannelId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    UserChannels = maps:get(UserId, VirtualAccess, sets:new()),
    UpdatedUserChannels = sets:add_element(ChannelId, UserChannels),
    UpdatedVirtualAccess = maps:put(UserId, UpdatedUserChannels, VirtualAccess),
    maps:put(virtual_channel_access, UpdatedVirtualAccess, State).

remove_virtual_access(UserId, ChannelId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    case maps:get(UserId, VirtualAccess, undefined) of
        undefined ->
            State;
        UserChannels ->
            UpdatedUserChannels = sets:del_element(ChannelId, UserChannels),
            case sets:size(UpdatedUserChannels) of
                0 ->
                    UpdatedVirtualAccess = maps:remove(UserId, VirtualAccess),
                    maps:put(virtual_channel_access, UpdatedVirtualAccess, State);
                _ ->
                    UpdatedVirtualAccess = maps:put(UserId, UpdatedUserChannels, VirtualAccess),
                    maps:put(virtual_channel_access, UpdatedVirtualAccess, State)
            end
    end.

has_virtual_access(UserId, ChannelId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    case maps:get(UserId, VirtualAccess, undefined) of
        undefined ->
            false;
        UserChannels ->
            sets:is_element(ChannelId, UserChannels)
    end.

get_virtual_channels_for_user(UserId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    case maps:get(UserId, VirtualAccess, undefined) of
        undefined ->
            [];
        UserChannels ->
            sets:to_list(UserChannels)
    end.

get_users_with_virtual_access(ChannelId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    maps:fold(
        fun(UserId, UserChannels, Acc) ->
            case sets:is_element(ChannelId, UserChannels) of
                true -> [UserId | Acc];
                false -> Acc
            end
        end,
        [],
        VirtualAccess
    ).

dispatch_channel_visibility_change(UserId, ChannelId, Action, State) ->
    Channel = find_channel_by_id(ChannelId, State),
    case Channel of
        undefined ->
            ok;
        _ ->
            Sessions = maps:get(sessions, State, #{}),
            GuildId = maps:get(id, State),

            UserSessions = maps:filter(
                fun(_Sid, SessionData) ->
                    maps:get(user_id, SessionData) =:= UserId
                end,
                Sessions
            ),

            case Action of
                add ->
                    ChannelWithGuild = maps:put(
                        <<"guild_id">>, integer_to_binary(GuildId), Channel
                    ),
                    maps:foreach(
                        fun(_Sid, SessionData) ->
                            Pid = maps:get(pid, SessionData),
                            gen_server:cast(Pid, {dispatch, channel_create, ChannelWithGuild})
                        end,
                        UserSessions
                    );
                remove ->
                    ChannelDelete = #{
                        <<"id">> => integer_to_binary(ChannelId),
                        <<"guild_id">> => integer_to_binary(GuildId)
                    },
                    maps:foreach(
                        fun(_Sid, SessionData) ->
                            Pid = maps:get(pid, SessionData),
                            gen_server:cast(Pid, {dispatch, channel_delete, ChannelDelete})
                        end,
                        UserSessions
                    )
            end
    end.
