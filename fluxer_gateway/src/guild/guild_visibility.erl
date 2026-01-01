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

-module(guild_visibility).

-export([
    get_user_viewable_channels/2,
    compute_and_dispatch_visibility_changes/2,
    viewable_channel_set/2,
    have_shared_viewable_channel/3
]).

-import(guild_member_list, [calculate_list_id/2, build_sync_response/4]).
-import(guild_permissions, [can_view_channel/4, find_member_by_user_id/2, find_channel_by_id/2]).

-spec get_user_viewable_channels(integer(), map()) -> [integer()].
get_user_viewable_channels(UserId, State) ->
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    Member = find_member_by_user_id(UserId, State),

    case Member of
        undefined ->
            [];
        _ ->
            lists:filtermap(
                fun(Channel) ->
                    ChannelId = map_utils:get_integer(Channel, <<"id">>, undefined),
                    case ChannelId of
                        undefined ->
                            false;
                        _ ->
                            case can_view_channel(UserId, ChannelId, Member, State) of
                                true -> {true, ChannelId};
                                false -> false
                            end
                    end
                end,
                Channels
            )
    end.

-spec viewable_channel_set(integer(), map()) -> sets:set().
viewable_channel_set(UserId, State) when is_integer(UserId) ->
    sets:from_list(get_user_viewable_channels(UserId, State));
viewable_channel_set(_, _) ->
    sets:new().

-spec have_shared_viewable_channel(integer(), integer(), map()) -> boolean().
have_shared_viewable_channel(UserId, OtherUserId, State) when is_integer(UserId), is_integer(OtherUserId), UserId =/= OtherUserId ->
    SetA = viewable_channel_set(UserId, State),
    SetB = viewable_channel_set(OtherUserId, State),
    not sets:is_empty(sets:intersection(SetA, SetB));
have_shared_viewable_channel(_, _, _) ->
    false.

-spec compute_and_dispatch_visibility_changes(map(), map()) -> ok.
compute_and_dispatch_visibility_changes(OldState, NewState) ->
    Sessions = maps:get(sessions, NewState, #{}),
    GuildId = maps:get(id, NewState, 0),

    lists:foreach(
        fun({SessionId, SessionData}) ->
            UserId = maps:get(user_id, SessionData),
            Pid = maps:get(pid, SessionData),

            OldViewable = get_user_viewable_channels(UserId, OldState),
            NewViewable = get_user_viewable_channels(UserId, NewState),

            OldSet = sets:from_list(OldViewable),
            NewSet = sets:from_list(NewViewable),

            Removed = sets:subtract(OldSet, NewSet),
            Added = sets:subtract(NewSet, OldSet),

            lists:foreach(
                fun(ChannelId) ->
                    dispatch_channel_delete(ChannelId, Pid, OldState, GuildId)
                end,
                sets:to_list(Removed)
            ),

            lists:foreach(
                fun(ChannelId) ->
                    dispatch_channel_create(ChannelId, Pid, NewState, GuildId),
                    send_member_list_sync(SessionId, SessionData, ChannelId, GuildId, NewState)
                end,
                sets:to_list(Added)
            )
        end,
        maps:to_list(Sessions)
    ),
    ok.

dispatch_channel_delete(ChannelId, SessionPid, OldState, GuildId) ->
    case is_pid(SessionPid) of
        true ->
            case find_channel_by_id(ChannelId, OldState) of
                undefined ->
                    ok;
                _Channel ->
                    ChannelDelete = #{
                        <<"id">> => integer_to_binary(ChannelId),
                        <<"guild_id">> => integer_to_binary(GuildId)
                    },
                    gen_server:cast(SessionPid, {dispatch, channel_delete, ChannelDelete})
            end;
        false ->
            ok
    end.

dispatch_channel_create(ChannelId, SessionPid, NewState, GuildId) ->
    case is_pid(SessionPid) of
        true ->
            case find_channel_by_id(ChannelId, NewState) of
                undefined ->
                    ok;
                Channel ->
                    ChannelWithGuild = maps:put(
                        <<"guild_id">>, integer_to_binary(GuildId), Channel
                    ),
                    gen_server:cast(SessionPid, {dispatch, channel_create, ChannelWithGuild})
            end;
        false ->
            ok
    end.

send_member_list_sync(SessionId, SessionData, ChannelId, GuildId, State) ->
    SessionPid = maps:get(pid, SessionData),
    case is_pid(SessionPid) of
        false ->
            ok;
        true ->
            ListId = calculate_list_id(ChannelId, State),
            MemberListSubs = maps:get(member_list_subscriptions, State, #{}),
            ListSubs = maps:get(ListId, MemberListSubs, #{}),
            Ranges = maps:get(SessionId, ListSubs, []),
            case Ranges of
                [] ->
                    ok;
                _ ->
                    SessionUserId = maps:get(user_id, SessionData),
                    case can_send_member_list(SessionUserId, ChannelId, State) of
                        true ->
                            SyncResponse = build_sync_response(GuildId, ListId, Ranges, State),
                            SyncResponseWithChannel = maps:put(<<"channel_id">>, integer_to_binary(ChannelId), SyncResponse),
                            gen_server:cast(SessionPid, {dispatch, guild_member_list_update, SyncResponseWithChannel});
                        false ->
                            ok
                    end
            end
    end.

can_send_member_list(UserId, ChannelId, State) ->
    is_integer(UserId) andalso
        guild_permissions:can_view_channel(UserId, ChannelId, undefined, State).
