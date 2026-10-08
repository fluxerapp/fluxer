%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_connected).
-typing([eqwalizer]).

-export([
    default_presence/0,
    resolve_presence_for_user/2,
    presence_context/1,
    add_presence_to_member/3,
    connected_session_user_ids/1,
    user_is_online/2,
    session_can_view_channel/3,
    session_can_view_channel_members/3
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type user_id() :: integer().
-type channel_id() :: integer().

-export_type([guild_state/0, list_id/0, user_id/0, channel_id/0]).

-spec default_presence() -> map().
default_presence() ->
    #{
        <<"status">> => <<"offline">>,
        <<"mobile">> => false,
        <<"afk">> => false
    }.

-spec resolve_presence_for_user(guild_state(), user_id()) -> map().
resolve_presence_for_user(State, UserId) ->
    case maps:get(member_presence, State, undefined) of
        undefined -> default_presence();
        Tab -> guild_state_member:lookup_presence(Tab, UserId)
    end.

-spec add_presence_to_member(map(), user_id(), map()) -> map().
add_presence_to_member(Member, UserId, PresenceCtx) when is_integer(UserId), UserId > 0 ->
    Presence = resolve_effective_presence_for_user(PresenceCtx, UserId),
    Member#{<<"presence">> => Presence};
add_presence_to_member(Member, _UserId, _PresenceCtx) ->
    Member#{<<"presence">> => default_presence()}.

-spec presence_context(guild_state()) -> map().
presence_context(State) ->
    #{
        member_presence => maps:get(member_presence, State, undefined),
        connected_user_ids => connected_session_user_ids(State)
    }.

-spec connected_session_user_ids(guild_state()) -> sets:set(integer()).
connected_session_user_ids(State) ->
    case maps:find(connected_user_ids, State) of
        {ok, Set} -> Set;
        error -> rebuild_connected_user_ids(State)
    end.

-spec user_is_online(user_id(), guild_state()) -> boolean().
user_is_online(UserId, State) when is_integer(UserId), UserId > 0 ->
    Presence = resolve_effective_presence_for_user(presence_context(State), UserId),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>;
user_is_online(_UserId, _State) ->
    false.

-spec rebuild_connected_user_ids(guild_state()) -> sets:set(integer()).
rebuild_connected_user_ids(State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun add_connected_session_user/3,
        sets:new(),
        Sessions
    ).

-spec add_connected_session_user(term(), map(), sets:set(integer())) -> sets:set(integer()).
add_connected_session_user(_SessionId, SessionData, Acc) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId), UserId > 0 ->
            sets:add_element(UserId, Acc);
        _ ->
            Acc
    end.

-spec resolve_effective_presence_for_user(map(), user_id()) -> map().
resolve_effective_presence_for_user(PresenceCtx, UserId) ->
    Presence = resolve_presence_from_context(PresenceCtx, UserId),
    case presence_is_visible_in_guild(UserId, Presence, PresenceCtx) of
        true -> Presence;
        false -> default_presence()
    end.

-spec resolve_presence_from_context(map(), user_id()) -> map().
resolve_presence_from_context(#{member_presence := undefined}, _UserId) ->
    default_presence();
resolve_presence_from_context(#{member_presence := PresenceTable}, UserId) ->
    guild_state_member:lookup_presence(PresenceTable, UserId).

-spec presence_is_visible_in_guild(user_id(), map(), map()) -> boolean().
presence_is_visible_in_guild(UserId, Presence, #{connected_user_ids := ConnectedUserIds}) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    IsOnlineStatus = Status =/= <<"offline">> andalso Status =/= <<"invisible">>,
    IsOnlineStatus andalso sets:is_element(UserId, ConnectedUserIds).

-spec session_can_view_channel(map(), channel_id(), guild_state()) -> boolean().
session_can_view_channel(_SessionData, ChannelId, _State) when
    not is_integer(ChannelId); ChannelId =< 0
->
    false;
session_can_view_channel(SessionData, ChannelId, State) ->
    session_can_view_channel_by_permissions(SessionData, ChannelId, State) andalso
        guild_thread_gate:channel_visible(SessionData, ChannelId, State).

-spec session_can_view_channel_by_permissions(map(), channel_id(), guild_state()) -> boolean().
session_can_view_channel_by_permissions(SessionData, ChannelId, State) ->
    case
        {
            maps:get(user_id, SessionData, undefined),
            maps:get(viewable_channels, SessionData, undefined)
        }
    of
        {UserId, ViewableChannels} when
            is_integer(UserId), UserId > 0, is_map(ViewableChannels)
        ->
            maps:is_key(ChannelId, ViewableChannels) orelse
                guild_permissions:can_view_channel(UserId, ChannelId, undefined, State);
        {UserId, _} when is_integer(UserId), UserId > 0 ->
            guild_permissions:can_view_channel(UserId, ChannelId, undefined, State);
        _ ->
            false
    end.

-spec session_can_view_channel_members(map(), channel_id(), guild_state()) -> boolean().
session_can_view_channel_members(_SessionData, ChannelId, _State) when
    not is_integer(ChannelId); ChannelId =< 0
->
    false;
session_can_view_channel_members(SessionData, ChannelId, State) ->
    session_can_view_channel(SessionData, ChannelId, State) andalso
        session_user_can_view_channel_members(SessionData, ChannelId, State).

-spec session_user_can_view_channel_members(map(), channel_id(), guild_state()) -> boolean().
session_user_can_view_channel_members(SessionData, ChannelId, State) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId), UserId > 0 ->
            guild_permissions:can_view_channel_members(UserId, ChannelId, undefined, State);
        _ ->
            false
    end.
