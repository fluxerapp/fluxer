%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_connected).
-typing([eqwalizer]).

-export([
    default_presence/0,
    resolve_presence_for_user/2,
    presence_context/1,
    add_presence_to_member/2,
    add_presence_to_member/3,
    connected_session_user_ids/1,
    user_is_online/2,
    partition_members_by_online/2,
    filter_members_for_list/3,
    list_id_channel_id/1,
    session_can_view_channel/3,
    session_can_view_channel_members/3,
    presence_status_changed/3,
    member_in_list/2
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
        <<"afk">> => false,
        <<"activities">> => []
    }.

-spec resolve_presence_for_user(guild_state(), user_id()) -> map().
resolve_presence_for_user(State, UserId) ->
    case maps:get(member_presence, State, undefined) of
        undefined -> default_presence();
        Tab -> guild_state_member:lookup_presence(Tab, UserId)
    end.

-spec add_presence_to_member(map(), guild_state()) -> map().
add_presence_to_member(Member, State) ->
    Presence = resolve_presence_for_member(presence_context(State), Member),
    Member#{<<"presence">> => Presence}.

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

-spec partition_members_by_online([map()], guild_state()) -> {[map()], [map()]}.
partition_members_by_online(Members, State) ->
    ConnectedUserIds = connected_session_user_ids(State),
    PresenceCtx = #{
        member_presence => maps:get(member_presence, State, undefined),
        connected_user_ids => ConnectedUserIds
    },
    lists:partition(
        fun(Member) ->
            member_is_online(Member, PresenceCtx, ConnectedUserIds)
        end,
        Members
    ).

-spec member_is_online(map(), map(), sets:set()) -> boolean().
member_is_online(Member, PresenceCtx, ConnectedUserIds) ->
    Presence = resolve_presence_for_member(PresenceCtx, Member),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    IsConnected = member_is_connected(Member, ConnectedUserIds),
    IsOnlineStatus = Status =/= <<"offline">> andalso Status =/= <<"invisible">>,
    IsConnected andalso IsOnlineStatus.

-spec resolve_presence_for_member(map(), map()) -> map().
resolve_presence_for_member(PresenceCtx, Member) ->
    case guild_member_list_common:get_member_user_id(Member) of
        UserId when is_integer(UserId) ->
            resolve_effective_presence_for_user(PresenceCtx, UserId);
        undefined ->
            default_presence()
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

-spec member_is_connected(map(), sets:set()) -> boolean().
member_is_connected(Member, ConnectedUserIds) ->
    case guild_member_list_common:get_member_user_id(Member) of
        UserId when is_integer(UserId) ->
            sets:is_element(UserId, ConnectedUserIds);
        undefined ->
            false
    end.

-spec filter_members_for_list(list_id(), [map()], guild_state()) -> [map()].
filter_members_for_list(<<"0">>, Members, _State) ->
    [
        Member
     || Member <- Members,
        is_integer(guild_member_list_common:get_member_user_id(Member))
    ];
filter_members_for_list(ListId, Members, State) ->
    case list_id_channel_id(ListId) of
        undefined ->
            [];
        ChannelId ->
            filter_members_for_channel(ChannelId, Members, State)
    end.

-spec filter_members_for_channel(channel_id(), [map()], guild_state()) -> [map()].
filter_members_for_channel(ChannelId, Members, State) ->
    lists:filter(
        fun(Member) ->
            member_can_view_channel(Member, ChannelId, State)
        end,
        Members
    ).

-spec member_can_view_channel(map(), channel_id(), guild_state()) -> boolean().
member_can_view_channel(Member, ChannelId, State) ->
    case guild_member_list_common:get_member_user_id(Member) of
        UserId when is_integer(UserId) ->
            guild_permissions:can_view_channel(UserId, ChannelId, Member, State);
        undefined ->
            false
    end.

-spec list_id_channel_id(list_id()) -> channel_id() | undefined.
list_id_channel_id(ListId) when is_binary(ListId) ->
    case snowflake_id:parse_maybe(ListId) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> undefined
    end;
list_id_channel_id(_) ->
    undefined.

-spec session_can_view_channel(map(), channel_id(), guild_state()) -> boolean().
session_can_view_channel(_SessionData, ChannelId, _State) when
    not is_integer(ChannelId); ChannelId =< 0
->
    false;
session_can_view_channel(SessionData, ChannelId, State) ->
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

-spec presence_status_changed(user_id(), guild_state(), guild_state()) -> boolean().
presence_status_changed(UserId, OldState, UpdatedState) ->
    OldPresence = resolve_presence_for_user(OldState, UserId),
    NewPresence = resolve_presence_for_user(UpdatedState, UserId),
    OldStatus = maps:get(<<"status">>, OldPresence, <<"offline">>),
    NewStatus = maps:get(<<"status">>, NewPresence, <<"offline">>),
    OldStatus =/= NewStatus.

-spec member_in_list(user_id(), [map()]) -> boolean().
member_in_list(UserId, Members) ->
    lists:any(fun(M) -> guild_member_list_common:get_member_user_id(M) =:= UserId end, Members).
