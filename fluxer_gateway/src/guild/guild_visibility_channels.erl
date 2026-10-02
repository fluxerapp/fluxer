%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_channels).
-typing([eqwalizer]).

-export([
    get_user_viewable_channels/2,
    shares_viewable_channel/3,
    viewable_channel_set/2,
    have_shared_viewable_channel/3,
    viewable_channel_map/1,
    get_cached_viewable_channel_map/2,
    cached_viewable_channel_set/3,
    connected_voice_channel_set/2,
    connected_voice_channel_sets/1,
    preserve_connected_channels/4,
    filter_connected_session_entries/1,
    ensure_viewable_channel_map/3,
    channel_is_visible/4,
    ensure_new_channel_visibility/4,
    ensure_new_channel_visibility/5,
    update_viewable_map_for_channel/3
]).

-export_type([guild_state/0, user_id/0, channel_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().

-spec get_user_viewable_channels(user_id(), guild_state()) -> [channel_id()].
get_user_viewable_channels(UserId, State) ->
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    case Member of
        undefined ->
            [];
        _ ->
            Base = guild_permissions:member_base_permissions(UserId, Member, State),
            compute_viewable_with_categories(UserId, Base, Channels, State)
    end.

-spec compute_viewable_with_categories(
    user_id(), guild_permissions:base_permissions(), [map()], guild_state()
) -> [channel_id()].
compute_viewable_with_categories(UserId, Base, Channels, State) ->
    Viewable = guild_permissions:viewable_channel_ids(UserId, Base, Channels, State),
    {ViewableIds, ViewableIdSet, NeededParentIds} = lists:foldl(
        fun(Channel, {Ids, IdSet, Parents}) ->
            collect_viewable_channel(Channel, Viewable, {Ids, IdSet, Parents})
        end,
        {[], #{}, #{}},
        Channels
    ),
    MissingParents = maps:without(maps:keys(ViewableIdSet), NeededParentIds),
    case map_size(MissingParents) of
        0 ->
            lists:reverse(ViewableIds);
        _ ->
            ExtraIds = collect_missing_parent_ids(Channels, MissingParents),
            lists:reverse(ViewableIds) ++ ExtraIds
    end.

-spec shares_viewable_channel(user_id(), map(), guild_state()) -> boolean().
shares_viewable_channel(UserId, ChannelMap, State) ->
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            lists:any(
                fun(Channel) ->
                    shares_channel_or_parent(
                        Channel, UserId, Member, ChannelMap, Channels, State
                    )
                end,
                Channels
            )
    end.

-spec shares_channel_or_parent(map(), user_id(), map(), map(), [map()], guild_state()) ->
    boolean().
shares_channel_or_parent(Channel, UserId, Member, ChannelMap, Channels, State) ->
    channel_viewable(UserId, Member, Channel, State) andalso
        (maps:is_key(channel_id(Channel), ChannelMap) orelse
            shared_parent(channel_parent_id(Channel), ChannelMap, Channels)).

-spec shared_parent(channel_id() | undefined, map(), [map()]) -> boolean().
shared_parent(undefined, _ChannelMap, _Channels) ->
    false;
shared_parent(ParentId, ChannelMap, Channels) ->
    maps:is_key(ParentId, ChannelMap) andalso
        lists:any(fun(C) -> channel_id(C) =:= ParentId end, Channels).

-spec collect_missing_parent_ids([map()], map()) -> [channel_id()].
collect_missing_parent_ids(Channels, MissingParents) ->
    lists:filtermap(
        fun(C) -> is_missing_parent(C, MissingParents) end,
        Channels
    ).

-spec is_missing_parent(map(), map()) -> {true, channel_id()} | false.
is_missing_parent(C, MissingParents) ->
    check_missing_parent(channel_id(C), MissingParents).

-spec check_missing_parent(channel_id() | undefined, map()) -> {true, channel_id()} | false.
check_missing_parent(CId, MissingParents) when is_integer(CId) ->
    case maps:is_key(CId, MissingParents) of
        true -> {true, CId};
        false -> false
    end;
check_missing_parent(_, _) ->
    false.

-spec collect_viewable_channel(map(), #{channel_id() => true}, {list(), map(), map()}) ->
    {list(), map(), map()}.
collect_viewable_channel(Channel, Viewable, {Ids, IdSet, Parents}) ->
    case maps:is_key(channel_id(Channel), Viewable) of
        false ->
            {Ids, IdSet, Parents};
        true ->
            CId = channel_id(Channel),
            PId = channel_parent_id(Channel),
            NewParents = maybe_add_parent_id(PId, Parents),
            {[CId | Ids], IdSet#{CId => true}, NewParents}
    end.

-spec maybe_add_parent_id(channel_id() | undefined, map()) -> map().
maybe_add_parent_id(undefined, Parents) ->
    Parents;
maybe_add_parent_id(PId, Parents) ->
    Parents#{PId => true}.

-spec viewable_channel_set(user_id(), guild_state()) -> sets:set(channel_id()).
viewable_channel_set(UserId, State) when is_integer(UserId) ->
    case get_cached_viewable_channel_map(UserId, State) of
        undefined ->
            sets:from_list(get_user_viewable_channels(UserId, State));
        ViewableChannelMap ->
            sets:from_list(maps:keys(ViewableChannelMap))
    end;
viewable_channel_set(_, _) ->
    sets:new().

-spec get_cached_viewable_channel_map(user_id(), guild_state()) -> map() | undefined.
get_cached_viewable_channel_map(UserId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    find_viewable_iter(UserId, maps:iterator(Sessions)).

-spec find_viewable_iter(user_id(), maps:iterator()) -> map() | undefined.
find_viewable_iter(UserId, Iterator) ->
    case maps:next(Iterator) of
        none ->
            undefined;
        {_, SessionData, Next} when is_map(SessionData) ->
            extract_or_find_next_viewable(UserId, SessionData, Next);
        {_, _, Next} ->
            find_viewable_iter(UserId, Next)
    end.

-spec extract_or_find_next_viewable(user_id(), map(), maps:iterator()) -> map() | undefined.
extract_or_find_next_viewable(UserId, SessionData, Next) ->
    case try_extract_viewable(UserId, SessionData) of
        undefined -> find_viewable_iter(UserId, Next);
        Map -> Map
    end.

-spec try_extract_viewable(user_id(), map()) -> map() | undefined.
try_extract_viewable(UserId, SessionData) ->
    ViewableChannels = maps:get(viewable_channels, SessionData, undefined),
    case {maps:get(user_id, SessionData, undefined), ViewableChannels} of
        {UserId, Map} when is_map(Map) ->
            Map;
        _ ->
            undefined
    end.

-spec have_shared_viewable_channel(user_id(), user_id(), guild_state()) -> boolean().
have_shared_viewable_channel(UserId, OtherUserId, State) when
    is_integer(UserId), is_integer(OtherUserId), UserId =/= OtherUserId
->
    MapA = cached_or_current_viewable_channel_map(UserId, State),
    ListB = get_user_viewable_channels(OtherUserId, State),
    lists:any(fun(ChId) -> maps:is_key(ChId, MapA) end, ListB);
have_shared_viewable_channel(_, _, _) ->
    false.

-spec cached_or_current_viewable_channel_map(user_id(), guild_state()) -> map().
cached_or_current_viewable_channel_map(UserId, State) ->
    case get_cached_viewable_channel_map(UserId, State) of
        undefined ->
            viewable_channel_map(sets:from_list(get_user_viewable_channels(UserId, State)));
        Map ->
            Map
    end.

-spec filter_connected_session_entries(map()) -> [{binary(), map()}].
filter_connected_session_entries(Sessions) ->
    maps:fold(fun collect_connected_session/3, [], Sessions).

-spec collect_connected_session(binary(), map(), [{binary(), map()}]) -> [{binary(), map()}].
collect_connected_session(SessionId, SessionData, Acc) ->
    case session_connected(SessionData) of
        true -> [{SessionId, SessionData} | Acc];
        false -> Acc
    end.

-spec session_connected(map()) -> boolean().
session_connected(SessionData) ->
    case maps:get(pending_connect, SessionData, false) of
        true -> false;
        _ -> true
    end.

-spec cached_viewable_channel_set(map(), user_id(), guild_state()) -> sets:set(channel_id()).
cached_viewable_channel_set(SessionData, UserId, State) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableMap when is_map(ViewableMap) ->
            sets:from_list(maps:keys(ViewableMap));
        _ ->
            sets:from_list(get_user_viewable_channels(UserId, State))
    end.

-spec viewable_channel_map(sets:set(channel_id())) -> #{channel_id() => true}.
viewable_channel_map(ChannelSet) ->
    sets:fold(
        fun(ChannelId, Acc) -> Acc#{ChannelId => true} end,
        #{},
        ChannelSet
    ).

-spec connected_voice_channel_set(user_id(), guild_state()) -> sets:set(channel_id()).
connected_voice_channel_set(UserId, State) ->
    maps:get(UserId, connected_voice_channel_sets(State), sets:new()).

-spec connected_voice_channel_sets(guild_state()) -> #{user_id() => sets:set(channel_id())}.
connected_voice_channel_sets(State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            add_voice_channel_to_user_set(VoiceState, Acc)
        end,
        #{},
        VoiceStates
    ).

-spec add_voice_channel_to_user_set(map(), #{user_id() => sets:set(channel_id())}) ->
    #{user_id() => sets:set(channel_id())}.
add_voice_channel_to_user_set(VoiceState, Acc) ->
    VoiceUserId = voice_state_utils:voice_state_user_id(VoiceState),
    VoiceChannelId = voice_state_utils:voice_state_channel_id(VoiceState),
    case {VoiceUserId, VoiceChannelId} of
        {UserId, ChannelId} when is_integer(UserId), is_integer(ChannelId) ->
            Channels = maps:get(UserId, Acc, sets:new()),
            Acc#{UserId => sets:add_element(ChannelId, Channels)};
        _ ->
            Acc
    end.

-spec preserve_connected_channels(
    user_id(), sets:set(channel_id()), sets:set(channel_id()), guild_state()
) -> {guild_state(), sets:set(channel_id())}.
preserve_connected_channels(UserId, RemovedSet, ConnectedSet, State) ->
    ToPreserve = sets:intersection(RemovedSet, ConnectedSet),
    UpdatedState = sets:fold(
        fun(ChannelId, AccState) ->
            grant_virtual_access_if_needed(UserId, ChannelId, AccState)
        end,
        State,
        ToPreserve
    ),
    {UpdatedState, ToPreserve}.

-spec grant_virtual_access_if_needed(user_id(), channel_id(), guild_state()) -> guild_state().
grant_virtual_access_if_needed(UserId, ChannelId, State) ->
    case guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) of
        true ->
            State;
        false ->
            State1 = guild_virtual_channel_access:add_virtual_access(UserId, ChannelId, State),
            guild_virtual_channel_access:clear_pending_join(UserId, ChannelId, State1)
    end.

-spec ensure_viewable_channel_map(map(), user_id(), guild_state()) -> #{channel_id() => true}.
ensure_viewable_channel_map(SessionData, UserId, State) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableChannels when is_map(ViewableChannels) ->
            ViewableChannels;
        _ ->
            viewable_channel_map(sets:from_list(get_user_viewable_channels(UserId, State)))
    end.

-spec channel_is_visible(user_id(), channel_id(), map() | undefined, guild_state()) ->
    boolean().
channel_is_visible(UserId, ChannelId, Member, State) ->
    guild_permissions:can_view_channel(UserId, ChannelId, Member, State).

-spec ensure_new_channel_visibility(
    user_id(), channel_id(), sets:set(channel_id()), guild_state()
) -> {guild_state(), boolean()}.
ensure_new_channel_visibility(UserId, ChannelId, ConnectedSet, State) ->
    NewMember = guild_permissions:find_member_by_user_id(UserId, State),
    ensure_new_channel_visibility(UserId, ChannelId, ConnectedSet, NewMember, State).

-spec ensure_new_channel_visibility(
    user_id(), channel_id(), sets:set(channel_id()), map() | undefined, guild_state()
) -> {guild_state(), boolean()}.
ensure_new_channel_visibility(UserId, ChannelId, ConnectedSet, NewMember, State) ->
    NewVisible0 = channel_is_visible(UserId, ChannelId, NewMember, State),
    case {NewVisible0, sets:is_element(ChannelId, ConnectedSet)} of
        {true, _} ->
            {State, true};
        {false, false} ->
            {State, false};
        {false, true} ->
            maybe_grant_virtual_access(UserId, ChannelId, State)
    end.

-spec maybe_grant_virtual_access(user_id(), channel_id(), guild_state()) ->
    {guild_state(), boolean()}.
maybe_grant_virtual_access(UserId, ChannelId, State) ->
    case guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) of
        true ->
            {State, true};
        false ->
            State1 = guild_virtual_channel_access:add_virtual_access(UserId, ChannelId, State),
            State2 = guild_virtual_channel_access:clear_pending_join(UserId, ChannelId, State1),
            {State2, true}
    end.

-spec update_viewable_map_for_channel(map(), channel_id(), boolean()) -> map().
update_viewable_map_for_channel(ViewableMap, ChannelId, true) ->
    ViewableMap#{ChannelId => true};
update_viewable_map_for_channel(ViewableMap, ChannelId, false) ->
    maps:remove(ChannelId, ViewableMap).

-spec channel_viewable(user_id(), map(), map(), guild_state()) -> boolean().
channel_viewable(UserId, Member, Channel, State) ->
    case channel_id(Channel) of
        ChannelId when is_integer(ChannelId) ->
            guild_permissions:can_view_channel(UserId, ChannelId, Member, State);
        undefined ->
            false
    end.

-spec channel_id(map()) -> channel_id() | undefined.
channel_id(Channel) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, Channel, undefined)).

-spec channel_parent_id(map()) -> channel_id() | undefined.
channel_parent_id(Channel) ->
    snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Channel, undefined)).
