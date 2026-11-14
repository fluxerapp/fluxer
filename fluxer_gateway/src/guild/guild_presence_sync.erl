%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_presence_sync).

-typing([eqwalizer]).

-export([
    sync_online_status/2,
    sync_member_data/2,
    partition_subscribed_sessions/5,
    get_user_viewable_channel_map/3,
    session_pids/2
]).

-export_type([guild_state/0, user_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().

-spec sync_online_status(user_id(), guild_state()) -> ok.
sync_online_status(UserId, State) ->
    IsOnline = compute_is_online(UserId, State),
    case maps:find(member_list_engine, State) of
        error ->
            ok;
        {ok, Ref} ->
            guild_member_list_engine:set_online(Ref, UserId, IsOnline)
    end,
    guild_member_list_channel_engine:sync_online(UserId, IsOnline, State).

-spec sync_member_data(user_id(), guild_state()) -> ok.
sync_member_data(UserId, State) ->
    Data = maps:get(data, State, #{}),
    case guild_data_index_members:get_member(UserId, Data) of
        undefined -> ok;
        Member -> sync_local_nif_member_data(UserId, Member, State)
    end.

-spec sync_local_nif_member_data(user_id(), map(), guild_state()) -> ok.
sync_local_nif_member_data(UserId, Member, State) ->
    ok = sync_default_member_list_data(UserId, Member, State),
    guild_member_list_channel_engine:update_user_all(UserId, State).

-spec sync_default_member_list_data(user_id(), map(), guild_state()) -> ok.
sync_default_member_list_data(UserId, Member, State) ->
    case maps:find(member_list_engine, State) of
        error ->
            ok;
        {ok, Ref} ->
            DisplayName = guild_member_list_common:get_member_display_name(Member),
            SortKey = guild_member_list_common:casefold_binary(DisplayName),
            RoleIds = guild_member_list_store:extract_role_ids(Member),
            IsOnline = compute_is_online(UserId, State),
            guild_member_list_engine:update_member(Ref, UserId, SortKey, RoleIds, IsOnline)
    end.

-spec compute_is_online(user_id(), guild_state()) -> boolean().
compute_is_online(UserId, State) ->
    ConnectedUserIds = guild_member_list_common:connected_session_user_ids(State),
    Presence = guild_state_member:lookup_presence(maps:get(member_presence, State), UserId),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    IsConnected = sets:is_element(UserId, ConnectedUserIds),
    IsConnected andalso Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec partition_subscribed_sessions([binary()], map(), map(), user_id(), guild_state()) ->
    {[binary()], [binary()]}.
partition_subscribed_sessions(SessionIds, Sessions, TargetChannelMap, TargetUserId, State) ->
    lists:foldl(
        fun(SessionId, {Valids, Invalids}) ->
            partition_session(SessionId, Sessions, TargetChannelMap, TargetUserId, State, {
                Valids, Invalids
            })
        end,
        {[], []},
        SessionIds
    ).

-spec partition_session(
    binary(), map(), map(), user_id(), guild_state(), {[binary()], [binary()]}
) -> {[binary()], [binary()]}.
partition_session(
    SessionId, Sessions, TargetChannelMap, TargetUserId, State, {Valids, Invalids}
) ->
    case classify_session(SessionId, Sessions, TargetChannelMap, TargetUserId, State) of
        valid -> {[SessionId | Valids], Invalids};
        invalid -> {Valids, [SessionId | Invalids]}
    end.

-spec classify_session(binary(), map(), map(), user_id(), guild_state()) -> valid | invalid.
classify_session(SessionId, Sessions, TargetChannelMap, TargetUserId, State) ->
    case maps:find(SessionId, Sessions) of
        error ->
            invalid;
        {ok, SessionData} ->
            classify_session_data(SessionData, TargetChannelMap, TargetUserId, State)
    end.

-spec classify_session_data(map(), map(), user_id(), guild_state()) -> valid | invalid.
classify_session_data(SessionData, TargetChannelMap, TargetUserId, State) ->
    SessionUserId = maps:get(user_id, SessionData, undefined),
    case SessionUserId of
        undefined -> invalid;
        TargetUserId -> invalid;
        _ -> classify_non_target_session(SessionData, SessionUserId, TargetChannelMap, State)
    end.

-spec classify_non_target_session(map(), user_id(), map(), guild_state()) -> valid | invalid.
classify_non_target_session(SessionData, SessionUserId, TargetChannelMap, State) ->
    case session_shares_channels(SessionData, SessionUserId, TargetChannelMap, State) of
        true -> valid;
        false -> invalid
    end.

-spec session_shares_channels(map(), user_id(), map(), guild_state()) -> boolean().
session_shares_channels(SessionData, SessionUserId, TargetChannelMap, State) ->
    case maps:find(viewable_channels, SessionData) of
        {ok, ViewableMap} when is_map(ViewableMap) ->
            map_utils:maps_share_any_key(ViewableMap, TargetChannelMap);
        _ ->
            session_channel_list_shares(SessionUserId, TargetChannelMap, State)
    end.

-spec session_channel_list_shares(user_id(), map(), guild_state()) -> boolean().
session_channel_list_shares(SessionUserId, TargetChannelMap, State) ->
    SessionChannelList = guild_visibility_channels:get_user_viewable_channels(
        SessionUserId, State
    ),
    lists:any(fun(Ch) -> maps:is_key(Ch, TargetChannelMap) end, SessionChannelList).

-spec get_user_viewable_channel_map(user_id(), map(), guild_state()) -> map().
get_user_viewable_channel_map(UserId, Sessions, State) ->
    case find_session_viewable_channels_for_user(UserId, Sessions) of
        undefined ->
            ChannelList = guild_visibility_channels:get_user_viewable_channels(UserId, State),
            maps:from_list([{Ch, true} || Ch <- ChannelList]);
        ViewableMap ->
            ViewableMap
    end.

-spec find_session_viewable_channels_for_user(user_id(), map()) -> map() | undefined.
find_session_viewable_channels_for_user(UserId, Sessions) ->
    find_session_viewable_channels_iter(UserId, maps:iterator(Sessions)).

-spec find_session_viewable_channels_iter(user_id(), maps:iterator()) -> map() | undefined.
find_session_viewable_channels_iter(UserId, Iterator) ->
    case maps:next(Iterator) of
        none ->
            undefined;
        {_, SessionData, NextIterator} when is_map(SessionData) ->
            viewable_channels_or_continue(UserId, SessionData, NextIterator);
        {_, _, NextIterator} ->
            find_session_viewable_channels_iter(UserId, NextIterator)
    end.

-spec viewable_channels_or_continue(user_id(), map(), maps:iterator()) -> map() | undefined.
viewable_channels_or_continue(UserId, SessionData, NextIterator) ->
    ViewableChannels = maps:get(viewable_channels, SessionData, undefined),
    case {maps:get(user_id, SessionData, undefined), ViewableChannels} of
        {UserId, Map} when is_map(Map) ->
            Map;
        _ ->
            find_session_viewable_channels_iter(UserId, NextIterator)
    end.

-spec session_pids([binary()], map()) -> [pid()].
session_pids(SessionIds, Sessions) ->
    lists:filtermap(
        fun(SessionId) ->
            session_pid(SessionId, Sessions)
        end,
        SessionIds
    ).

-spec session_pid(binary(), map()) -> {true, pid()} | false.
session_pid(SessionId, Sessions) ->
    case maps:find(SessionId, Sessions) of
        {ok, SessionData} -> session_data_pid(SessionData);
        error -> false
    end.

-spec session_data_pid(map()) -> {true, pid()} | false.
session_data_pid(SessionData) ->
    case maps:get(pid, SessionData, undefined) of
        Pid when is_pid(Pid) -> {true, Pid};
        _ -> false
    end.
