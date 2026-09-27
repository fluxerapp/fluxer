%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_virtual_channel_access).
-typing([eqwalizer]).

-export([
    add_virtual_access/3,
    remove_virtual_access/3,
    has_virtual_access/3,
    get_virtual_channels_for_user/2,
    get_users_with_virtual_access/2,
    dispatch_channel_visibility_change/4,
    mark_pending_join/3,
    clear_pending_join/3,
    mark_transition_flags/3,
    clear_transition_flags/3,
    is_pending_join/3,
    mark_preserve/3,
    clear_preserve/3,
    has_preserve/3,
    mark_move_pending/3,
    clear_move_pending/3,
    is_move_pending/3
]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().
-export_type([guild_state/0, user_id/0, channel_id/0]).

-spec add_virtual_access(user_id(), channel_id(), guild_state()) -> guild_state().
add_virtual_access(UserId, ChannelId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    UserChannels = maps:get(UserId, VirtualAccess, sets:new()),
    Updated = sets:add_element(ChannelId, UserChannels),
    State1 = State#{virtual_channel_access => VirtualAccess#{UserId => Updated}},
    State2 = update_user_session_view_cache(UserId, ChannelId, add, State1),
    mark_pending_join(
        UserId, ChannelId, guild_member_list_engine_inputs:mark_stale(ChannelId, State2)
    ).

-spec remove_virtual_access(user_id(), channel_id(), guild_state()) -> guild_state().
remove_virtual_access(UserId, ChannelId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    case maps:find(UserId, VirtualAccess) of
        error ->
            State;
        {ok, UserChannels} ->
            remove_channel_from_user(UserId, ChannelId, UserChannels, State)
    end.

-spec remove_channel_from_user(user_id(), channel_id(), sets:set(), guild_state()) ->
    guild_state().
remove_channel_from_user(UserId, ChannelId, UserChannels, State0) ->
    Updated = sets:del_element(ChannelId, UserChannels),
    State = guild_member_list_engine_inputs:mark_stale(ChannelId, State0),
    case sets:size(Updated) of
        0 -> remove_all_user_virtual_access(UserId, State);
        _ -> update_user_virtual_access(UserId, ChannelId, Updated, State)
    end.

-spec remove_all_user_virtual_access(user_id(), guild_state()) -> guild_state().
remove_all_user_virtual_access(UserId, State) ->
    VCA = maps:get(virtual_channel_access, State, #{}),
    VCP = maps:get(virtual_channel_access_pending, State, #{}),
    VCPr = maps:get(virtual_channel_access_preserve, State, #{}),
    VCM = maps:get(virtual_channel_access_move_pending, State, #{}),
    State1 = State#{
        virtual_channel_access => maps:remove(UserId, VCA),
        virtual_channel_access_pending => maps:remove(UserId, VCP),
        virtual_channel_access_preserve => maps:remove(UserId, VCPr),
        virtual_channel_access_move_pending => maps:remove(UserId, VCM)
    },
    clear_user_session_view_cache(UserId, State1).

-spec update_user_virtual_access(user_id(), channel_id(), sets:set(), guild_state()) ->
    guild_state().
update_user_virtual_access(UserId, ChannelId, UpdatedChans, State) ->
    VCA = maps:get(virtual_channel_access, State, #{}),
    VCP = maps:get(virtual_channel_access_pending, State, #{}),
    VCPr = maps:get(virtual_channel_access_preserve, State, #{}),
    VCM = maps:get(virtual_channel_access_move_pending, State, #{}),
    State1 = State#{
        virtual_channel_access => VCA#{UserId => UpdatedChans},
        virtual_channel_access_pending => del_from_user_set(UserId, ChannelId, VCP),
        virtual_channel_access_preserve => del_from_user_set(UserId, ChannelId, VCPr),
        virtual_channel_access_move_pending => del_from_user_set(UserId, ChannelId, VCM)
    },
    update_user_session_view_cache(UserId, ChannelId, remove, State1).

-spec del_from_user_set(user_id(), channel_id(), map()) -> map().
del_from_user_set(UserId, ChannelId, Map) ->
    UserSet = maps:get(UserId, Map, sets:new()),
    Map#{UserId => sets:del_element(ChannelId, UserSet)}.

-spec clear_from_user_set(user_id(), channel_id(), map()) -> map().
clear_from_user_set(UserId, ChannelId, Map) ->
    UserSet = maps:get(UserId, Map, sets:new()),
    Updated = sets:del_element(ChannelId, UserSet),
    case sets:size(Updated) of
        0 -> maps:remove(UserId, Map);
        _ -> Map#{UserId => Updated}
    end.

-spec has_virtual_access(user_id(), channel_id(), guild_state()) -> boolean().
has_virtual_access(UserId, ChannelId, State) ->
    user_channel_check(UserId, ChannelId, virtual_channel_access, State).

-spec get_virtual_channels_for_user(user_id(), guild_state()) -> [channel_id()].
get_virtual_channels_for_user(UserId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    case maps:find(UserId, VirtualAccess) of
        error -> [];
        {ok, UserChannels} -> sets:to_list(UserChannels)
    end.

-spec user_channel_check(user_id(), channel_id(), atom(), guild_state()) -> boolean().
user_channel_check(UserId, ChannelId, Key, State) ->
    Map = maps:get(Key, State, #{}),
    case maps:find(UserId, Map) of
        error -> false;
        {ok, UserSet} -> sets:is_element(ChannelId, UserSet)
    end.

-spec mark_transition_flags(user_id(), channel_id(), guild_state()) -> guild_state().
mark_transition_flags(UserId, ChannelId, State) ->
    S0 = mark_pending_join(UserId, ChannelId, State),
    S1 = mark_preserve(UserId, ChannelId, S0),
    mark_move_pending(UserId, ChannelId, S1).

-spec clear_transition_flags(user_id(), channel_id(), guild_state()) -> guild_state().
clear_transition_flags(UserId, ChannelId, State) ->
    S0 = clear_pending_join(UserId, ChannelId, State),
    S1 = clear_preserve(UserId, ChannelId, S0),
    clear_move_pending(UserId, ChannelId, S1).

-spec mark_pending_join(user_id(), channel_id(), guild_state()) -> guild_state().
mark_pending_join(UserId, ChannelId, State) ->
    PendingMap = maps:get(virtual_channel_access_pending, State, #{}),
    UserPending = sets:add_element(ChannelId, maps:get(UserId, PendingMap, sets:new())),
    State#{virtual_channel_access_pending => PendingMap#{UserId => UserPending}}.

-spec clear_pending_join(user_id(), channel_id(), guild_state()) -> guild_state().
clear_pending_join(UserId, ChannelId, State) ->
    Pending = maps:get(virtual_channel_access_pending, State, #{}),
    State#{
        virtual_channel_access_pending =>
            clear_from_user_set(UserId, ChannelId, Pending)
    }.

-spec is_pending_join(user_id(), channel_id(), guild_state()) -> boolean().
is_pending_join(UserId, ChannelId, State) ->
    user_channel_check(UserId, ChannelId, virtual_channel_access_pending, State).

-spec mark_preserve(user_id(), channel_id(), guild_state()) -> guild_state().
mark_preserve(UserId, ChannelId, State) ->
    PreserveMap = maps:get(virtual_channel_access_preserve, State, #{}),
    UserPreserve = sets:add_element(ChannelId, maps:get(UserId, PreserveMap, sets:new())),
    State#{virtual_channel_access_preserve => PreserveMap#{UserId => UserPreserve}}.

-spec clear_preserve(user_id(), channel_id(), guild_state()) -> guild_state().
clear_preserve(UserId, ChannelId, State) ->
    Preserve = maps:get(virtual_channel_access_preserve, State, #{}),
    State#{
        virtual_channel_access_preserve =>
            clear_from_user_set(UserId, ChannelId, Preserve)
    }.

-spec has_preserve(user_id(), channel_id(), guild_state()) -> boolean().
has_preserve(UserId, ChannelId, State) ->
    user_channel_check(UserId, ChannelId, virtual_channel_access_preserve, State).

-spec mark_move_pending(user_id(), channel_id(), guild_state()) -> guild_state().
mark_move_pending(UserId, ChannelId, State) ->
    MoveMap = maps:get(virtual_channel_access_move_pending, State, #{}),
    UserMoves = sets:add_element(ChannelId, maps:get(UserId, MoveMap, sets:new())),
    State#{virtual_channel_access_move_pending => MoveMap#{UserId => UserMoves}}.

-spec clear_move_pending(user_id(), channel_id(), guild_state()) -> guild_state().
clear_move_pending(UserId, ChannelId, State) ->
    MovePend = maps:get(virtual_channel_access_move_pending, State, #{}),
    State#{
        virtual_channel_access_move_pending =>
            clear_from_user_set(UserId, ChannelId, MovePend)
    }.

-spec is_move_pending(user_id(), channel_id(), guild_state()) -> boolean().
is_move_pending(UserId, ChannelId, State) ->
    user_channel_check(UserId, ChannelId, virtual_channel_access_move_pending, State).

-spec get_users_with_virtual_access(channel_id(), guild_state()) -> [user_id()].
get_users_with_virtual_access(ChannelId, State) ->
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    maps:fold(
        fun(UserId, UserChannels, Acc) ->
            collect_user_with_access(UserId, ChannelId, UserChannels, Acc)
        end,
        [],
        VirtualAccess
    ).

-spec collect_user_with_access(user_id(), channel_id(), sets:set(), [user_id()]) -> [user_id()].
collect_user_with_access(UserId, ChannelId, UserChannels, Acc) ->
    case sets:is_element(ChannelId, UserChannels) of
        true -> [UserId | Acc];
        false -> Acc
    end.

-spec dispatch_channel_visibility_change(user_id(), channel_id(), add | remove, guild_state()) ->
    ok.
dispatch_channel_visibility_change(UserId, ChannelId, Action, State) ->
    case guild_permissions:find_channel_by_id(ChannelId, State) of
        undefined ->
            ok;
        Channel ->
            dispatch_visibility_for_channel(UserId, ChannelId, Action, Channel, State)
    end.

-spec dispatch_visibility_for_channel(
    user_id(), channel_id(), add | remove, map(), guild_state()
) -> ok.
dispatch_visibility_for_channel(UserId, ChannelId, Action, Channel, State) ->
    Sessions = maps:get(sessions, State, #{}),
    GuildId = maps:get(id, State),
    UserSessions = maps:filter(
        fun(_Sid, SD) -> maps:get(user_id, SD) =:= UserId end,
        Sessions
    ),
    dispatch_to_user_sessions(Action, Channel, ChannelId, GuildId, UserSessions).

-spec dispatch_to_user_sessions(add | remove, map(), channel_id(), integer(), map()) -> ok.
dispatch_to_user_sessions(add, Channel, ChannelId, GuildId, UserSessions) ->
    ChannelWithGuild = Channel#{
        <<"id">> => integer_to_binary(ChannelId),
        <<"guild_id">> => integer_to_binary(GuildId)
    },
    gateway_dispatch_relay:dispatch_many(
        [
            Pid
         || SessionData <- maps:values(UserSessions),
            Pid <- [maps:get(pid, SessionData)],
            is_pid(Pid)
        ],
        channel_create,
        ChannelWithGuild,
        GuildId
    );
dispatch_to_user_sessions(remove, _Channel, ChannelId, GuildId, UserSessions) ->
    ChannelDelete = #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"guild_id">> => integer_to_binary(GuildId)
    },
    gateway_dispatch_relay:dispatch_many(
        [
            Pid
         || SessionData <- maps:values(UserSessions),
            Pid <- [maps:get(pid, SessionData)],
            is_pid(Pid)
        ],
        channel_delete,
        ChannelDelete,
        GuildId
    ).

-spec update_user_session_view_cache(user_id(), channel_id(), add | remove, guild_state()) ->
    guild_state().
update_user_session_view_cache(UserId, ChannelId, Action, State) ->
    Sessions = maps:get(sessions, State, #{}),
    Updated = maps:map(
        fun(_Sid, SD) ->
            maybe_update_view(SD, UserId, ChannelId, Action)
        end,
        Sessions
    ),
    State#{sessions => Updated}.

-spec maybe_update_view(map(), user_id(), channel_id(), add | remove) -> map().
maybe_update_view(SD, UserId, ChannelId, Action) ->
    case maps:get(user_id, SD, undefined) of
        UserId -> update_session_view_cache(SD, ChannelId, Action);
        _ -> SD
    end.

-spec clear_user_session_view_cache(user_id(), guild_state()) -> guild_state().
clear_user_session_view_cache(UserId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    Updated = maps:map(
        fun(_Sid, SD) -> maybe_clear_view(SD, UserId) end,
        Sessions
    ),
    State#{sessions => Updated}.

-spec maybe_clear_view(map(), user_id()) -> map().
maybe_clear_view(SD, UserId) ->
    case maps:get(user_id, SD, undefined) of
        UserId -> SD#{viewable_channels => #{}};
        _ -> SD
    end.

-spec update_session_view_cache(map(), channel_id(), add | remove) -> map().
update_session_view_cache(SessionData, ChannelId, add) ->
    VC = ensure_viewable_channel_map(maps:get(viewable_channels, SessionData, #{})),
    SessionData#{viewable_channels => VC#{ChannelId => true}};
update_session_view_cache(SessionData, ChannelId, remove) ->
    VC = ensure_viewable_channel_map(maps:get(viewable_channels, SessionData, #{})),
    SessionData#{viewable_channels => maps:remove(ChannelId, VC)}.

-spec ensure_viewable_channel_map(term()) -> map().
ensure_viewable_channel_map(ViewableChannels) when is_map(ViewableChannels) ->
    ViewableChannels;
ensure_viewable_channel_map(_) ->
    #{}.
