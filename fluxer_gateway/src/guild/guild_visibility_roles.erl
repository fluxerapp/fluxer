%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_roles).
-typing([eqwalizer]).

-export([
    dispatch_channel_delete/4,
    dispatch_channel_create/4,
    send_member_list_sync/5,
    maybe_ensure_parent_category_visible/5,
    can_send_member_list/3
]).

-export_type([guild_state/0, user_id/0, channel_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().

-spec maybe_ensure_parent_category_visible(
    channel_id(), map(), guild_state(), map(), integer()
) -> map().
maybe_ensure_parent_category_visible(ChannelId, ViewableMap, State, SessionData, GuildId) ->
    case {snowflake_id(GuildId), guild_permissions:find_channel_by_id(ChannelId, State)} of
        {undefined, _} ->
            ViewableMap;
        {_, undefined} ->
            ViewableMap;
        {ResolvedGuildId, Channel} ->
            ensure_parent_category_visible(
                channel_parent_id(Channel), ViewableMap, State, SessionData, ResolvedGuildId
            )
    end.

-spec ensure_parent_category_visible(
    channel_id() | undefined, map(), guild_state(), map(), integer()
) -> map().
ensure_parent_category_visible(undefined, ViewableMap, _State, _SessionData, _GuildId) ->
    ViewableMap;
ensure_parent_category_visible(
    ParentId, ViewableMap, State, #{pid := Pid} = SessionData, GuildId
) when
    is_pid(Pid)
->
    case maps:is_key(ParentId, ViewableMap) of
        true ->
            ViewableMap;
        false ->
            dispatch_channel_create(ParentId, SessionData, State, GuildId),
            ViewableMap#{ParentId => true}
    end;
ensure_parent_category_visible(_ParentId, ViewableMap, _State, _SessionData, _GuildId) ->
    ViewableMap.

-spec dispatch_channel_delete(channel_id(), pid(), guild_state(), integer()) -> ok.
dispatch_channel_delete(ChannelId, SessionPid, OldState, GuildId) ->
    case {is_pid(SessionPid), snowflake_id(ChannelId), snowflake_id(GuildId)} of
        {true, ResolvedChannelId, ResolvedGuildId} when
            is_integer(ResolvedChannelId), is_integer(ResolvedGuildId)
        ->
            maybe_dispatch_channel_delete(
                ResolvedChannelId, SessionPid, OldState, ResolvedGuildId
            );
        _ ->
            ok
    end.

-spec dispatch_channel_create(channel_id(), map(), guild_state(), integer()) -> ok.
dispatch_channel_create(ChannelId, SessionData, NewState, GuildId) ->
    case
        {maps:get(pid, SessionData, undefined), snowflake_id(ChannelId), snowflake_id(GuildId)}
    of
        {SessionPid, ResolvedChannelId, ResolvedGuildId} when
            is_pid(SessionPid), is_integer(ResolvedChannelId), is_integer(ResolvedGuildId)
        ->
            maybe_dispatch_channel_create(
                ResolvedChannelId, SessionData, NewState, ResolvedGuildId
            );
        _ ->
            ok
    end.

-spec send_member_list_sync(binary(), map(), channel_id(), integer(), guild_state()) -> ok.
send_member_list_sync(SessionId, SessionData, ChannelId, GuildId, State) ->
    SessionPid = maps:get(pid, SessionData),
    case {is_pid(SessionPid), snowflake_id(ChannelId), snowflake_id(GuildId)} of
        {true, ResolvedChannelId, ResolvedGuildId} when
            is_integer(ResolvedChannelId), is_integer(ResolvedGuildId)
        ->
            do_send_member_list_sync(
                SessionId, SessionData, SessionPid, ResolvedChannelId, ResolvedGuildId, State
            );
        _ ->
            ok
    end.

-spec do_send_member_list_sync(
    binary(), map(), pid(), channel_id(), integer(), guild_state()
) -> ok.
do_send_member_list_sync(SessionId, SessionData, SessionPid, ChannelId, GuildId, State) ->
    case guild_member_list:calculate_list_id(ChannelId, State) of
        undefined ->
            ok;
        ListId ->
            send_sync_for_list(
                SessionId, SessionData, SessionPid, ChannelId, GuildId, ListId, State
            )
    end.

-spec send_sync_for_list(
    binary(), map(), pid(), channel_id(), integer(), binary(), guild_state()
) -> ok.
send_sync_for_list(SessionId, SessionData, SessionPid, ChannelId, GuildId, ListId, State) ->
    SubsTab = maps:get(member_list_subscriptions, State),
    Ranges = guild_member_list_subs:get_session_ranges(SessionId, ListId, SubsTab),
    case Ranges of
        [] ->
            ok;
        _ ->
            SessionUserId = maps:get(user_id, SessionData),
            maybe_dispatch_sync(
                SessionUserId, SessionPid, ChannelId, GuildId, ListId, Ranges, State
            )
    end.

-spec maybe_dispatch_sync(
    user_id() | undefined, pid(), channel_id(), integer(), binary(), list(), guild_state()
) -> ok.
maybe_dispatch_sync(SessionUserId, SessionPid, ChannelId, GuildId, ListId, Ranges, State) ->
    case can_send_member_list(SessionUserId, ChannelId, State) of
        true ->
            SyncResponse = guild_member_list:build_sync_response(
                GuildId, ListId, Ranges, State
            ),
            SyncResponseWithChannel = SyncResponse#{
                <<"channel_id">> => integer_to_binary(ChannelId)
            },
            gateway_dispatch_relay:dispatch(
                SessionPid, guild_member_list_update, SyncResponseWithChannel, GuildId
            );
        false ->
            ok
    end.

-spec can_send_member_list(user_id() | undefined, channel_id(), guild_state()) -> boolean().
can_send_member_list(UserId, ChannelId, State) ->
    case snowflake_id(ChannelId) of
        ResolvedChannelId when is_integer(UserId), is_integer(ResolvedChannelId) ->
            guild_permissions:can_view_channel(UserId, ResolvedChannelId, undefined, State);
        undefined ->
            false
    end.

-spec maybe_dispatch_channel_delete(channel_id(), pid(), guild_state(), integer()) -> ok.
maybe_dispatch_channel_delete(ChannelId, SessionPid, OldState, GuildId) ->
    case guild_permissions:find_channel_by_id(ChannelId, OldState) of
        undefined ->
            ok;
        _Channel ->
            ChannelDelete = #{
                <<"id">> => integer_to_binary(ChannelId),
                <<"guild_id">> => integer_to_binary(GuildId)
            },
            gateway_dispatch_relay:dispatch(SessionPid, channel_delete, ChannelDelete, GuildId)
    end.

-spec maybe_dispatch_channel_create(channel_id(), map(), guild_state(), integer()) -> ok.
maybe_dispatch_channel_create(ChannelId, #{pid := SessionPid} = SessionData, NewState, GuildId) ->
    case guild_permissions:find_channel_by_id(ChannelId, NewState) of
        undefined ->
            ok;
        Channel ->
            SessionChannel = session_channel(Channel, SessionData, NewState),
            ChannelWithGuild = SessionChannel#{<<"guild_id">> => integer_to_binary(GuildId)},
            gateway_dispatch_relay:dispatch(
                SessionPid, channel_create, ChannelWithGuild, GuildId
            )
    end.

-spec session_channel(map(), map(), guild_state()) -> map().
session_channel(Channel, SessionData, State) ->
    case
        guild_thread_gate:needs_variant(State) andalso
            not guild_thread_gate:session_viewer(SessionData)
    of
        true ->
            guild_thread_gate:mask_overwrites(guild_thread_gate:strip_channel_surface(Channel));
        false ->
            Channel
    end.

-spec channel_parent_id(map()) -> channel_id() | undefined.
channel_parent_id(Channel) ->
    snowflake_id(maps:get(<<"parent_id">>, Channel, undefined)).

-spec snowflake_id(term()) -> channel_id() | undefined.
snowflake_id(Value) ->
    case snowflake_id:parse_optional(Value) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> undefined
    end.
