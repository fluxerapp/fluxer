%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_permissions).
-typing([eqwalizer]).

-export([check_voice_permissions_and_limits/6, users_in_channel/2]).

-export_type([
    guild_state/0,
    voice_state_map/0,
    channel/0
]).

-type guild_state() :: map().
-type voice_state_map() :: #{binary() => map()}.
-type channel() :: map().
-type channel_voice_stats() :: #{
    users := sets:set(integer()),
    any_camera_active := boolean(),
    user_connection_count := non_neg_integer()
}.

-define(DEFAULT_VOICE_CONNECTION_LIMIT, 5).
-define(MAX_VOICE_CONNECTION_LIMIT, 100).
-define(UNLIMITED_VOICE_USER_LIMIT, 0).

-spec check_voice_permissions_and_limits(
    integer(), integer(), channel(), voice_state_map(), guild_state(), boolean()
) ->
    {ok, allowed} | {error, atom(), atom()}.
check_voice_permissions_and_limits(
    UserId, ChannelIdValue, Channel, VoiceStates, State, IsUpdate
) ->
    case voice_join_allowed(UserId, ChannelIdValue, Channel, VoiceStates, State, IsUpdate) of
        ok -> {ok, allowed};
        {error, ErrorAtom} -> gateway_errors:error(ErrorAtom)
    end.

-spec voice_join_allowed(
    integer(), integer(), channel(), voice_state_map(), guild_state(), boolean()
) -> ok | {error, atom()}.
voice_join_allowed(UserId, ChannelIdValue, Channel, VoiceStates, State, IsUpdate) ->
    case is_member_timed_out(UserId, State) of
        true ->
            {error, voice_member_timed_out};
        false ->
            voice_perms_allowed(UserId, ChannelIdValue, Channel, VoiceStates, State, IsUpdate)
    end.

-spec voice_perms_allowed(
    integer(), integer(), channel(), voice_state_map(), guild_state(), boolean()
) -> ok | {error, atom()}.
voice_perms_allowed(UserId, ChId, Channel, VS, State, IsUpdate) ->
    case has_view_and_connect_perms(UserId, ChId, State) of
        false ->
            {error, voice_permission_denied};
        true ->
            voice_capacity_allowed(UserId, ChId, Channel, VS, State, IsUpdate)
    end.

-spec voice_capacity_allowed(
    integer(), integer(), channel(), voice_state_map(), guild_state(), boolean()
) -> ok | {error, atom()}.
voice_capacity_allowed(UserId, ChId, Channel, VS, State, IsUpdate) ->
    Stats = channel_voice_stats(UserId, ChId, VS),
    case channel_has_capacity(UserId, Channel, Stats, IsUpdate) of
        false ->
            {error, voice_channel_full};
        true ->
            voice_connection_limit_allowed(
                UserId, ChId, Channel, Stats, State, IsUpdate
            )
    end.

-spec voice_connection_limit_allowed(
    integer(), integer(), channel(), channel_voice_stats(), guild_state(), boolean()
) -> ok | {error, atom()}.
voice_connection_limit_allowed(UserId, ChannelIdValue, Channel, Stats, State, IsUpdate) ->
    case
        channel_allows_user_connection_limit(
            UserId, ChannelIdValue, Channel, Stats, State, IsUpdate
        )
    of
        true -> ok;
        false -> {error, voice_connection_limit_reached}
    end.

-spec has_view_and_connect_perms(integer(), integer(), guild_state()) -> boolean().
has_view_and_connect_perms(UserId, ChannelIdValue, State) ->
    guild_virtual_channel_access:has_voice_access(UserId, ChannelIdValue, State) orelse
        guild_virtual_channel_access:is_move_pending(UserId, ChannelIdValue, State) orelse
        has_resolved_view_and_connect_perms(UserId, ChannelIdValue, State).

-spec has_resolved_view_and_connect_perms(integer(), integer(), guild_state()) -> boolean().
has_resolved_view_and_connect_perms(UserId, ChannelIdValue, State) ->
    Permissions = resolve_permissions(UserId, ChannelIdValue, State),
    ViewPerm = constants:view_channel_permission(),
    ConnectPerm = constants:connect_permission(),
    HasView = permission_bits:has(Permissions, ViewPerm),
    HasConnect = permission_bits:has(Permissions, ConnectPerm),
    HasView andalso HasConnect.

-spec channel_has_capacity(integer(), channel(), channel_voice_stats(), boolean()) ->
    boolean().
channel_has_capacity(UserId, Channel, Stats, IsUpdate) ->
    UserLimit = channel_user_limit(Channel),
    AnyCameraActive = maps:get(any_camera_active, Stats),
    EffectiveLimit = effective_user_limit(UserLimit, AnyCameraActive),
    check_effective_limit(EffectiveLimit, UserId, Stats, IsUpdate).

-spec check_effective_limit(integer(), integer(), channel_voice_stats(), boolean()) ->
    boolean().
check_effective_limit(?UNLIMITED_VOICE_USER_LIMIT, _UserId, _Stats, _IsUpdate) ->
    true;
check_effective_limit(Limit, UserId, Stats, IsUpdate) when Limit > 0 ->
    UsersInChannel = maps:get(users, Stats),
    CurrentCount = sets:size(UsersInChannel),
    AlreadyPresent = sets:is_element(UserId, UsersInChannel),
    AdjustedCount = adjusted_user_count(CurrentCount, AlreadyPresent, IsUpdate),
    AdjustedCount < Limit;
check_effective_limit(_, _UserId, _Stats, _IsUpdate) ->
    true.

-spec adjusted_user_count(integer(), boolean(), boolean()) -> integer().
adjusted_user_count(CurrentCount, AlreadyPresent, IsUpdate) ->
    case AlreadyPresent orelse IsUpdate of
        true -> CurrentCount - 1;
        false -> CurrentCount
    end.

-spec channel_voice_stats(integer(), integer(), voice_state_map()) -> channel_voice_stats().
channel_voice_stats(UserId, ChannelIdValue, VoiceStates0) ->
    VoiceStates = voice_state_utils:ensure_voice_states(VoiceStates0),
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            add_channel_voice_state_stats(UserId, ChannelIdValue, VoiceState, Acc)
        end,
        #{users => sets:new(), any_camera_active => false, user_connection_count => 0},
        VoiceStates
    ).

-spec add_channel_voice_state_stats(integer(), integer(), map(), channel_voice_stats()) ->
    channel_voice_stats().
add_channel_voice_state_stats(UserId, ChannelIdValue, VoiceState, Acc) ->
    case voice_state_utils:voice_state_channel_id(VoiceState) of
        ChannelIdValue ->
            add_matching_channel_voice_state_stats(UserId, VoiceState, Acc);
        _ ->
            Acc
    end.

-spec add_matching_channel_voice_state_stats(integer(), map(), channel_voice_stats()) ->
    channel_voice_stats().
add_matching_channel_voice_state_stats(UserId, VoiceState, Acc) ->
    VoiceUserId = voice_state_utils:voice_state_user_id(VoiceState),
    Acc1 = add_voice_state_user_to_stats(VoiceUserId, Acc),
    Acc2 = update_user_connection_count(UserId, VoiceUserId, Acc1),
    update_camera_active(VoiceState, Acc2).

-spec add_voice_state_user_to_stats(integer() | undefined, channel_voice_stats()) ->
    channel_voice_stats().
add_voice_state_user_to_stats(undefined, Acc) ->
    Acc;
add_voice_state_user_to_stats(VoiceUserId, Acc) ->
    Acc#{users => sets:add_element(VoiceUserId, maps:get(users, Acc))}.

-spec update_user_connection_count(integer(), integer() | undefined, channel_voice_stats()) ->
    channel_voice_stats().
update_user_connection_count(UserId, UserId, Acc) ->
    Acc#{user_connection_count => maps:get(user_connection_count, Acc) + 1};
update_user_connection_count(_UserId, _VoiceUserId, Acc) ->
    Acc.

-spec update_camera_active(map(), channel_voice_stats()) -> channel_voice_stats().
update_camera_active(VoiceState, Acc) ->
    case maps:get(<<"self_video">>, VoiceState, false) of
        true -> Acc#{any_camera_active => true};
        _ -> Acc
    end.

-spec channel_user_limit(channel()) -> non_neg_integer().
channel_user_limit(Channel) ->
    case map_utils:get_integer(Channel, <<"user_limit">>, undefined) of
        Limit when is_integer(Limit), Limit >= 0 -> Limit;
        _ -> ?UNLIMITED_VOICE_USER_LIMIT
    end.

-spec effective_user_limit(integer(), boolean()) -> integer().
effective_user_limit(?UNLIMITED_VOICE_USER_LIMIT, false) -> ?UNLIMITED_VOICE_USER_LIMIT;
effective_user_limit(?UNLIMITED_VOICE_USER_LIMIT, true) -> 25;
effective_user_limit(Limit, false) -> Limit;
effective_user_limit(Limit, true) -> min(Limit, 25).

-spec channel_allows_user_connection_limit(
    integer(), integer(), channel(), channel_voice_stats(), guild_state(), boolean()
) -> boolean().
channel_allows_user_connection_limit(
    UserId, ChannelIdValue, Channel, Stats, State, IsUpdate
) ->
    Limit = effective_voice_connection_limit(Channel),
    ActiveCount = maps:get(user_connection_count, Stats),
    PendingCount = pending_user_connection_count(UserId, ChannelIdValue, State),
    AdjustedActiveCount =
        case IsUpdate andalso ActiveCount > 0 of
            true -> ActiveCount - 1;
            false -> ActiveCount
        end,
    AdjustedActiveCount + PendingCount < Limit.

-spec effective_voice_connection_limit(channel()) -> integer().
effective_voice_connection_limit(Channel) ->
    case
        map_utils:get_integer(
            Channel, <<"voice_connection_limit">>, ?DEFAULT_VOICE_CONNECTION_LIMIT
        )
    of
        Limit when is_integer(Limit), Limit >= 1, Limit =< ?MAX_VOICE_CONNECTION_LIMIT ->
            Limit;
        Limit when is_integer(Limit), Limit > ?MAX_VOICE_CONNECTION_LIMIT ->
            ?MAX_VOICE_CONNECTION_LIMIT;
        _ ->
            ?DEFAULT_VOICE_CONNECTION_LIMIT
    end.

-spec pending_user_connection_count(integer(), integer(), guild_state()) -> non_neg_integer().
pending_user_connection_count(UserId, ChannelIdValue, State) ->
    PendingConns = maps:get(pending_voice_connections, State, #{}),
    Now = erlang:system_time(millisecond),
    maps:fold(
        fun(_ConnId, PendingData, Acc) ->
            increment_if_pending_matches(UserId, ChannelIdValue, PendingData, Now, Acc)
        end,
        0,
        PendingConns
    ).

-spec increment_if_pending_matches(
    integer(), integer(), map(), integer(), non_neg_integer()
) -> non_neg_integer().
increment_if_pending_matches(UserId, ChannelIdValue, PendingData, Now, Acc) ->
    case pending_connection_matches(UserId, ChannelIdValue, PendingData, Now) of
        true -> Acc + 1;
        false -> Acc
    end.

-spec pending_connection_matches(integer(), integer(), map(), integer()) -> boolean().
pending_connection_matches(UserId, ChannelIdValue, PendingData, Now) ->
    PendingUserId = map_utils:get_integer(PendingData, user_id, undefined),
    PendingChannelId = map_utils:get_integer(PendingData, channel_id, undefined),
    ExpiresAt = map_utils:get_integer(PendingData, expires_at, undefined),
    PendingUserId =:= UserId andalso PendingChannelId =:= ChannelIdValue andalso
        (ExpiresAt =:= undefined orelse ExpiresAt > Now).

-spec is_member_timed_out(integer(), guild_state()) -> boolean().
is_member_timed_out(UserId, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            member_timed_out(Member)
    end.

-spec member_timed_out(map()) -> boolean().
member_timed_out(Member) ->
    TimeoutMs = utils:parse_iso8601_to_unix_ms(
        maps:get(<<"communication_disabled_until">>, Member, undefined)
    ),
    case TimeoutMs of
        undefined -> false;
        Value when is_integer(Value) -> Value > erlang:system_time(millisecond)
    end.

-spec users_in_channel(integer(), voice_state_map()) -> sets:set(integer()).
users_in_channel(ChannelIdValue, VoiceStates0) ->
    VoiceStates = voice_state_utils:ensure_voice_states(VoiceStates0),
    maps:fold(
        fun(_ConnId, VState, Acc) ->
            add_user_if_voice_state_in_channel(VState, ChannelIdValue, Acc)
        end,
        sets:new(),
        VoiceStates
    ).

-spec add_user_if_voice_state_in_channel(map(), integer(), sets:set(integer())) ->
    sets:set(integer()).
add_user_if_voice_state_in_channel(VState, ChannelIdValue, Acc) ->
    case voice_state_utils:voice_state_channel_id(VState) of
        ChannelIdValue -> add_voice_state_user(VState, Acc);
        _ -> Acc
    end.

-spec add_voice_state_user(map(), sets:set(integer())) -> sets:set(integer()).
add_voice_state_user(VState, Acc) ->
    case voice_state_utils:voice_state_user_id(VState) of
        undefined -> Acc;
        UserId -> sets:add_element(UserId, Acc)
    end.

-spec resolve_permissions(integer(), integer(), guild_state()) -> integer().
resolve_permissions(UserId, ChannelIdValue, State) ->
    case State of
        #{test_perm_fun := Fun} when is_function(Fun, 1) ->
            Fun(UserId);
        _ ->
            guild_permissions:get_member_permissions(UserId, ChannelIdValue, State)
    end.
