%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_subscribe).
-typing([eqwalizer]).

-export([
    subscribe_ranges/4,
    unsubscribe_session/2,
    send_member_list_update_to_sessions/5,
    dispatch_sync_to_subscribed_list/7,
    dispatch_sync_to_subscribed_sessions/6
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type range() :: {non_neg_integer(), non_neg_integer()}.
-type channel_id() :: integer().

-export_type([guild_state/0, list_id/0, range/0, channel_id/0]).

-spec subscribe_ranges(binary(), list_id(), [range()], guild_state()) ->
    {guild_state(), boolean(), [range()]}.
subscribe_ranges(SessionId, ListId, Ranges, State) ->
    case valid_list_id(ListId) of
        true ->
            NormalizedRanges = guild_member_list:normalize_ranges(Ranges),
            SubsTab = maps:get(member_list_subscriptions, State),
            {OldRanges, ShouldSync} =
                guild_member_list_subs:subscribe(SessionId, ListId, NormalizedRanges, SubsTab),
            StateWithStore = maybe_ensure_store(ListId, SubsTab, State),
            StateExclusive = enforce_single_session_list(
                SessionId, ListId, NormalizedRanges, SubsTab, StateWithStore
            ),
            RangesToSync = compute_ranges_to_sync(NormalizedRanges, OldRanges, ShouldSync),
            ShouldActuallySync = ShouldSync andalso RangesToSync =/= [],
            {StateExclusive, ShouldActuallySync, RangesToSync};
        false ->
            {State, false, []}
    end.

-spec unsubscribe_session(binary(), guild_state()) -> guild_state().
unsubscribe_session(SessionId, State) ->
    SubsTab = maps:get(member_list_subscriptions, State),
    RemovedListIds = guild_member_list_subs:unsubscribe_session(SessionId, SubsTab),
    lists:foldl(
        fun(ListId, AccState) -> maybe_drop_channel_engine(ListId, SubsTab, AccState) end,
        State,
        RemovedListIds
    ).

-spec enforce_single_session_list(
    binary(), list_id(), [range()], ets:table(), guild_state()
) -> guild_state().
enforce_single_session_list(_SessionId, _ListId, [], _SubsTab, State) ->
    State;
enforce_single_session_list(SessionId, ListId, _Ranges, SubsTab, State) ->
    RemovedListIds = guild_member_list_subs:retain_only_session_list(
        SessionId, ListId, SubsTab
    ),
    lists:foldl(
        fun(RemovedListId, AccState) ->
            maybe_drop_channel_engine(RemovedListId, SubsTab, AccState)
        end,
        State,
        RemovedListIds
    ).

-spec maybe_drop_channel_engine(list_id(), ets:table(), guild_state()) -> guild_state().
maybe_drop_channel_engine(ListId, SubsTab, State) ->
    case map_size(guild_member_list_subs:get_list_subs(ListId, SubsTab)) =:= 0 of
        true -> guild_member_list_channel_engine:drop(ListId, State);
        false -> State
    end.

-spec send_member_list_update_to_sessions(list_id(), map(), map(), map(), guild_state()) -> ok.
send_member_list_update_to_sessions(ListId, ListSubs, Sessions, Payload, State) ->
    case dispatch_context(ListId, State) of
        undefined ->
            ok;
        {GuildId, ChannelId} ->
            do_send_update(ListSubs, Sessions, Payload, ChannelId, GuildId, State)
    end.

-spec do_send_update(
    map(), map(), map(), channel_id() | undefined, pos_integer(), guild_state()
) -> ok.
do_send_update(ListSubs, Sessions, Payload, ChId, GuildId, State) ->
    Encoded = encode_wire_payload(Payload),
    Pids = collect_list_pids(ListSubs, Sessions, ChId, State),
    gateway_dispatch_relay:dispatch_many(
        Pids, guild_member_list_update, Encoded, GuildId
    ).

-spec collect_list_pids(
    map(), map(), channel_id() | undefined, guild_state()
) -> [pid()].
collect_list_pids(ListSubs, Sessions, ChId, State) ->
    maps:fold(
        fun(Sid, _Ranges, Acc) ->
            eligible_list_pid(Sid, Sessions, ChId, State, Acc)
        end,
        [],
        ListSubs
    ).

-spec eligible_list_pid(
    binary(), map(), channel_id() | undefined, guild_state(), [pid()]
) -> [pid()].
eligible_list_pid(Sid, Sessions, ChId, State, Acc) ->
    case maps:get(Sid, Sessions, undefined) of
        #{pid := Pid} = SD when is_pid(Pid) ->
            add_if_viewable(Pid, SD, ChId, State, Acc);
        _ ->
            Acc
    end.

-spec add_if_viewable(pid(), map(), channel_id() | undefined, guild_state(), [pid()]) ->
    [pid()].
add_if_viewable(Pid, SD, ChId, State, Acc) ->
    case session_can_view_list_members(SD, ChId, State) of
        true -> [Pid | Acc];
        false -> Acc
    end.

-spec dispatch_sync_to_subscribed_sessions(
    map(), map(), channel_id() | undefined, integer(), guild_state(), fun(([range()]) -> map())
) -> ok.
dispatch_sync_to_subscribed_sessions(
    _ListSubs, _Sessions, _ChannelId, GuildId, _State, _SyncFun
) when
    not is_integer(GuildId); GuildId =< 0
->
    ok;
dispatch_sync_to_subscribed_sessions(ListSubs, Sessions, ChannelId, GuildId, State, SyncFun) ->
    Groups = collect_sync_range_groups(ListSubs, Sessions, ChannelId, State),
    maps:foreach(
        fun(Ranges, Pids) ->
            dispatch_sync_group(Ranges, Pids, GuildId, SyncFun)
        end,
        Groups
    ).

-spec dispatch_sync_to_subscribed_list(
    list_id(),
    ets:table(),
    map(),
    channel_id() | undefined,
    integer(),
    guild_state(),
    fun(([range()]) -> map())
) -> ok.
dispatch_sync_to_subscribed_list(
    _ListId, _SubsTab, _Sessions, _ChannelId, GuildId, _State, _SyncFun
) when
    not is_integer(GuildId); GuildId =< 0
->
    ok;
dispatch_sync_to_subscribed_list(ListId, SubsTab, Sessions, ChannelId, GuildId, State, SyncFun) ->
    Groups = guild_member_list_subs:fold_list_subs(
        ListId,
        SubsTab,
        fun(SessionId, Ranges, Acc) ->
            collect_sync_session_group(SessionId, Ranges, Sessions, ChannelId, State, Acc)
        end,
        #{}
    ),
    maps:foreach(
        fun(Ranges, Pids) ->
            dispatch_sync_group(Ranges, Pids, GuildId, SyncFun)
        end,
        Groups
    ).

-spec maybe_ensure_store(list_id(), ets:table(), guild_state()) -> guild_state().
maybe_ensure_store(ListId, SubsTab, State) ->
    ensure_channel_engine_for_subs(ListId, SubsTab, State).

-spec ensure_channel_engine_for_subs(list_id(), ets:table(), guild_state()) -> guild_state().
ensure_channel_engine_for_subs(ListId, SubsTab, State) ->
    case map_size(guild_member_list_subs:get_list_subs(ListId, SubsTab)) > 0 of
        true -> guild_member_list_channel_engine:ensure(ListId, State);
        false -> guild_member_list_channel_engine:drop(ListId, State)
    end.

-spec compute_ranges_to_sync([range()], [range()], boolean()) -> [range()].
compute_ranges_to_sync(NormalizedRanges, _OldRanges, false) ->
    NormalizedRanges;
compute_ranges_to_sync(NormalizedRanges, OldRanges, true) ->
    case guild_member_list_sync:is_subset_of_ranges(NormalizedRanges, OldRanges) of
        true ->
            [];
        false ->
            compute_delta_or_full(NormalizedRanges, OldRanges)
    end.

-spec compute_delta_or_full([range()], [range()]) -> [range()].
compute_delta_or_full(NormalizedRanges, OldRanges) ->
    case guild_member_list_sync:compute_range_delta(NormalizedRanges, OldRanges) of
        [] -> NormalizedRanges;
        Delta -> Delta
    end.

-spec collect_sync_range_groups(map(), map(), channel_id() | undefined, guild_state()) ->
    #{[range()] => [pid()]}.
collect_sync_range_groups(ListSubs, Sessions, ChannelId, State) ->
    maps:fold(
        fun(SessionId, Ranges, Acc) ->
            collect_sync_session_group(SessionId, Ranges, Sessions, ChannelId, State, Acc)
        end,
        #{},
        ListSubs
    ).

-spec collect_sync_session_group(
    binary(), [range()], map(), channel_id() | undefined, guild_state(), #{[range()] => [pid()]}
) -> #{[range()] => [pid()]}.
collect_sync_session_group(SessionId, Ranges, Sessions, ChannelId, State, Acc) ->
    case maps:get(SessionId, Sessions, undefined) of
        #{pid := Pid} = SD when is_pid(Pid) ->
            add_sync_pid_if_viewable(Pid, SD, ChannelId, Ranges, State, Acc);
        _ ->
            Acc
    end.

-spec add_sync_pid_if_viewable(
    pid(),
    map(),
    channel_id() | undefined,
    [range()],
    guild_state(),
    #{[range()] => [pid()]}
) -> #{[range()] => [pid()]}.
add_sync_pid_if_viewable(SessionPid, SessionData, ChannelId, Ranges, State, Acc) ->
    case session_can_view_list_members(SessionData, ChannelId, State) of
        true -> Acc#{Ranges => [SessionPid | maps:get(Ranges, Acc, [])]};
        false -> Acc
    end.

-spec dispatch_sync_group([range()], [pid()], integer(), fun(([range()]) -> map())) -> ok.
dispatch_sync_group(_Ranges, [], _GuildId, _SyncFun) ->
    ok;
dispatch_sync_group(Ranges, Pids, GuildId, SyncFun) ->
    SyncResponse = SyncFun(Ranges),
    Encoded = encode_wire_payload(SyncResponse),
    gateway_dispatch_relay:dispatch_many(Pids, guild_member_list_update, Encoded, GuildId).

-spec encode_wire_payload(map()) -> {pre_encoded, binary()}.
encode_wire_payload(Payload) ->
    WirePayload = eqwalizer:dynamic_cast(guild_data_wire:payload(Payload)),
    {pre_encoded, iolist_to_binary(json:encode(WirePayload))}.

-spec session_can_view_list_members(map(), channel_id() | undefined, guild_state()) ->
    boolean().
session_can_view_list_members(SessionData, undefined, State) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId), UserId > 0 ->
            guild_permissions:find_member_by_user_id(UserId, State) =/= undefined;
        _ ->
            false
    end;
session_can_view_list_members(SessionData, ChannelId, State) ->
    guild_member_list_connected:session_can_view_channel_members(SessionData, ChannelId, State).

-spec dispatch_context(list_id(), guild_state()) ->
    {pos_integer(), channel_id() | undefined} | undefined.
dispatch_context(ListId, State) ->
    case {valid_list_id(ListId), guild_id(State)} of
        {true, GuildId} when is_integer(GuildId), GuildId > 0 ->
            {GuildId, list_channel_id(ListId)};
        _ ->
            undefined
    end.

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    case snowflake_id:parse_optional(maps:get(id, State, undefined)) of
        GuildId when is_integer(GuildId), GuildId > 0 -> GuildId;
        _ -> undefined
    end.

-spec valid_list_id(term()) -> boolean().
valid_list_id(<<"0">>) ->
    true;
valid_list_id(ListId) when is_binary(ListId) ->
    case snowflake_id:parse_optional(ListId) of
        Id when is_integer(Id), Id > 0 -> true;
        _ -> false
    end;
valid_list_id(_) ->
    false.

-spec list_channel_id(term()) -> channel_id() | undefined.
list_channel_id(<<"0">>) ->
    undefined;
list_channel_id(ListId) when is_binary(ListId) ->
    case snowflake_id:parse_optional(ListId) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> undefined
    end;
list_channel_id(_) ->
    undefined.
