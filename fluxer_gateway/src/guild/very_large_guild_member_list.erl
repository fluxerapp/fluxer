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

-module(very_large_guild_member_list).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type guild_id() :: integer().
-type shard_index() :: non_neg_integer().
-type session_id() :: binary().
-type user_id() :: integer().
-type channel_id() :: integer().
-type list_id() :: binary().
-type range() :: {non_neg_integer(), non_neg_integer()}.
-type ranges() :: [range()].

-type subscriptions() :: #{list_id() => #{session_id() => ranges()}}.

-type state() :: #{
    id := guild_id(),
    coordinator_pid := pid(),
    shard0_pid := pid(),
    subscriptions := subscriptions(),
    session_routes := #{session_id() => shard_index()},
    user_session_counts := #{user_id() => pos_integer()},
    member_presence := #{user_id() => map()},
    virtual_channel_access := #{user_id() => sets:set(channel_id())},
    base_data := map(),
    snapshot := map(),
    compute_inflight := boolean(),
    pending_delta := boolean(),
    pending_delta_user_id => user_id(),
    pending_full_sync_all := boolean(),
    pending_full_sync_channels := sets:set(channel_id()),
    pending_refresh_base_data := boolean(),
    refresh_base_data_retries := non_neg_integer()
}.

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Args) when is_map(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec init(map()) -> {ok, state()}.
init(Args) ->
    process_flag(trap_exit, true),
    GuildId = maps:get(id, Args),
    CoordPid = maps:get(coordinator_pid, Args),
    Shard0Pid = maps:get(shard0_pid, Args),
    Base0 = safe_call(Shard0Pid, {get_push_base_state}, 5000),
    Base = case is_map(Base0) of true -> Base0; false -> #{} end,
    Data0 = maps:get(data, Base, #{}),
    Data = maybe_normalize_data(Data0),
    Snapshot = #{
        id => GuildId,
        data => Data,
        sessions => #{},
        member_presence => #{},
        virtual_channel_access => #{}
    },
    {ok, #{
        id => GuildId,
        coordinator_pid => CoordPid,
        shard0_pid => Shard0Pid,
        subscriptions => #{},
        session_routes => #{},
        user_session_counts => #{},
        member_presence => #{},
        virtual_channel_access => #{},
        base_data => Data,
        snapshot => Snapshot,
        compute_inflight => false,
        pending_delta => false,
        pending_full_sync_all => false,
        pending_full_sync_channels => sets:new(),
        pending_refresh_base_data => false,
        refresh_base_data_retries => 0
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({session_connected, SessionId, ShardIndex, UserId}, State) ->
    Counts0 = maps:get(user_session_counts, State, #{}),
    PrevCount = maps:get(UserId, Counts0, 0),
    State1 = put_session_route(SessionId, ShardIndex, State),
    State2 = inc_user_session_count(UserId, State1),
    State3 =
        case PrevCount of
            0 -> schedule_delta(UserId, session_connected, State2);
            _ -> State2
        end,
    {noreply, State3};
handle_cast({session_disconnected, SessionId, UserId}, State) ->
    Counts0 = maps:get(user_session_counts, State, #{}),
    PrevCount = maps:get(UserId, Counts0, 0),
    State1 = remove_session_route(SessionId, State),
    State2 = dec_user_session_count(UserId, State1),
    State3 = remove_session_subscriptions(SessionId, State2),
    Counts1 = maps:get(user_session_counts, State3, #{}),
    State4 =
        case {PrevCount, maps:is_key(UserId, Counts1)} of
            {1, false} -> schedule_delta(UserId, session_disconnected, State3);
            _ -> State3
        end,
    {noreply, State4};
handle_cast({virtual_access_added, UserId, ChannelId}, State) ->
    State1 = put_virtual_access(UserId, ChannelId, State),
    {noreply, schedule_delta(UserId, virtual_access_added, State1)};
handle_cast({virtual_access_removed, UserId, ChannelId}, State) ->
    State1 = remove_virtual_access(UserId, ChannelId, State),
    {noreply, schedule_delta(UserId, virtual_access_removed, State1)};
handle_cast({virtual_access_cleanup, UserId}, State) ->
    VA0 = maps:get(virtual_channel_access, State, #{}),
    HadVirtualAccess = maps:is_key(UserId, VA0),
    State1 = cleanup_virtual_access(UserId, State),
    State2 =
        case HadVirtualAccess of
            true -> schedule_delta(UserId, virtual_access_cleanup, State1);
            false -> State1
        end,
    {noreply, State2};
handle_cast({presence_update, UserId, PresenceMap}, State) ->
    State1 = put_member_presence(UserId, PresenceMap, State),
    {noreply, schedule_delta(UserId, presence_update, State1)};
handle_cast({subscribe, SessionId, ChannelId, Ranges}, State) ->
    {noreply, handle_subscribe(SessionId, ChannelId, Ranges, State)};
handle_cast({upsert_member, MemberData}, State) when is_map(MemberData) ->
    {noreply, handle_upsert_member(MemberData, State)};
handle_cast({remove_member, UserId}, State) ->
    {noreply, handle_remove_member(UserId, State)};
handle_cast({notify_member_update, UserId}, State) ->
    {noreply, handle_refresh_member(UserId, State)};
handle_cast({upsert_channel, ChannelData}, State) when is_map(ChannelData) ->
    {noreply, handle_upsert_channel(ChannelData, State)};
handle_cast({channels_bulk_update, Channels}, State) ->
    {noreply, handle_channels_bulk_update(Channels, State)};
handle_cast({upsert_role, RoleData}, State) when is_map(RoleData) ->
    {noreply, handle_upsert_role(RoleData, State)};
handle_cast({roles_bulk_update, Roles}, State) ->
    {noreply, handle_roles_bulk_update(Roles, State)};
handle_cast({role_deleted, RoleIdBin}, State) ->
    BaseData0 = maps:get(base_data, State, #{}),
    Roles0 = maps:get(<<"roles">>, BaseData0, []),
    Roles1 = [R || R <- Roles0, maps:get(<<"id">>, R, undefined) =/= RoleIdBin],
    BaseData1 = maps:put(<<"roles">>, Roles1, BaseData0),
    State1 = maps:put(base_data, BaseData1, State),
    State2 = schedule_refresh_base_data(State1),
    {noreply, schedule_full_sync_all(role_deleted, State2)};
handle_cast({notify_role_update}, State) ->
    State1 = schedule_refresh_base_data(State),
    {noreply, schedule_full_sync_all(role_update, State1)};
handle_cast({notify_channel_update, ChannelId}, State) ->
    State1 = schedule_refresh_base_data(State),
    {noreply, schedule_full_sync_channel(ChannelId, State1)};
handle_cast({set_shard0_pid, Shard0Pid}, State) when is_pid(Shard0Pid) ->
    State1 = maps:put(shard0_pid, Shard0Pid, State),
    State2 = schedule_refresh_base_data(State1),
    {noreply, schedule_full_sync_all(shard0_pid_changed, State2)};
handle_cast({notify_reload, NewData}, State) ->
    Data0 = maybe_normalize_data(NewData),
    State1 = maps:put(base_data, Data0, State),
    {noreply, schedule_full_sync_all(reload, State1)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({send_initial_sync, SessionId, ChannelId, ListId, Ranges}, State) ->
    _ = send_initial_sync(SessionId, ChannelId, ListId, Ranges, State),
    {noreply, State};
handle_info(compute_next, State) ->
    {noreply, run_compute_cycle(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_subscribe(session_id(), channel_id(), ranges(), state()) -> state().
handle_subscribe(SessionId, ChannelId, Ranges, State) ->
    Snapshot0 = maps:get(snapshot, State),
    NormalizedRanges = guild_member_list:normalize_ranges(Ranges),
    ListId = guild_member_list:calculate_list_id(ChannelId, Snapshot0),
    Subs0 = maps:get(subscriptions, State, #{}),
    {Subs, _OldRanges, ShouldSync} =
        guild_member_list_common:update_subscriptions(SessionId, ListId, NormalizedRanges, Subs0),
    State1 = maps:put(subscriptions, Subs, State),
    case ShouldSync of
        true ->
            self() ! {send_initial_sync, SessionId, ChannelId, ListId, NormalizedRanges},
            State1;
        false ->
            State1
    end.

-spec send_initial_sync(session_id(), channel_id(), list_id(), ranges(), state()) -> ok.
send_initial_sync(SessionId, ChannelId, ListId, Ranges, State) ->
    Snapshot = maps:get(snapshot, State, #{}),
    GuildId = maps:get(id, State),
    SyncResponse0 = guild_member_list:build_sync_response(GuildId, ListId, Ranges, Snapshot),
    SyncResponse = maps:put(<<"channel_id">>, integer_to_binary(ChannelId), SyncResponse0),
    deliver_to_session(SessionId, SyncResponse, State).

-spec handle_upsert_member(map(), state()) -> state().
handle_upsert_member(MemberData, State) ->
    Base0 = maps:get(base_data, State, #{}),
    Base = guild_data_index:put_member(MemberData, Base0),
    State1 = maps:put(base_data, Base, State),
    schedule_delta(member_user_id(MemberData), member_update, State1).

-spec handle_remove_member(term(), state()) -> state().
handle_remove_member(UserId, State) when is_integer(UserId), UserId > 0 ->
    Base0 = maps:get(base_data, State, #{}),
    Base = guild_data_index:remove_member(UserId, Base0),
    State1 = maps:put(base_data, Base, State),
    schedule_delta(UserId, member_remove, State1);
handle_remove_member(_UserId, State) ->
    State.

-spec handle_refresh_member(term(), state()) -> state().
handle_refresh_member(UserId, State) when is_integer(UserId), UserId > 0 ->
    Shard0Pid = maps:get(shard0_pid, State),
    Reply = safe_call(Shard0Pid, {get_guild_member, #{user_id => UserId}}, 5000),
    case Reply of
        #{success := true, member_data := MemberData} when is_map(MemberData) ->
            handle_upsert_member(MemberData, State);
        _ ->
            handle_remove_member(UserId, State)
    end;
handle_refresh_member(_UserId, State) ->
    State.

-spec handle_upsert_channel(map(), state()) -> state().
handle_upsert_channel(ChannelData, State) ->
    Base0 = maps:get(base_data, State, #{}),
    Channels0 = guild_data_index:channel_list(Base0),
    ChannelId = maps:get(<<"id">>, ChannelData, undefined),
    Channels = upsert_item_by_id(ChannelId, ChannelData, Channels0),
    Base = guild_data_index:put_channels(Channels, Base0),
    State1 = maps:put(base_data, Base, State),
    case type_conv:to_integer(ChannelId) of
        Cid when is_integer(Cid), Cid > 0 ->
            schedule_full_sync_channel(Cid, State1);
        _ ->
            schedule_full_sync_all(channel_update, State1)
    end.

-spec handle_channels_bulk_update(term(), state()) -> state().
handle_channels_bulk_update(Channels, State) when is_list(Channels) ->
    Base0 = maps:get(base_data, State, #{}),
    Base = bulk_update_channels(Channels, Base0),
    State1 = maps:put(base_data, Base, State),
    schedule_full_sync_all(channel_update_bulk, State1);
handle_channels_bulk_update(_, State) ->
    State.

-spec handle_upsert_role(map(), state()) -> state().
handle_upsert_role(RoleData, State) ->
    Base0 = maps:get(base_data, State, #{}),
    Roles0 = guild_data_index:role_list(Base0),
    RoleId = maps:get(<<"id">>, RoleData, undefined),
    Roles = upsert_item_by_id(RoleId, RoleData, Roles0),
    Base = guild_data_index:put_roles(Roles, Base0),
    State1 = maps:put(base_data, Base, State),
    schedule_full_sync_all(role_update, State1).

-spec handle_roles_bulk_update(term(), state()) -> state().
handle_roles_bulk_update(Roles, State) when is_list(Roles) ->
    Base0 = maps:get(base_data, State, #{}),
    Base = bulk_update_roles(Roles, Base0),
    State1 = maps:put(base_data, Base, State),
    schedule_full_sync_all(role_update_bulk, State1);
handle_roles_bulk_update(_, State) ->
    State.

-spec schedule_delta(user_id(), term(), state()) -> state().
schedule_delta(UserId, _Reason, State) ->
    State1 = maps:put(pending_delta, true, State),
    State2 =
        case is_integer(UserId) andalso UserId > 0 of
            true -> maps:put(pending_delta_user_id, UserId, State1);
            false -> State1
        end,
    ensure_compute_scheduled(State2).

-spec schedule_full_sync_all(term(), state()) -> state().
schedule_full_sync_all(_Reason, State) ->
    ensure_compute_scheduled(maps:put(pending_full_sync_all, true, State)).

-spec schedule_full_sync_channel(channel_id(), state()) -> state().
schedule_full_sync_channel(ChannelId, State) when is_integer(ChannelId), ChannelId > 0 ->
    Channels0 = maps:get(pending_full_sync_channels, State, sets:new()),
    Channels = sets:add_element(ChannelId, Channels0),
    ensure_compute_scheduled(maps:put(pending_full_sync_channels, Channels, State));
schedule_full_sync_channel(_, State) ->
    State.

-spec schedule_refresh_base_data(state()) -> state().
schedule_refresh_base_data(State) ->
    ensure_compute_scheduled(maps:put(pending_refresh_base_data, true, State)).

-spec ensure_compute_scheduled(state()) -> state().
ensure_compute_scheduled(State) ->
    case maps:get(compute_inflight, State, false) of
        true ->
            State;
        false ->
            self() ! compute_next,
            maps:put(compute_inflight, true, State)
    end.

-spec has_pending_compute_work(state()) -> boolean().
has_pending_compute_work(State) ->
    maps:get(pending_refresh_base_data, State, false) orelse
        maps:get(pending_full_sync_all, State, false) orelse
        maps:get(pending_delta, State, false) orelse
        (not sets:is_empty(maps:get(pending_full_sync_channels, State, sets:new()))).

-spec run_compute_cycle(state()) -> state().
run_compute_cycle(State0) ->
    case has_pending_compute_work(State0) of
        false ->
            maps:put(compute_inflight, false, State0);
        true ->
            State1 = maybe_refresh_base_data_from_shard0(State0),
            OldSnapshot = maps:get(snapshot, State1, #{}),
            NewSnapshot = build_new_snapshot(State1),
            Subs = maps:get(subscriptions, State1, #{}),
            Deliveries =
                case map_size(Subs) of
                    0 -> #{};
                    _ -> compute_deliveries(OldSnapshot, NewSnapshot, Subs, State1)
                end,
            CoordPid = maps:get(coordinator_pid, State1),
            case {map_size(Deliveries), is_pid(CoordPid)} of
                {N, true} when N > 0 ->
                    deliver_grouped(Deliveries, CoordPid);
                _ ->
                    ok
            end,
            State2 = maps:put(snapshot, NewSnapshot, State1),
            State3 = clear_pending_after_compute(State2),
            case has_pending_compute_work(State3) of
                true ->
                    self() ! compute_next,
                    State3;
                false ->
                    maps:put(compute_inflight, false, State3)
            end
    end.

-spec maybe_refresh_base_data_from_shard0(state()) -> state().
maybe_refresh_base_data_from_shard0(#{pending_refresh_base_data := true, shard0_pid := Shard0Pid} = State) ->
    Retries = maps:get(refresh_base_data_retries, State, 0),
    case Retries >= 3 of
        true ->
            State#{pending_refresh_base_data => false, refresh_base_data_retries => 0};
        false ->
            case safe_call(Shard0Pid, {get_push_base_state}, 10000) of
                #{data := Data0} when is_map(Data0) ->
                    Data = maybe_normalize_data(Data0),
                    State#{base_data => Data, pending_refresh_base_data => false, refresh_base_data_retries => 0};
                _ ->
                    State#{refresh_base_data_retries => Retries + 1}
            end
    end;
maybe_refresh_base_data_from_shard0(State) ->
    State.

-spec build_new_snapshot(state()) -> map().
build_new_snapshot(State) ->
    GuildId = maps:get(id, State),
    BaseData = maps:get(base_data, State, #{}),
    apply_dynamic_fields(
        #{
            id => GuildId,
            data => BaseData,
            sessions => #{},
            member_presence => #{},
            virtual_channel_access => #{}
        },
        State
    ).

-spec clear_pending_after_compute(state()) -> state().
clear_pending_after_compute(State) ->
    State1 = maps:put(pending_delta, false, State),
    State2 = maps:remove(pending_delta_user_id, State1),
    State3 = maps:put(pending_full_sync_all, false, State2),
    State4 = maps:put(pending_full_sync_channels, sets:new(), State3),
    State4.

-spec compute_deliveries(map(), map(), subscriptions(), state()) -> map().
compute_deliveries(OldSnapshot, NewSnapshot, Subs, State) ->
    PendingAll = maps:get(pending_full_sync_all, State, false),
    PendingChannels = maps:get(pending_full_sync_channels, State, sets:new()),
    case PendingAll of
        true ->
            compute_full_sync_deliveries(NewSnapshot, Subs, State);
        false ->
            case sets:is_empty(PendingChannels) of
                false ->
                    compute_channel_sync_plus_delta(OldSnapshot, NewSnapshot, Subs, PendingChannels, State);
                true ->
                    case maps:get(pending_delta, State, false) of
                        true ->
                            UserId = pick_delta_user_id_hint(State),
                            compute_delta_deliveries(UserId, OldSnapshot, NewSnapshot, Subs, State);
                        false ->
                            #{}
                    end
            end
    end.

-spec compute_channel_sync_plus_delta(map(), map(), subscriptions(), sets:set(), state()) -> map().
compute_channel_sync_plus_delta(OldSnapshot, NewSnapshot, Subs, ChannelIdSet, State) ->
    ChannelIds = sets:to_list(ChannelIdSet),
    ListIds0 = [guild_member_list:calculate_list_id(Cid, NewSnapshot) || Cid <- ChannelIds],
    ListIds = lists:usort([L || L <- ListIds0, is_binary(L)]),
    DeliveriesSync = lists:foldl(
        fun(ListId, Acc) ->
            case maps:get(ListId, Subs, undefined) of
                undefined ->
                    Acc;
                ListSubs ->
                    Acc1 = compute_full_sync_deliveries_for_list(NewSnapshot, ListId, ListSubs, State),
                    merge_delivery_maps(Acc, Acc1)
            end
        end,
        #{},
        ListIds
    ),
    SubsDelta = remove_list_ids_from_subs(Subs, ListIds),
    DeliveriesDelta =
        case map_size(SubsDelta) of
            0 ->
                #{};
            _ ->
                UserId = pick_delta_user_id_hint(State),
                compute_delta_deliveries(UserId, OldSnapshot, NewSnapshot, SubsDelta, State)
        end,
    merge_delivery_maps(DeliveriesSync, DeliveriesDelta).

-spec remove_list_ids_from_subs(subscriptions(), [list_id()]) -> subscriptions().
remove_list_ids_from_subs(Subs, ListIds) ->
    lists:foldl(fun(ListId, Acc) -> maps:remove(ListId, Acc) end, Subs, ListIds).

-spec pick_delta_user_id_hint(state()) -> user_id().
pick_delta_user_id_hint(State) ->
    case maps:get(pending_delta_user_id, State, undefined) of
        UserId when is_integer(UserId), UserId > 0 ->
            UserId;
        _ ->
            PresenceKeys = maps:keys(maps:get(member_presence, State, #{})),
            case PresenceKeys of
                [UserId | _] when is_integer(UserId), UserId > 0 ->
                    UserId;
                _ ->
                    CountKeys = maps:keys(maps:get(user_session_counts, State, #{})),
                    case CountKeys of
                        [UserId | _] when is_integer(UserId), UserId > 0 -> UserId;
                        _ -> 0
                    end
            end
    end.

-spec apply_dynamic_fields(map(), state()) -> map().
apply_dynamic_fields(NewSnapshot0, State) ->
    Presence = maps:get(member_presence, State, #{}),
    VirtualAccess = maps:get(virtual_channel_access, State, #{}),
    Sessions = build_minimal_sessions_map(State),
    NewSnapshot0#{member_presence => Presence, virtual_channel_access => VirtualAccess, sessions => Sessions}.

-spec build_minimal_sessions_map(state()) -> map().
build_minimal_sessions_map(State) ->
    Counts = maps:get(user_session_counts, State, #{}),
    maps:fold(
        fun(UserId, _N, Acc) ->
            maps:put(integer_to_binary(UserId), #{user_id => UserId}, Acc)
        end,
        #{},
        Counts
    ).

-spec maybe_normalize_data(term()) -> map().
maybe_normalize_data(Data0) when is_map(Data0) ->
    case maps:get(<<"members">>, Data0, undefined) of
        Members when is_map(Members) ->
            Data0;
        _ ->
            guild_data_index:normalize_data(Data0)
    end;
maybe_normalize_data(_Data0) ->
    #{}.

-spec member_user_id(map()) -> user_id().
member_user_id(MemberData) ->
    guild_member_list_common:get_member_user_id(MemberData).

-spec upsert_item_by_id(term(), map(), [map()]) -> [map()].
upsert_item_by_id(Id, NewItem, Items) ->
    upsert_item_by_id(Id, NewItem, Items, [], false).

upsert_item_by_id(_Id, NewItem, [], Acc, false) ->
    lists:reverse([NewItem | Acc]);
upsert_item_by_id(_Id, _NewItem, [], Acc, true) ->
    lists:reverse(Acc);
upsert_item_by_id(Id, NewItem, [H | T], Acc, Found) ->
    case maps:get(<<"id">>, H, undefined) of
        Id -> upsert_item_by_id(Id, NewItem, T, [NewItem | Acc], true);
        _ -> upsert_item_by_id(Id, NewItem, T, [H | Acc], Found)
    end.

-spec bulk_update_channels([map()], map()) -> map().
bulk_update_channels(BulkChannels, Data) ->
    Channels0 = guild_data_index:channel_list(Data),
    Channels = bulk_update_items_by_id(Channels0, BulkChannels),
    guild_data_index:put_channels(Channels, Data).

-spec bulk_update_roles([map()], map()) -> map().
bulk_update_roles(BulkRoles, Data) ->
    Roles0 = guild_data_index:role_list(Data),
    Roles = bulk_update_items_by_id(Roles0, BulkRoles),
    guild_data_index:put_roles(Roles, Data).

-spec bulk_update_items_by_id([map()], [map()]) -> [map()].
bulk_update_items_by_id(Items, BulkItems) ->
    BulkById = lists:foldl(
        fun(BulkItem, Acc) ->
            case maps:get(<<"id">>, BulkItem, undefined) of
                Id when is_binary(Id) ->
                    maps:put(Id, BulkItem, Acc);
                _ ->
                    Acc
            end
        end,
        #{},
        BulkItems
    ),
    {UpdatedExisting, SeenIds} = lists:mapfoldl(
        fun(Item, Seen) ->
            case maps:get(<<"id">>, Item, undefined) of
                Id when is_binary(Id) ->
                    case maps:get(Id, BulkById, undefined) of
                        BulkItem when is_map(BulkItem) ->
                            {BulkItem, sets:add_element(Id, Seen)};
                        _ ->
                            {Item, Seen}
                    end;
                _ ->
                    {Item, Seen}
            end
        end,
        sets:new(),
        Items
    ),
    NewItems = lists:filter(
        fun(BulkItem) ->
            case maps:get(<<"id">>, BulkItem, undefined) of
                Id when is_binary(Id) ->
                    not sets:is_element(Id, SeenIds);
                _ ->
                    false
            end
        end,
        BulkItems
    ),
    UpdatedExisting ++ NewItems.

-spec compute_delta_deliveries(
    user_id(), map(), map(), subscriptions(), state()
) -> #{shard_index() => [{session_id(), map()}]}.
compute_delta_deliveries(UserId, OldSnapshot, NewSnapshot, Subs, State) ->
    GuildId = maps:get(id, State),
    maps:fold(
        fun(ListId, ListSubs, Acc) ->
            {MemberCount, OnlineCount, Groups, Ops, Changed} =
                guild_member_list:member_list_delta(ListId, OldSnapshot, NewSnapshot, UserId),
            case Changed of
                false ->
                    Acc;
                true ->
                    Payload = #{
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"id">> => ListId,
                        <<"member_count">> => MemberCount,
                        <<"online_count">> => OnlineCount,
                        <<"groups">> => Groups,
                        <<"ops">> => Ops
                    },
                    SessionIds = maps:keys(ListSubs),
                    add_deliveries_for_sessions(SessionIds, Payload, Acc, State)
            end
        end,
        #{},
        Subs
    ).

-spec compute_full_sync_deliveries(map(), subscriptions(), state()) ->
    #{shard_index() => [{session_id(), map()}]}.
compute_full_sync_deliveries(Snapshot, Subs, State) ->
    maps:fold(
        fun(ListId, ListSubs, Acc) ->
            Acc1 = compute_full_sync_deliveries_for_list(Snapshot, ListId, ListSubs, State),
            merge_delivery_maps(Acc, Acc1)
        end,
        #{},
        Subs
    ).

-spec compute_full_sync_deliveries_for_list(map(), list_id(), map(), state()) ->
    #{shard_index() => [{session_id(), map()}]}.
compute_full_sync_deliveries_for_list(Snapshot, ListId, ListSubs, State) ->
    GuildId = maps:get(id, State),
    maps:fold(
        fun(SessionId, Ranges, Acc) ->
            SyncResponse = guild_member_list:build_sync_response(GuildId, ListId, Ranges, Snapshot),
            Acc1 = add_delivery_for_session(SessionId, SyncResponse, Acc, State),
            Acc1
        end,
        #{},
        ListSubs
    ).

-spec deliver_to_session(session_id(), map(), state()) -> ok.
deliver_to_session(SessionId, Payload, State) ->
    CoordPid = maps:get(coordinator_pid, State),
    Deliveries = add_delivery_for_session(SessionId, Payload, #{}, State),
    deliver_grouped(Deliveries, CoordPid).

-spec deliver_grouped(#{shard_index() => [{session_id(), map()}]}, pid()) -> ok.
deliver_grouped(DeliveriesByShard, CoordPid) ->
    gen_server:cast(CoordPid, {very_large_guild_member_list_deliver, DeliveriesByShard}),
    ok.

-spec add_deliveries_for_sessions([session_id()], map(), map(), state()) -> map().
add_deliveries_for_sessions(SessionIds, Payload, Acc0, State) ->
    lists:foldl(fun(Sid, Acc) -> add_delivery_for_session(Sid, Payload, Acc, State) end, Acc0, SessionIds).

-spec add_delivery_for_session(session_id(), map(), map(), state()) -> map().
add_delivery_for_session(SessionId, Payload, Acc0, State) ->
    case maps:get(SessionId, maps:get(session_routes, State, #{}), undefined) of
        ShardIndex when is_integer(ShardIndex) ->
            Existing = maps:get(ShardIndex, Acc0, []),
            maps:put(ShardIndex, [{SessionId, Payload} | Existing], Acc0);
        _ ->
            Acc0
    end.

-spec merge_delivery_maps(map(), map()) -> map().
merge_delivery_maps(A, B) ->
    maps:fold(
        fun(ShardIndex, DeliveriesB, Acc) ->
            DeliveriesA = maps:get(ShardIndex, Acc, []),
            maps:put(ShardIndex, DeliveriesA ++ DeliveriesB, Acc)
        end,
        A,
        B
    ).

-spec put_session_route(session_id(), shard_index(), state()) -> state().
put_session_route(SessionId, ShardIndex, State) ->
    Routes0 = maps:get(session_routes, State, #{}),
    Routes = maps:put(SessionId, ShardIndex, Routes0),
    maps:put(session_routes, Routes, State).

-spec remove_session_route(session_id(), state()) -> state().
remove_session_route(SessionId, State) ->
    Routes0 = maps:get(session_routes, State, #{}),
    Routes = maps:remove(SessionId, Routes0),
    maps:put(session_routes, Routes, State).

-spec inc_user_session_count(user_id(), state()) -> state().
inc_user_session_count(UserId, State) ->
    Counts0 = maps:get(user_session_counts, State, #{}),
    Prev = maps:get(UserId, Counts0, 0),
    Counts = maps:put(UserId, Prev + 1, Counts0),
    maps:put(user_session_counts, Counts, State).

-spec dec_user_session_count(user_id(), state()) -> state().
dec_user_session_count(UserId, State) ->
    Counts0 = maps:get(user_session_counts, State, #{}),
    case maps:get(UserId, Counts0, 0) of
        N when is_integer(N), N > 1 ->
            Counts = maps:put(UserId, N - 1, Counts0),
            maps:put(user_session_counts, Counts, State);
        1 ->
            Counts = maps:remove(UserId, Counts0),
            maps:put(user_session_counts, Counts, State);
        _ ->
            State
    end.

-spec put_member_presence(user_id(), map(), state()) -> state().
put_member_presence(UserId, PresenceMap, State) ->
    Presence0 = maps:get(member_presence, State, #{}),
    Presence = maps:put(UserId, PresenceMap, Presence0),
    maps:put(member_presence, Presence, State).

-spec put_virtual_access(user_id(), channel_id(), state()) -> state().
put_virtual_access(UserId, ChannelId, State) ->
    VA0 = maps:get(virtual_channel_access, State, #{}),
    Channels0 = maps:get(UserId, VA0, sets:new()),
    Channels = sets:add_element(ChannelId, Channels0),
    maps:put(virtual_channel_access, maps:put(UserId, Channels, VA0), State).

-spec remove_virtual_access(user_id(), channel_id(), state()) -> state().
remove_virtual_access(UserId, ChannelId, State) ->
    VA0 = maps:get(virtual_channel_access, State, #{}),
    case maps:get(UserId, VA0, undefined) of
        undefined ->
            State;
        Channels0 ->
            Channels = sets:del_element(ChannelId, Channels0),
            VA =
                case sets:size(Channels) of
                    0 -> maps:remove(UserId, VA0);
                    _ -> maps:put(UserId, Channels, VA0)
                end,
            maps:put(virtual_channel_access, VA, State)
    end.

-spec cleanup_virtual_access(user_id(), state()) -> state().
cleanup_virtual_access(UserId, State) ->
    VA0 = maps:get(virtual_channel_access, State, #{}),
    maps:put(virtual_channel_access, maps:remove(UserId, VA0), State).

-spec remove_session_subscriptions(session_id(), state()) -> state().
remove_session_subscriptions(SessionId, State) ->
    Subs0 = maps:get(subscriptions, State, #{}),
    Subs = guild_member_list_common:remove_session_from_subscriptions(SessionId, Subs0),
    maps:put(subscriptions, Subs, State).

-spec safe_call(pid(), term(), timeout()) -> term().
safe_call(Pid, Msg, Timeout) ->
    try gen_server:call(Pid, Msg, Timeout) of
        Reply -> Reply
    catch
        _:_ -> {error, failed}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

put_remove_virtual_access_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {virtual_access_added, 10, 50}),
    gen_server:cast(Pid, {virtual_access_removed, 10, 50}),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

start_stub_shard0() ->
    spawn_link(fun() -> stub_shard0_loop() end).

stub_shard0_loop() ->
    receive
        {'$gen_call', From, {get_push_base_state}} ->
            gen_server:reply(From, #{data => #{<<"channels">> => [#{<<"id">> => <<"10">>}], <<"members">> => []}}),
            stub_shard0_loop();
        _ ->
            stub_shard0_loop()
    end.

start_counting_stub_shard0(Parent, Ref) ->
    spawn_link(fun() -> counting_stub_shard0_loop(Parent, Ref) end).

counting_stub_shard0_loop(Parent, Ref) ->
    receive
        {'$gen_call', From, {get_push_base_state}} ->
            Parent ! {Ref, shard0_call, get_push_base_state},
            gen_server:reply(From, #{data => #{<<"channels">> => [#{<<"id">> => <<"10">>}], <<"members">> => []}}),
            counting_stub_shard0_loop(Parent, Ref);
        {'$gen_call', From, {get_guild_member, #{user_id := UserId}}} ->
            Parent ! {Ref, shard0_call, get_guild_member, UserId},
            MemberData = #{
                <<"user">> => #{<<"id">> => integer_to_binary(UserId), <<"username">> => <<"user">>},
                <<"roles">> => []
            },
            gen_server:reply(From, #{success => true, member_data => MemberData}),
            counting_stub_shard0_loop(Parent, Ref);
        _ ->
            counting_stub_shard0_loop(Parent, Ref)
    end.

await_shard0_call(Ref, Expected) ->
    receive
        {Ref, shard0_call, Expected} -> ok;
        {Ref, shard0_call, get_guild_member, UserId} when Expected =:= {get_guild_member, UserId} -> ok
    after 500 ->
        timeout
    end.

assert_no_shard0_calls(Ref) ->
    receive
        {Ref, shard0_call, _} -> unexpected_shard0_call;
        {Ref, shard0_call, _, _} -> unexpected_shard0_call
    after 150 ->
        ok
    end.

session_disconnected_removes_subscriptions_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    SessionId = <<"sid-1">>,
    gen_server:cast(Pid, {session_connected, SessionId, 0, 10}),
    gen_server:cast(Pid, {subscribe, SessionId, 10, [{0, 50}]}),
    ok = await(fun() ->
        State0 = sys:get_state(Pid),
        Subs0 = maps:get(subscriptions, State0, #{}),
        ListSubs0 = maps:get(<<"10">>, Subs0, #{}),
        maps:is_key(SessionId, ListSubs0)
    end),
    gen_server:cast(Pid, {session_disconnected, SessionId, 10}),
    ok = await(fun() ->
        State1 = sys:get_state(Pid),
        Subs1 = maps:get(subscriptions, State1, #{}),
        ListSubs1 = maps:get(<<"10">>, Subs1, #{}),
        (not maps:is_key(SessionId, ListSubs1)) andalso
            (not maps:is_key(SessionId, maps:get(session_routes, State1, #{})))
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

notify_member_update_fetches_member_from_shard0_test() ->
    Ref = make_ref(),
    Shard0Pid = start_counting_stub_shard0(self(), Ref),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    gen_server:cast(Pid, {notify_member_update, 10}),
    ?assertEqual(ok, await_shard0_call(Ref, {get_guild_member, 10})),
    ?assertEqual(ok, assert_no_shard0_calls(Ref)),
    State = sys:get_state(Pid),
    Base = maps:get(base_data, State, #{}),
    Members = guild_data_index:member_map(Base),
    ?assertEqual(true, maps:is_key(10, Members)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

presence_update_does_not_fetch_shard0_test() ->
    Ref = make_ref(),
    Shard0Pid = start_counting_stub_shard0(self(), Ref),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"online">>}}),
    ?assertEqual(ok, assert_no_shard0_calls(Ref)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

subscribe_does_not_fetch_shard0_test() ->
    Ref = make_ref(),
    Shard0Pid = start_counting_stub_shard0(self(), Ref),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    SessionId = <<"sid-2">>,
    gen_server:cast(Pid, {session_connected, SessionId, 0, 10}),
    gen_server:cast(Pid, {subscribe, SessionId, 10, [{0, 50}]}),
    ?assertEqual(ok, assert_no_shard0_calls(Ref)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

start_counting_failing_stub_shard0(Parent, Ref) ->
    spawn_link(fun() -> counting_failing_stub_shard0_loop(Parent, Ref) end).

counting_failing_stub_shard0_loop(Parent, Ref) ->
    receive
        {'$gen_call', From, {get_push_base_state}} ->
            Parent ! {Ref, shard0_call, get_push_base_state},
            gen_server:reply(From, {error, failed}),
            counting_failing_stub_shard0_loop(Parent, Ref);
        _ ->
            counting_failing_stub_shard0_loop(Parent, Ref)
    end.

start_stub_shard0_with_roles(Roles) ->
    spawn_link(fun() -> stub_shard0_with_roles_loop(Roles) end).

stub_shard0_with_roles_loop(Roles) ->
    receive
        {'$gen_call', From, {get_push_base_state}} ->
            gen_server:reply(From, #{data => #{
                <<"channels">> => [#{<<"id">> => <<"10">>}],
                <<"members">> => [],
                <<"roles">> => Roles
            }}),
            stub_shard0_with_roles_loop(Roles);
        _ ->
            stub_shard0_with_roles_loop(Roles)
    end.

upsert_member_updates_base_data_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    MemberData = #{
        <<"user">> => #{<<"id">> => <<"42">>, <<"username">> => <<"alice">>},
        <<"roles">> => []
    },
    gen_server:cast(Pid, {upsert_member, MemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Members = guild_data_index:member_map(Base),
        maps:is_key(42, Members)
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

remove_member_updates_base_data_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    MemberData = #{
        <<"user">> => #{<<"id">> => <<"42">>, <<"username">> => <<"alice">>},
        <<"roles">> => []
    },
    gen_server:cast(Pid, {upsert_member, MemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Members = guild_data_index:member_map(Base),
        maps:is_key(42, Members)
    end),
    gen_server:cast(Pid, {remove_member, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Members = guild_data_index:member_map(Base),
        not maps:is_key(42, Members)
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

role_deleted_removes_from_base_data_test() ->
    Roles = [
        #{<<"id">> => <<"r1">>, <<"name">> => <<"Admin">>},
        #{<<"id">> => <<"r2">>, <<"name">> => <<"Member">>}
    ],
    Shard0Pid = start_stub_shard0_with_roles(Roles),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        length(maps:get(<<"roles">>, Base, [])) =:= 2
    end),
    gen_server:cast(Pid, {role_deleted, <<"r1">>}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        CurrentRoles = maps:get(<<"roles">>, Base, []),
        length(CurrentRoles) =:= 1 andalso
            maps:get(<<"id">>, hd(CurrentRoles), undefined) =:= <<"r2">>
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

refresh_base_data_retries_on_failure_test() ->
    Ref = make_ref(),
    Shard0Pid = start_counting_failing_stub_shard0(self(), Ref),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    gen_server:cast(Pid, {notify_role_update}),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    ?assertEqual(ok, assert_no_shard0_calls(Ref)),
    StateFinal = sys:get_state(Pid),
    ?assertEqual(false, maps:get(pending_refresh_base_data, StateFinal, true)),
    ?assertEqual(0, maps:get(refresh_base_data_retries, StateFinal, -1)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

upsert_channel_single_pass_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    UpdatedChannel = #{<<"id">> => <<"10">>, <<"name">> => <<"updated">>},
    gen_server:cast(Pid, {upsert_channel, UpdatedChannel}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Channels = guild_data_index:channel_list(Base),
        lists:any(fun(C) -> maps:get(<<"name">>, C, undefined) =:= <<"updated">> end, Channels)
    end),
    State1 = sys:get_state(Pid),
    Base1 = maps:get(base_data, State1, #{}),
    Channels1 = guild_data_index:channel_list(Base1),
    ChannelCount1 = length(Channels1),
    NewChannel = #{<<"id">> => <<"20">>, <<"name">> => <<"new-channel">>},
    gen_server:cast(Pid, {upsert_channel, NewChannel}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Channels = guild_data_index:channel_list(Base),
        lists:any(fun(C) -> maps:get(<<"id">>, C, undefined) =:= <<"20">> end, Channels)
    end),
    State2 = sys:get_state(Pid),
    Base2 = maps:get(base_data, State2, #{}),
    Channels2 = guild_data_index:channel_list(Base2),
    ?assertEqual(ChannelCount1 + 1, length(Channels2)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

bulk_update_roles_test() ->
    Roles = [
        #{<<"id">> => <<"r1">>, <<"name">> => <<"Admin">>},
        #{<<"id">> => <<"r2">>, <<"name">> => <<"Member">>}
    ],
    Shard0Pid = start_stub_shard0_with_roles(Roles),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        length(maps:get(<<"roles">>, Base, [])) =:= 2
    end),
    NewRoles = [
        #{<<"id">> => <<"r1">>, <<"name">> => <<"SuperAdmin">>},
        #{<<"id">> => <<"r3">>, <<"name">> => <<"Moderator">>}
    ],
    gen_server:cast(Pid, {roles_bulk_update, NewRoles}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        CurrentRoles = maps:get(<<"roles">>, Base, []),
        length(CurrentRoles) =:= 3
    end),
    State1 = sys:get_state(Pid),
    Base1 = maps:get(base_data, State1, #{}),
    FinalRoles = maps:get(<<"roles">>, Base1, []),
    R1 = lists:keyfind(<<"r1">>, 1, [{maps:get(<<"id">>, R), R} || R <- FinalRoles]),
    ?assertMatch({<<"r1">>, #{<<"name">> := <<"SuperAdmin">>}}, R1),
    ?assert(lists:any(fun(R) -> maps:get(<<"id">>, R, undefined) =:= <<"r3">> end, FinalRoles)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

notify_reload_replaces_base_data_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    NewData = #{
        <<"channels">> => [#{<<"id">> => <<"99">>}],
        <<"members">> => [#{
            <<"user">> => #{<<"id">> => <<"77">>, <<"username">> => <<"bob">>},
            <<"roles">> => []
        }],
        <<"roles">> => [#{<<"id">> => <<"r5">>, <<"name">> => <<"Owner">>}]
    },
    gen_server:cast(Pid, {notify_reload, NewData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Channels = guild_data_index:channel_list(Base),
        length(Channels) =:= 1 andalso
            maps:get(<<"id">>, hd(Channels), undefined) =:= <<"99">>
    end),
    State1 = sys:get_state(Pid),
    Base1 = maps:get(base_data, State1, #{}),
    Members1 = guild_data_index:member_map(Base1),
    ?assertEqual(true, maps:is_key(77, Members1)),
    FinalRoles = maps:get(<<"roles">>, Base1, []),
    ?assertEqual(1, length(FinalRoles)),
    ?assertEqual(<<"r5">>, maps:get(<<"id">>, hd(FinalRoles), undefined)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

await(Predicate) ->
    await(Predicate, 50, 20).

await(Predicate, 0, _SleepMs) ->
    case Predicate() of
        true -> ok;
        false -> timeout
    end;
await(Predicate, AttemptsLeft, SleepMs) ->
    case Predicate() of
        true ->
            ok;
        false ->
            timer:sleep(SleepMs),
            await(Predicate, AttemptsLeft - 1, SleepMs)
    end.

unsubscribe_with_empty_ranges_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    SessionId = <<"sid-unsub">>,
    gen_server:cast(Pid, {session_connected, SessionId, 0, 10}),
    gen_server:cast(Pid, {subscribe, SessionId, 10, [{0, 50}]}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Subs = maps:get(subscriptions, State, #{}),
        map_size(Subs) > 0
    end),
    gen_server:cast(Pid, {subscribe, SessionId, 10, []}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Subs = maps:get(subscriptions, State, #{}),
        map_size(Subs) =:= 0
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

subscribe_same_ranges_no_resync_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    SessionId = <<"sid-same-ranges">>,
    gen_server:cast(Pid, {session_connected, SessionId, 0, 10}),
    gen_server:cast(Pid, {subscribe, SessionId, 10, [{0, 50}]}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Subs = maps:get(subscriptions, State, #{}),
        map_size(Subs) > 0
    end),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {subscribe, SessionId, 10, [{0, 50}]}),
    timer:sleep(100),
    State = sys:get_state(Pid),
    ?assertEqual(false, maps:get(compute_inflight, State, true)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

upsert_role_updates_base_data_test() ->
    Roles = [#{<<"id">> => <<"r1">>, <<"name">> => <<"Admin">>}],
    Shard0Pid = start_stub_shard0_with_roles(Roles),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        length(maps:get(<<"roles">>, Base, [])) =:= 1
    end),
    NewRole = #{<<"id">> => <<"r2">>, <<"name">> => <<"Moderator">>, <<"permissions">> => <<"0">>},
    gen_server:cast(Pid, {upsert_role, NewRole}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        length(maps:get(<<"roles">>, Base, [])) =:= 2
    end),
    State = sys:get_state(Pid),
    Base = maps:get(base_data, State, #{}),
    FinalRoles = maps:get(<<"roles">>, Base, []),
    ?assert(lists:any(fun(R) -> maps:get(<<"id">>, R, undefined) =:= <<"r2">> end, FinalRoles)),
    UpdatedRole = #{<<"id">> => <<"r1">>, <<"name">> => <<"SuperAdmin">>, <<"permissions">> => <<"8">>},
    gen_server:cast(Pid, {upsert_role, UpdatedRole}),
    ok = await(fun() ->
        State1 = sys:get_state(Pid),
        Base1 = maps:get(base_data, State1, #{}),
        RolesNow = maps:get(<<"roles">>, Base1, []),
        lists:any(fun(R) -> maps:get(<<"name">>, R, undefined) =:= <<"SuperAdmin">> end, RolesNow)
    end),
    State2 = sys:get_state(Pid),
    Base2 = maps:get(base_data, State2, #{}),
    ?assertEqual(2, length(maps:get(<<"roles">>, Base2, []))),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

channels_bulk_update_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    BulkChannels = [
        #{<<"id">> => <<"10">>, <<"name">> => <<"renamed">>},
        #{<<"id">> => <<"30">>, <<"name">> => <<"brand-new">>}
    ],
    gen_server:cast(Pid, {channels_bulk_update, BulkChannels}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Channels = guild_data_index:channel_list(Base),
        lists:any(fun(C) -> maps:get(<<"id">>, C, undefined) =:= <<"30">> end, Channels)
    end),
    State = sys:get_state(Pid),
    Base = maps:get(base_data, State, #{}),
    Channels = guild_data_index:channel_list(Base),
    ?assertEqual(2, length(Channels)),
    ?assert(lists:any(fun(C) -> maps:get(<<"name">>, C, undefined) =:= <<"renamed">> end, Channels)),
    ?assert(lists:any(fun(C) -> maps:get(<<"name">>, C, undefined) =:= <<"brand-new">> end, Channels)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

virtual_access_cleanup_noop_when_no_access_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {virtual_access_cleanup, 999}),
    timer:sleep(100),
    State = sys:get_state(Pid),
    ?assertEqual(false, maps:get(pending_delta, State, true)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

remove_member_invalid_id_noop_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {remove_member, 0}),
    gen_server:cast(Pid, {remove_member, -1}),
    timer:sleep(100),
    State = sys:get_state(Pid),
    ?assertEqual(false, maps:get(pending_delta, State, true)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

multiple_session_connects_and_disconnects_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 10}),
    gen_server:cast(Pid, {session_connected, <<"s2">>, 0, 10}),
    gen_server:cast(Pid, {session_connected, <<"s3">>, 1, 20}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Counts = maps:get(user_session_counts, State, #{}),
        maps:get(10, Counts, 0) =:= 2 andalso maps:get(20, Counts, 0) =:= 1
    end),
    gen_server:cast(Pid, {session_disconnected, <<"s1">>, 10}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Counts = maps:get(user_session_counts, State, #{}),
        maps:get(10, Counts, 0) =:= 1
    end),
    gen_server:cast(Pid, {session_disconnected, <<"s2">>, 10}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Counts = maps:get(user_session_counts, State, #{}),
        not maps:is_key(10, Counts)
    end),
    State = sys:get_state(Pid),
    Routes = maps:get(session_routes, State, #{}),
    ?assertNot(maps:is_key(<<"s1">>, Routes)),
    ?assertNot(maps:is_key(<<"s2">>, Routes)),
    ?assert(maps:is_key(<<"s3">>, Routes)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

set_shard0_pid_triggers_refresh_test() ->
    Ref = make_ref(),
    Shard0Pid = start_counting_stub_shard0(self(), Ref),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    NewShard0Pid = start_counting_stub_shard0(self(), Ref),
    gen_server:cast(Pid, {set_shard0_pid, NewShard0Pid}),
    ?assertEqual(ok, await_shard0_call(Ref, get_push_base_state)),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(shard0_pid, State) =:= NewShard0Pid
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    unlink(NewShard0Pid),
    exit(NewShard0Pid, shutdown),
    ok.

presence_update_stores_in_state_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {presence_update, 42, #{<<"status">> => <<"online">>}}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Presence = maps:get(member_presence, State, #{}),
        maps:is_key(42, Presence)
    end),
    State = sys:get_state(Pid),
    Presence = maps:get(member_presence, State, #{}),
    ?assertEqual(#{<<"status">> => <<"online">>}, maps:get(42, Presence)),
    gen_server:cast(Pid, {presence_update, 42, #{<<"status">> => <<"idle">>}}),
    ok = await(fun() ->
        State1 = sys:get_state(Pid),
        Presence1 = maps:get(member_presence, State1, #{}),
        maps:get(42, Presence1, #{}) =:= #{<<"status">> => <<"idle">>}
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

notify_channel_update_triggers_channel_sync_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {notify_channel_update, 10}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(pending_refresh_base_data, State, false) =:= true orelse
            maps:get(compute_inflight, State, false) =:= true
    end),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

session_connected_first_session_triggers_delta_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Counts = maps:get(user_session_counts, State, #{}),
        maps:get(42, Counts, 0) =:= 1
    end),
    State = sys:get_state(Pid),
    Routes = maps:get(session_routes, State, #{}),
    ?assertEqual(0, maps:get(<<"s1">>, Routes)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

session_connected_second_session_no_delta_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {session_connected, <<"s2">>, 1, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(user_session_counts, State, #{}) =:= #{42 => 2}
    end),
    State = sys:get_state(Pid),
    Routes = maps:get(session_routes, State, #{}),
    ?assertEqual(0, maps:get(<<"s1">>, Routes)),
    ?assertEqual(1, maps:get(<<"s2">>, Routes)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

session_disconnected_last_session_triggers_delta_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Counts = maps:get(user_session_counts, State, #{}),
        maps:get(42, Counts, 0) =:= 1
    end),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {session_disconnected, <<"s1">>, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Counts = maps:get(user_session_counts, State, #{}),
        not maps:is_key(42, Counts)
    end),
    State = sys:get_state(Pid),
    Routes = maps:get(session_routes, State, #{}),
    ?assertNot(maps:is_key(<<"s1">>, Routes)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

session_disconnected_not_last_no_delta_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 42}),
    gen_server:cast(Pid, {session_connected, <<"s2">>, 1, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(user_session_counts, State, #{}) =:= #{42 => 2}
    end),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {session_disconnected, <<"s1">>, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(user_session_counts, State, #{}) =:= #{42 => 1}
    end),
    State = sys:get_state(Pid),
    ?assertNot(maps:is_key(<<"s1">>, maps:get(session_routes, State, #{}))),
    ?assert(maps:is_key(<<"s2">>, maps:get(session_routes, State, #{}))),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

virtual_access_added_and_removed_restores_state_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {virtual_access_added, 10, 50}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        VA = maps:get(virtual_channel_access, State, #{}),
        maps:is_key(10, VA)
    end),
    gen_server:cast(Pid, {virtual_access_removed, 10, 50}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        VA = maps:get(virtual_channel_access, State, #{}),
        not maps:is_key(10, VA)
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

virtual_access_multiple_channels_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {virtual_access_added, 10, 50}),
    gen_server:cast(Pid, {virtual_access_added, 10, 60}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        VA = maps:get(virtual_channel_access, State, #{}),
        case maps:get(10, VA, undefined) of
            undefined -> false;
            Channels -> sets:size(Channels) =:= 2
        end
    end),
    gen_server:cast(Pid, {virtual_access_removed, 10, 50}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        VA = maps:get(virtual_channel_access, State, #{}),
        case maps:get(10, VA, undefined) of
            undefined -> false;
            Channels -> sets:size(Channels) =:= 1
        end
    end),
    State = sys:get_state(Pid),
    VA = maps:get(virtual_channel_access, State, #{}),
    Channels = maps:get(10, VA),
    ?assertEqual(true, sets:is_element(60, Channels)),
    ?assertEqual(false, sets:is_element(50, Channels)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

virtual_access_cleanup_removes_all_channels_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {virtual_access_added, 10, 50}),
    gen_server:cast(Pid, {virtual_access_added, 10, 60}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        VA = maps:get(virtual_channel_access, State, #{}),
        maps:is_key(10, VA)
    end),
    gen_server:cast(Pid, {virtual_access_cleanup, 10}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        VA = maps:get(virtual_channel_access, State, #{}),
        not maps:is_key(10, VA)
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

virtual_access_removed_nonexistent_user_noop_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {virtual_access_removed, 999, 50}),
    timer:sleep(100),
    State = sys:get_state(Pid),
    VA = maps:get(virtual_channel_access, State, #{}),
    ?assertNot(maps:is_key(999, VA)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

presence_update_multiple_users_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"online">>}}),
    gen_server:cast(Pid, {presence_update, 20, #{<<"status">> => <<"idle">>}}),
    gen_server:cast(Pid, {presence_update, 30, #{<<"status">> => <<"dnd">>}}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Presence = maps:get(member_presence, State, #{}),
        maps:is_key(10, Presence) andalso maps:is_key(20, Presence) andalso maps:is_key(30, Presence)
    end),
    State = sys:get_state(Pid),
    Presence = maps:get(member_presence, State, #{}),
    ?assertEqual(#{<<"status">> => <<"online">>}, maps:get(10, Presence)),
    ?assertEqual(#{<<"status">> => <<"idle">>}, maps:get(20, Presence)),
    ?assertEqual(#{<<"status">> => <<"dnd">>}, maps:get(30, Presence)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

presence_update_overwrite_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"online">>}}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Presence = maps:get(member_presence, State, #{}),
        maps:get(10, Presence, #{}) =:= #{<<"status">> => <<"online">>}
    end),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"offline">>}}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Presence = maps:get(member_presence, State, #{}),
        maps:get(10, Presence, #{}) =:= #{<<"status">> => <<"offline">>}
    end),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"online">>}}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Presence = maps:get(member_presence, State, #{}),
        maps:get(10, Presence, #{}) =:= #{<<"status">> => <<"online">>}
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

subscribe_multiple_sessions_to_same_channel_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 10}),
    gen_server:cast(Pid, {session_connected, <<"s2">>, 0, 20}),
    gen_server:cast(Pid, {subscribe, <<"s1">>, 10, [{0, 50}]}),
    gen_server:cast(Pid, {subscribe, <<"s2">>, 10, [{25, 75}]}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Subs = maps:get(subscriptions, State, #{}),
        case maps:get(<<"10">>, Subs, undefined) of
            undefined -> false;
            ListSubs ->
                maps:is_key(<<"s1">>, ListSubs) andalso maps:is_key(<<"s2">>, ListSubs)
        end
    end),
    State = sys:get_state(Pid),
    Subs = maps:get(subscriptions, State, #{}),
    ListSubs = maps:get(<<"10">>, Subs),
    ?assertEqual([{0, 50}], maps:get(<<"s1">>, ListSubs)),
    ?assertEqual([{25, 75}], maps:get(<<"s2">>, ListSubs)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

subscribe_invalid_ranges_filtered_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 10}),
    gen_server:cast(Pid, {subscribe, <<"s1">>, 10, [{100, 50}, {-1, 10}]}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    State = sys:get_state(Pid),
    Subs = maps:get(subscriptions, State, #{}),
    ?assertEqual(#{}, Subs),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

upsert_member_with_no_user_field_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    MemberData = #{<<"nick">> => <<"orphan">>},
    gen_server:cast(Pid, {upsert_member, MemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

upsert_member_then_update_roles_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    MemberData = #{
        <<"user">> => #{<<"id">> => <<"42">>, <<"username">> => <<"alice">>},
        <<"roles">> => [<<"100">>]
    },
    gen_server:cast(Pid, {upsert_member, MemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Members = guild_data_index:member_map(Base),
        maps:is_key(42, Members)
    end),
    UpdatedMemberData = #{
        <<"user">> => #{<<"id">> => <<"42">>, <<"username">> => <<"alice">>},
        <<"roles">> => [<<"200">>, <<"300">>]
    },
    gen_server:cast(Pid, {upsert_member, UpdatedMemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Members = guild_data_index:member_map(Base),
        case maps:get(42, Members, undefined) of
            undefined -> false;
            M -> maps:get(<<"roles">>, M, []) =:= [<<"200">>, <<"300">>]
        end
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

remove_member_then_upsert_again_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    MemberData = #{
        <<"user">> => #{<<"id">> => <<"42">>, <<"username">> => <<"alice">>},
        <<"roles">> => []
    },
    gen_server:cast(Pid, {upsert_member, MemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        maps:is_key(42, guild_data_index:member_map(Base))
    end),
    gen_server:cast(Pid, {remove_member, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        not maps:is_key(42, guild_data_index:member_map(Base))
    end),
    gen_server:cast(Pid, {upsert_member, MemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        maps:is_key(42, guild_data_index:member_map(Base))
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

multiple_rapid_presence_updates_coalesce_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"online">>}}),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"idle">>}}),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"dnd">>}}),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"offline">>}}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    State = sys:get_state(Pid),
    Presence = maps:get(member_presence, State, #{}),
    ?assertEqual(#{<<"status">> => <<"offline">>}, maps:get(10, Presence)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

unknown_cast_ignored_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {unknown_message, <<"data">>}),
    timer:sleep(50),
    ?assertEqual(true, is_process_alive(Pid)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

unknown_info_ignored_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    Pid ! {unknown_info_message},
    timer:sleep(50),
    ?assertEqual(true, is_process_alive(Pid)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

snapshot_includes_presence_and_sessions_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    gen_server:cast(Pid, {session_connected, <<"s1">>, 0, 10}),
    gen_server:cast(Pid, {presence_update, 10, #{<<"status">> => <<"online">>}}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    State = sys:get_state(Pid),
    Snapshot = maps:get(snapshot, State),
    ?assertEqual(1, maps:get(id, Snapshot)),
    SnapshotPresence = maps:get(member_presence, Snapshot),
    ?assertEqual(#{<<"status">> => <<"online">>}, maps:get(10, SnapshotPresence)),
    SnapshotSessions = maps:get(sessions, Snapshot),
    ?assert(map_size(SnapshotSessions) > 0),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

channels_bulk_update_invalid_input_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {channels_bulk_update, not_a_list}),
    timer:sleep(100),
    ?assertEqual(true, is_process_alive(Pid)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

roles_bulk_update_invalid_input_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {roles_bulk_update, not_a_list}),
    timer:sleep(100),
    ?assertEqual(true, is_process_alive(Pid)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

upsert_channel_new_channel_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    NewChannel = #{<<"id">> => <<"999">>, <<"name">> => <<"new-channel">>},
    gen_server:cast(Pid, {upsert_channel, NewChannel}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        Channels = guild_data_index:channel_list(Base),
        lists:any(fun(C) -> maps:get(<<"id">>, C, undefined) =:= <<"999">> end, Channels)
    end),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

member_removed_while_presence_update_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    MemberData = #{
        <<"user">> => #{<<"id">> => <<"42">>, <<"username">> => <<"alice">>},
        <<"roles">> => []
    },
    gen_server:cast(Pid, {upsert_member, MemberData}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        maps:is_key(42, guild_data_index:member_map(Base))
    end),
    gen_server:cast(Pid, {presence_update, 42, #{<<"status">> => <<"online">>}}),
    gen_server:cast(Pid, {remove_member, 42}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        Base = maps:get(base_data, State, #{}),
        not maps:is_key(42, guild_data_index:member_map(Base))
    end),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    ?assertEqual(true, is_process_alive(Pid)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

upsert_member_non_map_ignored_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {upsert_member, not_a_map}),
    timer:sleep(100),
    ?assertEqual(true, is_process_alive(Pid)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

full_sync_all_clears_pending_test() ->
    Shard0Pid = start_stub_shard0(),
    {ok, Pid} = start_link(#{
        id => 1,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    gen_server:cast(Pid, {notify_role_update}),
    ok = await(fun() ->
        State = sys:get_state(Pid),
        maps:get(compute_inflight, State, true) =:= false
    end),
    State = sys:get_state(Pid),
    ?assertEqual(false, maps:get(pending_full_sync_all, State)),
    ?assertEqual(false, maps:get(pending_delta, State)),
    ?assertEqual(false, maps:get(pending_refresh_base_data, State)),
    catch gen_server:stop(Pid),
    unlink(Shard0Pid),
    exit(Shard0Pid, shutdown),
    ok.

-endif.

