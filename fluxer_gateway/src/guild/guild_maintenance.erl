%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_maintenance).
-typing([eqwalizer]).

-export([
    update_counts/1,
    maybe_put_permission_cache/1,
    maybe_put_permission_cache/4,
    maybe_delete_permission_cache/2,
    maybe_put_guild_count_cache/3,
    schedule_count_cache_refresh/1,
    maybe_prune_invalid_member_subscriptions/2,
    cleanup_removed_member_subscriptions/3,
    apply_everyone_perm_bit/2
]).
-export_type([guild_state/0]).

-define(COUNT_CACHE_REFRESH_INTERVAL, 30000).

-type user_id() :: integer().
-type session_id() :: binary().
-type subscription_state() :: guild_subscriptions:subscription_state().
-type guild_state() :: map().
-type prune_acc() :: {
    subscription_state(), #{user_id() => non_neg_integer()}, guild_viewable_memo:memo()
}.

-spec update_counts(guild_state()) -> guild_state().
update_counts(State) ->
    State0 = guild_members_table_repair:maybe_repair(State),
    Data = maps:get(data, State0, #{}),
    MemberCount = guild_data_index_members:member_count(Data),
    OnlineCount = guild_member_list_read:get_online_count(State0),
    PublicOnlineCount = guild_public_online:compute_count(State0),
    ok = maybe_put_guild_count_cache(State0, MemberCount, PublicOnlineCount),
    State0#{
        member_count => MemberCount,
        online_count => OnlineCount,
        public_online_count => PublicOnlineCount
    }.

-spec maybe_put_permission_cache(guild_state()) -> ok.
maybe_put_permission_cache(State) ->
    case maps:get(disable_permission_cache_updates, State, false) of
        true -> ok;
        false -> guild_permission_cache:put_state(State)
    end.

-spec maybe_put_permission_cache(term(), map(), guild_state(), guild_state()) -> ok.
maybe_put_permission_cache(Event, EventData, OldState, NewState) ->
    case permission_cache_needs_rebuild(Event, EventData, OldState, NewState) of
        true -> maybe_put_permission_cache(NewState);
        false -> ok
    end.

-spec permission_cache_needs_rebuild(term(), map(), guild_state(), guild_state()) -> boolean().
permission_cache_needs_rebuild(guild_member_update, EventData, OldState, NewState) ->
    guild_permission_cache:member_projection_changed(
        member_update_user_id(EventData),
        guild_data_index:ensure_data_map(OldState),
        guild_data_index:ensure_data_map(NewState)
    );
permission_cache_needs_rebuild(_Event, _EventData, _OldState, _NewState) ->
    true.

-spec member_update_user_id(map()) -> user_id() | undefined.
member_update_user_id(#{<<"user">> := #{<<"id">> := Id}}) ->
    snowflake_id:parse_maybe(Id);
member_update_user_id(_) ->
    undefined.

-spec maybe_delete_permission_cache(term(), guild_state()) -> ok.
maybe_delete_permission_cache(GuildId, State) ->
    case maps:get(disable_permission_cache_updates, State, false) of
        true -> ok;
        false when is_integer(GuildId) -> guild_permission_cache:delete(GuildId);
        false -> ok
    end.

-spec maybe_put_guild_count_cache(guild_state(), non_neg_integer(), non_neg_integer()) -> ok.
maybe_put_guild_count_cache(State, MemberCount, OnlineCount) ->
    case
        {
            maps:get(disable_guild_count_cache_updates, State, false),
            maps:get(id, State, undefined)
        }
    of
        {true, _} ->
            ok;
        {false, GuildId} when is_integer(GuildId) ->
            guild_counts_cache:update(GuildId, MemberCount, OnlineCount);
        _ ->
            ok
    end.

-spec schedule_count_cache_refresh(guild_state()) -> guild_state().
schedule_count_cache_refresh(State) ->
    case maps:get(disable_guild_count_cache_updates, State, false) of
        true ->
            State;
        false ->
            erlang:send_after(?COUNT_CACHE_REFRESH_INTERVAL, self(), count_cache_refresh),
            State
    end.

-spec maybe_prune_invalid_member_subscriptions(term(), guild_state()) -> guild_state().
maybe_prune_invalid_member_subscriptions(Event, State) ->
    case event_requires_prune(Event) of
        true -> prune_invalid_member_subscriptions(State);
        false -> State
    end.

-spec cleanup_removed_member_subscriptions(map(), map(), guild_state()) -> guild_state().
cleanup_removed_member_subscriptions(OldData, NewData, State) ->
    OldMemberIds = sets:from_list(guild_data_index_members:member_ids(OldData)),
    NewMemberIds = sets:from_list(guild_data_index_members:member_ids(NewData)),
    RemovedIds = sets:to_list(sets:subtract(OldMemberIds, NewMemberIds)),
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    NewPresenceSubs = unsubscribe_removed_members(RemovedIds, PresenceSubs),
    State#{presence_subscriptions => NewPresenceSubs}.

-spec apply_everyone_perm_bit(integer(), guild_state()) -> guild_state().
apply_everyone_perm_bit(Bit, State) ->
    GuildId = maps:get(id, State),
    Data = maps:get(data, State, #{}),
    Roles = guild_data_index_roles:role_list(Data),
    {Updated, Changed} = update_everyone_role(Roles, GuildId, Bit),
    case Changed of
        false ->
            State;
        true ->
            guild_member_list_engine_inputs:latch_stale(
                State#{data => guild_data_index:put_roles(Updated, Data)}
            )
    end.

-spec event_requires_prune(term()) -> boolean().
event_requires_prune(guild_member_remove) -> true;
event_requires_prune(guild_member_update) -> true;
event_requires_prune(guild_role_update) -> true;
event_requires_prune(guild_role_update_bulk) -> true;
event_requires_prune(guild_role_delete) -> true;
event_requires_prune(channel_update) -> true;
event_requires_prune(channel_update_bulk) -> true;
event_requires_prune(channel_delete) -> true;
event_requires_prune(_) -> false.

-spec prune_invalid_member_subscriptions(guild_state()) -> guild_state().
prune_invalid_member_subscriptions(State) ->
    MemberSubs = member_subscriptions(State),
    Sessions = maps:get(sessions, State, #{}),
    {NewMemberSubs, PresenceUnsubs} = prune_member_subscription_map(
        MemberSubs, Sessions, State
    ),
    State1 = State#{member_subscriptions => NewMemberSubs},
    apply_presence_unsub_counts(PresenceUnsubs, State1).

-spec member_subscriptions(guild_state()) -> subscription_state().
member_subscriptions(State) ->
    require_subscription_state(
        maps:get(member_subscriptions, State, guild_subscriptions:init_state())
    ).

-spec require_subscription_state(term()) -> subscription_state().
require_subscription_state(MemberSubs) when is_map(MemberSubs) ->
    maps:merge(guild_subscriptions:init_state(), eqwalizer:dynamic_cast(MemberSubs)).

-spec prune_member_subscription_map(subscription_state(), map(), guild_state()) ->
    {subscription_state(), #{user_id() => non_neg_integer()}}.
prune_member_subscription_map(MemberSubs, _Sessions, _State) when map_size(MemberSubs) =:= 0 ->
    {MemberSubs, #{}};
prune_member_subscription_map(MemberSubs, Sessions, State) ->
    {NewMemberSubs, Counts, _Memo} = maps:fold(
        fun(MemberId, Subscribers, Acc) ->
            prune_member_subscribers(MemberId, Subscribers, Sessions, State, Acc)
        end,
        {MemberSubs, #{}, guild_viewable_memo:new(State)},
        MemberSubs
    ),
    {NewMemberSubs, Counts}.

-spec prune_member_subscribers(
    term(),
    sets:set(session_id()),
    map(),
    guild_state(),
    prune_acc()
) -> prune_acc().
prune_member_subscribers(MemberId, Subscribers, Sessions, State, {Subs, Counts, Memo}) when
    is_integer(MemberId)
->
    {MemberViewable, Memo1} = guild_viewable_memo:memoised_channel_map(MemberId, State, Memo),
    {KeptSubscribers, RemovedCount} = prune_subscriber_set(
        Subscribers, Sessions, MemberViewable, State
    ),
    {NewSubs, NewCounts} = update_pruned_member_subscription(
        MemberId, KeptSubscribers, RemovedCount, {Subs, Counts}
    ),
    {NewSubs, NewCounts, Memo1};
prune_member_subscribers(_MemberId, _Subscribers, _Sessions, _State, Acc) ->
    Acc.

-spec session_viewable_channels(map(), user_id(), guild_state()) -> map().
session_viewable_channels(SessionData, SessionUserId, State) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableChannels when is_map(ViewableChannels) ->
            ViewableChannels;
        _ ->
            guild_sessions:build_viewable_channel_map(
                guild_visibility_channels:get_user_viewable_channels(SessionUserId, State)
            )
    end.

%% update_pruned_member_subscription/4 drops the whole result when nothing was removed,
%% so the kept set is only ever built when Removed is non-empty, and then by the same
%% fold over the same subscribers as before.
-spec prune_subscriber_set(sets:set(session_id()), map(), map(), guild_state()) ->
    {sets:set(session_id()), non_neg_integer()}.
prune_subscriber_set(Subscribers, Sessions, MemberViewable, State) ->
    Removed = sets:fold(
        fun(SessionId, Acc) ->
            collect_removed_subscriber(SessionId, Sessions, MemberViewable, State, Acc)
        end,
        #{},
        Subscribers
    ),
    keep_remaining_subscribers(Subscribers, Removed).

-spec collect_removed_subscriber(
    session_id(), map(), map(), guild_state(), #{session_id() => true}
) -> #{session_id() => true}.
collect_removed_subscriber(SessionId, Sessions, MemberViewable, State, Removed) ->
    case subscriber_can_still_view_member(SessionId, Sessions, MemberViewable, State) of
        true -> Removed;
        false -> Removed#{SessionId => true}
    end.

-spec keep_remaining_subscribers(sets:set(session_id()), #{session_id() => true}) ->
    {sets:set(session_id()), non_neg_integer()}.
keep_remaining_subscribers(Subscribers, Removed) when map_size(Removed) =:= 0 ->
    {Subscribers, 0};
keep_remaining_subscribers(Subscribers, Removed) ->
    Kept = sets:fold(
        fun(SessionId, Acc) -> add_kept_subscriber(SessionId, Removed, Acc) end,
        sets:new(),
        Subscribers
    ),
    {Kept, map_size(Removed)}.

-spec add_kept_subscriber(session_id(), #{session_id() => true}, sets:set(session_id())) ->
    sets:set(session_id()).
add_kept_subscriber(SessionId, Removed, Kept) ->
    case maps:is_key(SessionId, Removed) of
        true -> Kept;
        false -> sets:add_element(SessionId, Kept)
    end.

-spec subscriber_can_still_view_member(session_id(), map(), map(), guild_state()) ->
    boolean().
subscriber_can_still_view_member(SessionId, Sessions, MemberViewable, State) ->
    case maps:get(SessionId, Sessions, undefined) of
        SessionData when is_map(SessionData) ->
            session_shares_member_channels(SessionData, MemberViewable, State);
        _ ->
            false
    end.

-spec session_shares_member_channels(map(), map(), guild_state()) -> boolean().
session_shares_member_channels(SessionData, MemberViewable, State) ->
    case maps:get(user_id, SessionData, undefined) of
        SessionUserId when is_integer(SessionUserId) ->
            SessionViewable = session_viewable_channels(SessionData, SessionUserId, State),
            map_utils:maps_share_any_key(SessionViewable, MemberViewable);
        _ ->
            false
    end.

-spec update_pruned_member_subscription(
    user_id(),
    sets:set(session_id()),
    non_neg_integer(),
    {subscription_state(), #{user_id() => non_neg_integer()}}
) -> {subscription_state(), #{user_id() => non_neg_integer()}}.
update_pruned_member_subscription(_MemberId, _KeptSubscribers, 0, Acc) ->
    Acc;
update_pruned_member_subscription(MemberId, KeptSubscribers, RemovedCount, {Subs, Counts}) ->
    NewSubs =
        case sets:size(KeptSubscribers) of
            0 -> maps:remove(MemberId, Subs);
            _ -> Subs#{MemberId => KeptSubscribers}
        end,
    {NewSubs, Counts#{MemberId => RemovedCount}}.

-spec apply_presence_unsub_counts(#{user_id() => non_neg_integer()}, guild_state()) ->
    guild_state().
apply_presence_unsub_counts(Counts, State) ->
    maps:fold(
        fun guild_sessions_presence:unsubscribe_many_from_user_presence/3,
        State,
        Counts
    ).

-spec unsubscribe_removed_members([user_id()], map()) -> map().
unsubscribe_removed_members([], Subs) ->
    Subs;
unsubscribe_removed_members([UserId | Rest], Subs) ->
    NewSubs =
        case maps:is_key(UserId, Subs) of
            true ->
                safe_unsubscribe_presence(UserId),
                maps:remove(UserId, Subs);
            false ->
                Subs
        end,
    unsubscribe_removed_members(Rest, NewSubs).

-spec safe_unsubscribe_presence(user_id()) -> ok.
safe_unsubscribe_presence(UserId) ->
    try presence_bus:unsubscribe(UserId) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec update_everyone_role([map()], integer(), integer()) -> {[map()], boolean()}.
update_everyone_role(Roles, EveryoneId, Bit) ->
    lists:foldr(
        fun(Role, {Acc, ChangedAcc}) ->
            update_everyone_role_entry(Role, EveryoneId, Bit, Acc, ChangedAcc)
        end,
        {[], false},
        Roles
    ).

-spec update_everyone_role_entry(map(), integer(), integer(), [map()], boolean()) ->
    {[map()], boolean()}.
update_everyone_role_entry(Role, EveryoneId, Bit, Acc, ChangedAcc) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, Role, undefined)) of
        EveryoneId ->
            update_matching_everyone_role(Role, Bit, Acc, ChangedAcc);
        _ ->
            {[Role | Acc], ChangedAcc}
    end.

-spec update_matching_everyone_role(map(), integer(), [map()], boolean()) ->
    {[map()], boolean()}.
update_matching_everyone_role(Role, Bit, Acc, ChangedAcc) ->
    Current = role_permissions_int(Role),
    update_everyone_role_permissions(Role, Current, Bit, Acc, ChangedAcc).

-spec update_everyone_role_permissions(map(), integer(), integer(), [map()], boolean()) ->
    {[map()], boolean()}.
update_everyone_role_permissions(Role, Current, Bit, Acc, ChangedAcc) ->
    case bitset:has(Current, Bit) of
        true ->
            {[Role | Acc], ChangedAcc};
        false ->
            New = bitset:add(Current, Bit),
            NewRole = Role#{<<"permissions">> => New},
            {[NewRole | Acc], true}
    end.

-spec role_permissions_int(map()) -> integer().
role_permissions_int(Role) ->
    bitset:parse(maps:get(<<"permissions">>, Role, undefined)).
