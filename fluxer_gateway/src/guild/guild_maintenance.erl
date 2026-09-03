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
-type viewable_memo() :: #{exceptions := sets:set(user_id()), cache := #{term() => map()}}.
-type prune_acc() :: {subscription_state(), #{user_id() => non_neg_integer()}, viewable_memo()}.

-spec update_counts(guild_state()) -> guild_state().
update_counts(State) ->
    State0 = guild_members_table_repair:maybe_repair(State),
    Data = maps:get(data, State0, #{}),
    MemberCount = guild_data_index:member_count(Data),
    OnlineCount = guild_member_list:get_online_count(State0),
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
    OldMemberIds = sets:from_list(guild_data_index:member_ids(OldData)),
    NewMemberIds = sets:from_list(guild_data_index:member_ids(NewData)),
    RemovedIds = sets:to_list(sets:subtract(OldMemberIds, NewMemberIds)),
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    NewPresenceSubs = unsubscribe_removed_members(RemovedIds, PresenceSubs),
    State#{presence_subscriptions => NewPresenceSubs}.

-spec apply_everyone_perm_bit(integer(), guild_state()) -> guild_state().
apply_everyone_perm_bit(Bit, State) ->
    GuildId = maps:get(id, State),
    Data = maps:get(data, State, #{}),
    Roles = guild_data_index:role_list(Data),
    {Updated, Changed} = update_everyone_role(Roles, GuildId, Bit),
    case Changed of
        false -> State;
        true -> State#{data => guild_data_index:put_roles(Updated, Data)}
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
    maps:merge(guild_subscriptions:init_state(), MemberSubs).

-spec prune_member_subscription_map(subscription_state(), map(), guild_state()) ->
    {subscription_state(), #{user_id() => non_neg_integer()}}.
prune_member_subscription_map(MemberSubs, _Sessions, _State) when map_size(MemberSubs) =:= 0 ->
    {MemberSubs, #{}};
prune_member_subscription_map(MemberSubs, Sessions, State) ->
    {NewMemberSubs, Counts, _Memo} = maps:fold(
        fun(MemberId, Subscribers, Acc) ->
            prune_member_subscribers(MemberId, Subscribers, Sessions, State, Acc)
        end,
        {MemberSubs, #{}, new_viewable_memo(State)},
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
    {MemberViewable, Memo1} = memoised_member_viewable_channel_map(MemberId, State, Memo),
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
                guild_visibility:get_user_viewable_channels(SessionUserId, State)
            )
    end.

-spec member_viewable_channel_map(user_id(), guild_state()) -> map().
member_viewable_channel_map(MemberId, State) ->
    guild_sessions:build_viewable_channel_map(
        guild_visibility:get_user_viewable_channels(MemberId, State)
    ).

%% One pass reads a fixed state, and get_user_viewable_channels/2 reads nothing from a
%% member except its raw roles term, so two members holding the same term see the same
%% channels. The user-specific inputs are the guild owner, virtual channel access and
%% user-type permission overwrites, and every user named by one of those bypasses the
%% memo. A term is still computed the first time the pass meets it, so a term whose
%% computation raises raises on the same member it does without the memo.
-spec new_viewable_memo(guild_state()) -> viewable_memo().
new_viewable_memo(State) ->
    #{exceptions => viewable_exceptions(State), cache => #{}}.

-spec memoised_member_viewable_channel_map(user_id(), guild_state(), viewable_memo()) ->
    {map(), viewable_memo()}.
memoised_member_viewable_channel_map(MemberId, State, Memo) ->
    #{exceptions := Exceptions, cache := Cache} = Memo,
    case memo_key(MemberId, Exceptions, State) of
        bypass ->
            {member_viewable_channel_map(MemberId, State), Memo};
        {ok, RawRoles} ->
            cached_member_viewable_channel_map(MemberId, RawRoles, State, Cache, Memo)
    end.

-spec cached_member_viewable_channel_map(
    user_id(), term(), guild_state(), #{term() => map()}, viewable_memo()
) -> {map(), viewable_memo()}.
cached_member_viewable_channel_map(MemberId, RawRoles, State, Cache, Memo) ->
    case maps:find(RawRoles, Cache) of
        {ok, Viewable} ->
            {Viewable, Memo};
        error ->
            Viewable = member_viewable_channel_map(MemberId, State),
            {Viewable, Memo#{cache := Cache#{RawRoles => Viewable}}}
    end.

-spec memo_key(user_id(), sets:set(user_id()), guild_state()) -> {ok, term()} | bypass.
memo_key(MemberId, Exceptions, State) ->
    case sets:is_element(MemberId, Exceptions) of
        true -> bypass;
        false -> member_roles_key(guild_permissions:find_member_by_user_id(MemberId, State))
    end.

-spec member_roles_key(map() | undefined) -> {ok, term()} | bypass.
member_roles_key(Member) when is_map(Member) ->
    {ok, maps:get(<<"roles">>, Member, [])};
member_roles_key(_Member) ->
    bypass.

-spec viewable_exceptions(guild_state()) -> sets:set(user_id()).
viewable_exceptions(State) ->
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    Owner = owner_ids(maps:get(<<"owner_id">>, Guild, undefined)),
    Virtual = maps:keys(map_utils:ensure_map(maps:get(virtual_channel_access, State, #{}))),
    sets:from_list(Owner ++ Virtual ++ overwrite_target_ids(Data)).

%% parse_maybe/1, not parse_optional/1: an id this rejects makes the permission check
%% itself raise before it can match a user, so leaving it out keeps the memo exact.
-spec owner_ids(term()) -> [user_id()].
owner_ids(OwnerIdRaw) ->
    case snowflake_id:parse_maybe(OwnerIdRaw) of
        OwnerId when is_integer(OwnerId) -> [OwnerId];
        _ -> []
    end.

%% Both places a permission check can take overwrites from, and role targets alongside
%% user targets. Role ids can never collide with user ids, so the superset only costs a
%% few members their memo entry.
-spec overwrite_target_ids(map()) -> [user_id()].
overwrite_target_ids(Data) ->
    Cached = maps:fold(
        fun(_ChannelId, Entries, Acc) -> cached_target_ids(Entries, Acc) end,
        [],
        map_utils:ensure_map(maps:get(overwrite_perms_cache, Data, #{}))
    ),
    maps:fold(
        fun(_ChannelId, Channel, Acc) -> channel_target_ids(Channel, Acc) end,
        Cached,
        guild_data_index:channel_index(Data)
    ).

-spec cached_target_ids(term(), [user_id()]) -> [user_id()].
cached_target_ids(Entries, Acc) when is_list(Entries) ->
    [Id || {Id, _Type, _Allow, _Deny} <- Entries, is_integer(Id)] ++ Acc;
cached_target_ids(_Entries, Acc) ->
    Acc.

-spec channel_target_ids(term(), [user_id()]) -> [user_id()].
channel_target_ids(Channel, Acc) when is_map(Channel) ->
    lists:foldl(
        fun overwrite_target_id/2,
        Acc,
        map_utils:ensure_list(maps:get(<<"permission_overwrites">>, Channel, []))
    );
channel_target_ids(_Channel, Acc) ->
    Acc.

-spec overwrite_target_id(term(), [user_id()]) -> [user_id()].
overwrite_target_id(Overwrite, Acc) when is_map(Overwrite) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Overwrite, undefined)) of
        Id when is_integer(Id) -> [Id | Acc];
        _ -> Acc
    end;
overwrite_target_id(_Overwrite, Acc) ->
    Acc.

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
            maps_share_any_key(SessionViewable, MemberViewable);
        _ ->
            false
    end.

-spec maps_share_any_key(map(), map()) -> boolean().
maps_share_any_key(MapA, MapB) ->
    {Smaller, Larger} =
        case map_size(MapA) =< map_size(MapB) of
            true -> {MapA, MapB};
            false -> {MapB, MapA}
        end,
    maps_share_any_key_iter(maps:iterator(Smaller), Larger).

-spec maps_share_any_key_iter(maps:iterator(), map()) -> boolean().
maps_share_any_key_iter(Iterator, LargerMap) ->
    case maps:next(Iterator) of
        none ->
            false;
        {Key, _, Next} ->
            maps:is_key(Key, LargerMap) orelse maps_share_any_key_iter(Next, LargerMap)
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
    case permission_bits:has(Current, Bit) of
        true ->
            {[Role | Acc], ChangedAcc};
        false ->
            New = permission_bits:add(Current, Bit),
            NewRole = Role#{<<"permissions">> => New},
            {[NewRole | Acc], true}
    end.

-spec role_permissions_int(map()) -> integer().
role_permissions_int(Role) ->
    permission_bits:parse(maps:get(<<"permissions">>, Role, undefined)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prune_invalid_member_subscriptions_batches_by_member_test() ->
    State = prune_test_state(),
    Result = maybe_prune_invalid_member_subscriptions(guild_role_update, State),
    MemberSubs = maps:get(member_subscriptions, Result),
    ?assertEqual([<<"s1">>], guild_subscriptions:get_subscribed_sessions(20, MemberSubs)),
    ?assertEqual([], guild_subscriptions:get_subscribed_sessions(30, MemberSubs)),
    ?assertEqual(#{20 => 1, 30 => 1}, maps:get(presence_subscriptions, Result)).

prune_test_state() ->
    GuildId = 42,
    ViewerRole = 1000,
    OtherRole = 2000,
    ChannelA = 500,
    ChannelB = 600,
    MemberSubs0 = guild_subscriptions:init_state(),
    MemberSubs1 = guild_subscriptions:subscribe(<<"s1">>, 20, MemberSubs0),
    MemberSubs2 = guild_subscriptions:subscribe(<<"s1">>, 30, MemberSubs1),
    MemberSubs3 = guild_subscriptions:subscribe(<<"missing">>, 30, MemberSubs2),
    #{
        id => GuildId,
        sessions => #{
            <<"s1">> => #{
                session_id => <<"s1">>,
                user_id => 10,
                pid => self(),
                viewable_channels => #{ChannelA => true}
            }
        },
        member_subscriptions => MemberSubs3,
        presence_subscriptions => #{20 => 1, 30 => 3},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                prune_role(GuildId, 0),
                prune_role(ViewerRole, 0),
                prune_role(OtherRole, 0)
            ],
            <<"members">> => #{
                10 => prune_member(10, [ViewerRole]),
                20 => prune_member(20, [ViewerRole]),
                30 => prune_member(30, [OtherRole])
            },
            <<"channels">> => [
                prune_channel(ChannelA, [prune_overwrite(ViewerRole, 0)]),
                prune_channel(ChannelB, [prune_overwrite(OtherRole, 0)])
            ]
        }
    }.

prune_role(RoleId, Permissions) ->
    #{
        <<"id">> => integer_to_binary(RoleId),
        <<"permissions">> => integer_to_binary(Permissions)
    }.

prune_member(UserId, RoleIds) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"roles">> => [integer_to_binary(RoleId) || RoleId <- RoleIds]
    }.

update_counts_repairs_missing_members_table_test() ->
    ok = application:set_env(fluxer_gateway, guild_members_table_self_heal, true),
    try
        Result = update_counts(repair_counts_state()),
        Tab = maps:get(members_ets, maps:get(data, Result)),
        ?assert(is_reference(Tab)),
        ?assertNot(maps:is_key(members_ets_repair, Result)),
        ?assertEqual([1, 2], lists:sort([Id || {Id, _} <- ets:tab2list(Tab)])),
        ?assertEqual(2, maps:get(member_count, Result)),
        ets:delete(Tab)
    after
        application:unset_env(fluxer_gateway, guild_members_table_self_heal)
    end.

repair_counts_state() ->
    MemberMap = #{
        1 => #{<<"user">> => #{<<"id">> => 1}},
        2 => #{<<"user">> => #{<<"id">> => 2}}
    },
    #{
        id => 42,
        disable_guild_count_cache_updates => true,
        data => #{
            <<"channels">> => [],
            <<"members">> => MemberMap,
            members_normalized => MemberMap
        }
    }.

prune_channel(ChannelId, Overwrites) ->
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"permission_overwrites">> => Overwrites
    }.

prune_overwrite(TargetId, Type) ->
    #{
        <<"id">> => integer_to_binary(TargetId),
        <<"type">> => Type,
        <<"allow">> => integer_to_binary(constants:view_channel_permission()),
        <<"deny">> => <<"0">>
    }.

%% Members 30, 31, 99 and 4242 all carry the same roles term and reach only channel 600
%% through it, but 99 holds virtual access to channel 500 and 4242 is a user-overwrite
%% target on 500, so neither may be answered from that term. Member 7 owns the guild,
%% 777 is subscribed without being a member, and session s3 has no cached viewable map.
memo_prune_state() ->
    GuildId = 42,
    ViewerRole = 1000,
    OtherRole = 2000,
    Members = maps:from_list(
        [{7, prune_member(7, [])}] ++
            [{Id, prune_member(Id, [ViewerRole])} || Id <- [10, 20, 21]] ++
            [{Id, prune_member(Id, [OtherRole])} || Id <- [30, 31, 99, 4242]]
    ),
    #{
        id => GuildId,
        virtual_channel_access => #{99 => sets:from_list([500])},
        sessions => #{
            <<"s1">> => #{user_id => 10, viewable_channels => #{500 => true}},
            <<"s2">> => #{user_id => 30, viewable_channels => #{600 => true}},
            <<"s3">> => #{user_id => 10}
        },
        member_subscriptions => memo_prune_subscriptions(),
        presence_subscriptions => maps:from_list(
            [{Id, 3} || Id <- [7, 20, 21, 30, 31, 99, 4242, 777]]
        ),
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"7">>},
            <<"roles">> => [
                prune_role(GuildId, 0),
                prune_role(ViewerRole, 0),
                prune_role(OtherRole, 0)
            ],
            <<"members">> => Members,
            <<"channels">> => [
                prune_channel(500, [prune_overwrite(ViewerRole, 0), prune_overwrite(4242, 1)]),
                prune_channel(600, [prune_overwrite(OtherRole, 0)])
            ]
        }
    }.

memo_prune_subscriptions() ->
    lists:foldl(
        fun({SessionId, MemberIds}, Acc) ->
            guild_subscriptions:update_subscriptions(SessionId, MemberIds, Acc)
        end,
        guild_subscriptions:init_state(),
        [
            {<<"s1">>, [20, 21, 30, 31, 7, 99, 4242, 777]},
            {<<"s2">>, [20, 30]},
            {<<"s3">>, [20, 30]},
            {<<"missing">>, [31]}
        ]
    ).

%% prune_member_subscription_map/3 as it read before the memo: every subscribed member
%% materialises its own viewable channel map and every subscriber set is rebuilt.
reference_prune_member_subscription_map(MemberSubs, Sessions, State) ->
    maps:fold(
        fun(MemberId, Subscribers, Acc) ->
            reference_prune_member(MemberId, Subscribers, Sessions, State, Acc)
        end,
        {MemberSubs, #{}},
        MemberSubs
    ).

reference_prune_member(MemberId, Subscribers, Sessions, State, Acc) when is_integer(MemberId) ->
    MemberViewable = member_viewable_channel_map(MemberId, State),
    {Kept, RemovedCount} = reference_prune_subscriber_set(
        Subscribers, Sessions, MemberViewable, State
    ),
    update_pruned_member_subscription(MemberId, Kept, RemovedCount, Acc);
reference_prune_member(_MemberId, _Subscribers, _Sessions, _State, Acc) ->
    Acc.

reference_prune_subscriber_set(Subscribers, Sessions, MemberViewable, State) ->
    sets:fold(
        fun(SessionId, Acc) ->
            reference_prune_subscriber(SessionId, Sessions, MemberViewable, State, Acc)
        end,
        {sets:new(), 0},
        Subscribers
    ).

reference_prune_subscriber(SessionId, Sessions, MemberViewable, State, {Kept, RemovedCount}) ->
    case subscriber_can_still_view_member(SessionId, Sessions, MemberViewable, State) of
        true -> {sets:add_element(SessionId, Kept), RemovedCount};
        false -> {Kept, RemovedCount + 1}
    end.

prune_member_subscription_map_matches_unmemoised_reference_test() ->
    State = memo_prune_state(),
    MemberSubs = member_subscriptions(State),
    Sessions = maps:get(sessions, State),
    ?assertEqual(
        reference_prune_member_subscription_map(MemberSubs, Sessions, State),
        prune_member_subscription_map(MemberSubs, Sessions, State)
    ).

prune_keeps_exception_members_a_session_can_still_see_test() ->
    Result = maybe_prune_invalid_member_subscriptions(guild_role_update, memo_prune_state()),
    MemberSubs = maps:get(member_subscriptions, Result),
    Presence = #{7 => 3, 20 => 2, 21 => 3, 30 => 1, 31 => 1, 99 => 3, 4242 => 3, 777 => 2},
    ?assertEqual(
        [[<<"s1">>], [<<"s1">>], [<<"s1">>], [], []],
        [
            lists:sort(guild_subscriptions:get_subscribed_sessions(Id, MemberSubs))
         || Id <- [7, 99, 4242, 31, 777]
        ]
    ),
    ?assertEqual(Presence, maps:get(presence_subscriptions, Result)).

memo_reuses_one_entry_per_roles_term_test() ->
    State = memo_prune_state(),
    {First, Memo1} = memoised_member_viewable_channel_map(20, State, new_viewable_memo(State)),
    {Second, Memo2} = memoised_member_viewable_channel_map(21, State, Memo1),
    ?assertEqual(member_viewable_channel_map(21, State), Second),
    ?assertEqual(First, Second),
    ?assertEqual([1, 1], [map_size(maps:get(cache, M)) || M <- [Memo1, Memo2]]).

memo_never_answers_owner_virtual_or_overwrite_targets_from_the_cache_test() ->
    State = memo_prune_state(),
    {_, Memo} = memoised_member_viewable_channel_map(30, State, new_viewable_memo(State)),
    Answers = [memoised_member_viewable_channel_map(Id, State, Memo) || Id <- [7, 99, 4242]],
    ?assertEqual(
        [member_viewable_channel_map(Id, State) || Id <- [7, 99, 4242]],
        [Viewable || {Viewable, _} <- Answers]
    ),
    ?assertEqual([Memo, Memo, Memo], [M || {_, M} <- Answers]).

prune_subscriber_set_returns_the_original_set_when_nothing_is_removed_test() ->
    State = memo_prune_state(),
    Subscribers = sets:from_list([<<"s1">>, <<"s3">>]),
    Viewable = member_viewable_channel_map(20, State),
    ?assertEqual(
        {Subscribers, 0},
        prune_subscriber_set(Subscribers, maps:get(sessions, State), Viewable, State)
    ).
permission_cache_skips_nick_only_member_update_test() ->
    UserId = 4242,
    OldState = permission_cache_state(
        7710001, permission_cache_member(UserId, [10], <<"old">>)
    ),
    EventData = permission_cache_member(UserId, [10], <<"new">>),
    NewState = permission_cache_apply(EventData, OldState),
    ?assertNotEqual(
        guild_data_index:get_member(UserId, maps:get(data, OldState)),
        guild_data_index:get_member(UserId, maps:get(data, NewState))
    ),
    ?assertEqual(
        {error, not_found},
        permission_cache_refresh(guild_member_update, EventData, OldState, NewState)
    ).

permission_cache_rebuilds_on_member_role_change_test() ->
    UserId = 4242,
    OldState = permission_cache_state(
        7710002, permission_cache_member(UserId, [10], <<"old">>)
    ),
    EventData = permission_cache_member(UserId, [10, 11], <<"old">>),
    NewState = permission_cache_apply(EventData, OldState),
    {ok, Snapshot} = permission_cache_refresh(
        guild_member_update, EventData, OldState, NewState
    ),
    #{<<"roles">> := Roles} = guild_permissions:find_member_by_user_id(UserId, Snapshot),
    ?assertEqual([10, 11], Roles).

permission_cache_rebuilds_on_member_timeout_change_test() ->
    UserId = 4242,
    Until = <<"2026-09-01T00:00:00.000Z">>,
    OldState = permission_cache_state(
        7710003, permission_cache_member(UserId, [10], <<"old">>)
    ),
    EventData = (permission_cache_member(UserId, [10], <<"old">>))#{
        <<"communication_disabled_until">> => Until
    },
    NewState = permission_cache_apply(EventData, OldState),
    {ok, Snapshot} = permission_cache_refresh(
        guild_member_update, EventData, OldState, NewState
    ),
    #{<<"communication_disabled_until">> := MemberUntil} =
        guild_permissions:find_member_by_user_id(UserId, Snapshot),
    ?assertEqual(Until, MemberUntil).

permission_cache_rebuilds_for_other_mutating_events_test() ->
    State = permission_cache_state(7710004, permission_cache_member(4242, [10], <<"old">>)),
    ?assertMatch({ok, _}, permission_cache_refresh(guild_role_update, #{}, State, State)).

permission_cache_rebuilds_without_member_update_user_id_test() ->
    State = permission_cache_state(7710005, permission_cache_member(4242, [10], <<"old">>)),
    ?assertMatch({ok, _}, permission_cache_refresh(guild_member_update, #{}, State, State)).

permission_cache_refresh(Event, EventData, OldState, NewState) ->
    GuildId = maps:get(id, NewState),
    ok = guild_permission_cache:delete(GuildId),
    try
        ok = maybe_put_permission_cache(Event, EventData, OldState, NewState),
        guild_permission_cache:get_snapshot(GuildId)
    after
        ok = guild_permission_cache:delete(GuildId)
    end.

permission_cache_state(GuildId, Member) ->
    #{
        id => GuildId,
        data => guild_data_index:normalize_map(#{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [prune_role(10, 0), prune_role(11, 0)],
            <<"members">> => [Member],
            <<"channels">> => []
        })
    }.

permission_cache_member(UserId, RoleIds, Nick) ->
    (prune_member(UserId, RoleIds))#{<<"nick">> => Nick}.

permission_cache_apply(EventData, State) ->
    State#{data => guild_state_member:handle_member_update(EventData, maps:get(data, State))}.

-endif.
