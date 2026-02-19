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

-module(guild_subscription_handler).

-export([
    handle_call/3,
    handle_cast/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type guild_state() :: map().
-type user_id() :: integer().
-type session_id() :: binary().
-type channel_id() :: integer().

-spec handle_call(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}.
handle_call({lazy_subscribe, Request}, _From, State) ->
    handle_lazy_subscribe(Request, State).

-spec handle_cast(term(), guild_state()) -> {noreply, guild_state()}.
handle_cast({update_member_subscriptions, SessionId, MemberIds}, State) ->
    NewState0 = handle_update_member_subscriptions(SessionId, MemberIds, State),
    NewState = maybe_prune_very_large_guild_members(NewState0),
    {noreply, NewState};
handle_cast({very_large_guild_member_list_deliver, Deliveries}, State) when is_list(Deliveries) ->
    _ = deliver_member_list_updates(Deliveries, State),
    {noreply, State}.

-spec handle_lazy_subscribe(map(), guild_state()) -> {reply, ok, guild_state()}.
handle_lazy_subscribe(Request, State) ->
    #{session_id := SessionId, channel_id := ChannelId, ranges := Ranges} = Request,
    case should_ignore_member_list_subscribe(Ranges, State) of
        true ->
            {reply, ok, State};
        false ->
            Sessions0 = maps:get(sessions, State, #{}),
            SessionUserId = get_session_user_id(SessionId, Sessions0),
            case
                is_integer(SessionUserId) andalso
                    guild_permissions:can_view_channel(SessionUserId, ChannelId, undefined, State)
            of
                true ->
                    GuildId = maps:get(id, State),
                    ListId = guild_member_list:calculate_list_id(ChannelId, State),
                    {NewState, ShouldSendSync, NormalizedRanges} =
                        guild_member_list:subscribe_ranges(SessionId, ListId, Ranges, State),
                    handle_lazy_subscribe_sync(
                        ShouldSendSync, NormalizedRanges, GuildId, ListId, ChannelId, SessionId, NewState
                    );
                false ->
                    {reply, ok, State}
            end
    end.

-spec should_ignore_member_list_subscribe(list(), guild_state()) -> boolean().
should_ignore_member_list_subscribe([], _State) ->
    false;
should_ignore_member_list_subscribe(_Ranges, State) ->
    not guild_dispatch:is_member_list_updates_enabled(State).

-spec handle_lazy_subscribe_sync(
    boolean(), list(), integer(), term(), channel_id(), session_id(), guild_state()
) ->
    {reply, ok, guild_state()}.
handle_lazy_subscribe_sync(true, [], _GuildId, _ListId, _ChannelId, _SessionId, State) ->
    {reply, ok, State};
handle_lazy_subscribe_sync(true, RangesToSend, GuildId, ListId, ChannelId, SessionId, State) ->
    SyncResponse = guild_member_list:build_sync_response(GuildId, ListId, RangesToSend, State),
    SyncResponseWithChannel = maps:put(
        <<"channel_id">>, integer_to_binary(ChannelId), SyncResponse
    ),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #{pid := SessionPid} when is_pid(SessionPid) ->
            gen_server:cast(
                SessionPid, {dispatch, guild_member_list_update, SyncResponseWithChannel}
            );
        _ ->
            ok
    end,
    {reply, ok, State};
handle_lazy_subscribe_sync(_, _, _GuildId, _ListId, _ChannelId, _SessionId, State) ->
    {reply, ok, State}.

-spec get_session_user_id(session_id(), map()) -> user_id() | undefined.
get_session_user_id(SessionId, Sessions) ->
    case maps:get(SessionId, Sessions, undefined) of
        #{user_id := Uid} -> Uid;
        _ -> undefined
    end.

-spec handle_update_member_subscriptions(session_id(), [user_id()], guild_state()) -> guild_state().
handle_update_member_subscriptions(SessionId, MemberIds, State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    Sessions = maps:get(sessions, State, #{}),
    SessionUserId = get_session_user_id(SessionId, Sessions),
    StateWithPrimedMembers = maybe_prime_very_large_guild_members(MemberIds, State),
    FilteredMemberIds = filter_member_ids_with_mutual_channels(
        SessionUserId, MemberIds, StateWithPrimedMembers
    ),
    OldSubscriptions = guild_subscriptions:get_user_ids_for_session(SessionId, MemberSubs),
    NewMemberSubs = guild_subscriptions:update_subscriptions(
        SessionId, FilteredMemberIds, MemberSubs
    ),
    NewSubscriptions = guild_subscriptions:get_user_ids_for_session(SessionId, NewMemberSubs),
    Added = sets:to_list(sets:subtract(NewSubscriptions, OldSubscriptions)),
    Removed = sets:to_list(sets:subtract(OldSubscriptions, NewSubscriptions)),
    State1 = maps:put(member_subscriptions, NewMemberSubs, StateWithPrimedMembers),
    State2 = handle_added_subscriptions(Added, SessionId, State1),
    handle_removed_subscriptions(Removed, State2).

-spec handle_added_subscriptions([user_id()], session_id(), guild_state()) -> guild_state().
handle_added_subscriptions(Added, SessionId, State) ->
    lists:foldl(
        fun(UserId, Acc) ->
            StateWithPresence = guild_sessions:subscribe_to_user_presence(UserId, Acc),
            guild_presence:send_cached_presence_to_session(UserId, SessionId, StateWithPresence)
        end,
        State,
        Added
    ).

-spec handle_removed_subscriptions([user_id()], guild_state()) -> guild_state().
handle_removed_subscriptions(Removed, State) ->
    lists:foldl(
        fun(UserId, Acc) -> guild_sessions:unsubscribe_from_user_presence(UserId, Acc) end,
        State,
        Removed
    ).

-spec filter_member_ids_with_mutual_channels(user_id() | undefined, [user_id()], guild_state()) ->
    [user_id()].
filter_member_ids_with_mutual_channels(undefined, _, _) ->
    [];
filter_member_ids_with_mutual_channels(SessionUserId, MemberIds, State) ->
    SessionChannels = guild_visibility:viewable_channel_set(SessionUserId, State),
    lists:filtermap(
        fun(MemberId) ->
            case MemberId =:= SessionUserId of
                true ->
                    false;
                false ->
                    case has_shared_channels(SessionChannels, MemberId, State) of
                        true -> {true, MemberId};
                        false -> false
                    end
            end
        end,
        MemberIds
    ).

-spec has_shared_channels(sets:set(), user_id(), guild_state()) -> boolean().
has_shared_channels(_, MemberId, _) when not is_integer(MemberId) ->
    false;
has_shared_channels(SessionChannels, MemberId, State) ->
    CandidateChannels = guild_visibility:viewable_channel_set(MemberId, State),
    not sets:is_empty(sets:intersection(SessionChannels, CandidateChannels)).

-spec maybe_prime_very_large_guild_members([user_id()], guild_state()) -> guild_state().
maybe_prime_very_large_guild_members(UserIds, State) when is_list(UserIds) ->
    case
        {
            maps:get(very_large_guild_coordinator_pid, State, undefined),
            maps:get(very_large_guild_shard_index, State, undefined)
        }
    of
        {CoordPid, ShardIndex} when is_pid(CoordPid), is_integer(ShardIndex), ShardIndex =/= 0 ->
            UniqueUserIds = lists:usort([U || U <- UserIds, is_integer(U), U > 0]),
            case UniqueUserIds of
                [] ->
                    State;
                _ ->
                    MembersReply =
                        try gen_server:call(
                            CoordPid, {very_large_guild_get_members, UniqueUserIds}, 10000
                        ) of
                            Reply -> Reply
                        catch
                            _:_ -> #{}
                        end,
                    prime_members_from_reply(MembersReply, State)
            end;
        _ ->
            State
    end;
maybe_prime_very_large_guild_members(_, State) ->
    State.

-spec prime_members_from_reply(term(), guild_state()) -> guild_state().
prime_members_from_reply(MembersReply, State) when is_map(MembersReply) ->
    Data0 = maps:get(data, State, #{}),
    Data = maps:fold(
        fun(_UserId, Member, AccData) ->
            case is_map(Member) of
                true -> guild_data_index:put_member(Member, AccData);
                false -> AccData
            end
        end,
        Data0,
        MembersReply
    ),
    maps:put(data, Data, State);
prime_members_from_reply(_, State) ->
    State.

-spec maybe_prune_very_large_guild_members(guild_state()) -> guild_state().
maybe_prune_very_large_guild_members(State) ->
    case
        {
            maps:get(very_large_guild_coordinator_pid, State, undefined),
            maps:get(very_large_guild_shard_index, State, undefined)
        }
    of
        {CoordPid, ShardIndex} when is_pid(CoordPid), is_integer(ShardIndex), ShardIndex =/= 0 ->
            prune_member_cache_to_needed_users(State);
        _ ->
            State
    end.

-spec prune_member_cache_to_needed_users(guild_state()) -> guild_state().
prune_member_cache_to_needed_users(State) ->
    Data0 = maps:get(data, State, #{}),
    Members0 = maps:get(<<"members">>, Data0, #{}),
    case is_map(Members0) of
        false ->
            State;
        true ->
            NeededUserIds = needed_member_cache_user_ids(State),
            NeededSet = sets:from_list(NeededUserIds),
            FilteredMembers = maps:filter(
                fun(UserId, _Member) -> sets:is_element(UserId, NeededSet) end,
                Members0
            ),
            case map_size(FilteredMembers) =:= map_size(Members0) of
                true ->
                    State;
                false ->
                    Data1 = guild_data_index:put_member_map(FilteredMembers, Data0),
                    maps:put(data, Data1, State)
            end
    end.

-spec needed_member_cache_user_ids(guild_state()) -> [user_id()].
needed_member_cache_user_ids(State) ->
    Sessions = maps:get(sessions, State, #{}),
    SessionUserIds = maps:fold(
        fun(_SessionId, SessionData, Acc) ->
            case maps:get(user_id, SessionData, undefined) of
                UserId when is_integer(UserId), UserId > 0 -> [UserId | Acc];
                _ -> Acc
            end
        end,
        [],
        Sessions
    ),
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    SubscribedUserIds = maps:keys(MemberSubs),
    lists:usort(SessionUserIds ++ SubscribedUserIds).

-spec deliver_member_list_updates([{session_id(), map()}], guild_state()) -> ok.
deliver_member_list_updates(Deliveries, State) ->
    Sessions = maps:get(sessions, State, #{}),
    lists:foreach(
        fun({SessionId, Payload}) ->
            case maps:get(SessionId, Sessions, undefined) of
                #{pid := SessionPid} = SessionData when is_pid(SessionPid), is_map(Payload) ->
                    case maps:get(pending_connect, SessionData, false) of
                        true ->
                            ok;
                        false ->
                            ChannelId = member_list_payload_channel_id(Payload),
                            case can_session_view_channel(SessionData, ChannelId, State) of
                                true ->
                                    gen_server:cast(
                                        SessionPid, {dispatch, guild_member_list_update, Payload}
                                    );
                                false ->
                                    ok
                            end
                    end;
                _ ->
                    ok
            end
        end,
        Deliveries
    ),
    ok.

-spec member_list_payload_channel_id(map()) -> channel_id().
member_list_payload_channel_id(Payload) ->
    ChannelIdBin = maps:get(<<"channel_id">>, Payload, undefined),
    ListIdBin = maps:get(<<"id">>, Payload, <<"0">>),
    case ChannelIdBin of
        Bin when is_binary(Bin) ->
            case type_conv:to_integer(Bin) of
                undefined -> 0;
                Id -> Id
            end;
        _ ->
            case type_conv:to_integer(ListIdBin) of
                undefined -> 0;
                Id -> Id
            end
    end.

-spec can_session_view_channel(map(), channel_id(), guild_state()) -> boolean().
can_session_view_channel(_SessionData, ChannelId, _State) when not is_integer(ChannelId); ChannelId =< 0 ->
    false;
can_session_view_channel(SessionData, ChannelId, State) ->
    case {maps:get(user_id, SessionData, undefined), maps:get(viewable_channels, SessionData, undefined)} of
        {UserId, ViewableChannels} when is_integer(UserId), is_map(ViewableChannels) ->
            maps:is_key(ChannelId, ViewableChannels) orelse
                guild_permissions:can_view_channel(UserId, ChannelId, undefined, State);
        {UserId, _} when is_integer(UserId) ->
            guild_permissions:can_view_channel(UserId, ChannelId, undefined, State);
        _ ->
            false
    end.

-ifdef(TEST).

-spec disabled_operations_state(integer() | binary()) -> guild_state().
disabled_operations_state(Value) ->
    #{data => #{<<"guild">> => #{<<"disabled_operations">> => Value}}}.

should_ignore_member_list_subscribe_ignores_non_empty_ranges_when_disabled_test() ->
    ?assertEqual(
        true,
        should_ignore_member_list_subscribe(
            [{0, 99}],
            disabled_operations_state(1 bsl 6)
        )
    ).

should_ignore_member_list_subscribe_allows_empty_ranges_when_disabled_test() ->
    ?assertEqual(
        false,
        should_ignore_member_list_subscribe(
            [],
            disabled_operations_state(1 bsl 6)
        )
    ).

-endif.
