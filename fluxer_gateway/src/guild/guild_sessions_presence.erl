%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions_presence).
-typing([eqwalizer]).

-export([
    subscribe_connected_user_presence/2,
    subscribe_to_user_presence/2,
    subscribe_without_cached_presence/2,
    unsubscribe_from_user_presence/2,
    unsubscribe_many_from_user_presence/3,
    handle_user_offline/2
]).

-type guild_state() :: map().
-type user_id() :: integer().
-export_type([guild_state/0, user_id/0]).

-spec subscribe_connected_user_presence(user_id(), guild_state()) -> guild_state().
subscribe_connected_user_presence(UserId, State) ->
    subscribe_to_user_presence(UserId, State).

-spec subscribe_to_user_presence(user_id(), guild_state()) -> guild_state().
subscribe_to_user_presence(UserId, State) ->
    case subscribe_without_cached_presence(UserId, State) of
        {fresh, StateWithSubs} -> maybe_send_cached_presence(UserId, StateWithSubs);
        {existing, StateWithSubs} -> StateWithSubs
    end.

-spec subscribe_without_cached_presence(user_id(), guild_state()) ->
    {fresh | existing, guild_state()}.
subscribe_without_cached_presence(UserId, State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    case maps:get(UserId, PresenceSubs, 0) of
        0 ->
            presence_bus:subscribe(UserId),
            {fresh, State#{presence_subscriptions => PresenceSubs#{UserId => 1}}};
        CurrentCount ->
            {existing, State#{
                presence_subscriptions => PresenceSubs#{UserId => CurrentCount + 1}
            }}
    end.

-spec unsubscribe_from_user_presence(user_id(), guild_state()) -> guild_state().
unsubscribe_from_user_presence(UserId, State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    CurrentCount = maps:get(UserId, PresenceSubs, 0),
    case CurrentCount of
        0 ->
            State;
        1 ->
            safe_presence_unsubscribe(UserId),
            NewSubs = maps:remove(UserId, PresenceSubs),
            State#{presence_subscriptions => NewSubs};
        _ ->
            NewSubs = PresenceSubs#{UserId => CurrentCount - 1},
            State#{presence_subscriptions => NewSubs}
    end.

-spec unsubscribe_many_from_user_presence(user_id(), non_neg_integer(), guild_state()) ->
    guild_state().
unsubscribe_many_from_user_presence(_UserId, 0, State) ->
    State;
unsubscribe_many_from_user_presence(UserId, Count, State) when Count > 0 ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    CurrentCount = maps:get(UserId, PresenceSubs, 0),
    case CurrentCount of
        0 ->
            State;
        CurrentCount when CurrentCount =< Count ->
            safe_presence_unsubscribe(UserId),
            NewSubs = maps:remove(UserId, PresenceSubs),
            State#{presence_subscriptions => NewSubs};
        _ ->
            NewSubs = PresenceSubs#{UserId => CurrentCount - Count},
            State#{presence_subscriptions => NewSubs}
    end.

-spec safe_presence_unsubscribe(user_id()) -> ok.
safe_presence_unsubscribe(UserId) ->
    try
        presence_bus:unsubscribe(UserId)
    catch
        error:_ -> ok;
        exit:_ -> ok;
        throw:_ -> ok
    end.

-spec handle_user_offline(user_id(), guild_state()) -> guild_state().
handle_user_offline(UserId, State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    case maps:get(UserId, PresenceSubs, undefined) of
        0 ->
            presence_bus:unsubscribe(UserId),
            NewSubs = maps:remove(UserId, PresenceSubs),
            StateWithSubs = State#{presence_subscriptions => NewSubs},
            remove_member_presence(UserId, StateWithSubs);
        undefined ->
            remove_member_presence(UserId, State);
        _ ->
            State
    end.

-spec remove_member_presence(user_id(), guild_state()) -> guild_state().
remove_member_presence(UserId, State) ->
    Tab = maps:get(member_presence, State),
    ets:delete(Tab, UserId),
    ok = guild_member_list_read:note_presence_write(UserId),
    State.

-spec maybe_send_cached_presence(user_id(), guild_state()) -> guild_state().
maybe_send_cached_presence(UserId, State) ->
    case safe_presence_cache_get(UserId) of
        {ok, Payload} ->
            {noreply, UpdatedState} =
                guild_presence:handle_bus_presence(UserId, Payload, State),
            UpdatedState;
        _ ->
            State
    end.

-spec safe_presence_cache_get(user_id()) -> {ok, map()} | not_found.
safe_presence_cache_get(UserId) ->
    try presence_cache:get(UserId) of
        {ok, Payload} when is_map(Payload) -> {ok, Payload};
        _ -> not_found
    catch
        error:_ -> not_found;
        exit:_ -> not_found;
        throw:_ -> not_found
    end.
