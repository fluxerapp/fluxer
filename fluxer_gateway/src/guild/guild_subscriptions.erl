%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_subscriptions).

-typing([eqwalizer]).

-export([
    init_state/0,
    unsubscribe/3,
    unsubscribe_session/2,
    update_subscriptions_with_delta/3,
    get_subscribed_sessions/2,
    get_user_ids_for_session/2
]).

-type session_id() :: binary().
-type user_id() :: integer().
-type subscription_state() :: #{user_id() => sets:set(session_id())}.
-export_type([session_id/0, user_id/0, subscription_state/0]).

-spec init_state() -> subscription_state().
init_state() -> #{}.

-spec subscribe(session_id(), user_id(), subscription_state()) -> subscription_state().
subscribe(SessionId, UserId, State) ->
    Subscribers = maps:get(UserId, State, sets:new()),
    NewSubscribers = sets:add_element(SessionId, Subscribers),
    State#{UserId => NewSubscribers}.

-spec unsubscribe(session_id(), user_id(), subscription_state()) -> subscription_state().
unsubscribe(SessionId, UserId, State) ->
    case maps:get(UserId, State, undefined) of
        undefined ->
            State;
        Subscribers ->
            remove_session_subscriber(UserId, SessionId, Subscribers, State)
    end.

-spec remove_session_subscriber(
    user_id(), session_id(), sets:set(session_id()), subscription_state()
) ->
    subscription_state().
remove_session_subscriber(UserId, SessionId, Subscribers, State) ->
    NewSubscribers = sets:del_element(SessionId, Subscribers),
    case sets:size(NewSubscribers) of
        0 -> maps:remove(UserId, State);
        _ -> State#{UserId => NewSubscribers}
    end.

%% Every subscriber set in the map is non-empty: this module, guild_maintenance and
%% guild_presence_sync all drop a user key once its last subscriber goes. Entries the
%% session is absent from are therefore already correct and are left as they are
%% instead of being rebuilt into a fresh map.
-spec unsubscribe_session(session_id(), subscription_state()) -> subscription_state().
unsubscribe_session(SessionId, State) ->
    maps:fold(
        fun(UserId, Subscribers, Acc) ->
            drop_session_subscriber(UserId, SessionId, Subscribers, Acc)
        end,
        State,
        State
    ).

-spec drop_session_subscriber(
    user_id(), session_id(), sets:set(session_id()), subscription_state()
) ->
    subscription_state().
drop_session_subscriber(UserId, SessionId, Subscribers, Acc) ->
    case sets:is_element(SessionId, Subscribers) of
        true -> remove_session_subscriber(UserId, SessionId, Subscribers, Acc);
        false -> Acc
    end.

%% After both folds the session subscribes to exactly NewMemberIds, so the added and
%% removed sets are the ToAdd and ToRemove already computed here. Deriving them from
%% the resulting map instead would cost two more full folds of the subscription map.
-spec update_subscriptions_with_delta(session_id(), [user_id()], subscription_state()) ->
    {subscription_state(), [user_id()], [user_id()]}.
update_subscriptions_with_delta(SessionId, NewMemberIds, State) ->
    CurrentlySubscribed = get_user_ids_for_session(SessionId, State),
    NewMemberIdSet = sets:from_list(NewMemberIds),
    ToRemove = sets:subtract(CurrentlySubscribed, NewMemberIdSet),
    ToAdd = sets:subtract(NewMemberIdSet, CurrentlySubscribed),
    State1 = sets:fold(
        fun(UserId, AccState) ->
            unsubscribe(SessionId, UserId, AccState)
        end,
        State,
        ToRemove
    ),
    State2 = sets:fold(
        fun(UserId, AccState) ->
            subscribe(SessionId, UserId, AccState)
        end,
        State1,
        ToAdd
    ),
    {State2, sets:to_list(ToAdd), sets:to_list(ToRemove)}.

-spec get_subscribed_sessions(user_id(), subscription_state()) -> [session_id()].
get_subscribed_sessions(UserId, State) ->
    case maps:get(UserId, State, undefined) of
        undefined -> [];
        Subscribers -> sets:to_list(Subscribers)
    end.

-spec get_user_ids_for_session(session_id(), subscription_state()) -> sets:set(user_id()).
get_user_ids_for_session(SessionId, State) ->
    maps:fold(
        fun(UserId, Subscribers, Acc) ->
            collect_user_for_session(UserId, SessionId, Subscribers, Acc)
        end,
        sets:new(),
        State
    ).

-spec collect_user_for_session(
    user_id(), session_id(), sets:set(session_id()), sets:set(user_id())
) -> sets:set(user_id()).
collect_user_for_session(UserId, SessionId, Subscribers, Acc) ->
    case sets:is_element(SessionId, Subscribers) of
        true -> sets:add_element(UserId, Acc);
        false -> Acc
    end.
