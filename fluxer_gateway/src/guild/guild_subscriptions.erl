%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_subscriptions).
-typing([eqwalizer]).

-export([
    init_state/0,
    subscribe/3,
    unsubscribe/3,
    unsubscribe_session/2,
    update_subscriptions/3,
    update_subscriptions_with_delta/3,
    get_subscribed_sessions/2,
    is_subscribed/3,
    get_user_ids_for_session/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

-spec update_subscriptions(session_id(), [user_id()], subscription_state()) ->
    subscription_state().
update_subscriptions(SessionId, NewMemberIds, State) ->
    {NewState, _Added, _Removed} = update_subscriptions_with_delta(
        SessionId, NewMemberIds, State
    ),
    NewState.

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

-spec is_subscribed(session_id(), user_id(), subscription_state()) -> boolean().
is_subscribed(SessionId, UserId, State) ->
    case maps:get(UserId, State, undefined) of
        undefined -> false;
        Subscribers -> sets:is_element(SessionId, Subscribers)
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

-ifdef(TEST).

unsubscribe_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = unsubscribe(<<"session1">>, 123, State1),
    ?assertNot(is_subscribed(<<"session1">>, 123, State2)).

unsubscribe_one_of_many_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = subscribe(<<"session2">>, 123, State1),
    State3 = unsubscribe(<<"session1">>, 123, State2),
    ?assertNot(is_subscribed(<<"session1">>, 123, State3)),
    ?assert(is_subscribed(<<"session2">>, 123, State3)).

unsubscribe_session_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = subscribe(<<"session1">>, 456, State1),
    State3 = subscribe(<<"session2">>, 123, State2),
    State4 = unsubscribe_session(<<"session1">>, State3),
    ?assertNot(is_subscribed(<<"session1">>, 123, State4)),
    ?assertNot(is_subscribed(<<"session1">>, 456, State4)),
    ?assert(is_subscribed(<<"session2">>, 123, State4)).

update_subscriptions_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 100, State0),
    State2 = subscribe(<<"session1">>, 200, State1),
    State3 = update_subscriptions(<<"session1">>, [200, 300], State2),
    ?assertNot(is_subscribed(<<"session1">>, 100, State3)),
    ?assert(is_subscribed(<<"session1">>, 200, State3)),
    ?assert(is_subscribed(<<"session1">>, 300, State3)).

%% The subscription map as unsubscribe_session/2 built it before it stopped
%% rebuilding untouched entries.
%% The state and delta handle_update_member_subscriptions_local/4 derived before it
%% stopped re-folding the map to rediscover what changed.
update_subscriptions_with_delta_leaves_other_sessions_alone_test() ->
    State0 = subscribe(<<"s2">>, 100, subscribe(<<"s1">>, 100, init_state())),
    {NewState, Added, Removed} = update_subscriptions_with_delta(<<"s1">>, [200], State0),
    ?assertEqual([200], Added),
    ?assertEqual([100], Removed),
    ?assert(is_subscribed(<<"s2">>, 100, NewState)),
    ?assertNot(is_subscribed(<<"s1">>, 100, NewState)),
    ?assert(is_subscribed(<<"s1">>, 200, NewState)).

-endif.
